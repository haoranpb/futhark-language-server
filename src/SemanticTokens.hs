module SemanticTokens (getSemanticTokens) where

import Data.List (find, sortOn)
import Data.Text (Text, tail)
import Futhark.Compiler.Program (FileModule (fileProg), lpImports)
import Futhark.Util.Loc (Loc (Loc), Pos (Pos))
import Language.Futhark.Semantic (includeToString, mkInitialImport)
import Language.Futhark.Syntax
import Language.LSP.Types
  ( List (List),
    SemanticTokenAbsolute (SemanticTokenAbsolute, line, startChar),
    SemanticTokenTypes (..),
    SemanticTokens (..),
    UInt,
    encodeTokens,
    relativizeTokens,
  )
import qualified System.FilePath.Posix as Posix
import Utils (State (..), debug, emptySemanticTokens, semanticTokensLegend)

getSemanticTokens :: State -> Maybe FilePath -> IO SemanticTokens
getSemanticTokens state (Just path) =
  case stateProgram state of
    Nothing -> pure emptySemanticTokens
    Just loadedProg -> do
      -- Find the Prog that contains the AST
      -- Referencing Language.Futhark.Query
      let imports = lpImports loadedProg
          fileModule = snd <$> find ((== file) . fst) imports
          prog = fileProg <$> fileModule
      case prog of
        Nothing -> pure emptySemanticTokens
        Just prog' -> do
          let tokens = getTokens (progDecs prog')
          debug $ show tokens
          case transformTokens tokens of
            Left err -> do
              debug $ "Error transforming tokens: " ++ show err
              pure emptySemanticTokens
            Right transformedTokens -> do
              debug $ show transformedTokens
              pure $ SemanticTokens Nothing (List transformedTokens)
      where
        file = includeToString $ mkInitialImport $ fst $ Posix.splitExtension path
getSemanticTokens _ Nothing = pure emptySemanticTokens

getTokens :: [DecBase f vn] -> [SemanticTokenAbsolute]
getTokens = concatMap tokenDec

tokenDec :: DecBase f vn -> [SemanticTokenAbsolute]
tokenDec (ValDec vbind) =
  case valBindRetDecl vbind of
    Nothing -> tokenExpPat
    Just ret -> tokenExpPat ++ tokenTypeExp ret
  where
    tokenExpPat = tokenExp (valBindBody vbind) ++ concatMap tokenPat (valBindParams vbind)
tokenDec (TypeDec tbind) = tokenTypeExp (typeExp tbind)
tokenDec (SigDec sbind) = tokenSigExp (sigExp sbind)
tokenDec (ModDec mbind) =
  case modSignature mbind of
    Nothing -> tokenModExpParams
    Just (sig, _) -> tokenSigExp sig ++ tokenModExpParams
  where
    tokenModExpParams = concatMap tokenModParams (modParams mbind) ++ tokenModExp (modExp mbind)
tokenDec (OpenDec mexp _loc) = tokenModExp mexp
tokenDec (LocalDec dec _loc) = tokenDec dec
tokenDec (ImportDec _filepath _file _loc) = []

-- parameters in function declarations
tokenPat :: PatBase f vn -> [SemanticTokenAbsolute]
tokenPat (Id _vn _t loc) = [mkSematicToken loc SttParameter]
tokenPat (TuplePat pats _loc) = concatMap tokenPat pats
tokenPat (RecordPat fields _loc) = concatMap (tokenPat . snd) fields
tokenPat (PatParens pat _loc) = tokenPat pat
tokenPat (Wildcard _t _loc) = [] -- underscore
tokenPat (PatAscription pat typeDecl _loc) = tokenPat pat ++ tokenTypeExp (declaredType typeDecl)
tokenPat (PatLit _pLit _t _loc) = [] -- need example
tokenPat (PatConstr _name _t pats _loc) = concatMap tokenPat pats
tokenPat (PatAttr _attr pat _loc) = tokenPat pat

-- TODO
tokenExp :: ExpBase f vn -> [SemanticTokenAbsolute]
tokenExp (Literal _primv _loc) = []
tokenExp (IntLit _i _t loc) = [mkSematicToken loc SttNumber]
tokenExp (FloatLit _f _t loc) = [mkSematicToken loc SttNumber]
tokenExp (StringLit _s loc) = [mkSematicToken loc SttString]
tokenExp (Var _qn _t loc) = [mkSematicToken loc SttKeyword] -- Question: how to work on "f PatType"
tokenExp (Parens appExp _loc) = tokenExp appExp
tokenExp (AppExp appExp _appRes) = tokenAppExp appExp
tokenExp _ = []

-- Question: maybe go through some AppExp definations?
tokenAppExp :: AppExpBase f vn -> [SemanticTokenAbsolute]
tokenAppExp (BinOp _qn _t (exp1, _stuc1) (exp2, _struc2) _loc) = tokenExp exp1 ++ tokenExp exp2
tokenAppExp (Apply exp1 exp2 _diet _loc) = tokenExp exp1 ++ tokenExp exp2
tokenAppExp _ = []

tokenTypeExp :: TypeExp vn -> [SemanticTokenAbsolute]
tokenTypeExp t = [mkSematicToken (srclocOf t) SttType]

tokenSigExp :: SigExpBase f vn -> [SemanticTokenAbsolute]
tokenSigExp _ = []

tokenModParams :: ModParamBase f vn -> [SemanticTokenAbsolute]
tokenModParams _ = []

tokenModExp :: ModExpBase f vn -> [SemanticTokenAbsolute]
tokenModExp _ = []

-- Question: is this the case?
inferVarType :: PatType -> SemanticTokenTypes
inferVarType (Scalar _) = SttFunction
inferVarType _ = SttVariable

mkSematicToken :: SrcLoc -> SemanticTokenTypes -> SemanticTokenAbsolute
mkSematicToken srcLoc tokenType = do
  let Loc start end = locOf srcLoc
      Pos _ tLine col_start _ = start
      Pos _ _ col_end _ = end
  SemanticTokenAbsolute (toEnum tLine - 1) (toEnum col_start -1) (toEnum $ col_end - col_start + 1) tokenType []

-- encode tokens according to lsp spec
transformTokens :: [SemanticTokenAbsolute] -> Either Text [UInt]
transformTokens absTokens = do
  -- assume max 100 char per line
  let sortedTokens = sortOn (\token -> line token * 100 + startChar token) absTokens
      relTokens = relativizeTokens sortedTokens
  encodeTokens semanticTokensLegend relTokens
