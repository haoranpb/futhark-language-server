module SemanticTokens (getSemanticTokens) where

import Data.List (find, sortOn)
import Futhark.Compiler.Program (FileModule (fileProg), lpImports)
import Futhark.Util.Loc (Loc (Loc), Pos (Pos))
import Language.Futhark.Semantic (includeToString, mkInitialImport)
import Language.Futhark.Syntax
import Language.LSP.Types (List (..), SemanticTokens (..), UInt)
import qualified System.FilePath.Posix as Posix
import Utils (SemanticTokenTypes (..), State (..), debug, emptySemanticTokens)

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
          pure $ SemanticTokens Nothing (encodeTokens tokens)
      where
        file = includeToString $ mkInitialImport $ fst $ Posix.splitExtension path
getSemanticTokens _ Nothing = pure emptySemanticTokens

getTokens :: [DecBase f vn] -> [[UInt]]
getTokens = concatMap tokenDec

tokenDec :: DecBase f vn -> [[UInt]]
tokenDec (ValDec vbind) =
  case valBindRetDecl vbind of
    Nothing -> tokenExpPat
    Just ret -> tokenExpPat ++ tokenTypeExp ret
  where
    tokenExpPat = tokenExp (valBindBody vbind) : concatMap tokenPat (valBindParams vbind)
tokenDec (TypeDec tbind) = tokenTypeExp (typeExp tbind)
tokenDec (SigDec sbind) = [tokenSigExp (sigExp sbind)]
tokenDec (ModDec mbind) =
  case modSignature mbind of
    Nothing -> tokenModExpParams
    Just (sig, _) -> tokenSigExp sig : tokenModExpParams
  where
    tokenModExpParams = map tokenModParams (modParams mbind) ++ [tokenModExp (modExp mbind)]
tokenDec (OpenDec mexp _loc) = [tokenModExp mexp]
tokenDec (LocalDec dec _loc) = tokenDec dec
tokenDec (ImportDec _filepath _file _loc) = []

-- parameters in function declarations
tokenPat :: PatBase f vn -> [[UInt]]
tokenPat (Id _vn _t loc) = [mkSematicToken loc Parameter]
tokenPat (TuplePat pats _loc) = concatMap tokenPat pats
tokenPat (RecordPat fields _loc) = concatMap (tokenPat . snd) fields
tokenPat (PatParens pat _loc) = tokenPat pat
tokenPat (Wildcard _t _loc) = [] -- underscore
tokenPat (PatAscription pat typeDecl _loc) = tokenPat pat ++ tokenTypeExp (declaredType typeDecl)
tokenPat (PatLit _pLit _t _loc) = [] -- need example
tokenPat (PatConstr _name _t pats _loc) = concatMap tokenPat pats
tokenPat (PatAttr _attr pat _loc) = tokenPat pat

-- TODO
tokenExp :: ExpBase f vn -> [UInt]
tokenExp (Literal _primv _loc) = []
tokenExp (IntLit _i _t loc) = mkSematicToken loc Number
tokenExp (FloatLit _f _t loc) = mkSematicToken loc Number
tokenExp (StringLit _s loc) = mkSematicToken loc String
tokenExp (Var qn _t loc) = [] -- semantic depends on the context
tokenExp (AppExp appExp _appRes) = tokenAppExp appExp
tokenExp _ = []

tokenAppExp :: AppExpBase f vn -> [UInt]
tokenAppExp _ = []

tokenTypeExp :: TypeExp vn -> [[UInt]]
tokenTypeExp _ = []

tokenSigExp :: SigExpBase f vn -> [UInt]
tokenSigExp _ = []

tokenModParams :: ModParamBase f vn -> [UInt]
tokenModParams _ = []

tokenModExp :: ModExpBase f vn -> [UInt]
tokenModExp _ = []

mkSematicToken :: SrcLoc -> SemanticTokenTypes -> [UInt]
mkSematicToken srcLoc tokenType = do
  let Loc start end = locOf srcLoc
      Pos _ line col_start _ = start
      Pos _ _ col_end _ = end
  [toEnum line - 1, toEnum col_start, toEnum $ col_end - col_start + 2, toEnum $ fromEnum tokenType, 0]

-- encode tokens according to lsp spec
encodeTokens :: [[UInt]] -> List UInt
encodeTokens tokens = do
  let nonEmptyTokens = filter (not . null) tokens
      sortedTokens = sortOn (\token -> head token * 100 + token !! 1) nonEmptyTokens -- assume max 100 char per line
      encodedTokens =
        scanl1
          ( \t1 t2 ->
              if head t1 == head t2
                then [0, t2 !! 1 - t1 !! 1] ++ drop 2 t2
                else t2
          )
          sortedTokens
  List $ concat encodedTokens
