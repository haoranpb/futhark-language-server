{-# LANGUAGE OverloadedStrings #-}

module SemanticTokens (getSemanticTokens) where

-- Get semantic tokens from AST
-- Partially finished for proof of concept
-- The rest should be easily done following similar approach

import Control.Monad (msum)
import Control.Monad.IO.Class (liftIO)
import Data.List (find)
import Futhark.Compiler.Program (lpImports)
import Futhark.Util.Loc (Loc (Loc))
import Language.Futhark.Query
import Language.Futhark.Semantic (FileModule (fileProg), includeToString, mkInitialImport)
import Language.Futhark.Syntax
import Language.LSP.Types (List (List), SemanticTokens (..), UInt)
import qualified System.FilePath.Posix as Posix
import Utils (State (..), debug, emptySemanticTokens)

getSemanticTokens :: State -> Maybe FilePath -> IO SemanticTokens
getSemanticTokens state (Just path) =
  case stateProgram state of
    Nothing -> pure emptySemanticTokens
    Just loadedProg -> do
      -- Find the Prog that contains the AST
      -- Referencing Query
      let imports = lpImports loadedProg
          fileModule = snd <$> find ((== file) . fst) imports
          prog = fileProg <$> fileModule
      case prog of
        Nothing -> pure emptySemanticTokens
        Just prog' -> do
          finalTokens <- msum $ map getTokens (progDecs prog')
          debug $ "Final Tokens: " ++ show finalTokens
          pure emptySemanticTokens
      pure emptySemanticTokens
      where
        file = includeToString $ mkInitialImport $ fst $ Posix.splitExtension path
getSemanticTokens _ Nothing = pure emptySemanticTokens

-- Only focusing on ValDec for proof of concept
-- Others can be easily added by going through the AST
getTokens :: Showable f vn => DecBase f vn -> IO (List [UInt])
getTokens (ValDec vBind) = do
  debug "ValBind found, going in..."
  valBindTokens vBind
getTokens (LocalDec dec _loc) = getTokens dec
getTokens d = do
  debug $ show d
  pure $ List []

-- debug $ "Depth: " ++ show depth ++ "ValBind: " ++ show (valBindEntryPoint vBind)
-- debug $ "Depth: " ++ show depth ++ "ValBind: " ++ show (valBindName vBind)
-- debug $ "Depth: " ++ show depth ++ "ValBind: " ++ show (valBindRetDecl vBind)
-- debug $ "Depth: " ++ show depth ++ "ValBind: " ++ show (valBindRetType vBind)
-- debug $ "Depth: " ++ show depth ++ "ValBind: " ++ show (valBindTypeParams vBind)
-- debug $ "Depth: " ++ show depth ++ "ValBind: " ++ show (valBindBody vBind)
-- debug $ "Depth: " ++ show depth ++ "ValBind: " ++ show (valBindDoc vBind)
-- debug $ "Depth: " ++ show depth ++ "ValBind: " ++ show (valBindAttrs vBind)
-- debug $ "Depth: " ++ show depth ++ "ValBind: " ++ show (valBindLocation vBind)

-- Implementing valBindParams first, rest can be easily added
valBindTokens :: Showable f vn => ValBindBase f vn -> IO (List [UInt])
valBindTokens vBind = do
  debug "focusing on valBindParams..."
  msum $ map valBindParamsTokens (valBindParams vBind)

valBindParamsTokens :: Language.Futhark.Syntax.Showable f vn => PatBase f vn -> IO (List [UInt])
valBindParamsTokens (Id vn t loc) = do
  debug $ "ValBindParam: (ID) " ++ show vn
  let tokens = srcLocToToken loc 7
  debug $ "ID tokens: " ++ show tokens
  pure tokens
valBindParamsTokens (PatParens pat loc) = do
  debug $ "ValBindParam: (PatParens) " ++ show pat ++ " " ++ locStr loc
  valBindParamsTokens pat
valBindParamsTokens (PatAscription pat typ loc) = do
  debug $ "ValBindParam: (PatAscription) (Pat) " ++ show pat ++ " " ++ locStr loc
  debug $ "ValBindParam: (PatAscription) (Type) " ++ show typ ++ " " ++ locStr (locOf typ)
  typeDecTokens $ declaredType typ
  valBindParamsTokens pat
  pure $ List [[0, 0, 3, 1, 0]]
valBindParamsTokens _ = pure $ List []

-- valBindParamsTokens (TuplePat pats loc) = do
--   debug $ "ValBindParam: (TuplePat) " ++ show pats ++ " " ++ locStr loc
--   msum $ map valBindParamsTokens pats
-- valBindParamsTokens (RecordPat pats loc) = debug $ "ValBindParam: (RecordPat) " ++ show pats ++ " " ++ locStr loc
-- valBindParamsTokens (Wildcard typ loc) = debug $ "ValBindParam: (Wildcard) " ++ show typ ++ " " ++ locStr loc
-- valBindParamsTokens (PatLit lit typ loc) = debug $ "ValBindParam: (PatLit) " ++ show lit ++ " " ++ show typ ++ " " ++ locStr loc
-- valBindParamsTokens (PatConstr name typ pats loc) = do
--   debug $ "ValBindParam: (PatConstr) " ++ show name ++ " " ++ show typ ++ " " ++ show pats ++ " " ++ locStr loc
--   msum $ map valBindParamsTokens pats
-- valBindParamsTokens (PatAttr attr pat loc) = do
--   debug $ "ValBindParam: (PatAttr) " ++ show attr ++ " " ++ show pat ++ " " ++ locStr loc
--   valBindParamsTokens pat

typeDecTokens :: Show vn => TypeExp vn -> IO ()
typeDecTokens (TEArray typeExps dimExps loc) = do
  debug $ "TypeExp: (TEArray) " ++ locStr loc
  typeDecTokens typeExps
typeDecTokens (TEVar name loc) = do
  let Loc start end = locOf loc
  debug $ "TypeExp: (TEVar) " ++ show name ++ " " ++ locStr loc
typeDecTokens e = pure ()

-- Do not have multilineTokenSupport, as least for now
-- Type of the token should be encoded by mapping, see doc below
-- https://microsoft.github.io/language-server-protocol/specifications/specification-current/#textDocument_semanticTokens
srcLocToToken :: SrcLoc -> UInt -> List [UInt]
srcLocToToken srcLoc typeEnum = do
  let Loc start end = locOf srcLoc
      Pos _ line col_start _ = start
      Pos _ _ col_end _ = end
  List [[toEnum line, toEnum col_start, toEnum $ col_end - col_start + 2, typeEnum, 0]]
