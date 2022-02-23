{-# LANGUAGE OverloadedStrings #-}

module Tool (getHoverInfoFromImports) where

import qualified Data.Text as T
import Futhark.Compiler (Imports)
import Futhark.Util.Loc (srclocOf)
import Futhark.Util.Pretty (pretty)
import Language.Futhark.Query
import Language.Futhark.Syntax (locStr)

getHoverInfoFromImports :: Maybe Imports -> Maybe FilePath -> Int -> Int -> IO (Maybe T.Text)
getHoverInfoFromImports (Just imports) (Just path) l c = do
  case atPos imports $ Pos path l c 0 of
    Nothing -> pure $ Just "No information available"
    Just (AtName qn def _loc) -> do
      case def of
        Nothing -> pure $ Just "No information available"
        Just (BoundTerm t defloc) -> do
          pure $ Just $ T.pack $ pretty qn ++ " :: " ++ pretty t ++ "\n\n" ++ "**Definition: " ++ locStr (srclocOf defloc) ++ "**"
        Just (BoundType defloc) ->
          pure $ Just $ T.pack $ "Definition: " ++ locStr (srclocOf defloc)
        Just (BoundModule defloc) ->
          pure $ Just $ T.pack $ "Definition: " ++ locStr (srclocOf defloc)
        Just (BoundModuleType defloc) ->
          pure $ Just $ T.pack $ "Definition: " ++ locStr (srclocOf defloc)
getHoverInfoFromImports _ _ _ _ = pure $ Just "No information available"
