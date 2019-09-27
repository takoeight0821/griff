{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Control.Effect
import           Control.Monad.IO.Class
import           Data.Maybe
import           Data.Outputable
import           Data.String
import           Language.Griff.CodeGen
import           Language.Griff.Parser
import           Language.Griff.Rename
import           Language.Griff.Syntax
import           Language.Griff.Typing.Infer
import           Language.Griff.Typing.Monad
import           Language.Griff.Uniq
import           Text.Megaparsec

main :: IO ()
main = do
  src <- getContents
  let ast = parse pDecs "<stdin>" (fromString src)
  case ast of
    Right ast -> runM $ runUniq $ do
      ast' <- rename ast
      typeEnv <- runInfer mempty (infer ast')
      liftIO $ print $ ppr ast'
      liftIO $ print $ ppr typeEnv
      let scs = mapMaybe scDef ast'
      liftIO $ codegen scs
    Left err -> liftIO $ putStrLn $ errorBundlePretty err

  where
    scDef (ScDef _ f xs e) = Just (f, xs, e)
    scDef _                = Nothing
