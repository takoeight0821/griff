{-# LANGUAGE OverloadedStrings #-}
module Main where

import Language.Griff.Syntax
import Data.Maybe
import Control.Monad.IO.Class
import Data.Outputable
import Language.Griff.Monad
import Language.Griff.Typing.Monad
import Language.Griff.Typing.Infer
import Language.Griff.Parser
import Language.Griff.Rename
import Language.Griff.CodeGen
import Text.Megaparsec
import Data.String

main :: IO ()
main = do
  src <- getContents
  let ast = parse pDecs "<stdin>" (fromString src)
  case ast of
    Right ast -> runGriffT "<stdin>" $ do
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
