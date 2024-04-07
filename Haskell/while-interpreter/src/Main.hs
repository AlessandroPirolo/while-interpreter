module Main (main) where

import System.Directory.Internal.Prelude (getArgs)
import Iteractive.Iteractive ( fromFile, iteractive_ )
import Iteractive.Pretty (prettyItMode)


main :: IO()
main = do
  arg <- getArgs
  if null arg then do
    putStrLn prettyItMode
    iteractive_
  else
    fromFile (head arg)
  putStrLn "Finished."
