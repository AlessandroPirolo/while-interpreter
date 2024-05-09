module Main (main) where

import System.Directory.Internal.Prelude (getArgs)
import Iteractive.Iteractive ( fromFile, it )
import Iteractive.Pretty (prettyItMode)

main :: IO()
main = do
  arg <- getArgs
  if null arg 
    then do
      putStrLn prettyItMode
      it
    else
      fromFile (head arg)
  putStrLn "Finished."
