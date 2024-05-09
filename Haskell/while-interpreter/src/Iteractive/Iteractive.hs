module Iteractive.Iteractive (
    it
    , fromFile
) where

import Interp.Interp (start)
import System.IO (hFlush, stdout)
import Control.Monad (unless)
import Iteractive.Pretty (prettyMap)

fromFile :: String -> IO()
fromFile input = do
    print_ "computing...\n"
    let state = start input
    print_ $ prettyMap state

it :: IO()
it = do
  input <- read_
  unless (input == ":q" || input == ":quit")
       $ print_ ("computing...\n" ++ whileProg input) >> it

read_ :: IO String
read_ = putStr "\ESC[94mWhilePlus> \ESC[0m"
     >> hFlush stdout
     >> getLine

print_ :: String -> IO ()
print_ = putStrLn

whileProg :: String -> String
whileProg input = let
  state = start input
  in prettyMap state