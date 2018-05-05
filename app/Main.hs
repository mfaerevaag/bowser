module Main where

import Control.Monad
import Data.Maybe
import Text.Show.Pretty
import System.Console.ParseArgs

import Bowser.Parser
import Bowser.Engine
import Bowser.AST

data Options = Filename
             | Threshold
             | VerboseFlag
             deriving (Ord, Eq, Show)

argd :: [ Arg Options ]
argd = [ Arg { argIndex = Filename,
               argName = Just "file",
               argAbbr = Just 'f',
               argData = argDataRequired "filename" ArgtypeString,
               argDesc = "file to run" }
       , Arg { argIndex = Threshold,
               argName = Just "threshold",
               argAbbr = Just 't',
               argData = argDataDefaulted "default" ArgtypeInt 1000000,
               argDesc = "step threshold for execution" }
       , Arg { argIndex = VerboseFlag,
               argName = Just "verbose",
               argAbbr = Just 'v',
               argData = Nothing,
               argDesc = "verbose flag" }
       ]

main :: IO ()
main = do
  args <- parseArgsIO
          (ArgsParseControl ArgsComplete ArgsHardDash)
          argd

  ast <- parseFile (fromJust (getArg args Filename))

  when (gotArg args VerboseFlag) $ do
    putStrLn "ast:"
    pPrint ast
    putStrLn ""

  res <- eval ast (toInteger ((fromJust (getArg args Threshold)::Int)))

  when (gotArg args VerboseFlag) $ do
    putStrLn ""
    putStrLn "return:"
    pPrint res
