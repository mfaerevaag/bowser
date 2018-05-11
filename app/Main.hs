module Main where

import Control.Monad
import Data.Maybe
import Text.Show.Pretty
import System.Exit
import System.Console.ParseArgs

import Bowser.Parser
import Bowser.Types
import Bowser.Engine.Interp

data Options = Filename
             | Threshold
             | VerboseFlag
             deriving (Ord, Eq, Show)

argd :: [ Arg Options ]
argd = [ Arg { argIndex = Filename
             , argName = Just "file"
             , argAbbr = Just 'f'
             , argData = argDataRequired "filename" ArgtypeString
             , argDesc = "file to run" }
       , Arg { argIndex = Threshold
             , argName = Just "threshold"
             , argAbbr = Just 't'
             , argData = argDataOptional "threshold" ArgtypeInt
             , argDesc = "step threshold for execution" }
       , Arg { argIndex = VerboseFlag
             , argName = Just "verbose"
             , argAbbr = Just 'v'
             , argData = Nothing
             , argDesc = "verbose flag" }
       ]

main :: IO ()
main = do
  args <- parseArgsIO
          (ArgsParseControl ArgsComplete ArgsHardDash)
          argd
  verbose <- return $ gotArg args VerboseFlag

  ast <- parseFile (fromJust (getArg args Filename))

  when verbose $ do
    putStrLn "ast:"
    pPrint ast
    putStrLn ""

  when verbose $ putStrLn "running:"
  (res, state) <- eval ast (liftM toInteger ((getArg args Threshold)::Maybe Int))

  when verbose $ do
    putStrLn ""
    putStrLn "return:"
    pPrint res
    putStrLn ""
    putStrLn "state:"
    pPrint state

  case res of
    Left err -> die . show $ err
    Right x -> case x of
      JSNumber n | n == 0 -> exitSuccess
      JSNumber n -> exitWith . ExitFailure . floor $ n
      _ -> exitSuccess
