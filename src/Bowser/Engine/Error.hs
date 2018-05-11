module Bowser.Engine.Error where

data Error = EType String
           | ERef String
           | ESyntax String
           | EStep Integer
           | ETodo String
           deriving Eq

instance Show Error where
  show (EType s) = "TypeError: " ++ s
  show (ERef s) = "ReferenceError: " ++ s
  show (ESyntax s) = "SyntaxError: " ++ s
  show (EStep s) = "StepError: step threshold reached (" ++ show s ++ ")"
  show (ETodo s) = "NotImplementedError: " ++ s
