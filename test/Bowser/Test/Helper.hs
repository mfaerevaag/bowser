module Bowser.Test.Helper where

import Test.Tasty
import Test.Tasty.HUnit
import Control.Exception
import Control.Monad

import Bowser.Parser
import Bowser.Engine.Interp

t e s = do
  (res, _) <- eval (parseString s) (Just 1000)
  case res of
    Left e -> fail . show $ e
    Right x -> x @?= e

terr ex s = do
  (res, _) <- eval (parseString s) (Just 1000)
  case res of
    Left x -> x @?= ex
    Right e -> assertFailure $ "Expected exception: " ++ show ex

assertException :: (Exception e, Eq e) => e -> IO a -> IO ()
assertException ex action =
    handleJust isWanted (const $ return ()) $ do
        action
        assertFailure $ "Expected exception: " ++ show ex
  where isWanted = guard . (== ex)
