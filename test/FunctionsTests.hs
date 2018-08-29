{-# LANGUAGE OverloadedStrings #-}
module FunctionsTests
   (functionTests)
   where

import Text.Nonok

import Test.Tasty
import Test.Tasty.HUnit
import Assertions

import qualified Data.Map.Strict as M


functionTests :: TestTree
functionTests = testGroup "Functions"
     [ boolFunctionsTest]

boolFunctionsTest :: TestTree
boolFunctionsTest = testCase ("AND and OR functions test" :: String)
 (assertEqualIO ("Should render valid output" :: String)
    (return $ Right " second ")
    (feed defaultRenderState "{{ if and('xxx', []) }} first {{ elif or([1, 'xxx'], '') }} second {{ else }} third {{ endif }}")
 )
