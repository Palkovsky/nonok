module Main where

import Lib
import Test.Tasty
import Test.Tasty.HUnit

import StructureTagsTests ( forLoops, ifStatements, declarations
                          , includes, callTag, comments, raw)
import RenderTests (renderForLoop, renderDeclarations)

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [ forLoops, ifStatements, declarations
                          , includes, callTag, comments, raw
                          , renderForLoop, renderDeclarations]
