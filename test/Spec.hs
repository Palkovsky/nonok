module Main where

import Text.Nonok
import Test.Tasty
import Test.Tasty.HUnit

import StructureTagsTests ( forLoops, ifStatements, declarations
                          , includes, callTag, blocks, comments, raw)
import RenderTests ( renderForLoop, renderIfStatement, renderDeclarations
                   , renderIncludes, renderRawAndComment)
import ExpressionTests (expressions)

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [ forLoops, ifStatements, declarations
                          , includes, callTag, comments, raw
                          , renderForLoop, renderDeclarations
                          , renderIncludes, renderRawAndComment
                          , expressions, renderIfStatement
                          , blocks]
