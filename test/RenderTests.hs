module RenderTests
   (renderForLoop, renderDeclarations)
   where

import Lib

import Test.Tasty
import Test.Tasty.HUnit
import Assertions


renderForLoop :: TestTree
renderForLoop = testGroup "Rendering for loops"
     [ renderSingleForLoop
     , renderNestedForLoop
     , renderForLoopWithReferenceIterator
     , renderForWithOutsideReferenceDeclaration]

renderSingleForLoop :: TestTree
renderSingleForLoop = testCase "Single for loop"
   (assertEqual "Should render valid output"
      (Right " Numer: 1  Numer: 2  Numer: 3 ")
      (feed "{{for $i in [1,2,3]}} Numer: {- $i}} {{endfor}}")
   )

renderNestedForLoop :: TestTree
renderNestedForLoop = testCase "Two nested for loops"
   (assertEqual "Should render valid output"
      (Right "(1, a)(1, b)(1, c)(2, a)(2, b)(2, c)(3, a)(3, b)(3, c)")
      (feed "{{for $i in [1,2,3]}}{{for $j in 'abc'}}({-$i}}, {-$j}}){{endfor}}{{endfor}}")
   )

renderForLoopWithReferenceIterator :: TestTree
renderForLoopWithReferenceIterator = testCase "For loop with referenced iterator"
  (assertEqual "Should render valid output"
     (Right "abcdefghi")
     (feed "{{for $i in [['abc'],['def'],['ghi']]}}{{for $j in $i}}{-$j}}{{endfor}}{{endfor}}")
  )

renderForWithOutsideReferenceDeclaration :: TestTree
renderForWithOutsideReferenceDeclaration = testCase "For loop with referenced iterator declared in let"
  (assertEqual "Should render valid output"
     (Right "abcdefghi")
     (feed "{{let $arr=[['abc'],['def'],['ghi']]}}{{for $i in $arr}}{{for $j in $i}}{-$j}}{{endfor}}{{endfor}}")
  )

renderDeclarations :: TestTree
renderDeclarations = testGroup "Rendering declarations"
     [ renderAccessToUndefinedVar
     , renderOutOfScopeAccess]

renderAccessToUndefinedVar :: TestTree
renderAccessToUndefinedVar = testCase "Error out if accessed undefined var"
 (assertEither "Should error out"
    left
    (feed "{{for $i in $k}} dawid {{endfor}}")
 )

renderOutOfScopeAccess :: TestTree
renderOutOfScopeAccess = testCase "Error out if accessed out of scope var"
  (assertEither "Should error out"
     left
     (feed "{{for $i in [['abc'],['def'],['ghi']]}} {{let $k=20}} {{endfor}} {- $k }}")
  )
