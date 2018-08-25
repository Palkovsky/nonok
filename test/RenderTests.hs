module RenderTests
   (renderForLoop, renderIfStatement, renderDeclarations, renderIncludes, renderRawAndComment)
   where

import Lib

import Test.Tasty
import Test.Tasty.HUnit
import Assertions

import Templating.Expressible
import qualified Data.Map.Strict as M

renderForLoop :: TestTree
renderForLoop = testGroup "Rendering for loops"
     [ renderSingleForLoop
     , renderNestedForLoop
     , renderForLoopWithReferenceIterator
     , renderForWithOutsideReferenceDeclaration
     , loopThroughPeopleList
     , loopThroughFunctionResult]

renderSingleForLoop :: TestTree
renderSingleForLoop = testCase "Single for loop"
   (assertEqualIO  "Should render valid output"
      (return $ Right " Numer: 1  Numer: 2  Numer: 3 ")
      (feed M.empty "{{for $i in [1,2,3]}} Numer: {- $i}} {{endfor}}")
   )

renderNestedForLoop :: TestTree
renderNestedForLoop = testCase "Two nested for loops"
   (assertEqualIO "Should render valid output"
      (return $ Right "(1, a)(1, b)(1, c)(2, a)(2, b)(2, c)(3, a)(3, b)(3, c)")
      (feed M.empty "{{for $i in [1,2,3]}}{{for $j in 'abc'}}({-$i}}, {-$j}}){{endfor}}{{endfor}}")
   )

renderForLoopWithReferenceIterator :: TestTree
renderForLoopWithReferenceIterator = testCase "For loop with referenced iterator"
  (assertEqualIO "Should render valid output"
     (return $ Right "abcdefghi")
     (feed M.empty "{{for $i in [['abc'],['def'],['ghi']]}}{{for $j in $i}}{-$j}}{{endfor}}{{endfor}}")
  )

renderForWithOutsideReferenceDeclaration :: TestTree
renderForWithOutsideReferenceDeclaration = testCase "For loop with referenced iterator declared in let"
  (assertEqualIO "Should render valid output"
     (return $ Right "abcdefghi")
     (feed M.empty "{{let $arr=[['abc'],['def'],['ghi']]}}{{for $i in $arr}}{{for $j in $i}}{-$j}}{{endfor}}{{endfor}}")
  )

loopThroughPeopleList :: TestTree
loopThroughPeopleList = testCase "Loops throug list of maps representing people"
  (assertEqualIO "Should render valid output"
     (return $ Right "Name: andrzej Age: 18\nName: piotr Age: 20\nName: frank Age: 30\n")
     (feed M.empty
         "{{for $person in [{'name' : 'andrzej', 'age':18}, {'name' : 'piotr', 'age':20}, {'name':'frank', 'age':30}]}}\
         \Name: {-$person.name}} \
         \Age: {-$person.age}}\n\
         \{{endfor}}")
  )

loopThroughFunctionResult :: TestTree
loopThroughFunctionResult = testCase "For loop through function result"
  (assertEqualIO "Should render valid output"
     (return $ Right "D A W I D ")
     (feed M.empty "{{ let $name='dawid' }}{{ for $char in toUpperCase($name)}}{-$char}} {{endfor}}")
  )

renderDeclarations :: TestTree
renderDeclarations = testGroup "Rendering declarations"
     [ renderAccessToUndefinedVar
     , renderOutOfScopeAccess]

renderAccessToUndefinedVar :: TestTree
renderAccessToUndefinedVar = testCase "Error out if accessed undefined var"
 (assertEitherIO "Should error out"
    (return left)
    (feed M.empty "{{for $i in $k}} dawid {{endfor}}")
 )

renderOutOfScopeAccess :: TestTree
renderOutOfScopeAccess = testCase "Error out if accessed out of scope var"
  (assertEitherIO "Should error out"
     (return left)
     (feed M.empty "{{for $i in [['abc'],['def'],['ghi']]}} {{let $k=20}} {{endfor}} {- $k }}")
  )

renderIfStatement :: TestTree
renderIfStatement = testGroup "Rendering if statements"
     [ renderIfStatementWithFunctionResult
     , renderIfInForLoop]

renderIfStatementWithFunctionResult :: TestTree
renderIfStatementWithFunctionResult = testCase "If statement with function call as bool"
    (assertEqualIO "Should render valid output"
        (return $ Right "not empty")
        (feed M.empty "{{ let $name='dawid' }}{{ if toUpperCase($name) }}not empty{{endif}}")
    )

renderIfInForLoop :: TestTree
renderIfInForLoop = testCase "If statement inside for loop"
    (assertEqualIO "Should render valid output"
        (return $ Right "JESTEM D JESTEM A NIEWIEMKIMJESTEM NIEWIEMKIMJESTEM JESTEM D ")
        (feed M.empty "{{ for $char in 'dawid' }}{{ if equal($char, 'd') }}JESTEM D {{elif equal($char, 'a')}}JESTEM A {{else}}NIEWIEMKIMJESTEM {{endif}}{{endfor}}")
    )

renderIncludes :: TestTree
renderIncludes = testGroup "Rendering includes"
     [ renderIncludeFromVar
     , renderIncludeFromPath
     , renderIncludeFromVarWithError
     , renderIncludeFromPathWithError
     , renderIncludeLocalInheritanceError
     , renderIncludeGlobalInheritance
     , renderIncludeRefWithOverwrittenGlobal
     , renderIncludePathWithOverwrittenGlobal]

renderIncludeFromVar :: TestTree
renderIncludeFromVar = testCase "Include from var"
 (assertEqualIO "Should render valid output"
    (return $ Right "123")
    (feed M.empty "{{let $i='{{for $i in [1,2,3]}}{-$i}}{{endfor}}'}}{{include $i}}")
 )

renderIncludeFromPath :: TestTree
renderIncludeFromPath = testCase "Include from path"
 (assertEqualIO "Should render valid output"
    (return $ Right "123\n")
    (feed M.empty "{{include 'test/static/include_test_correct.txt'}}")
 )

renderIncludeLocalInheritanceError :: TestTree
renderIncludeLocalInheritanceError = testCase "Inheriting local variables"
  (assertEitherIO "Should error out"
     (return left)
     (feed M.empty "{{let $j=21, $i='{-$j}}'}}{{include $i}}")
  )

renderIncludeGlobalInheritance :: TestTree
renderIncludeGlobalInheritance = testCase "Inheriting global variables in includes"
  (assertEqualIO "Should render valid output"
     (return $ Right "andrzej inner: andrzej")
     (feed (M.fromList [("person", express $ M.fromList
         [("name", express "andrzej")])])
      "{{let $i='{-@person.name}}'}}{{include $i}} inner: {-@person.name}}")
  )

renderIncludeFromVarWithError :: TestTree
renderIncludeFromVarWithError = testCase "Include from var with error"
  (assertEitherIO "Should error out"
     (return left)
     (feed M.empty "{{let $i='{{for $i in [1,2,3]}}{-$i}}'}}{{include $i}}")
  )

renderIncludeFromPathWithError :: TestTree
renderIncludeFromPathWithError = testCase "Include from path with error"
 (assertEitherIO "Should error out"
    (return left)
    (feed M.empty "{{include 'test/static/include_test_error.txt'}}")
 )

renderIncludeRefWithOverwrittenGlobal :: TestTree
renderIncludeRefWithOverwrittenGlobal = testCase "Overriding old global in ref include"
 (assertEqualIO "Should render valid output"
    (return $ Right "dawid inner: andrzej")
    (feed (M.fromList [("person", express $ M.fromList
        [("name", express "andrzej")])])
     "{{let $i='{-@person.name}}'}}{{include $i, {'person' : {'name':'dawid'}} }} inner: {-@person.name}}")
 )

renderIncludePathWithOverwrittenGlobal :: TestTree
renderIncludePathWithOverwrittenGlobal = testCase "Overriding old global in path include"
    (assertEqualIO "Should render valid output"
        (return $ Right "dawid\n inner: andrzej")
        (feed (M.fromList [("person", express $ M.fromList
            [("name", express "andrzej")])])
        "{{include 'test/static/include_test_override.txt', {'person' : {'name':'dawid'}} }} inner: {-@person.name}}")
    )

renderRawAndComment :: TestTree
renderRawAndComment = testGroup "Rendering raw and comments"
     [ renderRaw
     , renderComment]

renderRaw :: TestTree
renderRaw = testCase "Rendering contents of raw tag"
    (assertEqualIO "Should render valid output"
         (return $ Right " {{if $x}}xxx{{endif}} ")
    (feed M.empty " {{raw }}{{if $x}}xxx{{endif}}{{endraw }} ")
    )

renderComment :: TestTree
renderComment = testCase "Skipping contents of comment tag"
    (assertEqualIO "Should render valid output"
         (return $ Right "  ")
    (feed M.empty " {{ comment }}  {   {i  f $x }}x xx{ { e n dif}}{{ endcomment }} ")
    )
