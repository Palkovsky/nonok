module ExpressionTests
   (expressions)
   where

import Lib

import Test.Tasty
import Test.Tasty.HUnit
import Assertions

import qualified Data.Map.Strict as M

-- Really have to add some king of auto type detector for variable passed.
people :: Expression
people = ListExpression $ map (MapExpression . M.fromList) $
    [ [("name", LiteralExpression $ LitString $ "Mirek"), ("age", LiteralExpression $ LitInteger $ 20)]
    , [("name", LiteralExpression $ LitString $ "Michal"), ("age", LiteralExpression $ LitInteger $ 50)]
    , [("name", LiteralExpression $ LitString $ "Dawid"), ("age", LiteralExpression $ LitInteger $ 12)]
    , [("name", LiteralExpression $ LitString $ "Andrzej"), ("age", LiteralExpression $ LitInteger $ 42)]]

expressions :: TestTree
expressions = testGroup "Expressions"
     [ mapWithVariableFields
     , mapWithGlobalVariableFields
     , includeWithMemberCallInGlobals
     , includeWithMapByReference ]

mapWithVariableFields :: TestTree
mapWithVariableFields = testCase "Map with variable fields"
 (assertEqualIO "Should render valid output"
    (return $ Right "Name: David, age: 20")
    (feed M.empty "{{let $n='David', $a=20, $person={'name':$n, 'age':$a}}}Name: {- $person.name }}, age: {- $person.age }}")
 )

mapWithGlobalVariableFields :: TestTree
mapWithGlobalVariableFields = testCase "Map with global variable fields"
  (assertEqualIO "Should render valid output"
    (return $ Right "Name: David, age: 30")
    (feed (M.fromList [("age", LiteralExpression $ LitInteger 30)])
     "{{let $age=21, $n='David', $person={'name' : $n, 'age' : @age}}}Name: {- $person.name }}, age: {- $person.age }}")
  )

includeWithMemberCallInGlobals :: TestTree
includeWithMemberCallInGlobals = testCase "Passing complicated expressions as map fields in include"
 (assertEqualIO "Should render valid output"
    (return $ Right "dawid")
    (feed M.empty
     "{{let $map={'x' : 'dawid', 'y' : 'andrzej'}, $i='{-@person.name}}'}}{{include $i, {'person' : {'name':$map.x }} }}")
 )

includeWithMapByReference :: TestTree
includeWithMapByReference = testCase "Passing reference to map in include"
   (assertEqualIO "Should render valid output"
      (return $ Right "dawid")
      (feed M.empty
       "{{let $map=  {   'person' :{ 'name':'dawid'}}, $i='{-@person.name}}'}}{{include $i, $map }}")
   )
