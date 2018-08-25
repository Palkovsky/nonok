module ExpressionTests
   (expressions)
   where

import Text.Nonok

import Test.Tasty
import Test.Tasty.HUnit
import Assertions

import Text.Nonok.Expressible
import qualified Data.Map.Strict as M

-- Really have to add some king of auto type detector for variable passed.
people :: Expression
people = express $ map (express . M.fromList) $
    [ [("name", express "Mirek"), ("age", expressInt 20)]
    , [("name", express "Michal"), ("age", expressInt 50)]
    , [("name", express "Dawid"), ("age", expressInt 12)]
    , [("name", express "Andrzej"), ("age", expressInt 42)]]

peopleLookup :: VariableLookup
peopleLookup = M.fromList [("people", people)]

expressions :: TestTree
expressions = testGroup "Expressions"
     [ mapWithVariableFields
     , mapWithGlobalVariableFields
     , includeWithMemberCallInGlobals
     , includeWithMapByReference
     , parsingNestedFunctions
     , printingUppercasedPeopleNames]

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
    (feed (M.fromList [("age", expressInt 30)])
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

parsingNestedFunctions :: TestTree
parsingNestedFunctions = testCase "Passing reference to map in include"
  (assertEqual "Should be parsed to valid AST"
     (Right $ [CallPiece $ FuncExpression "f1"
         [FuncExpression "f2" [express "x"], expressInt 12]])
     (generateAST "{- f1(f2('x'), 12) }}")
  )

printingUppercasedPeopleNames :: TestTree
printingUppercasedPeopleNames = testCase "Passing map members arguments to function"
  (assertEqualIO "Should render valid output"
     (return $ Right "MIREK MICHAL DAWID ANDRZEJ ")
     (feed peopleLookup "{{ for $person in @people }}{- upper($person.name) }} {{ endfor }}")
  )
