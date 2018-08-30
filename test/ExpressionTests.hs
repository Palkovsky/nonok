{-# LANGUAGE OverloadedStrings #-}
module ExpressionTests
   (expressions)
   where

import Text.Nonok

import Test.Tasty
import Test.Tasty.HUnit
import Assertions

import qualified Data.Map.Strict as M


-- Really have to add some king of auto type detector for variable passed.
genPerson :: String -> Integer -> VariableLookup
genPerson name age = buildVarLookup $ do
    addVar "name" name
    addVar "age" age

genPeople :: [(String, Integer)] -> VariableLookup
genPeople list = buildVarLookup $ addVar "people" $ map (\(name, age) -> genPerson name age) list

testState :: RenderState
testState = initialRenderState $ genPeople [("Mirek", 42), ("Michal", 50), ("Dawid", 12), ("Andrzej", 20)]

expressions :: TestTree
expressions = testGroup "Expressions"
     [ mapWithVariableFields
     , mapWithGlobalVariableFields
     , includeWithMemberCallInGlobals
     , includeWithMapByReference
     , printingUppercasedPeopleNames]

mapWithVariableFields :: TestTree
mapWithVariableFields = testCase ("Map with variable fields" :: String)
 (assertEqualIO ("Should render valid output" :: String)
    (return $ Right "Name: David, age: 20")
    (feed defaultRenderState "{{let $n='David', $a=20, $person={'name':$n, 'age':$a}}}Name: {- $person.name }}, age: {- $person.age }}")
 )

mapWithGlobalVariableFields :: TestTree
mapWithGlobalVariableFields = testCase ("Map with global variable fields" :: String)
  (assertEqualIO ("Should render valid output" :: String)
    (return $ Right "Name: David, age: 30")
    (feed (initialRenderState $ M.fromList [("age", express (30 :: Integer))])
     "{{let $age=21, $n='David', $person={'name' : $n, 'age' : @age}}}Name: {- $person.name }}, age: {- $person.age }}")
  )

includeWithMemberCallInGlobals :: TestTree
includeWithMemberCallInGlobals = testCase ("Passing complicated expressions as map fields in include"  :: String)
 (assertEqualIO ("Should render valid output" :: String)
    (return $ Right "dawid")
    (feed defaultRenderState
     "{{let $map={'x' : 'dawid', 'y' : 'andrzej'}, $i='{-@person.name}}'}}{{include $i, {'person' : {'name':$map.x }} }}")
 )

includeWithMapByReference :: TestTree
includeWithMapByReference = testCase ("Passing reference to map in include" :: String)
   (assertEqualIO ("Should render valid output" :: String)
      (return $ Right "dawid")
      (feed defaultRenderState
       "{{let $map=  {   'person' :{ 'name':'dawid'}}, $i='{-@person.name}}'}}{{include $i, $map }}")
   )


printingUppercasedPeopleNames :: TestTree
printingUppercasedPeopleNames = testCase "Passing map members arguments to function"
  (assertEqualIO "Should render valid output"
     (return $ Right "MIREK MICHAL DAWID ANDRZEJ ")
     (feed testState "{{ for $person in @people }}{- upper($person.name) }} {{ endfor }}")
  )
