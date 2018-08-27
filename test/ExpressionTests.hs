{-# LANGUAGE OverloadedStrings #-}
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
    [ [(("name" :: String), express ("Mirek" :: String)), (("age"::String), expressInt 20)]
    , [(("name" :: String), express ("Michal" :: String)), (("age"::String), expressInt 50)]
    , [(("name" :: String), express ("Dawid" :: String)), (("age"::String), expressInt 12)]
    , [(("name" :: String), express ("Andrzej" :: String)), (("age"::String), expressInt 42)]]

peopleLookup :: VariableLookup
peopleLookup = M.fromList [("people", people)]

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
    (feed M.empty "{{let $n='David', $a=20, $person={'name':$n, 'age':$a}}}Name: {- $person.name }}, age: {- $person.age }}")
 )

mapWithGlobalVariableFields :: TestTree
mapWithGlobalVariableFields = testCase ("Map with global variable fields" :: String)
  (assertEqualIO ("Should render valid output" :: String)
    (return $ Right "Name: David, age: 30")
    (feed (M.fromList [("age", expressInt 30)])
     "{{let $age=21, $n='David', $person={'name' : $n, 'age' : @age}}}Name: {- $person.name }}, age: {- $person.age }}")
  )

includeWithMemberCallInGlobals :: TestTree
includeWithMemberCallInGlobals = testCase ("Passing complicated expressions as map fields in include"  :: String)
 (assertEqualIO ("Should render valid output" :: String)
    (return $ Right "dawid")
    (feed M.empty
     "{{let $map={'x' : 'dawid', 'y' : 'andrzej'}, $i='{-@person.name}}'}}{{include $i, {'person' : {'name':$map.x }} }}")
 )

includeWithMapByReference :: TestTree
includeWithMapByReference = testCase ("Passing reference to map in include" :: String)
   (assertEqualIO ("Should render valid output" :: String)
      (return $ Right "dawid")
      (feed M.empty
       "{{let $map=  {   'person' :{ 'name':'dawid'}}, $i='{-@person.name}}'}}{{include $i, $map }}")
   )


printingUppercasedPeopleNames :: TestTree
printingUppercasedPeopleNames = testCase "Passing map members arguments to function"
  (assertEqualIO "Should render valid output"
     (return $ Right "MIREK MICHAL DAWID ANDRZEJ ")
     (feed peopleLookup "{{ for $person in @people }}{- upper($person.name) }} {{ endfor }}")
  )
