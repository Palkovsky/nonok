module StructureTagsTests
   (forLoops, ifStatements, declarations, includes, callTag, blocks, comments, raw)
   where

import Text.Nonok

import Test.Tasty
import Test.Tasty.HUnit
import Assertions

import qualified Data.Map.Strict as M

{-
  ------------------------- FOR LOOPS
-}

forLoops :: TestTree
forLoops = testGroup "For loops"
     [ singleForLoop
     , twoNestedForLoops
     , forLoopWithoutEndfor
     , forLoopIteratorWithoutDollarSign]

singleForLoop :: TestTree
singleForLoop = testCase "Simple for without nesting"
   (assertEqual "Should parse simple for loop to valid AST"
      (Right [ForPiece "i" (express "xxx") [StaticPiece " static "]])
      (generateAST "{{for $i in 'xxx'}} static {{endfor}}"))

twoNestedForLoops :: TestTree
twoNestedForLoops = testCase "Two nested for loops"
   (assertEqual "Should parse two nested for loop to valid AST"
      (Right
          [ForPiece "i" (express "xxx")
             [StaticPiece " ", ForPiece "j" (express "yyy") [], StaticPiece " "]
          ])
      (generateAST "{{for $i in 'xxx'}} {{for $j in 'yyy'}}{{endfor}} {{endfor}}"))

forLoopWithoutEndfor :: TestTree
forLoopWithoutEndfor = testCase "Forloop without 'endfor'"
   (assertEither "Should return error."
      left
      (generateAST "{{for $i in 'xxx'}} {{for $j in 'yyy'}} {{endfor}}"))

forLoopIteratorWithoutDollarSign :: TestTree
forLoopIteratorWithoutDollarSign = testCase "Forloop with iterator without dollarsign."
   (assertEither "Should return error."
      left
      (generateAST "{{for $i in 'xxx'}} {{for j in 'yyy'}} {{endfor}} {{endfor}}"))

{-
  ------------------------- IF STATEMENTS
-}


ifStatements :: TestTree
ifStatements = testGroup "If statements"
    [ ifWithoutElifsAndElse
    , ifWithElse
    , ifWithMultipleElifs
    , elifOrElseWithoutIf
    , multipleNestedIfs
    , ifWithMutipleElses]

ifWithoutElifsAndElse :: TestTree
ifWithoutElifsAndElse = testCase "Basic if without elifs and else"
   (assertEqual "Should parse simple if statement to valid AST"
      (Right [IfPiece [express "expr"] [[StaticPiece " wololo"]]])
      (generateAST "{{if   'expr'}} wololo{{endif}}"))

ifWithElse :: TestTree
ifWithElse = testCase "Basic if statement with else"
   (assertEqual "Should parse if with else to valid AST"
      (Right [
           IfPiece [express "expr", express True]
           [ [StaticPiece " wololo"]
           , [ForPiece "i" (express "ppp") [StaticPiece "wnetrze"]]
           ]])
      (generateAST "{{if   'expr'}} wololo{{else}}{{for $i in 'ppp'}}wnetrze{{endfor}}{{endif}}"))


ifWithMultipleElifs :: TestTree
ifWithMultipleElifs = testCase "If statement with more than one elif"
   (assertEqual "Should be parsed to valid AST"
      (Right
          [IfPiece [ express "expr1"
                   , express "expr2"
                   , express "expr3"
                   , express True]
                   [ [StaticPiece " first block "]
                   , [StaticPiece " second "]
                   , [StaticPiece " third "]
                   , [StaticPiece " else "]]])
      (generateAST "{{if 'expr1'}} first block {{elif 'expr2'}} second {{elif 'expr3'}} third {{else}} else {{endif}}"))


multipleNestedIfs :: TestTree
multipleNestedIfs = testCase "If statement with more than one elif"
   (assertEqual "Should parse to valid AST"
     (Right
         [IfPiece
             [ express "expr1"
             , express "expr2"
             , express True]
             [
                 [ IfPiece [LiteralExpression (LitString "expr1-nest")]
                      [ [ForPiece "i" (express "wolllo")
                            [ Decl [("j", express "xxx")]
                            , StaticPiece "nested for"]]
                      ]
                 ]
                 , [ IfPiece
                      [ express "expr2-nest"
                      , express "expr2-nest-2"
                      , express "expr2-nest-3"]
                      [[StaticPiece " one "],[StaticPiece ""],[StaticPiece " three "]]
                  ]
                , [StaticPiece "else"]
              ]
          ])
      (generateAST "{{if 'expr1'}}\
          \{{if 'expr1-nest'}}\
          \{{for $i in 'wolllo'}}\
          \{{let $j='xxx'}}\
          \nested for\
          \{{endfor}}\
          \{{endif}}\
          \{{elif 'expr2'}}\
          \{{if 'expr2-nest'}} one {{elif 'expr2-nest-2'}}{{elif 'expr2-nest-3'}} three {{endif}}\
          \{{else}}\
          \else\
          \{{endif}}"))


elifOrElseWithoutIf :: TestTree
elifOrElseWithoutIf = testCase "Elif or else without proceeding if"
   (assertEither "Should fail"
      left
      (generateAST "{{if 'expr1'}} first block  {{endif}} {{elif 'expr2'}} second {{else}} else {{endif}}"))

ifWithMutipleElses :: TestTree
ifWithMutipleElses = testCase "If statement with more than one else"
   (assertEither "Should fail"
      left
      (generateAST "{{if 'expr1'}} first block {{else}} second {{elif 'expr3'}} third {{else}} else {{endif}}"))

{-
  ------------------------- DECLARATIONS
-}

declarations :: TestTree
declarations = testGroup "Declarations" [singleDeclaration, multipleDeclaration, noDollarSignDeclaration]

singleDeclaration :: TestTree
singleDeclaration = testCase "Single declaration"
   (assertEqual "Should parse simple declaration to valid AST"
      (Right [StaticPiece "warszawa ",Decl [("name", express "dawid")],StaticPiece " legia"])
      (generateAST "warszawa {{let        $name='dawid'}} legia")
   )


multipleDeclaration :: TestTree
multipleDeclaration = testCase "Multiple declaration"
   (assertEqual "Should parse multiple declaration to valid AST"
      (Right [ Decl [("name", express "dawid"),("lastname", express "Palkovksy")]
             , StaticPiece "staticcontent"])
      (generateAST "{{let $name   =  'dawid',   $lastname='Palkovksy'}}staticcontent")
   )

noDollarSignDeclaration :: TestTree
noDollarSignDeclaration = testCase "Declaration without dollar sign by variable name"
   (assertEither "Should error out, due to lack of dollar sign by var name"
      left
      (generateAST "static   {{let $name='dawid', lastname='Palkovksy'}}staticcontent")
  )

{-
  ------------------------- INCLUDES
-}
includes :: TestTree
includes = testGroup "Includes"
    [ referenceInclude
    , pathInclude
    , referenceIncludeWithNewGlobals
    , pathIncludeWithNewGlobals]

referenceInclude :: TestTree
referenceInclude = testCase "Reference include"
   (assertEqual "Should parse include tag"
      (Right [StaticPiece " ", IncludeRefPiece (RefLocal "k") Nothing, StaticPiece " andrzej  "])
      (generateAST " {{include $k}} andrzej  ")
   )

pathInclude :: TestTree
pathInclude = testCase "Path include"
  (assertEqual "Should parse include tag"
     (Right [StaticPiece " ", IncludePathPiece "folder/file.html" Nothing, StaticPiece " andrzej  "])
     (generateAST " {{include 'folder/file.html'}} andrzej  ")
  )

referenceIncludeWithNewGlobals :: TestTree
referenceIncludeWithNewGlobals = testCase "Reference include with new globals"
   (assertEqual "Should parse include tag"
      (Right
          [IncludeRefPiece (RefLocal "k")
              (Just $ MapExpression $ M.fromList [("name", express "andrzej")])])
      (generateAST "{{include $k , {'name' : 'andrzej'} }}")
   )

pathIncludeWithNewGlobals :: TestTree
pathIncludeWithNewGlobals = testCase "Path include with new globals"
  (assertEqual "Should parse include tag"
     (Right
         [IncludePathPiece "folder/file.html"
             (Just $ express $ M.fromList
                 [("person", express $ M.fromList $ [("name", express "andrzej")])])]
     )
     (generateAST "{{include 'folder/file.html', {'person' : {'name' : 'andrzej'} } }}")
  )


{-
  ------------------------- CALL TAG
-}
callTag :: TestTree
callTag = testGroup "Call tag" [callAfterLet, parsingNestedFunctions, callInFor, callForMapMember, callForGlobalVar]

callInFor :: TestTree
callInFor = testCase "Call in for"
   (assertEqual "Should be parsed to valid AST"
      (Right [
          ForPiece "i"
              (express [ expressInt 1
                       , expressInt 2
                       , expressInt 3])
              [StaticPiece " ", CallPiece (LiteralExpression $ LitRef $ RefLocal "i"), StaticPiece " "]])
      (generateAST "{{for $i in [1,2,3]}} {- $i}} {{endfor}}")
   )

parsingNestedFunctions :: TestTree
parsingNestedFunctions = testCase "Passing reference to map in include"
 (assertEqual "Should be parsed to valid AST"
    (Right $ [CallPiece $ FuncExpression "f1"
        [FuncExpression "f2" [express "x"], expressInt 12]])
    (generateAST "{- f1(f2('x'), 12) }}")
 )


callAfterLet :: TestTree
callAfterLet = testCase "Call after let"
  (assertEqual "Should be parsed to valid AST"
     (Right [ Decl [("i", expressFloat 32.4),("j", express "xxx")]
            , StaticPiece " I'm ", CallPiece (LiteralExpression $ LitRef $ RefLocal "i"), StaticPiece " years old. "])
     (generateAST "{{let $i=32.4, $j='xxx'}} I'm {- $i}} years old. ")
  )

callForMapMember :: TestTree
callForMapMember = testCase "Call for map member"
  (assertEqual "Should be parsed to valid AST"
     (Right
         [ Decl [("m", express $ M.fromList
             [ ("age", expressInt 21), ("name", express "dawid")
             , ("pet", express $ M.fromList [("name", express "Azor")])])]
         , StaticPiece " "
         , CallPiece (MapMemberExpression (RefLocal "m") ["pet","name"])])
     (generateAST "{{let $m={'name':'dawid', 'age':21, 'pet':{'name':'Azor'}}}} {-$m.pet.name}}")
  )

callForGlobalVar :: TestTree
callForGlobalVar = testCase "Call for global variable"
  (assertEqual "Should be parsed to valid AST"
     (Right [ Decl [("i", expressFloat 32.4),("j", express "xxx")]
            , StaticPiece " I'm ", CallPiece (MapMemberExpression (RefGlobal "person") ["name"]), StaticPiece " years old. "])
     (generateAST "{{let $i=32.4, $j='xxx'}} I'm {- @person.name }} years old. ")
  )

{-
  ------------------------- BLOCKS
-}

blocks :: TestTree
blocks = testGroup "Blocks" [simpleBlocksAndExtends]

simpleBlocksAndExtends :: TestTree
simpleBlocksAndExtends = testCase "Simple block and extends"
    (assertEqual "Should be parsed to valid AST"
        (Right
          [ ExtendsPiece "base.html", StaticPiece " Web Page "
          , BlockPiece "title" [StaticPiece " My blog "], StaticPiece " "])
        (generateAST "  {{ extends 'base.html' }} Web Page {{ block title }} My blog {{endblock}} ")
    )

{-
  ------------------------- COMMENTS
-}
comments :: TestTree
comments = testGroup "Comments" [simpleComment]

simpleComment :: TestTree
simpleComment = testCase "Simple comment"
   (assertEqual "Should ignore contents of comment tag"
      (Right [CommentPiece])
      (generateAST "{{comment}} {{let $name   =  'dawid',   $lastname='Palkovksy'}}staticcontent {{endcomment}}")
   )

{-
  ------------------------- RAW TAG
-}
raw :: TestTree
raw = testGroup "Raw Tag" [rawWithStructuralTagsInside]

rawWithStructuralTagsInside :: TestTree
rawWithStructuralTagsInside = testCase "Raw tag with structural tags inside"
   (assertEqual "Should treat structural tags as static content"
      (Right [RawPiece " {{let $name   =  'dawid',   $lastname='Palkovksy'}}staticcontent "])
      (generateAST "{{raw}} {{let $name   =  'dawid',   $lastname='Palkovksy'}}staticcontent {{endraw}}")
   )
