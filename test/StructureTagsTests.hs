module StructureTagsTests
   (forLoops, ifStatements, declarations, includes, callTag, comments, raw)
   where

import Lib

import Test.Tasty
import Test.Tasty.HUnit
import Assertions

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
      (Right [ForPiece "i" (LiteralExpression (LitString "xxx")) [StaticPiece " static "]])
      (parseAll <^> "{{for $i in 'xxx'}} static {{endfor}}"))

twoNestedForLoops :: TestTree
twoNestedForLoops = testCase "Two nested for loops"
   (assertEqual "Should parse two nested for loop to valid AST"
      (Right
          [ForPiece "i" (LiteralExpression (LitString "xxx"))
             [StaticPiece " ", ForPiece "j" (LiteralExpression (LitString "yyy")) [], StaticPiece " "]
          ])
      (parseAll <^> "{{for $i in 'xxx'}} {{for $j in 'yyy'}}{{endfor}} {{endfor}}"))

forLoopWithoutEndfor :: TestTree
forLoopWithoutEndfor = testCase "Forloop without 'endfor'"
   (assertEither "Should return error."
      left
      (parseAll <^> "{{for $i in 'xxx'}} {{for $j in 'yyy'}} {{endfor}}"))

forLoopIteratorWithoutDollarSign :: TestTree
forLoopIteratorWithoutDollarSign = testCase "Forloop with iterator without dollarsign."
   (assertEither "Should return error."
      left
      (parseAll <^> "{{for $i in 'xxx'}} {{for j in 'yyy'}} {{endfor}} {{endfor}}"))

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
      (Right [IfPiece [LiteralExpression (LitString "expr")] [[StaticPiece " wololo"]]])
      (parseAll <^> "{{if   'expr'}} wololo{{endif}}"))

ifWithElse :: TestTree
ifWithElse = testCase "Basic if statement with else"
   (assertEqual "Should parse if with else to valid AST"
      (Right [
           IfPiece [LiteralExpression (LitString "expr"),LiteralExpression (LitBool True)]
           [ [StaticPiece " wololo"]
           , [ForPiece "i" (LiteralExpression (LitString "ppp")) [StaticPiece "wnetrze"]]
           ]])
      (parseAll <^> "{{if   'expr'}} wololo{{else}}{{for $i in 'ppp'}}wnetrze{{endfor}}{{endif}}"))


ifWithMultipleElifs :: TestTree
ifWithMultipleElifs = testCase "If statement with more than one elif"
   (assertEqual "Should be parsed to valid AST"
      (Right
          [IfPiece [ LiteralExpression (LitString "expr1")
                   , LiteralExpression (LitString "expr2")
                   , LiteralExpression (LitString "expr3")
                   , LiteralExpression (LitBool True)]
                   [ [StaticPiece " first block "]
                   , [StaticPiece " second "]
                   , [StaticPiece " third "]
                   , [StaticPiece " else "]]])
      (parseAll <^> "{{if 'expr1'}} first block {{elif 'expr2'}} second {{elif 'expr3'}} third {{else}} else {{endif}}"))


multipleNestedIfs :: TestTree
multipleNestedIfs = testCase "If statement with more than one elif"
   (assertEqual "Should parse to valid AST"
     (Right
         [IfPiece
             [ LiteralExpression (LitString "expr1")
             , LiteralExpression (LitString "expr2")
             , LiteralExpression (LitBool True)]
             [
                 [ IfPiece [LiteralExpression (LitString "expr1-nest")]
                      [ [ForPiece "i" (LiteralExpression (LitString "wolllo"))
                            [ Decl [("j",LiteralExpression (LitString "xxx"))]
                            , StaticPiece "nested for"]]
                      ]
                 ]
                 , [ IfPiece
                      [ LiteralExpression (LitString "expr2-nest")
                      , LiteralExpression (LitString "expr2-nest-2")
                      , LiteralExpression (LitString "expr2-nest-3")]
                      [[StaticPiece " one "],[StaticPiece ""],[StaticPiece " three "]]
                  ]
                , [StaticPiece "else"]
              ]
          ])
      (parseAll <^> "{{if 'expr1'}}\
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
      (parseAll <^> "{{if 'expr1'}} first block  {{endif}} {{elif 'expr2'}} second {{else}} else {{endif}}"))

ifWithMutipleElses :: TestTree
ifWithMutipleElses = testCase "If statement with more than one else"
   (assertEither "Should fail"
      left
      (parseAll <^> "{{if 'expr1'}} first block {{else}} second {{elif 'expr3'}} third {{else}} else {{endif}}"))

{-
  ------------------------- DECLARATIONS
-}

declarations :: TestTree
declarations = testGroup "Declarations" [singleDeclaration, multipleDeclaration, noDollarSignDeclaration]

singleDeclaration :: TestTree
singleDeclaration = testCase "Single declaration"
   (assertEqual "Should parse simple declaration to valid AST"
      (Right [StaticPiece "warszawa ",Decl [("name",LiteralExpression (LitString "dawid"))],StaticPiece " legia"])
      (parseAll <^> "warszawa {{let        $name='dawid'}} legia")
   )


multipleDeclaration :: TestTree
multipleDeclaration = testCase "Multiple declaration"
   (assertEqual "Should parse multiple declaration to valid AST"
      (Right [ Decl [("name",LiteralExpression (LitString "dawid")),("lastname",LiteralExpression (LitString "Palkovksy"))]
             , StaticPiece "staticcontent"])
      (parseAll <^> "{{let $name   =  'dawid',   $lastname='Palkovksy'}}staticcontent")
   )

noDollarSignDeclaration :: TestTree
noDollarSignDeclaration = testCase "Declaration without dollar sign by variable name"
   (assertEither "Should error out, due to lack of dollar sign by var name"
      left
      (parseAll <^> "static   {{let $name='dawid', lastname='Palkovksy'}}staticcontent")
  )

{-
  ------------------------- INCLUDES
-}
includes :: TestTree
includes = testGroup "Includes" [referenceInclude, pathInclude]

referenceInclude :: TestTree
referenceInclude = testCase "Reference include"
   (assertEqual "Should parse include tag"
      (Right [StaticPiece " ", IncludeRefPiece "k", StaticPiece " andrzej  "])
      (parseAll <^> " {{include $k}} andrzej  ")
   )

pathInclude :: TestTree
pathInclude = testCase "Path include"
  (assertEqual "Should parse include tag"
     (Right [StaticPiece " ", IncludePathPiece "folder/file.html", StaticPiece " andrzej  "])
     (parseAll <^> " {{include 'folder/file.html'}} andrzej  ")
  )

{-
  ------------------------- CALL TAG
-}
callTag :: TestTree
callTag = testGroup "Call tag" [callAfterLet, callInFor]

callInFor :: TestTree
callInFor = testCase "Call in for"
   (assertEqual "Should be parsed to valid AST"
      (Right [
          ForPiece "i"
              (ListExpression [ LiteralExpression $ LitInteger 1
                              , LiteralExpression $ LitInteger 2
                              , LiteralExpression $ LitInteger 3])
              [StaticPiece " ", CallPiece (ReferenceExpression "i"), StaticPiece " "]])
      (parseAll <^> "{{for $i in [1,2,3]}} {- $i}} {{endfor}}")
   )


callAfterLet :: TestTree
callAfterLet = testCase "Call after let"
  (assertEqual "Should be parsed to valid AST"
     (Right [ Decl [("i", LiteralExpression $ LitDouble 32.4),("j", LiteralExpression $ LitString "xxx")]
            , StaticPiece " I'm ", CallPiece (ReferenceExpression "i"), StaticPiece " years old. "])
     (parseAll <^> "{{let $i=32.4, $j='xxx'}} I'm {- $i}} years old. ")
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
      (parseAll <^> "{{comment}} {{let $name   =  'dawid',   $lastname='Palkovksy'}}staticcontent {{endcomment}}")
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
      (parseAll <^> "{{raw}} {{let $name   =  'dawid',   $lastname='Palkovksy'}}staticcontent {{endraw}}")
   )
