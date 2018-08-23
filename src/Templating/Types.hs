module Templating.Types where

import Text.Parsec
import Text.Parsec.Expr

import Data.List (intercalate)
import qualified Data.Map.Strict as M
import qualified Data.Set as S

import Control.Monad.Identity
import Control.Monad.Trans.State
import Control.Monad.Trans.Except
import Control.Monad.Trans.Writer


{-
    - w is an accumulator type for output
    - s is state type, for keeping track of declared variables
    - b and a are just left/right of either monad
-}
type Renderer w s b a = ExceptT b (WriterT w (StateT s IO)) a

type VariableLookup = M.Map String Literal
type ScopeStack = [S.Set String] --stack contains list of vars defined in scope

-- (local vars ($), global vars (@))
-- global vars are constant and are passed to includes
data RenderState = RenderState { localVars :: VariableLookup
                               , globalVars :: VariableLookup
                               , scopeStack :: ScopeStack
                               } deriving (Show, Eq)

data RenderError = RenderError String deriving (Show)
type Render a = Renderer String RenderState RenderError a

type Parser a = ParsecT String () Identity a

data Piece = StaticPiece String
           | CommentPiece
           | RawPiece String
           | IncludeRefPiece Reference -- Important: inclue have separate scope stack and local variables
           | IncludePathPiece String
           | BlockPiece String [Piece]
           | ForPiece String Expression [Piece] -- name of var, list expression, inside of block
           | IfPiece [Expression] [[Piece]]
           | CallPiece Expression
           | Decl [(String, Expression)]
           deriving (Show, Eq)

data Expression = LiteralExpression Literal
                | ListExpression [Expression]
                | ReferenceExpression Reference
                | MapMemberExpression Reference [String]
                deriving (Show, Eq)

data Reference = RefLocal String
               | RefGlobal String
               deriving (Show, Eq)

data Literal = LitString !String
             | LitList ![Literal]
             | LitBool !Bool
             | LitDouble !Double
             | LitInteger !Integer
             | LitMap (M.Map String Literal)
             | LitEmpty
             deriving (Eq)

instance Show Literal where
    show (LitString v) = v
    show (LitList list) = "[" ++ (intercalate ", "  $ map show list) ++ "]"
    show (LitBool b) = if b then "true" else "false"
    show (LitDouble n) = show n
    show (LitInteger n) = show n
    show (LitMap m) = "{" ++ (intercalate ", "  $
         map (\(key, val) -> "\"" ++ key ++ "\" : " ++ (show val)) $ M.toList m) ++ "}"
    show LitEmpty = ""
