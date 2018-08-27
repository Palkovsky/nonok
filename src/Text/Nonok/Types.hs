module Text.Nonok.Types where

import Text.Parsec
import Text.Parsec.Expr

import Data.List (intercalate)
import qualified Data.Text as T
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import qualified Data.Text.Lazy.Builder as B

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

type VariableLookup = M.Map String Expression
type FunctionStore = M.Map String Function
type ScopeStack = [S.Set String] --stack contains list of vars defined in scope

-- (local vars ($), global vars (@))
-- global vars are constant and are passed to includes
data RenderState = RenderState { localVars :: VariableLookup
                               , globalVars :: VariableLookup
                               , scopeStack :: ScopeStack
                               , functions :: FunctionStore
                               }

data RenderError = RenderError String
                 | FunctionError String
                 | ParsingError ParseError
                 deriving (Show, Eq)


type Render a = Renderer B.Builder RenderState RenderError a
type Parser a = ParsecT String () Identity a

data Piece = StaticPiece String
           | CommentPiece
           | RawPiece String
           | IncludeRefPiece Reference (Maybe Expression)-- Important: inclue have separate scope stack and local variables
           | IncludePathPiece String (Maybe Expression)
           | ForPiece String Expression [Piece] -- name of var, list expression, contents of block
           | IfPiece [Expression] [[Piece]]
           | CallPiece Expression
           | Decl [(String, Expression)]
           deriving (Show, Eq)

data Expression = LiteralExpression Literal
                | ListExpression [Expression]
                | MapExpression (M.Map String Expression)
                | MapMemberExpression Reference [String]
                | FuncExpression String [Expression]
                deriving (Show, Eq)

data PrintableExpression = PrintableExpression Expression

data Reference = RefLocal !String
               | RefGlobal !String
               deriving (Show, Eq)

data Function = FuncA0 (Render Expression)
              | FuncA1 (Expression -> Render Expression)
              | FuncA2 (Expression -> Expression -> Render Expression)
              | FuncA3 (Expression -> Expression -> Expression -> Render Expression)

data Literal = LitString !String
             | LitBool !Bool
             | LitDouble !Double
             | LitInteger !Integer
             | LitRef Reference
             | LitEmpty
             deriving (Eq)

instance Show Literal where
    show (LitString v) = v
    show (LitBool b) = if b then "true" else "false"
    show (LitDouble n) = show n
    show (LitInteger n) = show n
    show (LitRef (RefLocal var)) = "$" ++ var
    show (LitRef (RefGlobal var)) = "@" ++ var
    show LitEmpty = ""

instance Show PrintableExpression where
    show (PrintableExpression (LiteralExpression lit)) = show lit
    show x = show x
