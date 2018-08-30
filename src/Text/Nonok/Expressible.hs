{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module Text.Nonok.Expressible ( Expressible(..)
                              , buildVarLookup, addVar, addGen, addList) where

import Text.Nonok.Types

import Control.Monad.Trans.Writer

import qualified Data.Text.Lazy as LT
import qualified Data.Text as T
import qualified Data.Map.Strict as M

-- | This typeclass describes type ability to be converted to an expression.
-- | It is used to simplify creating an expression. For example, you would
-- | write (express "mystring") rather than (LiteralExpression $ LitString "mystring")
-- | Basically it lifts type to an expression.
class Expressible a where
    express :: a  -> Expression

instance Expressible Literal where
    express = LiteralExpression
instance Expressible Reference where
    express = LiteralExpression . LitRef

instance {-# OVERLAPPABLE #-} (Num a, Integral a) => (Expressible a) where
    express = LiteralExpression . LitNum . fromIntegral

instance  Expressible T.Text where
    express = LiteralExpression . LitString . T.unpack

instance  Expressible LT.Text where
    express = LiteralExpression . LitString . LT.unpack

instance  Expressible Bool where
    express = LiteralExpression . LitBool

instance   Expressible Char where
   express char = LiteralExpression $ LitString [char]

instance  Expressible [Char] where
    express = LiteralExpression . LitString

instance {-# OVERLAPPABLE #-} (Expressible a) => Expressible [a] where
    express list = ListExpression $ map express list

instance {-# OVERLAPPABLE #-} (Expressible a) => Expressible (M.Map String a) where
    express m = MapExpression $ M.map express m

instance Expressible Expression where
    express = id


buildVarLookup :: VarGenerator a -> VariableLookup
buildVarLookup = M.fromList . execWriter

addVar :: (Expressible a) => String -> a -> VarGenerator ()
addVar key val = tell [(key, express val)]

addGen :: String -> VarGenerator () -> VarGenerator ()
addGen key writer = tell [(key, express $ buildVarLookup writer)]

addList :: String -> (a -> VarGenerator ()) -> [a] -> VarGenerator ()
addList key f list = addVar key $ map (buildVarLookup . f) list
