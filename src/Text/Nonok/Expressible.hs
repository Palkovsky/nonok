{-# LANGUAGE FlexibleInstances #-}
module Text.Nonok.Expressible (Expressible(..), expressInt, expressFloat) where

import Text.Nonok.Types

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

newtype IntegralWrap a = IntegralWrap a
instance (Integral a) => Expressible (IntegralWrap a) where
    express (IntegralWrap x) = express $ LitInteger $ fromIntegral x
expressInt :: (Integral a) => a -> Expression
expressInt int = express $ IntegralWrap int

newtype FractionalWrap a = FractionalWrap a
instance (Real a, Fractional a) => Expressible (FractionalWrap a) where
    express (FractionalWrap x) = express $ LitDouble $ realToFrac x
expressFloat :: (Real a, Fractional a) => a -> Expression
expressFloat float = express $ FractionalWrap float

instance Expressible Bool where
    express = express . LitBool
instance Expressible Char where
   express char = express $ LitString [char]
instance {-# OVERLAPS #-} Expressible [Char] where
    express = express . LitString
instance {-# OVERLAPPABLE #-} (Expressible a) => Expressible [a] where
    express list = ListExpression $ map express list
instance (Expressible a) => Expressible (M.Map String a) where
    express m = MapExpression $ M.map express m
instance Expressible Expression where
    express = id
