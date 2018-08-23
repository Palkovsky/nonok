module Assertions
   (assertEither, assertEitherIO, assertEqualIO, right, left)
   where

import Test.Tasty.HUnit

right :: Either () ()
right = Right ()

left :: Either () ()
left = Left ()

-- | It only checks if result is left or right
assertEither :: (HasCallStack) => String -> Either a b -> Either c d -> Assertion
assertEither msg expected actual =
    case (expected, actual) of
        (Left _, Left _) -> assertEqual msg "" ""
        (Right _, Right _) -> assertEqual msg "" ""
        _ -> assertEqual msg "" "-"

assertEitherIO :: (HasCallStack) => String -> IO (Either a b) -> IO (Either c d) -> Assertion
assertEitherIO msg ex ac = do
    expected <- ex
    actual <- ac
    case (expected, actual) of
        (Left _, Left _) -> assertEqual msg "" ""
        (Right _, Right _) -> assertEqual msg "" ""
        _ -> assertEqual msg "" "-"

assertEqualIO :: (HasCallStack, Eq a, Show a) => String -> IO a -> IO a -> Assertion
assertEqualIO msg ex ac = do
    expected <- ex
    actual <- ac
    assertEqual msg expected actual
