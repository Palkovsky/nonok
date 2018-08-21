module Assertions
   (assertEither, right, left)
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
