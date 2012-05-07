module Fixtures where

import Control.Monad
import LogTime
import Test.QuickCheck
import Tracking

instance Arbitrary Tracking where
    arbitrary = liftM2 Tracking arbitraryName arbitrary

instance Arbitrary TrackingEntry where
    arbitrary = liftM3 TrackingEntry arbitraryTime arbitrary arbitraryComment

arbitraryName = listOf $ elements ['a'..'z']
arbitraryTime = return (parseLogTime "2012-11-11 11:11")
arbitraryComment = listOf $ elements ['a'..'z']
