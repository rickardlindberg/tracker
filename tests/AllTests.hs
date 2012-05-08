import Fixtures()
import LogTime
import Test.Hspec.HUnit()
import Test.Hspec.Monadic
import Test.Hspec.QuickCheck
import Test.HUnit
import Tracking
import Tracking.Persistence

main = hspecX $ do

    describe "time" $

        it "can be created, formatted, and parsed" $
            let t = parseLogTime "2012-05-05 18:00"
            in parseLogTime (formatLogTime t) @?= t

    describe "persistence of trackings" $ do

        it "can be converted to string" $
            formatTracking (Tracking "name here"
                [ TrackingEntry (parseLogTime "2012-05-05 18:00") 12.0 "a comment"
                ]
            )
            @?=
            (
                   "name here\n"
                ++ "2012-05-05 18:00 | 12.0 | a comment\n"
            )

        it "can be read from string" $
            parseTracking (
                   "name here\n"
                ++ "2012-05-05 18:00 | 12.0 | a comment\n"
            )
            @?=
            Just (Tracking "name here"
                [ TrackingEntry (parseLogTime "2012-05-05 18:00") 12.0 "a comment"
                ]
            )

        prop "roundtrip returns the same" $
            \tracking -> parseTracking (formatTracking tracking) == Just tracking
