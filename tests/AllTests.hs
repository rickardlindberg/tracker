import LogTime
import Test.Hspec.HUnit()
import Test.Hspec.Monadic
import Test.HUnit
import Tracking

main = hspecX $ do

    describe "time" $

        it "can be created, formatted, and parsed" $
            let t = parseLogTime "2012-05-05 18:00"
            in parseLogTime (formatLogTime t) @?= t

    describe "logs" $ do

        it "can be converted to strings" $
            let entries  = [ TrackingEntry (parseLogTime "2012-05-05 18:00") 12.0 "a comment"
                           ]
                tracking = Tracking "name here" entries
            in formatTracking tracking @?= ("name here\n" ++
                                            "2012-05-05 18:00 -> 12.0\n" ++
                                            "  a comment\n")

        it "can be read from strings" $
            let str  = ("name here\n" ++
                                            "2012-05-05 18:00 -> 12.0\n" ++
                                            "  a comment\n")
            in parseTracking str @?= Tracking "name here" [ TrackingEntry (parseLogTime "2012-05-05 18:00") 12.0 "a comment"
                                     ]
