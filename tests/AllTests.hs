import LogTime
import Test.Hspec.HUnit()
import Test.Hspec.Monadic
import Test.HUnit

main = hspecX $

    describe "time" $

        it "can be created, formatted, and parsed" $
            let t = parseLogTime "2012-05-05 18:00"
            in parseLogTime (formatLogTime t) @?= t
