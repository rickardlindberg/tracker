import Test.Hspec.HUnit()
import Test.Hspec.Monadic
import Test.HUnit
import Tracking

main = hspecX $

    describe "time" $

        it "can be created, formatted, and parsed" $
            let t = myParseTime "2012-05-05 18:00"
            in myParseTime (myFormatTime t) @?= t
