import Test.Framework (defaultMain, testGroup)
import Test.Framework.Providers.QuickCheck2 (testProperty)
import ICalParserTests

main = defaultMain tests

tests = [
        testGroup "Parser tests" [
                testProperty "Random test for uid" prop_uid
            ]
        ]
