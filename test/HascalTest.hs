import Test.Framework (defaultMain, testGroup)
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.Framework.Providers.HUnit (testCase)
import ICalParserTests

main = defaultMain tests

tests = [
        testGroup "Parser tests" [
                testGroup "Uid parser tests" [
                      testProperty "Random" prop_uid
                    , testCase "Escaped values" test_uid_escaped_value
                    , testCase "Empty value" test_uid_empty
                ]
            ]
        ]
