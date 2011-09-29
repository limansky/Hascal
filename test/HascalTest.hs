import ICalParser
import ICalDefs

import Test.QuickCheck
import Text.ParserCombinators.Parsec

fromRight (Right x) = x

checkUid s = (fromRight $ parse uid "" ("UID:" ++ s ++ "\r\n")) == Uid s

main = do
    quickCheck checkUid
