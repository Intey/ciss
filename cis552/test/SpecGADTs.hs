import Test.HUnit
import GADTs

-- The expression "1 + 3"
oe1 :: OExp
oe1 = OAdd (OInt 1) (OInt 3)
-- The expression "if (3 + -3) == 0 then 3 else 4"
oe2 :: OExp
oe2 = OIf (OIsZero (OAdd (OInt 3) (OInt (-3)))) (OInt 3) (OInt 4)

oe1_example :: Maybe (Either Int Bool)
oe1_example = oevaluate oe1
oe2_test :: Test
oe2_test = oevaluate oe2 ~?= Just (Left 3)

main :: IO ()
main = pure
