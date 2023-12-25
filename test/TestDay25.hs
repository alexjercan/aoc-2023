module TestDay25 (test) where

import Day25
import Test.Hspec

input :: String
input = "jqt: rhn xhk nvd\nrsh: frs pzl lsr\nxhk: hfx\ncmg: qnr nvd lhk bvb\nrhn: xhk bvb hfx\nbvb: xhk hfx\npzl: lsr hfx nvd\nqnr: nvd\nntq: jqt hfx bvb xhk\nnvd: lhk\nlsr: lhk\nrzs: qnr cmg lsr rsh\nfrs: qnr lhk lsr"

test1 :: Expectation
test1 = part1 input `shouldBe` "54"

test :: IO ()
test = hspec $ do
    describe "day25" $ do
        describe "part1" $ do
            it "should work for the examples" test1
