module AoC.Tester where

import AoC.Core.Date (Date)
import AoC.Core.File
import AoC.Prelude
import Test.Hspec

tester ::
  (Show a, Show b, Show c, Eq b, Eq c) =>
  Either String Date ->
  (String -> Maybe a) ->
  (a -> b, b) ->
  (a -> c, c) ->
  Spec
tester (Left s) _ _ _ = it "is an invalid year" $ expectationFailure s
tester (Right date) parse (solveA, solA) (solveB, solB) = do
  beforeAll (readInput date) $ do
    it ("parse " <> inputName date) $ \i -> do
      parse i `shouldSatisfy` isJust
    it "solve part one" $ \i -> do
      solveA <$> parse i `shouldBe` Just solA
    it "solve part two" $ \i -> do
      solveB <$> parse i `shouldBe` Just solB

testerPending ::
  forall a b c.
  (Show a, Show b, Show c, Eq b, Eq c) =>
  Either String Date ->
  ParseTest a ->
  RunTest a b ->
  RunTest a c ->
  Spec
testerPending (Left s) _ _ _ = it "is an invalid year" $ expectationFailure s
testerPending (Right date) p sa sb = do
  beforeAll (readInput date) $ do
    it ("parse " <> inputName date) $ \i -> case p of
      ParsePending -> pending
      Parse parse -> parse i `shouldSatisfy` isJust
    it "solve part one" $ \i -> case sa of
      RunPending -> pending
      Run parse (solveA, solA) -> solveA <$> parse i `shouldBe` Just solA
    it "solve part two" $ \i -> case sb of
      RunPending -> pending
      Run parse (solveB, solB) -> solveB <$> parse i `shouldBe` Just solB

parsePending :: ParseTest ()
parsePending = ParsePending

runPending :: RunTest a ()
runPending = RunPending

data ParseTest a
  = ParsePending
  | Parse (String -> Maybe a)

data RunTest a b
  = RunPending
  | Run (String -> Maybe a) (a -> b, b)
