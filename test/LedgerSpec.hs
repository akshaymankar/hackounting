{-# LANGUAGE OverloadedStrings #-}
module LedgerSpec where

import Test.Hspec
import Ledger
import Data.Set as Set
import Data.List as List

{-# ANN module ("HLint: ignore Redundant do"::String) #-}

fixtureAccounts = Set.fromList ["Root Account","Assets","Current Assets","Checking Account","Income","Expenses","Equity","Opening Balances","Template Root","Education","Savings Account"]

spec :: Spec
spec = do
  describe "LedgerSpec" $ do
    context "readGnuCash" $ do
      it "should read account names" $ do
        ledger <- readGnuCash "test/fixtures/test.gnucash"
        Set.fromList (Prelude.map accountName $ accounts ledger) `shouldBe` fixtureAccounts

      it "should read account names" $ do
        ledger <- readGnuCash "test/fixtures/test.gnucash"
        Set.fromList (Prelude.map accountName $ accounts ledger) `shouldBe` fixtureAccounts

      it "should read account types" $ do
        ledger <- readGnuCash "test/fixtures/test.gnucash"
        let root = accountType <$> find (\a -> accountName a == "Root Account") (accounts ledger)
            edAcc = accountType <$> find (\a -> accountName a == "Education") (accounts ledger) in do
          root `shouldBe` Just RootAccount
          edAcc `shouldBe` Just ExpensesAccount

      it "should read transactions" $ do
        ledger <- readGnuCash "test/fixtures/test.gnucash"
        let edAcc = find (\a -> accountName a == "Education") (accounts ledger) in do
          amount . head . transactions <$> edAcc `shouldBe` Just 20000
