{-# LANGUAGE OverloadedStrings #-}
module Ledger where

import Data.Text
import Data.Time
import Data.Time.Format
import Database.SQLite.Simple
import Database.SQLite.Simple.FromRow

data AccountType = RootAccount
                 | AssetsAccount
                 | BankAccount
                 | IncomeAccount
                 | ExpensesAccount
                 | EquityAccount
  deriving (Show, Eq)

data Account = Account { accountGuid  :: Text
                       , accountName  :: Text
                       , accountType  :: AccountType
                       , transactions :: [Transaction] }

data Transaction = Transaction { guid        :: Text
                               , description :: Text
                               , postDate    :: Day
                               , amount      :: Int }

data Ledger = Ledger { accounts :: [Account] }


textToAccountType :: Text -> AccountType
textToAccountType "ROOT" = RootAccount
textToAccountType "ASSET" = AssetsAccount
textToAccountType "BANK" = BankAccount
textToAccountType "INCOME" = IncomeAccount
textToAccountType "EQUITY" = EquityAccount
textToAccountType "EXPENSE" = ExpensesAccount
textToAccountType _ = error "Invalid account type"

entryToAccount :: AccountEntry -> Account
entryToAccount (AccountEntry g n t) = Account g n (textToAccountType t) []

data AccountEntry = AccountEntry Text Text Text
  deriving Show

data TransactionEntry = TransactionEntry Text Text Text Int
  deriving Show

instance FromRow AccountEntry where
  fromRow = AccountEntry <$> field <*> field <*> field

instance FromRow TransactionEntry where
  fromRow = TransactionEntry <$> field <*> field <*> field <*> field

entryToTransaction :: TransactionEntry -> Transaction
entryToTransaction (TransactionEntry g d pd a)= Transaction g d (parseDay pd) a

parseDay :: Text -> Day
parseDay = parseTimeOrError True defaultTimeLocale "%Y%0m%0d%0H%0M%0S" . unpack

transactionQuery :: Account -> Query
transactionQuery account = Query $
  "SELECT transactions.guid, description, post_date, splits.value_num \
    \ FROM transactions \
    \ JOIN splits \
    \ WHERE \
    \ splits.tx_guid == transactions.guid \
    \ AND splits.account_guid = '" `append` accountGuid account `append` "'"

readTransactions :: Connection -> Account -> IO [Transaction]
readTransactions conn account =
  Prelude.map entryToTransaction <$>
    (query_ conn (transactionQuery account) :: IO [TransactionEntry])

fillAccount :: Connection -> Account -> IO Account
fillAccount conn emptyAccount = do
  txns <- readTransactions conn emptyAccount
  return $ emptyAccount { transactions = txns }

readGnuCash :: FilePath -> IO Ledger
readGnuCash f = do
  conn <- open f
  accountEntries <- query_ conn "SELECT guid, name, account_type FROM accounts" :: IO [AccountEntry]
  accounts <- mapM (fillAccount conn . entryToAccount) accountEntries
  return $ Ledger accounts
