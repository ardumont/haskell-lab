module Account where

import Control.Concurrent.STM

-- Represent the balance of the account
type Account = TVar Int

-- transfer `amount` from account `from` to account `to`
transfer :: Account -> Account -> Int -> IO ()
transfer from to amount =
  atomically (
    do deposit to amount
       withdraw from amount
  )

deposit :: Account -> Int -> STM ()
deposit acc amt =
  do balance <- readTVar acc
     writeTVar acc (balance + amt)

withdraw :: Account -> Int -> STM ()
withdraw acc amt = deposit acc (- amt)

showAccount :: Account -> IO String
showAccount acc = do bal <- (atomically . readTVar) acc
                     return $ show bal

displayAccount :: String -> Account -> IO ()
displayAccount label acc =
  do bal <- showAccount acc
     putStrLn $ label ++ ": " ++ bal

transferFromAccountAToAccountB :: IO ()
transferFromAccountAToAccountB =
  do accA <- accountA
     accB <- accountB
     displayAccount "A" accA
     displayAccount "B" accB
     transfer accA accB 10
     putStrLn "Transfer 10 from A to B: "
     displayAccount "A" accA
     displayAccount "B" accB
    where accountA :: IO Account
          accountA = atomically . newTVar $ (100 :: Int)
          accountB :: IO Account
          accountB = atomically . newTVar $ (300 :: Int)

-- *Account> transferFromAccountAToAccountB
-- A: 100
-- B: 300
-- Transfer 10 from A to B:
-- A: 90
-- B: 310
