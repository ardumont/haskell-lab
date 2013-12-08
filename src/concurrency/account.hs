module Account where

import Control.Concurrent
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

limitedWithdraw :: Account -> Int -> STM ()
limitedWithdraw acc amt =
  do balance <- readTVar acc
     check (0 <= amt && amt <= balance)
     writeTVar acc (balance - amt)

-- Beware signature is different from deposit one
delayDeposit :: Account -> Int -> IO ()
delayDeposit acc amt =
  do threadDelay 3000000            -- wait 3 seconds
     atomically ( deposit acc amt )

withdrawFromAccountAWithRetry :: IO ()
withdrawFromAccountAWithRetry =
  do accA <- accountA
     displayAccount "A" accA
     putStrLn "Transfer 11 from A (waiting if not enough money)..."
     forkIO(delayDeposit accA 1)
     atomically ( limitedWithdraw accA 11 )
     displayAccount "A" accA
    where accountA :: IO Account
          accountA = atomically . newTVar $ (10 :: Int)

-- *Account> withdrawFromAccountAWithRetry
-- A: 10
-- Transfer 11 from A (waiting if not enough money)...
-- A: 0
