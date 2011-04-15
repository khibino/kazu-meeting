
import Control.Concurrent.STM
import Control.Concurrent
import System.Random


-- Reindeer and Elf

meetInStudy :: Int -> IO ()
meetInStudy id' = putStr ("Elf " ++ show id' ++ " meeting in th study\n")

deliverToys :: Int -> IO ()
deliverToys id' = putStr ("Reindeer " ++ show id' ++ " delivering toys\n")

helper1 :: Group -> IO () -> IO ()
helper1 group do_task = do { (in_gate, out_gate) <- joinGroup group
                           ; passGate in_gate
                           ; do_task
                           ; passGate out_gate }

elf1, reindeer1 :: Group -> Int -> IO ()
elf1      gp id' = helper1 gp (meetInStudy id')
reindeer1 gp id' = helper1 gp (deliverToys id')

-- Gate and Group

data Gate = MkGate Int (TVar Int)

newGate :: Int-> STM Gate
newGate n = do { tv <- newTVar 0; return (MkGate n tv) }

passGate :: Gate -> IO ()
passGate (MkGate _ tv) =
  atomically (do { n_left <- readTVar tv
                 ; _ <- check (n_left > 0)
                 ; writeTVar tv (n_left - 1) })

operateGate :: Gate -> IO ()
operateGate (MkGate n tv) =
  do { atomically (writeTVar tv n)
     ; atomically (do { n_left <- readTVar tv
                      ; check (n_left == 0) }) }


data Group = MkGroup Int (TVar (Int, Gate, Gate))

newGroup :: Int -> IO Group
newGroup n = atomically (do { g1 <- newGate n; g2 <- newGate n
                            ; tv <- newTVar (n, g1, g2)
                            ; return (MkGroup n tv) })

joinGroup :: Group -> IO (Gate, Gate)
joinGroup (MkGroup _ tv) =
  atomically (do { (n_left, g1, g2) <- readTVar tv
                 ; _ <- check (n_left > 0)
                 ; writeTVar tv (n_left - 1, g1, g2)
                 ; return (g1, g2) })

awaitGroup :: Group -> STM (Gate, Gate)
awaitGroup (MkGroup n tv) =
  do { (n_left, g1, g2) <- readTVar tv
     ; _ <- check (n_left == 0)
     ; new_g1 <- newGate n; new_g2 <- newGate n
     ; writeTVar tv (n, new_g1, new_g2)
     ; return (g1, g2) }

-- main

main :: IO ()
main = do { elf_group <- newGroup 3
          ; sequence_ [ elf elf_group n | n <- [1..10] ]
            
          ; rein_group <- newGroup 9
          ; sequence_ [ reindeer rein_group n | n <- [1..9] ]
            
          ; forever (santa elf_group rein_group) }

elf :: Group -> Int -> IO ThreadId
elf gp id' = forkIO (forever (do { elf1 gp id'; randomDelay }))

reindeer :: Group -> Int -> IO ThreadId
reindeer gp id' = forkIO (forever (do {reindeer1 gp id'; randomDelay}))

forever :: IO () -> IO ()
forever act = do { act; forever act }

randomDelay :: IO ()
randomDelay =
  do { waitTime <- getStdRandom (randomR (1, 3000000))
     ; threadDelay waitTime }

santa :: Group -> Group -> IO ()
santa elf_gp rein_gp =
  do { putStr "----------\n"
     ; choose [(awaitGroup rein_gp, run "deliver toys"),
               (awaitGroup elf_gp, run "meet in my study")] }
  where
    run :: String -> (Gate, Gate) -> IO ()
    run task (in_gate, out_gate) =
      do { putStr ("Ho! Ho! Ho! let's " ++ task ++ "\n")
         ; operateGate in_gate
         ; operateGate out_gate }

choose :: [(STM a, a-> IO ())] -> IO ()
choose choices = do { act <- atomically (foldr1 orElse actions)
                    ; act }
  where
    actions :: [STM (IO ())]
    actions = [ do { val <- guard; return (rhs val) }
              | (guard, rhs) <- choices ]

--
--
