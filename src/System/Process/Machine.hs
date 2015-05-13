{-# LANGUAGE RankNTypes #-}
module System.Process.Machine where

import Data.Machine
import Data.IOData (IOData)
import System.Exit (ExitCode(..))
import System.IO.Machine
import System.Process (CreateProcess(..), ProcessHandle, createProcess, waitForProcess)

type ProcessMachines a b k = (Maybe (ProcessT IO a b), Maybe (MachineT IO k a), Maybe (MachineT IO k a))

mStdIn :: IOSource a -> ProcessMachines a a0 k0 -> IO ()
mStdIn ms (Just stdIn, _, _)  = runT_ $ stdIn <~ ms
mStdIn _  _                   = return ()

mStdOut :: ProcessT IO a b -> ProcessMachines a a0 k0 -> IO [b]
mStdOut mp (_, Just stdOut, _)  = runT $ mp <~ stdOut
mStdOut _  _                    = return []

mStdErr :: ProcessT IO a b -> ProcessMachines a a0 k0 -> IO [b]
mStdErr mp (_, _, Just stdErr)  = runT $ mp <~ stdErr
mStdErr _  _                    = return []

callProcessMachines :: IOData a => forall b k. IODataMode a -> CreateProcess -> (ProcessMachines a b k -> IO c) -> IO (ExitCode, c)
callProcessMachines m cp f = do
  (machines, pHandle) <- createProcessMachines m cp
  x                   <- f machines
  exitCode            <- waitForProcess pHandle
  return (exitCode, x)

createProcessMachines :: IOData a => forall b k. IODataMode a -> CreateProcess -> IO (ProcessMachines a b k, ProcessHandle)
createProcessMachines (r, w) cp = do
  (pIn, pOut, pErr, pHandle) <- createProcess cp
  let pInSink = fmap (sinkHandleWith w) pIn
  let pOutSource = fmap sourceHandle' pOut
  let pErrSource = fmap sourceHandle' pErr
  return $ ((pInSink, pOutSource, pErrSource), pHandle) where
    sourceHandle' = sourceHandleWith r
