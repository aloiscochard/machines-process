{-# LANGUAGE ImpredicativeTypes #-}
module System.Process.Machine where

import Data.Machine
import Data.IOData (IOData, hGetLine, hPutStrLn)
import System.Exit (ExitCode)
import System.IO (Handle)
import System.IO.Machine
import System.Process (createProcess, waitForProcess, CreateProcess, ProcessHandle)

data ProcessMachines a a0 k0 = ProcessMachines (ProcessT IO a a0) (MachineT IO k0 a) (MachineT IO k0 a)
type ProcessExecution a b = (ExitCode, b)

createProcessMachines :: IOData a => forall a0 k0. IODataMode a -> CreateProcess -> IO (ProcessMachines a a0 k0, ProcessHandle)
createProcessMachines (IODataMode r w) cp = do
  (pIn, pOut, pErr, pHandle) <- createProcess cp
  let pInSink = getOrStop $ fmap (sinkHandleWith w) pIn
  let pOutSource = getOrStop $ fmap sourceHandle pOut
  let pErrSource = getOrStop $ fmap sourceHandle pOut
  return $ (ProcessMachines pInSink pOutSource pErrSource, pHandle) where
    sourceHandle = sourceHandleWith r
    getOrStop = maybe stopped id

withProcessMachines :: IOData a => IODataMode a -> CreateProcess -> (ProcessMachines a a0 k0 -> IO b) -> IO (ProcessExecution a b)
withProcessMachines m cp f = do
  (machines, pHandle) <- createProcessMachines m cp
  x                   <- f machines
  exitCode            <- waitForProcess pHandle
  return (exitCode, x)

