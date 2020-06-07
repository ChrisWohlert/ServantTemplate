module Main where

import Lib
import API

import           Control.Concurrent       (MVar, ThreadId, forkIO, killThread,
                                           newEmptyMVar, putMVar, takeMVar)
import           Control.Exception        (finally)
import           Control.Monad            ((>=>))
import           Data.IORef               (IORef, newIORef, readIORef,
                                           writeIORef)
import           Foreign.Store            (Store (..), lookupStore, readStore,
                                           storeAction, withStore)
import           GHC.Word                 (Word32)
import           Network.Wai.Handler.Warp (defaultSettings, runSettings,
                                           setPort)

main :: IO ()
main = genAuthMain

update :: IO ()
update = do
    mtidStore <- lookupStore tidStoreNum 
    case mtidStore of
      -- no server running
      Nothing -> do
          done <- storeAction doneStore newEmptyMVar
          tid <- start done
          _ <- storeAction (Store tidStoreNum) (newIORef tid)
          return ()
      -- server is already running
      Just tidStore -> restartAppInNewThread tidStore
  where
    doneStore :: Store (MVar ())
    doneStore = Store 0

    -- shut the server down with killThread and wait for the done signal
    restartAppInNewThread :: Store (IORef ThreadId) -> IO ()
    restartAppInNewThread tidStore = modifyStoredIORef tidStore $ \tid -> do
        killThread tid
        withStore doneStore takeMVar
        readStore doneStore >>= start


    -- | Start the server in a separate thread.
    start :: MVar () -- ^ Written to when the thread is killed.
          -> IO ThreadId
    start done = do
        print "Starting"
        forkIO (finally genAuthMain
                        -- Note that this implies concurrency
                        -- between shutdownApp and the next app that is starting.
                        -- Normally this should be fine
                        (putMVar done () >> shutdownApp))

                        
tidStoreNum :: Word32
tidStoreNum = 1


modifyStoredIORef :: Store (IORef a) -> (a -> IO a) -> IO ()
modifyStoredIORef store f = withStore store $ \ref -> do
    v <- readIORef ref
    f v >>= writeIORef ref

shutdownApp = pure ()