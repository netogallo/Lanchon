{-# LANGUAGE OverloadedStrings #-}
-- This file is part of Lanchon.

-- Lanchon is free software: you can redistribute it and/or modify
-- it under the terms of the GNU General Public License as published by
-- the Free Software Foundation, either version 3 of the License, or
-- (at your option) any later version.

-- Lanchon is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
-- GNU General Public License for more details.

-- You should have received a copy of the GNU General Public License
-- along with Lanchon.  If not, see <http://www.gnu.org/licenses/>.

-- | This modlule handles all the GUI rendering and input handling for Lanchon.
-- @Author Ernesto Rodriguez
module Lanchon.Core.Base where 

import Control.Concurrent.STM (atomically)
import Control.Concurrent.STM.TChan (newTChanIO,readTChan,writeTChan,tryReadTChan)
import Control.Monad (mapM_)
import Control.Concurrent (forkIO,threadDelay,killThread)
import Text.Regex.TDFA ((=~))
import Data.ByteString (ByteString)
import Data.ByteString.Char8 (pack)

-- | Datatype that represents a plugin for Lanchon
data Plugin = Plugin {
  regexp :: ByteString, -- | The regular expression that activates this plugin
  matcher :: ByteString -> IO [(ByteString,IO ())] -- | Action to be executed given the user input 
  }
              
-- | Represents an action that polls for results for all the active plugins
-- and an action that updates the user input in Lanchon
data PluginRunner = PluginRunner {
  inputUpdater :: ByteString -> IO (), -- | Action to update the user input
  outputStream :: IO (Maybe [(ByteString,IO ())]) -- | Stream of results from lanchon plugins
  }
                    
-- | Buld a PluginRunner for the given list of plugins. This runner executes each plugin
-- in a different thread
buildRunner plugins = do
  chan <- newTChanIO -- Channel to output results
  pidChan <- newTChanIO -- Channel to keep list of running threads so they can be killed
  updateFun <- return $ runPlugins pidChan chan
  readFun <- return $ atomically $ tryReadTChan chan
  return $ PluginRunner updateFun readFun
              
  where
    killThreads chan = do
      tId <- atomically $ tryReadTChan chan
      case tId of
        Just tId' -> killThread tId' >> killThreads chan
        Nothing -> return ()
    runPlugins pidChan chan text = killThreads pidChan >> putStrLn "running search" >> mapM_ (runPlugin chan pidChan text) plugins
    runPlugin chan pidChan text plugin = if text =~ regexp plugin then
                              do
                                tid <- forkIO $ do
                                  res <- (matcher plugin) text
                                  atomically $ writeTChan chan res 
                                atomically $ writeTChan pidChan tid
                                return $ Just tid
                            else
                              return Nothing
  
samplePlugin :: Int -> ByteString -> IO [(ByteString,IO ())]
samplePlugin time _ = do
  threadDelay $ time * 100000
  return [(pack $ show time,return ())]
  
samplePlugins = [Plugin ".*" (samplePlugin x) | x <- [1 .. 10]]