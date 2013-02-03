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
module Lanchon.UI.Launcher where

import Control.Monad
import Graphics.UI.WX hiding (Event)
import Graphics.UI.WXCore
import Lanchon.Core.Base
import Data.ByteString.Char8 (pack,unpack)
import Data.ByteString (ByteString)
import Reactive.Banana
import Reactive.Banana.WX
import Control.Concurrent (yield)

-- Plugins
import Lanchon.Plugins.ListExecutables (pathPlugin)

buildGui plugins = do
  f <- frame []
  cmdField <- textCtrlCreate f 0 "" (Rect 0 0 500 30) 0
  resultList <- listCtrlCreate f 0 (Rect 0 0 500 500) 0
  t <- timer f [ interval := 100 ]
  let
    g = grid 1 1 [[widget cmdField,widget resultList]]
  runner <- buildRunner plugins
  set f [layout := g]
  inputL <- compile $ inputListener cmdField resultList runner
  actuate inputL
  outputL <- compile $ resultListener resultList t runner
  actuate outputL
  
inputListener cmdField resultList runner = do
  cmdInput <- eventText cmdField  
  let notifyChange text = do
        listCtrlDeleteAllItems resultList
        (inputUpdater runner) (pack text)
        return ()
  reactimate $ notifyChange <$> cmdInput
  
resultListener resultList t runner = do
  resultStream' <- fromPoll $ outputStream runner
  resultStream <- changes resultStream'  >>= return . filterJust
  etick <- event0 t command
  let
    addItem (text,_) = do
      i <- listItemCreate
      listItemSetText i $ unpack text
      listCtrlInsertItem resultList i
    addItems items = mapM_ addItem items
  reactimate $ addItems <$> resultStream

runGui = do
  plugins <- mapM (\x -> x >>= return) [pathPlugin]
  start $ buildGui plugins
