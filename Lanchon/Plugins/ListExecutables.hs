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

-- | Lanchon plugin that lists all elements in the directories of the system path
-- @Author Ernesto Rodriguez
{-# LANGUAGE OverloadedStrings #-}
module Lanchon.Plugins.ListExecutables where

import System.Directory (getDirectoryContents)
import Control.Monad (foldM)
import System.Posix.Env (getEnv)
import Data.ByteString.Char8 (pack)
import Data.ByteString as BS (concat)
import Text.Regex.TDFA ((=~))
import Lanchon.Core.Base

pathEnv = "PATH"

split d str = let
  split' (x:xs) tmp
    | x == d = tmp : (split' xs "")
    | otherwise = (split' xs (tmp ++ [x]))
  split' _ "" = []
  split' _ tmp = [tmp]
  in
   split' str ""
   
getPathContents path = do
  let
    processContents acc d = do
      res <- catch (getDirectoryContents d) (\_-> return [])
      return $ acc ++ res
  foldM processContents [] path
             
pathPlugin = do
  folders <- getEnv pathEnv >>= \(Just f) -> return $ split ':' f
  elements <- getPathContents folders >>= return . (map pack)
  let
    filterPath arg = do
      let
        matches = take 20 $ filter (\x -> x =~ (BS.concat [arg,".*"])) elements
        results = map (\m -> (m,return ())) matches
      return results
  return $ Plugin ".*" filterPath