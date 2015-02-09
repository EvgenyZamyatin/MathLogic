module Util where

import System.IO  
import Control.Monad
import System.Directory

data Pair a b = 
	Pair {
				frs :: a
			, scn :: b
			} deriving (Show, Eq, Ord)

myCreateDirectory :: String -> IO ()
myCreateDirectory name = doesDirectoryExist name >>= (\b->if b then return () else createDirectory name) 
	