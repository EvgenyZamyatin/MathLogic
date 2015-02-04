module Util where

data Pair a b = 
	Pair {
				frs :: a
			, scn :: b
			} deriving (Show, Eq, Ord)


