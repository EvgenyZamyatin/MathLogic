
module Ordinal where

data Ordinal = O (Ordinal, Int) Ordinal
							|A Int

