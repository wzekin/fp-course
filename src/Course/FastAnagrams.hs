{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Course.FastAnagrams where

import Course.Core
import Course.Functor
import Course.List
import qualified Data.Set as S

-- Return all anagrams of the given string
-- that appear in the given dictionary file.
fastAnagrams :: Chars -> FilePath -> IO (List Chars)
fastAnagrams name f =
  (flip (filter . flip S.member) (permutations name) .
   S.fromList . hlist . lines) <$>
  readFile f

newtype NoCaseString = NoCaseString
  { ncString :: Chars
  }

instance Eq NoCaseString where
  (==) = (==) `on` map toLower . ncString

instance Show NoCaseString where
  show = show . ncString
