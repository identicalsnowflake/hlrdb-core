-- | Ranges in Redis

module HLRDB.Components.Indexes where

-- | A newtype with some common cases to make Redis's list indexing cleaner
newtype Indexes = Indexes (Integer, Integer)

-- | All items in a list
{-# INLINE fullIndex #-}
fullIndex :: Indexes
fullIndex = Indexes (0, (-1))

-- | The first item in a list
{-# INLINE firstIndex #-}
firstIndex :: Indexes
firstIndex = Indexes (0,0)

-- | @fromTo 5 10@ will refers to items 5 - 10 (inclusive).
{-# INLINE fromTo #-}
fromTo :: Integer -> Integer -> Indexes
fromTo = curry Indexes

