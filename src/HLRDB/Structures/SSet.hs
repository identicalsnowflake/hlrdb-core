-- | SortedSets use a probabalistic trimming scheme. If you don't like uncertainty, you may simply use probability 1.0 or not specify any max size at all. Otherwise, each item added will have the given probability of executing a trim operation.

-- | HLRDB exports a more limited and less easy-to-make-mistakes API than Redis actually supports. Scores are golf-(or race) style, where lower numbers are better. The API is setup to make retrieving the best items and discarding the worst items natural, rather than trying to remember which direction the data is sorted in.

module HLRDB.Structures.SSet
       (
         ssetUpdateScores
       , ssetGetBest
       , ssetGetWorst
       , ssetMember
       , ssetAdd
       , ssetScore
       , ssetGetRank
       , ssetGetReverseRank
       , ssetAddMany
       , ssetRemove
       , ssetRemoveMany
       , ssetIncrBy
       , ssetCardinality
       ) where

import Data.Functor.Identity
import Control.Lens
import Control.Monad.IO.Class
import Data.Maybe (isJust)
import Database.Redis as Redis
import System.Random
import HLRDB.Components.Indexes
import HLRDB.Components.RedisPrimitives
import HLRDB.Util


trimSortedSet :: RedisStructure SORTEDSET a b -> a -> Integer -> Redis ()
trimSortedSet (RSortedSet _ Nothing) _ _ = pure ()
trimSortedSet _ _ 0 = pure ()
trimSortedSet p@(RSortedSet _ (Just (limit, basep))) k count =
  let probability = 1.0 - (1.0 - maybe 1.0 id basep) ^ count in
  ignore $ performActionWithProbability probability
    $ unwrap $ zremrangebyrank (primKey p k) 0 ((limit * (-1)) - 1)
  where
    performActionWithProbability :: (MonadIO m) => Double -> m a -> m (Maybe a)
    performActionWithProbability pr a = do
      r :: Double <- liftIO $ randomRIO (0, 1.0)
      if r <= pr
         then Just <$> a
         else return Nothing

-- | Read the scores from Redis, apply the given trasformation, and write the resulting data
ssetUpdateScores :: RedisStructure SORTEDSET a b -> a -> (Double -> Double) -> Redis ()
ssetUpdateScores p k f = let key = primKey p k in
  unwrap (zrangeWithscores key 0 (-1)) >>=
  fixEmpty (ignore . unwrap . zadd key . fmap (\(bs,s) -> (f s , bs))) id

-- | Retrieve the given range of best-performing elements, e.g., fromTo 1 5 are the 5 best performers
ssetGetBest :: RedisStructure SORTEDSET a b -> a -> Indexes -> Redis [ b ]
ssetGetBest p@(RSortedSet (E _ _ d) _) k (Indexes (s , e)) =
  fmap (d . pure) <$> unwrap (zrange (primKey p k) s e)

-- | Retrieve the given range of worst-performing elements, e.g., fromTo 1 5 are the 5 worst performers
ssetGetWorst :: RedisStructure SORTEDSET a b -> a -> Indexes -> Redis [ b ]
ssetGetWorst p@(RSortedSet (E _ _ d) _) k (Indexes (s , e)) =
  fmap (d . pure) <$> unwrap (zrevrange (primKey p k) s e)

-- | Test if an object is a member of the set. Note that this uses the *encoded* equality, which may not (but typically does) coincide with the Haskell Eq instance.
ssetMember :: RedisStructure SORTEDSET a b -> a -> b -> Redis Bool
ssetMember p@(RSortedSet (E _ e _) _) k =
  fmap isJust . unwrap . zrank (primKey p k) . runIdentity . e

-- | Calculate the rank of an item
ssetGetRank :: RedisStructure SORTEDSET a b -> a -> b -> Redis (Maybe Integer)
ssetGetRank p@(RSortedSet (E _ e _) _) k =
  unwrap . zrank (primKey p k) . runIdentity . e

-- | Calculate the rank of an item starting from the end, e.g., 1 being the worst
ssetGetReverseRank :: RedisStructure SORTEDSET a b -> a -> b -> Redis (Maybe Integer)
ssetGetReverseRank p@(RSortedSet (E _ e _) _) k =
  unwrap . zrevrank (primKey p k) . runIdentity . e

-- | Lookup an element's score
ssetScore :: RedisStructure SORTEDSET a b -> a -> b -> Redis (Maybe Double)
ssetScore p@(RSortedSet (E _ e _) _) k =
  unwrap . zscore (primKey p k) . runIdentity . e

-- | Add an item and score to the sorted set
ssetAdd :: RedisStructure SORTEDSET a b -> a -> (Double,b) -> Redis (ActionPerformed Creation)
ssetAdd p k v = ssetAddMany p k [ v ]

-- | Add many pairs of items and scores to the sorted set
ssetAddMany :: (Traversable t) => RedisStructure SORTEDSET a b -> a -> t (Double,b) -> Redis (ActionPerformed Creation)
ssetAddMany p@(RSortedSet (E _ e _) _) k t = do
  i <- fixEmpty' (unwrap . zadd (primKey p k)) (over _2 (runIdentity . e)) t
  trimSortedSet p k i
  pure $ FreshlyCreated (fromIntegral i)

-- | Increment an item's score
ssetIncrBy :: RedisStructure SORTEDSET a b -> a -> (Integer,b) -> Redis Double
ssetIncrBy p@(RSortedSet (E _ e _) _) k (s,b) = do
  v <- unwrap $ zincrby (primKey p k) s $ runIdentity . e $ b
  trimSortedSet p k 1
  pure v

-- | Remove an item from a sorted set
ssetRemove :: RedisStructure SORTEDSET a b -> a -> b -> Redis (ActionPerformed Deletion)
ssetRemove p k b = ssetRemoveMany p k [ b ]

-- | Remove many items from a sorted set
ssetRemoveMany :: (Traversable t) => RedisStructure SORTEDSET a b -> a -> t b -> Redis (ActionPerformed Deletion)
ssetRemoveMany p@(RSortedSet (E _ e _) _) k =
  fmap Deleted <$> fixEmpty' (unwrap . zrem (primKey p k)) (runIdentity . e)

-- | The cardinality of a sorted set
ssetCardinality :: RedisStructure SORTEDSET a b -> a -> Redis Integer
ssetCardinality p = unwrap . zcard . primKey p

