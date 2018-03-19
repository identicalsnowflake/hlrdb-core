-- | SortedSets, like lists, support automatic cardinality management when provided a @TrimScheme@.
-- HLRDB exports a more opinionated and less easy-to-make-mistakes API than Redis supports. Scores are golf-(or race) style, where lower numbers are better. The API is setup to make retrieving the best items and discarding the worst items natural, rather than trying to remember which direction the data is sorted in.
-- 
-- You should ensure that your Haskell @Eq@ instances respect the equality induced by your encoding scheme, i.e., that @a == b ~ encode a == encode b@.

module HLRDB.Structures.SSet
       (
         HLRDB.Structures.SSet.zadd
       , HLRDB.Structures.SSet.zscore
       , HLRDB.Structures.SSet.zupdate
       , HLRDB.Structures.SSet.zbest
       , HLRDB.Structures.SSet.zworst
       , HLRDB.Structures.SSet.zmember
       , HLRDB.Structures.SSet.zrank
       , HLRDB.Structures.SSet.zrevrank
       , HLRDB.Structures.SSet.zrem
       , HLRDB.Structures.SSet.zincrby
       , HLRDB.Structures.SSet.zcard
       , HLRDB.Structures.SSet.zscan
       ) where

import Data.Functor.Identity
import Control.Lens
import Data.Maybe (isJust)
import Database.Redis as Redis
import HLRDB.Components.RedisPrimitives
import HLRDB.Internal


trimInternal :: RedisSSet a b -> a -> Integer -> Redis ()
trimInternal p k limit =
  ignore $ unwrap $ zremrangebyrank (primKey p k) 0 ((limit * (-1)) - 1)

trimSortedSet :: RedisSSet a b -> a -> Integer -> Redis ()
trimSortedSet (RSortedSet _ Nothing) _ _ = pure ()
trimSortedSet _ _ 0 = pure ()
trimSortedSet p@(RSortedSet _ (Just (limit, 1.0))) k _ =
  trimInternal p k limit
trimSortedSet p@(RSortedSet _ (Just (limit, basep))) k count =
  let probability = 1.0 - (1.0 - basep) ^ count in
  ignore $ probIO probability $ trimInternal p k limit


-- | Lookup an element's score
zscore :: RedisSSet a b -> a -> b -> Redis (Maybe Double)
zscore p@(RSortedSet (E _ e _) _) k =
  unwrap . Redis.zscore (primKey p k) . runIdentity . e

-- | Add items and scores
zadd :: (Traversable t) => RedisSSet a b -> a -> t (Double,b) -> Redis (ActionPerformed Creation)
zadd p@(RSortedSet (E _ e _) _) k t = do
  i <- fixEmpty' (unwrap . Redis.zadd (primKey p k)) (over _2 (runIdentity . e)) t
  trimSortedSet p k i
  pure $ FreshlyCreated (fromIntegral i)

-- | Read the scores from Redis, apply the given trasformation, and write the resulting data
zupdate :: RedisSSet a b -> a -> (Double -> Double) -> Redis ()
zupdate p k f = let key = primKey p k in
  unwrap (zrangeWithscores key 0 (-1)) >>=
  fixEmpty (ignore . unwrap . Redis.zadd key . fmap (\(bs,s) -> (f s , bs))) id

-- | Retrieve the given range of best-performing elements. Range is inclusive, just as with Haskell's [ 1 .. 5 ] notation, and it is 0-based, which means [ 0 .. 4 ] is what corresponds to the English phrase "Best 5."
zbest :: RedisSSet a b -> a -> Integer -> Integer -> Redis [ b ]
zbest p@(RSortedSet (E _ _ d) _) k s e =
  fmap (d . pure) <$> unwrap (zrange (primKey p k) s e)

-- | Retrieve the given range of worst-performing elements. Range is inclusive, just as with Haskell's [ 1 .. 5 ] notation, and it is 0-based, which means [ 0 .. 4 ] is what corresponds to the English phrase "Worst 5."
zworst :: RedisSSet a b -> a -> Integer -> Integer -> Redis [ b ]
zworst p@(RSortedSet (E _ _ d) _) k s e =
  fmap (d . pure) <$> unwrap (zrevrange (primKey p k) s e)

-- | Test if an object is a member of the set.
zmember :: RedisSSet a b -> a -> b -> Redis Bool
zmember p@(RSortedSet (E _ e _) _) k =
  fmap isJust . unwrap . Redis.zrank (primKey p k) . runIdentity . e

-- | Calculate the rank of an item. The best item has rank 0.
zrank :: RedisSSet a b -> a -> b -> Redis (Maybe Integer)
zrank p@(RSortedSet (E _ e _) _) k =
  unwrap . Redis.zrank (primKey p k) . runIdentity . e

-- | Calculate the rank of an item starting from the end, e.g., the worst item has rank 0.
zrevrank :: RedisSSet a b -> a -> b -> Redis (Maybe Integer)
zrevrank p@(RSortedSet (E _ e _) _) k =
  unwrap . Redis.zrevrank (primKey p k) . runIdentity . e

-- | Increment an item's score. If the item does not already exist, it is inserted with the given score.
zincrby :: RedisSSet a b -> a -> (Integer,b) -> Redis Double
zincrby p@(RSortedSet (E _ e _) _) k (s,b) = do
  v <- unwrap $ Redis.zincrby (primKey p k) s $ runIdentity . e $ b
  trimSortedSet p k 1
  pure v

-- | Remove items from a sorted set
zrem :: (Traversable t) => RedisSSet a b -> a -> t b -> Redis (ActionPerformed Deletion)
zrem p@(RSortedSet (E _ e _) _) k =
  fmap Deleted <$> fixEmpty' (unwrap . Redis.zrem (primKey p k)) (runIdentity . e)

-- | The cardinality of a sorted set
zcard :: RedisSSet a b -> a -> Redis Integer
zcard p = unwrap . Redis.zcard . primKey p

-- | Use a cursor to iterate a collection.
zscan :: RedisSSet a b -> a -> Cursor -> Redis (Maybe Cursor , [ (b , Double) ])
zscan p@(RSortedSet (E _ _ d) _) k =
  let f (x,s) = (d (pure x) , s) in
  unwrapCursor (fmap f) . Redis.zscan (primKey p k)

