-- | An HSet is a sub-hash table in Redis, indexed by both a key and a subkey.

module HLRDB.Structures.HSet where

import Data.Functor.Identity
import Database.Redis as Redis
import HLRDB.Components.RedisPrimitives
import HLRDB.Util


-- | Retrieve all elements of an HSet
hgetall :: RedisHSet a s b -> a -> Redis [ (s,b) ]
hgetall p@(RHSet (E _ _ d) (HSET _ ds)) =
 (fmap . fmap) (\(s,b) -> (ds s , d (pure b))) . unwrap . Redis.hgetall . primKey p

-- | Lookup via key and subkey
hget :: RedisHSet a s b -> a -> s -> Redis (Maybe b)
hget p@(RHSet (E _ _ d) (HSET e _)) k =
  (fmap . fmap) (d . pure) . unwrap . Redis.hget (primKey p k) . e

-- | Set via key and subkey
hset :: RedisHSet a s b -> a -> s -> b -> Redis (ActionPerformed Creation)
hset p@(RHSet (E _ eb _) (HSET e _)) k s =
  unwrapCreatedBool . Redis.hset (primKey p k) (e s) . runIdentity . eb


-- | Delete via key and subkeys
hdel :: (Traversable t) => RedisHSet a s b -> a -> t s -> Redis (ActionPerformed Deletion)
hdel p@(RHSet _ (HSET e _)) k =
  fmap Deleted .fixEmpty' (unwrap . Redis.hdel (primKey p k)) e

-- | Set a value only if it does not currently exist in the HSET
hsetnx :: RedisHSet a s b -> a -> s -> b -> Redis (ActionPerformed Creation)
hsetnx p@(RHSet (E _ eb _) (HSET e _)) k s =
  unwrapCreatedBool . Redis.hsetnx (primKey p k) (e s) . runIdentity . eb

