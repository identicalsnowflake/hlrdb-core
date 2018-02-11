-- | An HSet is a sub-hash table in Redis, indexed by both a key and a subkey.

module HLRDB.Structures.HSet where

import Data.Functor.Identity
import Database.Redis as Redis
import HLRDB.Components.RedisPrimitives
import HLRDB.Util


-- | Retrieve all elements of an HSet
hsetGetAll :: RedisStructure (HSET s) a b -> a -> Redis [ (s,b) ]
hsetGetAll p@(RHSet (E _ _ d) (HSET _ ds)) =
 (fmap . fmap) (\(s,b) -> (ds s , d (pure b))) . unwrap . hgetall . primKey p

-- | Lookup via key and subkey
hsetGet :: RedisStructure (HSET s) a b -> a -> s -> Redis (Maybe b)
hsetGet p@(RHSet (E _ _ d) (HSET e _)) k =
  (fmap . fmap) (d . pure) . unwrap . hget (primKey p k) . e

-- | Set via key and subkey
hsetSet :: RedisStructure (HSET s) a b -> a -> s -> b -> Redis (ActionPerformed Creation)
hsetSet p@(RHSet (E _ eb _) (HSET e _)) k s =
  unwrapCreatedBool . hset (primKey p k) (e s) . runIdentity . eb


-- | Delete via key and subkeys
hsetDel :: (Traversable t) => RedisStructure (HSET s) a b -> a -> t s -> Redis (ActionPerformed Deletion)
hsetDel p@(RHSet _ (HSET e _)) k =
  fmap Deleted .fixEmpty' (unwrap . hdel (primKey p k)) e

-- | Set a value only if it does not currently exist in the HSET
hsetSetNE :: RedisStructure (HSET s) a b -> a -> s -> b -> Redis (ActionPerformed Creation)
hsetSetNE p@(RHSet (E _ eb _) (HSET e _)) k s =
  unwrapCreatedBool . hsetnx (primKey p k) (e s) . runIdentity . eb

