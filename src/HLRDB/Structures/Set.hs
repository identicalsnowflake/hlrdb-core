-- | Important: it is required that the Hashable and Eq instances used for HashSet respect
-- whatever is used for serialization, since Redis will decide equality only on serialized values.

module HLRDB.Structures.Set where

import Data.Functor.Identity
import Data.Hashable
import Data.HashSet as S
import Database.Redis as Redis
import HLRDB.Components.RedisPrimitives
import HLRDB.Util


-- | Retrieve the elements of a set from Redis
setMembers :: (Eq b, Hashable b) => RedisStructure SET a b -> a -> Redis (HashSet b)
setMembers p@(RSet (E _ _ d)) =
  fmap (S.fromList . fmap (d . pure)) . unwrap . smembers . primKey p

-- | Test if an item is a member of a set
setIsMember :: RedisStructure SET a b -> a -> b -> Redis Bool
setIsMember p@(RSet (E _ e _)) k =
  unwrap . sismember (primKey p k) . runIdentity . e

-- | Add an item to a set
setInsert :: RedisStructure SET a b -> a -> b -> Redis ()
setInsert p k b = setInsertMany p k [ b ]

-- | Add many items to a set
setInsertMany :: (Traversable t) => RedisStructure SET a b -> a -> t b -> Redis ()
setInsertMany p@(RSet (E _ e _)) k =
  fixEmpty (ignore . unwrap . sadd (primKey p k)) (runIdentity . e)

-- | Remove an item from a set
setRemove :: RedisStructure SET a b -> a -> b -> Redis ()
setRemove p k b = setRemoveMany p k [ b ]

-- | Remove many items from a set
setRemoveMany :: (Traversable t) => RedisStructure SET a b -> a -> t b -> Redis ()
setRemoveMany p@(RSet (E _ e _)) k =
  fixEmpty (ignore . unwrap . srem (primKey p k)) (runIdentity . e)

-- | Retrieve the cardinality of a set
setCardinality :: RedisStructure SET a b -> a -> Redis Integer
setCardinality p = unwrap . scard . primKey p

-- | Retrieve a random element from a set
setRandom :: RedisStructure SET a b -> a -> Redis (Maybe b)
setRandom p@(RSet (E _ _ d)) =
  (fmap . fmap) (d . pure) . unwrap . srandmember . primKey p

