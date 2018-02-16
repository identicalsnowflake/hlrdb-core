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
smembers :: (Eq b, Hashable b) => RedisSet a b -> a -> Redis (HashSet b)
smembers p@(RSet (E _ _ d)) =
  fmap (S.fromList . fmap (d . pure)) . unwrap . Redis.smembers . primKey p

-- | Test if an item is a member of a set
sismember :: RedisSet a b -> a -> b -> Redis Bool
sismember p@(RSet (E _ e _)) k =
  unwrap . Redis.sismember (primKey p k) . runIdentity . e

-- | Add items to a set
sadd :: (Traversable t) => RedisSet a b -> a -> t b -> Redis ()
sadd p@(RSet (E _ e _)) k =
  fixEmpty (ignore . unwrap . Redis.sadd (primKey p k)) (runIdentity . e)

-- | Remove items from a set
srem :: (Traversable t) => RedisSet a b -> a -> t b -> Redis ()
srem p@(RSet (E _ e _)) k =
  fixEmpty (ignore . unwrap . Redis.srem (primKey p k)) (runIdentity . e)

-- | Retrieve the cardinality of a set
scard :: RedisSet a b -> a -> Redis Integer
scard p = unwrap . Redis.scard . primKey p

-- | Retrieve a random element from a set
srandmember :: RedisSet a b -> a -> Redis (Maybe b)
srandmember p@(RSet (E _ _ d)) =
  (fmap . fmap) (d . pure) . unwrap . Redis.srandmember . primKey p

