-- | Important: it is required that the Hashable and Eq instances used for HashSet respect
-- whatever is used for serialization, since Redis will decide equality only on serialized values.

module HLRDB.Structures.Set where

import Data.Hashable
import Data.HashSet as S
import Database.Redis as Redis
import HLRDB.Primitives.Redis
import HLRDB.Internal


-- | Retrieve the elements of a set from Redis
smembers :: (MonadRedis m , Eq b , Hashable b) => RedisSet a b -> a -> m (HashSet b)
smembers p@(RSet (E _ _ d)) =
    fmap (S.fromList . fmap (d . pure))
  . unwrap
  . Redis.smembers
  . primKey p

-- | Test if an item is a member of a set
sismember :: MonadRedis m => RedisSet a b -> a -> b -> m Bool
sismember p@(RSet (E _ e _)) k =
    unwrap
  . Redis.sismember (primKey p k)
  . runIdentity
  . e

-- | Add items to a set
sadd :: (MonadRedis m , Traversable t) => RedisSet a b -> a -> t b -> m ()
sadd p@(RSet (E _ e _)) k =
  fixEmpty (ignore . unwrap . Redis.sadd (primKey p k)) (runIdentity . e)

-- | Remove items from a set
srem :: (MonadRedis m , Traversable t) => RedisSet a b -> a -> t b -> m ()
srem p@(RSet (E _ e _)) k =
  fixEmpty (ignore . unwrap . Redis.srem (primKey p k)) (runIdentity . e)

-- | Retrieve the cardinality of a set
scard :: MonadRedis m => RedisSet a b -> a -> m Integer
scard p =
    unwrap
  . Redis.scard
  . primKey p

-- | Retrieve a random element from a set. The underlying Redis primitive uses a poor but efficient distribution, biased by the underlying hash bucket allocation.
srandmember :: MonadRedis m => RedisSet a b -> a -> m (Maybe b)
srandmember p@(RSet (E _ _ d)) =
    (fmap . fmap) (d . pure)
  . unwrap
  . Redis.srandmember
  . primKey p

-- | Retrieve up to N unique random elements, limited by the cardinality of the set.
srandmemberN :: MonadRedis m => RedisSet a b -> Integer -> a -> m [ b ]
srandmemberN p@(RSet (E _ _ d)) n =
    (fmap . fmap) (d . pure)
  . unwrap
  . flip Redis.srandmemberN n
  . primKey p

-- | Use a cursor to iterate a collection
sscan :: MonadRedis m => RedisSet a b -> a -> Cursor -> m (Maybe Cursor , [ b ])
sscan p@(RSet (E _ _ d)) k =
    unwrapCursor (fmap (d . pure))
  . Redis.sscan (primKey p k)

