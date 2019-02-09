-- | This package is an abstract API for modeling high-level Redis functionality. It makes no opinion on either serialization or key construction, which means there is a fair amount of work to do to make this library usable. If you do not want to do this work and don't mind these decisions being made for you, you may use the HLRDB library, which gives you a ready-to-go API.
-- 
-- This package depends on the Hedis library for low-level Redis bindings, but it is not recommended to import them together in the same module, as there are many name conflicts, since much of what HLRDB does is simply assign types to commands. Despite this, much of the HLRDB API does differ entirely, with many commands added, removed, merged, or simply rethought from a Haskell perspective.
--
-- When using this package, you should always ensure that your Eq instances respect the induced equality via whatever serialization mechanism you've specified, since many commands perform comparisons in Redis directly.

module HLRDB.Core
       (
         -- * Basic
         get
       , liftq
       , mget
       , set
       , set'
       , liftqs
       , mset
       , incr
       , incrby
       , decr
       , decrby

         -- * List         
       , lrange
       , lprepend
       , lappend
       , lpop
       , lrem
       , llen

         -- * HSet
       , hgetall
       , hget
       , hmget
       , hset
       , hmset
       , hdel
       , hsetnx
       , hscan

         -- * Set
       , smembers
       , sismember
       , sadd
       , srem
       , scard
       , srandmember
       , sscan

         -- * SSet
       , zadd
       , zscore
       , zupdate
       , zbest
       , zworst
       , zmember
       , zrank
       , zrevrank
       , zrem
       , zincrby
       , zcard
       , zscan
       , zrangebyscore

         -- * Universal
       , HLRDB.Core.del
       , HLRDB.Core.persist
       , HLRDB.Core.expire
       , HLRDB.Core.expireat
       
         -- * Re-exports from hedis
       , Redis
       , MonadRedis
       , liftRedis
       , Cursor
       , cursor0

         -- * HLRDB Primitive re-exports
       
       , module HLRDB.Primitives.Aggregate
       , module HLRDB.Primitives.Redis
       
       ) where

import Data.Time
import Data.Time.Clock.POSIX
import Database.Redis (Redis,MonadRedis,liftRedis,Cursor,cursor0,del,persist,expire,expireat)

import HLRDB.Primitives.Aggregate
import HLRDB.Primitives.Redis
import HLRDB.Internal
import HLRDB.Structures.Basic
import HLRDB.Structures.List
import HLRDB.Structures.HSet
import HLRDB.Structures.Set
import HLRDB.Structures.SSet


-- | Delete all data for the given keys in Redis
del :: (Traversable t , MonadRedis m) => RedisStructure v a b -> t a -> m (ActionPerformed Deletion)
del p =
    fmap Deleted
  . fixEmpty' (unwrap . Database.Redis.del) (primKey p)

-- | Discard any pending expirations of this key. Returns True if the key both exists and had a timeout which was removed by the command.
persist :: MonadRedis m => RedisStructure v a b -> a -> m Bool
persist p =
  liftRedis . unwrap . Database.Redis.persist . primKey p

-- | Expire after a given amount of time (in seconds). Returns True if the key existed and a timeout was set.
expire :: MonadRedis m => RedisStructure v a b -> a -> Integer -> m Bool
expire p k =
  liftRedis . unwrap . Database.Redis.expire (primKey p k)

-- | Expire at a given timestamp. Returns True if the key existed and a timeout was set.
expireat :: MonadRedis m => RedisStructure v a b -> a -> UTCTime -> m Bool
expireat p k =
    liftRedis
  . unwrap
  . Database.Redis.expireat (primKey p k)
  . round
  . utcTimeToPOSIXSeconds

