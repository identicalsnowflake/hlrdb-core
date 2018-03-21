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

import Database.Redis (Redis,MonadRedis,liftRedis,Cursor,cursor0)

import HLRDB.Primitives.Aggregate
import HLRDB.Primitives.Redis

import HLRDB.Structures.Basic
import HLRDB.Structures.List
import HLRDB.Structures.HSet
import HLRDB.Structures.Set
import HLRDB.Structures.SSet

