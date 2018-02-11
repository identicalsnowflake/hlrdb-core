-- | Basic storage is simply a key-value lookup in Redis. Multiple lookups can be combined into a single query via aggregation. Use liftq to lift a path to a query, then combine queries using the combinators in Aggregate, and reify them back into the Redis monad with @mget@.

module HLRDB.Structures.Basic where

import Database.Redis as Redis
import HLRDB.Components.Aggregate
import HLRDB.Components.RedisPrimitives
import HLRDB.Util
import Data.ByteString.Char8 (pack)

-- | Simple get command
get :: RedisStructure (BASIC w) a b -> a -> Redis b
get (RKeyValue (E k _ d)) a = Redis.get (k a) >>= \case
  Left e -> fail (show e)
  Right r -> pure (d r)
get (RKeyValueInteger k _ d) a = Redis.get (k a) >>= \case
  Left e -> fail (show e)
  Right r -> pure $ d . fromIntegral $ decodeMInteger r

-- | Construct an @mget@ query. You may combine many of these together to create complex queries. Use @mget@ to execute the query back in the Redis monad.
liftq :: RedisStructure (BASIC w) a b -> a ⟿ b
liftq (RKeyValue (E k _ d)) = T $ \f -> fmap d . f . k
liftq (RKeyValueInteger k _ d) = T $ \f -> fmap (d . fromIntegral . decodeMInteger) . f . k

-- | Set a value for a given key
set :: RedisStructure (BASIC w) a b -> a -> b -> Redis ()
set (RKeyValue (E k e _)) a b = case e b of
  Just bs -> ignore $ Redis.set (k a) bs
  Nothing -> ignore $ del [ k a ]
set (RKeyValueInteger k e _) a i = ignore $ Redis.set (k a) (pack $ show (e i))

-- | Convenient alias for setting a value for an optional path
set' :: RedisBasic a (Maybe b) -> a -> b -> Redis ()
set' (RKeyValue (E k e _)) a b = case e (Just b) of
  Just bs -> ignore $ Redis.set (k a) bs
  Nothing -> ignore $ del [ k a ]

-- | Increment an Integer in Redis. Empty values are treated as 0.
incrKey :: RedisIntegral a b -> a -> Redis b
incrKey (RKeyValueInteger p _ d) = fmap d . unwrap . incr . p

-- | Increment an Integer in Redis by a specific amount. Empty values are treated as 0.
incrKeyBy :: RedisIntegral a b -> a -> b -> Redis b
incrKeyBy (RKeyValueInteger p e d) k = fmap d . (unwrap . incrby (p k)) . e

-- | Decrement an Integer in Redis. Empty values are treated as 0.
decrKey :: RedisIntegral a b -> a -> Redis b
decrKey (RKeyValueInteger p _ d) = fmap d . unwrap . decr . p

-- | Decrement an Integer in Redis by a specific amount. Empty values are treated as 0.
decrKeyBy :: RedisIntegral a b -> a -> b -> Redis b
decrKeyBy (RKeyValueInteger p e d) k = fmap d . unwrap . decrby (p k) . e

