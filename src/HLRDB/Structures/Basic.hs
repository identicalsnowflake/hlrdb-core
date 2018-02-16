-- | Basic storage is simply a key-value lookup in Redis.

module HLRDB.Structures.Basic where

import Database.Redis as Redis
import HLRDB.Components.Aggregate
import HLRDB.Components.RedisPrimitives
import HLRDB.Util
import Data.ByteString.Char8 (pack)

-- | Simple get command. Works on @RedisBasic a b@ and @RedisIntegral a b@.
get :: RedisStructure (BASIC w) a b -> a -> Redis b
get (RKeyValue (E k _ d)) a = Redis.get (k a) >>= \case
  Left e -> fail (show e)
  Right r -> pure (d r)
get (RKeyValueInteger k _ d) a = Redis.get (k a) >>= \case
  Left e -> fail (show e)
  Right r -> pure $ d . fromIntegral $ decodeMInteger r

-- | Construct an @mget@ query. You may combine many of these together to create complex queries. Use @mget@ to execute the query back in the Redis monad. Works on @RedisBasic a b@ and @RedisIntegral a b@.
liftq :: RedisStructure (BASIC w) a b -> a ⟿ b
liftq (RKeyValue (E k _ d)) = T $ \f -> fmap d . f . k
liftq (RKeyValueInteger k _ d) = T $ \f -> fmap (d . fromIntegral . decodeMInteger) . f . k

-- | Reify a (⟿) query into the Redis monad via a single mget command
mget :: a ⟿ b -> a -> Redis b
mget = runT mget'
  where
    mget' [] = pure []
    mget' xs = Redis.mget xs >>= \case
      Left e -> fail (show e)
      Right vs -> pure vs

-- | Set a value for a given key. Works on @RedisBasic a b@ and @RedisIntegral a b@.
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
incr :: RedisIntegral a b -> a -> Redis b
incr (RKeyValueInteger p _ d) = fmap d . unwrap . Redis.incr . p

-- | Increment an Integer in Redis by a specific amount. Empty values are treated as 0.
incrby :: RedisIntegral a b -> a -> b -> Redis b
incrby (RKeyValueInteger p e d) k = fmap d . (unwrap . Redis.incrby (p k)) . e

-- | Decrement an Integer in Redis. Empty values are treated as 0.
decr :: RedisIntegral a b -> a -> Redis b
decr (RKeyValueInteger p _ d) = fmap d . unwrap . Redis.decr . p

-- | Decrement an Integer in Redis by a specific amount. Empty values are treated as 0.
decrby :: RedisIntegral a b -> a -> b -> Redis b
decrby (RKeyValueInteger p e d) k = fmap d . unwrap . Redis.decrby (p k) . e

