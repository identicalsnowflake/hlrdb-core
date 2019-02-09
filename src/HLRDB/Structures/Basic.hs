-- | Basic storage is simply a key-value lookup in Redis.

module HLRDB.Structures.Basic where

import Database.Redis as Redis
import HLRDB.Primitives.Aggregate
import HLRDB.Primitives.Redis
import HLRDB.Internal
import Data.ByteString.Char8 (pack)
import qualified Data.HashMap.Strict as HM


-- | Simple get command. Works on @RedisBasic a b@ and @RedisIntegral a b@.
get :: MonadRedis m => RedisStructure (BASIC w) a b -> a -> m b
get (RKeyValue (E k _ d)) a = liftRedis $ Redis.get (k a) >>= \case
  Left e -> fail (show e)
  Right r -> pure (d r)
get (RKeyValueInteger k _ d) a = liftRedis $ Redis.get (k a) >>= \case
  Left e -> fail (show e)
  Right r -> pure $ d . fromIntegral $ decodeMInteger r

-- | Construct a query to be used with @mget@. You may combine many of these together to create complex queries. Use @mget@ to execute the query back in the Redis monad. Works on @RedisBasic a b@ and @RedisIntegral a b@.
liftq :: RedisStructure (BASIC w) a b -> a ⟿ b
liftq (RKeyValue (E k _ d)) = T $ \f -> fmap d . f . k
liftq (RKeyValueInteger k _ d) = T $ \f -> fmap (d . fromIntegral . decodeMInteger) . f . k

-- | Reify a (⟿) query into the Redis monad via a single mget command.
mget :: MonadRedis m => a ⟿ b -> a -> m b
mget = runT (liftRedis . mget')
  where
    mget' [] = pure []
    mget' xs = Redis.mget xs >>= \case
      Left e -> fail (show e)
      Right vs -> pure vs

-- | Set a value for a given key. Works on @RedisBasic a b@ and @RedisIntegral a b@.
set :: MonadRedis m => RedisStructure (BASIC w) a b -> a -> b -> m ()
set (RKeyValue (E k e _)) a b = liftRedis $ case e b of
  Just bs -> ignore $ Redis.set (k a) bs
  Nothing -> ignore $ del [ k a ]
set (RKeyValueInteger k e _) a i = liftRedis $ ignore $ Redis.set (k a) (pack $ show (e i))

-- | Convenient alias for setting a value for an optional path
set' :: MonadRedis m => RedisBasic a (Maybe b) -> a -> b -> m ()
set' (RKeyValue (E k e _)) a b = liftRedis $ case e (Just b) of
  Just bs -> ignore $ Redis.set (k a) bs
  Nothing -> ignore $ del [ k a ]

-- | Construct a query to be used with @mset@. The @MSET@ type is a @Monoid@, so you may combine many of these together before executing the batch with the @mset@ command.
liftqs :: RedisStructure (BASIC w) a b -> (a , b) -> MSET
liftqs (RKeyValue (E k e _)) (a , b) = MSET $ (<>) [ (k a , e b) ]
liftqs (RKeyValueInteger k e _) (a , b) = MSET $ (<>) [ (k a , Just $ pack (show (e b))) ]

-- | Execute a @MSET@ query.
mset :: MonadRedis m => MSET -> m ()
mset = go . flip runMSET []
  where
    -- need this hashmap to/from in order to make sure deleting a value after setting it
    -- performs correctly.
    go xs = case (splitWith f . HM.toList . HM.fromList) xs of
      (as , bs) -> mdel' as >> mset' bs >> pure ()
      where
        f (x , Nothing) = Left x
        f (x , Just y) = Right (x , y)

    mdel' [] = pure 0
    mdel' xs = unwrap $ liftRedis $ Redis.del xs
    
    mset' [] = pure undefined
    mset' xs = unwrap $ liftRedis $ Redis.mset xs

-- | Set a value together with a given expiration timeout (in seconds).
setex :: MonadRedis m => RedisStructure (BASIC w) a b -> a -> Integer -> b -> m ()
setex (RKeyValue (E k e _)) a t b = liftRedis $ case e b of
  Just bs -> ignore $ Redis.setex (k a) t bs
  Nothing -> ignore $ del [ k a ]
setex (RKeyValueInteger k e _) a t i = liftRedis $ ignore $ Redis.setex (k a) t (pack $ show (e i))

-- | Increment an Integer in Redis. Empty values are treated as 0.
incr :: MonadRedis m => RedisIntegral a b -> a -> m b
incr (RKeyValueInteger p _ d) =
    fmap d
  . unwrap
  . Redis.incr
  . p

-- | Increment an Integer in Redis by a specific amount. Empty values are treated as 0.
incrby :: MonadRedis m => RedisIntegral a b -> a -> b -> m b
incrby (RKeyValueInteger p e d) k =
    fmap d
  . unwrap
  . Redis.incrby (p k)
  . e

-- | Decrement an Integer in Redis. Empty values are treated as 0.
decr :: MonadRedis m => RedisIntegral a b -> a -> m b
decr (RKeyValueInteger p _ d) =
    fmap d
  . unwrap
  . Redis.decr
  . p

-- | Decrement an Integer in Redis by a specific amount. Empty values are treated as 0.
decrby :: MonadRedis m => RedisIntegral a b -> a -> b -> m b
decrby (RKeyValueInteger p e d) k =
    fmap d
  . unwrap
  . Redis.decrby (p k)
  . e

