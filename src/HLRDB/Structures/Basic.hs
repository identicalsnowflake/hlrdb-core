{-# LANGUAGE BlockArguments #-}

-- | Basic storage is simply a key-value lookup in Redis.

module HLRDB.Structures.Basic where

import Control.Lens (unsafePartsOf)
import Database.Redis as Redis
import HLRDB.Primitives.Aggregate
import HLRDB.Primitives.Redis
import HLRDB.Internal
import Data.ByteString (ByteString)
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
get (RKeyValueByteString k) a = liftRedis $ Redis.get (k a) >>= \case
  Left e -> fail (show e)
  Right r -> pure (maybe mempty id r)

-- | @Q@ is an @Applicative@ that can be used to aggregate many independent queries, all to be reified in a single bulk @mget@ request. Works on @RedisBasic a b@ and @RedisIntegral a b@.
liftq :: RedisStructure (BASIC w) a b -> a -> Q b
liftq (RKeyValue (E k _ d)) i = Q \f -> fmap d . f . const (k i)
liftq (RKeyValueInteger k _ d) i = Q \f -> fmap (d . fromIntegral . decodeMInteger) . f . const (k i)
liftq (RKeyValueByteString k) i = Q \f -> fmap (maybe mempty id) . f . const (k i)

-- | Reify from the @Q@ applicative into the @Redis@ monad using a single @mget@ command.
mget :: MonadRedis m => Q a -> m a
mget = \(Q f) -> unsafePartsOf f (liftRedis . mget') ()
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
set (RKeyValueByteString k) a b = liftRedis $ if b == mempty
  then ignore $ Redis.del [ k a ]
  else ignore $ Redis.set (k a) b

-- | Convenient alias for setting a value for an optional path
set' :: MonadRedis m => RedisBasic a (Maybe b) -> a -> b -> m ()
set' (RKeyValue (E k e _)) a b = liftRedis $ case e (Just b) of
  Just bs -> ignore $ Redis.set (k a) bs
  Nothing -> ignore $ del [ k a ]

-- | Construct a query to be used with @mset@. The @MSET@ type is a @Monoid@, so you may combine many of these together before executing the batch with the @mset@ command.
liftqs :: RedisStructure (BASIC w) a b -> (a , b) -> MSET
liftqs (RKeyValue (E k e _)) (a , b) = MSET $ (<>) [ (k a , e b) ]
liftqs (RKeyValueInteger k e _) (a , b) = MSET $ (<>) [ (k a , Just $ pack (show (e b))) ]
liftqs (RKeyValueByteString k) (a , b) = MSET $ (<>) [ (k a , Just b) ]

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
    
    mset' [] = pure Ok
    mset' xs = unwrap $ liftRedis $ Redis.mset xs

-- | Set a value together with a given expiration timeout (in seconds).
setex :: MonadRedis m => RedisStructure (BASIC w) a b -> a -> Integer -> b -> m ()
setex (RKeyValue (E k e _)) a t b = liftRedis $ case e b of
  Just bs -> ignore $ Redis.setex (k a) t bs
  Nothing -> ignore $ del [ k a ]
setex (RKeyValueInteger k e _) a t i = liftRedis $ ignore $ Redis.setex (k a) t (pack $ show (e i))
setex (RKeyValueByteString k) a t b = liftRedis $ if b == mempty
  then ignore $ Redis.del [ k a ]
  else ignore $ Redis.setex (k a) t b

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

-- | Start and end indices are inclusive. Unlike @get@, the empty bytestring is returned if the key does not exist in Redis or if the specified range is out of range.
getrange :: MonadRedis m => RedisByteString a ByteString -> a -> Integer -> Integer -> m ByteString
getrange (RKeyValueByteString p) k start =
    unwrap
  . Redis.getrange (p k) start

-- | The @Integer@ paramter is the offset. Returns the length of the string after the command has been executed.
setrange :: MonadRedis m => RedisByteString a ByteString -> a -> Integer -> ByteString -> m Integer
setrange (RKeyValueByteString p) k start =
    unwrap
  . Redis.setrange (p k) start

-- | Get the bit stored at the specified offset. Note that if no value exists in Redis or if the specified range is outside the defined range, @False@ will be returned by default.
getbit :: MonadRedis m => RedisByteString a ByteString -> a -> Integer -> m Bool
getbit (RKeyValueByteString p) k =
    fmap (1==)
  . unwrap
  . Redis.getbit (p k)

-- | Set the bit at the specified offset. If the offset is outside the existing defined range of the value, 0s are implicitly inserted to fill the intermediate space. Returns the existing value of this bit, as defined by the @getbit@ semantics above.
setbit :: MonadRedis m => RedisByteString a ByteString -> a -> Integer -> Bool -> m Bool
setbit (RKeyValueByteString p) k i =
    fmap (1==)
  . unwrap
  . Redis.setbit (p k) i
  . \case
       True -> "1"
       False -> "0"

