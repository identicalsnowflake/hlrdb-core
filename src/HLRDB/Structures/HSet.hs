-- | An HSet is a sub-hash table in Redis, indexed by both a key and a subkey.

module HLRDB.Structures.HSet where

import Data.Functor.Identity
import Database.Redis as Redis
import HLRDB.Components.RedisPrimitives
import HLRDB.Util
import Control.Monad.State

-- I wanted to only have the list get/set commands, but ultimately
-- decided to include the single commands separately because `hset`
-- has a return value which specifies whether a subkey was created or not,
-- whereas hmset does not, and this can be critically useful. Amusingly,
-- even the standard `set` command does not return this information.


-- | Retrieve all elements of an HSet
hgetall :: RedisHSet a s b -> a -> Redis [ (s,b) ]
hgetall p@(RHSet (E _ _ d) (HSET _ ds)) =
 (fmap . fmap) (\(s,b) -> (ds s , d (pure b))) . unwrap . Redis.hgetall . primKey p

-- | Lookup via key and subkey
hget :: RedisHSet a s b -> a -> s -> Redis (Maybe b)
hget p@(RHSet (E _ _ d) (HSET e _)) k =
  (fmap . fmap) (d . pure) . unwrap . Redis.hget (primKey p k) . e

-- | Lookup via key and subkeys, pairing each given subkey with the lookup result
hmget :: Traversable t => RedisHSet a s b -> a -> t s -> Redis (t (s , Maybe b))
hmget p@(RHSet (E _ _ d) (HSET e _)) k t = do
  let f = (fmap . fmap . fmap) (d . pure) . unwrap . Redis.hmget (primKey p k) . fmap e
  let xs = foldr (:) [] t
  ys <- f xs
  pure $ reifyTraversal t ys
  where
    reifyTraversal :: Traversable t => t a -> [b] -> t (a,b)
    reifyTraversal tr bs = evalState (traverse g tr) bs
      where
        g a = do
          (b:bs') <- Control.Monad.State.get
          put bs'
          return (a,b)

-- | Set via key and subkey
hset :: RedisHSet a s b -> a -> s -> b -> Redis (ActionPerformed Creation)
hset p@(RHSet (E _ eb _) (HSET e _)) k s =
  unwrapCreatedBool . Redis.hset (primKey p k) (e s) . runIdentity . eb

-- | Set via key and subkeys
hmset :: Traversable t => RedisHSet a s b -> a -> t (s , b) -> Redis ()
hmset p@(RHSet (E _ eb _) (HSET e _)) k t =
  ignore $ Redis.hmset (primKey p k) $ foldr (\x a -> (e (fst x) , runIdentity (eb (snd x))) : a) [] t


-- | Delete via key and subkeys
hdel :: (Traversable t) => RedisHSet a s b -> a -> t s -> Redis (ActionPerformed Deletion)
hdel p@(RHSet _ (HSET e _)) k =
  fmap Deleted .fixEmpty' (unwrap . Redis.hdel (primKey p k)) e

-- | Set a value only if it does not currently exist in the HSET
hsetnx :: RedisHSet a s b -> a -> s -> b -> Redis (ActionPerformed Creation)
hsetnx p@(RHSet (E _ eb _) (HSET e _)) k s =
  unwrapCreatedBool . Redis.hsetnx (primKey p k) (e s) . runIdentity . eb

-- | Use a cursor to iterate a collection
hscan :: RedisHSet a s b -> a -> Cursor -> Redis (Maybe Cursor , [ (s , b) ])
hscan p@(RHSet (E _ _ d) (HSET _ d')) k =
  let f (a,b) = (d' a , d (pure b)) in
  unwrapCursor (fmap f) . Redis.hscan (primKey p k)

