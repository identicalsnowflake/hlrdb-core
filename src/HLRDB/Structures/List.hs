{- | If you've provided a @TrimScheme@ in your structure, adding content (via prepend/append) will trim your list
to preserve this, prioritizing newest content - i.e., prepend commands will result in content
being trimmed from the end on overflow, and append commands will result in content trimmed from
the front on overflow.

-}

module HLRDB.Structures.List
       (
         HLRDB.Structures.List.lrange
       , lprepend
       , lappend
       , HLRDB.Structures.List.lpop
       , HLRDB.Structures.List.lrem
       , HLRDB.Structures.List.llen
       -- * Other commands
       -- | The following commands are available in Redis, but are recommended to use only with caution, due to their behavior being either "unhaskell-ey" or downright exotic.
       , HLRDB.Structures.List.rpop
       , HLRDB.Structures.List.rpoplpush
       , HLRDB.Structures.List.blpop
       , HLRDB.Structures.List.brpop
       , HLRDB.Structures.List.brpoplpush
       ) where

import Data.Functor.Identity
import Database.Redis as Redis
import HLRDB.Components.RedisPrimitives
import HLRDB.Internal
import Data.Maybe (fromJust)


-- | Retrieve a range of elements. Endpoints are inclusive, just as with Haskell's [ 1 .. 5 ] notation.
lrange :: RedisList a b -> a -> Integer -> Integer -> Redis [ b ]
lrange p@(RList (E _ _ d) _) k i j =
  fmap (d . pure) <$> unwrap (Redis.lrange (primKey p k) i j)

-- | Append items to the end of a list
lappend :: Traversable t => RedisList a b -> a -> t b -> Redis ()
lappend = addItem True

-- | Prepend items to the front of a list
lprepend :: Traversable t => RedisList a b -> a -> t b -> Redis ()
lprepend = addItem False

addItem :: Traversable t => Bool -> RedisList a b -> a -> t b -> Redis ()
addItem toTheEnd p@(RList (E _ e _) trimScheme) k bs' =
  let bs = foldr (:) [] bs' in
  case bs of
  [] -> pure ()
  _ -> do
       let method = if toTheEnd then rpush else lpush
       let key = primKey p k
       itemCount <- unwrap $ method key (fmap (runIdentity . e) bs)
       case trimScheme of
         Just (maxItemCount , prob) -> fmap (const ()) $ probIO prob $
           if itemCount > maxItemCount
              then ignore $ if toTheEnd
                   then unwrap $ ltrim key (fromIntegral $ length bs) (-1)
                   else unwrap $ ltrim key 0 (maxItemCount - 1)
              else pure ()
         Nothing -> pure ()

-- | Remove an item from the list. You should ensure that any Eq instance in Haskell respects the induced equality by your encoding scheme, as Redis will use the latter.
lrem :: RedisList a b -> a -> b -> Redis ()
lrem p@(RList (E _ e _) _) k =
  ignore . unwrap . Redis.lrem (primKey p k) 0 . runIdentity . e

-- | Retrieve the length of a list.
llen :: RedisList a b -> a  -> Redis Integer
llen p = unwrap . Redis.llen . primKey p

-- | Remove and return an item from the head of the list.
lpop :: RedisList a b -> a -> Redis (Maybe b)
lpop p@(RList (E _ _ d) _) =
  (fmap . fmap) (d . pure) . unwrap . Redis.lpop . primKey p

-- | Remove and return an item from the end of the list.
rpop :: RedisList a b -> a -> Redis (Maybe b)
rpop p@(RList (E _ _ d) _) =
  (fmap . fmap) (d . pure) . unwrap . Redis.rpop . primKey p

-- | Remove and return an item from the first list and prepend it to the second list.
rpoplpush :: RedisList a b -> a -> a -> Redis (Maybe b)
rpoplpush p@(RList (E _ _ d) _) source dest =
  (fmap . fmap) (d . pure) $ unwrap $ Redis.rpoplpush (primKey p source) (primKey p dest)

-- | Blocking variant of rpoplpush
brpoplpush :: RedisList a b -> a -> a -> Integer -> Redis (Maybe b)
brpoplpush p@(RList (E _ _ d) _) source dest =
  (fmap . fmap) (d . pure) . unwrap . Redis.brpoplpush (primKey p source) (primKey p dest)

-- | Pop the first available value from a set of lists; if none is available, block the connection (!) until either the specified timeout completes, returning nothing, or until a value becomes available, returning the value and the key of the list in which it was added, whichever happens first. If multiple clients are waiting for an item from the same list, the one who has been waiting the longest will be given the item. If no keys are given, the command returns immediately with Nothing.
blpop :: Traversable t => RedisList a b -> t a -> Integer -> Redis (Maybe (a , b))
blpop p@(RList (E e _ d) _) ts t = case foldr (\x a -> (e x , x) : a) [] ts of
  [] -> pure Nothing
  xs ->
    let f (x , b) = (fromJust (lookup x xs) , (d . pure) b) in
    (fmap . fmap . fmap) f unwrap $ Redis.blpop (primKey p . snd <$> xs) t

-- | Similar to blpop, but popping from the right.
brpop :: Traversable t => RedisList a b -> t a -> Integer -> Redis (Maybe (a , b))
brpop p@(RList (E e _ d) _) ts t = case foldr (\x a -> (e x , x) : a) [] ts of
  [] -> pure Nothing
  xs ->
    let f (x , b) = (fromJust (lookup x xs) , (d . pure) b) in
    (fmap . fmap . fmap) f unwrap $ Redis.brpop (primKey p . snd <$> xs) t

