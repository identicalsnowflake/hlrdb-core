{- | If you've provided a maxLength in your structure, adding content (via prepend/append) will trim your list
to preserve this, prioritizing newest content - i.e., prepend commands will result in content
being trimmed from the end on overflow, and append commands will result in content trimmed from
the front on overflow.

-}

module HLRDB.Structures.List
       (
         HLRDB.Structures.List.lrange
       , lappend
       , lprepend
       , HLRDB.Structures.List.lrem
       , HLRDB.Structures.List.llen
       ) where

import Data.Functor.Identity
import Database.Redis as Redis
import HLRDB.Components.RedisPrimitives
import HLRDB.Util

-- | Retrieve a range of elements. Endpoints are inclusive, just as with Haskell's [ 1 .. 5 ] notation.
lrange :: RedisList a b -> a -> Integer -> Integer -> Redis [ b ]
lrange p@(RList (E _ _ d) _) k i j =
  fmap (d . pure) <$> unwrap (Redis.lrange (primKey p k) i j)

-- | Append items to the end of a list
lappend :: RedisList a b -> a -> [ b ] -> Redis ()
lappend = addItem True

-- | Prepend items to the front of a list
lprepend :: RedisList a b -> a -> [ b ] -> Redis ()
lprepend = addItem False

addItem :: Bool -> RedisList a b -> a -> [ b ] -> Redis ()
addItem toTheEnd p@(RList (E _ e _) trimScheme) k bs = case bs of
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

-- | Remove an item from the list. This will respect the encoded equality, which may not (but usually does) coincide with the Haskell Eq instance.
lrem :: RedisList a b -> a -> b -> Redis ()
lrem p@(RList (E _ e _) _) k =
  ignore . unwrap . Redis.lrem (primKey p k) 0 . runIdentity . e

-- | Retrieve the length of a list
llen :: RedisList a b -> a  -> Redis Integer
llen p = unwrap . Redis.llen . primKey p

