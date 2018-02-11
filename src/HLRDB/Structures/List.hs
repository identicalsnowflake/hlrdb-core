{- | If you've provided a maxLength in your structure, adding content (via prepend/append) will trim your list
to preserve this, prioritizing newest content - i.e., prepend commands will result in content
being trimmed from the end on overflow, and append commands will result in content trimmed from
the front on overflow.

-}

module HLRDB.Structures.List
       (
         listGet
       , listAppend
       , listPrepend
       , listAppend'
       , listPrepend'
       , listRemove
       , listLength
       ) where

import Data.Functor.Identity
import Database.Redis as Redis
import HLRDB.Components.RedisPrimitives
import HLRDB.Components.Indexes
import HLRDB.Util


-- | Retrieve a range of elements, common Indexes include @fullIndex@, @firstIndex@, and @fromTo 0 10@
listGet :: RedisList a b -> a -> Indexes -> Redis [ b ]
listGet p@(RList (E _ _ d) _) k (Indexes (i,j)) =
  fmap (d . pure) <$> unwrap (lrange (primKey p k) i j)

-- | Append an item to the end of a list. Unlike Haskell lists, this is O(1)
listAppend :: RedisList a b -> a -> b -> Redis ()
listAppend p k b = addItem True p k [ b ]

-- | Prepend an item
listPrepend :: RedisList a b -> a -> b -> Redis ()
listPrepend p k b = addItem False p k [ b ]

-- | Append a list of items
listAppend' :: RedisList a b -> a -> [ b ] -> Redis ()
listAppend' = addItem True

-- | Prepend a list of items
listPrepend' :: RedisList a b -> a -> [ b ] -> Redis ()
listPrepend' = addItem False

addItem :: Bool -> RedisList a b -> a -> [ b ] -> Redis ()
addItem toTheEnd p@(RList (E _ e _) maxLength) k bs = case bs of
  [] -> pure ()
  _ -> do
       let method = if toTheEnd then rpush else lpush
       let key = primKey p k
       itemCount <- unwrap $ method key (fmap (runIdentity . e) bs)
       case maxLength of
         Just maxItemCount ->
           if itemCount > maxItemCount
              then ignore $ if toTheEnd
                   then unwrap $ ltrim key (fromIntegral $ length bs) (-1)
                   else unwrap $ ltrim key 0 (maxItemCount - 1)
              else pure ()
         Nothing -> pure ()

-- | Remove an item from the list. This will respect the encoded equality, which may not (but usually does) coincide with the Haskell Eq instance.
listRemove :: RedisList a b -> a -> b -> Redis ()
listRemove p@(RList (E _ e _) _) k =
  ignore . unwrap . lrem (primKey p k) 0 . runIdentity . e

-- | Retrieve the length of a list
listLength :: RedisList a b -> a  -> Redis Integer
listLength p = unwrap . llen . primKey p

