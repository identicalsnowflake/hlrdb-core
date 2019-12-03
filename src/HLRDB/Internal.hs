-- | Internal module. Not intended for public use.

module HLRDB.Internal
       ( probIO
       , primKey
       , unwrap
       , unwrapCursor
       , unwrapCreatedBool
       , unwrapCreated
       , unwrapDeleted
       , ignore
       , fixEmpty
       , fixEmpty'
       , foldM
       , decodeMInteger
       , readInt
       , Int64
       , runIdentity
       , MSET(..)
       , HLRDB.Internal.splitWith
       ) where

import Data.Functor.Identity
import Database.Redis
import Data.ByteString hiding (foldr)
import HLRDB.Primitives.Redis
import qualified Data.ByteString as S
import qualified Data.ByteString.Unsafe as B
import GHC.Int
import Control.Monad.IO.Class
import System.Random (randomRIO)


probIO :: MonadIO m => Double -> m a -> m (Maybe a)
probIO pr a =
  if pr >= 1.0 then Just <$> a else do
    r :: Double <- liftIO $ randomRIO (0, 1.0)
    if r <= pr
       then Just <$> a
       else return Nothing


{-# INLINE primKey #-}
primKey :: RedisStructure v a b -> a -> ByteString
primKey (RKeyValue (E e _ _)) k = e k
primKey (RKeyValueInteger e _ _) k = e k
primKey (RList (E e _ _) _) k = e k
primKey (RHSet (E e _ _) _) k = e k
primKey (RSet (E e _ _)) k = e k
primKey (RSortedSet (E e _ _) _) k = e k

-- Redis should never respond with errors if we are using our types consistently,
-- so transform them into exceptions
failRedis :: Reply -> Redis a
failRedis = fail . (++) "Unexpected Redis response: " . show

{-# INLINE unwrap #-}
unwrap :: MonadRedis m => Redis (Either Reply a) -> m a
unwrap r = do
  res <- liftRedis r
  case res of
    Left e -> liftRedis $ failRedis e
    Right i -> return i

{-# INLINE unwrapCursor #-}
unwrapCursor :: MonadRedis m => (a -> b) -> Redis (Either Reply (Cursor , a)) -> m (Maybe Cursor , b)
unwrapCursor f =
  let g (c , x) = (if c == cursor0 then Nothing else Just c , f x) in
  fmap g . unwrap


{-# INLINE unwrapCreatedBool #-}
unwrapCreatedBool :: MonadRedis m => Redis (Either Reply Bool) -> m (ActionPerformed Creation)
unwrapCreatedBool = fmap (\b -> if b then FreshlyCreated 1 else FreshlyCreated 0) . unwrap

{-# INLINE unwrapCreated #-}
unwrapCreated :: MonadRedis m => Redis (Either Reply Integer) -> m (ActionPerformed Creation)
unwrapCreated = fmap FreshlyCreated . unwrap

{-# INLINE unwrapDeleted #-}
unwrapDeleted :: MonadRedis m => Redis (Either Reply Integer) -> m (ActionPerformed Deletion)
unwrapDeleted = fmap Deleted . unwrap

{-# INLINE ignore #-}
ignore :: Functor f => f a -> f ()
ignore = fmap (const ())

-- Redis does not treat treat zero cases properly, so use this to fix the algebra
{-# INLINE fixEmpty #-}
fixEmpty :: (MonadRedis m , Monoid e, Traversable t) => ([ b ] -> Redis e) -> (a -> b) -> t a -> m e
fixEmpty f e t = case foldr ((:) . e) [] t of
  [] -> pure mempty
  xs -> liftRedis $ f xs

{-# INLINE fixEmpty' #-}
fixEmpty' :: (MonadRedis m, Traversable t, Integral i) => ([ b ] -> Redis i) -> (a -> b) -> t a -> m i
fixEmpty' f e t = case foldr ((:) . e) [] t of
  [] -> pure 0
  xs -> liftRedis $ f xs

{-# INLINE foldM #-}
foldM :: Foldable t => (a -> b) -> t a -> [ b ]
foldM f t = foldr (\a xs -> f a : xs) [] t

{-# INLINE decodeMInteger #-}
decodeMInteger :: Maybe ByteString -> Int64
decodeMInteger Nothing = 0
decodeMInteger (Just bs) = readInt bs

-- From chrisdone's https://github.com/chrisdone/advent-2017-maze-rust-haskell
readInt :: ByteString -> Int64
readInt as
  | S.null as = 0
  | otherwise =
    case B.unsafeHead as of
      45 -> loop True 0 0 (B.unsafeTail as)
      43 -> loop False 0 0 (B.unsafeTail as)
      _ -> loop False 0 0 as
  where
    loop :: Bool -> Int64 -> Int64 -> S.ByteString -> Int64
    loop neg !i !n !ps
      | S.null ps = end neg i n
      | otherwise =
        case B.unsafeHead ps of
          w
            | w >= 0x30 && w <= 0x39 ->
              loop
                neg
                (i + 1)
                (n * 10 + (fromIntegral w - 0x30))
                (B.unsafeTail ps)
            | otherwise -> end neg i n
    end _ 0 _ = 0
    end True _ n = negate n
    end _ _ n = n

type DList a = ([ a ] -> [ a ])

-- | Aggregated @mset@ query
newtype MSET = MSET { runMSET :: DList (ByteString , Maybe ByteString) }

instance Semigroup MSET where
  {-# INLINE (<>) #-}
  (<>) (MSET as) (MSET bs) = MSET (as . bs)

instance Monoid MSET where
  {-# INLINE mempty #-}
  mempty = MSET $ (<>) []

splitWith :: (a -> Either b c) -> [ a ] -> ([ b ] , [ c ])
splitWith f = foldr g mempty
  where
    g x (as , bs) = case f x of
      Left a -> (a : as , bs)
      Right b -> (as , b : bs)

