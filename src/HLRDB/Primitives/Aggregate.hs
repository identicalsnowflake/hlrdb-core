{-# LANGUAGE BlockArguments #-}

-- | Combinators that can be used for aggregating independent queries. See my <https://identicalsnowflake.github.io/QueryAggregation.html article> about aggregating mget queries for more information.

module HLRDB.Primitives.Aggregate
       (
         T(..)
       , type (⟿)
       , type (~~>)
       , type Query
       , aggregatePair
       , remember
       , bitraverse'
       , runT

       -- | Aggregate, atomic multi-set query (as in setting multiple things in a single query)
       , MSET
       ) where

import Data.Bitraversable
import Data.Profunctor
import Data.Profunctor.Traversing
import Control.Lens hiding (Traversing)
import Data.ByteString
import HLRDB.Internal (MSET)


-- | Abstract representation for aggregation.
newtype T x y a b = T (Traversal a b x y) deriving (Functor)

instance Profunctor (T x y) where
  {-# INLINE lmap #-}
  lmap f (T t) = T \x -> t x . f
  {-# INLINE rmap #-}
  rmap g (T t) = T \x -> fmap g . t x
  {-# INLINE dimap #-}
  dimap f g (T t) = T \m -> fmap g . t m . f

instance Traversing (T x y) where
  {-# INLINE traverse' #-}
  traverse' (T t) = T (traverse . t)

instance Applicative (T x y a) where
  {-# INLINE pure #-}
  pure x = T $ \_ _ -> pure x
  {-# INLINE (<*>) #-}
  (<*>) (T f) (T x) = T \g a -> f g a <*> x g a

-- | We can merge any two arbitrary mget queries.
{-# INLINE aggregatePair #-}
aggregatePair :: (Traversing p , Functor (p (a , a')) , Applicative (p (a , a'))) => p a b -> p a' b' -> p (a , a') (b , b')
aggregatePair x y =
  (,) <$> lmap (view _1) x <*> lmap (view _2) y

-- Remember could probably be a Profunctor typeclass in general (is it?)
-- | And we can remember the lookup
{-# INLINE remember #-}
remember :: T x y a b -> T x y a (a , b)
remember (T f) = T \x a -> (,) a <$> f x a

{-# INLINABLE bitraverse' #-}
bitraverse' :: Bitraversable t => a ~~> b -> c ~~> d -> t a c ~~> t b d
bitraverse' x y = rev' (bitraverse (flip lmap x . const) (flip lmap y . const))
      where
        rev' :: (a -> () ~~> b) -> a ~~> b
        rev' f = T \g v -> case f v of
          T m -> m g ()

instance Strong (T x y) where
  {-# INLINE first' #-}
  first' = firstTraversing

instance Choice (T x y) where
  {-# INLINE left' #-}
  left' = leftTraversing

-- | Reify aggregation into a target functor.
{-# INLINE runT #-}
runT :: Functor f => ([x] -> f [y]) -> T x y a b -> a -> f b
runT i (T t) = unsafePartsOf t i


-- | A query using input of type 'a' and yielding an output of type 'b'
type (⟿) a b = T ByteString (Maybe ByteString) a b

-- | An ASCII version of ⟿
type (~~>) a b = T ByteString (Maybe ByteString) a b

-- | Non-infix alias of ⟿
type Query a b = a ⟿ b

