-- | Combinators that can be used for aggregating independent queries. See my <https://identicalsnowflake.github.io/QueryAggregation.html article> about aggregating mget queries for more information.

module HLRDB.Components.Aggregate
       (
         T(..)
       , type (⟿)
       , type Query
       , aggregatePair
       , mget
       ) where

import Data.Profunctor
import Data.Profunctor.Traversing
import Control.Lens hiding (Traversing)
import Data.ByteString
import Database.Redis hiding (mget)
import qualified Database.Redis

-- | Abstract representation for aggregation.
newtype T x y a b = T (Traversal a b x y) deriving (Functor)

instance Profunctor (T x y) where
  dimap f g (T t) = T $ \m -> fmap g . t m . f

instance Traversing (T x y) where
  traverse' (T t) = T (traverse . t)

instance Applicative (T x y a) where
  pure x = T $ \_ _ -> pure x
  (<*>) (T f) (T x) = T $ \g a -> f g a <*> x g a

-- | We can merge any two arbitrary mget queries.
{-# INLINE aggregatePair #-}
aggregatePair :: T x y a b -> T x y c d -> T x y (a,c) (b,d)
aggregatePair (T f) (T g) = T $ \h (a,c) ->
  (,) <$> f h a <*> g h c

instance Strong (T x y) where
  first' = firstTraversing

instance Choice (T x y) where
  left' = leftTraversing

{-# INLINE runT #-}
runT :: (Functor f) => ([x] -> f [y]) -> T x y a b -> a -> f b
runT i (T t) = unsafePartsOf t i


-- | A query using input of type 'a' and yielding an output of type 'b'
type (⟿) a b = T ByteString (Maybe ByteString) a b

-- | Non-infix alias of ⟿
type Query a b = a ⟿ b

-- | Reify a (⟿) query into the Redis monad via a single mget command
{-# INLINE mget #-}
mget :: a ⟿ b -> a -> Redis b
mget = runT mget'
  where
    mget' :: [ByteString] -> Redis [Maybe ByteString]
    mget' [] = pure []
    mget' xs = Database.Redis.mget xs >>= \case
      Left e -> fail (show e)
      Right vs -> pure vs

