{-# LANGUAGE BlockArguments #-}

module HLRDB.Primitives.Aggregate
       ( Q(..)
       -- | Aggregate, atomic multi-set query (as in setting multiple things in a single query)
       , MSET
       ) where

import Control.Lens
import Data.ByteString
import HLRDB.Internal (MSET)


-- | An applicative representing a single bulk query, reified into the Redis monad via @mget@.
newtype Q a = Q (Traversal () a ByteString (Maybe ByteString))

instance Functor Q where
  {-# INLINE fmap #-}
  fmap f = \(Q g) -> Q \x -> fmap f . g x

instance Applicative Q where
  {-# INLINE pure #-}
  pure x = Q \_ _ -> pure x
  {-# INLINE (<*>) #-}
  (<*>) (Q f) (Q x) = Q \g a -> f g a <*> x g a

