-- | A model for categorizing Redis commands for their particular structures using a GADT.

module HLRDB.Components.RedisPrimitives where

import Data.Functor.Identity
import Data.ByteString


-- | General primitive encoding. We need a way to serialize the key, and a way to both serialize and deserialize values.
data E f a b = E (a -> ByteString) (b -> f ByteString) (f ByteString -> b)

-- | Most structures don't rely on any special context
type RE a b = E Identity a b

-- | Type-level indicator for Redis basic types
data BASIC a
-- | Type-level indicator for Redis Lists
data LIST
-- | Type-level indicator for Redis HSets with sub-keys of type k; requires a way to serialize and deserialize sub-keys
data HSET k = HSET (k -> ByteString) (ByteString -> k)
-- | Type-level indicator for Redis Sets
data SET
-- | Type-level indicator for Redis SortedSets
data SORTEDSET

-- | MaxLength is optionally used to specify automatic trimming on Lists
type MaxLength = Integer
-- | For lists and sorted sets, you may optionally provide a TrimScheme, which entails a desired cardinality as well as a probability that content will be trimmed to this cardinality when new data is inserted.
type TrimScheme = (MaxLength, Probability)
-- | The probability that a trim will occur when adding an item
type Probability = Double

-- | GADT declaring the major Redis data structures
data RedisStructure t a b where
  RKeyValue :: E Maybe a b -> RedisStructure (BASIC ()) a b
  -- Do not allow specifying an encoding for Integer, since Redis commands like incr
  -- demand that *only* the standard encoding is used, e.g., 123 -> "123"
  RKeyValueInteger :: (a -> ByteString) -> (b -> Integer) -> (Integer -> b) -> RedisStructure (BASIC Integer) a b
  RList :: RE a b -> Maybe TrimScheme -> RedisStructure LIST a b
  RHSet :: RE a b -> HSET v -> RedisStructure (HSET v) a b
  RSet :: RE a b -> RedisStructure SET a b
  RSortedSet :: RE a b -> Maybe TrimScheme -> RedisStructure SORTEDSET a b

-- | Alias for simple key-value storage
type RedisBasic k v = RedisStructure (BASIC ()) k v
-- | Alias for simple Integer storage
type RedisIntegral k b = RedisStructure (BASIC Integer) k b
-- | Alias for a Redis List
type RedisList k v = RedisStructure LIST k v
-- | Alias for a Redis HSet
type RedisHSet k s v = RedisStructure (HSET s) k v
-- | Alias for a Redis Set
type RedisSet k v = RedisStructure SET k v
-- | Alias for a Redis SortedSet
type RedisSSet k v = RedisStructure SORTEDSET k v

-- | Many commands in Redis return information about whether items were added/removed or merely updated
data Creation
data Deletion

data ActionPerformed a where
  FreshlyCreated :: Integer -> ActionPerformed Creation
  Deleted :: Integer -> ActionPerformed Deletion

