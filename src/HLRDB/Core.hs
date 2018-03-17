-- | This package is an abstract API for modeling high-level Redis functionality. It makes no opinion on either serialization or key construction, which means there is a fair amount of work to do to make this library usable. If you do not want to do this work and don't mind these decisions being made for you, you may use the HLRDB library, which gives you a ready-to-go API.
-- This package depends on the Hedis library for low-level Redis bindings, but it is not recommended to import them together in the same module, as there are many name conflicts, since much of what HLRDB does is simply assign types to commands. Despite this, much of the HLRDB API does differ entirely, with many commands added, removed, merged, or simply rethought from a Haskell perspective.


module HLRDB.Core
       (
         module HLRDB.Components.Aggregate
       , module HLRDB.Components.RedisPrimitives
       
       , module HLRDB.Structures.Basic
       , module HLRDB.Structures.List
       , module HLRDB.Structures.HSet
       , module HLRDB.Structures.Set
       , module HLRDB.Structures.SSet

       , Redis
       ) where

import Database.Redis

import HLRDB.Components.Aggregate
import HLRDB.Components.RedisPrimitives

import HLRDB.Structures.Basic
import HLRDB.Structures.List
import HLRDB.Structures.HSet
import HLRDB.Structures.Set
import HLRDB.Structures.SSet

