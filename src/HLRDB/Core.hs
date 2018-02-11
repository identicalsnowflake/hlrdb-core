-- | This package is an abstract API for modeling high-level Redis functionality. It makes no opinion on either serialization or key construction, which means there is a fair amount of work to do to make this library usable. If you do not want to do this work and don't mind these decisions being made for you, you may use the HLRDB library, which gives you a ready-to-go API.

module HLRDB.Core
       (
         module HLRDB.Components.Aggregate
       , module HLRDB.Components.Indexes
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
import HLRDB.Components.Indexes
import HLRDB.Components.RedisPrimitives

import HLRDB.Structures.Basic
import HLRDB.Structures.List
import HLRDB.Structures.HSet
import HLRDB.Structures.Set
import HLRDB.Structures.SSet

