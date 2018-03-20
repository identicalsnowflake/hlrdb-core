# HLRDB Core

HLRDB Core defines a high-level, type-driven API for interacting with Redis. The API in this package is abstract and requires a fair amount of arbitrary decisions to implement, including how to construct path names and identifiers and how to serialize and deserialize data values. If you need to make these decisions yourself, use this package; if you just want to use the library with all these concerns taken care of, use the HLRDB main package.

This package depends on the Hedis library for low-level Redis bindings, but it is not recommended to import them together in the same module, as there are many name conflicts, since much of what HLRDB does is simply assign types to commands. Despite this, much of the HLRDB API does differ entirely, with many commands added, removed, merged, or simply rethought from a Haskell perspective.

When using this package, you should always ensure that your `Eq` instances respect the induced equality via whatever serialization mechanism you've specified, since many commands perform comparisons in Redis directly.
