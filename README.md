# HLRDB Core

HLRDB Core defines a high-level, type-driven API for interacting with Redis. The API in this package is abstract and requires a fair amount of arbitrary decisions to implement, including how to construct path names and identifiers and how to serialize and deserialize data values. If you need to make these decisions yourself, use this package; if you just want to use the library with all these concerns taken care of, use the HLRDB main package.
