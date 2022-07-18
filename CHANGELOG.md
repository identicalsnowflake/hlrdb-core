# 0.2.0.0
- Query aggregation is now done via the `Q` Applicative, which can be reified into the `Redis` monad via `mget`. Using `ApplicativeDo`, querying via @Q@ is as ergonomic as executing individual `get` commands.

# Fixed in 0.1.6.2
- Removed `undefined` in empty `mset` case

# New in 0.1.6.0

- `getrange`, `setrange`, `getbit`, `setbit` added for new ByteString-based primitive

# New in 0.1.4.0

- `srandmembersN` command added

# New in 0.1.3.0

- `mset` command with batch processing.

# Fixed in 0.1.1.1

- Zero cases for hmget and hmset are now handled correctly (previously, empty data was sent to Redis, resulting in an error)

# New in 0.1.1

- Added `del`, `persist`, `expire`, and `expireat`.
- Added ASCII `~~>` alias for query

