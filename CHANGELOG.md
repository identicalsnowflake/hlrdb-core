# New in 0.1.3.0

- `mset` command with batch processing.

# Fixed in 0.1.1.1

- Zero cases for hmget and hmset are now handled correctly (previously, empty data was sent to Redis, resulting in an error)

# New in 0.1.1

- Added `del`, `persist`, `expire`, and `expireat`.
- Added ASCII `~~>` alias for query

