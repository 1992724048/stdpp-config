<p align="right">
  <a href="README.md">English</a> | <a href="README.zh-CN.md">中文</a>
</p>

# stdpp::config

A **type-safe, event-driven, thread-safe** C++ configuration system with TOML persistence.  
It treats configuration as **strongly typed variables**, supports rich STL and chrono types, provides change events, and allows full customization via `Codec<T>`.

## Table of Contents

- [Features](#features)
- [Dependencies](#dependencies)
- [Field Identity & Default Value Rules](#field-identity--default-value-rules)
- [Operator Overload Support](#operator-overload-support)
- [Value Access Semantics](#value-access-semantics)
- [Supported Containers and Types](#supported-containers-and-types)
- [Example](#example)
- [Configuration Events](#configuration-events)
- [Custom Type Serialization](#custom-type-serialization)
- [Thread Safety](#thread-safety)
- [Design Notes](#design-notes)

---

## Features

- **Strongly typed fields**
  - `Field<T>` / `FieldValue<T>` behave like normal variables
  - Compile-time constraint: `Serializable`
- **Automatic TOML mapping**
  - Hierarchical paths via `::` (e.g. `net::http::port`)
  - Automatically builds nested TOML tables
- **Event-driven**
  - `VALUE_LOAD` when loaded from file
  - `VALUE_CHANGE` when modified at runtime
- **Thread-safe**
  - Lock hierarchy: global config lock → field registry lock → per-field value lock
  - Concurrent `load()` / `save()` and reads/writes are race-free
- **Extensible serialization**
  - Built-in support for many STL, chrono, and utility types
  - User-defined types via `Codec<T>`
- **Batch save**
  - Changes are marked dirty
  - `Config::save()` persists all modified fields at once

## Dependencies

- C++23
- [ToruNiina/toml11](https://github.com/ToruNiina/toml11) ≥ 4.4.0
- [Neargye/magic_enum](https://github.com/Neargye/magic_enum) ≥ 0.9.7
- [stdpp-event](https://github.com/1992724048/stdpp-event)

## Field Identity & Default Value Rules

```cpp
#include "config.hpp"
using namespace stdpp::config;
Field<int> a("x");
Field<int> b("x", 255);
Field<int> c("x::y", 5);
Field<std::string> d("x", "default");
```

- Fields are uniquely identified by their **full path string + type signature**  
  Internally the key is formed as `path + "#" + typeid(T).name()` (e.g. `x#int`, `x#NSt7__cxx1112basic_string...`)
- Fields with the same **name and type** share the same storage
- Fields with the same **name but different type** coexist independently — no exception thrown
- The **first constructed field** decides the default value
- Later declarations with the same name and type:
  - Same type → reuse existing value, ignore default
- On `Config::load()`: when a TOML value type doesn't match a field's `Codec<T>`, the field is silently skipped (keeps its default value)

| Declaration                 | Internal Key | Shared with same key | Default Used    |
| --------------------------- | ------------ | -------------------- | --------------- |
| `Field<int> a("x")`         | `x#int`      | yes                  | `int{}`         |
| `Field<int> b("x", 255)`    | `x#int`      | yes                  | ignored         |
| `Field<std::string> d("x")` | `x#NSt7...`  | no (different type)  | `std::string{}` |
| `Field<int> c("x::y", 5)`   | `x::y#int`   | no                   | `5`             |

## Operator Overload Support

`FieldValue<T>` behaves like `T` if `T` supports the operator.

### Assignment

- `=`
- assign from `T`
- assign from `FieldValue<T>`

### Arithmetic

- `+  -  *  /`
- `+= -= *= /=`

### Bitwise

- `|  &  ^`
- `|= &= ^=`

### Shift

- `<< >>`
- `<<= >>=`

### Increment / Decrement

- `++x  x++`
- `--x  x--`

### Mixed with raw values

- `i = field + 1`
- `i += field`
- `field += other_field`

> All operators are only enabled if the underlying `T` supports them.

> Compound assignments (`+= -= *= /= <<= >>= |= &= ^=`) and `++/--` automatically trigger `VALUE_CHANGE` and mark the field dirty, so the next `Config::save()` persists the result.

## Value Access Semantics

Reading a field always returns a **copy** of the underlying value, not a reference:

```cpp
Field<int> x("x", 42);
int val = x;          // copy: operator T()
int val2 = *x;        // copy: operator*()
int val3 = x.copy();  // explicit copy
```

For direct reference access, use `value_lock()` with RAII:

```cpp
auto lock = x.value_lock();  // locks value_mutex (exclusive)
*lock = 100;                 // modify in-place via reference
// ~FieldValueMutex() triggers VALUE_CHANGE automatically
```

For hot read paths, use `read_lock()` — a shared-lock, copy-free const view:

```cpp
auto guard = x.read_lock();  // shared lock, no copy, no event
const int& v = *guard;       // const reference to the current value
```

> The copy-by-default design ensures thread safety — reads never block the value for longer than necessary. `read_lock()` avoids the copy entirely while multiple readers share the lock concurrently.

## Supported Containers and Types

### Sequential Containers

- `std::vector<T>`
- `std::list<T>`
- `std::deque<T>`
- `std::forward_list<T>`
- `std::array<T, N>`

### Container Adapters

- `std::queue<T>`
- `std::stack<T>`
- `std::priority_queue<T>`

### Associative Containers

- `std::set<T>`
- `std::multiset<T>`
- `std::map<K, V>`
- `std::multimap<K, V>`
- `std::unordered_map<K, V>`

### Utility Types

- `std::pair<T1, T2>`
- `std::tuple<Ts...>`
- `std::optional<T>`
- `std::variant<Ts...>`
- `std::expected<T, E>`
- `std::complex<T>`
- `std::bitset<N>`
- `std::filesystem::path`
- `std::atomic<T>`

### Time & Date (std::chrono)

- `std::chrono::duration`
- `std::chrono::hh_mm_ss`
- `std::chrono::sys_time`
- `std::chrono::year_month_day`
- `std::chrono::zoned_time`

### Pointer Wrappers

- `std::unique_ptr<T>`
- `std::shared_ptr<T>`

### Enums

- Any `enum` or `enum class`  
  Serialized as **string names** via `magic_enum`

## Example

```cpp
Field<int> port("server::port", 8080);
Field<int> port2("server::port", 8080); // Ignore 8080 Parameter
// port == port2

Field<std::vector<int>> vec("test::vec", {1,2,3});

Config::load("config.toml");

Field<std::optional<int>> opt("test::opt", std::nullopt);
Field<Test> mode("app::mode", Test::A);

opt = std::nullopt; // triggers VALUE_CHANGE

Config::save(); // change only

```

TOML:

```toml
[server]
port = 8080

[test]
vec = [1,2,3]

[test.opt]
has = false
# value = 114

[app]
mode = "A"
```

## Configuration Events

```cpp
auto h = port.add_event([](auto&, Event ev){
    if(ev == Event::VALUE_CHANGE) { /* changed */ }
});
```

Event types:

```cpp
enum class Event {
    VALUE_CHANGE,
    VALUE_LOAD
};
```

## Custom Type Serialization

Define a `Codec<T>` specialization:

```cpp
struct Point { int x; int y; };

template<>
struct Codec<Point> {
    static toml::value encode(const Point& p) {
        return { {"x", p.x}, {"y", p.y} };
    }
    static Point decode(const toml::value& v) {
        return { v.at("x"), v.at("y") };
    }
};
```

Usage:

```cpp
Field<Point> pos("window::pos", {10,20});
```

## Thread Safety

Three-level lock hierarchy:

- `config_mutex` – protects the config path and the parsed TOML document (file I/O, snapshots)
- `field_mutex` – protects the field registry (concurrent field registration is safe)
- `per-field value_mutex` – a `std::shared_mutex` protecting a single field's value

Lock ordering is `config_mutex → field_mutex`; all other locks are acquired independently, so no deadlock cycle exists. Decoding runs outside the global lock on a deep-copied snapshot, and dirty-state tracking uses an atomic counter — concurrent `load()` / `save()` and save-during-field-registration cannot lose updates.

Manual access:

```cpp
auto lock = field.value_lock();  // exclusive write guard (auto VALUE_CHANGE on destruction)
auto guard = field.read_lock();  // shared read guard (no copy, no event)
```

## Breaking Changes

Introduced by the hardening refactor (all compile-time visible):

- `Field<T>()` / `FieldValue<T>()` **default constructors are deleted** — always use named constructors: `Field<int>("x", 42)`
- Aliases `STR / OPT / MAP / PTR / EXP` moved into `detail` — use `std::string`, `std::optional`, ... directly
- Binary `&` on `FieldValue` was removed (`&=` remains); built-in address-of is preserved
- Requires **C++23** (`std::expected`, chrono formatting) — enforced at compile time

## Design Notes

- Global static `Config`
- Fields cannot be removed at runtime
- Same-name different-type fields coexist independently via `#type` suffix
- TOML value type mismatch during load silently skips the field (no exception)
- File is created only on first successful `save()`
- `save()` returns `false` on write failure (e.g. disk full) and keeps the dirty state for retry
- `sys_time` keeps sub-second precision; `filesystem::path` round-trips as UTF-8 (non-ASCII paths safe)
- Invalid `year_month_day` values are rejected on load (field keeps its default)

## Testing

94 assertions covering: 4-thread load/save stress, concurrent field registration, VALUE_CHANGE/VALUE_LOAD/event unsubscribe, round-trip of 26 codec types (including sub-second `sys_time`, UTF-8 paths, `unique_ptr`), error paths (type mismatch, invalid dates, save failure retry) and read/write lock concurrency — see `test.cpp`.
