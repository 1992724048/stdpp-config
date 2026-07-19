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

* **Strongly typed fields**
  * `Field<T>` / `FieldValue<T>` behave like normal variables
  * Compile-time constraint: `Serializable`
* **Automatic TOML mapping**
  * Hierarchical paths via `::` (e.g. `net::http::port`)
  * Automatically builds nested TOML tables
* **Event-driven**
  * `VALUE_LOAD` when loaded from file
  * `VALUE_CHANGE` when modified at runtime
* **Thread-safe**
  * Separate locks for value / TOML / events
* **Extensible serialization**
  * Built-in support for many STL, chrono, and utility types
  * User-defined types via `Codec<T>`
* **Batch save**
  * Changes are marked dirty
  * `Config::save()` persists all modified fields at once

## Dependencies

* C++20
* [ToruNiina/toml11](https://github.com/ToruNiina/toml11) ≥ 4.4.0
* [Neargye/magic_enum](https://github.com/Neargye/magic_enum) ≥ 0.9.7
* [stdpp-event](https://github.com/1992724048/stdpp-event)

## Field Identity & Default Value Rules
```cpp
#include "config.hpp"
using namespace stdpp::config;
Field<int> a("x");
Field<int> b("x", 255);
Field<int> c("x::y", 5);
Field<std::string> d("x", "default");
```
* Fields are uniquely identified by their **full path string + type signature**  
  Internally the key is formed as `path + "#" + typeid(T).name()` (e.g. `x#int`, `x#NSt7__cxx1112basic_string...`)
* Fields with the same **name and type** share the same storage
* Fields with the same **name but different type** coexist independently — no exception thrown
* The **first constructed field** decides the default value
* Later declarations with the same name and type:
  * Same type → reuse existing value, ignore default
* On `Config::load()`: when a TOML value type doesn't match a field's `Codec<T>`, the field is silently skipped (keeps its default value)

| Declaration               | Internal Key         | Shared with same key | Default Used |
|---------------------------|----------------------|----------------------|--------------|
| `Field<int> a("x")`      | `x#int`              | yes                  | `int{}`      |
| `Field<int> b("x", 255)` | `x#int`              | yes                  | ignored      |
| `Field<std::string> d("x")` | `x#NSt7...`      | no (different type)  | `std::string{}` |
| `Field<int> c("x::y", 5)` | `x::y#int`          | no                   | `5`          |

## Operator Overload Support

`FieldValue<T>` behaves like `T` if `T` supports the operator.

### Assignment
* `=`
* assign from `T`
* assign from `FieldValue<T>`

### Arithmetic
* `+  -  *  /`
* `+= -= *= /=`

### Bitwise
* `|  &  ^`
* `|= &= ^=`

### Shift
* `<< >>`
* `<<= >>=`

### Increment / Decrement
* `++x  x++`
* `--x  x--`

### Mixed with raw values
* `i = field + 1`
* `i += field`
* `field += other_field`

> All operators are only enabled if the underlying `T` supports them.

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
auto lock = x.value_lock();  // locks value_mutex
*lock = 100;                 // modify in-place via reference
// ~FieldValueMutex() triggers VALUE_CHANGE automatically
```

> The copy-by-default design ensures thread safety — reads never block the value for longer than necessary.

## Supported Containers and Types

### Sequential Containers
* `std::vector<T>`
* `std::list<T>`
* `std::deque<T>`
* `std::forward_list<T>`
* `std::array<T, N>`

### Container Adapters
* `std::queue<T>`
* `std::stack<T>`
* `std::priority_queue<T>`

### Associative Containers
* `std::set<T>`
* `std::multiset<T>`
* `std::map<K, V>`
* `std::multimap<K, V>`
* `std::unordered_map<K, V>`

### Utility Types
* `std::pair<T1, T2>`
* `std::tuple<Ts...>`
* `std::optional<T>`
* `std::variant<Ts...>`
* `std::expected<T, E>`
* `std::complex<T>`
* `std::bitset<N>`
* `std::filesystem::path`
* `std::atomic<T>`

### Time & Date (std::chrono)
* `std::chrono::duration`
* `std::chrono::hh_mm_ss`
* `std::chrono::sys_time`
* `std::chrono::year_month_day`
* `std::chrono::zoned_time`

### Pointer Wrappers
* `std::unique_ptr<T>`
* `std::shared_ptr<T>`

### Enums
* Any `enum` or `enum class`  
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

````

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

Each field internally has:

* `value_mutex` – protects value
* `toml_mutex` – protects encode/decode
* `event_mutex` – protects callbacks

Manual lock:

```cpp
auto lock = field.value_lock();
// modify safely
```

## Design Notes

* Global static `Config`
* Fields cannot be removed at runtime
* Same-name different-type fields coexist independently via `#type` suffix
* TOML value type mismatch during load silently skips the field (no exception)
* File is created only on first successful `save()`
