# stdpp::config

A **type-safe, event-driven, thread-safe** C++ configuration system with JSON persistence.
It supports load/save, hierarchical path mapping, enum and container serialization, and configuration change notifications.

The core design philosophy is **“configuration as variables”**:
configuration entries are represented as strongly typed objects in code. Reading and writing them directly manipulates configuration values, changes can be observed via events, and persistence is handled centrally.

---

## Features

* **Strongly typed configuration fields**

  * `Field<T>` / `FieldValue<T>` provide type-safe access
  * Compile-time constraints for JSON-serializable types
* **Automatic JSON mapping**

  * Uses `::` to represent hierarchical paths (e.g. `net::http::port`)
  * Automatically creates and merges JSON object structures
* **Event-driven**

  * Supports configuration load events and value change events
  * Low-overhead event system (`stdpp::event`)
* **Thread-safe**

  * Separate read/write locks for value, JSON, and event handling
  * Safe for concurrent access and modification
* **Extensible serialization**

  * Custom types supported via `Codec<T>`
  * Built-in support for:

    * Fundamental types
    * STL containers
    * Enums (via `magic_enum`)
* **Explicit dirty marking & batch saving**

  * Modifications do not immediately write to disk
  * `Config::save()` persists all changes at once

---

## Dependencies

* C++20
* [nlohmann/json](https://github.com/nlohmann/json) ≥ 3.12.0
* [Neargye/magic_enum](https://github.com/Neargye/magic_enum) ≥ 0.9.7
* [stdpp-event](https://github.com/1992724048/stdpp-event)

---

## Quick Start

### Define configuration fields

```cpp
#include "config.hpp"

using namespace stdpp::config;

Field<int> port("server::port");
Field<int> port2("server::port", 255);
Field<int> port3("port", 255);
```

#### Field identity and default value rules

* Fields are uniquely identified by their **full path string**
* Fields with the same **name and type** share the **same underlying storage**
* The **first constructed field** determines the initial default value
* Subsequent fields with the same name **do not override** the existing value

| Field | Path           | Shared | Default Used |
| ----- | -------------- | ------ | ------------ |
| port  | `server::port` | yes    | `int{}`      |
| port2 | `server::port` | yes    | ignored      |
| port3 | `port`         | no     | `255`        |

Declaring the same name with a **different type** will throw an exception.

---

### Load configuration file

```cpp
Config::load("config.json");
```

* If a field exists in JSON → its value overrides the default
* If it does not exist → the constructor default is retained
* Successfully loaded fields emit a `VALUE_LOAD` event

---

### Read and write values

```cpp
int p = *port;   // read
port2 = 8080;    // write
port2 += 1;      // arithmetic operators supported
```

All write operations:

* Update the value
* Mark the configuration as dirty
* Emit the `VALUE_CHANG` event

---

### Runtime field enumeration (type-based)

All declared fields of a given type can be enumerated at runtime:

```cpp
for (const auto& field : Field<int>::get()) {
    auto value = FieldValue<int>::form_entry(field);
    std::cout << value.name()
              << " (" << value.type_name() << ")"
              << " ptr=" << value.ptr()
              << std::endl;
}
```

This enables **lightweight runtime introspection**, useful for:

* Debugging
* Diagnostics
* Configuration editors
* Tooling and visualization

---

### Save configuration

```cpp
Config::save();
```

* Writes to disk only if changes exist
* Automatically rebuilds the JSON hierarchy
* Outputs formatted JSON (`indent = 4`)
* Creates the file if it does not exist

---

## Configuration Events

### Listening for changes

```cpp
auto handle = port.add_event(
    [](const stdpp::config::FEBP& entry, Event ev) {
        if (ev == Event::VALUE_CHANG) {
            // value changed
        }
    }
);
```

### Event types

```cpp
enum class Event {
    VALUE_CHANG,  // value modified
    VALUE_LOAD    // value loaded from file
};
```

### Removing an event

```cpp
port.remove_event(handle.value());
```

---

## STL Container Support

```cpp
Field<std::vector<int>> ports("net::ports", {80, 443});
```

JSON representation:

```json
{
  "net": {
    "ports": [80, 443]
  }
}
```

All containers matching the following form are supported:

```cpp
template<typename C, typename T> concept HasInsert = requires(C c, T v) {
    c.insert(c.end(), v);
};

template<template<class...> class C, typename T, typename... Args> requires HasInsert<C<T, Args...>, T>
```

Examples include `std::vector`, `std::list`, `std::set` (must support `insert`).

---

## Enum Support

Enum types are supported out of the box and are serialized as **string names**.

```cpp
enum class Mode {
    Debug,
    Release
};

Field<Mode> mode("app::mode", Mode::Debug);
```

JSON representation:

```json
{
  "app": {
    "mode": "Debug"
  }
}
```

### Behavior

* Enums are encoded as their **identifier names**
* Decoding uses `magic_enum::enum_cast`
* Invalid or unknown enum strings will throw an exception
* Enum fields fully support:

  * Default values
  * Events (`VALUE_LOAD`, `VALUE_CHANG`)
  * Dirty tracking and persistence
  * Runtime enumeration via `Field<Enum>::get()`

Enum containers (e.g. `std::vector<Enum>`) are automatically supported via the generic container codec and are serialized as JSON arrays of strings.

---

## Custom Type Serialization

To support a custom type, specialize `Codec<T>`:

```cpp
struct Point {
    int x;
    int y;
};

template<>
struct stdpp::config::Codec<Point> {
    static nlohmann::json encode(const Point& p) {
        return {{"x", p.x}, {"y", p.y}};
    }

    static Point decode(const nlohmann::json& j) {
        return { j.at("x"), j.at("y") };
    }
};
```

Usage:

```cpp
Field<Point> pos("window::pos", {100, 200});
```

---

## Thread Safety

* Each configuration entry internally maintains:

  * `value_mutex` – protects the value
  * `json_mutex` – protects serialization
  * `event_mutex` – protects event handlers
* Concurrent reads are supported
* Writes are synchronized and automatically trigger events

Manual locking is also available:

```cpp
auto lock = port.write_lock();
// safely modify the value
```

---

## Design Notes & Limitations

* All fields are managed via a **global static `Config`**
* Field names (paths) must be unique
* Type mismatches for the same name will throw exceptions
* Fields cannot be removed at runtime
* Configuration files are not created automatically on load
  (the first successful `save()` will create the file)

---

