# stdpp::config

**类型安全、事件驱动、线程安全** 的 C++ 配置系统，基于 TOML 持久化。  
它将配置视为 **强类型变量**，内置丰富的 STL 和 chrono 类型支持，提供变更事件，并支持通过 `Codec<T>` 完全自定义。

## 目录

- [功能特性](#功能特性)
- [依赖](#依赖)
- [字段标识与默认值规则](#字段标识与默认值规则)
- [运算符重载支持](#运算符重载支持)
- [值访问语义](#值访问语义)
- [支持的容器与类型](#支持的容器与类型)
- [示例](#示例)
- [配置事件](#配置事件)
- [自定义类型序列化](#自定义类型序列化)
- [线程安全](#线程安全)
- [设计说明](#设计说明)

---

## 功能特性

* **强类型字段**
  * `Field<T>` / `FieldValue<T>` 使用起来如同普通变量
  * 编译期约束：`Serializable`
* **自动 TOML 映射**
  * 通过 `::` 实现层级路径（如 `net::http::port`）
  * 自动构建嵌套 TOML 表
* **事件驱动**
  * `VALUE_LOAD` — 从文件加载时触发
  * `VALUE_CHANGE` — 运行时值变更时触发
* **线程安全**
  * 值 / TOML / 事件使用独立锁
* **可扩展序列化**
  * 内置支持众多 STL、chrono 和工具类型
  * 用户自定义类型通过 `Codec<T>`
* **批量保存**
  * 变更自动标记脏状态
  * `Config::save()` 一次性持久化所有已修改字段

## 依赖

* C++20
* [ToruNiina/toml11](https://github.com/ToruNiina/toml11) ≥ 4.4.0
* [Neargye/magic_enum](https://github.com/Neargye/magic_enum) ≥ 0.9.7
* [stdpp-event](https://github.com/1992724048/stdpp-event)

## 字段标识与默认值规则

```cpp
#include "config.hpp"
using namespace stdpp::config;
Field<int> a("x");
Field<int> b("x", 255);
Field<int> c("x::y", 5);
Field<std::string> d("x", "default");
```

* 字段由其**完整路径字符串 + 类型签名**唯一标识  
  内部 key 格式为 `path + "#" + typeid(T).name()`（如 `x#int`、`x#NSt7__cxx1112basic_string...`）
* 相同 **name 和 type** 的字段共享同一存储
* 相同 **name 但不同 type** 的字段独立共存 — 不再抛异常
* **首个构造的字段** 决定默认值
* 后续同名同类型声明：
  * 同类型 → 复用已有值，忽略默认参数
* `Config::load()` 加载时：当 TOML 值类型与字段的 `Codec<T>` 不匹配时，该字段静默跳过（保留默认值）

| 声明                        | 内部 Key             | 相同 Key 共享？ | 生效默认值    |
|----------------------------|----------------------|----------------|--------------|
| `Field<int> a("x")`       | `x#int`              | 是             | `int{}`      |
| `Field<int> b("x", 255)`  | `x#int`              | 是             | 忽略         |
| `Field<std::string> d("x")` | `x#NSt7...`       | 否（类型不同） | `std::string{}` |
| `Field<int> c("x::y", 5)` | `x::y#int`           | 否             | `5`          |

## 运算符重载支持

`FieldValue<T>` 的行为与 `T` 一致（前提是 `T` 支持对应运算符）。

### 赋值
* `=`
* 从 `T` 赋值
* 从 `FieldValue<T>` 赋值

### 算术运算
* `+  -  *  /`
* `+= -= *= /=`

### 位运算
* `|  &  ^`
* `|= &= ^=`

### 移位
* `<< >>`
* `<<= >>=`

### 自增 / 自减
* `++x  x++`
* `--x  x--`

### 与原始值混用
* `i = field + 1`
* `i += field`
* `field += other_field`

> 所有运算符仅在底层类型 `T` 支持时才会启用。

## 值访问语义

读取字段时始终返回底层值的 **拷贝**，而非引用：

```cpp
Field<int> x("x", 42);
int val = x;          // 拷贝：operator T()
int val2 = *x;        // 拷贝：operator*()
int val3 = x.copy();  // 显式拷贝
```

如需直接引用访问，使用 `value_lock()` RAII 锁：

```cpp
auto lock = x.value_lock();  // 锁定 value_mutex
*lock = 100;                 // 通过引用原地修改
// ~FieldValueMutex() 自动触发 VALUE_CHANGE 事件
```

> 默认返回拷贝的设计保证了线程安全 — 读取操作不会长时间阻塞值的写访问。

## 支持的容器与类型

### 顺序容器
* `std::vector<T>`
* `std::list<T>`
* `std::deque<T>`
* `std::forward_list<T>`
* `std::array<T, N>`

### 容器适配器
* `std::queue<T>`
* `std::stack<T>`
* `std::priority_queue<T>`

### 关联容器
* `std::set<T>`
* `std::multiset<T>`
* `std::map<K, V>`
* `std::multimap<K, V>`
* `std::unordered_map<K, V>`

### 工具类型
* `std::pair<T1, T2>`
* `std::tuple<Ts...>`
* `std::optional<T>`
* `std::variant<Ts...>`
* `std::expected<T, E>`
* `std::complex<T>`
* `std::bitset<N>`
* `std::filesystem::path`
* `std::atomic<T>`

### 时间与日期（std::chrono）
* `std::chrono::duration`
* `std::chrono::hh_mm_ss`
* `std::chrono::sys_time`
* `std::chrono::year_month_day`
* `std::chrono::zoned_time`

### 指针包装器
* `std::unique_ptr<T>`
* `std::shared_ptr<T>`

### 枚举
* 任意 `enum` 或 `enum class`  
  通过 `magic_enum` 序列化为**字符串名称**

## 示例

```cpp
Field<int> port("server::port", 8080);
Field<int> port2("server::port", 8080); // 忽略 8080 参数
// port == port2

Field<std::vector<int>> vec("test::vec", {1,2,3});

Config::load("config.toml");

Field<std::optional<int>> opt("test::opt", std::nullopt);
Field<Test> mode("app::mode", Test::A);

opt = std::nullopt; // 触发 VALUE_CHANGE

Config::save(); // 仅保存变更
````

TOML 文件内容：

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

## 配置事件

```cpp
auto h = port.add_event([](auto&, Event ev){
    if(ev == Event::VALUE_CHANGE) { /* 值已变更 */ }
});
```

事件类型：

```cpp
enum class Event {
    VALUE_CHANGE,
    VALUE_LOAD
};
```

## 自定义类型序列化

定义一个 `Codec<T>` 特化：

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

使用：

```cpp
Field<Point> pos("window::pos", {10,20});
```

## 线程安全

每个字段内部持有三把锁：

* `value_mutex` — 保护值读写
* `toml_mutex` — 保护编解码
* `event_mutex` — 保护回调

手动加锁：

```cpp
auto lock = field.value_lock();
// 安全修改
```

## 设计说明

* 全局静态 `Config` 单例
* 字段在运行时不可移除
* 同名不同类型字段通过 `#type` 后缀独立共存
* 加载时 TOML 类型不匹配时静默跳过（不抛异常）
* 文件仅在首次成功 `save()` 时创建
