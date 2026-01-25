#pragma once

// https://github.com/1992724048/stdpp-config
// 1.1.0

#include <unordered_map>
#include <string>
#include <string_view>
#include <memory>
#include <vector>
#include <fstream>
#include <typeinfo>
#include <atomic>
#include <filesystem>
#include <expected>
#include <ranges>
#include <type_traits>
#include <shared_mutex>
#include <array>
#include <optional>
#include <variant>
#include <chrono>

// nlohmann/json 3.12.0
#include "toml11/toml.hpp"
// Neargye/magic_enum 0.9.7
#include "magic_enum/magic_enum.hpp"
// 1992724048/stdpp-event 1.0.2
#include "stdpp/event.h"

namespace stdpp::config {
    template<typename K, typename V>
    using MAP = std::unordered_map<K, V>;
    template<typename T, typename Err>
    using EXP = std::expected<T, Err>;
    template<typename T>
    using OPT = std::optional<T>;
    template<typename T>
    using PTR = std::shared_ptr<T>;
    using STR = std::string;

    enum class Event { VALUE_CHANG, VALUE_LOAD, };

    enum class LockMode { Read, Write };

    /**
     * @brief 类型到 TOML 的编解码器
     * Codec<T> 定义类型 T 与 toml::value 之间的转换规则。
     * 默认行为：
     * - encode：直接返回 toml::value 可隐式构造的值
     * - decode：使用 toml::get<T>()
     * 用户可以通过特化 Codec<T> 来支持自定义类型。
     * @tparam T 可序列化类型
     */
    template<typename T>
    struct Codec {
        static auto encode(const T& v) -> toml::value {
            return toml::value(v);
        }

        static auto decode(const toml::value& v) -> T {
            return toml::get<T>(v);
        }
    };

    template<typename T> concept Serializable = requires(const T& v, const toml::value& tv) {
        { Codec<T>::encode(v) } -> std::same_as<toml::value>; { Codec<T>::decode(tv) } -> std::same_as<T>;
    };


    template<typename T>
    class Field;

    template<typename T>
    class FieldValue;

    /**
     * @brief 配置字段的非模板基类
     * FieldEntryBase 提供所有字段共享的基础能力：
     * - 字段元信息（名称、类型）
     * - TOML 编解码的虚接口
     * - 多线程读写保护
     * - 变更标记与事件分发
     * 该类型：
     * - 不直接存储字段值
     * - 仅由 Config / Field / FieldValue 内部使用
     * - 通过多态支持不同类型字段的统一管理
     */
    struct FieldEntryBase {
        FieldEntryBase(STR name, STR type_name, const std::type_index& type) : name{std::move(name)}, type_name{std::move(type_name)}, type{type} {}

        virtual ~FieldEntryBase() = default;

        /**
         * @brief 将字段当前值编码为 TOML
         * @return TOML value
         * @note 默认实现返回空 table
         */
        virtual auto encode() -> toml::value {
            return toml::table{};
        }

        /**
         * @brief 从 TOML 解码并更新字段值
         * @param value TOML 数据
         * @note 默认实现不做任何处理
         */
        virtual auto decode(const toml::value& value) -> void {}

    protected:
        friend class Config;
        friend class FieldValueMutex;

        STR name;
        STR type_name;
        std::type_index type;

        std::shared_mutex toml_mutex;
        std::shared_mutex value_mutex;
        std::shared_mutex event_mutex;

        std::atomic_bool is_chang{false};

        std::vector<STR> path_parts;

        event::FastEvent<void, const PTR<FieldEntryBase>&, const Event> events;
    };

    /**
     * @brief 强类型字段实体
     * FieldEntry<T> 是：
     * - 某一具体类型 T 的字段存储单元
     * - FieldEntryBase 的模板派生
     * 职责：
     * - 持有字段真实值
     * - 提供线程安全的 TOML 编解码
     * @tparam T 字段值类型，必须满足 Serializable
     */
    template<Serializable T>
    struct FieldEntry : FieldEntryBase {
        template<typename... Args> requires std::constructible_from<T, Args...>
        FieldEntry(STR name, const STR& type, Args&&... args) : FieldEntryBase{std::move(name), typeid(T).name(), typeid(T)}, value{std::forward<Args>(args)...} {}

    private:
        friend class Field<T>;
        friend class FieldValue<T>;
        friend class Config;

        T value;

        auto encode() -> toml::value override {
            std::shared_lock _(value_mutex);
            std::unique_lock _(toml_mutex);
            return Codec<T>::encode(value);
        }

        auto decode(const toml::value& value_toml) -> void override {
            std::shared_lock _(toml_mutex);
            std::unique_lock _(value_mutex);
            value = Codec<T>::decode(value_toml);
        }
    };

    template<typename T>
    using FEP = std::shared_ptr<FieldEntry<T>>;
    using FEB = FieldEntryBase;
    using FEBP = std::shared_ptr<FieldEntryBase>;
    using CFEBP = std::shared_ptr<const FieldEntryBase>;
    template<typename T>
    using CFEP = std::shared_ptr<const FieldEntry<T>>;

    template<typename T>
    using FE = FieldEntry<T>;

    /**
    * @brief 全局配置管理器
    * 负责：
    * - 配置字段的创建与查找
    * - JSON 文件的加载 / 保存
    * - 字段值与配置文件的同步
    * - 配置变更事件分发
    * 该类为纯静态工具类，不可实例化。
    */
    class Config {
    public:
        /**
         * @brief 查找已存在的字段，若不存在则创建
         *
         * @tparam T 字段值类型
         * @tparam Args 构造字段初始值所需的参数
         * @param name 字段完整路径名（使用 "::" 分隔层级）
         * @param type 字段类型名（通常为 typeid(T).name()）
         * @param args 用于构造字段初始值的参数
         * @return 对应字段的共享指针
         * @exception std::runtime_error 若已存在字段但类型与 T 不一致
         */
        template<typename T, typename... Args>
        static auto find_or_create(const STR& name, const STR& type, Args&&... args) -> FEP<T> {
            if (const auto opt = find_entry(name)) {
                const auto& ptr = opt.value();
                if (ptr->type != typeid(T)) {
                    throw std::runtime_error("Types are not equal");
                }
                return std::static_pointer_cast<FE<T>>(ptr);
            }

            auto entry = std::make_shared<FE<T>>(name, type, std::forward<Args>(args)...);
            entry->path_parts = split_path(name);
            find_config_value(entry);
            {
                std::unique_lock _(field_mutex);
                field_entrys[name] = entry;
            }
            return entry;
        }

        /**
         * @brief 从指定路径加载配置文件
         * - 若文件不存在或解析失败，返回 false
         * - 成功后会尝试将已注册字段与配置文件内容同步
         * @param config_path 配置文件路径
         * @return 是否加载成功
         */
        static auto load(const std::filesystem::path& config_path) -> bool {
            path = config_path;

            if (!load_config_from_file()) {
                return false;
            }

            std::shared_lock _(field_mutex);
            for (auto& entry : field_entrys | std::views::values) {
                find_config_value(entry);
            }

            return true;
        }

        /**
         * @brief 重新加载上一次成功 load 的配置文件
         * @return 是否加载成功
         */
        static auto refresh() -> bool {
            return load(path);
        }

        /**
         * @brief 将所有被修改的字段写回配置文件
         * - 仅当存在脏数据时才会写入
         * - 写入成功后清除脏标记
         * @return 是否保存成功
         */
        static auto save() -> bool {
            if (!is_dirty.exchange(false)) {
                return true;
            }


            if (path.empty()) {
                return false;
            }

            std::vector<FEBP> entries;
            {
                std::shared_lock _(field_mutex);
                for (auto& e : field_entrys | std::views::values) {
                    entries.push_back(e);
                }
            }

            {
                std::unique_lock _(config_mutex);
                for (const auto& entry : entries) {
                    value_to_config(entry);
                }
            }

            return save_config_to_file();
        }

        /**
        * @brief 标记当前配置为脏状态
        * 通常在字段值被修改时由内部自动调用。
        */
        static auto mark_dirty() -> void {
            is_dirty = true;
        }

        /**
         * @brief 为指定字段添加事件回调
         * @param name 字段名
         * @param func 回调函数
         * @return 成功时返回事件句柄，否则返回 std::nullopt
         */
        static auto add_event(const STR& name, const event::FastEvent<void, const FEBP&, const Event>::Func func) -> OPT<event::FastEvent<void, const FEBP&, const Event>::Handle> {
            const auto entry = find_entry(name);
            if (!entry) {
                return std::nullopt;
            }
            return add_event(entry.value(), func);
        }

        /**
        * @brief 移除指定字段的事件回调
        * @param name 字段名
        * @param handle 事件句柄
        */
        static auto remove_event(const STR& name, const event::FastEvent<void, const FEBP&, const Event>::Handle handle) -> void {
            const auto entry = find_entry(name);
            if (!entry) {
                return;
            }
            return remove_event(entry.value(), handle);
        }

        /**
         * @brief 为指定字段实体添加事件回调
         * @param entry 字段实体
         * @param func 回调函数
         * @return 事件句柄（可能为空）
         */
        static auto add_event(const FEBP& entry, const event::FastEvent<void, const FEBP&, const Event>::Func func) -> OPT<event::FastEvent<void, const FEBP&, const Event>::Handle> {
            return entry->events += func;
        }

        /**
        * @brief 从字段实体移除事件回调
        * @param entry 字段实体
        * @param handle 事件句柄
        */
        static auto remove_event(const FEBP& entry, const event::FastEvent<void, const FEBP&, const Event>::Handle handle) -> void {
            return entry->events -= handle;
        }

        /**
         * @brief 查找指定名称的字段实体
         * @param name 字段名
         * @return 若存在返回字段指针，否则返回 std::nullopt
         */
        static auto find_entry(const STR& name) -> OPT<FEBP> {
            std::shared_lock _(field_mutex);
            if (const auto it = field_entrys.find(name); it != field_entrys.end()) {
                return it->second;
            }
            return std::nullopt;
        }

    private:
        inline static toml::value loaded_config = toml::table{};
        inline static bool has_loaded_config = false;
        inline static std::shared_mutex field_mutex;
        inline static std::shared_mutex config_mutex;
        inline static std::atomic_bool is_dirty{false};

        inline static std::filesystem::path path;
        inline static MAP<STR, FEBP> field_entrys;

        static auto split_path(STR name) -> std::vector<STR> {
            using namespace std::literals::string_view_literals;
            std::vector<std::string> parts;

            for (auto&& r : name | std::views::split("::"sv)) {
                parts.emplace_back(&*r.begin(), std::ranges::distance(r));
            }

            return parts;
        }

        static auto load_config_from_file() -> bool {
            if (!std::filesystem::exists(path)) {
                loaded_config = toml::table{};
                has_loaded_config = false;
                return false;
            }

            try {
                loaded_config = toml::parse(path.string());
                has_loaded_config = true;
                return true;
            } catch (...) {
                has_loaded_config = false;
                return false;
            }
        }

        static auto save_config_to_file() -> bool {
            std::ofstream ofs(path);
            if (!ofs) {
                return false;
            }
            ofs << toml::format(loaded_config);
            return true;
        }

        static auto find_config_value(const FEBP& entry) -> void {
            const auto& parts = entry->path_parts;
            const toml::value* node = &loaded_config;

            bool found = true;
            for (const auto& p : parts) {
                if (!node->is_table()) {
                    found = false;
                    break;
                }
                const auto& tbl = node->as_table();
                if (!tbl.contains(p)) {
                    found = false;
                    break;
                }
                node = &tbl.at(p);
            }

            if (found) {
                entry->decode(*node);
                entry->is_chang = false;
                std::shared_lock _(entry->event_mutex);
                entry->events(entry, Event::VALUE_LOAD);
            }
        }

        static auto value_to_config(const FEBP& entry) -> void {
            if (!entry->is_chang.exchange(false)) {
                return;
            }

            const auto parts = split_path(entry->name);
            toml::value* node = &loaded_config;

            for (size_t i = 0; i < parts.size(); ++i) {
                const auto& p = parts[i];

                if (i + 1 == parts.size()) {
                    (*node)[p] = entry->encode();
                } else {
                    if (!node->is_table()) {
                        *node = toml::table{};
                    }
                    auto& tbl = node->as_table();
                    if (!tbl.contains(p)) {
                        tbl[p] = toml::table{};
                    }
                    node = &tbl[p];
                }
            }
        }
    };

    /**
    * @brief 字段值互斥访问的 RAII 封装
    * 用于安全访问 FieldValue 内部的 value，
    * 在作用域结束时自动释放锁。
    */
    class FieldValueMutex {
    public:
        FieldValueMutex(const FieldValueMutex&) = delete;
        auto operator=(const FieldValueMutex&) -> FieldValueMutex& = delete;

        FieldValueMutex(FieldValueMutex&&) = default;
        auto operator=(FieldValueMutex&&) -> FieldValueMutex& = default;

        /**
         * @brief 构造并获取字段值锁
         * @param entry 字段实体
         * @param mode 锁模式（读 / 写）
         */
        FieldValueMutex(FEBP entry, const LockMode mode) : entry_(std::move(entry)), mode_(mode) {
            if (mode_ == LockMode::Write) {
                write_lock_.emplace(entry_->value_mutex);
            } else {
                read_lock_.emplace(entry_->value_mutex);
            }
        }

    private:
        FEBP entry_;
        LockMode mode_;

        std::optional<std::unique_lock<std::shared_mutex>> write_lock_;
        std::optional<std::shared_lock<std::shared_mutex>> read_lock_;
    };

    /**
    * @brief 字段值的轻量级访问与操作封装
    * 提供：
    * - 线程安全的读写
    * - 运算符重载
    * - 事件通知
    * 不负责字段的创建与注册。
    */
    template<typename T>
    class FieldValue {
    public:
        using Type = T;

        /**
         * @brief 获取字段值的只读引用
         */
        auto operator*() const -> const T& {
            return value();
        }

        /**
         * @brief 指针访问运算符
         */
        auto operator->() -> T* {
            return &value_->value;
        }

        auto operator->() const -> const T* {
            return &value_->value;
        }

        /**
         * @brief 获取字段值的只读引用
         */
        [[nodiscard]] auto value() const -> const T& {
            return value_->value;
        }

        /**
         * @brief 获取字段值的引用
         */
        auto value() -> T& {
            return value_->value;
        }

        /**
         * @brief 赋值并触发变更事件
         * @param rhs 新值
         * @return 当前对象
         */
        auto operator=(const T& rhs) -> FieldValue& {
            std::unique_lock lock(value_->value_mutex);
            value() = rhs;
            chang();
            return *this;
        }

        /**
         * @brief 赋值并触发变更事件
         * @param rhs 新值
         * @return 当前对象
         */
        auto operator=(const FieldValue& rhs) -> FieldValue& {
            if (rhs.value_ == value_) {
                return *this;
            }
            std::unique_lock lock(value_->value_mutex);
            value() = rhs.value();
            chang();
            return *this;
        }

        auto operator++() -> FieldValue& {
            std::unique_lock lock(value_->value_mutex);
            ++value();
            chang();
            return *this;
        }

        auto operator--() -> FieldValue& {
            std::unique_lock lock(value_->value_mutex);
            --value();
            chang();
            return *this;
        }

        auto operator++(int) -> FieldValue& {
            std::unique_lock lock(value_->value_mutex);
            ++value();
            chang();
            return *this;
        }

        auto operator--(int) -> FieldValue& {
            std::unique_lock lock(value_->value_mutex);
            --value();
            chang();
            return *this;
        }

        template<typename... Args>
        decltype(auto) operator()(Args&&... args) {
            std::unique_lock lock(value_->value_mutex);
            chang();
            return value()(std::forward<Args>(args)...);
        }

        /**
         * @brief 获取字段底层共享指针
         */
        auto ptr() -> FEP<T> {
            return value_;
        }

        [[nodiscard]] auto ptr() const -> FEP<T> {
            return value_;
        }

        /**
         * @brief 获取字段名称
         */
        auto name() -> STR {
            return value_->name;
        }

        /**
        * @brief 获取字段类型索引
        */
        auto type() -> std::type_index {
            return value_->type;
        }

        /**
         * @brief 获取字段类型名称（字符串）
         */
        auto type_name() -> STR {
            return value_->type_name;
        }

        /**
         * @brief 获取字段值的读锁
         */
        [[nodiscard]] auto read_lock() const -> FieldValueMutex {
            return FieldValueMutex(value_, LockMode::Read);
        }

        /**
         * @brief 获取字段值的写锁
         *
         * @note 写锁析构后不会自动触发 chang()
         */
        [[nodiscard]] auto write_lock() -> FieldValueMutex {
            return FieldValueMutex(value_, LockMode::Write);
        }

        /**
         * @brief 主动标记字段发生变更并触发事件
         */
        auto chang() -> void {
            value_->is_chang = true;
            Config::mark_dirty();
            std::shared_lock _(value_->event_mutex);
            value_->events(value_, Event::VALUE_CHANG);
        }

        /**
         * @brief 从字段实体创建 FieldValue
         * @tparam Type 字段实际类型
         * @param entry 字段实体
         * @return FieldValue 实例
         */
        template<typename Type>
        static auto form_entry(const CFEP<Type>& entry) -> FieldValue {
            FieldValue value;
            value.value_ = std::const_pointer_cast<FE<Type>>(entry);
            return value;
        }

        /**
         * @brief 从字段实体创建 FieldValue
         * @tparam Type 字段实际类型
         * @param entry 字段实体
         * @return FieldValue 实例
         */
        static auto form_entry(const FEBP& entry) -> FieldValue {
            return form_entry<T>(std::static_pointer_cast<FE<T>>(entry));
        }

        /**
         * @brief 为当前字段添加事件监听
         * @param func 回调函数
         * @return 事件句柄
         */
        auto add_event(event::FastEvent<void, const FEBP&, const Event>::Func func) -> OPT<event::FastEvent<void, const FEBP&, const Event>::Handle> {
            return Config::add_event(value_, func);
        }

        /**
         * @brief 移除字段事件监听
         * @param handle 事件句柄
         */
        auto remove_event(const event::FastEvent<void, const FEBP&, const Event>::Handle handle) -> void {
            return Config::remove_event(value_, handle);
        }

    protected:
        FEP<T> value_;
    };

    template<typename T> concept HasValueType = requires { typename T::value_type; };
    template<typename T> concept InitListConstructible = HasValueType<T> && std::constructible_from<T, std::initializer_list<typename T::value_type>>;

    /**
     * @brief 强类型配置字段声明
     * Field 在构造时会：
     * - 自动注册到 Config
     * - 自动参与配置加载 / 保存
     * 推荐作为全局或静态对象使用。
     */
    template<typename T>
    class Field : public FieldValue<T> {
    public:
        using Type = T;
        using FieldValue<T>::operator=;
        using FieldValue<T>::operator++;
        using FieldValue<T>::operator--;
        using FieldValue<T>::operator->;
        using FieldValue<T>::operator*;
        using FieldValue<T>::operator();

        /**
         * @brief 声明一个字段（无初始值）
         * @param field_name 字段完整路径名
         */
        explicit Field(const STR& field_name) {
            this->value_ = Config::find_or_create<T>(field_name, typeid(T).name());
            init(field_name);
        }

        /**
        * @brief 声明一个字段并提供初始值
        * @param field_name 字段完整路径名
        * @param args 构造初始值所需参数
        */
        template<typename... Args> requires std::constructible_from<T, Args...>
        explicit Field(const STR& field_name, Args&&... args) {
            this->value_ = Config::find_or_create<T>(field_name, typeid(T).name(), std::forward<Args>(args)...);
            init(field_name);
        }

        template<typename U = T> requires InitListConstructible<U>
        explicit Field(const STR& field_name, std::initializer_list<typename U::value_type> il) {
            this->value_ = Config::find_or_create<T>(field_name, typeid(T).name(), T(il));
            init(field_name);
        }

        ~Field() = default;

        /**
         * @brief 获取当前类型的所有字段实例
         * @return 字段只读指针列表
         */
        static auto get() -> std::vector<CFEP<T>> {
            std::unique_lock _(mutex);
            std::vector<CFEP<T>> v;
            for (auto& field : fields | std::views::values) {
                v.push_back(field);
            }
            return v;
        }

    private:
        inline static std::shared_mutex mutex;
        inline static MAP<STR, FEP<T>> fields;

        auto init(const STR& field_name) -> void {
            if (this->value_->name.empty()) {
                this->value_->name = field_name;
            }
            std::unique_lock _(mutex);
            if (fields.contains(field_name)) {
                return;
            }
            fields[field_name] = this->value_;
        }
    };

    template<typename T> concept NotString = !std::is_same_v<std::decay_t<T>, std::string>;

    template<typename C, typename T> concept HasInsert = requires(C c, T v) {
        c.insert(c.end(), v);
    };

    template<typename M> concept MapLike = requires {
        typename M::key_type; typename M::mapped_type; typename M::value_type;
    };

    /**
     * @brief STL 风格顺序容器的 TOML 编解码特化
     * 支持：
     * - vector / list / deque 等
     * - 要求容器支持 insert(end(), value)
     * TOML 表示为数组。
     * @tparam C 容器模板
     * @tparam T 元素类型
     */
    template<template<class...> class C, typename T, typename... Args> requires HasInsert<C<T, Args...>, T> && (!MapLike<C<T, Args...>>) && NotString<C<T, Args...>>
    struct Codec<C<T, Args...>> {
        static auto encode(const C<T, Args...>& c) -> toml::value {
            toml::array arr;
            for (const auto& e : c) {
                arr.push_back(Codec<T>::encode(e));
            }
            return arr;
        }

        static auto decode(const toml::value& v) -> C<T, Args...> {
            if (!v.is_array()) {
                throw std::runtime_error("Container decode failed: toml is not array");
            }

            C<T, Args...> c;
            for (const auto& e : v.as_array()) {
                c.insert(c.end(), Codec<T>::decode(e));
            }
            return c;
        }
    };

    /**
     * @brief 二元组(pair)的 TOML 编解码特化
     * 支持：
     * - std::pair<T1, T2>
     * TOML 表示为长度为 2 的数组：[first, second]。
     * @tparam T1 第一个元素类型
     * @tparam T2 第二个元素类型
     */
    template<typename T1, typename T2>
    struct Codec<std::pair<T1, T2>> {
        static auto encode(const std::pair<T1, T2>& p) -> toml::value {
            toml::array arr;
            arr.push_back(Codec<T1>::encode(p.first));
            arr.push_back(Codec<T2>::encode(p.second));
            return arr;
        }

        static auto decode(const toml::value& v) -> std::pair<T1, T2> {
            if (!v.is_array() || v.as_array().size() != 2) {
                throw std::runtime_error("pair decode failed: toml is not [2]");
            }
            const auto& a = v.as_array();
            return {Codec<T1>::decode(a[0]), Codec<T2>::decode(a[1])};
        }
    };

    /**
     * @brief STL 风格关联映射容器(map-like)的 TOML 编解码特化
     * 支持：
     * - std::map
     * - std::unordered_map
     * - std::multimap
     * 要求：
     * - 容器元素语义等价于 pair<key, value>
     * - 支持 emplace(key, value)
     * TOML 表示为 pair 的数组：
     *   [ [key1, value1], [key2, value2], ... ]
     * @tparam M 映射容器类型
     */
    template<MapLike M>
    struct Codec<M> {
        using K = M::key_type;
        using V = M::mapped_type;
        using P = std::pair<K, V>;

        static auto encode(const M& m) -> toml::value {
            toml::array arr;
            for (auto& [k, v] : m) {
                arr.push_back(Codec<P>::encode({k, v}));
            }
            return arr;
        }

        static auto decode(const toml::value& v) -> M {
            if (!v.is_array()) {
                throw std::runtime_error("map decode failed: toml is not array");
            }

            M m;
            for (auto& e : v.as_array()) {
                auto [k, val] = Codec<P>::decode(e);
                m.emplace(std::move(k), std::move(val));
            }
            return m;
        }
    };

    /**
     * @brief STL 定长顺序容器(std::array)的 TOML 编解码特化
     * 支持：
     * - std::array<T, N>
     * 要求：
     * - TOML 数组长度必须等于 N
     * TOML 表示为数组。
     * @tparam T 元素类型
     * @tparam N 编译期长度
     */
    template<typename T, std::size_t N>
    struct Codec<std::array<T, N>> {
        static auto encode(const std::array<T, N>& a) -> toml::value {
            toml::array arr;
            for (const auto& e : a) {
                arr.push_back(Codec<T>::encode(e));
            }
            return arr;
        }

        static auto decode(const toml::value& v) -> std::array<T, N> {
            if (!v.is_array()) {
                throw std::runtime_error("array decode failed: toml is not array");
            }
            if (v.as_array().size() != N) {
                throw std::runtime_error("array decode failed: size mismatch");
            }

            std::array<T, N> a{};
            for (std::size_t i = 0; i < N; ++i) {
                a[i] = Codec<T>::decode(v.as_array()[i]);
            }
            return a;
        }
    };

    /**
     * @brief 元组(tuple)的 TOML 编解码特化
     * 支持：
     * - std::tuple<Ts...>
     * TOML 表示为按顺序排列的数组：
     *   [e0, e1, e2, ...]
     * @tparam Ts 元素类型参数包
     */
    template<typename... Ts>
    struct Codec<std::tuple<Ts...>> {
        static auto encode(const std::tuple<Ts...>& t) -> toml::value {
            toml::array arr;
            encode_impl(arr, t, std::index_sequence_for<Ts...>{});
            return arr;
        }

        static auto decode(const toml::value& v) -> std::tuple<Ts...> {
            if (!v.is_array() || v.as_array().size() != sizeof...(Ts)) {
                throw std::runtime_error("tuple decode failed: size mismatch");
            }
            return decode_impl(v.as_array(), std::index_sequence_for<Ts...>{});
        }

    private:
        template<std::size_t... I>
        static auto encode_impl(toml::array& arr, const std::tuple<Ts...>& t, std::index_sequence<I...>) -> void {
            (arr.push_back(Codec<std::tuple_element_t<I, std::tuple<Ts...>>>::encode(std::get<I>(t))), ...);
        }

        template<std::size_t... I>
        static auto decode_impl(const toml::array& arr, std::index_sequence<I...>) -> std::tuple<Ts...> {
            return {Codec<std::tuple_element_t<I, std::tuple<Ts...>>>::decode(arr[I])...};
        }
    };

    /**
     * @brief 可选值(optional)的 TOML 编解码特化
     * 支持：
     * - std::optional<T>
     * TOML 表示为 table：
     * - 无值：{ has = false }
     * - 有值：{ has = true, value = T }
     * @tparam T 元素类型
     */
    template<typename T>
    struct Codec<std::optional<T>> {
        static auto encode(const std::optional<T>& o) -> toml::value {
            toml::table tbl;
            if (!o) {
                tbl["has"] = false;
            } else {
                tbl["has"] = true;
                tbl["value"] = Codec<T>::encode(*o);
            }
            return tbl;
        }

        static auto decode(const toml::value& v) -> std::optional<T> {
            if (!v.is_table()) {
                throw std::runtime_error("optional decode failed: toml is not table");
            }
            const auto& tbl = v.as_table();
            if (!tbl.contains("has")) {
                throw std::runtime_error("optional decode failed: missing 'has'");
            }

            const bool has = toml::get<bool>(tbl.at("has"));
            if (!has) {
                return std::nullopt;
            }

            if (!tbl.contains("value")) {
                throw std::runtime_error("optional decode failed: missing 'value'");
            }
            return Codec<T>::decode(tbl.at("value"));
        }
    };


    /**
     * @brief 变体类型(variant)的 TOML 编解码特化
     * 支持：
     * - std::variant<Ts...>
     * TOML 表示为对象：
     *   { index = i, value = v }
     * @tparam Ts 可选类型参数包
     */
    template<typename... Ts>
    struct Codec<std::variant<Ts...>> {
        static auto encode(const std::variant<Ts...>& v) -> toml::value {
            toml::table tbl;
            tbl["index"] = static_cast<int64_t>(v.index());
            tbl["value"] = std::visit([]<typename T0>(T0&& arg) {
                                          using T = std::decay_t<T0>;
                                          return Codec<T>::encode(arg);
                                      },
                                      v);
            return tbl;
        }

        static auto decode(const toml::value& v) -> std::variant<Ts...> {
            if (!v.is_table() || !v.as_table().contains("index") || !v.as_table().contains("value")) {
                throw std::runtime_error("variant decode failed: invalid toml table");
            }

            const auto& tbl = v.as_table();
            std::size_t index = toml::get<std::size_t>(tbl.at("index"));
            return decode_impl(index, tbl.at("value"), std::index_sequence_for<Ts...>{});
        }

    private:
        template<std::size_t... I>
        static auto decode_impl(std::size_t index, const toml::value& value, std::index_sequence<I...>) -> std::variant<Ts...> {
            std::variant<Ts...> v;
            const bool matched = ((index == I ? (v = Codec<std::tuple_element_t<I, std::tuple<Ts...>>>::decode(value), true) : false) || ...);
            if (!matched) {
                throw std::runtime_error("variant decode failed: index out of range");
            }
            return v;
        }
    };

    /**
     * @brief 时分秒结构(std::chrono::hh_mm_ss)的 TOML 编解码特化
     * 支持：
     * - std::chrono::hh_mm_ss<Duration>
     * TOML 表示为对象：
     *   { hh = hour, mm = minute, ss = second, sub = subsecond }
     * @tparam Duration 底层时间精度类型
     */
    template<typename Duration>
    struct Codec<std::chrono::hh_mm_ss<Duration>> {
        using HMS = std::chrono::hh_mm_ss<Duration>;

        static auto encode(const HMS& t) -> toml::value {
            toml::table tbl;
            tbl["hh"] = t.hours().count();
            tbl["mm"] = t.minutes().count();
            tbl["ss"] = t.seconds().count();
            tbl["sub"] = t.subseconds().count();
            return tbl;
        }

        static auto decode(const toml::value& v) -> HMS {
            if (!v.is_table()) {
                throw std::runtime_error("hh_mm_ss decode failed: toml is not table");
            }
            const auto& tbl = v.as_table();
            if (!tbl.contains("hh") || !tbl.contains("mm") || !tbl.contains("ss") || !tbl.contains("sub")) {
                throw std::runtime_error("hh_mm_ss decode failed: missing field");
            }

            const auto h = std::chrono::hours(toml::get<int64_t>(tbl.at("hh")));
            const auto m = std::chrono::minutes(toml::get<int64_t>(tbl.at("mm")));
            const auto s = std::chrono::seconds(toml::get<int64_t>(tbl.at("ss")));
            auto sub = Duration(toml::get<typename Duration::rep>(tbl.at("sub")));

            return HMS{h + m + s + sub};
        }
    };

    /**
     * @brief 枚举类型的 TOML 编解码特化
     * 使用 magic_enum：
     * - TOML 中存储枚举名字符串
     * - 解码时根据字符串反射枚举值
     * @tparam E 枚举类型
     */
    template<typename E> requires std::is_enum_v<E>
    struct Codec<E> {
        static auto encode(E v) -> toml::value {
            auto name = magic_enum::enum_name(v);
            if (name.empty()) {
                throw std::runtime_error("Enum encode failed: unknown value");
            }
            return std::string{name};
        }

        static auto decode(const toml::value& v) -> E {
            if (!v.is_string()) {
                throw std::runtime_error("Enum decode failed: toml is not string");
            }

            const auto str = toml::get<std::string>(v);
            if (auto opt = magic_enum::enum_cast<E>(str)) {
                return *opt;
            }

            throw std::runtime_error("Enum decode failed: invalid enum name: " + str);
        }
    };

    template<typename T>
    auto operator<=>(const FieldValue<T>& lhs, const FieldValue<T>& rhs) requires requires(const T& a, const T& b) { a <=> b; } {
        auto _ = lhs.read_lock();
        auto _ = rhs.read_lock();
        return lhs.value() <=> rhs.value();
    }

    template<typename T>
    auto operator==(const FieldValue<T>& lhs, const FieldValue<T>& rhs) requires requires(const T& a, const T& b) { a == b; } {
        auto _ = lhs.read_lock();
        auto _ = rhs.read_lock();
        return lhs.value() == rhs.value();
    }

    template<typename T>
    auto operator<(const FieldValue<T>& lhs, const T& rhs) -> bool {
        auto _ = lhs.read_lock();
        return lhs.value() < rhs;
    }

    template<typename T>
    auto operator>(const FieldValue<T>& lhs, const T& rhs) -> bool {
        auto _ = lhs.read_lock();
        return rhs < lhs.value();
    }

    template<typename T>
    auto operator<=(const FieldValue<T>& lhs, const T& rhs) -> bool {
        auto _ = lhs.read_lock();
        return !(lhs > rhs);
    }

    template<typename T>
    auto operator>=(const FieldValue<T>& lhs, const T& rhs) -> bool {
        auto _ = lhs.read_lock();
        return !(lhs < rhs);
    }

    template<typename T>
    auto operator==(const FieldValue<T>& lhs, const T& rhs) -> bool {
        auto _ = lhs.read_lock();
        return lhs.value() == rhs;
    }

    template<typename T>
    auto operator<(const T& lhs, const FieldValue<T>& rhs) -> bool {
        auto _ = rhs.read_lock();
        return lhs < rhs.value();
    }

    template<typename T>
    auto operator>(const T& lhs, const FieldValue<T>& rhs) -> bool {
        auto _ = rhs.read_lock();
        return rhs.value() < lhs;
    }

    template<typename T>
    auto operator<=(const T& lhs, const FieldValue<T>& rhs) -> bool {
        auto _ = rhs.read_lock();
        return !(lhs > rhs);
    }

    template<typename T>
    auto operator>=(const T& lhs, const FieldValue<T>& rhs) -> bool {
        auto _ = rhs.read_lock();
        return !(lhs < rhs);
    }

    template<typename T>
    auto operator==(const T& lhs, const FieldValue<T>& rhs) -> bool {
        auto _ = rhs.read_lock();
        return lhs == rhs.value();
    }

    template<typename T>
    auto operator+(const FieldValue<T>& lhs, const FieldValue<T>& rhs) -> T {
        auto _ = lhs.read_lock();
        auto _ = rhs.read_lock();
        return lhs.value() + rhs.value();
    }

    template<typename T>
    auto operator-(const FieldValue<T>& lhs, const FieldValue<T>& rhs) -> T {
        auto _ = lhs.read_lock();
        auto _ = rhs.read_lock();
        return lhs.value() - rhs.value();
    }

    template<typename T>
    auto operator*(const FieldValue<T>& lhs, const FieldValue<T>& rhs) -> T {
        auto _ = lhs.read_lock();
        auto _ = rhs.read_lock();
        return lhs.value() * rhs.value();
    }

    template<typename T>
    auto operator/(const FieldValue<T>& lhs, const FieldValue<T>& rhs) -> T {
        auto _ = lhs.read_lock();
        auto _ = rhs.read_lock();
        return lhs.value() / rhs.value();
    }

    template<typename T>
    auto operator+(const FieldValue<T>& lhs, const T& rhs) -> T {
        auto _ = lhs.read_lock();
        return lhs.value() + rhs;
    }

    template<typename T>
    auto operator-(const FieldValue<T>& lhs, const T& rhs) -> T {
        auto _ = lhs.read_lock();
        return lhs.value() - rhs;
    }

    template<typename T>
    auto operator*(const FieldValue<T>& lhs, const T& rhs) -> T {
        auto _ = lhs.read_lock();
        return lhs.value() * rhs;
    }

    template<typename T>
    auto operator/(const FieldValue<T>& lhs, const T& rhs) -> T {
        auto _ = lhs.read_lock();
        return lhs.value() / rhs;
    }

    template<typename T>
    auto operator+(const T& lhs, const FieldValue<T>& rhs) -> T {
        auto _ = rhs.read_lock();
        return lhs + rhs.value();
    }

    template<typename T>
    auto operator-(const T& lhs, const FieldValue<T>& rhs) -> T {
        auto _ = rhs.read_lock();
        return lhs - rhs.value();
    }

    template<typename T>
    auto operator*(const T& lhs, const FieldValue<T>& rhs) -> T {
        auto _ = rhs.read_lock();
        return lhs * rhs.value();
    }

    template<typename T>
    auto operator/(const T& lhs, const FieldValue<T>& rhs) -> T {
        auto _ = rhs.read_lock();
        return lhs / rhs.value();
    }

    template<typename T>
    auto operator+=(FieldValue<T>& lhs, const T& rhs) -> FieldValue<T>& {
        auto _ = lhs.write_lock();
        lhs.value() += rhs;
        lhs.chang();
        return lhs;
    }

    template<typename T>
    auto operator+=(T& lhs, FieldValue<T>& rhs) -> T& {
        auto _ = rhs.read_lock();
        lhs += rhs.value();
        return lhs;
    }

    template<typename T>
    auto operator-=(FieldValue<T>& lhs, const T& rhs) -> FieldValue<T>& {
        auto _ = lhs.write_lock();
        lhs.value() -= rhs;
        lhs.chang();
        return lhs;
    }

    template<typename T>
    auto operator-=(T& lhs, FieldValue<T>& rhs) -> T& {
        auto _ = rhs.read_lock();
        lhs -= rhs.value();
        return lhs;
    }

    template<typename T>
    auto operator*=(FieldValue<T>& lhs, const T& rhs) -> FieldValue<T>& {
        auto _ = lhs.write_lock();
        lhs.value() *= rhs;
        lhs.chang();
        return lhs;
    }

    template<typename T>
    auto operator*=(T& lhs, FieldValue<T>& rhs) -> T& {
        auto _ = rhs.read_lock();
        lhs *= rhs.value();
        return lhs;
    }

    template<typename T>
    auto operator/=(FieldValue<T>& lhs, const T& rhs) -> FieldValue<T>& {
        auto _ = lhs.write_lock();
        lhs.value() /= rhs;
        lhs.chang();
        return lhs;
    }

    template<typename T>
    auto operator/=(T& lhs, FieldValue<T>& rhs) -> T& {
        auto _ = rhs.read_lock();
        lhs /= rhs.value();
        return lhs;
    }

    template<typename T>
    auto operator+=(FieldValue<T>& lhs, const FieldValue<T>& rhs) -> FieldValue<T>& {
        auto _ = lhs.write_lock();
        if (lhs.ptr() == rhs.ptr()) {
            lhs.value() += rhs.value();
        } else {
            auto _ = rhs.read_lock();
            lhs.value() += rhs.value();
        }
        lhs.chang();
        return lhs;
    }

    template<typename T>
    auto operator-=(FieldValue<T>& lhs, const FieldValue<T>& rhs) -> FieldValue<T>& {
        auto _ = lhs.write_lock();
        if (lhs.ptr() == rhs.ptr()) {
            lhs.value() -= rhs.value();
        } else {
            auto _ = rhs.read_lock();
            lhs.value() -= rhs.value();
        }
        lhs.chang();
        return lhs;
    }

    template<typename T>
    auto operator*=(FieldValue<T>& lhs, const FieldValue<T>& rhs) -> FieldValue<T>& {
        auto _ = lhs.write_lock();
        if (lhs.ptr() == rhs.ptr()) {
            lhs.value() *= rhs.value();
        } else {
            auto _ = rhs.read_lock();
            lhs.value() *= rhs.value();
        }
        lhs.chang();
        return lhs;
    }

    template<typename T>
    auto operator/=(FieldValue<T>& lhs, const FieldValue<T>& rhs) -> FieldValue<T>& {
        auto _ = lhs.write_lock();
        if (lhs.ptr() == rhs.ptr()) {
            lhs.value() /= rhs.value();
        } else {
            auto _ = rhs.read_lock();
            lhs.value() /= rhs.value();
        }
        lhs.chang();
        return lhs;
    }

    template<typename T>
    auto operator&(const FieldValue<T>& lhs, const FieldValue<T>& rhs) -> T {
        auto _ = lhs.read_lock();
        auto _ = rhs.read_lock();
        return lhs.value() & rhs.value();
    }

    template<typename T>
    auto operator|(const FieldValue<T>& lhs, const FieldValue<T>& rhs) -> T {
        auto _ = lhs.read_lock();
        auto _ = rhs.read_lock();
        return lhs.value() | rhs.value();
    }

    template<typename T>
    auto operator^(const FieldValue<T>& lhs, const FieldValue<T>& rhs) -> T {
        auto _ = lhs.read_lock();
        auto _ = rhs.read_lock();
        return lhs.value() ^ rhs.value();
    }

    template<typename T>
    auto operator&(const FieldValue<T>& lhs, T rhs) -> T {
        auto _ = lhs.read_lock();
        return lhs.value() & rhs;
    }

    template<typename T>
    auto operator|(const FieldValue<T>& lhs, T rhs) -> T {
        auto _ = lhs.read_lock();
        return lhs.value() | rhs;
    }

    template<typename T>
    auto operator^(const FieldValue<T>& lhs, T rhs) -> T {
        auto _ = lhs.read_lock();
        return lhs.value() ^ rhs;
    }

    template<typename T>
    auto operator&(T lhs, const FieldValue<T>& rhs) -> T {
        auto _ = rhs.read_lock();
        return lhs & rhs.value();
    }

    template<typename T>
    auto operator|(T lhs, const FieldValue<T>& rhs) -> T {
        auto _ = rhs.read_lock();
        return lhs | rhs.value();
    }

    template<typename T>
    auto operator^(T lhs, const FieldValue<T>& rhs) -> T {
        auto _ = rhs.read_lock();
        return lhs ^ rhs.value();
    }

    template<typename T>
    auto operator<<(const FieldValue<T>& lhs, std::size_t shift) -> T {
        auto _ = lhs.read_lock();
        return lhs.value() << shift;
    }

    template<typename T>
    auto operator<<=(const FieldValue<T>& lhs, std::size_t shift) -> T {
        auto _ = lhs.write_lock();
        lhs.chang();
        return lhs.value() <<= shift;
    }

    template<typename T>
    auto operator>>(const FieldValue<T>& lhs, std::size_t shift) -> T {
        auto _ = lhs.read_lock();
        return lhs.value() >> shift;
    }

    template<typename T>
    auto operator>>=(const FieldValue<T>& lhs, std::size_t shift) -> T {
        auto _ = lhs.write_lock();
        lhs.chang();
        return lhs.value() >>= shift;
    }

    template<typename T>
    auto operator~(const FieldValue<T>& v) -> T {
        auto _ = v.read_lock();
        return ~v.value();
    }

    template<typename T>
    auto operator&=(FieldValue<T>& lhs, T rhs) -> FieldValue<T>& {
        auto _ = lhs.write_lock();
        lhs.value() &= rhs;
        lhs.chang();
        return lhs;
    }

    template<typename T>
    auto operator&=(T& lhs, FieldValue<T>& rhs) -> T& {
        auto _ = rhs.read_lock();
        lhs &= rhs.value();
        return lhs;
    }

    template<typename T>
    auto operator|=(FieldValue<T>& lhs, T rhs) -> FieldValue<T>& {
        auto _ = lhs.write_lock();
        lhs.value() |= rhs;
        lhs.chang();
        return lhs;
    }

    template<typename T>
    auto operator|=(T& lhs, FieldValue<T>& rhs) -> T& {
        auto _ = rhs.read_lock();
        lhs |= rhs.value();
        return lhs;
    }

    template<typename T>
    auto operator^=(FieldValue<T>& lhs, T rhs) -> FieldValue<T>& {
        auto _ = lhs.write_lock();
        lhs.value() ^= rhs;
        lhs.chang();
        return lhs;
    }

    template<typename T>
    auto operator^=(T& lhs, FieldValue<T>& rhs) -> T& {
        auto _ = rhs.read_lock();
        lhs ^= rhs.value();
        return lhs;
    }

    template<typename T>
    auto operator&=(FieldValue<T>& lhs, const FieldValue<T>& rhs) -> FieldValue<T>& {
        auto _ = lhs.write_lock();
        if (lhs.ptr() == rhs.ptr()) {
            lhs.value() &= rhs.value();
        } else {
            auto _ = rhs.read_lock();
            lhs.value() &= rhs.value();
        }
        lhs.chang();
        return lhs;
    }

    template<typename T>
    auto operator|=(FieldValue<T>& lhs, const FieldValue<T>& rhs) -> FieldValue<T>& {
        auto _ = lhs.write_lock();
        if (lhs.ptr() == rhs.ptr()) {
            lhs.value() |= rhs.value();
        } else {
            auto _ = rhs.read_lock();
            lhs.value() |= rhs.value();
        }
        lhs.chang();
        return lhs;
    }

    template<typename T>
    auto operator^=(FieldValue<T>& lhs, const FieldValue<T>& rhs) -> FieldValue<T>& {
        auto _ = lhs.write_lock();
        if (lhs.ptr() == rhs.ptr()) {
            lhs.value() ^= rhs.value();
        } else {
            auto _ = rhs.read_lock();
            lhs.value() ^= rhs.value();
        }
        lhs.chang();
        return lhs;
    }
}
