// 2026-03-29 04:58:49

#pragma once

// https://github.com/1992724048/stdpp-config
// 1.4.0

#include <array>
#include <atomic>
#include <bitset>
#include <chrono>
#include <complex>
#include <deque>
#include <expected>
#include <filesystem>
#include <fstream>
#include <list>
#include <map>
#include <memory>
#include <optional>
#include <ranges>
#include <set>
#include <shared_mutex>
#include <stack>
#include <string>
#include <string_view>
#include <type_traits>
#include <typeindex>
#include <typeinfo>
#include <unordered_map>
#include <unordered_set>
#include <variant>
#include <vector>

// ToruNiina/toml11 4.4.0
#include "toml11/toml.hpp"
// Neargye/magic_enum 0.9.7
#include "magic_enum/magic_enum.hpp"
// 1992724048/stdpp-event 1.0.3
#include "stdpp/event.hpp"

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

    enum class Event : std::uint8_t { VALUE_CHANGE, VALUE_LOAD, };

    template<typename T>
    struct Codec {
        static auto encode(const T& v) -> toml::value {
            return toml::value(v);
        }

        static auto decode(const toml::value& v) -> T {
            return toml::get<T>(v);
        }
    };

    template<typename T> concept Serializable = requires(const T& t, const toml::value& v) {
        { Codec<T>::encode(t) } -> std::same_as<toml::value>;
    } && (requires(const toml::value& v2) {
            { Codec<T>::decode(v2) } -> std::same_as<T>;
        } || requires(T& t2, const toml::value& v2) {
            { Codec<T>::decode_into(v2, t2) } -> std::same_as<void>;
        });

    template<typename T>
    class Field;

    template<typename T>
    class FieldValue;

    struct FieldEntryBase {
        FieldEntryBase(STR name, STR type_name, const std::type_index& type) :
            name{std::move(name)},
            type_name{std::move(type_name)},
            type{type} {}

        virtual ~FieldEntryBase() = default;

        FieldEntryBase(const FieldEntryBase&) = delete;
        auto operator=(const FieldEntryBase&) -> FieldEntryBase& = delete;
        FieldEntryBase(FieldEntryBase&&) = delete;
        auto operator=(FieldEntryBase&&) -> FieldEntryBase& = delete;

        virtual auto encode() -> toml::value {
            return toml::table{};
        }

        virtual auto decode(const toml::value& value) -> void {}
    protected:
        friend class Config;
        template<typename T>
        friend class FieldValueMutex;

        STR name;
        STR type_name;
        std::type_index type;

        std::shared_mutex toml_mutex;
        std::shared_mutex value_mutex;
        std::shared_mutex event_mutex;

        std::atomic_bool is_change{false};

        std::vector<STR> path_parts;

        event::Event<void(const PTR<FieldEntryBase>&, Event)> events;
    };

    template<Serializable T>
    struct FieldEntry final : FieldEntryBase {
        template<typename... Args> requires std::constructible_from<T, Args...>
        FieldEntry(STR name, const STR& type, Args&&... args) :
            FieldEntryBase{std::move(name), typeid(T).name(), typeid(T)},
            value{std::forward<Args>(args)...} {}

        FieldEntry(const FieldEntry&) = delete;
        auto operator=(const FieldEntry&) -> FieldEntry& = delete;
        FieldEntry(FieldEntry&&) = delete;
        auto operator=(FieldEntry&&) -> FieldEntry& = delete;
    private:
        friend class Field<T>;
        friend class FieldValue<T>;
        friend class Config;
        template<typename Type>
        friend class FieldValueMutex;

        T value;
    public:
        auto encode() -> toml::value override {
            std::shared_lock _(value_mutex);
            std::unique_lock _(toml_mutex);
            return Codec<T>::encode(value);
        }

        auto decode(const toml::value& value_toml) -> void override {
            std::unique_lock _(value_mutex);
            std::shared_lock _(toml_mutex);
            if constexpr (requires { Codec<T>::decode_into(value_toml, value); }) {
                Codec<T>::decode_into(value_toml, value);
            } else {
                value = Codec<T>::decode(value_toml);
            }
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

    class Config {
        template<typename T>
        friend class Field;
        template<typename T>
        friend class FieldValue;
        template<typename T>
        friend class FieldValueMutex;
    public:
        auto load(const std::filesystem::path& config_path) -> bool {
            {
                std::unique_lock _(config_mutex);
                path = config_path;
                if (!load_config_from_file()) {
                    return false;
                }
            }

            std::shared_lock _(field_mutex);
            for (auto& entry : field_entrys | std::views::values) {
                find_config_value(entry);
            }

            return true;
        }

        auto refresh() -> bool {
            return load(path);
        }

        auto save() -> bool {
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

        auto mark_dirty() -> void {
            is_dirty = true;
        }

        auto config_path() -> std::filesystem::path {
            return path;
        }

        static auto instance() -> Config& {
            static Config config;
            return config;
        }
    private:
        template<typename T, typename... Args>
        auto find_or_create(const STR& name, const STR& type, Args&&... args) -> FEP<T> {
            const auto typed_name = name + "#" + type;

            if (const auto opt = find_entry(typed_name)) {
                return std::static_pointer_cast<FE<T>>(opt.value());
            }

            auto entry = std::make_shared<FE<T>>(name, type, std::forward<Args>(args)...);
            entry->path_parts = split_path(name);
            find_config_value(entry);
            {
                std::unique_lock _(field_mutex);
                field_entrys[typed_name] = entry;
            }
            return entry;
        }

        auto find_entry(const STR& name) -> OPT<FEBP> {
            std::shared_lock _(field_mutex);
            if (const auto it = field_entrys.find(name); it != field_entrys.end()) {
                return it->second;
            }
            return std::nullopt;
        }

        static auto add_event(const FEBP& entry, const event::Event<void(const FEBP&, Event)>::Func& func) -> OPT<event::Event<void(const FEBP&, Event)>::Handle> {
            return entry->events += func;
        }

        static auto remove_event(const FEBP& entry, const event::Event<void(const FEBP&, Event)>::Handle& handle) -> void {
            return entry->events -= handle;
        }

        toml::value loaded_config = toml::table{};
        bool has_loaded_config = false;
        std::shared_mutex field_mutex;
        std::shared_mutex config_mutex;
        std::atomic_bool is_dirty{false};
        std::filesystem::path path;
        MAP<STR, FEBP> field_entrys;

        static auto split_path(STR name) -> std::vector<STR> {
            using namespace std::literals::string_view_literals;
            std::vector<std::string> parts;

            for (auto&& r : name | std::views::split("::"sv)) {
                parts.emplace_back(&*r.begin(), std::ranges::distance(r));
            }

            return parts;
        }

        auto load_config_from_file() -> bool {
            if (!exists(path)) {
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

        auto save_config_to_file() const -> bool {
            std::ofstream ofs(path);
            if (!ofs) {
                return false;
            }
            ofs << format(loaded_config);
            return true;
        }

        auto find_config_value(const FEBP& entry) const -> void {
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
                try {
                    entry->decode(*node);
                    entry->is_change = false;
                    std::shared_lock _(entry->event_mutex);
                    entry->events(entry, Event::VALUE_LOAD);
                } catch (const toml::type_error&) {}
            }
        }

        auto value_to_config(const FEBP& entry) -> void {
            if (!entry->is_change.exchange(false)) {
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

    template<typename T>
    class FieldValueMutex {
    public:
        explicit FieldValueMutex(FEP<T> entry) :
            entry_(std::move(entry)),
            lock_(entry_->value_mutex) {}

        ~FieldValueMutex() {
            if (dirty_) {
                lock_.unlock();
                entry_->is_change = true;
                Config::instance().mark_dirty();
                std::shared_lock _(entry_->event_mutex);
                entry_->events(entry_, Event::VALUE_CHANGE);
            }
        }

        FieldValueMutex(const FieldValueMutex&) = delete;
        auto operator=(const FieldValueMutex&) -> FieldValueMutex& = delete;

        FieldValueMutex(FieldValueMutex&& other) noexcept :
            entry_(std::move(other.entry_)),
            lock_(std::move(other.lock_)),
            dirty_(other.dirty_) {
            other.dirty_ = false;
        }

        auto operator=(FieldValueMutex&&) -> FieldValueMutex& = delete;

        auto operator=(const T& rhs) -> FieldValueMutex& {
            entry_->value = rhs;
            dirty_ = true;
            return *this;
        }

        auto operator++() -> FieldValueMutex& {
            ++entry_->value;
            dirty_ = true;
            return *this;
        }

        auto operator--() -> FieldValueMutex& {
            --entry_->value;
            dirty_ = true;
            return *this;
        }

        explicit operator bool() const {
            return static_cast<bool>(entry_->value);
        }

        auto operator*() -> T& {
            return entry_->value;
        }

        auto operator*() const -> const T& {
            return entry_->value;
        }

        auto operator->() -> T* {
            return &entry_->value;
        }

        auto operator->() const -> const T* {
            return &entry_->value;
        }

        auto commit() -> void {
            if (dirty_) {
                entry_->is_change = true;
                Config::instance().mark_dirty();
                std::shared_lock _(entry_->event_mutex);
                entry_->events(entry_, Event::VALUE_CHANGE);
                dirty_ = false;
            }
        }
    private:
        FEP<T> entry_;
        std::unique_lock<std::shared_mutex> lock_;
        bool dirty_ = false;
    };

    template<typename T>
    class FieldValue {
    public:
        using Type = T;

        FieldValue() = default;

        operator T() const {
            std::shared_lock _(value_->value_mutex);
            return value_->value;
        }

        auto operator*() const -> T {
            std::shared_lock _(value_->value_mutex);
            return value_->value;
        }

        [[nodiscard]] auto copy() const -> T {
            std::shared_lock _(value_->value_mutex);
            return value_->value;
        }

        auto operator=(const T& rhs) -> FieldValue& {
            std::unique_lock _(value_->value_mutex);
            value_->value = rhs;
            _.unlock();
            change();
            return *this;
        }

        auto operator=(const FieldValue& rhs) -> FieldValue& {
            if (rhs.value_ == value_) {
                return *this;
            }
            {
                std::scoped_lock _(value_->value_mutex, rhs.value_->value_mutex);
                value_->value = rhs.value_->value;
            }
            change();
            return *this;
        }

        auto operator++() -> FieldValue& {
            std::unique_lock _(value_->value_mutex);
            ++value_->value;
            _.unlock();
            change();
            return *this;
        }

        auto operator--() -> FieldValue& {
            std::unique_lock _(value_->value_mutex);
            --value_->value;
            _.unlock();
            change();
            return *this;
        }

        auto operator++(int) -> T {
            std::unique_lock _(value_->value_mutex);
            T old = value_->value;
            ++value_->value;
            _.unlock();
            change();
            return old;
        }

        auto operator--(int) -> T {
            std::unique_lock _(value_->value_mutex);
            T old = value_->value;
            --value_->value;
            _.unlock();
            change();
            return old;
        }

        auto ptr() -> FEP<T> {
            return value_;
        }

        [[nodiscard]] auto ptr() const -> FEP<T> {
            return value_;
        }

        auto name() -> STR {
            return value_->name;
        }

        auto type() -> std::type_index {
            return value_->type;
        }

        auto type_name() -> STR {
            return value_->type_name;
        }

        [[nodiscard]] auto value_lock() const -> FieldValueMutex<T> {
            return FieldValueMutex<T>(value_);
        }

        auto change(const bool has_mutex = false) -> void {
            value_->is_change = true;
            Config::instance().mark_dirty();
            if (has_mutex) {
                value_->events(value_, Event::VALUE_CHANGE);
                return;
            }
            std::shared_lock _(value_->event_mutex);
            value_->events(value_, Event::VALUE_CHANGE);
        }

        auto add_event(event::Event<void(const FEBP&, Event)>::Func func) -> OPT<event::Event<void(const FEBP&, Event)>::Handle> {
            return Config::instance().add_event(value_, func);
        }

        auto remove_event(const event::Event<void(const FEBP&, Event)>::Handle& handle) -> void {
            return Config::instance().remove_event(value_, handle);
        }

        FieldValue(FieldValue&&) = default;
        auto operator=(FieldValue&&) -> FieldValue& = default;
    protected:
        FEP<T> value_;
        friend FieldValueMutex<T>;
    };

    template<typename T> concept HasValueType = requires { typename T::value_type; };
    template<typename T> concept InitListConstructible = HasValueType<T> && std::constructible_from<T, std::initializer_list<typename T::value_type>>;

    template<typename T>
    class Field : public FieldValue<T> {
    public:
        using Type = T;
        using FieldValue<T>::operator=;
        using FieldValue<T>::operator++;
        using FieldValue<T>::operator--;
        using FieldValue<T>::operator*;
    private:
        using FieldValue<T>::ptr;
        using FieldValue<T>::change;
    public:
        Field() = default;

        explicit Field(const STR& field_name) :
            FieldValue<T>{} {
            this->value_ = Config::instance().find_or_create<T>(field_name, typeid(T).name());
            init(field_name);
        }

        template<typename... Args> requires std::constructible_from<T, Args...>
        explicit Field(const STR& field_name, Args&&... args) :
            FieldValue<T>{} {
            this->value_ = Config::instance().find_or_create<T>(field_name, typeid(T).name(), std::forward<Args>(args)...);
            init(field_name);
        }

        template<typename U = T> requires InitListConstructible<U>
        explicit Field(const STR& field_name, std::initializer_list<typename U::value_type> il) :
            FieldValue<T>{} {
            this->value_ = Config::instance().find_or_create<T>(field_name, typeid(T).name(), T(il));
            init(field_name);
        }

        auto create(const STR& field_name) -> void {
            this->value_ = Config::instance().find_or_create<T>(field_name, typeid(T).name());
            init(field_name);
        }

        template<typename... Args> requires std::constructible_from<T, Args...>
        auto create(const STR& field_name, Args&&... args) -> void {
            this->value_ = Config::instance().find_or_create<T>(field_name, typeid(T).name(), std::forward<Args>(args)...);
            init(field_name);
        }

        template<typename U = T> requires InitListConstructible<U>
        auto create(const STR& field_name, std::initializer_list<typename U::value_type> il) -> void {
            this->value_ = Config::instance().find_or_create<T>(field_name, typeid(T).name(), T(il));
            init(field_name);
        }

        ~Field() = default;

        Field(Field&&) = default;
        auto operator=(Field&&) -> Field& = default;

        static auto get() -> std::vector<CFEP<T>> {
            auto& map = registry();
            auto& mtx = registry_mutex();
            std::unique_lock _(mtx);
            std::vector<CFEP<T>> v;
            for (auto& field : map | std::views::values) {
                v.push_back(field);
            }
            return v;
        }
    private:
        static auto registry() -> MAP<STR, FEP<T>>& {
            static MAP<STR, FEP<T>> fields;
            return fields;
        }

        static auto registry_mutex() -> std::shared_mutex& {
            static std::shared_mutex mutex;
            return mutex;
        }

        auto init(const STR& field_name) -> void {
            auto& map = registry();
            auto& mtx = registry_mutex();
            std::unique_lock _(mtx);
            if (map.contains(field_name)) {
                return;
            }
            map[field_name] = this->value_;
        }
    };

    template<typename T> concept NotString = !std::is_same_v<std::decay_t<T>, std::string>;

    template<typename C, typename T> concept HasInsert = requires(C c, T v) {
        c.insert(c.end(), v);
    };

    template<typename M> concept MapLike = requires {
        typename M::key_type; typename M::mapped_type; typename M::value_type;
    };

    template<typename Q> concept AdapterLike = requires(Q q) {
        q.empty(); q.pop();
    };

    template<typename P>
    using ElementTypeT = P::element_type;

    template<typename P> concept PointerLike = requires(P p) {
        typename ElementTypeT<P>; { p.get() } -> std::same_as<ElementTypeT<P>*>; static_cast<bool>(p);
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

    template<typename Q>
    struct AdapterTraits;

    template<typename T, typename Container>
    struct AdapterTraits<std::queue<T, Container>> {
        using value_type = T;

        static auto get(const std::queue<T, Container>& q) -> const T& {
            return q.front();
        }

        static auto pop(std::queue<T, Container>& q) -> void {
            q.pop();
        }

        static auto push(std::queue<T, Container>& q, T&& v) -> void {
            q.push(std::move(v));
        }

        static constexpr bool reverse_on_decode = false;
    };

    template<typename T, typename Container>
    struct AdapterTraits<std::stack<T, Container>> {
        using value_type = T;

        static auto get(const std::stack<T, Container>& s) -> const T& {
            return s.top();
        }

        static auto pop(std::stack<T, Container>& s) -> void {
            s.pop();
        }

        static auto push(std::stack<T, Container>& s, T&& v) -> void {
            s.push(std::move(v));
        }

        static constexpr bool reverse_on_decode = true;
    };

    template<typename T, typename Container, typename Compare>
    struct AdapterTraits<std::priority_queue<T, Container, Compare>> {
        using value_type = T;

        static auto get(const std::priority_queue<T, Container, Compare>& pq) -> const T& {
            return pq.top();
        }

        static auto pop(std::priority_queue<T, Container, Compare>& pq) -> void {
            pq.pop();
        }

        static auto push(std::priority_queue<T, Container, Compare>& pq, T&& v) -> void {
            pq.push(std::move(v));
        }

        static constexpr bool reverse_on_decode = false;
    };

    /**
     * @brief 单向链表(std::forward_list)的 TOML 编解码特化
     * 支持：
     * - std::forward_list<T, Alloc>
     * TOML 表示为数组：
     *   [e0, e1, e2, ...]
     * 顺序与链表遍历顺序一致(front -> next -> ...)
     * @tparam T 元素类型
     * @tparam Alloc 分配器类型
     */
    template<typename T, typename Alloc>
    struct Codec<std::forward_list<T, Alloc>> {
        static auto encode(const std::forward_list<T, Alloc>& l) -> toml::value {
            toml::array arr;
            for (const auto& e : l) {
                arr.push_back(Codec<T>::encode(e));
            }
            return arr;
        }

        static auto decode(const toml::value& v) -> std::forward_list<T, Alloc> {
            if (!v.is_array()) {
                throw std::runtime_error("forward_list decode failed: toml is not array");
            }

            std::forward_list<T, Alloc> l;
            auto before = l.before_begin();
            for (const auto& e : v.as_array()) {
                before = l.insert_after(before, Codec<T>::decode(e));
            }
            return l;
        }
    };

    /**
     * @brief 容器适配器(queue / stack / priority_queue)的 TOML 编解码统一实现
     * TOML 表示为数组：
     *   [e0, e1, e2, ...]
     * 顺序由 AdapterTraits 控制。
     * @tparam Q 容器适配器类型
     */
    template<AdapterLike Q> requires requires { typename AdapterTraits<Q>::value_type; }
    struct CodecAdapter {
        using Traits = AdapterTraits<Q>;
        using T = Traits::value_type;

        static auto encode(const Q& q) -> toml::value {
            toml::array arr;
            auto copy = q;
            while (!copy.empty()) {
                arr.push_back(Codec<T>::encode(Traits::get(copy)));
                Traits::pop(copy);
            }
            return arr;
        }

        static auto decode(const toml::value& v) -> Q {
            if (!v.is_array()) {
                throw std::runtime_error("adapter decode failed: toml is not array");
            }

            Q q;
            const auto& a = v.as_array();

            if constexpr (Traits::reverse_on_decode) {
                for (auto it = a.rbegin(); it != a.rend(); ++it) {
                    Traits::push(q, Codec<T>::decode(*it));
                }
            } else {
                for (const auto& e : a) {
                    Traits::push(q, Codec<T>::decode(e));
                }
            }
            return q;
        }
    };

    template<typename T, typename Container>
    struct Codec<std::queue<T, Container>> : CodecAdapter<std::queue<T, Container>> {};

    template<typename T, typename Container>
    struct Codec<std::stack<T, Container>> : CodecAdapter<std::stack<T, Container>> {};

    template<typename T, typename Container, typename Compare>
    struct Codec<std::priority_queue<T, Container, Compare>> : CodecAdapter<std::priority_queue<T, Container, Compare>> {};

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

            if (!toml::get<bool>(tbl.at("has"))) {
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
            tbl["value"] = std::visit([]<typename T0>(T0&& arg) -> auto {
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
        static auto decode_impl(std::size_t index, const toml::value& value, std::index_sequence<I...> /*unused*/) -> std::variant<Ts...> {
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
            tbl["hours"] = t.hours().count();
            tbl["minutes"] = t.minutes().count();
            tbl["seconds"] = t.seconds().count();
            tbl["subseconds"] = t.subseconds().count();
            return tbl;
        }

        static auto decode(const toml::value& v) -> HMS {
            if (!v.is_table()) {
                throw std::runtime_error("hh_mm_ss decode failed: toml is not table");
            }
            const auto& tbl = v.as_table();
            if (!tbl.contains("hours") || !tbl.contains("minutes") || !tbl.contains("seconds") || !tbl.contains("subseconds")) {
                throw std::runtime_error("hh_mm_ss decode failed: missing field");
            }

            const auto h = std::chrono::hours(toml::get<int64_t>(tbl.at("hours")));
            const auto m = std::chrono::minutes(toml::get<int64_t>(tbl.at("minutes")));
            const auto s = std::chrono::seconds(toml::get<int64_t>(tbl.at("seconds")));
            auto sub = Duration(toml::get<typename Duration::rep>(tbl.at("subseconds")));

            return HMS{h + m + s + sub};
        }
    };

    /**
     * @brief 系统时间点(std::chrono::sys_time)的 TOML 编解码特化
     * TOML 表示为 ISO-8601 字符串：
     *   "2026-01-25T13:45:30Z"
     * @tparam Duration 时间精度
     */
    template<typename Duration>
    struct Codec<std::chrono::sys_time<Duration>> {
        static auto encode(const std::chrono::sys_time<Duration>& t) -> toml::value {
            auto tp = std::chrono::time_point_cast<std::chrono::seconds>(t);
            return std::format("{:%FT%TZ}", tp);
        }

        static auto decode(const toml::value& v) -> std::chrono::sys_time<Duration> {
            if (!v.is_string()) {
                throw std::runtime_error("sys_time decode failed: not string");
            }

            std::istringstream iss(toml::get<std::string>(v));
            std::chrono::sys_time<Duration> tp;
            iss >> std::chrono::parse("%FT%TZ", tp);
            if (iss.fail()) {
                throw std::runtime_error("sys_time decode failed: parse error");
            }

            return tp;
        }
    };

    /**
     * @brief 日期类型(std::chrono::year_month_day)的 TOML 编解码特化
     * TOML 表示为对象：
     *   { y = 2026, m = 1, d = 25 }
     */
    template<>
    struct Codec<std::chrono::year_month_day> {
        static auto encode(const std::chrono::year_month_day& d) -> toml::value {
            toml::table tbl;
            tbl["year"] = static_cast<int>(d.year());
            tbl["month"] = static_cast<unsigned>(d.month());
            tbl["day"] = static_cast<unsigned>(d.day());
            return tbl;
        }

        static auto decode(const toml::value& v) -> std::chrono::year_month_day {
            if (!v.is_table()) {
                throw std::runtime_error("ymd decode failed: not table");
            }

            const auto& t = v.as_table();
            return std::chrono::year{toml::get<int>(t.at("year"))} / toml::get<unsigned>(t.at("month")) / toml::get<unsigned>(t.at("day"));
        }
    };

    /**
     * @brief 带时区时间(std::chrono::zoned_time)的 TOML 编解码特化
     * TOML 表示为对象：
     *   { zone = "Asia/Shanghai", time = "2026-01-25T13:45:30Z" }
     * @tparam Duration 时间精度
     */
    template<typename Duration>
    struct Codec<std::chrono::zoned_time<Duration>> {
        using ZT = std::chrono::zoned_time<Duration>;
        using ST = std::chrono::sys_time<Duration>;

        static auto encode(const ZT& zt) -> toml::value {
            toml::table tbl;
            tbl["zone"] = std::string(zt.get_time_zone()->name());
            tbl["time"] = Codec<ST>::encode(zt.get_sys_time());
            return tbl;
        }

        static auto decode(const toml::value& v) -> ZT {
            if (!v.is_table()) {
                throw std::runtime_error("zoned_time decode failed: not table");
            }

            const auto& t = v.as_table();
            if (!t.contains("zone") || !t.contains("time")) {
                throw std::runtime_error("zoned_time decode failed: missing field");
            }

            const auto zone_name = toml::get<std::string>(t.at("zone"));
            auto st = Codec<ST>::decode(t.at("time"));

            const std::chrono::time_zone* tz = std::chrono::get_tzdb().locate_zone(zone_name);

            return ZT{tz, st};
        }
    };

    /**
     * @brief 时间间隔(std::chrono::duration)的 TOML 编解码特化
     * TOML 表示为对象：
     *   { count = 1500, unit = "ms" }
     * @tparam Rep  数值类型
     * @tparam Period 时间单位
     */
    template<typename Rep, typename Period>
    struct Codec<std::chrono::duration<Rep, Period>> {
        using D = std::chrono::duration<Rep, Period>;

        static auto encode(const D& d) -> toml::value {
            toml::table tbl;
            tbl["count"] = d.count();
            tbl["unit"] = unit_name();
            return tbl;
        }

        static auto decode(const toml::value& v) -> D {
            if (!v.is_table()) {
                throw std::runtime_error("duration decode failed: not table");
            }

            const auto& t = v.as_table();
            if (!t.contains("count") || !t.contains("unit")) {
                throw std::runtime_error("duration decode failed: missing field");
            }

            const auto unit = toml::get<std::string>(t.at("unit"));
            if (unit != unit_name()) {
                throw std::runtime_error("duration decode failed: unit mismatch");
            }

            return D{toml::get<Rep>(t.at("count"))};
        }
    private:
        static auto unit_name() -> std::string {
            if constexpr (std::is_same_v<Period, std::nano>) {
                return "ns";
            }
            if constexpr (std::is_same_v<Period, std::micro>) {
                return "us";
            }
            if constexpr (std::is_same_v<Period, std::milli>) {
                return "ms";
            }
            if constexpr (std::is_same_v<Period, std::ratio<1>>) {
                return "s";
            }
            if constexpr (std::is_same_v<Period, std::ratio<60>>) {
                return "min";
            }
            if constexpr (std::is_same_v<Period, std::ratio<3600>>) {
                return "h";
            } else {
                return "custom";
            }
        }
    };

    /**
     * @brief 指针型包装器(std::unique_ptr / std::shared_ptr)的统一 TOML 编解码
     * TOML 表示为对象：
     *   { has = false }
     *   { has = true, value = T }
     */
    template<PointerLike P>
    struct Codec<P> {
        using T = ElementTypeT<P>;

        static auto encode(const P& p) -> toml::value {
            toml::table tbl;
            if (!p) {
                tbl["has"] = false;
            } else {
                tbl["has"] = true;
                tbl["value"] = Codec<T>::encode(*p);
            }
            return tbl;
        }

        static auto decode(const toml::value& v) -> P {
            if (!v.is_table()) {
                throw std::runtime_error("pointer decode failed: toml is not table");
            }

            const auto& t = v.as_table();
            if (!t.contains("has")) {
                throw std::runtime_error("pointer decode failed: missing 'has'");
            }

            if (!toml::get<bool>(t.at("has"))) {
                return P{};
            }

            if (!t.contains("value")) {
                throw std::runtime_error("pointer decode failed: missing 'value'");
            }

            auto obj = Codec<T>::decode(t.at("value"));

            if constexpr (std::is_same_v<P, std::unique_ptr<T>>) {
                return std::make_unique<T>(std::move(obj));
            } else if constexpr (std::is_same_v<P, std::shared_ptr<T>>) {
                return std::make_shared<T>(std::move(obj));
            } else {
                static_assert(sizeof(P) == 0, "Unsupported pointer type");
            }
            return P{};
        }
    };

    /**
     * @brief 原子类型(std::atomic<T>)的 TOML 编解码特化
     * TOML 表示为：
     * - 与 T 相同的表示形式
     * @tparam T 原子内部值类型
     */
    template<typename T> requires std::is_trivially_copyable_v<T>
    struct Codec<std::atomic<T>> {
        static auto encode(const std::atomic<T>& a) -> toml::value {
            return Codec<T>::encode(a.load(std::memory_order_relaxed));
        }

        static auto decode_into(const toml::value& v, std::atomic<T>& a) -> void {
            a.store(Codec<T>::decode(v), std::memory_order_relaxed);
        }
    };

    /**
     * @brief 结果类型(std::expected<T,E>)的 TOML 编解码特化
     * TOML 表示为对象：
     *   { has = true, value = ... }
     *   { has = false, error = ... }
     */
    template<typename T, typename E>
    struct Codec<std::expected<T, E>> {
        static auto encode(const std::expected<T, E>& e) -> toml::value {
            toml::table tbl;
            if (e.has_value()) {
                tbl["has"] = true;
                tbl["value"] = Codec<T>::encode(*e);
            } else {
                tbl["has"] = false;
                tbl["error"] = Codec<E>::encode(e.error());
            }
            return tbl;
        }

        static auto decode(const toml::value& v) -> std::expected<T, E> {
            if (!v.is_table()) {
                throw std::runtime_error("expected decode failed: not table");
            }

            const auto& t = v.as_table();
            if (toml::get<bool>(t.at("has"))) {
                return Codec<T>::decode(t.at("value"));
            }
            return std::unexpected(Codec<E>::decode(t.at("error")));
        }
    };

    /**
     * @brief 复数(std::complex<T>)的 TOML 编解码特化
     * TOML 表示为数组：
     *   [ real, imag ]
     */
    template<typename T>
    struct Codec<std::complex<T>> {
        static auto encode(const std::complex<T>& c) -> toml::value {
            toml::array arr;
            arr.push_back(c.real());
            arr.push_back(c.imag());
            return arr;
        }

        static auto decode(const toml::value& v) -> std::complex<T> {
            if (!v.is_array() || v.as_array().size() != 2) {
                throw std::runtime_error("complex decode failed: not [2]");
            }

            const auto& a = v.as_array();
            return {toml::get<T>(a[0]), toml::get<T>(a[1])};
        }
    };

    /**
     * @brief 位集合(std::bitset<N>)的 TOML 编解码特化
     * TOML 表示为二进制字符串：
     *   "10101001"
     */
    template<std::size_t N>
    struct Codec<std::bitset<N>> {
        static auto encode(const std::bitset<N>& b) -> toml::value {
            return b.to_string();
        }

        static auto decode(const toml::value& v) -> std::bitset<N> {
            if (!v.is_string()) {
                throw std::runtime_error("bitset decode failed: not string");
            }

            auto s = toml::get<std::string>(v);
            if (s.size() != N) {
                throw std::runtime_error("bitset decode failed: size mismatch");
            }

            return std::bitset<N>(s);
        }
    };

    /**
     * @brief 路径类型特化
     */
    template<>
    struct Codec<std::filesystem::path> {
        static auto encode(const std::filesystem::path& p) -> toml::value {
            return p.string();
        }

        static auto decode(const toml::value& v) -> std::filesystem::path {
            return std::filesystem::path(toml::get<std::string>(v));
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

    namespace detail {
        template<typename Op, typename T>
        auto binop(const FieldValue<T>& a, const FieldValue<T>& b, Op&& op) -> T {
            auto pa = a.ptr();
            auto pb = b.ptr();
            if (pa == pb) {
                std::shared_lock _(pa->value_mutex);
                return op(pa->value, pa->value);
            }
            if (pa < pb) {
                std::shared_lock _(pa->value_mutex);
                std::shared_lock _(pb->value_mutex);
                return op(pa->value, pb->value);
            }
            std::shared_lock _(pb->value_mutex);
            std::shared_lock _(pa->value_mutex);
            return op(pa->value, pb->value);
        }

        template<typename Op, typename T>
        auto binop(const FieldValue<T>& a, const T& b, Op&& op) -> T {
            std::shared_lock _(a.ptr()->value_mutex);
            return op(a.ptr()->value, b);
        }

        template<typename Op, typename T, typename U>
        auto binop(const U& a, const FieldValue<T>& b, Op&& op) -> T {
            std::shared_lock _(b.ptr()->value_mutex);
            return op(static_cast<T>(a), b.ptr()->value);
        }

        template<typename Op, typename T>
        auto cmpassign_self(FieldValue<T>& a, const T& b, Op&& op) -> FieldValue<T>& {
            auto lock = a.value_lock();
            op(*lock, b);
            return a;
        }

        template<typename Op, typename T>
        auto cmpassign_other(T& a, FieldValue<T>& b, Op&& op) -> T& {
            auto lock = b.value_lock();
            op(a, *lock);
            return a;
        }

        template<typename Op, typename T>
        auto cmpassign_same(FieldValue<T>& a, const FieldValue<T>& b, Op&& op) -> FieldValue<T>& {
            if (a.ptr() == b.ptr()) {
                auto lock = a.value_lock();
                op(*lock, *lock);
            } else {
                std::scoped_lock _(a.ptr()->value_mutex, b.ptr()->value_mutex);
                op(a.ptr()->value, b.ptr()->value);
            }
            return a;
        }

        template<typename Op, typename T>
        auto unop(const FieldValue<T>& v, Op&& op) -> T {
            std::shared_lock _(v.ptr()->value_mutex);
            return op(v.ptr()->value);
        }
    } // namespace detail

    #define FV_BINARY_OP(op, op_sym)                                                           \
        template<typename T>                                                                   \
        auto operator op(const FieldValue<T>& a, const FieldValue<T>& b) -> T {                \
            return detail::binop(a, b, [](const T& x, const T& y) { return x op_sym y; });     \
        }                                                                                      \
        template<typename T>                                                                   \
        auto operator op(const FieldValue<T>& a, const T& b) -> T {                            \
            return detail::binop(a, b, [](const T& x, const T& y) { return x op_sym y; });     \
        }                                                                                      \
        template<typename T, typename U>                                                       \
        auto operator op(const U& a, const FieldValue<T>& b) -> T {                            \
            return detail::binop(a, b, [](const T& x, const T& y) { return x op_sym y; });     \
        }

    #define FV_CMP_ASSIGN_OP(op, op_sym)                                                       \
        template<typename T>                                                                   \
        auto operator op(FieldValue<T>& a, const T& b) -> FieldValue<T>& {                     \
            return detail::cmpassign_self(a, b, [](T& x, const T& y) { x op_sym y; });          \
        }                                                                                      \
        template<typename T>                                                                   \
        auto operator op(T& a, FieldValue<T>& b) -> T& {                                       \
            return detail::cmpassign_other(a, b, [](T& x, const T& y) { x op_sym y; });         \
        }                                                                                      \
        template<typename T>                                                                   \
        auto operator op(FieldValue<T>& a, const FieldValue<T>& b) -> FieldValue<T>& {         \
            return detail::cmpassign_same(a, b, [](T& x, const T& y) { x op_sym y; });          \
        }

    FV_BINARY_OP(+, +)
    FV_BINARY_OP(-, -)
    FV_BINARY_OP(*, *)
    FV_BINARY_OP(/, /)

    FV_CMP_ASSIGN_OP(+=, +=)
    FV_CMP_ASSIGN_OP(-=, -=)
    FV_CMP_ASSIGN_OP(*=, *=)
    FV_CMP_ASSIGN_OP(/=, /=)

    FV_BINARY_OP(&, &)
    FV_BINARY_OP(|, |)
    FV_BINARY_OP(^, ^)

    template<typename T>
    auto operator<<(const FieldValue<T>& lhs, std::size_t shift) -> T {
        auto lock = lhs.value_lock();
        return *lock << shift;
    }

    template<typename T>
    auto operator>>(const FieldValue<T>& lhs, std::size_t shift) -> T {
        auto lock = lhs.value_lock();
        return *lock >> shift;
    }

    template<typename T>
    auto operator<<=(const FieldValue<T>& lhs, std::size_t shift) -> T {
        auto lock = lhs.value_lock();
        return *lock <<= shift;
    }

    template<typename T>
    auto operator>>=(const FieldValue<T>& lhs, std::size_t shift) -> T {
        auto lock = lhs.value_lock();
        return *lock >>= shift;
    }

    template<typename T>
    auto operator~(const FieldValue<T>& v) -> T {
        return detail::unop(v,
                            [](const T& x) {
                                return ~x;
                            });
    }

    FV_CMP_ASSIGN_OP(&=, &=)
    FV_CMP_ASSIGN_OP(|=, |=)
    FV_CMP_ASSIGN_OP(^=, ^=)

    template<typename T>
    auto operator<=>(const FieldValue<T>& lhs, const FieldValue<T>& rhs) requires requires(const T& a, const T& b) { a <=> b; } {
        auto lock_a = lhs.value_lock();
        auto lock_b = rhs.value_lock();
        return *lock_a <=> *lock_b;
    }

    template<typename T>
    auto operator<=>(const FieldValue<T>& lhs, const T& rhs) requires requires(const T& a, const T& b) { a <=> b; } {
        auto lock = lhs.value_lock();
        return *lock <=> rhs;
    }

    template<typename T>
    auto operator<=>(const T& lhs, const FieldValue<T>& rhs) requires requires(const T& a, const T& b) { a <=> b; } {
        auto lock = rhs.value_lock();
        return lhs <=> *lock;
    }

    #undef FV_BINARY_OP
    #undef FV_CMP_ASSIGN_OP
} // namespace stdpp::config
