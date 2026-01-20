#pragma once

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
#include <optional>

// nlohmann/json 3.12.0
#include "json/json.hpp"
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

    template<typename T>
    struct Codec {
        static auto encode(const T& v) -> nlohmann::json {
            return v;
        }

        static auto decode(const nlohmann::json& j) -> T {
            return j.get<T>();
        }
    };

    template<typename T> concept JsonSerializable = requires(const T& v, const nlohmann::json& j) {
        { Codec<T>::encode(v) } -> std::same_as<nlohmann::json>; { Codec<T>::decode(j) } -> std::same_as<T>;
    };

    template<typename T>
    class Field;

    template<typename T>
    class FieldValue;

    struct FieldEntryBase {
        FieldEntryBase(STR name, STR type_name, const std::type_index& type) : name{std::move(name)}, type_name{std::move(type_name)}, type{type} {}
        virtual ~FieldEntryBase() = default;

        virtual auto encode() -> nlohmann::json {
            return nlohmann::json::object();
        }

        virtual auto decode(const nlohmann::json& json) -> void {}

    protected:
        friend class Config;

        STR name;
        STR type_name;
        std::type_index type;

        std::shared_mutex json_mutex;
        std::shared_mutex value_mutex;
        std::shared_mutex event_mutex;

        std::atomic_bool is_chang{false};

        std::vector<STR> path_parts;

        event::FastEvent<void, const PTR<FieldEntryBase>&, const Event> events;
    };

    template<JsonSerializable T>
    struct FieldEntry : FieldEntryBase {
        template<typename... Args>
        FieldEntry(STR name, const STR& type, Args&&... args) : FieldEntryBase{std::move(name), typeid(T).name(), typeid(T)}, value(std::forward<Args>(args)...) {}

    private:
        friend class Field<T>;
        friend class FieldValue<T>;
        friend class Config;

        T value;

        auto encode() -> nlohmann::json override {
            std::shared_lock _(value_mutex);
            std::unique_lock _(json_mutex);
            return Codec<T>::encode(value);
        }

        auto decode(const nlohmann::json& json) -> void override {
            std::shared_lock _(json_mutex);
            std::unique_lock _(value_mutex);
            value = Codec<T>::decode(json);
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
    public:
        /**
         * @brief 
         * @tparam T 
         * @tparam Args 
         * @param name 
         * @param type 
         * @param args 
         * @return 
         * @exception std::runtime_error 
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

        static auto refresh() -> bool {
            return load(path);
        }

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

        static auto mark_dirty() -> void {
            is_dirty = true;
        }

        static auto add_event(const STR& name, const event::FastEvent<void, const FEBP&, const Event>::Func func) -> OPT<event::FastEvent<void, const FEBP&, const Event>::Handle> {
            const auto entry = find_entry(name);
            if (!entry) {
                return std::nullopt;
            }
            return add_event(entry.value(), func);
        }

        static auto remove_event(const STR& name, const event::FastEvent<void, const FEBP&, const Event>::Handle handle) -> void {
            const auto entry = find_entry(name);
            if (!entry) {
                return;
            }
            return remove_event(entry.value(), handle);
        }

        static auto add_event(const FEBP& entry, const event::FastEvent<void, const FEBP&, const Event>::Func func) -> OPT<event::FastEvent<void, const FEBP&, const Event>::Handle> {
            return entry->events += func;
        }

        static auto remove_event(const FEBP& entry, const event::FastEvent<void, const FEBP&, const Event>::Handle handle) -> void {
            return entry->events -= handle;
        }

        static auto find_entry(const STR& name) -> OPT<FEBP> {
            std::shared_lock _(field_mutex);
            if (const auto it = field_entrys.find(name); it != field_entrys.end()) {
                return it->second;
            }
            return std::nullopt;
        }

    private:
        inline static nlohmann::json loaded_config = nlohmann::json::object();
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
                loaded_config = nlohmann::json::object();
                has_loaded_config = false;
                return false;
            }

            std::ifstream ifs(path);
            if (!ifs) {
                has_loaded_config = false;
                return false;
            }

            ifs >> loaded_config;
            has_loaded_config = true;
            return true;
        }

        static auto save_config_to_file() -> bool {
            std::ofstream ofs(path);
            if (!ofs) {
                return false;
            }
            ofs << loaded_config.dump(4);
            return true;
        }

        static auto find_config_value(const FEBP& entry) -> void {
            const auto& parts = entry->path_parts;
            const nlohmann::json* node = &loaded_config;

            bool found = true;
            for (const auto& p : parts) {
                if (!node->contains(p)) {
                    found = false;
                    break;
                }
                node = &(*node)[p];
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
            nlohmann::json* node = &loaded_config;

            for (size_t i = 0; i < parts.size(); ++i) {
                const auto& p = parts[i];
                if (i + 1 == parts.size()) {
                    (*node)[p] = entry->encode();
                } else {
                    node = &(*node)[p];
                    if (!node->is_object()) {
                        *node = nlohmann::json::object();
                    }
                }
            }
        }
    };

    template<typename T>
    class FieldValue {
    public:
        auto operator*() const -> const T& {
            return value();
        }

        [[nodiscard]] auto value() const -> const T& {
            return value_->value;
        }

        auto operator=(const T& rhs) -> FieldValue& {
            std::unique_lock lock(value_->value_mutex);
            value() = rhs;
            chang();
            return *this;
        }

        auto operator=(const FieldValue& rhs) -> FieldValue& {
            std::unique_lock lock(value_->value_mutex);
            value() = rhs.value();
            chang();
            return *this;
        }

        auto ptr() -> FEP<T> {
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

        auto write_lock() -> std::shared_ptr<std::unique_lock<std::shared_mutex>> {
            return std::make_shared<std::unique_lock<std::shared_mutex>>(value_->value_mutex);
        }

        auto read_lock() -> std::shared_ptr<std::shared_lock<std::shared_mutex>> {
            return std::make_shared<std::shared_lock<std::shared_mutex>>(value_->value_mutex);
        }

        auto chang() -> void {
            value_->is_chang = true;
            Config::mark_dirty();
            std::shared_lock _(value_->event_mutex);
            value_->events(value_, Event::VALUE_CHANG);
        }

        template<typename Type>
        static auto form_entry(const CFEP<Type>& entry) -> FieldValue {
            FieldValue value;
            value.value_ = std::const_pointer_cast<FE<Type>>(entry);
            return value;
        }

        static auto form_entry(const FEBP& entry) -> FieldValue {
            return form_entry<T>(std::static_pointer_cast<FE<T>>(entry));
        }

        auto add_event(event::FastEvent<void, const FEBP&, const Event>::Func func) -> OPT<event::FastEvent<void, const FEBP&, const Event>::Handle> {
            return Config::add_event(value_, func);
        }

        auto remove_event(const event::FastEvent<void, const FEBP&, const Event>::Handle handle) -> void {
            return Config::remove_event(value_, handle);
        }

    protected:
        FEP<T> value_;

        auto value() -> T& {
            return value_->value;
        }
    };

    template<typename T>
    class Field : public FieldValue<T> {
    public:
        using FieldValue<T>::operator=;

        explicit Field(const STR& field_name) {
            this->value_ = Config::find_or_create<T>(field_name, typeid(T).name());
            init(field_name);
        }

        template<typename... Args>
        explicit Field(const STR& field_name, Args&&... args) {
            this->value_ = Config::find_or_create<T>(field_name, typeid(T).name(), std::forward<Args>(args)...);
            init(field_name);
        }

        ~Field() = default;

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

    template<typename C, typename T> concept HasInsert = requires(C c, T v) {
        c.insert(c.end(), v);
    };

    template<template<class...> class C, typename T, typename... Args> requires HasInsert<C<T, Args...>, T>
    struct Codec<C<T, Args...>> {
        static auto encode(const C<T, Args...>& c) -> nlohmann::json {
            nlohmann::json j = nlohmann::json::array();
            for (const auto& e : c) {
                j.push_back(Codec<T>::encode(e));
            }
            return j;
        }

        static auto decode(const nlohmann::json& j) -> C<T, Args...> {
            if (!j.is_array()) {
                throw std::runtime_error("Container decode failed: json is not array");
            }

            C<T, Args...> c;
            for (const auto& e : j) {
                c.insert(c.end(), Codec<T>::decode(e));
            }
            return c;
        }
    };

    template<typename E> requires std::is_enum_v<E>
    struct Codec<E> {
        static auto encode(E v) -> nlohmann::json {
            auto name = magic_enum::enum_name(v);
            if (name.empty()) {
                throw std::runtime_error("Enum encode failed: unknown value");
            }
            return std::string{name};
        }

        static auto decode(const nlohmann::json& j) -> E {
            if (!j.is_string()) {
                throw std::runtime_error("Enum decode failed: json is not string");
            }

            const auto str = j.get<std::string>();
            if (auto opt = magic_enum::enum_cast<E>(str)) {
                return *opt;
            }

            throw std::runtime_error("Enum decode failed: invalid enum name: " + str);
        }
    };


    template<typename T>
    auto operator<=>(const FieldValue<T>& lhs, const FieldValue<T>& rhs) requires requires(const T& a, const T& b) { a <=> b; } {
        return lhs.value() <=> rhs.value();
    }

    template<typename T>
    auto operator==(const FieldValue<T>& lhs, const FieldValue<T>& rhs) requires requires(const T& a, const T& b) { a == b; } {
        return lhs.value() == rhs.value();
    }

    template<typename T>
    auto operator<(const FieldValue<T>& lhs, const T& rhs) -> bool {
        return lhs.value() < rhs;
    }

    template<typename T>
    auto operator>(const FieldValue<T>& lhs, const T& rhs) -> bool {
        return rhs < lhs.value();
    }

    template<typename T>
    auto operator<=(const FieldValue<T>& lhs, const T& rhs) -> bool {
        return !(lhs > rhs);
    }

    template<typename T>
    auto operator>=(const FieldValue<T>& lhs, const T& rhs) -> bool {
        return !(lhs < rhs);
    }

    template<typename T>
    auto operator==(const FieldValue<T>& lhs, const T& rhs) -> bool {
        return lhs.value() == rhs;
    }

    template<typename T>
    auto operator<(const T& lhs, const FieldValue<T>& rhs) -> bool {
        return lhs < rhs.value();
    }

    template<typename T>
    auto operator>(const T& lhs, const FieldValue<T>& rhs) -> bool {
        return rhs.value() < lhs;
    }

    template<typename T>
    auto operator<=(const T& lhs, const FieldValue<T>& rhs) -> bool {
        return !(lhs > rhs);
    }

    template<typename T>
    auto operator>=(const T& lhs, const FieldValue<T>& rhs) -> bool {
        return !(lhs < rhs);
    }

    template<typename T>
    auto operator==(const T& lhs, const FieldValue<T>& rhs) -> bool {
        return lhs == rhs.value();
    }

    template<typename T>
    auto operator+(const FieldValue<T>& lhs, const FieldValue<T>& rhs) -> T {
        return lhs.value() + rhs.value();
    }

    template<typename T>
    auto operator-(const FieldValue<T>& lhs, const FieldValue<T>& rhs) -> T {
        return lhs.value() - rhs.value();
    }

    template<typename T>
    auto operator*(const FieldValue<T>& lhs, const FieldValue<T>& rhs) -> T {
        return lhs.value() * rhs.value();
    }

    template<typename T>
    auto operator/(const FieldValue<T>& lhs, const FieldValue<T>& rhs) -> T {
        return lhs.value() / rhs.value();
    }

    template<typename T>
    auto operator+(const FieldValue<T>& lhs, const T& rhs) -> T {
        return lhs.value() + rhs;
    }

    template<typename T>
    auto operator-(const FieldValue<T>& lhs, const T& rhs) -> T {
        return lhs.value() - rhs;
    }

    template<typename T>
    auto operator*(const FieldValue<T>& lhs, const T& rhs) -> T {
        return lhs.value() * rhs;
    }

    template<typename T>
    auto operator/(const FieldValue<T>& lhs, const T& rhs) -> T {
        return lhs.value() / rhs;
    }

    template<typename T>
    auto operator+(const T& lhs, const FieldValue<T>& rhs) -> T {
        return lhs + rhs.value();
    }

    template<typename T>
    auto operator-(const T& lhs, const FieldValue<T>& rhs) -> T {
        return lhs - rhs.value();
    }

    template<typename T>
    auto operator*(const T& lhs, const FieldValue<T>& rhs) -> T {
        return lhs * rhs.value();
    }

    template<typename T>
    auto operator/(const T& lhs, const FieldValue<T>& rhs) -> T {
        return lhs / rhs.value();
    }

    template<typename T>
    auto operator+=(FieldValue<T>& lhs, const T& rhs) -> FieldValue<T>& {
        lhs.value() += rhs;
        lhs.chang();
        return lhs;
    }

    template<typename T>
    auto operator-=(FieldValue<T>& lhs, const T& rhs) -> FieldValue<T>& {
        lhs.value() -= rhs;
        lhs.chang();
        return lhs;
    }

    template<typename T>
    auto operator*=(FieldValue<T>& lhs, const T& rhs) -> FieldValue<T>& {
        lhs.value() *= rhs;
        lhs.chang();
        return lhs;
    }

    template<typename T>
    auto operator/=(FieldValue<T>& lhs, const T& rhs) -> FieldValue<T>& {
        lhs.value() /= rhs;
        lhs.chang();
        return lhs;
    }

    template<typename T>
    auto operator+=(FieldValue<T>& lhs, const FieldValue<T>& rhs) -> FieldValue<T>& {
        lhs.value() += rhs.value();
        lhs.chang();
        return lhs;
    }

    template<typename T> concept Bitwiseable = std::is_integral_v<T> || std::is_enum_v<T>;

    template<Bitwiseable T>
    auto operator&(const FieldValue<T>& lhs, const FieldValue<T>& rhs) -> T {
        return lhs.value() & rhs.value();
    }

    template<Bitwiseable T>
    auto operator|(const FieldValue<T>& lhs, const FieldValue<T>& rhs) -> T {
        return lhs.value() | rhs.value();
    }

    template<Bitwiseable T>
    auto operator^(const FieldValue<T>& lhs, const FieldValue<T>& rhs) -> T {
        return lhs.value() ^ rhs.value();
    }

    template<Bitwiseable T>
    auto operator&(const FieldValue<T>& lhs, T rhs) -> T {
        return lhs.value() & rhs;
    }

    template<Bitwiseable T>
    auto operator|(const FieldValue<T>& lhs, T rhs) -> T {
        return lhs.value() | rhs;
    }

    template<Bitwiseable T>
    auto operator^(const FieldValue<T>& lhs, T rhs) -> T {
        return lhs.value() ^ rhs;
    }

    template<Bitwiseable T>
    auto operator&(T lhs, const FieldValue<T>& rhs) -> T {
        return lhs & rhs.value();
    }

    template<Bitwiseable T>
    auto operator|(T lhs, const FieldValue<T>& rhs) -> T {
        return lhs | rhs.value();
    }

    template<Bitwiseable T>
    auto operator^(T lhs, const FieldValue<T>& rhs) -> T {
        return lhs ^ rhs.value();
    }

    template<Bitwiseable T>
    auto operator<<(const FieldValue<T>& lhs, std::size_t shift) -> T {
        return lhs.value() << shift;
    }

    template<Bitwiseable T>
    auto operator>>(const FieldValue<T>& lhs, std::size_t shift) -> T {
        return lhs.value() >> shift;
    }

    template<Bitwiseable T>
    auto operator~(const FieldValue<T>& v) -> T {
        return ~v.value();
    }

    template<Bitwiseable T>
    auto operator&=(FieldValue<T>& lhs, T rhs) -> FieldValue<T>& {
        lhs.value() &= rhs;
        lhs.chang();
        return lhs;
    }

    template<Bitwiseable T>
    auto operator|=(FieldValue<T>& lhs, T rhs) -> FieldValue<T>& {
        lhs.value() |= rhs;
        lhs.chang();
        return lhs;
    }

    template<Bitwiseable T>
    auto operator^=(FieldValue<T>& lhs, T rhs) -> FieldValue<T>& {
        lhs.value() ^= rhs;
        lhs.chang();
        return lhs;
    }

    template<Bitwiseable T>
    auto operator&=(FieldValue<T>& lhs, const FieldValue<T>& rhs) -> FieldValue<T>& {
        lhs.value() &= rhs.value();
        lhs.chang();
        return lhs;
    }

    template<Bitwiseable T>
    auto operator|=(FieldValue<T>& lhs, const FieldValue<T>& rhs) -> FieldValue<T>& {
        lhs.value() |= rhs.value();
        lhs.chang();
        return lhs;
    }

    template<Bitwiseable T>
    auto operator^=(FieldValue<T>& lhs, const FieldValue<T>& rhs) -> FieldValue<T>& {
        lhs.value() ^= rhs.value();
        lhs.chang();
        return lhs;
    }
}
