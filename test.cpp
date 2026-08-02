// 2026-08-02 17:19:16

#include <array>
#include <atomic>
#include <bitset>
#include <chrono>
#include <complex>
#include <cstdlib>
#include <deque>
#include <filesystem>
#include <fstream>
#include <future>
#include <iostream>
#include <list>
#include <map>
#include <memory>
#include <optional>
#include <queue>
#include <set>
#include <stack>
#include <string>
#include <system_error>
#include <tuple>
#include <type_traits>
#include <utility>
#include <variant>
#include <vector>

#include "stdpp/config.hpp"

using stdpp::config::Config;
using stdpp::config::Event;
using stdpp::config::FEBP;
using stdpp::config::Field;
using STR = std::string; // config.hpp 已将 STR 移入 detail，测试本地定义

namespace {
    int g_failures = 0;
    int g_checks = 0;

    auto check(const bool ok, const std::string& name) -> void {
        ++g_checks;
        std::cout << (ok ? "[PASS] " : "[FAIL] ") << name << std::endl;
        if (!ok) {
            ++g_failures;
        }
    }

    auto write_toml(const std::filesystem::path& path, const std::string& content) -> bool {
        std::ofstream ofs(path);
        if (!ofs) {
            return false;
        }
        ofs << content;
        return true;
    }

    // 测试 1（S2 核心）：复合赋值触发事件 + 落盘
    auto test1_compound_assign(const std::filesystem::path& path) -> bool {
        if (!write_toml(path, "num = 10\n")) {
            check(false, "t1: 写入测试文件");
            return false;
        }

        Field<int> num("num");
        auto change_count = std::make_shared<std::atomic<int>>(0);
        num.add_event([change_count](const FEBP&, const Event e) {
            if (e == Event::VALUE_CHANGE) {
                ++*change_count;
            }
        });

        check(Config::instance().load(path), "t1: load 初始文件");
        check(num.copy() == 10, "t1: 初始值 num == 10");

        // 每个运算符独立验证：设基准值 -> save -> load 确认基准 -> 应用运算符 -> save
        // -> load 验证结果
        const auto run_op = [&](const char* op_name, const auto& apply, const int base, const int expected) -> bool {
            num = base;
            Config::instance().save();
            Config::instance().load(path);
            bool ok = true;
            if (num.copy() != base) {
                check(false, std::string("t1: ") + op_name + " 基准值设置失败");
                ok = false;
            }
            change_count->store(0);
            apply();
            const bool event_fired = change_count->load() > 0;
            Config::instance().save();
            Config::instance().load(path);
            const int value_after = num.copy();
            check(event_fired, std::string("t1: ") + op_name + " 触发 VALUE_CHANGE");
            check(value_after == expected, std::string("t1: ") + op_name + " 落盘重载后值正确 (期望 " + std::to_string(expected) + ", 实际 " + std::to_string(value_after) + ")");
            return ok && event_fired && value_after == expected;
        };

        bool ok = true;
        ok &= run_op("+=",
                     [&] {
                         num += 5;
                     },
                     7,
                     12);
        ok &= run_op("-=",
                     [&] {
                         num -= 2;
                     },
                     7,
                     5);
        ok &= run_op("*=",
                     [&] {
                         num *= 2;
                     },
                     7,
                     14);
        ok &= run_op("<<=",
                     [&] {
                         num <<= 1;
                     },
                     7,
                     14);
        ok &= run_op(">>=",
                     [&] {
                         num >>= 1;
                     },
                     7,
                     3);
        return ok;
    }

    enum class TestColor { Red, Green, Blue };

    // 测试 2（S3）：解码失败不抛异常、保留默认值、load 返回 true
    auto test2_decode_failure(const std::filesystem::path& path) -> bool {
        if (!write_toml(path,
                        "bad_vec = \"not an array\"\n" // vector 特化解码抛 runtime_error
                        "bad_num = [1, 2, 3]\n" // int 解码抛 toml::type_error
                        "bad_color = \"Purple\"\n")) { // 枚举特化解码抛 runtime_error
            check(false, "t2: 写入测试文件");
            return false;
        }

        const Field<std::vector<int>> bad_vec("bad_vec");
        const Field<int> bad_num("bad_num");
        const Field<TestColor> bad_color("bad_color");

        bool threw = false;
        bool loaded = false;
        try {
            loaded = Config::instance().load(path);
        } catch (...) {
            threw = true;
        }
        check(!threw, "t2: 类型不匹配时 load 不抛异常");
        check(loaded, "t2: load 返回 true");
        check(bad_vec.copy().empty(), "t2: vector 保留默认值(空)");
        check(bad_num.copy() == 0, "t2: int 保留默认值(0)");
        check(bad_color.copy() == TestColor::Red, "t2: 枚举保留默认值(Red)");

        // load 的 bool 契约：文件本身非法时返回 false 而非抛异常
        write_toml(path, "not valid toml {{{");
        bool threw_invalid = false;
        bool loaded_invalid = true;
        try {
            loaded_invalid = Config::instance().load(path);
        } catch (...) {
            threw_invalid = true;
        }
        check(!threw_invalid && !loaded_invalid, "t2: 非法 TOML 文件 load 返回 false 不抛异常");
        return !threw && loaded && bad_vec.copy().empty() && bad_num.copy() == 0 && bad_color.copy() == TestColor::Red && !threw_invalid && !loaded_invalid;
    }

    // 测试 3（S1）：并发 load/save 压测（2 写 + 2 读）
    auto test3_concurrent(const std::filesystem::path& path) -> bool {
        if (!write_toml(path, "c_num = 1\nc_str = \"a\"\nc_vec = [1, 2, 3]\n")) {
            check(false, "t3: 写入测试文件");
            return false;
        }

        Field<int> c_num("c_num");
        Field<STR> c_str("c_str");
        Field<std::vector<int>> c_vec("c_vec");
        check(Config::instance().load(path), "t3: load 初始文件");

        constexpr int iteration_count = 300;

        const auto writer = [&]() -> void {
            for (int i = 0; i < iteration_count; ++i) {
                c_num = i;
                c_str = std::to_string(i);
                c_vec = std::vector{i, i + 1};
                Config::instance().save();
            }
        };

        const auto reader = [&]() -> void {
            for (int i = 0; i < iteration_count; ++i) {
                Config::instance().load(path);
            }
        };

        std::vector<std::future<void>> futures;
        futures.push_back(std::async(std::launch::async, writer));
        futures.push_back(std::async(std::launch::async, writer));
        futures.push_back(std::async(std::launch::async, reader));
        futures.push_back(std::async(std::launch::async, reader));

        bool completed = true;
        for (auto& fut : futures) {
            if (fut.wait_for(std::chrono::seconds(30)) != std::future_status::ready) {
                completed = false;
            }
        }
        check(completed, "t3: 4 线程各 300 轮并发 load/save 无死锁(30s 超时)");
        if (!completed) {
            std::cout << "[FAIL] t3: 检测到死锁/超时，终止进程" << std::endl;
            std::exit(EXIT_FAILURE);
        }

        bool thread_exception = false;
        for (auto& fut : futures) {
            try {
                fut.get();
            } catch (const std::exception& e) {
                thread_exception = true;
                std::cout << "      t3 线程异常: " << e.what() << std::endl;
            } catch (...) {
                thread_exception = true;
            }
        }
        check(!thread_exception, "t3: 线程无异常");

        // 压测后配置仍可用
        const bool reload_ok = Config::instance().load(path);
        check(reload_ok, "t3: 压测后 load 正常");
        c_num = 12345;
        const bool save_ok = Config::instance().save();
        check(save_ok, "t3: 压测后 save 正常");
        return completed && !thread_exception && reload_ok && save_ok;
    }

    // 测试 4（回归）：常规读写路径
    auto test4_regression(const std::filesystem::path& path) -> bool {
        if (!write_toml(path, "r_num = 42\n" "r_str = \"hello\"\n" "r_vec = [1, 2, 3]\n" "r_map = [[\"a\", 1], [\"b\", 2]]\n" "[server]\n" "port = 8080\n")) {
            check(false, "t4: 写入测试文件");
            return false;
        }

        Field<int> r_num("r_num");
        const Field<STR> r_str("r_str");
        const Field<std::vector<int>> r_vec("r_vec");
        const Field<std::map<std::string, int>> r_map("r_map");
        const Field<int> r_port("server::port");

        check(Config::instance().load(path), "t4: load");
        check(r_num.copy() == 42, "t4: int 往返一致");
        check(r_str.copy() == "hello", "t4: string 往返一致");
        check(r_vec.copy() == std::vector({1, 2, 3}), "t4: vector 往返一致");
        check(r_map.copy() == std::map<std::string, int>({{"a", 1}, {"b", 2}}), "t4: map 往返一致");
        check(r_port.copy() == 8080, "t4: 嵌套 table 字段往返一致");

        auto change_count = std::make_shared<std::atomic<int>>(0);
        r_num.add_event([change_count](const FEBP&, const Event e) {
            if (e == Event::VALUE_CHANGE) {
                ++*change_count;
            }
        });
        r_num = 100;
        check(change_count->load() == 1, "t4: operator= 触发一次 VALUE_CHANGE");
        check(Config::instance().save(), "t4: save");
        check(Config::instance().load(path), "t4: 重载");
        check(r_num.copy() == 100, "t4: 赋值后落盘重载一致");
        check(r_str.copy() == "hello", "t4: 未修改字段保持不变");
        return change_count->load() == 1 && r_num.copy() == 100 && r_str.copy() == "hello";
    }

    // 测试 5（轮 2 回归）：M2 亚秒时间 / M3 中文路径 / L7 非法日期 / R3 二次保存
    auto to_utf8(const char8_t* s) -> std::string {
        return std::string(reinterpret_cast<const char*>(s));
    }

    auto test5_round2_regression(const std::filesystem::path& path) -> bool {
        using SysMs = std::chrono::sys_time<std::chrono::milliseconds>;
        constexpr auto base_ts = std::chrono::sys_days{std::chrono::year{2026} / 1 / 25} + std::chrono::hours{13} + std::chrono::minutes{45} + std::chrono::seconds{30} + std::chrono::milliseconds{123};

        const std::string content = to_utf8(u8"t5_time = \"2026-01-25T13:45:30.123Z\"\n") + to_utf8(u8"t5_path = \"C:\\\\测试\\\\配置.toml\"\n") + "t5_ymd = { year = 2026, month = 13, day = 1 }\n" + "t5_num = 1\n";
        if (!write_toml(path, content)) {
            check(false, "t5: 写入测试文件");
            return false;
        }

        Field<SysMs> t5_time("t5_time");
        const Field<std::filesystem::path> t5_path("t5_path");
        const Field<std::chrono::year_month_day> t5_ymd("t5_ymd");
        Field<int> t5_num("t5_num");

        bool ok = true;
        try {
            ok = Config::instance().load(path);
        } catch (...) {
            ok = false;
        }
        check(ok, "t5: load 不抛异常且返回 true");
        check(t5_time.copy() == base_ts, "t5: sys_time 毫秒精度往返");
        check(t5_path.copy() == std::filesystem::path(std::u8string(u8"C:\\测试\\配置.toml")), "t5: 中文路径往返");
        check(t5_ymd.copy() == std::chrono::year_month_day{}, "t5: 非法日期保留默认值(S3 兜底)");

        // R3 回归：连续两次 save 均生效，无丢失更新
        t5_num = 2;
        Config::instance().save();
        t5_num = 3;
        Config::instance().save();
        Config::instance().load(path);
        check(t5_num.copy() == 3, "t5: 连续 save 无丢失更新");

        // M2：修改亚秒值后再次往返
        constexpr auto shifted = base_ts + std::chrono::milliseconds{7};
        t5_time = shifted;
        Config::instance().save();
        Config::instance().load(path);
        check(t5_time.copy() == shifted, "t5: 亚秒修改后落盘重载一致");

        return ok && t5_time.copy() == shifted && t5_num.copy() == 3;
    }

    // 测试 6（R3 窗口 + M1）：save 与并发注册新字段交错，动态字段必须全部落盘
    auto test6_concurrent_register(const std::filesystem::path& path) -> bool {
        if (!write_toml(path, "t6_base = 0\n")) {
            check(false, "t6: 写入测试文件");
            return false;
        }

        Field<int> t6_base("t6_base");
        check(Config::instance().load(path), "t6: load 初始文件");

        constexpr int dyn_count = 200;

        const auto writer = [&]() -> void {
            for (int i = 0; i < dyn_count; ++i) {
                t6_base = i;
                Config::instance().save();
            }
        };

        const auto registrar = [&]() -> void {
            for (int i = 0; i < dyn_count; ++i) {
                Field<int> dyn("dyn_" + std::to_string(i));
                dyn = i * 2;
            }
        };

        std::vector<std::future<void>> futures;
        futures.push_back(std::async(std::launch::async, writer));
        futures.push_back(std::async(std::launch::async, registrar));

        bool completed = true;
        for (auto& fut : futures) {
            if (fut.wait_for(std::chrono::seconds(30)) != std::future_status::ready) {
                completed = false;
            }
        }
        check(completed, "t6: save 与并发注册无死锁(30s 超时)");
        if (!completed) {
            std::cout << "[FAIL] t6: 检测到死锁/超时，终止进程" << std::endl;
            std::exit(EXIT_FAILURE);
        }
        for (auto& fut : futures) {
            try {
                fut.get();
            } catch (...) {
                completed = false;
            }
        }
        check(completed, "t6: 线程无异常");

        // 重新加载后：所有动态字段必须落盘（修复前 save 末尾覆盖
        // mark_dirty，动态字段永久丢失）
        const bool reload_ok = Config::instance().load(path);
        check(reload_ok, "t6: 重载成功");
        bool all_dyn_ok = reload_ok;
        for (int i = 0; i < dyn_count; ++i) {
            Field<int> dyn("dyn_" + std::to_string(i)); // 复用已有 entry
            if (dyn.copy() != i * 2) {
                all_dyn_ok = false;
            }
        }
        check(all_dyn_ok, "t6: 200 个动态字段全部落盘");
        return completed && reload_ok && all_dyn_ok;
    }

    // 测试 7（事件）：VALUE_LOAD / 多订阅者 / 退订
    auto test7_events(const std::filesystem::path& path) -> bool {
        if (!write_toml(path, "ev_num = 1\n")) {
            check(false, "t7: 写入测试文件");
            return false;
        }

        Field<int> ev_num("ev_num");
        auto load_count = std::make_shared<std::atomic<int>>(0);
        auto change_count_a = std::make_shared<std::atomic<int>>(0);
        auto change_count_b = std::make_shared<std::atomic<int>>(0);

        ev_num.add_event([load_count](const FEBP&, const Event e) {
            if (e == Event::VALUE_LOAD) {
                ++*load_count;
            }
        });
        ev_num.add_event([change_count_a](const FEBP&, const Event e) {
            if (e == Event::VALUE_CHANGE) {
                ++*change_count_a;
            }
        });
        const auto handle_b = ev_num.add_event([change_count_b](const FEBP&, const Event e) {
            if (e == Event::VALUE_CHANGE) {
                ++*change_count_b;
            }
        });

        check(Config::instance().load(path), "t7: load");
        check(load_count->load() == 1, "t7: VALUE_LOAD 触发一次");

        ev_num = 42;
        check(change_count_a->load() == 1 && change_count_b->load() == 1, "t7: 两个订阅者都收到 VALUE_CHANGE");

        ev_num.remove_event(*handle_b);
        ev_num = 43;
        check(change_count_a->load() == 2 && change_count_b->load() == 1, "t7: 退订后不再收到事件");
        return load_count->load() == 1 && change_count_a->load() == 2 && change_count_b->load() == 1;
    }

    // 测试 8（Codec 覆盖）：各类型 save/load 往返一致
    auto test8_codec_roundtrip(const std::filesystem::path& path) -> bool {
        if (!write_toml(path, "t8_placeholder = 0\n")) {
            check(false, "t8: 写入测试文件");
            return false;
        }

        Field<TestColor> f_enum("t8_enum");
        Field<bool> f_bool("t8_bool");
        Field<double> f_double("t8_double");
        Field<float> f_float("t8_float");
        Field<char> f_char("t8_char");
        Field<std::optional<int>> f_opt_val("t8_opt_val");
        Field<std::optional<int>> f_opt_empty("t8_opt_empty");
        Field<std::expected<int, std::string>> f_exp_val("t8_exp_val");
        Field<std::expected<int, std::string>> f_exp_err("t8_exp_err");
        Field<std::variant<int, std::string>> f_var_i("t8_var_i");
        Field<std::variant<int, std::string>> f_var_s("t8_var_s");
        Field<std::pair<int, std::string>> f_pair("t8_pair");
        Field<std::tuple<int, std::string, double>> f_tuple("t8_tuple");
        Field<std::array<int, 3>> f_array("t8_array");
        Field<std::bitset<8>> f_bitset("t8_bitset");
        Field<std::complex<double>> f_complex("t8_complex");
        Field<std::chrono::milliseconds> f_dur("t8_dur");
        Field<std::chrono::year_month_day> f_ymd("t8_ymd");
        Field<std::deque<int>> f_deque("t8_deque");
        Field<std::list<int>> f_list("t8_list");
        Field<std::set<int>> f_set("t8_set");
        Field<std::stack<int>> f_stack("t8_stack");
        Field<std::queue<int>> f_queue("t8_queue");
        Field<std::shared_ptr<int>> f_sptr("t8_sptr");
        Field<std::unique_ptr<int>> f_uptr("t8_uptr");
        Field<int> f_nested("a::b::c");

        f_enum = TestColor::Blue;
        f_bool = true;
        f_double = 1.5;
        f_float = 2.25f;
        f_char = 'A';
        f_opt_val = std::optional{7};
        f_opt_empty = std::optional<int>{};
        f_exp_val = std::expected<int, std::string>{9};
        f_exp_err = std::expected<int, std::string>{std::unexpected(std::string("boom"))};
        f_var_i = 1;
        f_var_s = std::string("hi");
        f_pair = std::pair<int, std::string>{2, "b"};
        f_tuple = std::tuple<int, std::string, double>{3, "c", 0.5};
        f_array = std::array{4, 5, 6};
        f_bitset = std::bitset<8>(0b10101010);
        f_complex = std::complex{1.0, -2.0};
        f_dur = std::chrono::milliseconds{1500};
        f_ymd = std::chrono::year{2024} / 2 / 29;
        f_deque = std::deque{1, 2, 3};
        f_list = std::list{4, 5, 6};
        f_set = std::set{7, 8, 9};
        {
            std::stack<int> s;
            s.push(1);
            s.push(2);
            s.push(3);
            f_stack = std::move(s);
        }
        {
            std::queue<int> q;
            q.push(1);
            q.push(2);
            q.push(3);
            f_queue = std::move(q);
        }
        f_sptr = std::make_shared<int>(11);
        f_uptr = std::make_unique<int>(12);
        f_nested = 13;

        check(Config::instance().save(), "t8: save");
        check(Config::instance().load(path), "t8: load");

        check(f_enum.copy() == TestColor::Blue, "t8: enum 往返");
        check(f_bool.copy() == true, "t8: bool 往返");
        check(f_double.copy() == 1.5, "t8: double 往返");
        check(f_float.copy() == 2.25f, "t8: float 往返");
        check(f_char.copy() == 'A', "t8: char 往返");
        check(f_opt_val.copy().has_value() && *f_opt_val.copy() == 7, "t8: optional 有值往返");
        check(!f_opt_empty.copy().has_value(), "t8: optional 空往返");
        check(f_exp_val.copy().has_value() && *f_exp_val.copy() == 9, "t8: expected 有值往返");
        check(!f_exp_err.copy().has_value() && f_exp_err.copy().error() == "boom", "t8: expected 错误往返");
        check(std::holds_alternative<int>(f_var_i.copy()) && std::get<int>(f_var_i.copy()) == 1, "t8: variant int 往返");
        check(std::holds_alternative<std::string>(f_var_s.copy()) && std::get<std::string>(f_var_s.copy()) == "hi", "t8: variant string 往返");
        check(f_pair.copy() == std::pair<int, std::string>{2, "b"}, "t8: pair 往返");
        check(f_tuple.copy() == std::tuple<int, std::string, double>{3, "c", 0.5}, "t8: tuple 往返");
        check(f_array.copy() == std::array{4, 5, 6}, "t8: array 往返");
        check(f_bitset.copy() == std::bitset<8>(0b10101010), "t8: bitset 往返");
        check(f_complex.copy() == std::complex{1.0, -2.0}, "t8: complex 往返");
        check(f_dur.copy() == std::chrono::milliseconds{1500}, "t8: duration 往返");
        check(f_ymd.copy() == std::chrono::year{2024} / 2 / 29, "t8: year_month_day 往返");
        check(f_deque.copy() == std::deque({1, 2, 3}), "t8: deque 往返");
        check(f_list.copy() == std::list({4, 5, 6}), "t8: list 往返");
        check(f_set.copy() == std::set({7, 8, 9}), "t8: set 往返");
        check(f_stack.copy().top() == 3 && f_stack.copy().size() == 3, "t8: stack 往返(top=3)");
        check(f_queue.copy().front() == 1 && f_queue.copy().size() == 3, "t8: queue 往返(front=1)");
        check(f_sptr.copy() && *f_sptr.copy() == 11, "t8: shared_ptr 往返");
        {
            auto lock = f_uptr.value_lock(); // unique_ptr 不可拷贝，经引用通道读取
            check(*lock && **lock == 12, "t8: unique_ptr 往返");
        }
        check(f_nested.copy() == 13, "t8: 嵌套 a::b::c 往返");
        return true;
    }

    // 测试 9（边界与错误）：save 失败重试 / 空文件 / 字段缺失 / refresh / create /
    // 尾空段
    auto test9_edge_cases(const std::filesystem::path& path, const std::filesystem::path& bad_path) -> bool {
        constexpr bool ok = true;

        // 1. save 失败保留 dirty：path 指向不存在的目录
        Field<int> e_num("e_num");
        e_num = 77;
        const bool bad_load = Config::instance().load(bad_path); // 文件不存在 → false，但 path 已设置
        const bool bad_save = Config::instance().save(); // 目录不存在 → 写文件失败
        check(!bad_load && !bad_save, "t9: 无效路径 load/save 返回 false 不抛异常");

        // 2. 切换到合法路径后重试：dirty 保留 → 值落盘
        if (!write_toml(path, "")) {
            check(false, "t9: 写入测试文件");
            return false;
        }
        const bool good_load = Config::instance().load(path); // 空 TOML 文件 → true
        const bool good_save = Config::instance().save();
        check(good_load && good_save, "t9: 合法路径 load/save 成功");
        Config::instance().load(path);
        check(e_num.copy() == 77, "t9: save 失败重试后值落盘");

        // 3. 字段缺失：TOML 中没有该字段 → 保留默认值
        const Field<int> e_missing("e_missing");
        check(e_missing.copy() == 0, "t9: 字段缺失保留默认值");

        // 4. refresh()：文件改动后重载
        write_toml(path, "e_ref = 5\n");
        const Field<int> e_ref("e_ref");
        Config::instance().load(path);
        check(e_ref.copy() == 5, "t9: refresh 前值");
        write_toml(path, "e_ref = 6\n");
        check(Config::instance().refresh(), "t9: refresh 成功");
        check(e_ref.copy() == 6, "t9: refresh 后值更新");

        // 5. create() 重绑定到新名字
        Field<int> e_cre("e_cre");
        e_cre.create("e_cre2");
        e_cre = 88;
        check(Config::instance().save(), "t9: create 后 save");
        Config::instance().load(path);
        check(e_cre.copy() == 88, "t9: create 重绑定后读写生效");

        // 6. split_path 尾空段（L6）
        Field<int> e_trail("trail::");
        e_trail = 99;
        check(Config::instance().save(), "t9: 尾空段 save");
        Config::instance().load(path);
        check(e_trail.copy() == 99, "t9: 尾双冒号字段不崩溃且往返一致");
        return e_num.copy() == 77 && e_ref.copy() == 6 && e_cre.copy() == 88 && e_trail.copy() == 99;
    }

    // 测试 10（P1 读守卫）：read_lock 只读通道
    auto test10_read_lock() -> bool {
        Field<int> rl_num("rl_num");
        Field<STR> rl_str("rl_str");
        rl_num = 42;
        rl_str = "hello";

        // 1. 读值与当前值一致；*guard 是 const T&（编译期 const 视图，无法经其写值）
        {
            const auto guard = rl_num.read_lock();
            static_assert(std::is_same_v<std::remove_reference_t<decltype(*guard)>, const int>, "read_lock 必须是 const 视图");
            check(*guard == 42, "t10: read_lock int 读取一致");
        }
        {
            const auto guard = rl_str.read_lock();
            check(*guard == "hello", "t10: read_lock string 读取一致");
        }

        // 2. unique_ptr 经 read_lock 可读（不可拷贝类型，免拷贝引用通道）
        Field<std::unique_ptr<int>> rl_uptr("rl_uptr");
        rl_uptr = std::make_unique<int>(7);
        {
            const auto guard = rl_uptr.read_lock();
            check(*guard && **guard == 7, "t10: read_lock unique_ptr 可读");
        }

        // 3. 读守卫不触发事件、不置 dirty
        auto change_count = std::make_shared<std::atomic<int>>(0);
        rl_num.add_event([change_count](const FEBP&, const Event e) {
            if (e == Event::VALUE_CHANGE) {
                ++*change_count;
            }
        });
        {
            const auto guard = rl_num.read_lock();
            (void)*guard;
        }
        check(change_count->load() == 0, "t10: read_lock 不触发事件");

        // 4. 并发：读线程 read_lock vs 写线程 value_lock
        constexpr int iter = 200;
        const auto reader = [&]() -> void {
            for (int i = 0; i < iter; ++i) {
                const auto guard = rl_num.read_lock();
                (void)*guard;
            }
        };
        const auto writer = [&]() -> void {
            for (int i = 0; i < iter; ++i) {
                auto lock = rl_num.value_lock();
                *lock = i;
            }
        };
        std::vector<std::future<void>> futures;
        futures.push_back(std::async(std::launch::async, reader));
        futures.push_back(std::async(std::launch::async, writer));
        bool completed = true;
        for (auto& fut : futures) {
            if (fut.wait_for(std::chrono::seconds(30)) != std::future_status::ready) {
                completed = false;
            }
        }
        check(completed, "t10: 读写并发无死锁(30s 超时)");
        for (auto& fut : futures) {
            try {
                fut.get();
            } catch (...) {
                completed = false;
            }
        }
        check(completed, "t10: 读写并发无异常");
        return completed;
    }
} // namespace

auto main() -> int {
    const auto dir = std::filesystem::temp_directory_path();
    const std::filesystem::path t1 = dir / "stdpp_cfg_t1.toml";
    const std::filesystem::path t2 = dir / "stdpp_cfg_t2.toml";
    const std::filesystem::path t3 = dir / "stdpp_cfg_t3.toml";
    const std::filesystem::path t4 = dir / "stdpp_cfg_t4.toml";
    const std::filesystem::path t5 = dir / "stdpp_cfg_t5.toml";
    const std::filesystem::path t6 = dir / "stdpp_cfg_t6.toml";
    const std::filesystem::path t7 = dir / "stdpp_cfg_t7.toml";
    const std::filesystem::path t8 = dir / "stdpp_cfg_t8.toml";
    const std::filesystem::path t9 = dir / "stdpp_cfg_t9.toml";
    const std::filesystem::path bad = dir / "stdpp_cfg_no_dir" / "x.toml"; // 目录不存在

    std::cout << "=== stdpp::config S1-S3 + M/L 修复验证 ===" << std::endl;

    bool ok = true;
    ok &= test1_compound_assign(t1);
    ok &= test2_decode_failure(t2);
    ok &= test3_concurrent(t3);
    ok &= test4_regression(t4);
    ok &= test5_round2_regression(t5);
    ok &= test6_concurrent_register(t6);
    ok &= test7_events(t7);
    ok &= test8_codec_roundtrip(t8);
    ok &= test9_edge_cases(t9, bad);
    ok &= test10_read_lock();

    for (const auto& file : {t1, t2, t3, t4, t5, t6, t7, t8, t9}) {
        std::error_code ec;
        std::filesystem::remove(file, ec);
    }

    if (g_failures == 0) {
        std::cout << "ALL PASS (" << g_checks << " checks)" << std::endl;
        system("pause");
        return 0;
    }
    std::cout << "FAILED: " << g_failures << " / " << g_checks << std::endl;
    system("pause");
    return 1;
}
