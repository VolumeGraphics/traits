#include <algorithm>
#include <any>
#include <array>
#include <cassert>
#include <cstdint>
#include <format>
#include <functional>
#include <iostream>
#include <numbers>
#include <numeric>
#include <ranges>
#include <source_location>
#include <stdexcept>
#include <tuple>
#include <type_traits>
#include <typeinfo>
#include <utility>

#include "traits.h"
using namespace traits;

#ifdef __clang__
#pragma clang diagnostic push
#pragma clang diagnostic ignored "-Wunused-const-variable"
// #pragma clang diagnostic ignored "-Wunneeded-internal-declaration"
#endif

TRAITS_METHOD_DECLARATION(alloc);
TRAITS_METHOD_DECLARATION(author);
TRAITS_METHOD_DECLARATION(bar);
TRAITS_METHOD_DECLARATION(check);
TRAITS_METHOD_DECLARATION(cleanup);
TRAITS_METHOD_DECLARATION(draw);
TRAITS_METHOD_DECLARATION(free);
TRAITS_METHOD_DECLARATION(init);
TRAITS_METHOD_DECLARATION(isBreaking);
TRAITS_METHOD_DECLARATION(isRunning);
TRAITS_METHOD_DECLARATION(name);
TRAITS_METHOD_DECLARATION(print);
TRAITS_METHOD_DECLARATION(run);
TRAITS_METHOD_DECLARATION(runTests);
TRAITS_METHOD_DECLARATION(setBreaking);
TRAITS_METHOD_DECLARATION(start);
TRAITS_METHOD_DECLARATION(stop);
TRAITS_METHOD_DECLARATION(type);

// just some little helpers for demonstration purposes

void printSourceLocation(std::source_location location = std::source_location::current()) {
    std::cout << std::format("{:4}: {}\n", location.line(), location.function_name());
}

struct test {
    test(auto callable) { tests.emplace_back(callable); }

    static void run_all() {
        for (auto const& test : tests) test();
    }

   private:
    static inline std::vector<std::function<void()>> tests;
};

template <typename T, std::size_t N>
constexpr auto iota_array() {
    return []<std::size_t... Is>(std::index_sequence<Is...>) {
        return std::array<T, N>{Is...};
    }(std::make_index_sequence<N>());
}

namespace trait_examples {

// traits - define shared behavior in a non-intrusive way while preserving value semantics

// motivating example

constexpr auto Drawable = trait{Method<"draw", void(std::ostream& stream) const>};

struct Circle {
    void draw(std::ostream& stream) const {
        stream << std::format("Circle {{ radius = {} }}\n", radius);
    }
    double radius{0.0};
};

struct Square {
    void draw(std::ostream& stream) const {
        stream << std::format("Square {{ length = {} }}\n", length);
    }
    double length{0.0};
};

const auto MotivationalExample = test([] {
    std::vector<some<Drawable>> someDrawables;

    someDrawables.emplace_back(Circle{1.0});
    someDrawables.emplace_back(Square{2.0});

    for (auto const& drawable : someDrawables) drawable.draw(std::cout);
});

// traits step by step

// traits allow you to define shared behavior with a declarative syntax

constexpr auto WithAuthor = trait{
    Method<"author", std::string() const>  // needs previous declaration of 'author'
};

// you need to declare the method name with this macro (in the global namespace)

// TRAITS_METHOD_DECLARATION(author);

// alternative trait syntax

constexpr auto WithSummary = trait{
    TRAITS_METHOD(summary, std::string() const)  // no previous declaration of 'summary' necessary
};

// traits can be used to constrain generic types (static polymorphism)

#if 0

    // instead of ...

    decltype (auto) operator<< (std::ostream& stream, auto const& drawable)
    requires requires { { drawable.draw(stream) } -> std::same_as<void>; }
    {
        drawable.draw (stream);
        return stream;
    }

    // ... or ...

    template <typename T>
    concept Drawable = requires (T drawable, std::ostream& stream) { { drawable.draw(stream) } -> std::same_as<void>; };

    decltype (auto) operator<< (std::ostream& stream, Drawable auto const& drawable)
    {
        drawable.draw (stream);
        return stream;
    }

#endif

// ... you can use a trait

auto operator<<(std::ostream& stream, is<Drawable> auto const& drawable) -> decltype(auto) {
    drawable.draw(stream);
    return stream;
}

// NOTE: where is<'trait'> is a C++ concept

auto StreamTest = test([] { std::cout << Circle{3.0}; });

// traits can have multiple behaviors

constexpr auto Runnable = trait{
    Method<"start", void()>,
    Method<"stop", void()>,

    Method<"isRunning", bool() const>,
};

void run(is<Runnable> auto& runnable) {
    if (not runnable.isRunning()) {
        runnable.start();

        // ...

        runnable.stop();
    }
}

// traits support overloaded methods ...

constexpr auto OverloadedConstness = trait{
    Method<"bar", void() const>,
    Method<"bar", void()>,
};

constexpr auto OverloadedArgumentType = trait{
    Method<"bar", void(float value)>,
    Method<"bar", void(double value)>,
};

constexpr auto OverloadedValueCategory = trait{
    Method<"bar", void(char const& lvalue)>, Method<"bar", void(char& lvalue)>,
    Method<"bar", void(char&& lvalue)>,      Method<"bar", void(int const& lvalue)>,
    Method<"bar", void(int& lvalue)>,        Method<"bar", void(int&& rvalue)>,
};

constexpr auto OverloadedArity = trait{
    Method<"bar", void(bool value)>,
    Method<"bar", void()>,
};

// ... and (certain) overloaded operators

constexpr auto Callback = trait{
    Method<"operator()", void()>,
};

void myAlgorithm(is<Callback> auto& eventProcessing) {
    // ...

    eventProcessing();

    // ...

    eventProcessing();

    //...
}

// a trait can be templated

template <typename T>
constexpr auto ValidatorFor = trait{
    Method<"check", bool(T const&) const>,
};

constexpr auto IntValidator = ValidatorFor<int>;

// traits can be combined with + (this syntax is borrowed from Rust)

void print(std::ostream& out, is<WithAuthor + WithSummary> auto const& article) {
    out << std::format("{} by {}\n", article.summary(), article.author());
}

// ... or maybe better with boolean syntax?

constexpr auto WithAuthorAndSummary = WithAuthor and WithSummary;  // declare trait for later reuse

// traits support an optional type constraint (first parameter)

// a constraint is a templated callable: <typename> () -> bool

constexpr auto DefaultConstructible = []<typename T>() {
    return std::is_default_constructible_v<T>;
};

// constraints can be used to check arbitrary type properties

constexpr auto Empty = []<typename T>() { return std::is_empty_v<T>; };

constexpr auto StatelessAllocator = trait{Empty and DefaultConstructible,

                                          Method<"alloc", void*(std::size_t byteCount) const>,
                                          Method<"free", void(void* ptr) const>};

// C++ concepts make it hard to define derived concepts

#if 0

    struct Any final
    {
        Any (auto&& value); // OOPS ... clashes with copy/move constructor

        // let's define a constructor which takes anything but ourselves

        // this syntax is currently not allowed
        Any (not std::same_as<Any> auto&& value);

        // this syntax is somewhat awkward
        Any (auto&& value) requires (not std::same_as<std::remove_cvref_t<decltype(value)>, Any>);

        // this syntax requires explicit definition of (yet) another concept, see below
        Any (not_same_as<Any> auto&& value);
    };

    template <typename T, typename U>
    concept not_same_as = not std::same_as<T, U>;

#endif

// with a constraint ...

template <typename U>
constexpr auto SameAs = []<typename T>() { return std::same_as<T, U>; };

// ... you can define derived constraints as required

struct Any {
    Any(is<not SameAs<Any>> auto&& value);
};

// NOTE: is<constraint> is equivalent to is<trait{constraint}>

// a constraint can be used to force strong(er) coupling

template <typename Interface>
constexpr auto DerivedFrom = []<typename T>() { return std::derived_from<T, Interface>; };

struct TestableMarker {};

constexpr auto Testable = trait{
    DerivedFrom<TestableMarker>,  // easier to find all testable elements in the code base

    Method<"runTests", bool() const>,
};

// constraints allow for variants, too

// MSVC bug

// template <typename... Types>
// requires (sizeof...(Types) > 1)
// constexpr auto OneOf = [] <typename T> () { return (... or std::same_as<T, Types>); };

template <typename... Types>
    requires(sizeof...(Types) > 1)
struct one_of {
    template <typename T>
    constexpr auto operator()() const noexcept {
        return (... or std::same_as<T, Types>);
    }
};

template <typename... Types>
    requires(sizeof...(Types) > 1)
constexpr auto OneOf = one_of<Types...>{};

void printArea(is<OneOf<Circle, Square>> auto shape) {
    if constexpr (std::same_as<decltype(shape), Circle>)
        std::cout << std::format("Circle area = {}\n",
                                 std::numbers::pi * shape.radius * shape.radius);
    else
        std::cout << std::format("Square area = {}\n", shape.length * shape.length);
}

auto PrintAreaOfShapes = test([] {
    printArea(Circle{1.0});
    printArea(Square{2.0});
});

// traits support default method implementations

constexpr auto Action =
    trait{Method<"run", bool()>,

          // many actions don't need initialization
          Method<"init", bool()> = []([[maybe_unused]] auto& action) { return true; },

          // cleanup neither
          Method<"cleanup", void()> = []([[maybe_unused]] auto& action) {}};

// however, instead of ...

#if 0

    auto run (is<Action> auto& action)
    {
        if (not action.init ()) // OOPS ... may not compile
            return false;

        const bool ok = action.run();

        action.cleanup (); // OOPS ... may not compile
        return ok;
    }

#endif

// you'll then have to write

auto run(is<Action> auto& action) {
    auto action_impl = as<Action>(action);  // OR: trait_cast<Action> (action)

    if (not action_impl.init()) return false;

    const bool ok = action_impl.run();

    action_impl.cleanup();
    return ok;
}

// let's use it

struct SimpleAction {
    auto run() -> bool {
        printSourceLocation();
        return true;
    }
};

auto RunSimpleAction = test([] {
    auto action = SimpleAction{};
    return run(action);
});

// traits allow you to implement behavior in a non-intrusive manner

struct ForeignAction {
    enum class Status : std::uint8_t { Failed, Ok };

    auto execute() {
        if (not ready) return Status::Failed;

        printSourceLocation();
        return Status::Ok;
    }

    bool ready{false};
};

// provide trait implementation in the same namespace so ADL kicks in

constexpr auto get(impl_for<Action, ForeignAction>) {
    return impl{"run"_method = [](ForeignAction& action) -> bool {
                    return action.execute() == ForeignAction::Status::Ok;
                },
                "init"_method = [](ForeignAction& action) -> bool {
                    action.ready = true;
                    return true;
                },
                "cleanup"_method = [](ForeignAction& action) -> void { action.ready = false; }};
}

// let's test it

auto RunForeignAction = test([] {
    auto action = ForeignAction{};
    return run(action);
});

// trait implementations behave as expected

struct DerivedForeignAction : ForeignAction {};

auto RunDerivedForeignAction = test([] {
    auto action = DerivedForeignAction{};
    return run(action);
});

// another example

struct Tweet {
    std::string user;
    std::string text;

    static auto getUser(Tweet const& tweet) { return tweet.user; }
    static auto getText(Tweet const& tweet) { return tweet.text; }
};

// you can also you function pointers instead of lambdas

constexpr auto get(impl_for<WithAuthor, Tweet>) { return impl{"author"_method = &Tweet::getUser}; }

// slightly more compact syntax is also valid

constexpr auto get(impl_for<WithSummary, Tweet>) { return "summary"_method = &Tweet::getText; }

// NOTE: the short syntax also works for multiple methods

// return "author"_method = &Tweet::getUser, "summary"_method = &Tweet::getText;

// let's test it

void post(is<WithAuthorAndSummary> auto const& message) {
    auto withAuthorAndSummary = as<WithAuthorAndSummary>(message);
    std::cout << std::format("{}: {}\n", withAuthorAndSummary.author(),
                             withAuthorAndSummary.summary());
}

auto PostTweet = test([] { post(Tweet{"@elonmusk", "X > Twitter"}); });

// so far we've only talked about static polymorphism,
// but traits work very well with runtime polymorphism

// INTRODUCING: some<'trait'>

// some<> has value semantics like std::any, but offers an interface that is defined by the trait
// you can think of some<> as generalization of std::any with std::any ~ some<Unconstrained>
// some<> is implicit constructible from anything which implements the trait

auto check(some<Action>& action) {
    if (not action.init()) return false;

    action.cleanup();
    return true;
}

auto CheckForeignAction = test([] {
    auto action = some<Action>{ForeignAction{}};
    return check(action);
});

// another example

struct FirstCallback {
    void operator()() { printSourceLocation(); }
};

struct SecondCallback {
    void operator()() { printSourceLocation(); }
};

auto InvokeCallbacks = test([] {
    std::vector<some<Callback>> someCallbacks;

    someCallbacks.emplace_back(FirstCallback{});
    someCallbacks.emplace_back(SecondCallback{});

    for (auto& callback : someCallbacks) callback();
});

// last example

struct Foo {
    void bar() { printSourceLocation(); }
    void bar() const { printSourceLocation(); }
    void bar(bool) { printSourceLocation(); }
    void bar(char const&) { printSourceLocation(); }
    void bar(char&) { printSourceLocation(); }
    void bar(char&&) { printSourceLocation(); }
    void bar(int const&) { printSourceLocation(); }
    void bar(int&) { printSourceLocation(); }
    void bar(int&&) { printSourceLocation(); }
    void bar(float) { printSourceLocation(); }
    void bar(double) { printSourceLocation(); }
};

auto FooBar = test([] {
    some<OverloadedConstness> overloadedConstness = Foo{};

    std::as_const(overloadedConstness).bar();
    overloadedConstness.bar();

    some<OverloadedArgumentType> overloadedArgumentType = Foo{};

    overloadedArgumentType.bar(1.0f);
    overloadedArgumentType.bar(1.0);

    some<OverloadedValueCategory> overloadedValueCategory = Foo{};

    char c = ' ';

    overloadedValueCategory.bar(std::as_const(c));
    overloadedValueCategory.bar(c);
    overloadedValueCategory.bar(c);

    int i = 0;

    overloadedValueCategory.bar(std::as_const(i));
    overloadedValueCategory.bar(i);
    overloadedValueCategory.bar(i);

    some<OverloadedArity> overloadedArity = Foo{};

    overloadedArity.bar(true);
    overloadedArity.bar();
});

// advanced topics

// some has some optional fine-tuning parameters for

//  - small buffer optimization
//  - inlined methods

// let's revisit our region grower example

// do you remember?

struct Vector3i {
    std::int32_t x, y, z;
};

namespace legacy {
template <typename T>
class EmbeddedListNode { /* ... */
};

template <typename T>
class EmbeddedList {
   public:
    using iterator = const T*;
    [[nodiscard]] auto begin() const -> iterator;
    [[nodiscard]] auto end() const -> iterator;
    [[nodiscard]] auto empty() const -> bool;
    auto front() -> T&;
    void push_back(T&);

    /* ... */
};

class GeoConstraintIF : public EmbeddedListNode<GeoConstraintIF> {
   public:
    GeoConstraintIF() = default;
    virtual ~GeoConstraintIF() = default;

    void setBreaking(bool breaking) { mBreaking = breaking; }
    [[nodiscard]] auto isBreaking() const -> bool { return mBreaking; }

    [[nodiscard]] virtual auto checkPoint(const Vector3i& point) const -> bool = 0;

   private:
    bool mBreaking{false};
};

class RegionGrowerIF {
   public:
    virtual ~RegionGrowerIF() {
        while (not mGeoConstraints.empty()) {
            delete &(mGeoConstraints.front());
        }
    }

    auto addGeoConstraint(GeoConstraintIF* constraint) -> bool {
        if (! constraint) {
            return false;
        }

        mGeoConstraints.push_back(*constraint);
        return true;
    }

    // don't know how to copy the constraints ...
    RegionGrowerIF(RegionGrowerIF const& other) = delete;
    auto operator=(RegionGrowerIF const& other) -> RegionGrowerIF& = delete;

   protected:
    auto checkGeoConstraints(Vector3i const& point) -> bool {
        for (const auto& constraint : mGeoConstraints)
            if (not constraint.checkPoint(point)) return false;

        return true;
    }

    EmbeddedList<GeoConstraintIF> mGeoConstraints;
};
}  // namespace legacy

// let's do it with a trait instead!

inline namespace with_traits {
// define behavior(s) of a geo constraint

constexpr auto GeoConstraint =
    trait{Method<"setBreaking", void(bool breaking)>, Method<"isBreaking", bool() const>,

          Method<"check", bool(const Vector3i& point) const>};

// use trait for region grower implementation

class RegionGrower {
   public:
    void addGeoConstraint(is<GeoConstraint> auto constraint) {
        mGeoConstraints.emplace_back(std::move(constraint));
    }

   protected:
    auto checkGeoConstraints(Vector3i const& point) -> bool {
        return std::ranges::all_of(mGeoConstraints,
                                   [&](auto const& constraint) { return constraint.check(point); });
    }

   private:
    // NOTE: we could also make SomeGeoConstraint a public type definition and accept
    // SomeGeoConstraint arguments in our API
    using SomeGeoConstraint = some<GeoConstraint, buffer<112>,
                                   inlined{Method<"check", bool(const Vector3i& point) const>}>;
    static_assert(sizeof(SomeGeoConstraint) == 128);

    std::vector<SomeGeoConstraint> mGeoConstraints;
};

// let's make it easier to implement different constraints

struct Breakable {
    constexpr void setBreaking(bool breaking) { mBreaking = breaking; }
    [[nodiscard]] constexpr auto isBreaking() const -> bool { return mBreaking; }

   private:
    bool mBreaking{false};

   private:
    // add some unnecessary bytes for demonstration purposes
    alignas(8) std::array<char, 104> reserved_for_future_usage = iota_array<char, 104>();
};

static_assert(sizeof(Breakable) == 112);

// example constraint(s)

template <Vector3i Low>
struct GreaterOrEqual : Breakable {
    [[nodiscard]] auto check(const Vector3i& point) const -> bool {
        return point.x >= Low.x and point.y >= Low.y and point.z >= Low.z;
    }
};

template <Vector3i High>
struct LessOrEqual : Breakable {
    [[nodiscard]] auto check(const Vector3i& point) const -> bool {
        return point.x <= High.x and point.y <= High.y and point.z <= High.z;
    }
};

// and use it

class TestableRegionGrower : public RegionGrower {
    static auto makeRegionGrower() {
        TestableRegionGrower regionGrower;

        regionGrower.addGeoConstraint(GreaterOrEqual<Vector3i{0, 0, 0}>{});
        regionGrower.addGeoConstraint(LessOrEqual<Vector3i{0, 1, 2}>{});

        return regionGrower;
    }

   public:
    static auto test() -> bool {
        auto regionGrower = makeRegionGrower();
        return regionGrower.checkGeoConstraints(Vector3i{0, 1, 2});
    }
};

// so many benefits:
//  - simpler implementation of region grower
//  - region grower and geo constraint have value semantics
//  - simpler client code
//  - fastest possible implementation
}  // namespace with_traits

namespace with_stronger_coupling {
// define behavior(s) of a geo constraint

struct GeoConstraintIF {
    constexpr void setBreaking(bool breaking) { mBreaking = breaking; }
    [[nodiscard]] constexpr auto isBreaking() const -> bool { return mBreaking; }

   private:
    bool mBreaking{false};
};

constexpr auto GeoConstraint = trait{DerivedFrom<GeoConstraintIF>,

                                     Method<"check", bool(const Vector3i& point) const>};

// use trait for region grower implementation

class RegionGrower {
   public:
    void addGeoConstraint(is<GeoConstraint> auto constraint) {
        mGeoConstraints.emplace_back(std::move(constraint));
    }

   protected:
    auto checkGeoConstraints(Vector3i const& point) -> bool {
        return std::ranges::all_of(mGeoConstraints,
                                   [&](auto const& constraint) { return constraint.check(point); });
    }

   private:
    using SomeGeoConstraint = some<GeoConstraint, buffer<112>,
                                   inlined{Method<"check", bool(const Vector3i& point) const>}>;
    std::vector<SomeGeoConstraint> mGeoConstraints;
};

// example constraint(s)

template <Vector3i Low>
struct GreaterOrEqual : GeoConstraintIF {
    [[nodiscard]] auto check(const Vector3i& point) const -> bool {
        return point.x >= Low.x and point.y >= Low.y and point.z >= Low.z;
    }
};

template <Vector3i High>
struct LessOrEqual : GeoConstraintIF {
    [[nodiscard]] auto check(const Vector3i& point) const -> bool {
        return point.x <= High.x and point.y <= High.y and point.z <= High.z;
    }
};
}  // namespace with_stronger_coupling

// there is an explicit some_variant class

// why would you use this instead of std::variant

// wehn you want to provide an API

// when you want to implement the variant behavior for each type individually

// when you want a different storage model for the variant (only small values are stored inside)

// with traits you can define shared behavior and restrict the set of valid types

// you can also

void printCircumference(some_variant<Circle, Square> const& shape) {
    visit(overload{[](Circle const& circle) {
                       std::cout << std::format("Circle circumference = {}\n",
                                                std::numbers::pi * 2.0 * circle.radius);
                   },
                   [](Square const& square) {
                       std::cout << std::format("Square circumference = {}\n", 4.0 * square.length);
                   }},
          shape);
}

// NOTE: some_variant is equivalent to std::variant
// - other than visit() it provides no dedicated API
// - its size is large enough to hold all alternatives inplace

auto PrintCircumferenceOfShapes = test([] {
    printCircumference(Circle{1.0});
    printCircumference(Square{2.0});
});

// however, you can also define some variants with additional traits or customized storage

// here you can change the inline buffer and decide where to implement the behavior for alternatives

constexpr auto WithType = trait{Method<"type", std::string() const>};

using Shape = some<WithType>::variant<Circle, Square>;

constexpr auto get(impl_for<WithType, Circle>) {
    return "type"_method = [](Circle const&) -> std::string { return "Circle"; };
}

constexpr auto get(impl_for<WithType, Square>) {
    return "type"_method = [](Square const&) -> std::string { return "Square"; };
}

void printType(Shape const& shape) { std::cout << std::format("Type = {}\n", shape.type()); }

auto PrintName = test([] {
    printType(Circle{1.0});
    printType(Square{2.0});
});

}  // namespace trait_examples

#ifdef __clang__
#pragma clang diagnostic pop
#endif

auto main() -> int {
    using namespace trait_examples;

    test::run_all();

    TestableRegionGrower::test();

    return 0;
}
