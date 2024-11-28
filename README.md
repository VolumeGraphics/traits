# Traits for C++

[![windows](https://github.com/VolumeGraphics/traits/actions/workflows/windows.yml/badge.svg?cache-control=no-cache)](https://github.com/VolumeGraphics/traits/actions/workflows/windows.yml)
[![linux](https://github.com/VolumeGraphics/traits/actions/workflows/linux.yml/badge.svg?cache-control=no-cache)](https://github.com/VolumeGraphics/traits/actions/workflows/linux.yml)
[![macos](https://github.com/VolumeGraphics/traits/actions/workflows/macos.yml/badge.svg?cache-control=no-cache)](https://github.com/VolumeGraphics/traits/actions/workflows/macos.yml)
[![lint](https://github.com/VolumeGraphics/traits/actions/workflows/lint.yml/badge.svg?cache-control=no-cache)](https://github.com/VolumeGraphics/traits/actions/workflows/lint.yml)
[![codecov](https://codecov.io/github/VolumeGraphics/traits/graph/badge.svg?token=BK2TDD58X6)](https://codecov.io/github/VolumeGraphics/traits)

[![License](https://img.shields.io/badge/License-BSD_3--Clause-blue.svg)](LICENSE)

Define shared behavior in a non-intrusive way while preserving value semantics.

This library is inspired by [Rust Traits](https://doc.rust-lang.org/book/ch10-02-traits.html) and previous projects like [Dyno](https://github.com/ldionne/dyno).

> [!CAUTION]
> At this point, this library is experimental and it is a pure curiosity.
> No stability of interface or quality of implementation is guaranteed.
> Some design decisions are likely to change
> and have a big effect on the way the library is used.
> Use at your own risks.

## Quick Start

*traits* is a single header C++20 library. To use the library, make sure you meet the [minimum requirements](#minimum-requirements) and just include the header file [traits.h](https://github.com/VolumeGraphics/traits/blob/main/include/traits.h) in your source code.
Alternatively, you can first try it out in [Compiler Explorer](https://godbolt.org/z/be31xGjrK).

CMake projects might build, install and `find_package(traits)` or use fetch content:

```cmake
include(FetchContent)
FetchContent_Declare(traits URL https://github.com/VolumeGraphics/traits/releases/latest/download/traits.zip)
FetchContent_MakeAvailable(traits)
```

There are currently no plans to support [vcpkg](https://learn.microsoft.com/en-us/vcpkg/get_started/overview) or [conan](https://conan.io/), as I do not recommend using the library in a productive environment ([see below](#motivation)).

### Canonical usage example

```c++
#include <format>
#include <iostream>
#include <vector>

#include "traits.h"
using namespace traits;

constexpr auto Drawable = trait{
    TRAITS_METHOD(draw, void(std::ostream& stream) const)
};

struct Circle {
  double radius{ 0.0 };
};

constexpr auto get(impl_for<Drawable, Circle>) {
  return "draw"_method = [](Circle const& circle, std::ostream& stream) {
    stream << std::format("Circle {{ radius = {} }}\n", circle.radius);
    };
}

struct Square {
  double length{ 0.0 };
};

constexpr auto get(impl_for<Drawable, Square>) {
  return "draw"_method = [](Square const& square, std::ostream& stream) {
    stream << std::format("Square {{ length = {} }}\n", square.length);
    };
}

int main()
{
  std::vector<some<Drawable>> someDrawables;

  someDrawables.emplace_back(Circle{ 1.0 });
  someDrawables.emplace_back(Square{ 2.0 });

  for (auto const& drawable : someDrawables)
    drawable.draw(std::cout);
}
```

## <a name="motivation">Motivation</a>

Polymorphism is probably used too often. Nevertheless, it remains a useful concept for numerous use cases. Unfortunately, the standard approach to runtime polymorphism in C++ has some disadvantages, as many have already pointed out.

For example, polymorphism via inheritance breaks value semantics and typically forces you to use dynamic memory management. This not only harbors risks for unsafe code (e.g. in the case of ignorance of modern language concepts), but above all leads to unnecessary complexity.

This project was primarily intended as a personal learning experience:
- I wanted to explore ways to avoid accidental complexity
- I wanted to deepen my understanding of cross-language concepts and their implementation in other languages
- I wanted to become more familiar with current C++ features and learn techniques that are useful for other tasks

While I am very happy with the outcome, the resulting code is not yet ready for production and probably never will be.

> [!CAUTION]
> **You should *NOT* use this library in productive environments.**

In general, it is a bad idea to implement such a feature at the library level:
- The implementation is very complex and therefore difficult to maintain
  - Only works with the help of (a few) macros
  - Various workarounds for compiler bugs and language restrictions
  - Probably also some serious bugs in the implementation
- Often really bad error messages (not only but also because of the macros)
- Poor compilation times and some annoying compiler warnings
- Bad debugging experience
- No additional support from the IDE
- Only some of these problems could be mitigated, e.g. through precompiled traits

Such library-level implementations ultimately highlight the current weaknesses of C++ and hopefully increase the incentives for the C++ committee to address these shortcomings in the language itself, as they emphasize the community's need for such features.

This implementation shows once again that polymorphism can be easily combined with value semantics. In many cases, this reduces the amount of code that deals with dynamic memory allocation and thus potentially unsafe code. Ultimately, developers can concentrate more on the what and not on the how.

## Related projects

There have been some exciting projects dedicated to this topic for a long time:
- [Dyno: Runtime polymorphism done right](https://github.com/ldionne/dyno)
- [Boost.TypeErasure](https://www.boost.org/doc/libs/1_86_0/doc/html/boost_typeerasure.html)
- ...

But new implementations are also emerging more recently, e.g. [Proxy: Next Generation Polymorphism in C++](https://github.com/microsoft/proxy).

Ideas for offering this feature at the language level seem to make the most sense:
- [Typeclasses in C++](https://github.com/TartanLlama/typeclasses)
 
You might also take also a look at the rust documentation to get familiar with the basic idea of [traits](https://doc.rust-lang.org/book/ch10-02-traits.html). Some explanations from there have been included in this documentation.

## <a name="minimum-requirements">Minimum Requirements</a>

CMake version 3.24 or higher is required to build the library.

### <a name="compiler-requirements"> Compiler

| Family | Minimum version | Required flags |
| ------ | --------------- | -------------- |
| clang  | 16.0.0          | -std=c++20     |
| gcc    | 13.3            | -std=c++20     |
| MSVC   | 19.34           | /std:c++20     |

## Using the library ... step by step

A trait defines the functionality a particular type has and can share with other types. We can use traits to define shared behavior in an abstract way.

> [!TIP]
> All of the code below can be found in the [example](https://github.com/VolumeGraphics/traits/blob/main/example/readme.cpp).
> To keep this overview compact, definitions that have already been used in previous examples are not repeated. It is therefore highly recommended that you read all the examples in sequence.

### traits allow you to define shared behavior with a declarative syntax

A type’s behavior consists of the methods we can call on that type.

```c++
constexpr auto WithAuthor = trait
{
    Method<"author", std::string() const>
};
```

Every method is uniquely identified by its signature, which consists of a name and a function type.

A type that supports this trait must therefore offer a method with exactly this signature.
With the help of a special `target` type it is also possible to explicitly mention the target object in the signature, i.e.

```c++
constexpr auto WithAuthor = trait
{
    Method<"author", std::string(target const& self)>
};
```

is an equivalent definition of the `WithAuthor` trait.
This syntax is more explicit and is also more similar to the syntax for trait implementations, but is also more verbose.

> [!IMPORTANT]  
> `Method<>` refers to a predefined variable template.
> To be able to use this syntax, you must first make exactly the same method name available for the traits library with the help of a macro that is used in the global namespace.
> 
> ```c++
> TRAITS_METHOD_DECLARATION(author);
> ```

There is an alternative syntax for defining traits without having to declare a method name first.

```c++
constexpr auto WithSummary = trait
{
    TRAITS_METHOD (summary, std::string() const) // no previous declaration of 'summary' necessary
};
```

> [!TIP]
> Please always pay attention to the canonical spelling of method names. For example, no extra spaces should appear in overloaded operators.

### traits can be used to constrain generic types (static polymorphism)

Different types share the same behavior if we can call the same methods on all of those types.

Instead of ...

```c++
decltype (auto) operator<< (std::ostream& stream, auto const& drawable)
requires requires { { drawable.draw(stream) } -> std::same_as<void>; }
{
    drawable.draw (stream);
    return stream;
}
```

... or ...

```c++
template <typename T>
concept Drawable = requires (T drawable, std::ostream& stream) { { drawable.draw(stream) } -> std::same_as<void>; };

decltype (auto) operator<< (std::ostream& stream, Drawable auto const& drawable)
{
    drawable.draw (stream);
    return stream;
}
```

... you can use a trait like the one in the initial example above:

```c++
decltype (auto) operator<< (std::ostream& stream, is<Drawable> auto const& drawable)
{
    drawable.draw (stream);
    return stream;
}

auto drawCircle ()
{
    std::cout << Circle{3.0};
}
```

`is<'trait'>` is a C++ concept provided by the library that checks the type without `const` or `volatile` modifiers and as a non-reference type (i.e. the result of `std::remove_cvref_t`).
This makes it easier to use this concept for forwarding references.

### traits can have multiple behaviors

Trait definitions are a way to group method signatures together to define a set of behaviors necessary to accomplish some purpose.

```c++
constexpr auto Runnable = trait
{
    Method<"start", void()>,
    Method<"stop", void()>,

    Method<"isRunning", bool() const>,
};

void run (is<Runnable> auto& runnable)
{
    if (not runnable.isRunning())
    {
        runnable.start ();

        // ...

        runnable.stop ();
    }
}
```

### traits support overloaded methods 

```c++
constexpr auto OverloadedConstness = trait
{
    Method<"bar", void() const>,
    Method<"bar", void()>,
};

constexpr auto OverloadedArgumentType = trait
{
    Method<"bar", void(float value)>,
    Method<"bar", void(double value)>,
};

constexpr auto OverloadedValueCategory = trait
{
    Method<"bar", void(int const& lvalue)>,
    Method<"bar", void(int& lvalue)>,
    Method<"bar", void(int&& rvalue)>,
};

constexpr auto OverloadedArity = trait
{
    Method<"bar", void(bool value)>,
    Method<"bar", void()>,
};
```

### traits support (certain) overloaded operators

```c++
constexpr auto Callback = trait
{
    Method<"operator()", void()>,
};

void myAlgorithm (is<Callback> auto& eventProcessing)
{
    // ...

    eventProcessing();

    // ...

    eventProcessing();

    //...
}
```

> [!TIP]
> The function call operator does not have to be declared separately with `TRAITS_METHOD_DECLARATION(operator())`.

### traits can be templated

```c++
template <typename T>
constexpr auto ValidatorFor = trait
{
    Method<"check", bool(T const&) const>,
};

constexpr auto IntValidator = ValidatorFor<int>;
```

### traits are composable

traits can be combined with `+` (this syntax is borrowed from Rust) ...

```c++
void print (std::ostream& out, is<WithAuthor + WithSummary> auto const& article)
{
    out << std::format ("{} by {}\n", article.summary(), article.author());
}
```

... but they also support a boolean syntax:

```c++
constexpr auto WithAuthorAndSummary = WithAuthor and WithSummary; // declare trait for later reuse
```

### traits support additional type constraints

In contrast to rust, these traits support an optional constraint at the beginning of the parameter list.

A constraint is a templated callable: `<typename> () -> bool`

```c++
constexpr auto DefaultConstructible = [] <typename T> () { return std::is_default_constructible_v<T>; };
```

A number of use cases are supported by constraints.

#### constraints can check arbitrary type properties

Sometimes you want to ensure not only the behaviors of a type, but also other characteristics.

```c++
constexpr auto Empty = [] <typename T> () { return std::is_empty_v<T>; };

constexpr auto StatelessAllocator = trait
{
    Empty and DefaultConstructible,

    Method<"alloc", void* (std::size_t byteCount) const>,
    Method<"free" , void  (void* ptr) const>
};
```

#### constraints allow easy definition of derived constraints

C++ concepts are not first class citizens at the moment:
- you can't pass them as template parameters
- it is complicated to define derived concepts

Look at this example:

```c++
struct Any final
{
    Any (auto&& value); // OOPS ... clashes with copy/move constructor



    // let's define a constructor which takes anything but ourselves instead

    // 1. this syntax is currently not allowed
    Any (not std::same_as<Any> auto&& value);

    // 2. this syntax is somewhat awkward
    Any (auto&& value) requires (not std::same_as<std::remove_cvref_t<decltype(value)>, Any>);

    // 3. this syntax requires explicit definition of another concept, see below
    Any (not_same_as<Any> auto&& value);
};

template <typename T, typename U>
concept not_same_as = not std::same_as<std::remove_cvref_t<T>, U>; // sic! T might be deduced to a reference type
```

On the other hand, with a constraint ...

```c++
template <typename U>
constexpr auto SameAs = [] <typename T> () { return std::same_as<T, U>; };
```

... we can define derived constraints as required, because they support all common boolean operators:

```c++
struct Any
{
    Any (is<not SameAs<Any>> auto&& value);
};
```

> [!NOTE]  
> `is<'constraint'>` is equivalent to `is<trait{'constraint'}>`

This check will work even when value will be deduced as reference type.

#### constraints can be used to force strong(er) coupling

It may be advantageous to manage all implementations of a trait in a class hierarchy because, for example, the IDE supports inheritance particularly well.

```c++
template <typename Interface>
constexpr auto DerivedFrom = [] <typename T> () { return std::derived_from<T, Interface>; };

struct TestableMarker
{        
};

constexpr auto Testable = trait
{
    DerivedFrom<TestableMarker>, // make it easier to find all testable elements in the code base

    Method<"runTests", bool() const>,
};
```

#### constraints allow easy definition of variant types

Given a simple constraint:

```c++
template <typename... Types>
requires (sizeof...(Types) > 1)
constexpr auto OneOf = [] <typename T> () { return (... or std::same_as<T, Types>); };
```

We can easily define variant types.

```c++
void printArea (is<OneOf<Circle, Square>> auto shape)
{
    if constexpr (std::same_as<decltype (shape), Circle>)
        std::cout << std::format ("Circle area = {}\n", std::numbers::pi * shape.radius * shape.radius);
    else
        std::cout << std::format ("Square area = {}\n", shape.length * shape.length);
}
```

And use them as expected.

```c++
printArea (Circle{1.0});
printArea (Square{2.0});
```

> [!NOTE]  
> There is a bug in the current MSVC compilers, so the constraint should actually be written as follows:
> ```c++
> // template <typename... Types>
> // requires (sizeof...(Types) > 1)
> // constexpr auto OneOf = [] <typename T> () { return (... or std::same_as<T, Types>); };
>
> template <typename... Types>
> requires (sizeof...(Types) > 1)
> struct one_of
> {
>     template <typename T>
>     constexpr auto operator() () const noexcept
>     {
>         return (... or std::same_as<T, Types>);
>     }
> };
>
> template <typename... Types>
> requires (sizeof...(Types) > 1)
> constexpr auto OneOf = one_of<Types...>{};
> ```

### traits support default method implementations

Sometimes it’s useful to have default behavior for some or all of the methods in a trait instead of requiring implementations for all methods on every type. 

```c++
constexpr auto Action = trait
{
    Method<"run", bool()>,

    // many actions don't need initialization
    Method<"init", bool()> = [] ([[maybe_unused]] auto& action)
    {
        return true;
    },

    // cleanup neither
    Method<"cleanup", void()> = [] ([[maybe_unused]] auto& action)
    {
    }
};
```

However, instead of ...

```c++
auto run (is<Action> auto& action)
{
    if (not action.init ()) // OOPS ... may not compile
        return false;

    const bool ok = action.run();

    action.cleanup (); // OOPS ... may not compile
    return ok;
}
```

... you’ll then have to write:

```c++
auto run (is<Action> auto& action)
{
    auto action_impl = as<Action> (action); // OR: trait_cast<Action> (action)

    if (not action_impl.init ())
        return false;

    const bool ok = action_impl.run();

    action_impl.cleanup ();
    return ok;
}
```

`as<'trait'> (lvalue_ref)` creates a reference wrapper which provides all trait behaviors as public API.

> [!TIP]
> You should always access trait behaviors of an object via the reference wrapper (even when behaviors do not have a default implementation) because traits allow behaviors to be defined non-intrusively (see below).

Now this code compiles and uses the given default implementations:

```c++
struct SimpleAction
{
    bool run ()
    {
        return true;
    }
};

auto runSimpleAction ()
{
    auto action = SimpleAction{};
    return run (action);
}
```

### traits allow you to implement behavior in a non-intrusive manner

Given some type for which we want to support all `Action` behaviors from above ...

```c++
struct ForeignAction
{
    enum class Status { Failed, Ok };

    auto execute ()
    {
        if (not ready)
            return Status::Failed;

        // ...

        return Status::Ok;
    }

    bool ready{false};
};
```

... we can provide an implementation of the `Action` trait in the same namespace (so ADL kicks in):

```c++
constexpr auto get (impl_for<Action, ForeignAction>)
{
    return impl
    {
        "run"_method = [] (ForeignAction& action) -> bool
        {
            return action.execute () == ForeignAction::Status::Ok;
        },
        "init"_method = [] (ForeignAction& action) -> bool
        {
            action.ready = true;
            return true;
        },
        "cleanup"_method = [] (ForeignAction& action) -> void
        {
            action.ready = false;
        }
    };
}
```

> [!NOTE]  
> `"..."_method` is a user-defined string literal to make the code more readable.
> You can also use the `Method<"..."> =` syntax which is a bit more consistent with the trait definition syntax.
> However, make sure that you omit the parameter for the function type, as this is automatically derived.

> [!IMPORTANT]  
> You must provide an implementation for all behaviors which do not already have a default implementation, but you can override a default behavior of course.

Let’s test it:

```c++
auto runForeignAction ()
{
    auto action = ForeignAction{};
    return run (action);
}
```

A trait implementation is valid for all derived types, unless there is a more specialized implementation.

```c++
struct DerivedForeignAction : ForeignAction
{
};

auto runDerivedForeignAction ()
{
    auto action = DerivedForeignAction{};
    return run (action);
}
```

Let’s give another example:

```c++
struct Tweet
{
    std::string user;
    std::string text;

    static auto getUser (Tweet const& tweet) { return tweet.user; }
    static auto getText (Tweet const& tweet) { return tweet.text; }
};
```

You can also use function pointers instead of lambdas.

```c++
constexpr auto get (impl_for<WithAuthor, Tweet>)
{
    return impl { "author"_method = &Tweet::getUser };
}
```

A slightly more compact syntax is also valid, because `impl` is only an optional wrapper to make the code more explicit.

```c++
constexpr auto get (impl_for<WithSummary, Tweet>)
{
    return "summary"_method = &Tweet::getText;
}
```

> [!TIP]  
> The short syntax also works for multiple methods and lambda implementations.

We can now use the type in a function that requires both traits.

```c++
void post (is<WithAuthorAndSummary> auto const& message)
{
    auto withAuthorAndSummary = as<WithAuthorAndSummary> (message);
    std::cout << std::format ("{}: {}\n", withAuthorAndSummary.author(), withAuthorAndSummary.summary());
}

auto postSomeTweet ()
{
    post (Tweet{"@elonmusk", "X > Twitter"});
}
```

So far we've only talked about static polymorphism, but ...

### traits work very well with runtime polymorphism 

Introducing ... `some<'trait'>`

`some<>` has value semantics like `std::any`, but offers a public API that is defined by the trait.
You can think of `some<>` as generalization of `std::any` with `std::any` ~ `some<trait{}>`.
`some<>` is implicit constructible from anything which implements the trait.

```c++
auto onlyCheck (some<Action>& action)
{
    if (not action.init ())
        return false;

    action.cleanup ();
    return true;
}

auto onlyCheckForeignAction ()
{
    auto action = some<Action> {ForeignAction{}};
    return check (action);
}
```

> [!NOTE]  
> Here we no longer use static polymorphism and provide a function template, but `some<>` erases the concrete type and we only define a single (exportable) function.

Another example.

```c++
struct FirstCallback
{
    void operator () () {}
};

struct SecondCallback
{
    void operator () () {}
};

auto invokeCallbacks ()
{
    std::vector<some<Callback>> someCallbacks;

    someCallbacks.emplace_back (FirstCallback{});
    someCallbacks.emplace_back (SecondCallback{});

    for (auto& callback : someCallbacks)
        callback ();
}
```

Last example.

```c++
struct Foo
{
    void bar () {}
    void bar () const {}
    void bar (bool) {}
    void bar (int const&) {}
    void bar (int&) {}
    void bar (int&&) {}
    void bar (float) {}
    void bar (double) {}
};

auto fooBar ()
{
    some<OverloadedConstness> overloadedConstness = Foo{};

    std::as_const (overloadedConstness).bar();
    overloadedConstness.bar();

    some<OverloadedArgumentType> overloadedArgumentType = Foo{};

    overloadedArgumentType.bar(1.0f);
    overloadedArgumentType.bar(1.0);

    some<OverloadedValueCategory> overloadedValueCategory = Foo{};

    int i = 0;

    overloadedValueCategory.bar(std::as_const (i));
    overloadedValueCategory.bar(i);
    overloadedValueCategory.bar(std::move (i));

    some<OverloadedArity> overloadedArity = Foo{};

    overloadedArity.bar(true);
    overloadedArity.bar();
}
```

## Using the library ... advanced concepts

### precise control of the memory requirements

`some<>` offers the following customization options:
- small buffer optimization
- inlined methods

### unerasing some types

If you ever need to unerase the type stored within a `some<>`, you can ask with `.type()` for the `std::type_info` and try a `some_cast<Type>` which behaves exactly like a `std::any_cast<Type>`.

```c++
auto changeShape (some<OneOf<Circle, Square>> shape)
{
    if (shape.type () == typeid (Circle))
        shape = Square { some_cast<Circle> (shape).radius / std::numbers::inv_sqrtpi };
    else
        shape = Circle { some_cast<Square> (shape).length * std::numbers::inv_sqrtpi };

    return shape;
}

auto changeShapeTest()
{
    auto circle = Circle{1.0};
    printArea (circle);

    auto square = some_cast<Square> (changeShape (circle));
    printArea (square);

    auto circleAgain = some_cast<Circle> (changeShape (square));
    printArea (circleAgain);
}
```

### explicit support for variant types

For a number of reasons, it makes sense to explicitly support `some<>` variant types and offer an alternative to `std::variant`:
- if you want to centrally define not only the possible types, but also the possible behaviors on these types
- if you want to implement the variant behaviors separately for each type
- if you require a different storage model for your variant type

`some_variant<'Types'...>` is a type alias for a specially constrained `some<>` that can be used as a replacement for `std::variant`.

`some<>` provides a `visit()` overload for this purpose:

```c++
void printCircumference (some_variant<Circle, Square> const& shape)
{
    visit (overload // famous overload pattern
    {
        [] (Circle const& circle)
        {
            std::cout << std::format ("Circle circumference = {}\n", std::numbers::pi * 2.0 * circle.radius);
        },
        [] (Square const& square)
        {
            std::cout << std::format ("Square circumference = {}\n", 4.0 * square.length);
        }
    }, shape);
}

auto printCircumferenceOfShapes ()
{
    printCircumference (Circle{1.0});
    printCircumference (Square{2.0});
}
```

`some_variant<>` provides no dedicated API other than `visit()`.
The size of a `some_variant<>` is large enough to store all alternatives inplace.

However, you can also define `some_variant<>`s with additional constraints, expected behaviors or customized storage.
`some<>` offers a special type alias template `variant` for this purpose:

```c++
constexpr auto WithType = trait
{
    Method<"type", std::string () const>
};

using Shape = some<WithType>::variant<Circle, Square>;

constexpr auto get (impl_for<WithType, Circle>)
{
    return "type"_method = [] (Circle const&) -> std::string { return "Circle"; };
}

constexpr auto get (impl_for<WithType, Square>)
{
    return "type"_method = [] (Square const&) -> std::string { return "Square"; };
}

void printType (Shape const& shape)
{
    std::cout << std::format ("Type = {}\n", shape.type ());
}

auto printName ()
{
    printType (Circle{1.0});
    printType (Square{2.0});
}
```

## Tips for use

Since traits are essentially used within `is<...>`, the trait names should be chosen appropriately to maintain a natural reading flow.
For this reason, a noun or the paraphrase *with ... behavior* instead of *has ... behavior* is used in all examples .

## Implementation notes

The implementation uses snake case for all concepts, types and type aliases. CamelCase is used for all global variables.

The current implementation defines the following C++ concepts:
- `function_type`: a function signature
- `callable`: a valid `std::function` target
- `method_id`: a unique identifier for a method
- `constraint`: a test for any type attributes
- `behavior`: a certain behavior
- `behavior_implementation`: an implementation of a behavior
- `is`: a type supports a specific trait

The following types are used in the implementation:
- `method_name`: unique name of a method
- `method_signature<method_name, function_type>` is the only implementation of the `method_id` concept
- `method_implementation<method_id, callable>` is the only implementation of the `behavior_implementation` concept

## Open issues

Here is a list of possible API improvements, in no particular order:
- traits: you must define an empty implementation of a trait, even if all methods have default implementations
- constraints: add support for all boolean operators
- behaviors: add support for more overloaded operators, esp. `operator<<`
- function types: add support for noexcept
- function types: add support for volatile
- `some<>`: always has a value; use `optional<some<>>` instead or introduce `maybe_some<>`
- `some<>`: add conversion from `some<>` other type
- `some<>`: improve syntax for inlined methods

Here is a list of possible implementation improvements, in no particular order:
- fix internal linkage warning
- remove dependency to std::tuple
- remove dependency to std::variant
- do not use unnamed inline namespaces
- move method_kernel into method_name ?
- hide non-public stuff in a detail namespace
- better check for canonical method names
- tests: check macro syntax with method inlining and trait implementations
 
## Known limitations

Here is a list of known problems:
- *clang* generates a warning for unused traits, so they must be annotated with `[[maybe_unused]]` or the warnings must be suppressed in some other way
 
## License

*traits* is BSD-3 licensed, as found in the [LICENSE][l] file.

[l]: https://github.com/VolumeGraphics/traits/blob/main/LICENSE
