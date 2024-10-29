# Traits for C++

Define shared behavior in a non-intrusive way while preserving value semantics.

This library is inspired by [Rust Traits](https://doc.rust-lang.org/book/ch10-02-traits.html) and previous projects like [Dyno](https://github.com/ldionne/dyno).

> [!CAUTION]
> At this point, this library is experimental and it is a pure curiosity.
> No stability of interface or quality of implementation is guaranteed.
> Use at your own risks.

## Quick Start

*traits* is a header-only C++20 library. To use the library, make sure your compiler meets the [minimum requirements](#compiler-req) and just include the header file [traits.h](https://github.com/VolumeGraphics/traits/blob/main/traits.h) in your source code.
Alternatively, you can try it out in [Compiler Explorer](https://godbolt.org/z/jGoM3jeob).

<!-- Alternatively, you can install the library via [vcpkg](https://learn.microsoft.com/en-us/vcpkg/get_started/overview) or [conan](https://conan.io/), by searching for "proxy" (see [vcpkg.io](https://vcpkg.io/en/package/proxy) and [conan.io](https://conan.io/center/recipes/proxy)). -->

### Example usage

```c++
#include <format>
#include <iostream>
#include <vector>

#include <https://raw.githubusercontent.com/VolumeGraphics/traits/refs/heads/main/traits.h>
using namespace traits;

constexpr auto Drawable = trait
{
    Method<"draw", void(std::ostream& stream) const>
};

struct Circle
{
    void draw (std::ostream& stream) const
    {
        stream << std::format ("Circle {{ radius = {} }}\n", radius);            
    }
    double radius{0.0};
};

struct Square
{
    void draw (std::ostream& stream) const
    {
        stream << std::format ("Square {{ length = {} }}\n", length);
    }
    double length{0.0};
};

int main()
{
    std::vector<some<Drawable>> someDrawables;

    someDrawables.emplace_back (Circle{1.0});
    someDrawables.emplace_back (Square{2.0});

    for (auto const& drawable : someDrawables)
        drawable.draw (std::cout);
}
```

## Motivation

This project was primarily intended as a personal learning experience:
- I wanted to explore ways to avoid accidental complexity
- I wanted to deepen my understanding of cross-language concepts and their implementation in other languages
- I wanted to become more familiar with current C++ features and learn techniques that are useful for other tasks

While I am very happy with the outcome, the resulting code is not yet ready for production and probably never will be.

> [!CAUTION]
> **You should not use this library in productive environments.**

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

Such library-level implementations ultimately highlight the weaknesses of C++ and hopefully increase the incentives for the C++ committee to address these shortcomings in the language itself, as they emphasize the community's need for such features.

This implementation shows once again that polymorphism can be easily combined with value semantics. In many cases, this reduces the amount of code that deals with dynamic memory allocation and thus potentially unsafe code. Ultimately, developers can concentrate more on the what and not on the how.

## <a name="compiler-req">Minimum Requirements for Compilers</a>

| Family | Minimum version | Required flags |
| ------ | --------------- | -------------- |
| clang  | 17.0.1          | -std=c++20     |
| gcc    | 13.3            | -std=c++20     |
| MSVC   | 19.36           | /std:c++20     |

## Using the library ... step by step

All of the code below can be found in the [example](...).

### traits allow you to define shared behavior with a declarative syntax

A trait defines the functionality a particular type has and can share with other types. We can use traits to define shared behavior in an abstract way.

```c++
constexpr auto WithAuthor = trait
{
    Method<"author", std::string() const> // needs previous declaration of 'author'
};
```

> [!IMPORTANT]  
> To be able to use this syntax, you must first declare the method name in the global namespace with the help of a macro.

```c++
TRAITS_METHOD_DECLARATION(author);
```

There is an alternative syntax for defining traits without first declaring the method names.

```c++
constexpr auto WithSummary = trait
{
    TRAITS_METHOD (summary, std::string() const) // no previous declaration of 'summary' necessary
};
```

> [!TIP]
> Please always pay attention to the canonical spelling of method names. For example, no extra spaces should appear in overloaded operators.

### traits can be used to constrain generic types (static polymorphism)

A type’s behavior consists of the methods we can call on that type. Different types share the same behavior if we can call the same methods on all of those types.

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
```

> [!NOTE]  
> `is<'trait'>` denotes a C++ concept

And it will behave as expected:
```c++
std::cout << Circle{3.0};
```

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

traits can be combined with `+` (this syntax is borrowed from Rust)

```c++
void print (std::ostream& out, is<WithAuthor + WithSummary> auto const& article)
{
    out << std::format ("{} by {}\n", article.summary(), article.author());
}
```

but they also support a boolean syntax

```c++
constexpr auto WithAuthorAndSummary = WithAuthor and WithSummary; // declare trait for later reuse
```

### traits support an optional type constraint (first parameter)

A constraint is a templated callable: `<typename> () -> bool`

```c++
constexpr auto DefaultConstructible = [] <typename T> () { return std::is_default_constructible_v<T>; };
```

Constraints can be used to check arbitrary type properties.

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
concept not_same_as = not std::same_as<T, U>;
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

... you'll then have to write:

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
        printSourceLocation ();
        return true;
    }
};

auto runSimpleAction ()
{
    auto action = SimpleAction{};
    return run (action);
}
```




## License

*traits* is BSD-3 licensed, as found in the [LICENSE][l] file.

[l]: https://github.com/VolumeGraphics/traits/blob/main/LICENSE

## Related projects

- [Dyno: Runtime polymorphism done right](https://github.com/ldionne/dyno)
- [Proxy: Next Generation Polymorphism in C++](https://github.com/microsoft/proxy)
