# Traits for C++

Define shared behavior in a non-intrusive way while preserving value semantics.

This library is inspired by [Rust Traits](https://doc.rust-lang.org/book/ch10-02-traits.html) and previous projects like [Dyno](https://github.com/ldionne/dyno).

> [!CAUTION]
> At this point, this library is experimental and it is a pure curiosity.
> No stability of interface or quality of implementation is guaranteed.
> Use at your own risks.

## Quick Start

*traits* is a header-only C++20 library. To use the library, make sure your compiler meets the [minimum requirements](#compiler-req) and just include the header file [traits.h](https://github.com/VolumeGraphics/traits/blob/main/traits.h) in your source code.
Alternatively, you can evaluate the library directly in [Compiler Explorer](https://godbolt.org/...).

<!-- Alternatively, you can install the library via [vcpkg](https://learn.microsoft.com/en-us/vcpkg/get_started/overview) or [conan](https://conan.io/), by searching for "proxy" (see [vcpkg.io](https://vcpkg.io/en/package/proxy) and [conan.io](https://conan.io/center/recipes/proxy)). -->

### Example

```c++
#include <format>
#include <iostream>

#include "traits.h"

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
- I was able to become more familiar with many C++ features and learn techniques that are useful for other tasks

**In general, it is a bad idea to implement such a feature at the library level:**
- The code is not yet ready for production and probably never will be
- The implementation is very complex and therefore difficult to maintain
  - Only works with the help of (a few) macros
  - Various workarounds for compiler bugs and language restrictions
  - Probably also some serious bugs in the implementation
- Often really bad error messages (not only but also because of the macros)
- Poor compilation times and some annoying compiler warnings
- Bad debugging experience
- No additional support from the IDE
- Only some of these problems could be mitigated, e.g. through precompiled traits

> [!CAUTION]
> **I therefore explicitly advise against using this library in productive applications.**

While I am very happy with the outcome, such library-level implementations ultimately highlight the weaknesses of C++ and hopefully increase the incentives for the C++ committee to address these shortcomings in the language itself, as they emphasize the community's need for such features.

This implementation shows once again that polymorphism can be easily combined with value semantics. In many cases, this reduces the amount of code that deals with dynamic memory allocation and thus potentially unsafe code. Ultimately, developers can concentrate more on the what and not on the how.

## <a name="compiler-req">Minimum Requirements for Compilers</a>

| Family | Minimum version | Required flags |
| ------ | --------------- | -------------- |
| clang  | 17.0.1          | -std=c++20     |
| gcc    | 13.3            | -std=c++20     |
| MSVC   | 19.36           | /std:c++20     |

## Using the library step by step

All of the code below can be found in the [example](...).

### traits allow you to define shared behavior with a declarative syntax

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

### traits can be used to constrain generic types (static polymorphism)

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

... you can use the trait from the initial example above:

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

## License

*traits* is BSD-3 licensed, as found in the [LICENSE][l] file.

[l]: https://github.com/VolumeGraphics/traits/blob/main/LICENSE

## Related projects

- [Dyno: Runtime polymorphism done right](https://github.com/ldionne/dyno)
- [Proxy: Next Generation Polymorphism in C++](https://github.com/microsoft/proxy)
