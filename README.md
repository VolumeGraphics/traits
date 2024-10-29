# Traits for C++

Define shared behavior in a non-intrusive way while preserving value semantics.

## DISCLAIMER

At this point, this library is experimental and it is a pure curiosity.
No stability of interface or quality of implementation is guaranteed.
Use at your own risks.

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


## Quick Start

*Traits* is a header-only C++20 library. To use the library, make sure your compiler meets the [minimum requirements](#compiler-req) and just include the header file [traits.h](https://github.com/VolumeGraphics/traits/...) in your source code.
Alternatively, you can also evaluate the library in the [Compiler Explorer](https://godbolt.org/...).

<!-- Alternatively, you can install the library via [vcpkg](https://learn.microsoft.com/en-us/vcpkg/get_started/overview) or [conan](https://conan.io/), by searching for "proxy" (see [vcpkg.io](https://vcpkg.io/en/package/proxy) and [conan.io](https://conan.io/center/recipes/proxy)). -->

### Example

```c++
#include <format>
#include <iostream>

#include "traits.h"

constexpr auto Drawable = trait {
    Method<"draw", void(std::ostream& stream) const>
};

struct Circle {
    void draw (std::ostream& stream) const {
        stream << std::format ("Circle {{ radius = {} }}\n", radius);            
    }
    double radius{0.0};
};

struct Square {
    void draw (std::ostream& stream) const {
        stream << std::format ("Square {{ length = {} }}\n", length);
    }
    double length{0.0};
};

int main() {
    std::vector<some<Drawable>> someDrawables;

    someDrawables.emplace_back (Circle{1.0});
    someDrawables.emplace_back (Square{2.0});

    for (auto const& drawable : someDrawables)
        drawable.draw (std::cout);
}
```

## <a name="compiler-req">Minimum Requirements for Compilers</a>

| Family | Minimum version | Required flags |
| ------ | --------------- | -------------- |
| clang  | 17.0.1          | -std=c++20     |
| gcc    | 13.3            | -std=c++20     |
| MSVC   | 19.36           | /std:c++20     |

## Using the library step by step

## Related projects

- [Dyno: Runtime polymorphism done right](https://github.com/ldionne/dyno)
- [Proxy: Next Generation Polymorphism in C++](https://github.com/microsoft/proxy)
