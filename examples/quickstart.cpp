#include <algorithm>
#include <format>
#include <iostream>
#include <string>
#include <vector>

#include "traits.h"
using namespace traits;

constexpr auto Drawable = trait{TRAITS_METHOD(draw, void(std::ostream& stream) const)};

struct Circle {
    double radius{0.0};
};

constexpr auto get(impl_for<Drawable, Circle>) {
    return "draw"_method = [](Circle const& circle, std::ostream& stream) {
        stream << std::format("Circle {{ radius = {} }}\n", circle.radius);
    };
}

struct Square {
    double length{0.0};
};

constexpr auto get(impl_for<Drawable, Square>) {
    return "draw"_method = [](Square const& square, std::ostream& stream) {
        stream << std::format("Square {{ length = {} }}\n", square.length);
    };
}

auto main() -> int {
    std::vector<some<Drawable>> someDrawables;

    someDrawables.emplace_back(Circle{1.0});
    someDrawables.emplace_back(Square{2.0});

    for (auto const& drawable : someDrawables) drawable.draw(std::cout);
}
