#include <iostream>
#include <numbers>
#include <vector>

#include "traits.h"
using namespace traits;

struct Circle {
    double radius{0.0};
};

struct Square {
    double length{0.0};
};

constexpr auto Shape = trait{
    TRAITS_METHOD(area, double() const),
};

constexpr auto get(impl_for<Shape, Circle>) {
    return "area"_method = [](Circle const& circle) {
        return std::numbers::pi * circle.radius * circle.radius;
    };
}

constexpr auto get(impl_for<Shape, Square>) {
    return "area"_method = [](Square const& square) { return square.length * square.length; };
}

int main() {
    std::vector<some<Shape>> someShapes;

    someShapes.emplace_back(Circle{1.0});
    someShapes.emplace_back(Square{1.0});

    for (auto const& shape : someShapes)
        std::cout << "Shape with area " << shape.area() << "\n";
}
