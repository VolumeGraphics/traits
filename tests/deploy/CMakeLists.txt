cmake_minimum_required(VERSION 3.0)
project(deploy CXX)
find_package(traits REQUIRED)

add_executable(main main.cpp)
target_link_libraries(main Traits::traits)
