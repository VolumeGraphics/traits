cmake_minimum_required(VERSION 3.12)

project(Traits VERSION 0.0.1 LANGUAGES CXX)

add_library(traits INTERFACE)
add_library(Traits::traits ALIAS traits)
target_compile_features(traits INTERFACE cxx_std_20)
target_include_directories(traits INTERFACE "$<BUILD_INTERFACE:${CMAKE_CURRENT_SOURCE_DIR}/include>")

if(CMAKE_CURRENT_SOURCE_DIR STREQUAL CMAKE_SOURCE_DIR)
 
    add_custom_target(check COMMENT "Build and run all the tests and examples.")

    enable_testing()
    add_subdirectory(example)
#    add_subdirectory(test)

endif()
