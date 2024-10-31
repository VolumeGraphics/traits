cmake_minimum_required(VERSION 3.12)

project(Traits VERSION 0.0.1 LANGUAGES CXX)

add_library(traits INTERFACE)
add_library(Traits::traits ALIAS traits)
target_compile_features(traits INTERFACE cxx_std_20)
target_include_directories(traits INTERFACE "$<BUILD_INTERFACE:${CMAKE_CURRENT_SOURCE_DIR}>")

if(CMAKE_CURRENT_SOURCE_DIR STREQUAL CMAKE_SOURCE_DIR)
 
#    option(TRAITS_BUILD_EXAMPLES "whether or not examples should be built" ON)
#    option(TRAITS_BUILD_TESTS    "whether or not tests should be built" ON)

#    if(TRAITS_BUILD_EXAMPLES)
#        add_subdirectory(example)
#    endif()

#    if(TRAITS_BUILD_TESTS)
#        enable_testing()
#        add_subdirectory(tests)
#    endif()

endif()
