cmake_minimum_required(VERSION 3.24)

project(Traits VERSION 0.0.1 LANGUAGES CXX)

add_library(traits INTERFACE)
add_library(Traits::traits ALIAS traits)

target_compile_features(traits INTERFACE cxx_std_20)
target_sources(traits INTERFACE
  FILE_SET HEADERS
  BASE_DIRS "${CMAKE_CURRENT_SOURCE_DIR}/include"
  FILES "${CMAKE_CURRENT_SOURCE_DIR}/include/traits.h"
  )

if(NOT CMAKE_CURRENT_SOURCE_DIR STREQUAL CMAKE_SOURCE_DIR)
    return()
endif()

option(TRAITS_BUILD_EXAMPLES "whether or not examples should be built" ON)
option(TRAITS_BUILD_TESTS "whether or not tests should be built" ON)

if(TRAITS_BUILD_EXAMPLES)
    add_subdirectory(examples)
endif()

if(TRAITS_BUILD_TESTS)
    enable_testing()
    add_subdirectory(tests)
endif()

# installation rules

configure_file("cmake/traits-config-version.cmake.in" "${CMAKE_CURRENT_BINARY_DIR}/traits-config-version.cmake" @ONLY)

install(TARGETS traits EXPORT traits-targets FILE_SET HEADERS DESTINATION "include/traits-${PROJECT_VERSION}")

install(EXPORT traits-targets NAMESPACE Traits:: FILE "traits-targets.cmake" DESTINATION "lib/cmake/traits-${PROJECT_VERSION}")
install(FILES "cmake/traits-config.cmake" "${CMAKE_CURRENT_BINARY_DIR}/traits-config-version.cmake" DESTINATION "lib/cmake/traits-${PROJECT_VERSION}")

# downloadable package for fetch content

set (package_name "${PROJECT_NAME}.zip")
add_custom_command(
    OUTPUT "${CMAKE_CURRENT_BINARY_DIR}/${package_name}"
    COMMAND ${CMAKE_COMMAND} -E tar c "${CMAKE_CURRENT_BINARY_DIR}/${package_name}" --format=zip --
    cmake/ examples/ include/ tests/ CMakeLists.txt LICENSE README.md
    WORKING_DIRECTORY "${CMAKE_CURRENT_SOURCE_DIR}"
    )
add_custom_command(
    OUTPUT "${CMAKE_CURRENT_BINARY_DIR}/package_name"
    COMMAND ${CMAKE_COMMAND} -E echo "name=${package_name}" > "${CMAKE_CURRENT_BINARY_DIR}/package_name"
    )
add_custom_target(package DEPENDS "${CMAKE_CURRENT_BINARY_DIR}/${package_name}" "${CMAKE_CURRENT_BINARY_DIR}/package_name")
