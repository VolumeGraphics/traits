cmake_minimum_required(VERSION 3.24)

project(
  Traits
  VERSION 0.0.4
  LANGUAGES CXX)

add_library(traits INTERFACE)
add_library(Traits::traits ALIAS traits)

target_compile_features(traits INTERFACE cxx_std_20)
target_sources(traits INTERFACE FILE_SET HEADERS BASE_DIRS "${CMAKE_CURRENT_SOURCE_DIR}/include"
                                FILES "${CMAKE_CURRENT_SOURCE_DIR}/include/traits.h")

# only proceed if we are building the library directly

if(NOT CMAKE_PROJECT_NAME STREQUAL PROJECT_NAME)
  return()
endif()

option(TRAITS_BUILD_EXAMPLES "whether or not examples should be built" ON)
option(TRAITS_BUILD_TESTS "whether or not tests should be built" ON)
option(TRAITS_TEST_COVERAGE "whether or not test coverage should be generated" OFF)

# installation rules

configure_file("cmake/traits-config-version.cmake.in"
               "${CMAKE_CURRENT_BINARY_DIR}/traits-config-version.cmake" @ONLY)

install(
  TARGETS traits
  EXPORT traits-targets
  FILE_SET HEADERS
  DESTINATION "include/traits-${PROJECT_VERSION}")

install(
  EXPORT traits-targets
  NAMESPACE Traits::
  FILE "traits-targets.cmake"
  DESTINATION "lib/cmake/traits-${PROJECT_VERSION}")
install(FILES "cmake/traits-config.cmake" "${CMAKE_CURRENT_BINARY_DIR}/traits-config-version.cmake"
        DESTINATION "lib/cmake/traits-${PROJECT_VERSION}")

# downloadable package for fetch content

set(PACKAGE_NAME "traits.zip")

add_custom_command(
  OUTPUT "${CMAKE_CURRENT_BINARY_DIR}/${PACKAGE_NAME}"
  COMMAND ${CMAKE_COMMAND} -E tar c "${CMAKE_CURRENT_BINARY_DIR}/${PACKAGE_NAME}" --format=zip --
          cmake/ examples/ include/ tests/ CMakeLists.txt LICENSE README.md
  WORKING_DIRECTORY "${CMAKE_CURRENT_SOURCE_DIR}")
add_custom_target(package DEPENDS "${CMAKE_CURRENT_BINARY_DIR}/${PACKAGE_NAME}")

# create a release (by pushing a tag to the repository)

add_custom_target(
  release
  WORKING_DIRECTORY "${CMAKE_CURRENT_SOURCE_DIR}"
  COMMAND bash release.sh "${CMAKE_PROJECT_VERSION}")

# optional examples

if(TRAITS_BUILD_EXAMPLES)
  add_subdirectory(examples)
endif()

# optional tests

if(TRAITS_BUILD_TESTS)
  enable_testing()
  add_subdirectory(tests)
endif()
