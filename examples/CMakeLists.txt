cmake_minimum_required(VERSION 3.24)

project(
  TraitsExamples
  VERSION 0.0.1
  LANGUAGES CXX)

if(CMAKE_CURRENT_SOURCE_DIR STREQUAL CMAKE_SOURCE_DIR)
  include(FetchContent)

  FetchContent_Declare(
    traits
    GIT_REPOSITORY https://github.com/VolumeGraphics/traits.git
    GIT_TAG main)
  FetchContent_MakeAvailable(traits)
endif()

enable_testing()

list(APPEND examples quickstart readme)

foreach(example IN LISTS examples)
  add_executable(${example} "${example}.cpp")
  target_link_libraries(${example} PRIVATE Traits::traits)

  add_test(NAME ${example} COMMAND $<TARGET_FILE:${example}>)
endforeach()
