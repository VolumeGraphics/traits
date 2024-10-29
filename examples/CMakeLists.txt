cmake_minimum_required(VERSION 3.24)

project(TraitsExamples VERSION 0.0.1 LANGUAGES CXX)

if(CMAKE_CURRENT_SOURCE_DIR STREQUAL CMAKE_SOURCE_DIR)
    include(FetchContent)

    FetchContent_Declare(traits GIT_REPOSITORY https://github.com/VolumeGraphics/traits.git GIT_TAG main)
    FetchContent_MakeAvailable(traits)
endif()

enable_testing()

set (EXAMPLES quickstart readme)

foreach(EXAMPLE IN LISTS EXAMPLES)
  add_executable(${EXAMPLE} "${EXAMPLE}.cpp")
  target_link_libraries(${EXAMPLE} PRIVATE Traits::traits)

  add_test(NAME ${EXAMPLE} COMMAND $<TARGET_FILE:${EXAMPLE}>)
endforeach()
