cmake_minimum_required(VERSION 3.24)
project(fetch CXX)

if(NOT DEFINED PACKAGE_PATH)
  message(FATAL_ERROR "PACKAGE_PATH is required. Please set -DPACKAGE_PATH=/path/to/package.zip")
endif()

if(NOT EXISTS "${PACKAGE_PATH}")
  message(FATAL_ERROR "Package file not found at: ${PACKAGE_PATH}")
endif()

include(FetchContent)
FetchContent_Declare(traits URL "${PACKAGE_PATH}")
FetchContent_MakeAvailable(traits)

add_executable(main main.cpp)
target_link_libraries(main Traits::traits)
