include(FetchContent)
include(GoogleTest)

FetchContent_Declare(googletest
                     URL https://github.com/google/googletest/archive/refs/tags/v1.15.2.zip)

set(gtest_force_shared_crt
    ON
    CACHE BOOL "" FORCE)
FetchContent_MakeAvailable(googletest)

enable_testing()

list(APPEND tests function_type)

foreach(test IN LISTS tests)
  add_executable(${test} ${test}.cpp)
  target_link_libraries(${test} PRIVATE Traits::traits GTest::gtest_main)

  gtest_discover_tests(${test})
endforeach()

# Add the deployment test target, which checks that we can indeed install traits and then use
# `find_package` to depend on it from another CMake project.

include(ExternalProject)
set(FAKE_INSTALL_DIR "${PROJECT_BINARY_DIR}/fakeroot")
ExternalProject_Add(
  test.deploy.fakeroot
  SOURCE_DIR "${PROJECT_SOURCE_DIR}"
  EXCLUDE_FROM_ALL TRUE
  BUILD_ALWAYS TRUE
  INSTALL_DIR "${FAKE_INSTALL_DIR}"
  CMAKE_ARGS -DCMAKE_INSTALL_PREFIX=<INSTALL_DIR> -DCMAKE_CXX_COMPILER=${CMAKE_CXX_COMPILER}
             -DCMAKE_PREFIX_PATH=${CMAKE_PREFIX_PATH} -DCMAKE_TOOLCHAIN_FILE=${CMAKE_TOOLCHAIN_FILE}
  TEST_COMMAND "" # Disable test step
  UPDATE_COMMAND "" # Disable source work-tree update
)

add_custom_target(
  test.deploy
  DEPENDS test.deploy.fakeroot
  COMMAND ${CMAKE_COMMAND} -E remove_directory "${CMAKE_CURRENT_BINARY_DIR}/deploy"
  COMMAND ${CMAKE_COMMAND} -E make_directory "${CMAKE_CURRENT_BINARY_DIR}/deploy"
  COMMAND
    ${CMAKE_COMMAND} -E chdir "${CMAKE_CURRENT_BINARY_DIR}/deploy" ${CMAKE_COMMAND}
    "${CMAKE_CURRENT_SOURCE_DIR}/deploy" -DCMAKE_CXX_COMPILER="${CMAKE_CXX_COMPILER}"
    -DCMAKE_PREFIX_PATH="${CMAKE_PREFIX_PATH}\;${FAKE_INSTALL_DIR}"
    -DCMAKE_TOOLCHAIN_FILE=${CMAKE_TOOLCHAIN_FILE}
  COMMAND ${CMAKE_COMMAND} --build "${CMAKE_CURRENT_BINARY_DIR}/deploy")

# Add the fetch test target, which checks that we can indeed use fetch content to depend on traits
# from another CMake project.

add_custom_target(
  test.fetch
  DEPENDS package
  COMMAND ${CMAKE_COMMAND} -E remove_directory "${CMAKE_CURRENT_BINARY_DIR}/fetch"
  COMMAND ${CMAKE_COMMAND} -E make_directory "${CMAKE_CURRENT_BINARY_DIR}/fetch"
  COMMAND
    ${CMAKE_COMMAND} -S "${CMAKE_CURRENT_SOURCE_DIR}/fetch" -B "${CMAKE_CURRENT_BINARY_DIR}/fetch"
    -DPACKAGE_PATH="${CMAKE_CURRENT_BINARY_DIR}/../${PACKAGE_NAME}"
    -DCMAKE_CXX_COMPILER="${CMAKE_CXX_COMPILER}" -DCMAKE_PREFIX_PATH="${CMAKE_PREFIX_PATH}"
    -DCMAKE_TOOLCHAIN_FILE="${CMAKE_TOOLCHAIN_FILE}"
  COMMAND ${CMAKE_COMMAND} --build "${CMAKE_CURRENT_BINARY_DIR}/fetch")

# Add the coverage target, which generates a coverage report

if(TRAITS_TEST_COVERAGE)
  find_program(LCOV lcov REQUIRED)
  find_program(PYTHON3 python3 REQUIRED)

  set(CMAKE_CXX_FLAGS "-O0 -coverage")

  add_custom_target(
    coverage
    COMMAND "${LCOV}" --capture --directory "${CMAKE_BINARY_DIR}" --exclude '/usr/*' --exclude
            '/*/gtest/*' --output-file "${CMAKE_BINARY_DIR}/coverage.info"
    COMMAND "${LCOV}" --list "${CMAKE_BINARY_DIR}/coverage.info"
    COMMAND
      wget -O "${CMAKE_BINARY_DIR}/lcov_cobertura.py"
      https://raw.github.com/eriwen/lcov-to-cobertura-xml/master/lcov_cobertura/lcov_cobertura.py
    COMMAND
      "${PYTHON3}" "${CMAKE_BINARY_DIR}/lcov_cobertura.py" "${CMAKE_BINARY_DIR}/coverage.info"
      --base-dir "${CMAKE_SOURCE_DIR}" --demangle --output
      "${CMAKE_BINARY_DIR}/coverage.cobertura.xml"
    WORKING_DIRECTORY "${CMAKE_BINARY_DIR}")
endif()
