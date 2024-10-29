include(FetchContent)
include(GoogleTest)

FetchContent_Declare(
    googletest
    URL https://github.com/google/googletest/archive/refs/tags/v1.15.2.zip
    )

set(gtest_force_shared_crt ON CACHE BOOL "" FORCE)
FetchContent_MakeAvailable(googletest)

enable_testing()

set (TESTS
    function_type
    )

foreach(TEST IN LISTS TESTS)
    add_executable(${TEST} ${TEST}.cpp)
    target_link_libraries(${TEST}
        PRIVATE Traits::traits
        PRIVATE GTest::gtest_main
        )

    gtest_discover_tests(${TEST})
endforeach()

# Add the deployment test, which checks that we can indeed install traits and
# then use `find_package` to depend on it from another CMake project.
include(ExternalProject)
set(TRAITS_FAKE_INSTALL_DIR "${PROJECT_BINARY_DIR}/fakeroot")
ExternalProject_Add(test.deploy.fakeroot
  SOURCE_DIR "${PROJECT_SOURCE_DIR}"
  EXCLUDE_FROM_ALL TRUE
  BUILD_ALWAYS TRUE
  INSTALL_DIR "${TRAITS_FAKE_INSTALL_DIR}"
  CMAKE_ARGS -DCMAKE_INSTALL_PREFIX=<INSTALL_DIR>
             -DCMAKE_CXX_COMPILER=${CMAKE_CXX_COMPILER}
             -DCMAKE_PREFIX_PATH=${CMAKE_PREFIX_PATH}
             -DCMAKE_TOOLCHAIN_FILE=${CMAKE_TOOLCHAIN_FILE}
  TEST_COMMAND ""      # Disable test step
  UPDATE_COMMAND ""    # Disable source work-tree update
)

add_custom_target(test.deploy
  DEPENDS test.deploy.fakeroot
  COMMAND ${CMAKE_COMMAND} -E remove_directory "${CMAKE_CURRENT_BINARY_DIR}/deploy"
  COMMAND ${CMAKE_COMMAND} -E make_directory "${CMAKE_CURRENT_BINARY_DIR}/deploy"
  COMMAND ${CMAKE_COMMAND} -E chdir "${CMAKE_CURRENT_BINARY_DIR}/deploy"
            ${CMAKE_COMMAND} "${CMAKE_CURRENT_SOURCE_DIR}/deploy"
                             -DCMAKE_CXX_COMPILER="${CMAKE_CXX_COMPILER}"
                             -DCMAKE_PREFIX_PATH="${CMAKE_PREFIX_PATH}\;${TRAITS_FAKE_INSTALL_DIR}"
                             -DCMAKE_TOOLCHAIN_FILE=${CMAKE_TOOLCHAIN_FILE}
  COMMAND ${CMAKE_COMMAND} --build "${CMAKE_CURRENT_BINARY_DIR}/deploy"
)
add_test(NAME test.deploy COMMAND ${CMAKE_COMMAND} --build "${PROJECT_BINARY_DIR}" --target test.deploy)
