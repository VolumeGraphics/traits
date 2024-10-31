add_custom_target(examples
  COMMAND ${CMAKE_CTEST_COMMAND} --output-on-failure
  COMMENT "Build and run all the examples."
  USES_TERMINAL)
add_dependencies(check examples)

add_executable(readme EXCLUDE_FROM_ALL "readme.cpp")
target_link_libraries(readme PRIVATE Traits::traits)

add_test(readme readme)
add_dependencies(examples readme)
