cmake_minimum_required (VERSION 3.0)
project(cello)

find_package(LLVM REQUIRED CONFIG)
message(STATUS "Found LLVM ${LLVM_PACKAGE_VERSION}")
message(STATUS "Using LLVMConfig.cmake in: ${LLVM_DIR}")

set (CMAKE_C_COMPILER gcc)
set (CMAKE_CXX_COMPILER g++)

set (root-dir ${CMAKE_CURRENT_SOURCE_DIR})
set (src-dir ${root-dir}/src)

set (cello-srcs
  ${src-dir}/main.cpp
  ${src-dir}/lexer.cpp
  ${src-dir}/ast_type_ident.cpp
  ${src-dir}/ast_expr.cpp
  ${src-dir}/ast_control.cpp
  ${src-dir}/ast_function.cpp
  ${src-dir}/scope.cpp
  ${src-dir}/error.cpp
  ${src-dir}/type.cpp
  ${src-dir}/builtin_types.cpp
  )

add_executable(cello ${cello-srcs})

target_include_directories(cello PUBLIC ${LLVM_INCLUDE_DIRS})
target_include_directories(cello PUBLIC ${root-dir}/deps/variant/include)
target_include_directories(cello PUBLIC ${root-dir}/deps/string-view-lite/include)
target_include_directories(cello PUBLIC ${root-dir}/deps/optional-lite/include)

add_definitions(${LLVM_DEFINITIONS})

# Find the libraries that correspond to the LLVM components
# that we wish to use
llvm_map_components_to_libnames(llvm_libs support core irreader target all)

# Link against LLVM libraries
target_link_libraries(cello ${llvm_libs})

if ("${CMAKE_CXX_COMPILER_ID}" STREQUAL "GNU")
	link_directories(/usr/local/lib)
  set (CMAKE_CXX_FLAGS "--std=c++14 -Wall -Werror -g -O3")
endif()

message(${CMAKE_CXX_COMPILER_ID})
if ("${CMAKE_CXX_COMPILER_ID}" STREQUAL "Clang" OR "${CMAKE_CXX_COMPILER_ID}" STREQUAL "AppleClang")
	link_directories(/usr/local/lib)
  set (CMAKE_CXX_FLAGS "-std=c++14 -Wall -Werror -g -O3")
endif()
