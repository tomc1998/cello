cmake_minimum_required (VERSION 3.0)
project(cello)

set (CMAKE_C_COMPILER gcc)
set (CMAKE_CXX_COMPILER g++)

set (root-dir ${CMAKE_CURRENT_SOURCE_DIR})
set (src-dir ${root-dir}/src)

set (cello-srcs
  ${src-dir}/main.cpp
  ${src-dir}/lexer.cpp
  )

add_executable(cello ${cello-srcs})
target_include_directories(cello PUBLIC ${root-dir}/deps/variant/include)
target_include_directories(cello PUBLIC ${root-dir}/deps/string-view-lite/include)
target_include_directories(cello PUBLIC ${root-dir}/deps/optional-lite/include)

if ("${CMAKE_CXX_COMPILER_ID}" STREQUAL "GNU")
	link_directories(/usr/local/lib)
  set (CMAKE_CXX_FLAGS "--std=c++14 -Wall -Werror -g")
endif()

message(${CMAKE_CXX_COMPILER_ID})
if ("${CMAKE_CXX_COMPILER_ID}" STREQUAL "Clang" OR "${CMAKE_CXX_COMPILER_ID}" STREQUAL "AppleClang")
	link_directories(/usr/local/lib)
  set (CMAKE_CXX_FLAGS "-std=c++14 -Wall -Werror -g")
endif()
