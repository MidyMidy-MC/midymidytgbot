cmake_minimum_required(VERSION 3.9)

add_library(uselibcurl SHARED uselibcurl.c)
target_link_libraries(uselibcurl curl)

add_executable(test_uselibcurl test.c)

set(CMAKE_C_FLAGS "-O2 -ldl")        ## Optimize
