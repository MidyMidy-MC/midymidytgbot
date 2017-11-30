add_library(uselibcurl SHARED uselibcurl.c)
target_link_libraries(uselibcurl curl)

set(CMAKE_C_FLAGS "-O2 -ldl")        ## Optimize
