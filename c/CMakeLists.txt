add_library(uselibcurl SHARED uselibcurl.c)

if(CMAKE_COMPILER_IS_GNUCXX)
    set(CMAKE_CXX_FLAGS "-O2")        ## Optimize
endif()
