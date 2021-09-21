target_sources(ext PRIVATE
    ext.c
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/xlisp.h")

target_include_directories(ext PUBLIC ${CMAKE_CURRENT_SOURCE_DIR}/../include)
target_link_libraries(ext xlisp)
