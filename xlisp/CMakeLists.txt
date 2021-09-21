target_sources(xlisp-repl PRIVATE
    xlisp.c
    "${CMAKE_CURRENT_SOURCE_DIR}/../include/xlisp.h")

target_include_directories(xlisp-repl PUBLIC ${CMAKE_CURRENT_SOURCE_DIR}/../include)
target_link_libraries(xlisp-repl xlisp)
set_target_properties(xlisp-repl PROPERTIES
    PUBLIC_HEADER ${CMAKE_CURRENT_SOURCE_DIR}/../include/xlisp.h)
