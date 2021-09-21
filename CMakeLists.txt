cmake_minimum_required(VERSION 3.14)
project(XLisp
    VERSION 3.3
    DESCRIPTION "XLISP - an object-oriented LISP"
    LANGUAGES C)

add_library(xlisp "")
add_subdirectory(src)

add_library(ext SHARED "")
add_subdirectory(ext)

add_executable(xlisp-repl)
add_subdirectory(xlisp)

include(CheckIPOSupported)
check_ipo_supported(RESULT can_link_time_optimize)
if(can_link_time_optimize)
    set_target_properties(xlisp PROPERTIES INTERPROCEDURAL_OPTIMIZATION TRUE)
    set_target_properties(ext PROPERTIES INTERPROCEDURAL_OPTIMIZATION TRUE)
    set_target_properties(xlisp-repl PROPERTIES INTERPROCEDURAL_OPTIMIZATION TRUE)
endif()

#---------------------------------installation---------------------------------#
# if(NOT WIN32 AND CMAKE_SOURCE_DIR STREQUAL CMAKE_CURRENT_SOURCE_DIR)
#     set(CMAKE_INSTALL_PREFIX "/opt/${PROJECT_NAME}")
# endif()

# The '.' ends up being relative to CMAKE_INSTALL_PREFIX.
install(TARGETS
    xlisp-repl ext
    COMPONENT xlisp_component
    RUNTIME DESTINATION "./bin"
    LIBRARY DESTINATION "./lib"
    PUBLIC_HEADER DESTINATION "./include")

install(FILES
    api.doc crec.doc xlisp.doc
    COMPONENT xlisp_component
    DESTINATION "./doc")

install(FILES
    clisp.lsp
    compile.lsp
    crec.lsp
    env.lsp
    fact.lsp
    fasl.lsp
    macros.lsp
    math.lsp
    obj.lsp
    objects.lsp
    pp.lsp
    qquote.lsp
    xlinit.lsp
    xlisp.lsp
    COMPONENT xlisp_component
    DESTINATION "./bin")

#----------------------------------packaging-----------------------------------#
set(CPACK_VERBATIM_VARIABLES YES)
set(CPACK_RESOURCE_FILE_LICENSE ${CMAKE_CURRENT_LIST_DIR}/LICENSE.txt)
set(CPACK_PACKAGE_FILE_README ${CMAKE_CURRENT_LIST_DIR}/README.txt)
set(CPACK_SOURCE_IGNORE_FILES
      /\\.git/
      \\.swp
      \\.orig
      /CMakeLists\\.txt\\.user
      /privateDir/)

if(WIN32)
    set(CPACK_GENERATOR ZIP WIX)
elseif(APPLE)
    set(CPACK_GENERATOR TGZ productbuild)
elseif(CMAKE_SYSTEM_NAME STREQUAL "Linux")
    set(CPACK_GENERATOR TGZ RPM)
else()
    set(CPACK_GENERATOR TGZ)
endif()

# `make package`
include(CPack)
