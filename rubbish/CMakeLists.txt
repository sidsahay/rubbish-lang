# CMakeList.txt : CMake project for rubbish, include source and define
# project specific logic here.
#
cmake_minimum_required (VERSION 3.8)


execute_process(WORKING_DIRECTORY ${CMAKE_SOURCE_DIR}/rubbish COMMAND lex lexer.l)
execute_process(WORKING_DIRECTORY ${CMAKE_SOURCE_DIR}/rubbish COMMAND yacc -d parser.y)
# Add source to this project's executable.
add_executable (rubbish main.c)


set_target_properties(rubbish
    PROPERTIES
    ARCHIVE_OUTPUT_DIRECTORY_RELEASE "${CMAKE_BINARY_DIR}/build_output/lib"
    LIBRARY_OUTPUT_DIRECTORY_RELEASE "${CMAKE_BINARY_DIR}/build_output/lib"
    RUNTIME_OUTPUT_DIRECTORY_RELEASE "${CMAKE_BINARY_DIR}/build_output"
)

# TODO: Add tests and install targets if needed.
