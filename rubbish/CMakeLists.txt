# CMakeList.txt : CMake project for rubbish, include source and define
# project specific logic here.
#
cmake_minimum_required (VERSION 3.8)

# Add source to this project's executable.
add_executable (rubbish "rubbish.cpp" "rubbish.h" "defs.cpp" "defs.h" "parser.cpp" "parser.h")

# TODO: Add tests and install targets if needed.
