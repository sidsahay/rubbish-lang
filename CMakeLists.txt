# CMakeList.txt : Top-level CMake project file, do global configuration
# and include sub-projects here.
#
cmake_minimum_required (VERSION 3.8)

project ("rubbish")

#Run stack setup to build stuff
execute_process(WORKING_DIRECTORY ${CMAKE_SOURCE_DIR}/rubbishc COMMAND stack setup)

# Include sub-projects.
add_subdirectory ("rubbish")
add_subdirectory ("rubbishc")