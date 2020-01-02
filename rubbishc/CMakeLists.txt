add_custom_target(rubbishc ALL WORKING_DIRECTORY ${CMAKE_SOURCE_DIR}/rubbishc COMMAND stack build && stack install --local-bin-path ${CMAKE_BINARY_DIR}/build_output)
