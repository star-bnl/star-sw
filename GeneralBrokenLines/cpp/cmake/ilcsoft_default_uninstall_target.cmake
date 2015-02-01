GET_FILENAME_COMPONENT( _current_dir ${CMAKE_CURRENT_LIST_FILE} PATH )

IF( EXISTS "${_current_dir}/cmake_uninstall.cmake.in" )

    # create uninstall configuration file 
    CONFIGURE_FILE( "${_current_dir}/cmake_uninstall.cmake.in"
                    "${PROJECT_BINARY_DIR}/cmake_uninstall.cmake" @ONLY )

    # add uninstall target
    ADD_CUSTOM_TARGET( uninstall "${CMAKE_COMMAND}" -P "${PROJECT_BINARY_DIR}/cmake_uninstall.cmake" )

ENDIF( EXISTS "${_current_dir}/cmake_uninstall.cmake.in" )

