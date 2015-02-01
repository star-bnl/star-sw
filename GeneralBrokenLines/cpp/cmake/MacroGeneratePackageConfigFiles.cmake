#GET_FILENAME_COMPONENT( _current_dir ${CMAKE_CURRENT_LIST_FILE} PATH )

# helper macro for generating project configuration file
MACRO( GENERATE_PACKAGE_CONFIGURATION_FILES )

    FOREACH( arg ${ARGN} )
        IF( ${arg} MATCHES "Config.cmake" )
            IF( EXISTS "${PROJECT_SOURCE_DIR}/cmake/${arg}.in" )
                CONFIGURE_FILE( "${PROJECT_SOURCE_DIR}/cmake/${arg}.in"
                                "${PROJECT_BINARY_DIR}/${arg}" @ONLY
                )
                INSTALL( FILES "${PROJECT_BINARY_DIR}/${arg}" DESTINATION . )
                #IF( EXISTS "${_current_dir}/MacroCheckPackageLibs.cmake" )
                #    INSTALL( FILES "${_current_dir}/MacroCheckPackageLibs.cmake" DESTINATION cmake )
                #ENDIF()
                #IF( EXISTS "${_current_dir}/MacroExportPackageDeps.cmake" )
                #    INSTALL( FILES "${_current_dir}/MacroExportPackageDeps.cmake" DESTINATION cmake )
                #ENDIF()
            ENDIF()
        ENDIF()


        IF( ${arg} MATCHES "ConfigVersion.cmake" )
            # version configuration file
            IF( EXISTS "${PROJECT_SOURCE_DIR}/cmake/${arg}.in" )
                CONFIGURE_FILE( "${PROJECT_SOURCE_DIR}/cmake/${arg}.in"
                                "${PROJECT_BINARY_DIR}/${arg}" @ONLY
                )
                INSTALL( FILES "${PROJECT_BINARY_DIR}/${arg}" DESTINATION . )
                #IF( EXISTS "${_current_dir}/MacroCheckPackageVersion.cmake" )
                #    INSTALL( FILES "${_current_dir}/MacroCheckPackageVersion.cmake" DESTINATION cmake )
                #ENDIF()
            ENDIF( EXISTS "${PROJECT_SOURCE_DIR}/cmake/${arg}.in" )
        ENDIF()

        IF( ${arg} MATCHES "LibDeps.cmake" )
            EXPORT_LIBRARY_DEPENDENCIES( "${arg}" )
            INSTALL( FILES "${PROJECT_BINARY_DIR}/${arg}" DESTINATION lib/cmake )
        ENDIF()

    ENDFOREACH()

ENDMACRO( GENERATE_PACKAGE_CONFIGURATION_FILES )

