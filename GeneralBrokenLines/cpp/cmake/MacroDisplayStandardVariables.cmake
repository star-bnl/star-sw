
# helper macro to display standard cmake variables and force write to cache
# otherwise outdated values may appear in ccmake gui
MACRO( DISPLAY_STD_VARIABLES )
    MESSAGE( STATUS )
    MESSAGE( STATUS "-------------------------------------------------------------------------------" )
    MESSAGE( STATUS "Change values with: cmake -D<Variable>=<Value>" )

    IF( DEFINED CMAKE_INSTALL_PREFIX )
        MESSAGE( STATUS "CMAKE_INSTALL_PREFIX = ${CMAKE_INSTALL_PREFIX}" )
    ENDIF()


    IF( DEFINED CMAKE_BUILD_TYPE )
        MESSAGE( STATUS "CMAKE_BUILD_TYPE = ${CMAKE_BUILD_TYPE}" )
    ENDIF()

    IF( DEFINED BUILD_SHARED_LIBS )
        MESSAGE( STATUS "BUILD_SHARED_LIBS = ${BUILD_SHARED_LIBS}" )
    ENDIF()

    IF( DEFINED BUILD_TESTING )
        MESSAGE( STATUS "BUILD_TESTING = ${BUILD_TESTING}" )
    ENDIF()

    IF( DEFINED INSTALL_DOC )
        MESSAGE( STATUS "INSTALL_DOC = ${INSTALL_DOC}" )
    ENDIF()

    IF( DEFINED CMAKE_PREFIX_PATH )
        LIST( REMOVE_DUPLICATES CMAKE_PREFIX_PATH )
        #MESSAGE( STATUS "CMAKE_PREFIX_PATH = ${CMAKE_PREFIX_PATH}" )
        MESSAGE( STATUS "CMAKE_PREFIX_PATH =" )
        FOREACH( _path ${CMAKE_PREFIX_PATH} )
            MESSAGE( STATUS "   ${_path};" )
        ENDFOREACH()
        #SET( CMAKE_PREFIX_PATH "${CMAKE_PREFIX_PATH}" CACHE PATH "CMAKE_PREFIX_PATH" FORCE )
    ENDIF()

    IF( DEFINED CMAKE_MODULE_PATH )
        LIST( REMOVE_DUPLICATES CMAKE_MODULE_PATH )
        #MESSAGE( STATUS "CMAKE_MODULE_PATH = ${CMAKE_MODULE_PATH}" )
        MESSAGE( STATUS "CMAKE_MODULE_PATH =" )
        FOREACH( _path ${CMAKE_MODULE_PATH} )
            MESSAGE( STATUS "   ${_path};" )
        ENDFOREACH()
        SET( CMAKE_MODULE_PATH "${CMAKE_MODULE_PATH}" CACHE PATH "CMAKE_MODULE_PATH" FORCE )
    ENDIF()

    MESSAGE( STATUS "-------------------------------------------------------------------------------" )
    MESSAGE( STATUS )

ENDMACRO( DISPLAY_STD_VARIABLES )

