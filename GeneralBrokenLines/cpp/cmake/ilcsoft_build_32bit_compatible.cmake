#---------------- 32/64 bit issues ---------------------------------------
OPTION( BUILD_32BIT_COMPATIBLE "Set to ON to build in 32 bit compatibility mode" ON )

IF( BUILD_32BIT_COMPATIBLE )

    IF( CMAKE_SIZEOF_VOID_P EQUAL 8 ) # 64 bit architecture detected

        IF( COMMAND SET_PROPERTY )
            SET_PROPERTY(GLOBAL PROPERTY FIND_LIBRARY_USE_LIB64_PATHS 0)
        ELSE()
            MESSAGE( "Command SET_PROPERTY not found. do you have cmake >= 2.6 ?" )
        ENDIF()

        SET( CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -m32" )
        SET( CMAKE_C_FLAGS "${CMAKE_C_FLAGS} -m32" )
        SET( CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -m32" )
        SET( CMAKE_EXE_LINKER_FLAGS "${CMAKE_EXE_LINKER_FLAGS} -m32" )
        SET( CMAKE_SHARED_LINKER_FLAGS "${CMAKE_SHARED_LINKER_FLAGS} -m32" )
        SET( CMAKE_MODULE_LINKER_FLAGS "${CMAKE_MODULE_LINKER_FLAGS} -m32" )

    ENDIF()

ENDIF()
#MESSAGE( STATUS "BUILD_32BIT_COMPATIBLE = ${BUILD_32BIT_COMPATIBLE}" )
#-------------------------------------------------------------------------

