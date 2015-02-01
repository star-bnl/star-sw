MARK_AS_ADVANCED( CMAKE_BACKWARDS_COMPATIBILITY )
SET( CMAKE_ALLOW_LOOSE_LOOP_CONSTRUCTS TRUE ) # default in cmake 2.6

# DEPRECATED
#OPTION( BUILD_SHARED_LIBS "Set to OFF to build static libraries" ON )
#SET( BUILD_SHARED_LIBS "${BUILD_SHARED_LIBS}" CACHE BOOL "Set to OFF to build static libraries" FORCE )

# include helper macros
INCLUDE( MacroAddSharedLibrary )
INCLUDE( MacroDisplayStandardVariables )
INCLUDE( MacroGeneratePackageConfigFiles )


# include ilcsoft default settings
INCLUDE( ilcsoft_default_install_prefix )
INCLUDE( ilcsoft_default_build_type )
INCLUDE( ilcsoft_default_enable_ctest )
INCLUDE( ilcsoft_default_library_versioning )
INCLUDE( ilcsoft_default_build_output_directories )
INCLUDE( ilcsoft_default_rpath_settings )
#INCLUDE( ilcsoft_build_32bit_compatible )


# uninstall target may only be created once per project
# otherwise problems occur due to duplicate targets. even
# setting CMAKE_POLICY(SET CMP0002 OLD) causes an error
# calling 'make uninstall'
# e.g. streamlog in Marlin
IF( NOT _ilcsoft_default_settings_loaded )

    INCLUDE( ilcsoft_default_uninstall_target )

    SET( _ilcsoft_default_settings_loaded TRUE )

ENDIF()
