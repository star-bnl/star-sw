#------------------------------------------------
# The Virtual Monte Carlo packages
# Copyright (C) 2014 Ivana Hrivnacova
# All rights reserved.
#
# For the licensing terms see geant4_vmc/LICENSE.
# Contact: root-vmc@cern.ch
#-------------------------------------------------

# Configuration file for CMake build for Geant3 VMC package
# which defines customised installation.
#
# I. Hrivnacova, 13/06/2014

# Copy the custom cmake modules into the build tree
foreach(_mod CMakeMacroParseArguments UseVMC UseMC VMCBuildMode VMCInstallLibDir)
  configure_file(
    ${PROJECT_SOURCE_DIR}/cmake/${_mod}.cmake
    ${PROJECT_BINARY_DIR}/Modules/${_mod}.cmake
    COPYONLY
  )
endforeach()

# Find modules
foreach(_find_mod Garfield Pythia6 ROOT VMC MC)
  configure_file(
    ${PROJECT_SOURCE_DIR}/cmake/Find${_find_mod}.cmake
    ${PROJECT_BINARY_DIR}/Modules/Find${_find_mod}.cmake
    COPYONLY
  )
endforeach()

# Set needed variables for the install tree
set(Geant3_CMAKE_DIR ${CMAKE_INSTALL_PREFIX}/cmake)

# Install the custom modules for the examples
install(DIRECTORY
  ${PROJECT_BINARY_DIR}/Modules/
  DESTINATION ${CMAKE_INSTALL_LIBDIR}/Geant3-${Geant3_VERSION}/Modules
  COMPONENT Development
)

# Install examples
if(Geant3_INSTALL_DATA)
  install(DIRECTORY
    ${PROJECT_SOURCE_DIR}/data
    DESTINATION ${CMAKE_INSTALL_PREFIX}/share/Geant3-${Geant3_VERSION}
  )
endif()

#
# Install the Geant3Config, Geant3ConfigVersion
#
configure_file(
  "${PROJECT_SOURCE_DIR}/cmake/Geant3Config.cmake.in"
  "${PROJECT_BINARY_DIR}/Geant3Config.cmake" @ONLY)

configure_file(
  "${PROJECT_SOURCE_DIR}/cmake/Geant3ConfigVersion.cmake.in"
  "${PROJECT_BINARY_DIR}/Geant3ConfigVersion.cmake" @ONLY)

install(FILES
  "${PROJECT_BINARY_DIR}/Geant3Config.cmake"
  "${PROJECT_BINARY_DIR}/Geant3ConfigVersion.cmake"
  DESTINATION ${CMAKE_INSTALL_LIBDIR}/Geant3-${Geant3_VERSION})

install(EXPORT Geant3Targets
  DESTINATION ${CMAKE_INSTALL_LIBDIR}/Geant3-${Geant3_VERSION})
