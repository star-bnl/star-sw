#------------------------------------------------
# The Virtual Monte Carlo packages
# Copyright (C) 2014 Ivana Hrivnacova
# All rights reserved.
#
# For the licensing terms see geant4_vmc/LICENSE.
# Contact: root-vmc@cern.ch
#-------------------------------------------------

# Configuration file for CMake build for Geant3 with VMC package.
#
# I. Hrivnacova, 13/11/2014

set(CPACK_PACKAGE_VERSION_MAJOR ${${PROJECT_NAME}_VERSION_MAJOR})
set(CPACK_PACKAGE_VERSION_MINOR ${${PROJECT_NAME}_VERSION_MINOR})
set(CPACK_PACKAGE_VERSION_PATCH ${${PROJECT_NAME}_VERSION_PATCH})
set(CPACK_SOURCE_GENERATOR "TGZ")
if(${${PROJECT_NAME}_VERSION_PATCH} EQUAL "0")
  set(CPACK_SOURCE_PACKAGE_FILE_NAME
    "geant3+_vmc.${CPACK_PACKAGE_VERSION_MAJOR}.${CPACK_PACKAGE_VERSION_MINOR}")
else()
  set(CPACK_SOURCE_PACKAGE_FILE_NAME
    "geant3+_vmc.${CPACK_PACKAGE_VERSION_MAJOR}.${CPACK_PACKAGE_VERSION_MINOR}.p${CPACK_PACKAGE_VERSION_PATCH}")
endif()      
# Do not use CPACK_SOURCE_IGNORE_FILES as this would exclude test scripts
# and log_ref directory
#set(CPACK_SOURCE_IGNORE_FILES
#  "/build/;/.bzr/;~$;${CPACK_SOURCE_IGNORE_FILES}")
include(CPack)
