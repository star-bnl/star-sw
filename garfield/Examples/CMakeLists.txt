########################################################
# @author Klaus Zenker
CMAKE_MINIMUM_REQUIRED(VERSION 2.6 FATAL_ERROR)
########################################################
PROJECT(GarfieldExamples)

#### GasFile               #############################
add_executable(gasfile GasFile/gasfile.C)
target_link_libraries(gasfile Garfield ${ROOT_LIBRARIES} ${ROOT_COMPONENT_LIBRARIES} )

#### Heed                   ############################
add_executable(edep Heed/edep.C)
target_link_libraries(edep Garfield ${ROOT_LIBRARIES} ${ROOT_COMPONENT_LIBRARIES} )

#### Geat4GarfieldInterface ############################
option(G4ANALYSIS_USE "use ROOT" ON)
SET(CMAKE_MODULE_PATH /afs/cern.ch/sw/lcg/external/geant4/10.1/share/cmake/Modules)

#----------------------------------------------------------------------------
# Find Geant4 package, activating all available UI and Vis drivers by default
# You can set WITH_GEANT4_UIVIS to OFF via the command line or ccmake/cmake-gui
# to build a batch mode only executable
#
option(WITH_GEANT4_UIVIS "Build example with Geant4 UI and Vis drivers" ON)
if(WITH_GEANT4_UIVIS)
  find_package(Geant4 COMPONENTS ui_all vis_all)
else()
  find_package(Geant4 )
endif()
if(Geant4_FOUND)
#----------------------------------------------------------------------------
# Setup Geant4 include directories and compile definitions
# Setup include directory for this project
#
include(${Geant4_USE_FILE})
SET(G4_DIR Geant4GarfieldInterface )
include_directories( ${G4_DIR}/include )
file(GLOB sources ${G4_DIR}/src/*.cc)
file(GLOB headers ${G4_DIR}/include/*.hh)
file(GLOB headers ${garfield_SOURCE_DIR}/Include/*.hh)

#----------------------------------------------------------------------------
# Add the executable, and link it to the Geant4 libraries
#
add_executable(exampleGeant4Interface ${G4_DIR}/exampleGarfield.cc ${sources} ${headers})
target_link_libraries(exampleGeant4Interface ${Geant4_LIBRARIES} Garfield ${ROOT_COMPONENT_LIBRARIES} ${ROOT_LIBRARIES})

#----------------------------------------------------------------------------
# Copy all scripts to the build directory, i.e. the directory in which we
# build exampleGarfield. This is so that we can run the executable directly because it
# relies on these scripts being in the current working directory.
#
set(EXAMPLEGARFIELD_SCRIPTS
  icons.mac
  gui.mac
  physics.mac
  init_vis.mac
  run.mac
  vis.mac
  ar_70_co2_30_1000mbar.gas
  )
foreach(_script ${EXAMPLEGARFIELD_SCRIPTS})
  configure_file(
    ${G4_DIR}/${_script}
    ${PROJECT_BINARY_DIR}/../bin/${_script}
    COPYONLY
    )
  INSTALL(FILES ${G4_DIR}/${_script} DESTINATION ${CMAKE_INSTALL_PREFIX}/bin/)
endforeach()
endif(Geant4_FOUND)
#### Install                ############################
if(Geant4_FOUND)
INSTALL(TARGETS gasfile edep exampleGeant4Interface DESTINATION ${CMAKE_INSTALL_PREFIX}/bin/)
else()
INSTALL(TARGETS gasfile edep DESTINATION ${CMAKE_INSTALL_PREFIX}/bin/)
endif(Geant4_FOUND)
