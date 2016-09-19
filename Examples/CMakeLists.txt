########################################################
# @author Klaus Zenker
CMAKE_MINIMUM_REQUIRED(VERSION 2.6 FATAL_ERROR)
########################################################
PROJECT(GarfieldExamples)

add_executable(gasfile GasFile/gasfile.C)
target_link_libraries(gasfile Garfield ${ROOT_LIBRARIES} ${ROOT_COMPONENT_LIBRARIES} )

add_executable(edep Heed/edep.C)
target_link_libraries(edep Garfield ${ROOT_LIBRARIES} ${ROOT_COMPONENT_LIBRARIES} )

INSTALL(TARGETS gasfile edep DESTINATION ${CMAKE_INSTALL_PREFIX}/bin/)