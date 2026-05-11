ADD_LIBRARY (GeantCrmc SHARED  G4HadronPhysicsCRMC.cc G4HadronicInelasticModelCRMC.cc)

TARGET_compile_definitions (GeantCrmc PRIVATE ${Geant4_DEFINITIONS})
TARGET_include_directories (GeantCrmc PRIVATE ${Geant4_INCLUDE_DIRS})
TARGET_LINK_LIBRARIES (GeantCrmc ${Geant4_LIBRARIES})

INSTALL (
  TARGETS GeantCrmc
  RUNTIME DESTINATION bin
  LIBRARY DESTINATION lib
  ARCHIVE DESTINATION lib/static
  )

INSTALL (
  FILES G4HadronPhysicsCRMC.hh G4HadronicInelasticModelCRMC.hh
  DESTINATION src
  )
