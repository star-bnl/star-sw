INCLUDE_DIRECTORIES("${CMAKE_CURRENT_SOURCE_DIR}")
INCLUDE_DIRECTORIES("${CMAKE_CURRENT_BINARY_DIR}")
FILE(GLOB files
  ${CMAKE_CURRENT_SOURCE_DIR}/*.f
  ${CMAKE_CURRENT_BINARY_DIR}/*.f
  ${CMAKE_SOURCE_DIR}/src/lzma-read/*.cc)
set_source_files_properties(${base_files} PROPERTIES COMPILE_DEFINITIONS __QGSJETIII__)
#CHECK_CXX_COMPILER_FLAG("-std=c++17" COMPILER_CXX_MCMODEL)

IF (CRMC_STATIC)
  add_library(QgsjetIII STATIC ${files})
ELSE (CRMC_STATIC)
  add_library(QgsjetIII SHARED ${base_files} ${files})
  target_link_libraries(QgsjetIII CrmcBasic)
ENDIF (CRMC_STATIC)


INSTALL (TARGETS QgsjetIII
        RUNTIME DESTINATION bin
        LIBRARY DESTINATION lib
        ARCHIVE DESTINATION lib/static
        )
