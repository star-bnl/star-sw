
set ( fio_source_list    
			 HEPEVT_Wrapper.cc
			 HerwigWrapper.cc
			 IO_HEPEVT.cc
			 IO_HERWIG.cc
			  )

# this library is meant to interact with Fortran and needs extra flags
    if(APPLE)
      set( CMAKE_SHARED_LIBRARY_CREATE_CXX_FLAGS 
           "${CMAKE_SHARED_LIBRARY_CREATE_CXX_FLAGS} -Wl,-undefined -Wl,dynamic_lookup -Wl,-single_module")
    endif(APPLE)

# give up and only build static library for Windows
if( ${CMAKE_SYSTEM_NAME} MATCHES "Windows" )
      set( CMAKE_SHARED_LIBRARY_CREATE_CXX_FLAGS 
           "${CMAKE_SHARED_LIBRARY_CREATE_CXX_FLAGS} /ALLOWISOLATION:NO /FORCE:UNRESOLVED")
else()
ADD_LIBRARY (HepMCfio  SHARED ${fio_source_list})
SET_TARGET_PROPERTIES (HepMCfio  PROPERTIES OUTPUT_NAME HepMCfio )
SET_TARGET_PROPERTIES (HepMCfio  PROPERTIES VERSION 4.0.0 SOVERSION 4 )
SET_TARGET_PROPERTIES(HepMCfio  PROPERTIES CLEAN_DIRECT_OUTPUT 1)
INSTALL (TARGETS HepMCfio 
    RUNTIME DESTINATION bin
    LIBRARY DESTINATION lib
    ARCHIVE DESTINATION lib
    ) 
endif()

ADD_LIBRARY (HepMCfioS STATIC ${fio_source_list})
SET_TARGET_PROPERTIES (HepMCfioS PROPERTIES OUTPUT_NAME HepMCfio )
SET_TARGET_PROPERTIES(HepMCfioS PROPERTIES CLEAN_DIRECT_OUTPUT 1)

INSTALL (TARGETS HepMCfioS
    RUNTIME DESTINATION bin
    LIBRARY DESTINATION lib
    ARCHIVE DESTINATION lib
    ) 
