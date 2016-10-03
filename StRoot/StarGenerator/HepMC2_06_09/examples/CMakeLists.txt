
set( example_code 
		  example_BuildEventFromScratch.cc
		  example_EventSelection.cc
		  example_UsingIterators.cc
		  example_UsingIterators.txt
		  example_VectorConversion.cc
		  VectorConversion.h
		    )

CONFIGURE_FILE( ${CMAKE_CURRENT_SOURCE_DIR}/GNUmakefile.example.in 
                ${CMAKE_CURRENT_BINARY_DIR}/GNUmakefile  @ONLY )

INSTALL (FILES ${example_code}  
               ${CMAKE_CURRENT_BINARY_DIR}/GNUmakefile
         DESTINATION share/HepMC/examples )

add_subdirectory(fio) 
add_subdirectory(pythia8) 
