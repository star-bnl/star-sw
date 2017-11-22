

macro( hepmc_test testname )

  message( STATUS "building ${testname} for ${testname}.sh" )

  find_file( ${testname}_source ${testname}.cc ${CMAKE_CURRENT_SOURCE_DIR}
${CMAKE_CURRENT_BINARY_DIR} )
  ADD_EXECUTABLE(${testname} ${${testname}_source} ${ARGN} )
 
  if( ${CMAKE_SYSTEM_NAME} MATCHES "Windows" )
    message( STATUS "skipping ${testname}.sh on ${CMAKE_SYSTEM_NAME}")
  else()
    # automake/autoconf variables for ${testname}.sh.in 
    set(srcdir ${CMAKE_CURRENT_SOURCE_DIR} )
    set(builddir ${CMAKE_CURRENT_BINARY_DIR} )
    configure_file( ${testname}.sh.in 
                    ${CMAKE_CURRENT_BINARY_DIR}/${testname}.sh  @ONLY )
    add_test( ${testname}.sh ${CMAKE_CURRENT_BINARY_DIR}/${testname}.sh )
  endif()

endmacro( hepmc_test )

macro( hepmc_simple_test testname )

  message( STATUS "building simple test ${testname} " )

  # include test in search path
  include_directories ("${CMAKE_CURRENT_SOURCE_DIR}")

  find_file( ${testname}_source ${testname}.cc ${CMAKE_CURRENT_SOURCE_DIR}
${CMAKE_CURRENT_BINARY_DIR} )
  #message(STATUS "building simple test ${testname} from ${${testname}_source}")
  ADD_EXECUTABLE(${testname} ${${testname}_source} ${ARGN} )

  add_test( ${testname} ${testname} )

endmacro( hepmc_simple_test )


link_libraries( HepMC )

set( HepMC_tests 
		 testHepMCIteration
		 testMass
		 testFlow
                 testPrintBug
		 testPolarization )
set( HepMC_simple_tests testSimpleVector 
                	testUnits
			testMultipleCopies 
			testWeights )

# automake/autoconf variables for *.cc.in 
set(srcdir ${CMAKE_CURRENT_SOURCE_DIR} )

FILE( GLOB process_source *.cc.in )
foreach( sfile ${process_source} )
  get_filename_component(basename ${sfile} NAME_WE )
  configure_file( ${sfile}
                  ${CMAKE_CURRENT_BINARY_DIR}/${basename}.cc  @ONLY )
endforeach( sfile ${process_source} )

hepmc_test( testHepMC testHepMCMethods.cc )
hepmc_test( testStreamIO testHepMCMethods.cc )

foreach ( test ${HepMC_tests} )
  hepmc_test( ${test} )
endforeach ( test ${HepMC_tests} )

foreach ( test ${HepMC_simple_tests} )
  hepmc_simple_test( ${test} )
endforeach ( test ${HepMC_simple_tests} )
