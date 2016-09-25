
set ( hepmc_source_list 
			 CompareGenEvent.cc
			 Flow.cc
			 GenEvent.cc
			 GenEventStreamIO.cc
			 GenParticle.cc
			 GenCrossSection.cc
			 GenVertex.cc
			 GenRanges.cc
			 HeavyIon.cc
			 IO_AsciiParticles.cc
			 IO_GenEvent.cc
			 PdfInfo.cc
			 Polarization.cc
			 SearchVector.cc
			 StreamHelpers.cc
			 StreamInfo.cc
			 ${CMAKE_CURRENT_BINARY_DIR}/Units.cc
			 WeightContainer.cc
			 )

configure_file( Units.cc.in ${CMAKE_CURRENT_BINARY_DIR}/Units.cc  @ONLY )

ADD_LIBRARY (HepMC  SHARED ${hepmc_source_list})
ADD_LIBRARY (HepMCS STATIC ${hepmc_source_list})
SET_TARGET_PROPERTIES (HepMC  PROPERTIES OUTPUT_NAME HepMC )
SET_TARGET_PROPERTIES (HepMC  PROPERTIES VERSION 4.0.0 SOVERSION 4 )
SET_TARGET_PROPERTIES (HepMCS PROPERTIES OUTPUT_NAME HepMC )
SET_TARGET_PROPERTIES(HepMC  PROPERTIES CLEAN_DIRECT_OUTPUT 1)
SET_TARGET_PROPERTIES(HepMCS PROPERTIES CLEAN_DIRECT_OUTPUT 1)

INSTALL (TARGETS HepMC HepMCS
    RUNTIME DESTINATION bin
    LIBRARY DESTINATION lib
    ARCHIVE DESTINATION lib
    ) 
