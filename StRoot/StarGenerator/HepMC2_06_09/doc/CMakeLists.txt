# build HepMC documentation
# for documentation

if ( build_docs )

find_package(LATEX)
find_package(Doxygen)

macro( hepmc_latex package main_tex_file )
IF(LATEX_COMPILER)
   FOREACH(arg ${ARGN})  
     CONFIGURE_FILE( ${CMAKE_CURRENT_SOURCE_DIR}/${arg} 
                     ${CMAKE_CURRENT_BINARY_DIR}/${arg}  COPYONLY)
     set( depend_list ${arg} ${depend_list} )
   ENDFOREACH(arg) 
   ##message(STATUS "${package} ${main_tex_file} depends on ${depend_list}" )
   ADD_CUSTOM_COMMAND( 
    OUTPUT    ${CMAKE_CURRENT_BINARY_DIR}/${main_tex_file}.aux
    COMMAND   ${LATEX_COMPILER}
              -interaction=batchmode ${CMAKE_CURRENT_SOURCE_DIR}/${main_tex_file}
    DEPENDS   ${CMAKE_CURRENT_SOURCE_DIR}/${main_tex_file}.tex ${depend_list}
    COMMENT   "Latex - first pass"
  )
   ADD_CUSTOM_COMMAND( 
    OUTPUT    ${CMAKE_CURRENT_BINARY_DIR}/${main_tex_file}.dvi
    COMMAND   ${LATEX_COMPILER}
              -interaction=batchmode ${CMAKE_CURRENT_SOURCE_DIR}/${main_tex_file}
    DEPENDS   ${CMAKE_CURRENT_BINARY_DIR}/${main_tex_file}.aux
    COMMENT   "Latex - second pass"
  )
   ADD_CUSTOM_COMMAND( 
    OUTPUT    ${CMAKE_CURRENT_BINARY_DIR}/${main_tex_file}.log
    COMMAND   ${LATEX_COMPILER}
              -interaction=batchmode ${CMAKE_CURRENT_SOURCE_DIR}/${main_tex_file}
    DEPENDS   ${CMAKE_CURRENT_BINARY_DIR}/${main_tex_file}.dvi
    COMMENT   "Latex - third pass"
  )
  ADD_CUSTOM_TARGET(${main_tex_file} ALL echo
    DEPENDS   ${CMAKE_CURRENT_BINARY_DIR}/${main_tex_file}.log
    )

IF(DVIPS_CONVERTER)
    ADD_CUSTOM_COMMAND( 
      OUTPUT    ${CMAKE_CURRENT_BINARY_DIR}/${main_tex_file}.ps
      COMMAND   ${DVIPS_CONVERTER}
                ${CMAKE_CURRENT_BINARY_DIR}/${main_tex_file}.dvi
                -o ${CMAKE_CURRENT_BINARY_DIR}/${main_tex_file}.ps
      DEPENDS   ${CMAKE_CURRENT_BINARY_DIR}/${main_tex_file}.dvi
      COMMENT   "dvi2ps"
   )

 IF(PS2PDF_CONVERTER)
    ADD_CUSTOM_COMMAND( 
      OUTPUT    ${CMAKE_CURRENT_BINARY_DIR}/${main_tex_file}.pdf
      COMMAND   ${PS2PDF_CONVERTER}
                ${CMAKE_CURRENT_BINARY_DIR}/${main_tex_file}.ps
      DEPENDS   ${CMAKE_CURRENT_BINARY_DIR}/${main_tex_file}.ps
      COMMENT   "ps2pdf"
    )

 ADD_CUSTOM_TARGET( ${package}${main_tex_file}Document ALL echo
      DEPENDS   ${CMAKE_CURRENT_BINARY_DIR}/${main_tex_file}.pdf
    )
    ENDIF(PS2PDF_CONVERTER)
  ENDIF(DVIPS_CONVERTER)
ENDIF(LATEX_COMPILER)

INSTALL (FILES ${CMAKE_CURRENT_BINARY_DIR}/${main_tex_file}.pdf
         DESTINATION share/HepMC/doc )
endmacro( hepmc_latex )

# use the macro to build the user manual
hepmc_latex( HepMC HepMC2_user_manual 
                   graph_interpretation.eps 
		   HepMC_basic_properties.eps
		   physicist_visualization.eps )

# now try doxygen
IF (DOXYGEN_FOUND)
  SET(DOXYGEN_INPUT doxygen.conf)
  SET(DOXYGEN_OUTPUT ${CMAKE_BINARY_DIR}/html/index.html )

  ADD_CUSTOM_COMMAND(
    OUTPUT ${DOXYGEN_OUTPUT}
    COMMAND ${CMAKE_COMMAND} -E echo_append "Building HepMC Documentation..."
    COMMAND ${DOXYGEN_EXECUTABLE} ${DOXYGEN_INPUT}
    COMMAND ${CMAKE_COMMAND} -E echo "Done."
    WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}
    DEPENDS ${DOXYGEN_INPUT}
    )

  ADD_CUSTOM_TARGET(hepmcdoc ALL DEPENDS ${DOXYGEN_OUTPUT})

  ADD_CUSTOM_TARGET(hepmcdoc_forced
    COMMAND ${CMAKE_COMMAND} -E echo_append "Building API Documentation..."
    COMMAND ${DOXYGEN_EXECUTABLE} ${DOXYGEN_INPUT}
    COMMAND ${CMAKE_COMMAND} -E echo "Done."
    WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}
    )

ENDIF (DOXYGEN_FOUND)

else( build_docs )

# look for existing pdf files
FILE( GLOB pdf_files *.pdf )
INSTALL (FILES ${pdf_files}
         DESTINATION share/HepMC/doc )


endif( build_docs )
