IF(APPLE)
    SET( LD_LIBRARY_PATH_VAR DYLD_LIBRARY_PATH )
ELSE()
    SET( LD_LIBRARY_PATH_VAR LD_LIBRARY_PATH )
ENDIF()
SET( LD_LIBRARY_PATH_CONTENTS $ENV{${LD_LIBRARY_PATH_VAR}} )
#MESSAGE( STATUS "LD_LIBRARY_PATH_CONTENTS: ${LD_LIBRARY_PATH_CONTENTS}" )

SET( ROOT_CINT_WRAPPER ${LD_LIBRARY_PATH_VAR}=${ROOT_LIBRARY_DIR}:${LD_LIBRARY_PATH_CONTENTS} ${ROOTCINT_EXECUTABLE} )

IF( NOT DEFINED ROOT_DICT_OUTPUT_DIR )
    SET( ROOT_DICT_OUTPUT_DIR "${PROJECT_BINARY_DIR}/rootdict" )
ENDIF()

# clean generated header files with 'make clean'
SET_DIRECTORY_PROPERTIES( PROPERTIES ADDITIONAL_MAKE_CLEAN_FILES "${ROOT_DICT_OUTPUT_DIR}" )

IF( NOT ROOT_FIND_QUIETLY )
    MESSAGE( STATUS "Check for ROOT_DICT_OUTPUT_DIR: ${PROJECT_BINARY_DIR}/rootdict" )
    MESSAGE( STATUS "Check for ROOT_DICT_CINT_DEFINITIONS: ${ROOT_DICT_CINT_DEFINITIONS}" )
ENDIF()


# ============================================================================
# helper macro to prepare input headers for GEN_ROOT_DICT_SOURCES
#   sorts LinkDef.h to be the last header (required by rootcint)
#
# arguments:
#   input_dir - directory to search for headers matching *.hh
#
# returns:
#   ROOT_DICT_INPUT_HEADERS - all header files found in input_dir with
#       ${input_dir}_LinkDef.h as the last header (if found)
#
# ----------------------------------------------------------------------------
MACRO( PREPARE_ROOT_DICT_HEADERS _input_dir )

    FILE( GLOB ROOT_DICT_INPUT_HEADERS "${_input_dir}/*.hh" )
    FILE( GLOB _linkdef_hdr "${_input_dir}/LinkDef.h" )

    #LIST( FIND ROOT_DICT_INPUT_HEADERS ${_linkdef_hdr} _aux )
    #IF( ${_aux} EQUAL 0 OR ${_aux} GREATER 0 )
    #    LIST( REMOVE_ITEM ROOT_DICT_INPUT_HEADERS "${_linkdef_hdr}" )
    #    LIST( APPEND ROOT_DICT_INPUT_HEADERS "${_linkdef_hdr}" )
    #ENDIF()

    IF( _linkdef_hdr )
        LIST( REMOVE_ITEM ROOT_DICT_INPUT_HEADERS "${_linkdef_hdr}" )
        LIST( APPEND ROOT_DICT_INPUT_HEADERS "${_linkdef_hdr}")
    ENDIF()

    #MESSAGE( STATUS "ROOT_DICT_INPUT_HEADERS: ${ROOT_DICT_INPUT_HEADERS}" )

ENDMACRO( PREPARE_ROOT_DICT_HEADERS )



# ============================================================================
# helper macro to generate Linkdef.h files for rootcint
#
# arguments:
#   namespace - prefix used for creating header <namespace>_Linkdef.h
#   ARGN      - list of sources to be used for generating Linkdef.h
#
# returns:
#   ROOT_DICT_INPUT_HEADERS - all header files + <namespace>_LinkDef.h in the
#       correct order to be used by macro GEN_ROOT_DICT_SOURCES
#
# ----------------------------------------------------------------------------
MACRO( GEN_ROOT_DICT_LINKDEF_HEADER _namespace )

    SET( _input_headers ${ARGN} )
    SET( _linkdef_header "${ROOT_DICT_OUTPUT_DIR}/${_namespace}_Linkdef.h" )

    FOREACH( _header ${_input_headers} )
        SET( ${_namespace}_file_contents "${${_namespace}_file_contents}\\#pragma link C++ defined_in \\\"${_header}\\\"\\;\\\\n" )
    ENDFOREACH()

    ADD_CUSTOM_COMMAND(
        OUTPUT ${_linkdef_header}
        COMMAND mkdir -p ${ROOT_DICT_OUTPUT_DIR}
        COMMAND printf "${${_namespace}_file_contents}" > ${_linkdef_header}
        DEPENDS ${_input_headers}
        COMMENT "generating: ${_linkdef_header}"
    )

    SET( ROOT_DICT_INPUT_HEADERS ${_input_headers} ${_linkdef_header} )

ENDMACRO()


# ============================================================================
# macro for generating root dict sources with rootcint
#
# arguments:
#   dict_src_filename - filename of the dictionary source (to be generated)
#
# requires following variables:
#       ROOT_DICT_INPUT_HEADERS - list of headers needed to generate dict source
#           * if $LinkDef.h is in the list it must be at the end !!
#       ROOT_DICT_INCLUDE_DIRS - list of include dirs to pass to rootcint -I..
#       ROOT_DICT_CINT_DEFINITIONS - extra definitions to pass to rootcint
#       ROOT_DICT_OUTPUT_DIR - where dictionary source should be generated
#
# returns:
#       ROOT_DICT_OUTPUT_SOURCES - list containing generated source and other
#           previously generated sources
                                    
# ----------------------------------------------------------------------------
MACRO( GEN_ROOT_DICT_SOURCE _dict_src_filename )

    # TODO check for ROOT_CINT_EXECUTABLE

    # need to prefix all include dirs with -I
    set( _dict_includes )
    FOREACH( _inc ${ROOT_DICT_INCLUDE_DIRS} )
        SET( _dict_includes "${_dict_includes}\t-I${_inc}")  #fg: the \t fixes a wired string expansion 
        #SET( _dict_includes ${_dict_includes} -I${_inc} )
    ENDFOREACH()

    STRING( REPLACE "/" "_" _dict_src_filename_nosc ${_dict_src_filename} )
    SET( _dict_src_file ${ROOT_DICT_OUTPUT_DIR}/${_dict_src_filename_nosc} )
    STRING( REGEX REPLACE "^(.*)\\.(.*)$" "\\1.h" _dict_hdr_file "${_dict_src_file}" )
    ADD_CUSTOM_COMMAND(
        OUTPUT  ${_dict_src_file} ${_dict_hdr_file}
        COMMAND mkdir -p ${ROOT_DICT_OUTPUT_DIR}
        COMMAND ${ROOT_CINT_WRAPPER} -f "${_dict_src_file}" -c ${ROOT_DICT_CINT_DEFINITIONS} ${_dict_includes} ${ROOT_DICT_INPUT_HEADERS}
        WORKING_DIRECTORY "${CMAKE_CURRENT_SOURCE_DIR}"
        DEPENDS ${ROOT_DICT_INPUT_HEADERS}
        COMMENT "generating: ${_dict_src_file} ${_dict_hdr_file}"
    )
    LIST( APPEND ROOT_DICT_OUTPUT_SOURCES ${_dict_src_file} )

ENDMACRO()

# for backwards compatibility
MACRO( GEN_ROOT_DICT_SOURCES _dict_src_filename )
    #MESSAGE( "USING DEPRECATED GEN_ROOT_DICT_SOURCES. PLEASE USE GEN_ROOT_DICT_SOURCE instead." )
    SET( ROOT_DICT_OUTPUT_SOURCES )
    GEN_ROOT_DICT_SOURCE( ${_dict_src_filename} )
ENDMACRO()
# ============================================================================

