##################################################################
# This module tries to find the HepMC installation on your system.
# Usage:
#       FIND_PACKAGE( HepMC [REQUIRED] [COMPONENTS HepMCfio ...] )
#
# It sets the following variables:
#       ${HEPMC_FOUND}
#       ${HepMC_INCLUDE_DIRS}
#       ${HepMC_LIBRARY_DIRS}
#       ${HepMC_LIBRARIES}
#
#       ${HepMC_HEPEVT_SIZE}
#
##################################################################

MESSAGE(STATUS "Looking for HepMC...")

# default hepmc common block size: 
# Set( HepMC_HEPEVT_SIZE 99990 )

## try to find each HepMC components in user defined path
FOREACH (COMPONENT ${HepMC_FIND_COMPONENTS})
        FIND_LIBRARY(${COMPONENT}_PATH
                NAMES
                ${COMPONENT}

                PATHS
		/usr
		/usr/local
		${HEPMC_PREFIX}
                ${HEPMC_ROOT}
                ${HEPMC_DIR}
                ${HEPMC_ROOT_DIR}
                ${HEP_ROOT}
		$ENV{HEPMC_PREFIX}
                $ENV{HEPMC_ROOT}
                $ENV{HEPMC_DIR}
                $ENV{HEPMC_ROOT_DIR}
                $ENV{HEP_ROOT}

                PATH_SUFFIXES
                lib
                lib64
        )

        # set found flag
        IF ({${${COMPONENT}_PATH} MATCHES "${COMPONENT}_PATH-NOTFOUND")
     	   SET(HEPMC_FOUND FALSE)
        ELSE ({${${COMPONENT}_PATH} MATCHES "${COMPONENT}_PATH-NOTFOUND")
           MESSAGE (STATUS "HepMC ${COMPONENT} library path found: ${${COMPONENT}_PATH}")
        ENDIF ({${${COMPONENT}_PATH} MATCHES "${COMPONENT}_PATH-NOTFOUND")

ENDFOREACH (COMPONENT HepMC_FIND_COMPONENTS)

## search for include path
FIND_PATH(HepMC_INCLUDE_DIRS
                NAMES
                HepMC/GenEvent.h

                PATHS
		/usr
		/usr/local
		${HEPMC_PREFIX}
                ${HEPMC_ROOT}
                ${HEPMC_ROOT_DIR}
                ${HEP_ROOT}
		$ENV{HEPMC_PREFIX}
                $ENV{HEPMC_ROOT}
                $ENV{HEPMC_ROOT_DIR}
                $ENV{HEP_ROOT}

                PATH_SUFFIXES
                include
	)

# set found flag
IF (${HepMC_INCLUDE_DIRS} MATCHES "HepMC_INCLUDE_DIRS-NOTFOUND")
	SET(HEPMC_FOUND FALSE)
ELSE (${HepMC_INCLUDE_DIRS} MATCHES "HepMC_INCLUDE_DIRS-NOTFOUND")
	MESSAGE(STATUS "HepMC include path found: " ${HepMC_INCLUDE_DIRS} )
ENDIF (${HepMC_INCLUDE_DIRS} MATCHES "HepMC_INCLUDE_DIRS-NOTFOUND")

## final printout

# if not found

IF(${HEPMC_FOUND} MATCHES "FALSE")
        IF (HepMC_FIND_REQUIRED)
        	MESSAGE( FATAL_ERROR
                		     "HepMC shared library or includes not found\n"
                                     "Please install HepMC and/or set HEPMC_PREFIX environment\n"
                                     )
        ELSE (HepMC_FIND_REQUIRED)
                MESSAGE("HepMC2 library not found")
        ENDIF (HepMC_FIND_REQUIRED)


# end if not found
# if found :

ELSE(${HEPMC_FOUND} MATCHES "FALSE")
	SET(HEPMC_FOUND TRUE)

# find length of hepmc common block pre-defined and use this for epos
        # EXECUTE_PROCESS (COMMAND cat ${HepMC_INCLUDE_DIRS}/HepMC/HEPEVT_Wrapper.h
	#                  COMMAND grep "#define HEPEVT_EntriesAllocation" 
	# 		 COMMAND awk  "{print $3}"
	# 		 COMMAND tr "\n" " "
	# 		 COMMAND sed "s/ //"
	# 		 OUTPUT_VARIABLE HepMC_HEPEVT_SIZE
	#   )
	# MESSAGE( INFO "-HepMC: HEPEVT_EntriesAllocation in file \"${HepMC_INCLUDE_DIRS}/HepMC/HEPEVT_Wrapper.h\" is \"${HepMC_HEPEVT_SIZE}\"" )
	# MESSAGE( INFO "-HepMC: This limits the maximum number of particles per event you can have. " )
	# MESSAGE( INFO "-HepMC: Change you HepMC library if you run into limitation! " )
	
        #set HepMC_LIBRARY_DIRS (extract from first component)
        list(GET HepMC_FIND_COMPONENTS 0 TMP)
        #extract the path from variable component_path and put in hepmc_library_dirs
        GET_FILENAME_COMPONENT(HepMC_LIBRARY_DIRS ${TMP}_PATH PATH)

        #set HepMC_LIBRARIES
        FOREACH(COMPONENT ${HepMC_FIND_COMPONENTS})
            list(APPEND HepMC_LIBRARIES ${${COMPONENT}_PATH})
        ENDFOREACH(COMPONENT ${HepMC_FIND_COMPONENTS})
ENDIF(${HEPMC_FOUND} MATCHES "FALSE")

