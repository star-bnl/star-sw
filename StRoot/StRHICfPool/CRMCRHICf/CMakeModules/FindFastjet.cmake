##################################################################
# This module tries to find the Fastjet installation on your system.
# Usage:
#       FIND_PACKAGE( fastjet [REQUIRED] [COMPONENTS ...] )
#
# It sets the following variables:
#       ${Fastjet_FOUND}
#       ${Fastjet_INCLUDE_DIRS}
#       ${Fastjet_LIBRARY_DIRS}
#       ${Fastjet_LIBRARIES}
#
##################################################################

MESSAGE(STATUS "Looking for fastjet...")

## try to find each Fastjet components in user defined path
FOREACH (COMPONENT ${Fastjet_FIND_COMPONENTS})
        FIND_LIBRARY(${COMPONENT}_PATH
                NAMES
                ${COMPONENT}

                PATHS
		/usr
		/usr/local
		${Fastjet_PREFIX}
                ${Fastjet_ROOT}
                ${Fastjet_DIR}
                ${Fastjet_ROOT_DIR}
                ${HEP_ROOT}
		$ENV{FASTJET_PREFIX}
                $ENV{FASTJET_ROOT}
		$ENV{Fastjet_PREFIX}
                $ENV{Fastjet_ROOT}
                $ENV{Fastjet_DIR}
                $ENV{Fastjet_ROOT_DIR}
                $ENV{HEP_ROOT}

                PATH_SUFFIXES
                lib
                lib64
        )

        # set found flag
        IF ({${${COMPONENT}_PATH} MATCHES "${COMPONENT}_PATH-NOTFOUND")
     	   SET(Fastjet_FOUND FALSE)
        ELSE ({${${COMPONENT}_PATH} MATCHES "${COMPONENT}_PATH-NOTFOUND")
           MESSAGE (STATUS "Fastjet ${COMPONENT} library path found: ${${COMPONENT}_PATH}")
        ENDIF ({${${COMPONENT}_PATH} MATCHES "${COMPONENT}_PATH-NOTFOUND")

ENDFOREACH (COMPONENT Fastjet_FIND_COMPONENTS)

## search for include path
FIND_PATH(Fastjet_INCLUDE_DIRS
                NAMES
		fastjet/PseudoJetStructureBase.hh

                PATHS
		/usr
		/usr/local
		${Fastjet_PREFIX}
                ${Fastjet_ROOT}
                ${Fastjet_DIR}
                ${Fastjet_ROOT_DIR}
                ${HEP_ROOT}
		$ENV{FASTJET_PREFIX}
                $ENV{FASTJET_ROOT}
		$ENV{Fastjet_PREFIX}
                $ENV{Fastjet_ROOT}
                $ENV{Fastjet_DIR}
                $ENV{Fastjet_ROOT_DIR}
                $ENV{HEP_ROOT}

                PATH_SUFFIXES
                .
                include
	)

# set found flag
IF (${Fastjet_INCLUDE_DIRS} MATCHES "Fastjet_INCLUDE_DIRS-NOTFOUND")
	SET(Fastjet_FOUND FALSE)
ELSE (${Fastjet_INCLUDE_DIRS} MATCHES "Fastjet_INCLUDE_DIRS-NOTFOUND")
	MESSAGE(STATUS "Fastjet include path found: " ${Fastjet_INCLUDE_DIRS} )
ENDIF (${Fastjet_INCLUDE_DIRS} MATCHES "Fastjet_INCLUDE_DIRS-NOTFOUND")

## final printout

# if not found

IF(${Fastjet_FOUND} MATCHES "FALSE")
        IF (Fastjet_FIND_REQUIRED)
        	MESSAGE( FATAL_ERROR
                		     "Fastjet shared library or includes not found\n"
                                     "Please install Fastjet and/or set Fastjet_PREFIX environment\n"
                                     )
        ELSE (Fastjet_FIND_REQUIRED)
                MESSAGE( WARNING
                		     "Fastjet shared library or includes not found\n"
                                     "Please install Fastjet and/or set Fastjet_PREFIX environment\n"
                                     )
        ENDIF (Fastjet_FIND_REQUIRED)


# end if not found
# if found :

ELSE(${Fastjet_FOUND} MATCHES "FALSE")
	SET(Fastjet_FOUND TRUE)
	
        #set Fastjet_LIBRARIES
        FOREACH(COMPONENT ${Fastjet_FIND_COMPONENTS})
            list(APPEND Fastjet_LIBRARIES ${${COMPONENT}_PATH})
        ENDFOREACH(COMPONENT ${Fastjet_FIND_COMPONENTS})
ENDIF(${Fastjet_FOUND} MATCHES "FALSE")

