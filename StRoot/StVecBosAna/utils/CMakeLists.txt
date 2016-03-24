INCLUDE_DIRECTORIES(
	${CMAKE_CURRENT_SOURCE_DIR}
	${ROOT_INCLUDE_DIR}
	)

SET(DICT_FILE RootHelperUtilsDict)

ROOT_GENERATE_DICTIONARY(
	${DICT_FILE}
	H1F.h
	H1D.h
	H1I.h
	H2D.h
	H2F.h
	H2I.h
	MultiGraph.h
	PlotHelper.h
	ProtoEvent.h
	ValErrPair.h
	LINKDEF ${CMAKE_CURRENT_SOURCE_DIR}/LinkDef.h
	)

SET(
	SOURCES
	H1I.cxx
	H1F.cxx
	H1D.cxx
	H2I.cxx
	H2F.cxx
	H2D.cxx
	PlotHelper.cxx
	MultiGraph.cxx
	ProtoEvent.cxx
	ValErrPair.cxx
	${DICT_FILE}.cxx
	utils.cc
	)

ADD_LIBRARY(
	root-helper-utils
	STATIC
	${SOURCES}
	)

ADD_LIBRARY(
	root-helper-utils-shared
	SHARED
	${SOURCES}
	)

TARGET_LINK_LIBRARIES(
	root-helper-utils
	${ROOT_LIBRARIES}
	)

TARGET_LINK_LIBRARIES(
	root-helper-utils-shared
	${ROOT_LIBRARIES}
	)

IF(FORCE_32BIT)
        SET_TARGET_PROPERTIES(root-helper-utils PROPERTIES COMPILE_FLAGS "-m32" LINK_FLAGS "-m32")
        SET_TARGET_PROPERTIES(root-helper-utils-shared PROPERTIES COMPILE_FLAGS "-m32" LINK_FLAGS "-m32")
ENDIF()
