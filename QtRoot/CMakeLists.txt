# $Id: CMakeLists.txt,v 1.1 2013/08/30 15:59:47 perev Exp $
ROOT_USE_PACKAGE(graf2d/gpad)
ROOT_USE_PACKAGE(graf2d/graf)
ROOT_USE_PACKAGE(misc/table)

add_subdirectory(qt)
add_subdirectory(qtgui)
add_subdirectory(qt4ged)
add_subdirectory(qtroot)
add_subdirectory(qtthread)
