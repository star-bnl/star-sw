## #####################################################################
## 
##  File:       Makefile.am
## 
##  Purpose:     
## 
##  Version:      $Id: Makefile.am,v 1.1 2008/12/08 21:10:28 fine Exp $
## 
## Copyright 2007, Tech-X Corporation, All Rights Reserved.
## 
## #####################################################################

AM_CXXFLAGS = -w -Wno-deprecated -W -Wpointer-arith -pipe \
	      -D_POSIX_THREAD -D_POSIX_THREAD_SAFE_FUNCTIONS -D_REENTRANT
AUTOMAKE_OPTIONS = subdir-objects
INCLUDES = -I. -I./modules -I./base -I$(MYSQL_INCDIR) 
LIBS     =  $(MYSQL_LIBS)
ALL_LIBS = $(LIBS) 

## Create a shared library 
BASE_SOURCES = base/TxField.cpp base/TxHandler.cpp base/TxRecord.cpp base/TxDataException.cpp  \
		base/TxStoreHandler.cpp base/TxCollectionHandler.cpp
MODULE_SOURCES = modules/TxModuleException.cpp modules/TxModule.cpp modules/TxModuleFactory.cpp \
		modules/TxMySQLConnection.cpp modules/TxMySQLModule.cpp modules/TxMySQLResult.cpp \
		modules/TxMySQLRow.cpp 
COMMON_SOURCES = TxUCMException.cpp
lib_LTLIBRARIES = libTxUCMData.la
libTxUCMData_la_SOURCES = $(COMMON_SOURCES) $(BASE_SOURCES) $(MODULE_SOURCES)
libTxUCMData_la_LDFLAGS = -no-undefined -release $(VERSION)
libTxUCMData_la_LIBADD = $(ALL_LIBS)

include $(top_srcdir)/config/clean-cxx.am
