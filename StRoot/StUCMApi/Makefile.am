## #####################################################################
## 
##  File:       Makefile.am
## 
##  Purpose:     
## 
##  Version:      $Id: Makefile.am,v 1.1 2008/12/08 21:11:28 fine Exp $
## 
## Copyright 2007, Tech-X Corporation, All Rights Reserved.
## 
## #####################################################################

AM_CXXFLAGS = -w -Wno-deprecated -W -Wpointer-arith -pipe \
	      -D_POSIX_THREAD -D_POSIX_THREAD_SAFE_FUNCTIONS -D_REENTRANT

INCLUDES = -I. -I../data/base -I../data/modules -I../data
LIBS     = ../data/libTxUCMData.la
ALL_LIBS = $(LIBS) 

## Create a shared library 
SOURCES = TxEventLog.cpp TxTask.cpp TxJob.cpp
lib_LTLIBRARIES = libTxUCM.la
libTxUCM_la_SOURCES = $(SOURCES)
libTxUCM_la_LIBADD =  $(ALL_LIBS)
libTxUCM_la_LDFLAGS = -no-undefined -release $(VERSION)

include $(top_srcdir)/config/clean-cxx.am
