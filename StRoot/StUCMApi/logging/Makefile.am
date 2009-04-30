## #####################################################################
## 
##  File:       Makefile.am
## 
##  Purpose:     
## 
##  Version:      $Id: Makefile.am,v 1.2 2009/04/30 17:09:27 fine Exp $
## 
## Copyright 2007, Tech-X Corporation, All Rights Reserved.
## 
## #####################################################################

AM_CXXFLAGS = -w -Wno-deprecated -W -Wpointer-arith -pipe \
	      -D_POSIX_THREAD -D_POSIX_THREAD_SAFE_FUNCTIONS -D_REENTRANT

INCLUDES = -I.
LIBS     = 
ALL_LIBS = $(LIBS) 

## Create a shared library 
CXXSOURCES              = TxEventLog.cpp 
lib_LTLIBRARIES         = libTxLogging.la
libTxLogging_la_SOURCES = $(CXXSOURCES)
libTxLogging_la_LIBADD  =  $(ALL_LIBS)
libTxLogging_la_LDFLAGS = -no-undefined -release $(VERSION)

bin_PROGRAMS       = txlogevent TxEventLogMain 

## Create the Log Event Application Executable
txlogevent_SOURCES = TxLogEventCmd.cxx
txlogevent_LDADD   = ./libTxLogging.la

## Create the test executable
TxEventLogMain_SOURCES = TxEventLogMain.cxx
TxEventLogMain_LDADD   = ./libTxLogging.la

GEN_FILES = *~ 

include $(top_srcdir)/config/clean-cxx.am
