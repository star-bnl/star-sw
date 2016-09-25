
F77     = g77
FFLAGS  = -fno-automatic -g -ffixed-line-length-132 -fno-backslash
LD      = $(F77)
LDFLAGS = -fno-automatic -g
AR      = ar
RANLIB  = ranlib


CPP     = /lib/cpp 
CPPFLAGS= -C -P -traditional-cpp
DIFF    = diff -ib
