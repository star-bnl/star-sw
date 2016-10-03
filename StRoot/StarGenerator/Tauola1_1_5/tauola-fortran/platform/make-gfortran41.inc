F77      = gfortran
F77FLAGS = -fPIC -g -fno-automatic -fno-backslash -ffixed-line-length-132
FFLAGS   = $(F77FLAGS)


LD       = gfortran
LDFLAGS  = -g
SOFLAGS  = -shared

RANLIB   = ranlib
AR       = ar
DIFF    = diff -ib

CPP     = /lib/cpp 
CPPFLAGS= -C -P -traditional-cpp

CC      = g++
CFLAGS  = -fPIC -g

GCC     = gcc
GCCFLAGS= -O3 -fomit-frame-pointer -DHAVE_UNDERSCORE

#Uncomment to compile with debug mode turned on
#DEBUG = -D"_LOG_DEBUG_MODE_"
