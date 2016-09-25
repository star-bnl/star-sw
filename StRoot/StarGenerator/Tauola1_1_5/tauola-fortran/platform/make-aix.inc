########### collection of IBM  flags ###########
# IBM options: fixed=100 for 100 or more columns,
#              qextname for C-style external names with underscore at the end
#FFLAGS  =  -qfixed=100 -qextname -g -C -qextchk -qinitauto=FF -qflttrap=overflow:underflow:zerodivide:invalid:enable  

FFLAGS  =  -qfixed=100 -qextname -O -C -qextchk
F77     = f77
LD      = $(F77)
LDFLAGS = 
AR      = ar
RANLIB  = ranlib


CPP     = /usr/lib/cpp 
CPPFLAGS= -P
DIFF    = diff -ib