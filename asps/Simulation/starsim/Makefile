# $Name:  $
# $Id: Makefile,v 1.1.1.1 2004/01/12 23:49:36 potekhin Exp $
######################################################################
.SILENT:
.SUFFIXES:
.SUFFIXES: .o .cdf .age .F .c .cxx 
.PHONY   : debug all

TAGS         = $@
UNAME       := $(shell uname)
SHELL       := /bin/sh
RMF         := /bin/rm  -f
CMDS        := atlsim.exe staf.exe
SUBDIRS     := atmain atgeant atutil agzio rebank dzdoc geant comis deccc 
WRK_DIR     := .wrk
OBJ_DIR     := .obj

BASE        := $(word 1,$(ATLAS_ROOT) /afs/cern.ch/atlas)
SOURCE      := $(BASE)/software/dist/HourlyHeadFromCVS

atlsim_USES  =  age f77 c c++
atlsim_SRC   =  aversion.F \
                  $(foreach suffix,age F c cxx,\
                     $(foreach dir,$(SUBDIRS),\
                        $(wildcard $(dir)/*.$(suffix))))
atlsim_LIST := $(sort $(basename $(notdir $(atlsim_SRC))))
atlsim_OBJ  := $(addprefix $(OBJ_DIR)/, $(addsuffix .o, $(atlsim_LIST)))

CPP_NAME    := 'atlsim'
CWD         := $(shell pwd)
version     := $(notdir $(CWD))
CPP_TIME    := $(shell date +%H%M)
CPP_DATE    := $(shell date +%y%m%d)
CPP_TITLE   := $(shell echo $(version)|sed -e 's%[-].*%%;s%^$$%Unknown%')
CPP_VERS    := $(shell echo $(version)|sed -e 's%^[^-]*%%;s%^-%%;s%^$$%Unknown%')
SYS         := linux
PP          := g77 -E -P -Df2cFortran
CC          := cc  -g
CPP         := g++ -g
F77         := g77
O           := -o
AGE         := agetof.exe
DIRS        := $(sort $(WRK_DIR) $(OBJ_DIR))
VPATH       := $(SUBDIRS)

MYSQL := /afs/cern.ch/atlas/offline/external/MySQL/4.0.13/i686-rh73-gcc32
FLAGS := -fno-second-underscore -w -g -O2 \
        -DCPP_DATE="$(CPP_DATE)" -DCPP_TIME="$(CPP_TIME)" \
        -DCPP_TITLE_CH="'$(CPP_TITLE)'" -DCPP_VERS="'$(CPP_VERS)'"  \
        -DATLAS_TYPE    -DATLAS_UNIX    -DATLAS_LINUX     -Df2cFortran \
        -DCERNLIB_TYPE  -DCERNLIB_UNIX  -DCERNLIB_LINUX   -DCERNLIB_MYSQL \
        -DCERNLIB_DZDOC -DCERNLIB_SHL   -DCERNLIB_NONEWL  -DCERNLIB_HADRON \
        -DCERNLIB_HIGZ  -DCERNLIB_CG    -DCERNLIB_GCALOR  -DCERNLIB_BSLASH 
INCS   = $(FLAGS) -I$(dir $+) -Iinclude -I$(MYSQL)/include -I/cern/pro/include 

############################################################################
all:     debug dirs objects lib exe
dirs:    $(DIRS)
$(DIRS):;@mkdir -p $(TAGS)
objects: $(atlsim_OBJ)

lib:	 $(WRK_DIR)/libstarsim.a
$(WRK_DIR)/libstarsim.a:  $(atlsim_OBJ) 
	 ar -ruc $(WRK_DIR)/libstarsim.a  $(atlsim_OBJ) 

exe:    $(WRK_DIR)/atlsim.exe 
$(WRK_DIR)/atlsim.exe:  $(WRK_DIR)/libstarsim.a 
	gcc -w -g -O2 -export-dynamic -rdynamic \
	-o  $(WRK_DIR)/atlsim.exe        \
        -Wl,-noinhibit-exec,-Bdynamic    acmain.cxx \
        -Wl,--whole-archive           $(WRK_DIR)/libstarsim.a  \
        -Wl,-Bstatic -lg2c -lstdc++   -Wl,--no-whole-archive   \
        -L$(SOURCE)/../7.1.0/InstallArea/i686-rh73-gcc32-dbg/lib -lgcalor \
        -L$(MYSQL)/lib -lmysqlclient  -lssl -lcrypto -lz -Wl,-Bdynamic \
        `cernlib geant321 pawlib graflib/X11 packlib mathlib kernlib` \
        -lpthread -lm

debug:
#	echo atlsim_SRC = $(atlsim_SRC)
#	echo atlsim_OBJ = $(atlsim_OBJ)
	echo version    = $(version)
############################################################################
$(OBJ_DIR)/%.o:   %.age
	@ls $+; cat $+ >  $(WRK_DIR)/$*.F
	@rm -rf           $(WRK_DIR)/$*.g      $(OBJ_DIR)/$*.o
	$(PP) $(INCS)     $(WRK_DIR)/$*.F $(O) $(WRK_DIR)/$*.g
	@$(AGE)           $(WRK_DIR)/$*.g  -o  $(WRK_DIR)/$*.f
	$(F77)  -w -O2 -c $(WRK_DIR)/$*.f  -o  $(OBJ_DIR)/$*.o
$(OBJ_DIR)/%.o:   %.F
	@ls $+;  cat $+ > $(WRK_DIR)/$*.F; rm -f $(OBJ_DIR)/$*.o
	$(F77) $(INCS) -c $(WRK_DIR)/$*.F    -o  $(OBJ_DIR)/$*.o
$(OBJ_DIR)/%.o:   %.c
	@ls $+; rm -f $(OBJ_DIR)/$*.o
	$(CC)  $(INCS) -c $+ -o $(OBJ_DIR)/$*.o
$(OBJ_DIR)/%.o:   %.cxx
	@ls $+; rm -f $(OBJ_DIR)/$*.o
	$(CPP) $(INCS) -c $+ -o $(OBJ_DIR)/$*.o
$(OBJ_DIR)/%.o:   %.cdf
	@ls $+; kuipc  $+ $(WRK_DIR)/$*.c
	@$(CC)         -c $(WRK_DIR)/$*.c  -o  $(OBJ_DIR)/$*.o
##############################################################################
