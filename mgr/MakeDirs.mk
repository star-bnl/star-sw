# $Id: MakeDirs.mk,v 1.10 1999/09/03 17:49:50 fisyak Exp $
# $Log: MakeDirs.mk,v $
# Revision 1.10  1999/09/03 17:49:50  fisyak
# Make makel and cons compartible in OBJ
#
# Revision 1.9  1999/08/24 16:00:10  fisyak
# Bug in STAR_OBJ_DIR
#
# Revision 1.8  1999/08/24 13:27:27  fisyak
# Fix St_Tables name
#
# Revision 1.7  1999/08/20 22:59:14  fisyak
# Fix problem with / in ROOT_DIR
#
# Revision 1.6  1999/08/16 16:31:32  fisyak
# Simplify Makefiles
#
# Revision 1.5  1999/05/08 23:18:17  fisyak
# Clean up
#
# Revision 1.4  1999/02/13 01:52:07  didenko
# Fix bug with list of tables
#
# Revision 1.3  1999/02/12 22:12:20  fisyak
# Fix STAR_OBJ_DIR for nondebug version
#
# Revision 1.2  1999/02/12 02:50:29  fisyak
# Fix St_Tables, single module
#
# Revision 1.1  1999/02/08 02:29:19  fisyak
# New Makefile scheme
#

SRC_DIR := $(INP_DIR)
SYS_DIR := $(ROOT_DIR)/.$(STAR_HOST_SYS)
LIB_DIR := $(SYS_DIR)/lib
DEP_DIR := $(SYS_DIR)/dep/$(branch)/$(DOMAIN)
OBJ_DIR := $(SYS_DIR)/obj/$(branch)/$(DOMAIN)
ifndef STAR_OBJ_DIR 
  STAR_OBJ_DIR := $(subst $(ROOT_DIR),$(STAR),$(OBJ_DIR))
endif
DIR_GEN := $(ROOT_DIR)/.share
GEN_TMP := $(DIR_GEN)/tmp
GEN_TAB := $(DIR_GEN)/tables
GEN_DIR := $(DIR_GEN)/$(DOMAIN)
DOM_DIRS:= $(filter-out CVS, $(notdir $(wildcard $(ROOT_DIR)/pams/*)))
#.
check_sys   := $(shell test -d $(SYS_DIR)  || mkdir -p $(SYS_DIR)) 
check_lib   := $(shell test -d $(LIB_DIR)  || mkdir -p $(LIB_DIR))
check_obj   := $(shell test -d $(OBJ_DIR)  || mkdir -p $(OBJ_DIR))
check_dep   := $(shell test -d $(DEP_DIR)  || mkdir -p $(DEP_DIR))
check_gen   := $(shell test -d $(DIR_GEN)  || mkdir -p $(DIR_GEN))
check_neg   := $(shell test -d $(GEN_DIR)  || mkdir -p $(GEN_DIR))
check_tab   := $(shell test -d $(GEN_TAB)  || mkdir -p $(GEN_TAB))
check_tmp   := $(shell test -d $(GEN_TMP)  || mkdir -p $(GEN_TMP))
