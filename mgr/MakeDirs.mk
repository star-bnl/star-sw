# $Id: MakeDirs.mk,v 1.5 1999/05/08 23:18:17 fisyak Exp $
# $Log: MakeDirs.mk,v $
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
ifeq (,$(findstring $(LEVEL),0 1))
  PKG     := $(notdir $(INP_DIR))
  D       := $(subst /, ,$(subst $(ROOT_DIR),,$(INP_DIR)))
  DOMAIN  := $(word 2, $(D))
  ifeq ($(DOMAIN),gen)
    DOMAIN  := $(word 3, $(D))
  endif
  ifeq ($(DOMAIN),sim)
    DOMAIN  := $(word 3, $(D))
  endif
  ifndef NODEBUG
      LIB_DIR := $(SYS_DIR)/lib
      DEP_DIR := $(SYS_DIR)/dep/$(DOMAIN)
      OBJ_DIR := $(SYS_DIR)/obj/$(DOMAIN)
   ifndef STAR_OBJ_DIR 
      STAR_OBJ_DIR := $(STAR)/.$(STAR_HOST_SYS)/obj/$(PKG)
   endif
  else
      LIB_DIR := $(SYS_DIR)/LIB
      DEP_DIR := $(SYS_DIR)/DEP/$(DOMAIN)
    ifndef STAR_OBJ_DIR
      OBJ_DIR := $(SYS_DIR)/OBJ/$(DOMAIN)
      STAR_OBJ_DIR := $(STAR)/.$(STAR_HOST_SYS)/OBJ/$(PKG)
   endif
  endif
  ifndef NT
      TMP_DIR := $(SYS_DIR)/tmp
  else
      TMP_DIR := $(TEMP)/tmp
  endif # /NT/
  export LIB_DIR	#especially for .rootrc
  ifdef NT
      BIN_DIR := $(SYS_DIR)/bin
  endif
  DIR_GEN := $(ROOT_DIR)/.share
  GEN_TMP := $(DIR_GEN)/tmp
  GEN_TAB := $(DIR_GEN)/tables
  GEN_DIR := $(DIR_GEN)/$(DOMAIN)
  DOM_DIRS:= $(filter-out CVS, $(notdir $(wildcard $(ROOT_DIR)/pams/*)))
#.
      check_out   := $(shell test -d $(ROOT_DIR) || mkdir -p $(ROOT_DIR)) 
      check_sys   := $(shell test -d $(SYS_DIR)  || mkdir -p $(SYS_DIR)) 
      check_lib   := $(shell test -d $(LIB_DIR)  || mkdir -p $(LIB_DIR))
      check_obj   := $(shell test -d $(OBJ_DIR)  || mkdir -p $(OBJ_DIR))
      check_dep   := $(shell test -d $(DEP_DIR)  || mkdir -p $(DEP_DIR))
      check_gen   := $(shell test -d $(DIR_GEN)  || mkdir -p $(DIR_GEN))
      check_neg   := $(shell test -d $(GEN_DIR)  || mkdir -p $(GEN_DIR))
      check_tab   := $(shell test -d $(GEN_TAB)  || mkdir -p $(GEN_TAB))
      check_tmp   := $(shell test -d $(GEN_TMP)  || mkdir -p $(GEN_TMP))
endif
