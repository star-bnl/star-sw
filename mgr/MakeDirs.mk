# $Id: MakeDirs.mk,v 1.3 1999/02/12 22:12:20 fisyak Exp $
# $Log: MakeDirs.mk,v $
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
      STAR_OBJ_DIR := $(STAR)/.$(STAR_HOST_SYS)/obj/$(PKG)
  else
      LIB_DIR := $(SYS_DIR)/LIB
      DEP_DIR := $(SYS_DIR)/DEP/$(DOMAIN)
      OBJ_DIR := $(SYS_DIR)/OBJ/$(DOMAIN)
      STAR_OBJ_DIR := $(STAR)/.$(STAR_HOST_SYS)/OBJ/$(PKG)
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
  ifndef NT
      check_out   := $(shell test -d $(ROOT_DIR) || mkdir -p $(ROOT_DIR)) 
      check_sys   := $(shell test -d $(SYS_DIR) || mkdir -p $(SYS_DIR)) 
      check_lib   := $(shell test -d $(LIB_DIR) || mkdir -p $(LIB_DIR))
      check_obj   := $(shell test -d $(OBJ_DIR) || mkdir -p $(OBJ_DIR))
      check_dep   := $(shell test -d $(DEP_DIR) || mkdir -p $(DEP_DIR))
      check_gen   := $(shell test -d $(DIR_GEN) || mkdir -p $(DIR_GEN))
      check_neg   := $(shell test -d $(GEN_DIR) || mkdir -p $(GEN_DIR))
      check_tab   := $(shell test -d $(GEN_TAB) || mkdir -p $(GEN_TAB))
      check_tmp   := $(shell test -d $(GEN_TMP) || mkdir -p $(GEN_TMP))
  else # /* NT */
      check_out   := $(shell $(MKDIR) $(subst /,\,$(subst \,/,$(ROOT_DIR))))
      check_sys   := $(shell $(MKDIR) $(subst /,\,$(subst \,/,$(SYS_DIR))))
      check_lib   := $(shell $(MKDIR) $(subst /,\,$(subst \,/,$(LIB_DIR))))
      check_obj   := $(shell $(MKDIR) $(subst /,\,$(subst \,/,$(OBJ_DIR))))
#     check_dep   := $(shell $(MKDIR) $(subst /,\,$(subst \,/,$(DEP_DIR))))
      check_gen   := $(shell $(MKDIR) $(subst /,\,$(subst \,/,$(DIR_GEN))))
      check_neg   := $(shell $(MKDIR) $(subst /,\,$(subst \,/,$(GEN_DIR))))
      check_tab   := $(shell $(MKDIR) $(subst /,\,$(subst \,/,$(GEN_TAB))))
      check_tmp   := $(shell $(MKDIR) $(subst /,\,$(subst \,/,$(GEN_TMP))))
  endif #/* NT */
endif
ifdef NEVER
ifneq (,$(ROOT_DIR))
ifneq ($(STAR),$(ROOT_DIR))
  StTables    :=$(strip $(wildcard $(ROOT_DIR)/.share/tables))
  ifeq (,$(StTables))
    checkout := $(shell  mkdir -p $(ROOT_DIR)/.share/tables)
    checkout := $(foreach p,$(wildcard $(STAR)/.share/tables/*.*),\
                            $(shell $(LN) $(p)  $(ROOT_DIR)/.share/tables/.))
  endif
  StTables :=$(strip $(wildcard $(SYS_DIR)/obj/tables))
  ifeq (,$(StTables)) 
    checkout := $(shell  mkdir -p $(SYS_DIR)/obj/tables)
    checkout := $(foreach p,$(wildcard  $(STAR)/.$(SYS_HOST_SYS)/obj/tables/*.*),\
                           $(shell $(LN) $(p)  $(SYS_DIR)/obj/tables/.))
  endif
  StTables :=$(strip $(wildcard $(SYS_DIR)/dep/tables))
  ifeq (,$(StTables)) 
    checkout := $(shell  mkdir -p $(SYS_DIR)/dep/tables)
    checkout := $(foreach p,$(wildcard  $(STAR)/.$(SYS_HOST_SYS)/dep/tables/*.*),\
                           $(shell $(LN) $(p)  $(SYS_DIR)/dep/tables/.))

  endif
endif
endif
endif
