#  $Log: Makeloop.mk,v $
#  Revision 1.17  1998/09/18 00:32:14  fisyak
#  Add kuip break off for Linux
#
#  Revision 1.16  1998/09/16 21:52:14  fisyak
#  Add dependencies for StRoot
#
#  Revision 1.15  1998/09/15 22:15:29  fisyak
#  Fix root/noroot options
#
#  Revision 1.14  1998/09/13 22:58:34  fisyak
#  ROOT documentation only for dev
#
#  Revision 1.13  1998/09/13 19:49:57  fisyak
#  Add root documetation generation
#
#  Revision 1.12  1998/09/02 14:51:25  didenko
#  correction
#
#  Revision 1.11  1998/08/25 02:10:09  fisyak
#  Add nodebug
#
#  Revision 1.10  1998/08/22 23:35:10  fisyak
#  Add St_base.so dependence
#
#  Revision 1.9  1998/08/21 15:44:48  fisyak
#  Add reco_ds
#
#  Revision 1.8  1998/08/20 15:42:40  fisyak
#  Remove MakeSYS.mk
#
#  Revision 1.7  1998/08/19 22:26:04  fisyak
#  Split base
#
#  Revision 1.6  1998/08/19 21:41:41  fisyak
#  Split base -> base + xdf2root
#
#  Revision 1.5  1998/08/18 18:53:16  fisyak
#  Add root I/O
#
#  Revision 1.4  1998/08/12 21:45:27  fisyak
#  Add test on StRoot directories
#
#  Revision 1.3  1998/08/10 23:20:54  fisyak
#  Add test for base and tables
#
#  Revision 1.2  1998/07/01 12:15:57  fisyak
#  Move NODEBUG flag in Env, variable
#
#  Revision 1.1  1998/06/11 12:39:24  fisyak
#  New STAR/STAF makefiles
#
#  Revision 1.18  1998/05/06 17:27:39  didenko
#  makefile updated by Yuri
#
#  Revision 1.17  1998/05/03 18:27:41  fisyak
#  Set fixed path to geant3
#
#  Revision 1.16  1998/04/28 16:36:48  fisyak
#  remove or merger from stic -M
#
#  Revision 1.15  1998/04/26 02:49:35  fisyak
#  Fix fortran dependencies
#
#  Revision 1.14  1998/04/20 23:41:09  fisyak
#  Remove -traditional from gcc
#
#  Revision 1.13  1998/04/20 22:39:10  fisyak
#  Correct dependencies
#
#  Revision 1.12  1998/04/20 15:06:47  fisya
#  user CERNLIN include
#
#  Revision 1.11  1998/04/13 16:03:48  fisyak
#  Correct HPUX flags
#
#  Revision 1.10  1998/04/10 14:03:14  fisyak
#  Add supermodule in shared libraries
#
#  Revision 1.9  1998/04/07 22:02:49  fisyak
#  fogotten bracket
#
#  Revision 1.7  1998/04/07 20:57:14  fisyak
#  Add standard library to path
#
#  Revision 1.6  1998/04/04 14:45:50  fisyak
#  Fix bug with geant3.def
#
#  Revision 1.5  1998/03/27 14:32:54  fisyak
#  Simplify MakePam
#
#  Revision 1.3  1998/03/23 02:31:42  fisyak
#  move staff in group_dir
#
#  Revision 1.2  1998/03/09 14:36:30  fisyak
#  Switch varibales
#
#  Revision 1.1  1998/03/09 13:31:50  fisyak
#  Remove environment Variables
#
#  Revision 1.13  1998/02/22 02:08:25  fisyak
#  CPPFLAGS for HPX
#
#  Revision 1.12  1998/02/21 18:33:54  fisyak
#  Add util domain
#
#  Revision 1.11  1998/02/21 00:58:31  fisyak
#  Add tls library
#
#  Revision 1.10  1998/02/18 22:58:03  fisyak
#  Move staf
#
#  Revision 1.9  1998/02/17 18:06:46  fisyak
#  Add dropit for PATH
#
#  Revision 1.8  1998/02/14 02:08:20  fisyak
#  Add silent mode for make
#
#  Revision 1.7  1998/02/13 14:18:19  fisyak
#  Simplify Makefile, reduce SLibrary
#
#  Revision 1.5  1998/02/10 00:06:07  fisyak
#  SL98a second version
#
#  Revision 1.4  1998/01/31 23:32:51  fisyak
#  New Environment variables
#
#  Revision 1.3  1998/01/30 12:42:14  fisyak
#  Save changes before moving to SL97b
#
#  Revision 1.2  1998/01/01 03:28:11  fisyak
#  New make.kumac
#
#  Revision 1.1.1.1  1997/12/31 14:35:23  fisyak
#
#           Last modification $Date: 1998/09/18 00:32:14 $ 
#  default setings
# Current Working Directory
#
CWD := $(shell pwd)
ifdef SILENT
.SILENT:
endif       
include $(STAR)/mgr/MakeEnv.mk
include $(STAR)/mgr/MakeArch.mk
EMPTY      :=
ZERO       :=0
ONE        :=1
TWO        :=2
THREE      :=3
FOUR       :=4
FIVE       :=5
ifndef MakePam
MakePam = $(STAR)/mgr/Makeloop.mk
Makepam = $(STAR)/mgr/MakePam.mk
MakeDll = $(STAR)/mgr/MakeDll.mk
endif          
ifndef INP_DIR 
INP_DIR := $(CWD)
endif           
NAME    := $(notdir $(INP_DIR))
# define level pams -> domain -> package from *.idl and *.g files
#======================= level ===========================
PAMS    := pams
pams    := $(findstring $(PAMS),$(INP_DIR))
LEVEL   := $(words  $(subst /, ,$(subst $(word 1, $(subst /pams, ,$(INP_DIR))),, $(INP_DIR))))
ROOT_DIR:= $(word 1,$(subst /pams, ,$(INP_DIR)))
#___________ non PAM ___________________
ifeq ($(LEVEL),$(ZERO))
LEVEL   := $(words  $(subst /, ,$(subst $(word 1, $(subst /StRoot, ,$(INP_DIR))),, $(INP_DIR))))
endif
ifeq ($(LEVEL),$(ZERO))
	SUBDIRS :=$(shell test -d pams && echo pams)
else
	DIRS    := $(strip $(wildcard *))
	SUBDIRS := $(foreach dir, $(DIRS), $(shell test -d $(dir) && echo $(dir))) 
	SUBDIRS := $(filter-out inc, $(SUBDIRS))
	SUBDIRS := $(filter-out idl, $(SUBDIRS))
	SUBDIRS := $(filter-out doc, $(SUBDIRS))
	SUBDIRS := $(filter-out CVS, $(SUBDIRS))
	SUBDIRS := $(filter-out wrk, $(SUBDIRS))
	SUBDIRS := $(filter-out src, $(SUBDIRS))
	SUBDIRS := $(filter-out exa, $(SUBDIRS))
	SUBDIRS := $(filter-out kumac,$(SUBDIRS))
	SUBDIRS := $(filter-out html,$(SUBDIRS))
	SUBDIRS := $(strip    $(sort $(SUBDIRS)))
        SUBDIRS := $(filter-out util, $(SUBDIRS))
ifneq (,$(findstring $(STAF_ARCH),hp_ux102))
        SUBDIRS := $(filter-out trg, $(SUBDIRS))
endif
ifneq (,$(findstring sim, $(SUBDIRS)))
        SUBDIRS := $(filter-out sim, $(SUBDIRS))
        DIRS    := $(strip $(wildcard sim/*))
        SUBDIRS += $(foreach dir, $(DIRS), $(shell test -d $(dir) && \
                test $(dir) != sim/CVS && test $(dir) != sim/kumac && \
                test $(dir) != sim/inc && test $(dir) != sim/idl && \
                echo $(dir))) 
endif
ifneq (,$(findstring gen, $(SUBDIRS)))
        SUBDIRS := $(filter-out gen, $(SUBDIRS))
        DIRS    := $(strip $(wildcard gen/*))
        SUBDIRS += $(foreach dir, $(DIRS), $(shell test -d $(dir) && \
                test $(dir) != gen/CVS && test $(dir) != gen/kumac && \
                test $(dir) != gen/inc && test $(dir) != gen/idl && \
                echo $(dir))) 
endif
#. take out trg for sgi
ifneq (,$(findstring $(STAF_ARCH),sgi_64 ))
        SUBDIRS := $(filter-out trg, $(SUBDIRS))
endif
endif
ifeq ($(LEVEL), $(TWO))  #default is domain
DOM_DIR := $(CWD)
PKG     := $(notdir $(DOM_DIR))
ifeq (gen,$(PKG))              
PKG     :=
endif                          
ifeq (sim,$(PKG))              
PKG     :=
endif                          
endif
ifeq ($(LEVEL), $(THREE)) #package level
DOM_DIR := $(shell cd $(INP_DIR)/../; pwd)
PKG     := $(NAME)
SUBDIRS:=
endif                       
ifeq ($(LEVEL),$(FOUR)) #subpackage level
DOM_DIR := $(shell cd $(INP_DIR)/../../; pwd)
PKG     := $(NAME)
endif
ifndef DOMAIN
DOMAIN  := $(notdir $(DOM_DIR))                          
endif
SYS_DIR := $(ROOT_DIR)/.$(STAR_HOST_SYS)
LIB_DIR := $(SYS_DIR)/lib
SO_LIB  := $(LIB_DIR)/St_$(PKG).so
ifeq ($(LEVEL),$(FIVE))        
.PHONY               : default
all:
	@echo "Please run make in parent directory"
else                           
ifndef ROOT_DIR                 
	override ROOT_DIR := $(shell cd $(ROOT_DIR); pwd)
endif                          
ifeq ($(NAME),$(PKG))          
	SUBDIRS :=
endif                          
ifneq ($(EMPTY),$(findstring $(LEVEL),0 1))
ifneq ($(EMPTY),$(wildcard $(ROOT_DIR)/StRoot/base))
BASE := St_base 
endif
ifneq ($(EMPTY),$(wildcard $(ROOT_DIR)/StRoot/xdf2root))
XDF2ROOT := xdf2root
endif
ifneq ($(EMPTY),$(SUBDIRS))     
TARGETS += $(addsuffix _all, $(SUBDIRS))
endif
ifneq ($(EMPTY),$(wildcard $(ROOT_DIR)/.share/tables))
TARGETS += St_Tables
endif
ifneq ($(EMPTY),$(wildcard $(ROOT_DIR)/StRoot/StChain))
TARGETS += StChain
endif
ifneq ($(EMPTY),$(wildcard $(ROOT_DIR)/StRoot/St_*_Maker))
TARGETS += St_Makers
endif
Makers  :=  $(notdir $(wildcard $(ROOT_DIR)/StRoot/St_*_Maker))
Makers  :=  $(filter-out St_calib_Maker, $(Makers))
#Makers  :=  $(filter-out St_dst_Maker, $(Makers))
Makers  :=  $(filter-out St_ebye_Maker, $(Makers))
#          I have subdrs
.PHONY               :  all $(BASE) $(XDF2ROOT) $(TARGET) St_Tables StChain test clean clean_lib clean_share clean_obj
#      I_have_subdirs
all:  $(BASE) $(XDF2ROOT)  $(TARGETS)
ifndef NOROOT
ROOT:      St_base xdf2root St_Makers StChain St_Tables
St_base:
	$(MAKE) -f $(MakeDll) -C $(ROOT_DIR)/StRoot/base  SO_LIB=$(ROOT_DIR)/.$(STAR_SYS)/lib/St_base.so
xdf2root:
	$(MAKE) -f $(MakeDll) -C $(ROOT_DIR)/StRoot/xdf2root    SO_LIB=$(ROOT_DIR)/.$(STAR_SYS)/lib/xdf2root.so 
St_Makers: $(Makers)
StChain:   
	$(MAKE) -f $(MakeDll) -C $(ROOT_DIR)/StRoot/StChain    SO_LIB=$(ROOT_DIR)/.$(STAR_SYS)/lib/StChain.so
St_Tables:
	$(MAKE) -f $(MakeDll) -C $(ROOT_DIR)/.share/tables  SO_LIB=$(ROOT_DIR)/.$(STAR_SYS)/lib/St_Tables.so NODEBUG=YES
ifneq ($(EMPTY),$(findstring $(STAR_LEVEL),dev))
St_TablesDoc: 
	root.exe -b -q MakeHtmlTables.cxx
endif
St_%_Maker: 
	$(MAKE) -f $(MakeDll) -C $(ROOT_DIR)/StRoot/St_$(STEM)_Maker  SO_LIB=$(ROOT_DIR)/.$(STAR_SYS)/lib/St_$(STEM)_Maker.so
endif
%_all:  $(ROOT_DIR)/.$(STAR_SYS)/lib/St_base.so
	$(MAKE) -f $(MakePam) -C $(STEM) $(MAKFLAGS) 
test:  $(BASE) $(addsuffix _test, $(SUBDIRS))
%_test: 
	$(MAKE) -f $(MakePam) -C $(STEM) test $(MAKFLAGS) 
clean: $(addsuffix _clean, $(SUBDIRS))
%_clean: 
	$(MAKE) -f $(MakePam) -C $(STEM) clean $(MAKFLAGS) 
clean_lib: $(addsuffix _clean_lib, $(SUBDIRS))
%_clean_lib: 
	$(MAKE) -f $(MakePam) -C $(STEM) clean_lib $(MAKFLAGS) 
clean_share: $(addsuffix _clean_share, $(SUBDIRS))
%_clean_share: 
	$(MAKE) -f $(MakePam) -C $(STEM) clean_share $(MAKFLAGS) 
clean_obj: $(addsuffix _clean_obj, $(SUBDIRS))
%_clean_obj: 
	$(MAKE) -f $(MakePam) -C $(STEM) clean $(MAKFLAGS) 
else # I have no subdirs
PKG     := $(notdir $(shell pwd))
GEN_DIR := $(ROOT_DIR)/.share/$(PKG)
GEN_TAB := $(DIR_GEN)/tables
.PHONY               : default clean clean_lib clean_share clean_obj test
all:
	$(MAKE) -f $(Makepam) $(MAKFLAGS)
ifndef NOROOT
	$(MAKE) -f $(MakeDll) $(MAKFLAGS) -C  $(GEN_DIR) SO_LIB=$(SO_LIB)
 endif
clean:;      $(MAKE) -f $(Makepam) $(MAKFLAGS)  clean
clean_lib:;  $(MAKE) -f $(Makepam) $(MAKFLAGS)  clean_lib
clean_share:;$(MAKE) -f $(Makepam) $(MAKFLAGS)  clean_share
clean_obj:;  $(MAKE) -f $(Makepam) $(MAKFLAGS)  clean_obj
clean_test:; $(MAKE) -f $(Makepam) $(MAKFLAGS)  test
endif
endif
test: test_level
test_level:
	@echo "LEVEL     =" $(LEVEL)
	@echo "SUBDIRS   =" $(SUBDIRS)
	@echo "INP_DIR   =" $(INP_DIR)
	@echo "ROOT_DIR  =" $(ROOT_DIR)
	@echo "CWD       =" $(CWD)
	@echo "NAME      =" $(NAME)
	@echo "ROOT_DIR  =" $(ROOT_DIR)
	@echo "TARGETS   =" $(TARGETS)