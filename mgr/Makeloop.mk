#  $Log: Makeloop.mk,v $
#  Revision 1.48  1999/01/31 23:12:08  fisyak
#  Cleanup St Wrapper libraries
#
#  Revision 1.47  1999/01/31 20:54:57  fisyak
#  Fix bugs
#
#  Revision 1.46  1999/01/30 04:08:23  fisyak
#  Add StRootEvent
#
#  Revision 1.45  1999/01/28 16:49:59  fisyak
#  remove copying of h-files for rootcint
#
#  Revision 1.44  1999/01/27 23:46:29  fisyak
#  Add Templates
#
#  Revision 1.43  1999/01/25 23:49:14  fisyak
#  Add MAKEFLAG
#
#  Revision 1.42  1999/01/20 02:16:52  fisyak
#  Active STAR_HOST_SYS for egcs
#
#  Revision 1.41  1999/01/14 13:56:41  fisyak
#  Add Victors MakeFun.mk, Add StMagF
#
#  Revision 1.40  1999/01/04 16:24:21  fisyak
#  Add StDisplay
#
#  Revision 1.39  1998/12/17 17:21:00  fisyak
#  Add Akio's insure++
#
#  Revision 1.38  1998/12/16 16:38:39  fisyak
#  Add gstar to Root
#
#  Revision 1.37  1998/12/12 00:58:42  fisyak
#  remove STAF
#
#  Revision 1.36  1998/12/10 22:48:01  fine
#  Correction for fit Window NT branch
#
#  Revision 1.35  1998/12/04 01:17:30  fisyak
#  fix for fortran source in StRoot
#
#  Revision 1.34  1998/12/02 20:42:37  perev
#  cleanup
#
#  Revision 1.33  1998/12/02 20:01:51  fisyak
#  More NT
#
#  Revision 1.32  1998/12/01 01:53:29  fisyak
#  Merge with NT
#
#  Revision 1.31  1998/11/27 20:01:38  fisyak
#  put l3 after trg (bug in usage of idl)
#
#  Revision 1.30  1998/11/25 21:51:08  fisyak
#  remove tcc trg and l3 from directories list for hp_ux102
#
#  Revision 1.29  1998/11/15 21:12:58  fisyak
#  fix shared libraries versioning for St_Root
#
#  Revision 1.28  1998/11/14 01:16:59  fisyak
#  Post NT updates
#
#  Revision 1.27  1998/11/13 15:48:44  fisyak
#  Merged version with NT
#
#  Revision 1.26  1998/11/13 00:19:31  fisyak
#  Add flags for SCL St_trs_Maker
#
#  Revision 1.25  1998/11/05 20:09:24  fisyak
#  add OBJY CPPFLAGS
#
#  Revision 1.24  1998/10/29 23:34:26  fisyak
#  set ASU_MALLOC_OFF for PAMS
#
#  Revision 1.23  1998/10/08 15:57:37  fisyak
#  Remove trailing blanks
#
#  Revision 1.22  1998/10/08 15:39:51  perev
#  add strip
#
#  Revision 1.21  1998/10/08 15:37:37  perev
#  add strip
#
#  Revision 1.20  1998/10/07 20:23:47  perev
#  cleanup of .mk
#
#  Revision 1.18  1998/09/22 02:21:32  fisyak
#  Fix NOROOT version
#
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
#           Last modification $Date: 1999/01/31 23:12:08 $ 
#  default setings
# Current Working Directory
#
ifdef SILENT
.SILENT:.
endif       

ifdef STAR_MAKE_HOME
  include $(STAR_MAKE_HOME)/MakeEnv.mk
  include $(STAR_MAKE_HOME)/MakeArch.mk
else
  include $(STAR)/mgr/MakeEnv.mk
  include $(STAR)/mgr/MakeArch.mk
endif

EMPTY      :=
ZERO       :=0
ONE        :=1
TWO        :=2
THREE      :=3
FOUR       :=4
FIVE       :=5

ifndef STAR_MAKE_HOME
  STAR_MAKE_HOME := $(STAR)/mgr
endif

ifndef MakePam
  MakePam := $(STAR_MAKE_HOME)/MakePam.mk
endif
ifndef MakeFun
  MakeFun := $(STAR_MAKE_HOME)/MakeFun.mk
endif

ifndef MakeDll
MakeDll :=$(STAR_MAKE_HOME)/MakeDll.mk
endif

ifndef Makeloop
Makeloop :=$(STAR_MAKE_HOME)/Makeloop.mk
endif

export Makeloop
export MakeDll
export MakePam
export MakeFun
ifndef INP_DIR 
  INP_DIR := $(CWD)
endif           
NAME    := $(notdir $(INP_DIR))
MAKFLAGS := $(filter-out w, $(MAKEFLAGS))
#MAKFLAGS := $(filter-out s, $(MAKFLAGS))
#MAKFLAGS := $(filter-out --, $(MAKFLAGS))
# define level pams -> domain -> package from *.idl and *.g files
#======================= level ===========================
PAMS    := $(findstring /pams,$(INP_DIR))
ifndef PAMS
  PAMS    := $(findstring /StRoot,$(INP_DIR))
endif
ROOT_DIR:= $(word 1,$(subst $(PAMS), ,$(INP_DIR)))
LEVEL   := $(words  $(subst /, ,$(subst $(ROOT_DIR),, $(INP_DIR))))

ifeq ($(LEVEL),$(ZERO))
	SUBDIRS := $(wildcard pams)
else
	DIRS    := $(subst /.,,$(strip $(wildcard */.)))
#	SUBDIRS := $(foreach dir, $(DIRS), $(shell test -d $(dir) && echo $(dir))) 
	SUBDIRS := $(filter-out inc idl doc CVS wrk src exa kumac html, $(DIRS))
	SUBDIRS := $(strip    $(sort $(SUBDIRS)))
ifndef NT
        SUBDIRS := $(filter-out util, $(SUBDIRS))
else
	SUBDIRS := $(filter-out strange, $(SUBDIRS))
endif
ifneq (,$(findstring sim, $(SUBDIRS)))
        SUBDIRS := $(filter-out sim, $(SUBDIRS))
        DIRS    := $(subst /.,,$(strip $(wildcard sim/*/.)))
        SUBDIRS += $(filter-out sim/CVS sim/kumac sim/idl sim/inc,$(DIRS))
endif
ifneq (,$(findstring gen, $(SUBDIRS)))
        SUBDIRS := $(filter-out gen, $(SUBDIRS))
        DIRS    := $(subst /.,,$(strip $(wildcard gen/*/.)))
        SUBDIRS += $(filter-out gen/CVS gen/kumac gen/inc gen/idl,$(DIRS))
endif
#. take out trg for sgi
ifneq (,$(findstring $(STAR_SYS),sgi_64 ))
#        SUBDIRS := $(filter-out trg, $(SUBDIRS))
endif
ifdef NT
SUBDIRS := $(filter-out db, $(SUBDIRS)) 
SUBDIRS := $(filter-out sim, $(SUBDIRS))
SUBDIRS := $(filter-out g2t, $(SUBDIRS))
SUBDIRS := $(filter-out gstar, $(SUBDIRS))
SUBDIRS := $(filter-out dig, $(SUBDIRS))
SUBDIRS := $(filter-out trg, $(SUBDIRS))
endif
SUBDIRS := $(filter-out global, $(SUBDIRS)) $(filter global, $(SUBDIRS))
SUBDIRS := $(filter util, $(SUBDIRS)) $(filter-out util, $(SUBDIRS)) 
SUBDIRS := $(filter-out l3, $(SUBDIRS)) $(filter l3, $(SUBDIRS))
SUBDIRS := $(filter-out objy, $(SUBDIRS))
ifneq (,$(findstring $(STAR_SYS),hp_ux102 ))
         SUBDIRS := $(filter-out tcc, $(SUBDIRS))
         SUBDIRS := $(filter-out trg, $(SUBDIRS))
         SUBDIRS := $(filter-out l3, $(SUBDIRS))
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
ifndef SUBDIRS
SUBDIRS := ./
endif         
endif
ifeq ($(LEVEL), $(THREE)) #package level
DOM_DIR := $(subst / ,,$(dir $(INP_DIR)/../) )
PKG     := $(NAME)
SUBDIRS:=
endif                       
ifeq ($(LEVEL),$(FOUR)) #subpackage level
DOM_DIR := $(subst / ,,$(INP_DIR)/../../) )
PKG     := $(NAME)
endif
ifndef DOMAIN
DOMAIN  := $(notdir $(DOM_DIR))                          
endif
SYS_DIR := $(ROOT_DIR)/.$(STAR_HOST_SYS)
LIB_DIR := $(SYS_DIR)/lib
SO_LIB  := $(LIB_DIR)/St_$(PKG).$(So)
ifeq ($(LEVEL),$(FIVE))        
.PHONY               : default
all:
	@echo "Please run make in parent directory"
else                           
ifndef ROOT_DIR                 
	override ROOT_DIR := $(subst /.,,$(wildcard $(ROOT_DIR)/.))
endif                          
ifeq ($(NAME),$(PKG))          
	SUBDIRS :=
endif                          
ifneq ($(EMPTY),$(findstring $(LEVEL),0 1))
ifneq ($(EMPTY),$(SUBDIRS))     
TARGETS += $(addsuffix _all, $(SUBDIRS))
endif
ifndef NOROOT
ifneq ($(EMPTY),$(wildcard $(ROOT_DIR)/StRoot/base))
BASE := St_base 
endif
ifndef NT
ifneq ($(EMPTY),$(wildcard $(ROOT_DIR)/StRoot/xdf2root))
XDF2ROOT := xdf2root
endif
endif
ifneq ($(EMPTY),$(wildcard $(ROOT_DIR)/.share/tables))
StRoot += St_Tables
endif
ifneq ($(EMPTY),$(wildcard $(ROOT_DIR)/StRoot/StChain))
StRoot += StChain
endif
Makers  :=  $(notdir $(wildcard $(ROOT_DIR)/StRoot/St*Maker)) 
#Makers  :=  $(notdir $(wildcard $(ROOT_DIR)/StRoot/St*)) 
#ifneq (,$(findstring $(STAR_SYS),sun4x_55 sun4x_56 hp_ux102))
ifneq (,$(findstring $(STAR_SYS),hp_ux102))
Makers  :=  $(filter-out StTrsMaker, $(Makers))
endif
Makers  :=  $(filter-out St_ebye_Maker, $(Makers))
Makers  :=  $(filter-out St_laser_Maker, $(Makers))
Makers  :=  $(filter-out St_mev_Maker, $(Makers))
Makers  :=  $(filter-out St_hbt_Maker, $(Makers))
Makers  :=  $(filter-out StEventReaderMaker, $(Makers))
ifneq ($(EMPTY),$(Makers))
StRoot += St_Makers
endif
ifneq ($(EMPTY),$(wildcard $(ROOT_DIR)/StRoot/StDisplay))
StRoot += StDisplay
endif
ifneq ($(EMPTY),$(wildcard $(ROOT_DIR)/StRoot/StMagF))
StRoot += StDisplay
endif

endif
#          I have subdrs
.PHONY               :  all $(TARGET) ROOT test clean clean_lib clean_share clean_obj
#      I_have_subdirs
all:    $(TARGETS) ROOT
ifndef NOROOT
ROOT:   $(BASE) $(XDF2ROOT)  $(StRoot)
St_base:
	$(MAKE)  -f $(MakeDll) -C $(ROOT_DIR)/StRoot/base     SO_LIB=$(ROOT_DIR)/.$(STAR_HOST_SYS)/$(SO_SUBDIR)/St_base.$(So)
xdf2root:
	$(MAKE)  -f $(MakeDll) -C $(ROOT_DIR)/StRoot/xdf2root SO_LIB=$(ROOT_DIR)/.$(STAR_HOST_SYS)/$(SO_SUBDIR)/xdf2root.$(So) 
StSclRoot:
	$(MAKE)  -f $(MakeDll) -C $(ROOT_DIR)/StRoot/StSclRoot SO_LIB=$(ROOT_DIR)/.$(STAR_HOST_SYS)/$(SO_SUBDIR)/StSclRoot.$(So) 
StDisplay:
	$(MAKE)  -f $(MakeDll) -C $(ROOT_DIR)/StRoot/StDisplay SO_LIB=$(ROOT_DIR)/.$(STAR_HOST_SYS)/$(SO_SUBDIR)/StDisplay.$(So) 
StMakers: St_Makers
St_Makers: $(Makers)
StChain:   
	$(MAKE)  -f $(MakeDll) -C $(ROOT_DIR)/StRoot/StChain  SO_LIB=$(ROOT_DIR)/.$(STAR_HOST_SYS)/$(SO_SUBDIR)/StChain.$(So)
St_Tables:
	$(MAKE)  -f $(MakeDll) -C $(ROOT_DIR)/.share/tables   SO_LIB=$(ROOT_DIR)/.$(STAR_HOST_SYS)/$(SO_SUBDIR)/St_Tables.$(So) NODEBUG=YES
StMagF: 
#	$(MAKE) -f $(MakeFun) fun=StMagF -C $(ROOT_DIR)/StRoot/StMagF   SO_LIB=$(ROOT_DIR)/.$(STAR_HOST_SYS)/$(SO_SUBDIR)/StMagF.$(So)
	$(MAKE)  -f $(MakeDll) -C $(ROOT_DIR)/StRoot/StMagF  SO_LIB=$(ROOT_DIR)/.$(STAR_HOST_SYS)/$(SO_SUBDIR)/StMagF.$(So)
StRootEvent: 
	$(MAKE)  -f $(MakeDll) -C $(ROOT_DIR)/StRoot/StRootEvent  SO_LIB=$(ROOT_DIR)/.$(STAR_HOST_SYS)/$(SO_SUBDIR)/StRootEvent.$(So)
ifneq ($(EMPTY),$(findstring $(STAR_LEVEL),dev))
St_TablesDoc: 
	root.exe -b -q MakeHtmlTables.cxx
endif
St%Maker: 
	$(MAKE)  -f $(MakeDll) -C $(ROOT_DIR)/StRoot/St$(STEM)Maker  SO_LIB=$(ROOT_DIR)/.$(STAR_HOST_SYS)/$(SO_SUBDIR)/St$(STEM)Maker.$(So)
endif
%_all:  $(BASE)
	$(MAKE)  -f $(Makeloop) -C $(STEM) $(MAKFLAGS) 
test:  $(BASE) $(addsuffix _test, $(SUBDIRS))
%_test: 
	$(MAKE)  -f $(Makeloop) -C $(STEM) test $(MAKFLAGS) 
clean: $(addsuffix _clean, $(SUBDIRS))
%_clean: 
	$(MAKE)  -f $(Makeloop) -C $(STEM) clean $(MAKFLAGS) 
clean_lib: $(addsuffix _clean_lib, $(SUBDIRS))
%_clean_lib: 
	$(MAKE)  -f $(Makeloop) -C $(STEM) clean_lib $(MAKFLAGS) 
clean_share: $(addsuffix _clean_share, $(SUBDIRS))
%_clean_share: 
	$(MAKE)  -f $(Makeloop) -C $(STEM) clean_share $(MAKFLAGS) 
clean_obj: $(addsuffix _clean_obj, $(SUBDIRS))
%_clean_obj: 
	$(MAKE)  -f $(Makeloop) -C $(STEM) clean $(MAKFLAGS) 
else # I have no subdirs
PKG     := $(notdir $(shell $(PWD)))
GEN_DIR := $(ROOT_DIR)/.share/$(PKG)
GEN_TAB := $(DIR_GEN)/tables
.PHONY               : default clean clean_lib clean_share clean_obj test
all:
	$(MAKE)  -f $(MakePam) $(MAKFLAGS)
ifndef NOROOT
	$(MAKE)  -f $(MakeDll) $(MAKFLAGS) -C  $(GEN_DIR) SO_LIB=$(SO_LIB)
endif
clean:;      $(MAKE)  -f $(MakePam) $(MAKFLAGS)  clean
clean_lib:;  $(MAKE)  -f $(MakePam) $(MAKFLAGS)  clean_lib
clean_share:;$(MAKE)  -f $(MakePam) $(MAKFLAGS)  clean_share
clean_obj:;  $(MAKE)  -f $(MakePam) $(MAKFLAGS)  clean_obj
clean_test:; $(MAKE)  -f $(MakePam) $(MAKFLAGS)  test
endif
endif
w:
test: test_level
test_level:
	@echo MAKELEVEL = $(MAKELEVEL) 
	@echo MAKEFILE  = $(MAKEFILE) 
	@echo MAKFLAGS  = $(MAKFLAGS) 
	@echo "PWD       = |"$(PWD)"|"
	@echo "LEVEL     = |"$(LEVEL)"|"
	@echo "SUBDIRS   = |"$(SUBDIRS)"|"
	@echo "INP_DIR   = |"$(INP_DIR)"|"
	@echo "ROOT_DIR  = |"$(ROOT_DIR)"|"
	@echo "CWD       = |"$(CWD)"|"
	@echo "NAME      = |"$(NAME)"|"
	@echo "ROOT_DIR  = |"$(ROOT_DIR)"|"
	@echo "DOM_DIR   = |"$(DOM_DIR)"|"
	@echo "TARGETS   = |"$(TARGETS)"|"
	@echo "Makeloop  = |"$(Makeloop)"|"	
	@echo "MakePam   = |"$(MakePam)"|"	
	@echo "MakeDll   = |"$(MakeDll)"|"
	@echo "STAR_MAKE_HOME=|"$(STAR_MAKE_HOME)"|"
	@echo "DIRS=|"$(DIRS)"|"
	@echo "Makers    = |"$(Makers)"|"
#	@echo "NT        ="$(NT)"
