#  $Log: Makeloop.mk,v $
#  Revision 1.91  1999/08/17 14:45:35  fisyak
#  take out StAssociationMaker for SUN/HP
#
#  Revision 1.90  1999/08/16 16:31:33  fisyak
#  Simplify Makefiles
#
#  Revision 1.89  1999/08/14 00:37:35  fisyak
#  New Cons stuff
#
#  Revision 1.88  1999/07/20 00:24:55  fisyak
#  Remove Objy
#
#  Revision 1.87  1999/07/16 23:22:10  fisyak
#  Clean up list of skipped directories
#
#  Revision 1.86  1999/07/16 18:08:21  fisyak
#  Remove Makers
#
#  Revision 1.85  1999/07/16 17:59:09  fisyak
#  Add cons
#
#  Revision 1.84  1999/07/13 15:54:13  fisyak
#  Add SKIP_DIRS
#
#  Revision 1.83  1999/07/13 14:41:42  fisyak
#  Add SKIP_LIB env. varibale to skip unwanted directory
#
#  Revision 1.82  1999/07/10 19:06:33  fisyak
#  Fix St_base
#
#  Revision 1.81  1999/07/07 14:08:37  fisyak
#  Extract dependencies from standard make path, add pathes to StarClassLibrary and MySql
#
#  Revision 1.80  1999/06/27 22:44:03  fisyak
#  Merge StRootEvent and StEvent
#
#  Revision 1.79  1999/06/24 18:00:20  fisyak
#  Take out StDaqLib for sun
#
#  Revision 1.78  1999/06/16 12:37:03  fisyak
#  Changes for egcs-1.1.2 on Solaris
#
#  Revision 1.77  1999/06/11 12:47:09  fisyak
#  Add rtti & exceptions, more fixes for StDaqLib
#
#  Revision 1.76  1999/06/08 11:30:14  fisyak
#  take out NT stuff for the moment
#
#  Revision 1.75  1999/05/10 19:43:38  fisyak
#  Add Victor test for last library
#
#  Revision 1.74  1999/05/01 18:50:42  fisyak
#  put ctf as last pams to build
#
#  Revision 1.73  1999/04/30 13:22:06  fisyak
#  put StRootEvent into loop
#
#  Revision 1.72  1999/04/24 13:15:25  fisyak
#  Add --sillent mode for set SILENT environmnet variable
#
#  Revision 1.71  1999/04/22 23:44:28  fisyak
#  put St_io_Maker back in
#
#  Revision 1.70  1999/04/18 23:36:05  fisyak
#  Add -lpgc for new pgf77
#
#  Revision 1.69  1999/04/18 21:22:28  wenaus
#  Builds HTML to browse CVS commit history
#
#  Revision 1.68  1999/04/13 20:36:21  fisyak
#  clean up St_run_summary_Maker
#
#  Revision 1.67  1999/04/06 21:26:26  fisyak
#  take out StDisplay
#
#  Revision 1.66  1999/04/02 22:59:01  fisyak
#  filter-out St_laser_Maker St_run_summary_Maker St_tpctest_Maker
#
#  Revision 1.65  1999/04/02 22:56:56  fisyak
#  Remove Objy if OBJY_HOME is not defined
#
#  Revision 1.64  1999/03/30 21:46:26  didenko
#  unfilter some makers
#
#  Revision 1.63  1999/03/21 20:41:01  fisyak
#  Cleanup for SL99d
#
#  Revision 1.62  1999/03/12 01:33:41  fisyak
#  Take out -lI77 -lF77 for RedHat 5.1/5.2
#
#  Revision 1.61  1999/03/10 20:56:52  fisyak
#  Cleanup for SL99c tag
#
#  Revision 1.60  1999/02/25 22:24:40  fisyak
#  Add ROOTCINTD flag
#
#  Revision 1.59  1999/02/23 01:07:12  fisyak
#  Cleanup for SL99a
#
#  Revision 1.58  1999/02/19 15:07:56  fisyak
#  take out StTrsMaker from main loop
#
#  Revision 1.57  1999/02/19 14:40:14  fisyak
#  remove extra dependencies for idls
#
#  Revision 1.56  1999/02/16 15:37:38  fisyak
#  Clean up HP stuff
#
#  Revision 1.55  1999/02/14 23:10:10  fisyak
#  split tables for HP, remove duplicates for root4star
#
#  Revision 1.54  1999/02/12 22:12:21  fisyak
#  Fix STAR_OBJ_DIR for nondebug version
#
#  Revision 1.53  1999/02/12 02:50:33  fisyak
#  Fix St_Tables, single module
#
#  Revision 1.52  1999/02/10 14:12:43  fisyak
#  Turn on StEventReaderMaker and StTrsMaker
#
#  Revision 1.51  1999/02/10 02:19:58  fisyak
#  put back l3 and trg
#
#  Revision 1.50  1999/02/09 19:14:51  fisyak
#  Add objy
#
#  Revision 1.49  1999/02/08 02:29:20  fisyak
#  New Makefile scheme
#
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
#           Last modification $Date: 1999/08/17 14:45:35 $ 
#  default setings
# Current Working Directory
#
ifndef STAR_MAKE_HOME
  STAR_MAKE_HOME := $(STAR)/mgr
endif

include $(STAR_MAKE_HOME)/MakeEnv.mk
include $(STAR_MAKE_HOME)/MakeDirs.mk
ifneq (,$(wildcard StRoot/St_base))
  BASE := St_base
endif
ifeq ($(LEVEL),$(ZERO))
  SUBDIRS :=$(wildcard pams) $(wildcard StRoot)
  ifneq ($(EMPTY),$(wildcard $(ROOT_DIR)/.share/tables))
    St_Tables := St_Tables
  endif
endif
ifndef SUBDIRS
  DIRS    := $(subst /.,,$(strip $(wildcard */.)))
  SUBDIRS := $(DIRS)
  SUBDIRS := $(filter-out inc idl doc CVS wrk src exa kumac html test macros tmp, $(DIRS))
  SUBDIRS := $(strip    $(sort $(SUBDIRS)))
  SUBDIRS := $(filter StChain, $(SUBDIRS)) $(filter-out StChain, $(SUBDIRS)) 
  SUBDIRS := $(filter-out ctf, $(SUBDIRS)) $(filter ctf, $(SUBDIRS)) 
  SUBDIRS := $(filter St_base, $(SUBDIRS)) $(filter-out St_base, $(SUBDIRS)) 
  ifdef NT
    SUBDIRS := $(filter-out db sim g2t gstar dig trg strange, $(SUBDIRS))
  endif
  SUBDIRS := $(filter-out global, $(SUBDIRS)) $(filter global, $(SUBDIRS))
  SUBDIRS := $(filter util, $(SUBDIRS)) $(filter-out util, $(SUBDIRS)) 
  ifneq (,$(findstring $(STAR_SYS),sun4x_56 hp_ux102))
    SUBDIRS := $(filter-out StPeCMaker StHbtMaker StAssociationMaker, $(SUBDIRS))
  endif
  ifdef SKIP_LIB
    SUBDIRS := $(filter-out $(SKIP_LIB),  $(SUBDIRS))
  endif
  ifdef SKIP_DIRS
    SUBDIRS := $(filter-out $(SKIP_DIRS),  $(SUBDIRS))
  endif
  ifneq (,$(findstring $(PKG),sim gen))
   PKG :=
  endif
  ifneq (,$(strip $(PKG)))
    SUBDIRS:=
  endif
endif
#          I have subdrs
.PHONY               :  all $(BASE)  $(St_Tables) test clean clean_lib clean_share clean_obj
all:    $(BASE) $(addsuffix _loop, $(SUBDIRS))  $(addsuffix _$(branch),$(PKG)) $(St_Tables)
$(BASE): 
	$(MAKE)  -f $(MakeDll) -C $(ROOT_DIR)/StRoot/$(BASE) depend NODEPEND=1999
	$(MAKE)  -f $(MakeDll) -C $(ROOT_DIR)/StRoot/$(BASE)
St_TablesDoc: 
	root.exe -b -q MakeHtmlTables.cxx
%_loop:
	$(MAKE)  -f $(Makeloop) -C $(STEM)
%_pams:
	$(MAKE)  -f $(MakePam) depend NODEPEND=1999
	$(MAKE)  -f $(MakePam)
	$(MAKE)  -f $(MakeDll) -C  $(GEN_DIR) depend NODEPEND=1999
	$(MAKE)  -f $(MakeDll) -C  $(GEN_DIR)
%_StRoot:
	$(MAKE)  -f $(MakeDll) depend NODEPEND=1999
	$(MAKE)  -f $(MakeDll)
St_Tables:
	$(MAKE)  -f $(MakeDll) -C $(ROOT_DIR)/.share/tables depend NODEPEND=1999
	$(MAKE)  -f $(MakeDll) -C $(ROOT_DIR)/.share/tables \
         SO_LIB=$(ROOT_DIR)/.$(STAR_HOST_SYS)/$(SO_SUBDIR)/St_Tables.$(So) NODEBUG=1999
test:   $(addsuffix _test, $(SUBDIRS))
%_test:
	$(MAKE)  -f $(Makeloop) -C $(STEM) test 
clean: $(addsuffix _clean, $(SUBDIRS))
%_clean: 
	$(MAKE)  -f $(Makeloop) -C $(STEM) clean 
clean_lib: $(addsuffix _clean_lib, $(SUBDIRS))
%_clean_lib: 
	$(MAKE)  -f $(Makeloop) -C $(STEM) clean_lib 
clean_share: $(addsuffix _clean_share, $(SUBDIRS))
%_clean_share: 
	$(MAKE)  -f $(Makeloop) -C $(STEM) clean_share 
clean_obj: $(addsuffix _clean_obj, $(SUBDIRS))
%_clean_obj: 
	$(MAKE)  -f $(Makeloop) -C $(STEM) clean 
clean:;      $(MAKE)  -f $(MakePam)  clean
clean_lib:;  $(MAKE)  -f $(MakePam)  clean_lib
clean_share:;$(MAKE)  -f $(MakePam)  clean_share
clean_obj:;  $(MAKE)  -f $(MakePam)  clean_obj
clean_test:; $(MAKE)  -f $(MakePam)  test
test: test_level
test_level:
	@echo MAKELEVEL = $(MAKELEVEL) 
	@echo MAKEFILE  = $(MAKEFILE) 
	@echo "PWD       = |"$(PWD)"|"
	@echo "LEVEL     = |"$(LEVEL)"|"
	@echo "SUBDIRS   = |"$(SUBDIRS)"|"
	@echo "INP_DIR   = |"$(INP_DIR)"|"
	@echo "ROOT_DIR  = |"$(ROOT_DIR)"|"
	@echo "CWD       = |"$(CWD)"|"
	@echo "NAME      = |"$(NAME)"|"
	@echo "ROOT_DIR  = |"$(ROOT_DIR)"|"
	@echo "ROOT_LVL  = |"$(ROOT_LVL)"|"
	@echo "DOM_DIR   = |"$(DOM_DIR)"|"
	@echo "TARGETS   = |"$(TARGETS)"|"
	@echo "Makeloop  = |"$(Makeloop)"|"	
	@echo "MakePam   = |"$(MakePam)"|"	
	@echo "MakeDll   = |"$(MakeDll)"|"
	@echo "STAR_MAKE_HOME=|"$(STAR_MAKE_HOME)"|"
	@echo "DIRS=|"$(DIRS)"|"
	@echo "Makers    = |"$(Makers)"|"
	@echo "PAMS      = |"$(PAMS)"|"
#	@echo "NT        ="$(NT)"
	@echo "DIR_GEN   = |"$(DIR_GEN)"|"
	@echo "GEN_TMP   = |"$(GEN_TMP)"|"
	@echo "GEN_TAB   = |"$(GEN_TAB)"|"
	@echo "LIB_DIR   = |"$(LIB_DIR)"|"
	@echo "OBJ_DIR   = |"$(OBJ_DIR)"|"
	@echo "DEP_DIR   = |"$(DEP_DIR)"|"
	@echo "SYS_DIR   = |"$(SYS_DIR)"|"
	@echo "SRC_DIR   = |"$(SRC_DIR)"|"
	@echo "PKG       = |"$(PKG)"|"
	@echo "DOMAIN    = |"$(DOMAIN)"|"
	@echo "NAME      = |"$(NAME)"|"
	@echo "branch    = |"$(branch)"|"
	@echo "PAMS      = |"$(PAMS)"|"
