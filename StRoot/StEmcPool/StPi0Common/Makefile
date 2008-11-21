
-include $(ROOTSYS)/test/Makefile.arch
HSuf = h

MODULE_NAME = StPi0Common

ifeq (1$(OUTPUT_DIR),1)
OUTPUT_DIR = $(shell pwd)
endif
ifeq (1$(OUTPUT_NAME),1)
OUTPUT_NAME = $(MODULE_NAME)
endif
ifeq (1$(INCDIR),1)
INCDIR = ../..
endif

MYS      = \
           StPi0CommonUtil.$(SrcSuf) \
           StPi0DataStructures.$(SrcSuf)
MYDict   = $(OUTPUT_NAME)_Dict.$(SrcSuf)
MYO      = $(MYS:.$(SrcSuf)=.$(ObjSuf)) $(MYDict:.$(SrcSuf)=.$(ObjSuf))
MY       = $(OUTPUT_DIR)/$(OUTPUT_NAME).$(DllSuf)

DICTIONARIES  = $(MYDict) *_Dict.$(SrcSuf)

MYINCLUDES = -I$(INCDIR)
CXXFLAGS = $(shell root-config --cflags) -Wall -fPIC -g $(MYINCLUDES)
LDFLAGS  = $(shell root-config --ldflags) -shared
LIBS     = $(shell root-config --libs)

#------------------------------------------------------------------------------

.SUFFIXES: .$(SrcSuf) .$(ObjSuf) .$(DllSuf)
.PHONY : clean distclean 

all: $(MY)

$(MY): $(MYO)
	@echo $(LD) $(LDFLAGS) $^ $(LIBS) $(OutPutOpt)$@
	@$(LD) $(LDFLAGS) $^ $(LIBS) $(OutPutOpt)$@
	@echo "$@ done"

clean:
	@-rm -f $(MYO) *.d $(DICTIONARIES) $(DICTIONARIES:.$(SrcSuf)=.$(HSuf)) $(DICTIONARIES:.$(SrcSuf)=.$(ObjSuf)) *.stackdump

distclean: clean
	@-rm -f $(MY)

.SUFFIXES: .$(SrcSuf)

StPi0CommonUtil.$(SrcSuf): StPi0CommonUtil.$(HSuf)

StPi0DataStructures.$(SrcSuf): StPi0DataStructures.$(HSuf)

$(MYDict): \
           StPi0CommonUtil.$(HSuf) \
           StPi0DataStructures.$(HSuf) \
           StPi0CommonLinkDef.$(HSuf)
	@echo rootcint -f $@ -c -p $(MYINCLUDES) $^
	@rootcint -f $@ -c -p $(MYINCLUDES) $^

.$(SrcSuf).$(ObjSuf):
	@echo $(CXX) $(CXXFLAGS) -c $< -o $@
	@$(CXX) $(CXXFLAGS) -c $< -o $@
