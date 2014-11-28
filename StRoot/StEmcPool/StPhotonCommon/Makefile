
-include $(ROOTSYS)/test/Makefile.arch
HSuf = h

MODULE_NAME = StPhotonCommon

ifeq (1$(OUTPUT_DIR),1)
OUTPUT_DIR = "$(shell pwd)"
endif
ifeq (1$(OUTPUT_NAME),1)
OUTPUT_NAME = $(MODULE_NAME)
endif
ifeq (1$(INCDIR),1)
INCDIR = ../..
endif
ifeq (1$(MYCXXFLAGS),1)
MYCXXFLAGS = "-Wall -fPIC -g"
endif

MYS      = \
           MyEvent.$(SrcSuf) \
           MyMcTrack.$(SrcSuf) \
           MyPoint.$(SrcSuf) \
           PhotonAnalysisSettings.$(SrcSuf)
MYDict   = $(OUTPUT_NAME)_Dict.$(SrcSuf)
MYO      = $(MYS:.$(SrcSuf)=.$(ObjSuf)) $(MYDict:.$(SrcSuf)=.$(ObjSuf))
MY       = $(OUTPUT_NAME).$(DllSuf)
MYINST = "$(OUTPUT_DIR)/$(MY)"

DICTIONARIES  = $(MYDict) *_Dict.$(SrcSuf)

MYINCLUDES = -I"$(INCDIR)"
CXXFLAGS = $(shell root-config --cflags) $(MYCXXFLAGS) $(MYINCLUDES)
LDFLAGS  = $(shell root-config --ldflags) -shared
LIBS     = $(shell root-config --libs)

#------------------------------------------------------------------------------

.SUFFIXES: .$(SrcSuf) .$(ObjSuf) .$(DllSuf)
.PHONY : clean distclean install

all: $(MY)

$(MY): $(MYO)
	$(LD) $(LDFLAGS) $^ $(LIBS) $(OutPutOpt)$@
	@echo "$@ done"

install:
	@if ! test -d "$(OUTPUT_DIR)"; then mkdir -p "$(OUTPUT_DIR)"; fi
	@if test $(MY) -nt $(MYINST); then cp -f $(MY) $(MYINST); echo "Installed $(MY) as $(MYINST)"; fi

clean:
	-rm -f $(MYO) *.d $(DICTIONARIES) $(DICTIONARIES:.$(SrcSuf)=.$(HSuf)) $(DICTIONARIES:.$(SrcSuf)=.$(ObjSuf)) *.stackdump

distclean: clean
	-rm -f $(MY)
	-rm -f "$(OUTPUT_DIR)"/$(MY)

.SUFFIXES: .$(SrcSuf)

MyEvent.$(SrcSuf): MyEvent.$(HSuf)

MyEvent.$(HSuf): MyMcTrack.$(HSuf)
MyEvent.$(HSuf): MyPoint.$(HSuf)

MyMcTrack.$(SrcSuf): MyMcTrack.$(HSuf)

MyPoint.$(SrcSuf): MyPoint.$(HSuf)

PhotonAnalysisSettings.$(SrcSuf): PhotonAnalysisSettings.$(HSuf)

$(MYDict): \
           MyEvent.$(HSuf) \
           MyMcTrack.$(HSuf) \
           MyPoint.$(HSuf) \
           PhotonAnalysisSettings.$(HSuf) \
           MyEventLinkDef.$(HSuf)
	rootcint -f $@ -c -p $(MYINCLUDES) $^

.$(SrcSuf).$(ObjSuf):
	$(CXX) $(CXXFLAGS) -c $< -o $@
