#	Determine Make variables
ALL_TAGS = $@
TARGET   = $@
ALL_DEPS = $^
1ST_DEPS = $<
NEW_DEPS = $?
FUL_DEPS = $+
STEM     = $*
EMPTY      :=
MakeFile   :=$(STAR)/mgr/asis.mk
ifndef DIR
#DIR :=/afs/cern.ch/asis/hp_ux102/usr.local/lib/hepix
DIR :=/afs/cern.ch/asis
endif 
CERN_ASIS := /afs/cern.ch/asis
CERN_DIR  := $(DIR)
cern_dir  := $(subst $(CERN_ASIS)/,,$(CERN_DIR))
RHIC_ASIS := /afs/rhic/.asis
RHIC_DIR  := $(subst $(CERN_ASIS),$(RHIC_ASIS),$(DIR))
rhic_dir  := $(subst $(RHIC_ASIS)/,,$(RHIC_DIR))
SOURCES   := $(subst $(CERN_DIR)/,,$(wildcard $(CERN_DIR)/* $(CERN_DIR)/.*) )
SOURCES   := $(subst  .. , ,$(SOURCES))
SOURCES   := $(strip $(subst  . , ,$(SOURCES)))
SRC       := $(strip $(foreach dir, $(SOURCES), $(shell test -f $(CERN_DIR)/$(dir) && echo $(dir))))
DIRS      := $(sort $(strip $(foreach dir, $(SOURCES), $(shell test -d $(CERN_DIR)/$(dir) && echo $(dir)))))
src       := $(strip $(filter-out $(DIRS),$(SRC)))
src       := $(filter-out %.gz %.tar %.Z,$(src))
dirs      := $(filter-out alpha_% tar hp700_ux90 rs_aix% sgi_% sun4c_%,$(DIRS))
ifneq (,$(strip $(dirs)))
dirs      :=$(filter-out alpha_% rs_aix% sgi% 94a 95a 95b 96a 97a metahtml%,$(dirs))
ifeq ($(RHIC_DIR),$(RHIC_ASIS))
skip_list :=adm packages
skip_list += a10_sr104 a68_sr104 alpha_% amiga_aux20 bin cern convex% cray% etc hp700_ux101 hp700_ux807 hp700_ux90 
skip_list += i386_linux1 incoming mac% next_mach30 pc_% pmax_ul43 pub rs_aix% scratch sgi% specific 
skip_list += src sun4c% sun4m% unicos61 usr var vax% vm_cms ymp%
dirs      :=$(filter-out $(skip_list),$(dirs))
endif
dir_tags  := $(addprefix tags_,$(dirs))
endif
ifneq (,$(strip $(src))) 
RHIC_SOURCES :=$(addprefix $(RHIC_DIR)/,$(src))
RHIC_SOURCES :=$(filter-out %/usr.local/lib/hepix/tools/make.pl,$(RHIC_SOURCES))
endif
all: $(RHIC_SOURCES) $(dir_tags)
$(RHIC_DIR)/%:$(CERN_DIR)/%
	@echo "Update file $(RHIC_ASIS)/$(rhic_dir)/$(STEM) from $(CERN_ASIS)/$(cern_dir)/$(STEM)"
	cd $(CERN_ASIS); tar cvf - $(cern_dir)/$(STEM) | (cd $(RHIC_ASIS); tar xvf -);
tags_%:
	@echo "Update directory $(STEM) from $(CERN_DIR)"
	gmake -r -f $(MakeFile) DIR=$(CERN_DIR)/$(STEM)
test:
	@echo "CERN_DIR    = |"$(CERN_DIR)"|"
	@echo "RHIC_DIR    = |"$(RHIC_DIR)"|"
	@echo "SOURCES     = |"$(SOURCES)"|"
	@echo "src         = |"$(src)"|"
	@echo "RHIC_SOURCES= |"$(RHIC_SOURCES)"|"
	@echo "skip_list   = |"$(skip_list)"|"
	@echo "SKIP_LIST   = |"$(SKIP_LIST)"|"
	@echo "dirs        = |"$(dirs)"|"
	@echo "DIRS        = |"$(DIRS)"|"
