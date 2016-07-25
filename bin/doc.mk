ALL_DEPS    = $^
FIRST_DEP   = $<
FIRSTF      = $(<D)/$(<F)
ALL_TAGS    = $@
STEM        = $*
STEMF       = $(*D)/$(*F)

ALL_IDL  := $(shell find $(STAR)/pams -name "*.idl")
ALL_IDM  := $(shell egrep -l 'interface.*:.*amiModule' $(ALL_IDL))
ALL_IDT  := $(shell egrep -l 'struct '                 $(ALL_IDL))
ALL_MOD  := $(notdir $(ALL_IDM))
ALL_TAB  := $(notdir $(ALL_IDL))
DOMS   := $(filter-out CVS, $(shell cd $(STAR)/pams; ls))
PKGS  := $(subst /,=,$(subst /TRAIL,,$(addsuffix TRAIL,$(sort  $(dir $(subst $(STAR)/pams/,,$(ALL_IDM)))))))
MODS   := $(subst /,=,                                  $(sort  $(basename $(subst $(STAR)/pams/,,$(ALL_IDM)))))
TABS    := $(subst /,=,                                  $(sort  $(basename $(subst $(STAR)/pams/,,$(ALL_IDT)))))
pkg       := ctf
dom_dep   := dom_head_$(pkg) $(addprefix tab_dom_, $(filter $(pkg)%, $(TABS)))  $(addprefix pkg_, $(filter $(pkg)%, $(PKGS)))
.PHONY: default test
default: all
all: domain 
domain: index $(addprefix dom_, $(DOMS))  
	@echo "============= domain ==================="
	@echo  "Overall" $(ALL_TAGS) "<-" $(ALL_DEPS)
index: 
#	$(RM) index.html; touch index.html
	@echo "<HTML>"                                      # >> index.html
	@echo "<HEAD>"                                      # >> index.html
	@echo "<TITLE>List of STAR pams and tables</TITLE>" # >> index.html
	@echo "</HEAD>"                                     # >> index.html
	@echo "<BODY>"                                      # >> index.html
	@echo "<H2>List of STAR pams</H2>"                  # >> index.html
	@echo "<hr>"                                        # >> index.html
	@echo "<hr>"                                        # >> index.html
dom_ctf:     dhead_ctf $(addprefix dtab_, $(filter ctf%, $(TABS)))  $(addprefix pkg_, $(filter ctf%, $(PKGS)))
dom_emc:     dhead_emc $(addprefix dtab_, $(filter emc%, $(TABS)))  $(addprefix pkg_, $(filter emc%, $(PKGS)))
dom_ftpc:    dhead_ftpc $(addprefix dtab_, $(filter ftpc%, $(TABS)))  $(addprefix pkg_, $(filter ftpc%, $(PKGS)))
dom_gen:     dhead_gen $(addprefix dtab_, $(filter gen%, $(TABS)))  $(addprefix pkg_, $(filter gen%, $(PKGS)))
dom_geometry:dhead_geometry $(addprefix dtab_, $(filter geometry%, $(TABS)))  $(addprefix pkg_, $(filter geometry%, $(PKGS)))
dom_magnet:  dhead_magnet $(addprefix dtab_, $(filter magnet%, $(TABS)))  $(addprefix pkg_, $(filter magnet%, $(PKGS)))
dom_mwc:     dhead_mwc $(addprefix dtab_, $(filter mwc%, $(TABS)))  $(addprefix pkg_, $(filter mwc%, $(PKGS)))
dom_sim:     dhead_sim $(addprefix dtab_, $(filter sim%, $(TABS)))  $(addprefix pkg_, $(filter sim%, $(PKGS)))
dom_svt:     dhead_svt $(addprefix dtab_, $(filter svt%, $(TABS)))  $(addprefix pkg_, $(filter svt%, $(PKGS)))
dom_tpc:     dhead_tpc $(addprefix dtab_, $(filter tpc%, $(TABS)))  $(addprefix pkg_, $(filter tpc%, $(PKGS)))
dom_trg:     dhead_trg $(addprefix dtab_, $(filter trg%, $(TABS)))  $(addprefix pkg_, $(filter trg%, $(PKGS)))
dom_vpd:     dhead_vpd $(addprefix dtab_, $(filter vpd%, $(TABS)))  $(addprefix pkg_, $(filter vpd%, $(PKGS)))
dom_global:  dhead_global $(addprefix dtab_, $(filter global%, $(TABS)))  $(addprefix pkg_, $(filter global%, $(PKGS)))
dom_doc:  
dom_params:  
dhead_%:
#	@echo "============= dhead_"$(STEM)"==================="
#	@echo  "domain head" $(ALL_TAGS) "<-" $(ALL_DEPS)
	@echo  "<H3>Domain "$(STEM)"</h3>"                    # >> index.html  
	@echo  "<h4>List of tables create in "$(STEM)"</h4>" # >> index.html  
pkg_%:  #$(addprefix mod_, $(filter $(ALL_TAGS)%, $(MODS)))  
	@echo "============= pkg_"$(STEM)"==================="
	@echo  "package" $(ALL_TAGS) "<-" $(ALL_DEPS)
tab_%:  #$(addprefix dtab_, $(filter $(ALL_TAGS)%, $(TABS)))
	@echo "============= tab_"$(STEM)"==================="
	@echo  "tabs" $(ALL_TAGS) "<-" $(ALL_DEPS)
dtab_%:  
#	@echo "============= dtab_"$(STEM)"==================="
#	@echo  "dtab" $(ALL_TAGS) "<-" $(ALL_DEPS)
	@echo  '<a name="'`echo $(STEM) | sed -e 's/=/\//g' -e 's/.idl//g'`'"</a>' # >> index.html 
	@echo  '<a href="http://www.rhic.bnl.gov/star/doc/www/pakages_l/dev/pams/'`echo $(STEM) | sed -e 's/=/\//g' -e 's/.idl//g'`'.idl">'`echo $(STEM) | sed -e 's/=/\//g' -e 's/.idl//g'`'</a>'                 # >> index.html 
mod_%:
	@echo "============= mod_"$(STEM)"==================="
	@echo  "module:" $(ALL_TAGS) "<-" $(ALL_DEPS)
test:
	@echo ALL_IDL   := $(ALL_IDL)
	@echo ALL_IDM   := $(ALL_IDM)	
	@echo ALL_IDT   := $(ALL_IDT) 
	@echo ALL_MOD   := $(ALL_MOD)	
	@echo ALL_TAB   := $(ALL_TAB)
	@echo DOMS   := $(DOMS)
	@echo PKGS  := $(PKGS)
	@echo MODS   := $(MODS)
	@echo TABS    := $(TABS)
	@echo dom_dep   := $(dom_dep)