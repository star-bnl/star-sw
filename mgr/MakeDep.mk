ifneq (,$(FILES_DCINT))
$(FILES_DCINT): $(DEP_DIR)/%Cint.d: %.h 
ifdef NT
	@echo $(FILES_DCINT) $<
endif #/* NT */
	$(RM) $(ALL_TAGS)
	$(MAKEDEPEND)  $(CPPFLAGS) $(INCLUDES) -x c $(1ST_DEPS) | sed -e \
's/$(notdir $(STEM))\.h\.$(O)/$(subst .,\.,$(subst /,\/,$(GEN_DIR)/$(STEM)Cint.cxx)) $(subst .,\.,$(subst /,\/,$(ALL_TAGS)))/g'\
        > $(ALL_TAGS)
endif
$(DEP_DIR)/%.d: %.c 
	$(RM) $(ALL_TAGS)
	$(MAKEDEPEND)  $(CPPFLAGS) $(INCLUDES) $(1ST_DEPS) | sed -e \
's/$(notdir $(STEM))\.$(O)/$(subst .,\.,$(subst /,\/,$(OBJ_DIR)/$(STEM).$(O))) $(subst .,\.,$(subst /,\/,$(ALL_TAGS)))/g'\
        > $(ALL_TAGS)

$(DEP_DIR)/%.d: %.cxx
	$(RM) $(ALL_TAGS)
	$(MAKEDEPEND) $(CPPFLAGS) $(INCLUDES) $(1ST_DEPS) | sed -e \
's/$(notdir $(STEM))\.o/$(subst .,\.,$(subst /,\/,$(OBJ_DIR)/$(STEM).o)) $(subst .,\.,$(subst /,\/,$(ALL_TAGS)))/g'\
        > $(ALL_TAGS)

$(DEP_DIR)/%.d: %.cc
	$(RM) $(ALL_TAGS)
	$(MAKEDEPEND) $(CPPFLAGS) $(INCLUDES) $(1ST_DEPS) | sed -e \
's/$(notdir $(STEM))\.o/$(subst .,\.,$(subst /,\/,$(OBJ_DIR)/$(STEM).o)) $(subst .,\.,$(subst /,\/,$(ALL_TAGS)))/g'\
        > $(ALL_TAGS)
$(DEP_DIR)/%.d:%.g
	$(RM) $(ALL_TAGS)
	$(MAKEDEPEND) -traditional -x c $(CPPFLAGS) $(INCLUDES) $(1ST_DEPS) | sed -e \
's/$(notdir $(STEM))\.g\.$(O)/$(subst .,\.,$(subst /,\/,$(LIB_PKG)($(STEM).$(O)))) $(subst .,\.,$(subst /,\/,$(ALL_TAGS)))/g'\
        > $(ALL_TAGS)
$(DEP_DIR)/%.d:%.cdf
	$(RM) $(ALL_TAGS)
	cd $(SRC_DIR); \
        echo "$(notdir $(STEM)).c $(ALL_TAGS): $(ALL_DEPS)" > $(ALL_TAGS) ;
        echo "$(STEM).$(O): $(STEM).c" >> $(ALL_TAGS)
$(DEP_DIR)/%.d:%.F
	$(RM) $(ALL_TAGS)
	$(MAKEDEPEND) -traditional -x c $(CPPFLAGS) $(INCLUDES) $(1ST_DEPS) | sed -e \
's/$(notdir $(STEM))\.F\.$(O)/$(subst .,\.,$(subst /,\/,$(LIB_PKG)($(STEM).$(O)))) $(subst .,\.,$(subst /,\/,$(ALL_TAGS)))/g'\
        > $(ALL_TAGS)
$(DEP_DIR)/%.d:%.f
	$(RM) $(ALL_TAGS)
	$(MAKEDEPEND) -traditional -x c $(CPPFLAGS) $(INCLUDES) $(1ST_DEPS) | sed -e \
's/$(notdir $(STEM))\.F\.$(O)/$(subst .,\.,$(subst /,\/,$(LIB_PKG)($(STEM).$(O)))) $(subst .,\.,$(subst /,\/,$(ALL_TAGS)))/g'\
        > $(ALL_TAGS)
