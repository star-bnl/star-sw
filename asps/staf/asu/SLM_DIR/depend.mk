# depend.mk - Generates dependencies for .cc & .c & .F files.
########################################################################
.PHONY: depend_all depend_clean depend_test depend_show
#
ifndef DEPEND_MK
export DEPEND_MK += DONE
#
export DFILES = $(OBJS:.o=.d)
export GFLAGS := -traditional -M -x c
#
endif #DEPEND_MK
#
ifndef DEPEND_MAK
DEPEND_MAK += DONE
#
depend_all:
	-cd $(STAR_ARCH); \
	make -f $(SLM_DIR)/depend.mk $(DFILES)
depend_clean:
depend_test:
	cd $(STAR_ARCH); \
	make depend_show
depend_show:
	@echo " ****************************** "
	@echo " ** depend.mk                ** "
	@echo " ****************************** "
	@echo "   DFILES = " $(DFILES)
	@echo "     OBJS = " $(OBJS)
#
%.d: %.cc
	gcc -MM $(CPPFLAGS) $< | sed -e 's/$*.o/& $@/g'\
	-e 's/[^ :\/]*\/\/*//g' > $@
#
%.d: %.c
	gcc -MM $(CPPFLAGS) $< | sed -e 's/$*.o/& $@/g'\
	-e 's/[^ :\/]*\/\/*//g' > $@
#
%.d: %.F
	gcc $(GFLAGS) $(CPPFLAGS) $< | sed -e 's/$*.F.o/$*.o $@/g'\
	-e 's/[^ :\/]*\/\/*//g' > $@
#
endif #DEPEND_MAK
#
ifeq ($(MAKE_DEPEND),$(TRUE))
ifneq ($(DFILES),$(EMPTY))
include $(DFILES)
endif
endif
#
