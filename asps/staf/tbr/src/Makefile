headers := $(wildcard *.h)
uname   := $(shell uname)
sys     := $(shell sys)
objs    := $(patsubst %.c,%.o,$(wildcard *.c))
objs    := $(addprefix $(sys)/,$(objs))
tbr     := $(sys)/tbr
syslib  := /afs/rhic/star/packages/RL98c/.`sys`/lib
o       := ooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo

# Flags are the same for both Solaris Pentium and Solaris Sparc.
ifeq ($(uname),SunOS)
cflags   := -I/usr/openwin/include -I/usr/dt/include
lflags1  := -L/usr/openwin/lib -L/usr/dt/lib -lnsl
lflags2  := -R/usr/openwin/lib -R/usr/dt/lib
endif

lflags3   := -lXm -lXt -lX11 -lm

####################################################################
.PHONY: test show clean
####################################################################
$(tbr):	$(objs) $(sys)
	@echo $(o)
	cc -o $(tbr) \
	$(objs) \
	$(lflags1) \
	$(lflags2) \
	$(lflags3) \
	$(syslib)/libdsl.a $(syslib)/libdsu.a
####################################################################
$(sys):
	mkdir -p $(sys)
####################################################################
$(sys)/%.o:	%.c $(headers) $(sys)
	@echo $(o)
	cc -DSTANDALONE -c \
	-I../inc -I/afs/rhic/star/packages/RL98c/asps/staf/inc \
	-o $@ $< \
	$(cflags)
####################################################################
show: test
test:
	@echo $(o)
	@echo "cflags =" $(cflags)
	@echo "lflags1 =" $(lflags1)
	@echo "lflags2 =" $(lflags2)
	@echo "lflags3 =" $(lflags3)
	@echo "  objs =" $(objs)
	@echo "   tbr =" $(tbr)
	@echo "   sys =" $(sys)
####################################################################
clean:
	@echo $(o)
	rm -f $(sys)/*
####################################################################
