# Module.mk for qtgl module
# Copyright (c) 2000 [BNL] Valeri Fine
#
# Author: Valeri Fine, 11/11/2002

##### libRQTGL #####

ifeq ($(ARCH),win32old)
include qtgl/Module.mk.win32
else
include qtgl/Module.mk.unix
endif

