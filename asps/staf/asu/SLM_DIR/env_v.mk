# env_v.mk - make variables for creating local environment
########################################################################
#
ifndef ENV_V_MK
export ENV_V_MK += DONE
#
export ENVDIRS := bin cdf doc idl inc lib solib src srcm wrk \
	aix hpux irix sun4 sun4os5 osf1 \
	bin/aix bin/hpux bin/irix bin/sun4 bin/sun4os5 bin/osf1 \
	lib/aix lib/hpux lib/irix lib/sun4 lib/sun4os5 lib/osf1 \
	solib/aix solib/hpux solib/irix solib/sun4 solib/sun4os5 \
			solib/osf1
export ENVFILES := Makefile.control Makefile.user
export ENVMAKES := Makefile \
	bin/Makefile cdf/Makefile doc/Makefile idl/Makefile \
	inc/Makefile lib/Makefile src/Makefile srcm/Makefile \
	wrk/Makefile
#
export MAKEFILEBASE := $(SLM_DIR)/Makefile
export MAKEFILESUBD := $(SLM_DIR)/Makefile.sub
#
endif #ENV_V_MK
#
