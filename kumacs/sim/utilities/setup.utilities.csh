#! /bin/csh -f
#
# some general utilities needed by all setup scripts
# pmj 14/4/98
#

#--------------------------------------------------------

# alias gstar to execute setup.kumac upon startup
unalias gstar

if ( $OSTYPE == "unicos" ) then

# use "-P 0" to set NwPAW to zero

   alias gstar "~nevski/gstar/bin/gstar -l $GSTAR_UTIL_DIR/setup.kumac -P 0"

else

   alias gstar "`which staf` -l $GSTAR_UTIL_DIR/setup.kumac"

endif

echo `which gstar`

# get operating system, set kumac for os-specific setup
# (needed for T3E's)
# => could also execute some shell scripts here if needed
#--------------------------------------------------------

if ( $OSTYPE == "unicos" ) then

   \rm -f setup.os_specific.kumac
   ln -s $GSTAR_UTIL_DIR/setup_t3e.kumac setup.os_specific.kumac

else

   \rm -f setup.os_specific.kumac
   ln -s $GSTAR_UTIL_DIR/setup_not_t3e.kumac setup.os_specific.kumac

endif
#--------------------------------------------------------




