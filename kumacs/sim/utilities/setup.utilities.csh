#! /bin/csh -f
#
# some general utilities needed by all setup scripts
# pmj 14/4/98
#

#--------------------------------------------------------

# alias gstar to execute setup.kumac upon startup

unalias gstar

if ( $?GSTAR_EXECUTABLE ) then

   if ( -x $GSTAR_EXECUTABLE ) then

      alias gstar "$GSTAR_EXECUTABLE -l $GSTAR_UTIL_DIR/setup.kumac"

   else

      echo "File $GSTAR_EXECUTABLE is not an executable..."

   endif

else if ( $HOST == "mcurie" ) then

   alias gstar "~nevski/gstar/bin/gstar -l $GSTAR_UTIL_DIR/setup.kumac"

else if ( $HOST == "pierre" ) then

   alias gstar "~nevski/gstar/bin/gstar -l $GSTAR_UTIL_DIR/setup.kumac"

else if ( $HOST == "jaromir" ) then

   alias gstar "/chkpnt/star/pjacobs/bin -l $GSTAR_UTIL_DIR/setup.kumac"

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




