#! /bin/csh -f
#
####################################################

setenv GSTAR_UTIL_DIR $STAR/kumacs/sim/utilities

#--------------------------------------------------

# set up links to various utilities
source $GSTAR_UTIL_DIR/setup.utilities.csh

#--------------------------------------------------

\rm -f set_phys.kumac
ln -s $GSTAR_UTIL_DIR/set_phys.hadronic_on.kumac set_phys.kumac

\rm -f set_geom.kumac
ln -s $GSTAR_UTIL_DIR/set_geom.year2a.kumac set_geom.kumac
