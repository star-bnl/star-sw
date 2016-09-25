#!/bin/sh

if test ! -d ../src; then
  echo "WARNING! Script must be running from TAUOLA/platform directory!"
  exit
fi

if test ! -d ../tauola-fortran/tauola; then
  echo "WARNING! Script 'tauola-fortran/onlyLHC.sh' must be run first!"
  exit
fi

echo "This option will overwrite all configuration scripts/makefiles"
echo "and modify the configuration procedure to match LCG setup."
echo ""
echo "You will need autotools version 2.59 or higher."
echo ""
echo "Proceed? (Yes/No)"
read ANSWER

ANSWER=`echo $ANSWER | tr "[:upper:]" "[:lower:]"`

if test "$ANSWER" = "yes" || test "$ANSWER" = "y"; then
  echo "Renaming .c files"
  mv ../src/tauolaFortranInterfaces/f_Init.c ../src/tauolaFortranInterfaces/f_Init.cxx
  mv ../src/tauolaFortranInterfaces/f_FilHep.c ../src/tauolaFortranInterfaces/f_FilHep.cxx
  mv ../src/tauolaFortranInterfaces/f_Decay.c ../src/tauolaFortranInterfaces/f_Decay.cxx

  mv ../examples/taumain_hepevt_example.c ../examples/taumain_hepevt_example.cxx
  mv ../examples/taumain_stand_alone_example.c ../examples/taumain_stand_alone_example.cxx
  mv ../examples/taummk_pythia_example.c ../examples/taummk_pythia_example.cxx
  mv ../examples/taumain_pythia_example.c ../examples/taumain_pythia_example.cxx
  mv ../examples/single_tau_gun_example.c ../examples/single_tau_gun_example.cxx

  echo "Removing previous installation scripts"
  rm -rf ../config* ../make* ../Make*
  rm -rf ../src/make.inc ../src/*/Makefile ../tauola-fortran/make*
  rm -rf ../TauSpinner/Makefile
  rm -rf ../TauSpinner/examples/config* ../TauSpinner/examples/Makefile
  rm -f  ../TauSpinner/examples/tauspinner-validation/Makefile
  rm -f  ../TauSpinner/examples/tauspinner-validation/test-*/Makefile
  rm -f  ../TauSpinner/examples/CP-tests/Makefile
  rm -rf ../examples/config* ../examples/make* ../examples/Make*

  echo "Copying and configuring new scripts"
  cp -rf LCGCONFIG/* ../.
  cd ..
  autoreconf --install --force

  echo "Copying header files"
  mkdir -p include/Tauola
  mkdir -p include/TauSpinner
  cp src/*/*.h include/Tauola/.
  cp TauSpinner/include/TauSpinner/*.h include/TauSpinner/.

  echo "Done."
else
  echo "Aborted."
fi
