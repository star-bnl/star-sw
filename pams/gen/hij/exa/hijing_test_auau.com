#!/bin/csh
# 
#  HIJING_TEST.COM
# 
setenv Z_TAPE "`pwd`/test_auau.hij"
#
# generate the egzpar.par file needed for the ZEBRA I/O inteface
# for the event generators...
#
echo " "\$"ZPAR" >egzpar.par
echo " Z_DOUBLE_OUT    = F,">>egzpar.par
echo " Z_BLOCKSIZE     =        3600,">>egzpar.par
echo " Z_FZ_OPTION_OUT = 'AO',">>egzpar.par
echo " Z_FZ_OPTION_IN  = 'AI',">>egzpar.par
echo " Z_FZLOGL        =          -2">>egzpar.par
echo " "\$"END">>egzpar.par
#
# make input parameter file...
#
echo "Sample file with parameters to run HIJING">hij_params_auau.dat
echo "Generate 1 au+au event at sqrt(s(NN))=200 GeV">>hij_params_auau.dat
echo "One sample jet of pt=30 GeV per event">>hij_params_auau.dat
echo "This text header can have up to 100 lines.">>hij_params_auau.dat
echo "But the last line should always be at least four dollar signs in a row.">>hij_params_auau.dat
echo \$\$\$\$\$\$\$\$\$\$>>hij_params_auau.dat
echo "200.0              ! nucleon-nucleon energy">>hij_params_auau.dat
echo "'CMS'              ! frame">>hij_params_auau.dat
echo "'A','A'            ! proj, targ  (the quotation marks are important)">>hij_params_auau.dat
echo "197, 79, 197, 79   ! A,Z projectile, A, Z target">>hij_params_auau.dat
echo "1                  ! number of events">>hij_params_auau.dat
echo "0.  .75            ! minimum, maximum impact parameter">>hij_params_auau.dat
echo "-7.  7.            ! minimum, maximum eta">>hij_params_auau.dat
echo "3912785            ! seed">>hij_params_auau.dat
echo "'HIPR1' 10  30.    ! sample jet pt">>hij_params_auau.dat
echo "'IHPR2' 3   1      ! switch on sample jets">>hij_params_auau.dat
echo "'IHPR2' 12  0      ! permit resonance decays...">>hij_params_auau.dat
echo "'IHPR2' 18  1      ! keep all parents...">>hij_params_auau.dat
# 
setenv Z_TAPE "`pwd`/test_auau.hij"
#
time $STAR_LIB/star/$STAR_LEVEL/sim/bin/$STAR_ARCH/hijevt <<eof
hij_params_auau.dat
1
eof
#
unsetenv Z_TAPE
if ("`alias rm `" == "rm -i") then
   unalias rm
   rm hij_params_auau.dat egzpar.par
   alias rm 'rm -i'
else
   rm hij_params_auau.dat egzpar.par
endif
