#! /bin/tcsh -f
#set TRIG = AuAu11_production
#set PicoDIR  = /gpfs02/eic/ayk/STAR/reco/Pico/BES-I/${TRIG}/2010
#set TRIG = AuAu19_production
set TRIG = AuAu39_productionP10ik
set PicoDIR  = /gpfs02/eic/ayk/STAR/reco/Pico/BES-I/${TRIG}/2010
setenv NODEBUG yes
setup 64b
#xtitl
cd ${PicoDIR}
foreach f (`ls -1d */*/MuDst.list`)
  set dir = `dirname ${f}`; cd ${dir}; pwd;
    PicoBES1.pl
    if ( ! $? ) then
      lsf ~/xml/PicoBES1.xml;
    else
      mv MuDst.list MuDst.list.Done
    endif
  cd -;
end
