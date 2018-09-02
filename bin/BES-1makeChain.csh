#! /bin/tcsh -f
set TRIG = AuAu7_production
set PicoDIR  = /gpfs02/eic/ayk/STAR/reco/Pico/BES-I/${TRIG}/2010
#setenv NODEBUG yes
#setup 64b
#xtitl
cd ${PicoDIR}
@ count = 0
foreach f (`ls -1d ???/*/MuDst.list.Done`)
  set d = `dirname ${f}`;
  cd ${d}; pwd;
  if (! -r Chain.log) then	
    rm -rf .sl*
    ln -s ../../.sl73_* .	
    root.exe -q -b 'Chain.C+("*.root","PicoDst")' >&  Chain.log  &
    @ count++;  echo "count $count";
    if ($count > 40) break;
  endif
  cd -;
end
