#! /bin/csh -f 
foreach f (`ls -1d */*/*.root`)
    set d = `dirname ${f}`; set b = `basename $f`;
    cd ${d}; pwd;
    if (! -r Title.txt) then
      rm -rf .sl*
      ln -s ~/macros/.sl* .
      root.exe ${b} brtw.C+ ppbarM2.C+; 
      root.exe ${b} DrawSignalBg7.C
    endif
    cd -
 end
