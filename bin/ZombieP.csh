#! /bin/tcsh -f
if ($#argv != 1) then
  set list = '.picoDst.root';
else 
  set list = $argv[1];
endif
if (! -r Recover.log) root.exe -q -b 'Recover.C("./*'${list}'")' | tee Recover.log
if (! -d Zombie) mkdir Zombie
foreach f (` grep Zombie Recover.log | awk '{print $3}'`)
  set b = `basename ${f} ${list}`; mv ${b}* Zombie/
end
