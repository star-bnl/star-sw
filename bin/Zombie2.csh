#! /bin/tcsh -f
if ($#argv != 1) then
  set list = '.event.root';
else 
  set list = $argv[1];
endif
if (! -r Recover.log) root.exe -q -b 'Recover.C("*/*'${list}'")' | tee Recover.log
if (! -d Zombie) mkdir Zombie
foreach f (` grep Zombie Recover.log | awk '{print $3}'`)
  set b = `basename ${f} ${list}`; set d = `dirname ${f}`; mv ${d}/${b}* Zombie/
end
