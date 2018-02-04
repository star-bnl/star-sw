#! /bin/tcsh -f
if (! -r Recover.log) root.exe -q -b Recover.C | tee Recover.log
if (! -d Zombie) mkdir Zombie
foreach f (` grep Zombie Recover.log | awk '{print $3}'`)
  set b = `basename ${f} .MuDst.root`; mv ${b}* Zombie/
end
