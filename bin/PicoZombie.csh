#! /bin/tcsh -f
if ($#argv != 1) then
  set list = '*/*/*.picoDst.root';
else 
  set list = $argv[1];
endif
#if (! -r Chain.log) root.exe -q -b 'Chain.C+("'${list}'","PicoDst")' | tee Chain.log
#if (! -d Zombie) mkdir Zombie
foreach f (` grep missing Chain.log | awk '{print $3}'`)
  set b = `basename ${f} .picoDst.root`; set d = `dirname ${f}`; 
  echo ${d}/${b}* 
  dir  ${d}/${b}* 19GeV_2019/log/19GeV_2019-${b}*
  mv  ${d}/${b}* 19GeV_2019/log/19GeV_2019-${b}* Zombie
end
#  mv ${b}* Zombie/
