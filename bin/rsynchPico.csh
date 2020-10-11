#! /usr/bin/tcsh -f
echo "pwd = $PWD"
set dir = `basename ${PWD}`; #11p5GeV.B
set topdir = `dirname ${PWD}`;
set top = `basename  ${topdir}`
#set dir = 5p75GeV_fixedTarget.B
#cd ~/reco/2020/TFG19m/RF/${dir}
set log =  rsynchPico.`date +%m%d%y%H`.log;
touch ${log}
foreach d (`ls -1d 2??`)
  cd ${d}
rsync -avrz -h                        \
    --include='*picoDst.root'                  \
    --exclude='*.*'  --exclude='Sub*' --exclude='Done' --exclude='cpu*' --exclude='.sl*'  \
    ./ rftpexp01.rhic.bnl.gov:/gpfs01/star/pwg_tasks/tfg02/2020/TFG20a/RF/${dir}/${d} >>& ../${log}
cd -
end
