#! /usr/bin/tcsh -f
cd /hlt/cephfs/reco/2021/RF/TFG21c.B/7p7GeV_2021
echo "pwd = $PWD"
set dir = `basename ${PWD}`; #11p5GeV.B
set topdir = `dirname ${PWD}`;
set top = `basename  ${topdir}`
#set dir = 5p75GeV_fixedTarget.B
#cd ~/reco/2020/TFG19m/RF/${dir}
set log =  rsynchMuDst.`date +%m%d%y%H`.log;
touch ${log}
foreach d (`ls -1d 03?`)
  cd ${d}
rsync -avrz -h                        \
    --include='*MuDst.root'                  \
    --exclude='*.*'  --exclude='Sub*' --exclude='Done' --exclude='cpu*' --exclude='.sl*'  \
    ./ rftpexp01.rhic.bnl.gov:/gpfs01/star/pwg_tasks/TF_TrkEff/reco/2021/RF/TFG21c.B/7p7GeV_2021/${dir}/${d} >>& ../${log}
cd -
end
