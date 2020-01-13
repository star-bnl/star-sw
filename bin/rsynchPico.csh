#! /usr/bin/tcsh -f
set dir = 11p5GeV.B
#set dir = 5p75GeV_fixedTarget.B
cd ~/reco/2020/TFG19m/RF/${dir}
echo "pwd = $PWD"
set log =  rsynchPico.`date +%m%d%y%H`.log;
touch ${log}
foreach d (`ls -1d ???`)
  cd ${d}
rsync -avrz -h                        \
    --include='*picoDst.root'                  \
    --exclude='*.*'  --exclude='Done' --exclude='.sl*'  \
    ./ rftpexp01.rhic.bnl.gov:/gpfs01/star/pwg_tasks/tfg02/2020/TFG19m/RF/${dir}/${d} >>& ../${log}
cd -
end
