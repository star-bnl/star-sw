#! /usr/bin/tcsh -f
#echo "pwd = $PWD"
#set dir = `basename ${PWD}`; #11p5GeV.B
#set topdir = `dirname ${PWD}`;
#set top = `basename  ${topdir}`
##set dir = 5p75GeV_fixedTarget.B
##cd ~/reco/2020/TFG19m/RF/${dir}
#set log =  rsynchPico.`date +%m%d%y%H`.log;
#touch ${log}
#foreach d (`ls -1d 0??`)
#  cd ${d}
#  rsync -avrz -h                        \
#    --include='*picoDst.root'                  \
#    --exclude='*.*'  --exclude='Sub*' --exclude='Done' --exclude='cpu*' --exclude='.sl*'  \
#    ./ /direct/gpfs01/star/pwg/fisyak/Pico/2021/RF/TFG21c.B/${dir}/${d} >>& ../${log}
#  cd -
#end
#
##    ./ sftp.sdcc.bnl.gov:/gpfs01/star/pwg/fisyak/Pico/2021/RF/TFG21c.B/{dir}/${d} >>& ../${log}
# 
##rsync -avrz -h                        \
##    --include='*picoDst.root'                  \
#    --exclude='*.*'  --exclude='Sub*' --exclude='Done' --exclude='cpu*' --exclude='.sl*'  \
#    ./ rftpexp01.rhic.bnl.gov:/gpfs01/star/pwg_tasks/tfg02/2020/TFG20a/RF/${dir}/${d} >>& ../${log}
#
#  rsync -avrz -h --include='*picoDst.root' --exclude='*.*' --exclude='Sub*' --exclude='*T' --exclude='Done' --exclude='cpu*' --exclude='.sl*'  ./??? /direct/gpfs01/star/subsysg/TFG/Pico/2021/RF/TFG21c.B/7p7GeV_2021 
#rsync -avrz -h --include='*Dst.root' --exclude='*.*' --exclude='Sub*' --exclude='*T' --exclude='Done' --exclude='cpu*' --exclude='.sl*'  ./??? /direct/gpfs01/star/subsysg/TFG/Pico/2021/RF/TFG21e/3p85GeV_fixedTarget_2021
#rsync -avrz -h --include='*picoDst.root' --exclude='*.*' --exclude='Sub*' --exclude='*T' --exclude='Done' --exclude='cpu*' --exclude='.sl*'  ./??? /direct/gpfs01/star/subsysg/TFG/Pico/2021/RF/TFG21e/3p85GeV_fixedTarget_2021
#rsync -avrz -h --include='*picoDst.root' --exclude='*.*' --exclude='Sub*' --exclude='*T' --exclude='Done' --exclude='cpu*' --exclude='.sl*'  ./??? /direct/gpfs01/star/subsysg/TFG/Pico/2021/RF/TFG21e/ps_OO_200GeV_2021
#rsync -avrz -h --include='*picoDst.root' --exclude='*.*' --exclude='Sub*' --exclude='*T' --exclude='Done' --exclude='cpu*' --exclude='.sl*'  ./??? /direct/gpfs01/star/subsysg/TFG/Pico/2021/RF/TFG21f/3p85GeV_fixedTarget_2021
#rsync -avrz -h --include='*picoDst.root' --exclude='*.*' --exclude='Sub*' --exclude='*T' --exclude='Done' --exclude='cpu*' --exclude='.sl*'  ./??? /direct/gpfs01/star/subsysg/TFG/Pico/2021/RF/TFG21e/ps_OO_200GeV_2021
#rsync -avrz -h --include='*picoDst.root' --exclude='*.*' --exclude='Sub*' --exclude='*T' --exclude='Done' --exclude='cpu*' --exclude='.sl*'  ./??? /direct/gpfs01/star/subsysg/TFG/Pico/2021/RF/TFG21g/3p85GeV_fixedTarget_2021
#rsync -avrz -h --include='*picoDst.root' --exclude='*.*' --exclude='Sub*' --exclude='*T' --exclude='Done' --exclude='cpu*' --exclude='.sl*'  ./??? /direct/gpfs01/star/subsysg/TFG/Pico/2021/RF/TFG21g.B/3p85GeV_fixedTarget_2021
#rsync -avrz -h --include='*picoDst.root' --exclude='*.*' --exclude='Sub*' --exclude='*T' --exclude='Done' --exclude='cpu*' --exclude='.sl*'  ./??? /direct/gpfs01/star/pwg/fisyak/Pico/2021/RF/TFG21e/17p3GeV_2021 >& rsynch.log &
# rsync -avrz -h --include='*picoDst.root' --exclude='*.*' --exclude='Sub*' --exclude='*T' --exclude='Done' --exclude='cpu*' --exclude='.sl*'  ./??? /direct/gpfs01/star/subsysg/TFG/Pico/2021/RF/TFG21g.B/3p85GeV_fixedTarget_2021 >& rsynch.log &
# rsync -avrz -h --include='*picoDst.root' --exclude='*.*' --exclude='Sub*' --exclude='*T' --exclude='Done' --exclude='cpu*' --exclude='.sl*'  ./??? /direct/gpfs01/star/pwg/fisyak/Pico/2021/RF/TFG21e/100GeV_fixedTarget_2021 >& rsynch.log &
# rsync -avrz -h --include='*picoDst.root' --exclude='*.*' --exclude='Sub*' --exclude='*T' --exclude='Done' --exclude='cpu*' --exclude='.sl*'  ./??? /direct/gpfs01/star/pwg/fisyak/Pico/2021/RF/TFG21e/44p5GeV_fixedTarget_2021 >& rsynch.log &

# rsync -avrz -h --include='*picoDst.root' --exclude='*.*' --exclude='Sub*' --exclude='*T' --exclude='Done' --exclude='cpu*' --exclude='.sl*'  ./??? /direct/gpfs01/star/pwg/fisyak/Pico/2021/RF/TFG21g.B/dAu200_2021 >& rsynch.log &
rsync -avrz -h --include='*picoDst.root' --exclude='*.*' --exclude='Sub*' --exclude='*T' --exclude='Done' --exclude='*GB' --exclude='cpu*' --exclude='.sl*'  ./??? /direct/gpfs01/star/pwg_tasks/tfg02/2021/RF/TFG21g.B/3p85GeV_fixedTarget_2021  >& rsynch.log &
