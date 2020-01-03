#! /usr/bin/tcsh -f
set dir = 11p5GeV.B
#set dir = 5p75GeV_fixedTarget.B
cd ~/reco/2020/TFG19m/RF/${dir}
echo "pwd = $PWD"
foreach d (`ls -1d 3??`)
  cd ${d}
rsync -avrz -h                        \
    --include='*picoDst.root'                  \
    --exclude='*.*'  --exclude='Done' --exclude='.sl*'  \
    ./ rftpexp01.rhic.bnl.gov:/gpfs01/star/pwg_tasks/tfg02/2020/TFG19m/RF/${dir}/${d}
cd -
end


#    --exclude='*.log' --exclude='*.xml'--exclude='sched*' --exclude='Done' --exclude='.sl*'  \
# rsync -avz -h  --remove-source-files   -include='*event.root' ./ /hlt/cephfs/reco/2019/FF
#if ($?) exit 0;
#foreach f (`ls -1d *.root`)
#  mv ${f} ${f}.HOLD
#end
