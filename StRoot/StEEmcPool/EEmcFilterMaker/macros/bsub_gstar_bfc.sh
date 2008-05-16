#!/bin/sh

# input params

neve=$1
seed=$[ 200 + $2 ]

#core=setC2_Wprod_$seed 
#job_kumac=ppWprod.kumac

#core=setC2_Wdec_$seed 
#job_kumac=ppWdec.kumac

core=setC4_Qcd_pt30c_$seed 
job_kumac=ppQCDprod.kumac

#core=setC2_Zprod_$seed 
#job_kumac=ppZprod.kumac

#core=setC2_WZ_$seed 
#job_kumac=ppWZ.kumac

#core=setC2_Wjet_$seed 
#job_kumac=ppWjet.kumac

#core=setC2_Zjet_$seed 
#job_kumac=ppZjet.kumac


jobName=JBw$neve\_$seed
fzdName=$core.fzd
echo JOB  $neve $core

#tmpDir=/star/data05/scratch/balewski/2008-fgt-pythia3/$core/
tmpDir=/tmp/balewski/$core/
inpDir=/star/u/balewski/2008-Fgt-Simu-PhysicsSamples/eval4/
#outDir=/star/institutions/iucf/balewski/2008-FGT-simu/setC2-800inv_pb/
#outDir=/star/data05/scratch/balewski/tmp-2008-fgt-simu-setC2/
outDir=/star/data04/sim/balewski/tmp-2008-fgt-simu-setC4d/

queue=star_cas_big
#queue=star_cas_short


inpArg="$fzdName"
echo inpArg=$inpArg


chain="DbV20080310 trs -ssd upgr13  Idst IAna l0 tpcI fcf -ftpc Tree logger ITTF Sti StiRnd  -IstIT -SvtIt -NoSvtIt SvtCL,svtDb -SsdIt MakeEvent McEvent geant evout geantout IdTruth  bbcSim emcY2 EEfs bigbig -dstout fzin -MiniMcMk McEvOut clearmem -ctbMatchVtx VFPPV eemcDb beamLine"

# full log file
glog=$core.glog
log=$core.log
blog=$core.blog


#-----------------
#echo 'started job --> '$log

# to use this script without LSF swap & uncomment next two lines

#csh <<EOF
bsub -R "rusage[mem=650]" -q $queue  -J $jobName -o $outDir$blog   << EOF
#!/bin/csh

#source $GROUP_PATH/group_env.csh
 time
 date
 uname -a

rm -rf  $tmpDir
mkdir -p $tmpDir
cd $tmpDir
ln -s $inpDir/.sl44_gcc346
ln -s $inpDir/bfc.C
ln -s $inpDir/$job_kumac

echo PRESET DIR IS:
pwd
ls -l
#source ${GROUP_DIR}/.stardev
source ${GROUP_DIR}/.starver SL08a
echo ROOT$STAR IS LOCATED AT:
 echo $STAR
which root4star
echo PRINT STARVER: 
echo $STAR
echo RUN ROOT4STAR ...


echo "in scratch dir ..."
(time starsim -w 0 -b ./$job_kumac $neve $fzdName  $seed)>& $glog
echo GSTAR done

# The job
root4star -b <<EOF2  >&  $tmpDir$log
 .x bfc.C($neve,"$chain","$inpArg")
 .q
EOF2

echo ROOT4STAR DONE
pwd

gzip $log
gzip $glog
ls -l
cp -rp *log.gz  $outDir
cp -rp *root $outDir
cp -rp $fzdName $outDir
cp -rp star_filter.hbook $outDir/$core\_star_filter.hbook

rm -r $tmpDir

EOF





