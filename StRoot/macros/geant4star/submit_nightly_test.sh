#!/usr/bin/bash

# Submit simulation jobs
#
# optional arguments:
#   -h, --help            show this help message and exit
#   -w WORKINGDIR, --workdir WORKINGDIR
#                         Submission directory (default: /gpfs/mnt/gpfs01/star/simu/simu/jwebb/2026/Geant4STAR/geant4star-sl26-prune)
#   -l LOGDIR, --logdir LOGDIR
#                         Log directory (default: /star/simu/simu/g4star/.simulations/log/)
#   -o OUTDIR, --outdir OUTDIR
#                         Output directory (default: /star/simu/simu/g4star/.simulations/out/)
#   -j JOBDIR, --jobdir JOBDIR
#                         Job directory (default: /star/simu/simu/g4star/.simulations/job/)
#   -b BASELIB, --baselib BASELIB
#                         Specify the library (default: SL26x)
#   --overlay OVERLAY     Spack overlay on the environment (default: config/v0.3.0-rhel7-root6.24.06)
#   -g GROUPDIR, --group GROUPDIR
#                         Select the STAR group directory (default: /star/u/jeromel/work/STAR/group)
#   --nevents NEVENTS     Set number of events per job (default: 1)
#   --njobs NJOBS         Set the number of jobs to run (default: 1)
#   --offset OFFSET       Sets production tag (default: 0)
#   --series SERIES       Runs all productions in a given series... or matches anything containing (default: None)
#   --list                Lists available series and exists (default: False)
#   --verbose             Full output from SUMS submission (default: False)
#   --no-submit           Print command to submit and exit. (default: True)

TODAY=$( date +%a )

#WORKINGDIR=`pwd`
LOGDIR=/star/data19/G4STAR/tests/${TODAY}/log
OUTDIR=/star/data19/G4STAR/tests/${TODAY}/out
JOBDIR=/star/data19/G4STAR/tests/${TODAY}/job
BASELIB=SL26x
NEVENTS=10
NJOBS=1

# Remove prior tests
rm    -r ${LOGDIR} ${OUTDIR} ${JOBDIR}
mkdir -p ${LOGDIR} ${OUTDIR} ${JOBDIR}

OPTS="$@"

SYS=${STAR}
#SYS=.

for tag in $( root -n -q -b ${STAR}/StRoot/macros/geant4star/StarSimOpts.h\(-1\) | awk '/rcf/{ print $1 }' )
do

    echo
    echo $tag
    echo
    ${SYS}/StRoot/macros/geant4star/submit_simulation_jobs.py ${OPTS} --series ${tag}  -l ${LOGDIR} -o ${OUTDIR} -j ${JOBDIR} -b ${BASELIB} --nevents ${NEVENTS} --njobs ${NJOBS}

done


for tag in P21icAuAu19 P21idIsobar200
do

    echo
    echo $tag
    echo
    ${SYS}/StRoot/macros/geant4star/submit_simulation_jobs.py ${OPTS} --series ${tag}  -l ${LOGDIR} -o ${OUTDIR} -j ${JOBDIR} -b ${BASELIB} --nevents ${NEVENTS} --njobs ${NJOBS} --embedding
    
done
