#!/bin/csh -f

source $GROUP_DIR/.stardev

set Start    = `pwd`
set Source   = /star/u/ckimstar/work/tools/merge_hadd_list.C
set Location = $1
set List     = $2

setenv ListName `basename $List | awk -F '.' '{print $1}'`
setenv SubLists mergeSub_${ListName}
setenv Log mergeLog_${ListName}
mkdir -p $Start/$SubLists
mkdir -p $Start/$Log

foreach Run (`cat $List`)

	ls ${Location}/*${Run}* > ${Run}.txt
	set nSubRuns = `wc -l ${Run}.txt | awk '{print $1}'`

	if ($nSubRuns == 0) then
		echo
		echo "No files exist to merge for run" ${Run}
		rm ${Run}.txt
		continue
	else
		mv ${Run}.txt ${SubLists}
	endif

	echo "Run" $Run", # of files to be merged:" ${nSubRuns}

setenv Condor $Run.cmd
cat << EOF > $Condor
    Universe     = vanilla
    GetEnv       = True
    Requirements = (CPU_Experiment == "star" && CPU_Speed >= 2)
    Rank         = CPU_Speed
    Priority     = 1
    PeriodicHold = (NumJobStarts >= 1 && JobStatus == 1)

    Executable = $STAR_BIN/root4star
    Arguments  = " -l -b -q '${Source}+(""$Run"", ""${SubLists}/${Run}.txt"")' "
    Initialdir = $Start
    Output     = $Start/$Log/$Run.out
    Error      = $Start/$Log/$Run.err
    Log        = $Start/$Log/$Run.log

    #Notification = Error
    #Notify_user  = ckim@bnl.gov
    +Experiment = "star"
    +Job_Type   = "cas"

    Queue
EOF
    condor_submit $Condor
    rm $Condor

end

#rm -r $Start/$SubLists
#rm -r $Start/$Log
