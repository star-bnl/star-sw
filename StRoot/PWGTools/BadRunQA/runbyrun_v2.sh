#!/bin/bash
# Run-by-run QA script developed for STAR BES-II run-by-run QA
# authors (c) C.Y. Tsang, Y. Hu, P. Tribedy 2022-07-19
### PLEASE WRITE TO <ctsang@bnl.gov>, <yuhu@bnl.gov>, <yuhu@lbl.gov>, <ptribedy@bnl.gov> for any questions on the package
# last edit by Y. Hu, and C.Y. Tsang, 2022-07-21
# last edit by Y.Hu to fix 1) the issue in badrunfind.sh when calculate the RMS; 2) the plotting issue (missing last run) 2022-08-25

if printf '%s ' "${@}" | grep -ow -- '-d' >/dev/null;
then 
    printf "\e[33m Enable debug mode where stderr is not suppressed. \n" 1>&2
else
    # run current script with debug mode enabled, but all stderr are redirected to void
    bash ${@:0} -d 2>/dev/null
    printf "\e[35m If there is anything wrong with the result, or if the script generates no result at all, please run it again with the debug flag -d appended to the end of your arguments.\n"
    printf "\e[35m Check and see if there are any suppressed warnings and report them.\n"
    printf "\e[39m"
    exit
fi



ROOTFILE=$1
QALIST=$2
SYSTEM=$3
ENERGY=$4

echo '-------------------------------------------------------------------------------'
echo '-------------------------------------------------------------------------------'
printf "\e[35m-------------------Run-by-Run QA script for STAR data analysis ---------------- \n"
printf "\e[39m-------------------------------- New Version 2.3 ------------------------------ \n"
echo '-------------------Input1: ROOT file with profile histograms vs runid ---------'
echo '-------------------Input2: a list of profile histograms -----------------------'
echo '-------------------Output: lists of bad runs & stable regions -----------------'
echo '-------------------------------------------------------------------------------'
echo '--------------------------------Example to run --------------------------------'
printf "\e[35m--------- bash runbyrun_v2.sh qahist.root QA_variable.list Au+Au 19.6 -------- \n"
printf "\e[39m------------------------------------------------------------------------------- \n"
echo '-------------------- Developed for STAR BES-II run-by-run QA ------------------'
echo '----------------------------  last edit on 2022-10-22 -------------------------'
printf "\e[35m------- Contact: <ctsang@bnl.gov>, <yuhu@bnl.gov>, <ptribedy@bnl.gov> --------- \n"
printf "\e[39m------------------------------------------------------------------------------- \n"
echo '-------------------------------------------------------------------------------'
echo '-------------------------------------------------------------------------------'


if [ -z "$1" ]
then
    printf "\e[31m ERROR: (Argument 1) No ROOT file supplied, will exit \n"
    printf "\e[39m "
    printf "\e[34m Try: bash runbyrun_v2.sh FILENAME [e.g qahist.root ] NAMELIST [e.g name.list ] SYSTEM [e.g Au+Au ] ENERGY [19.6] (Optional) Enable debug -d\n" 
    printf "\e[35m e.g.:  bash runbyrun_v2.sh qahist.root QA_variable.list  Au+Au 19.6\n"
    printf "\e[39m "
    exit
fi

if [ -z "$2" ]
then
    printf "\e[31m ERROR: (Argument 2) No NAMELIST supplied, will exit \n"
    printf "\e[39m "
    printf "\e[34m Try: bash runbyrun_v2.sh FILENAME [e.g qahist.root ] NAMELIST [e.g name.list ] SYSTEM [e.g Au+Au ] ENERGY [19.6] (Optional) Enable debug -d \n" 
    printf "\e[35m e.g.:  bash runbyrun_v2.sh qahist.root QA_variable.list  Au+Au 19.6 \n"
    printf "\e[39m "
    exit
fi

if [ -z "$3" ]
then
    printf "\e[33m Warning: (Argument 3) No SYSTEM supplied, will use Au+Au \n"
    printf "\e[39m "
    printf "\e[34m To change, try: bash runbyrun_v2.sh FILENAME [e.g qahist.root ] NAMELIST [e.g name.list ]  SYSTEM [e.g Au+Au ] ENERGY [19.6] (Optional) Enable debug -d\n" 
    printf "\e[35m e.g.:  bash runbyrun_v2.sh qahist.root QA_variable.list Au+Au 19.6\n"
    printf "\e[39m "
    SYSTEM=Au+Au
fi


if [ -z "$4" ]
then
    printf "\e[33m Warning: (Argument 4) No ENERGY supplied, will use 19.6 \n"
    printf "\e[39m "
    printf "\e[34m To change, try: bash runbyrun_v2.sh FILENAME [e.g qahist.root ] NAMELIST [e.g name.list ]  SYSTEM [e.g Au+Au ] ENERGY [19.6] (Optional) Enable debug -d\n" 
    printf "\e[35m e.g.:  bash runbyrun_v2.sh qahist.root QA_variable.list Au+Au 19.6\n"
    printf "\e[39m "
    ENERGY=19.6
fi
    
WHOLEFILENAME=runInfoAllBadRuns
WHOLEFILE=${WHOLEFILENAME}.txt

#### Load the QA list from file
hist_option=$(awk '{printf "%s" FS,$1}' ${QALIST})

#### STEP 1 Prepare the data file from the root file
echo "Getting data file from root ......"
hist_count=1
for option in $hist_option
do
   root -l "readdata.C+("'"'"${ROOTFILE}"'"'","'"'"${option}"'"'")" -q >/dev/null

   if [[ ${hist_count} > 1 ]]; then  
       join ${WHOLEFILE} <(awk '{print($1,$2,$3)}' runInfo_${option}.txt) > ${WHOLEFILENAME}_tmp 
       mv ${WHOLEFILENAME}_tmp ${WHOLEFILE}
   else
       cp runInfo_${option}.txt  ${WHOLEFILE}
   fi
   hist_count=$((${hist_count}+1))   
done

#### STEP 2 run through the algorithm (1st time)
source ~/.bashrc
conda activate Segment 

function findBadRuns() { 
    hist_option=$1
    WHOLEFILE=$2
    AGGRESSIVE=$3
    echo " "
    echo "Running the new jumpcheck algorithm..."
    python Segmentation2.py -i ${WHOLEFILE} -o breakPt.txt -mr 5 --pen ${AGGRESSIVE} >/dev/null
    
    MaxRun=`awk 'END {print($1)}' ${WHOLEFILE}`
    echo $((${MaxRun}+1)) >> breakPt.txt
    
    if [ -f badrun.list ];
    then 
        rm badrun.list
    fi
    
    hist_count=1
    for option in ${hist_option}
    do
        bash badrunfinder2.sh runInfo_${option}.txt breakPt.txt 5
        awk '{print($1,"'${option}'")}' tmp_badrun.list > badrun_${option}.list
        if [[ ${hist_count} > 1 ]]; then
    	join <(sort badrun.list) <(sort badrun_${option}.list) -a 1 > l1
    	join <(sort badrun.list) <(sort badrun_${option}.list) -a 2 > l2
    	cat l1 l2 | sort -n -u -k1 >  badrun.list
    	rm l1 l2
        else
    	sort -n -u -k1 badrun_${option}.list > badrun.list
        fi
       hist_count=$((${hist_count}+1))    
    done
    
    rm tmp_*
    
    grep -vwEf <(awk '{print ("^"$1) }' badrun.list) ${WHOLEFILE}  > ${WHOLEFILENAME}_2.txt
    NumA=`cat badrun.list | wc -l`
    
    if [[ ${NumA} == 0 ]]; then  
        echo "#####################################"
        echo "Run-by-Run QA check is finished"
        echo "No bad-run is found!"
        echo "Here is the jump points (run-region)"
        echo "You can find them in the <breakPt.txt> file"
        cat breakPt.txt
        echo " "
    else
        for Rindex in {2..10}
        do
    	NumA=`cat badrun.list | wc -l`
    	
    	python Segmentation2.py -i ${WHOLEFILENAME}_${Rindex}.txt -o breakPt.txt -mr 5 --pen ${AGGRESSIVE} >/dev/null
    	echo $((${MaxRun}+1)) >> breakPt.txt
    
    	for option in ${hist_option}
    	do
    	    grep -vwEf <(awk '{print ("^"$1) }' badrun.list) runInfo_${option}.txt > runInfo_${option}_${Rindex}.txt 
    	    bash badrunfinder2.sh runInfo_${option}_${Rindex}.txt breakPt.txt 5
    	    awk '{print($1,"'${option}'")}' tmp_badrun.list > badrun_${option}.list
    	    join <(sort badrun.list) <(sort badrun_${option}.list) -a 1 > l1
    	    join <(sort badrun.list) <(sort badrun_${option}.list) -a 2 > l2
    	    cat l1 l2 | sort -n -u -k1 >  badrun.list
    	    rm l1 l2
    	    rm runInfo_${option}_${Rindex}.txt
    	done
    
    	NumB=`cat badrun.list | wc -l`
    	
    	Rindex1=$((${Rindex}+1))
    	grep -vwEf <(awk '{print ("^"$1) }' badrun.list) ${WHOLEFILENAME}_${Rindex}.txt > ${WHOLEFILENAME}_${Rindex1}.txt
    
    	if [ $NumA -eq $NumB ]; then
    	    break;
    	fi
    
        done
    
        rm runInfoAllBadRuns_*.txt tmp_* badrun_*.list
    fi
}

# loop through all segmentation settings
findBadRunsAndRename() {
    hist_option=$1
    WHOLEFILE=$2
    s=$3
    mkdir tempdir_${s}
    cp ${WHOLEFILE} tempdir_${s}
    cp Segmentation2.py tempdir_${s}
    cp badrunfinder2.sh tempdir_${s}
    cp runInfo_* tempdir_${s}
    cd tempdir_${s}
    findBadRuns "$hist_option" $WHOLEFILE $s
    cd ..
    mv tempdir_${s}/badrun.list ${s}_badrun.list
    mv tempdir_${s}/breakPt.txt ${s}_breakPt.txt
    rm -rf tempdir_${s}
}

sarray=('0.5' '1' '2' '5' '9')

for s in ${sarray[@]}
do
    echo ""
    echo "Testing segmentation with aggressiveness level " ${s}
    findBadRunsAndRename "$hist_option" $WHOLEFILE $s &
done

wait

#choose segmentation results that returns the greatest number of breakpoints
NOBADCURR=0
for s in ${sarray[@]}
do
    NOBAD=$(more ${s}_badrun.list | wc -l )
    echo "Find " ${NOBAD} " bad runs with -s " ${s}
    if (( NOBAD >= NOBADCURR )); then
        NOBADCURR=${NOBAD}
        [ -e badrun.list ] && rm badrun.list
        [ -e breakPt.txt ] && rm breakPt.txt
        mv ${s}_badrun.list badrun.list
        mv ${s}_breakPt.txt breakPt.txt
    else
        rm ${s}_badrun.list
        rm ${s}_breakPt.txt
    fi
done

FNL_bad=$(cat badrun.list | wc -l)
FNL_run=$(($(cat ${WHOLEFILE} | wc -l)-1))

f_bad=`echo "scale=1; 100*${FNL_bad}/${FNL_run}" |bc`

# show results
echo " "
echo "#####################################"
echo "Run-by-Run QA check is finished"
echo "Here is the jump points (run-region)"
echo "You can find them in the <breakPt.txt> file"
cat breakPt.txt
echo " "
echo 'We found '${FNL_bad}' out of '${FNL_run}' runs marked as bad('${f_bad}'%)'
echo "You can find them in the <badrun.list> file"
echo  "Here is the bad-run list"
cat badrun.list


echo " "
echo "#####################################"
echo "Now working on plotting"
#### Need the orignal file to plot
for option in $hist_option
do
    echo "Plotting " ${option} "..."
    bash plot_runbyrun.sh ${option} ${SYSTEM} ${ENERGY} 20 
done
