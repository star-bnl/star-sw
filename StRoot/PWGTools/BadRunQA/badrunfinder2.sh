#!/bin/bash
# Run-by-run QA script developed by Fudan-BNL group for BES-II data
# author (c) Y. Hu, P. Tribedy, S. Choudhury, C. Tsang Jun 29, 2022
# last edit on Jun 29, 2022

file=$1
region=$2
nsigma=$3


if [ -z "$1" ]
  then
    printf "\e[31m ERROR: (Argument 1) No data file supplied, will exit \n"
     printf "\e[39m "
     printf "\e[34m Try: bash badrunfinder2.sh HISTNAME [e.g runidvsrefmult]  RUN_REGIONS [e.g run_regions.list]  NSIGMA [e.g 5, 10] \n" 
     printf "\e[39m "
    exit
fi

if [ -z "$2" ]
  then
    printf "\e[31m ERROR: (Argument 2) No region file supplied, will exit \n"
     printf "\e[39m "
     printf "\e[34m Try: bash badrunfinder2.sh HISTNAME [e.g runidvsrefmult]  RUN_REGIONS [e.g run_regions.list]  NSIGMA [e.g 5, 10] \n" 
     printf "\e[39m "
    exit
fi

if [ -z "$3" ]
  then
    printf "\e[31m ERROR: (Argument 3) No sigma supplied, will use 5 as default \n"
     printf "\e[39m "
     nsigma=5
     exit
fi

BreakPointString=`awk '{ printf("%s ", $1); }' $region`
MinBreakPoint=-1

if [ -f tmp_badrun.list ]; then
    rm tmp_badrun.list
fi

for BreakPoint in ${BreakPointString}
do
    awk '{if(($1>='$MinBreakPoint')&&($1<'$BreakPoint')){print($0)}}' $file > tmp_group_${BreakPoint}
    MinBreakPoint=$BreakPoint
    runnb=$(cat tmp_group_${BreakPoint} | wc -l )

    RegionMean=`awk '{if ($3!=0){ u += $2*(1/($3*$3)); w += (1/($3*$3)); m = u/w; s += (1/($3*$3)); sig = (1/s)^0.5} } END {print m}' "tmp_group_${BreakPoint}"`
    RegionRMS=`awk 'BEGIN {u=0;w=0} {if ($3!=0){ u += ((($2)-('${RegionMean}'))^2*(1/($3*$3))); w += (1/($3*$3))}} END {print ((u/w)^0.5)}' "tmp_group_${BreakPoint}"`

    awk '{if(((($2)-('${RegionMean}'))^2)>(('${nsigma}'*'${nsigma}')*($3*$3+('$RegionRMS'*'$RegionRMS')))){print($1)}}' tmp_group_${BreakPoint} >> tmp_badrun.list 
done
