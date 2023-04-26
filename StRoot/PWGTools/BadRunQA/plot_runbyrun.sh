#!/bin/bash

option=$1
system=$2
energy=$3
Nsigma=$4

if [ -z "$1" ]
  then
    printf "\e[31m ERROR: (Argument 1) No quanlity name supplied, will exit \n"
     printf "\e[39m "
     printf "\e[34m Try: bash plot_runbyrun.sh HISTNAME [e.g refmult]  SYSTEM [e.g Au+Au]  ENERGY [e.g 19.6] PLOT-NSIGMA [e.g 20]\n" 
     printf "\e[39m "
    exit
fi

if [ -z "$2" ]
  then
    printf "\e[33m WARNING: (Argument 2) No system supplied, will use Au+Au \n"
     printf "\e[39m "
     system=Au+Au
fi

if [ -z "$3" ]
  then
    printf "\e[33m WARNING: (Argument 3) No energy supplied, will use 19.6 \n"
     printf "\e[39m "
     energy=19.6
fi

if [ -z "$4" ]
  then
    printf "\e[33m WARNING: (Argument 4) No N-sigma for plotting supplied, will use 20 \n"
     printf "\e[39m "
     Nsigma=20     
fi

### Some necessary define for plotting color
color1='#'0066cc  # color for good run
color2='#'ff0000  # color for bad runs
color3='#'90ee90  # color for 5-RMS
color4='#'ffff77  # color for 10-RMS
color5='#'800080  # color for region border
color6='#'ff1493  # color for the arrow 

### Some necessary define for file name
OrigFile=runInfo_${option}.txt
BreakFile=breakPt.txt

file1=tmp_plot_data.txt
file2=tmp_plot_badrun.txt
file3=tmp_plot_breakpoint.txt
file4=tmp_plot_badrun_${option}.txt

if [ -f tmp_plot_data_wRMS.txt ];
then
    rm tmp_plot_data_wRMS.txt
fi

### files without badruns
grep -vwEf <(awk '{print ("^"$1) }' badrun.list) ${OrigFile} > tmp_goodrun_${OrigFile}

### Preparing for plotting
GlobalMean=`awk '{if (($3!=0)&&($1+0==$1)){ u += $2*(1/($3*$3)); w += (1/($3*$3)); m = u/w; s += (1/($3*$3)); sig = (1/s)^0.5} } END {print m}' "tmp_goodrun_${OrigFile}"`
GlobalRMS=`awk 'BEGIN {u=0;w=0} {if (($3!=0)&&($1+0==$1)){ u += (('${GlobalMean}'-$2)^2*(1/($3*$3))); w += (1/($3*$3))}} END {print ((u/w)^0.5)}' "tmp_goodrun_${OrigFile}"`
MaxRun=`cat ${OrigFile} | wc -l`
BreakPointString=`awk '{ printf("%s ", $1); }' ${BreakFile}`
MinBreakPoint=-1

for BreakPoint in ${BreakPointString}
do
    awk '{if(($1>='$MinBreakPoint')&&($1<'$BreakPoint')){print($1, $2, $3)}}' tmp_goodrun_$OrigFile > tmp_group_${BreakPoint}
    awk '{if(($1>='$MinBreakPoint')&&($1<'$BreakPoint')){print($1, $2, $3)}}' $OrigFile > tmp_group_all_${BreakPoint}
    MinBreakPoint=$BreakPoint    
    RegionMean=`awk '{if (($3!=0)&&($1+0==$1)){ u += $2*(1/($3*$3)); w += (1/($3*$3)); m = u/w; s += (1/($3*$3)); sig = (1/s)^0.5} } END {print m}' "tmp_group_${BreakPoint}"`
    RegionRMS=`awk 'BEGIN {u=0;w=0} {if (($3!=0)&&($1+0==$1)){ u += (('${RegionMean}'-$2)^2*(1/($3*$3))); w += (1/($3*$3))}} END {print ((u/w)^0.5)}' "tmp_group_${BreakPoint}"`
    awk '{print($0,"'${RegionMean}'","'${RegionRMS}'")}' tmp_group_all_${BreakPoint} >> tmp_plot_data_wRMS.txt    
done

awk '{print($1,NR,$2,$3,$4,$5)}' tmp_plot_data_wRMS.txt > ${file1}
grep -wEf <(awk '{print("^"$1)}' badrun.list) ${file1} > ${file2}
grep -wEf <(awk '{print("^"$1)}' ${BreakFile}) ${file1} > ${file3}
grep -wEf <(awk '{print("^"$1)}' <(grep ${option} badrun.list))  ${file1}  > ${file4}


/star/u/jdb/.exodus/bin/gnuplot <<EOF 2>/dev/null
set terminal postscript eps enhanced font "Helvetica, 24" color #solid 0.25
set output 'Fig_runbyrun_${option}.eps'
set term postscript size 9,3
set bar 0

LFT=0.12
WID=0.82
TOP=0.88
DY=0.71

## Global setting for the plot
## Global labels
 set label 2 at screen LFT+0.0,TOP+0.045 "STAR" font "Helvetica-Bold, 28" left
 set label 3 at screen LFT+0.07,TOP+0.04 "${system}  {/Symbol @{\140\140\140}\326}s_{NN} = ${energy} GeV" font "Helvetica, 26" left
 set label 4 at screen LFT+0.32,TOP+0.04 "Global: ${GlobalMean} {/Symbol \261} ${GlobalRMS}" font "Helvetica Oblique, 18" left

### set up the x and y
set xrange [*:${MaxRun}]
set yrange [${GlobalMean}-${Nsigma}*${GlobalRMS}:${GlobalMean}+${Nsigma}*${GlobalRMS}]
set xlabel "Run ID" font "Helvetica, 32" offset 1
set ylabel "<${option}>" font "Helvetica,28" offset 1


set tmargin at screen TOP
set bmargin at screen TOP-DY
set lmargin at screen LFT
set rmargin at screen LFT+WID

set key at screen WID-0.17,TOP+0.09 reverse Left left
set key font "Helvetica, 18"
set key samplen 1.
set key spacing 1
set key maxrows 2

set key width -8

#count and plot
 
plot "${file1}" u 2:(\$5-10.*\$6):(\$5+10.*\$6) with filledcurves fs transparent  lc rgb "${color4}" ti	 "10-RMS",\
     "${file1}" u 2:(\$5-5.*\$6):(\$5+5.*\$6) with filledcurves fs transparent lc rgb "${color3}" ti "5-RMS",\
     ${GlobalMean} w l lw 2 dashtype 2 lc 8 notitle,\
     "<awk '{if(\$3>(${GlobalMean}+${Nsigma}*${GlobalRMS})){print(\$2,${GlobalMean},0,${Nsigma}*${GlobalRMS})}}' ${file1}" using 1:2:3:4 notitle with vector lc rgb "${color6}" lw 2,\
     "<awk '{if(\$3<(${GlobalMean}-${Nsigma}*${GlobalRMS})){print(\$2,${GlobalMean},0,-${Nsigma}*${GlobalRMS})}}' ${file1}" using 1:2:3:4 notitle with vector lc rgb "${color6}" lw 2,\
     "${file3}" u (\$2-0.5):(${GlobalMean}):(${GlobalMean}+${Nsigma}*${GlobalRMS}*1.5) w e ps 0 lw 3 lc rgb "${color5}" notitle,\
     "${file1}" u 2:3:4 with e pt 7 lc rgb "${color1}" notitle,\
     "${file2}" u 2:3 w p pt 6 ps 1 lc rgb "${color2}" ti "badruns",\
     "${file4}" u 2:3 w p pt 7 ps 1 lc rgb "${color2}" ti "${option} bad"

EOF

rm tmp_group_* tmp_plot_* tmp_good*
