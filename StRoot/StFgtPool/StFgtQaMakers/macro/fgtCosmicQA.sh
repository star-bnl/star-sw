#!/bin/zsh

# Usage: <filename.sfs> <runname> <quadname1> <quadname2> <quadname3> 

# can also set time bin via enviromental variable COSMIC_QA_TIMEBIN to
# the correct bit mask value

### variables which can be overwritten with enviromental variables

COSMIC_QA_TIMEBIN=${COSMIC_QA_TIMEBIN:=4}
COSMIC_QA_NUM_EVENTS=${COSMIC_QA_NUM_EVENTS:=-1}
COSMIC_QA_NUM_PED_EVENTS=${COSMIC_QA_NUM_PED_EVENTS:=1000}
COSMIC_QA_CUT_SHORT=${COSMIC_QA_CUT_SHORT:=1}
COSMIC_QA_SKIP_COR=${COSMIC_QA_SKIP_COR:=0}

# number of channels to average over for correlation plot
COSMIC_QA_NUM_COR_CHAN=${COSMIC_QA_NUM_COR_CHAN:=8}


# example
# COSMIC_QA_NUM_EVENTS=199 COSMIC_QA_TIMEBIN=4 ./fgtCosmicQA.sh ../data/010_011_013_test_9 test_9 010 011 013

if [ $# -ne 5 ]; then
        echo 'Usage: <filename.sfs> <runname> <quadname1> <quadname2> <quadname3>'
        exit 127;
fi

# find the macros

QA_SRC=.
if [ ! -e $QA_SRC/fgtCosmicQA.C ]; then
    QA_SRC=StRoot/StFgtQaMakers/macros

    if [ ! -e $QA_SRC/fgtCosmicQA.C ]; then
        echo "Cannot find file simpleFgtCosmicQA.C"
        exit 127;
    fi
fi

PED_SRC=.
if [ ! -e $PED_SRC/makeCosmicPeds.C ]; then
    PED_SRC=StRoot/StFgtPedMaker/macro

    if [ ! -e $PED_SRC/makeCosmicPeds.C ]; then
        echo "Cannot find file makeCosmicPeds.C"
        exit 127;
    fi
fi

datafile=$1
runname=$2.tb$COSMIC_QA_TIMEBIN

# names of the quadrants
qname1=$3
qname2=$4
qname3=$5

# make the name shorter
tb=$COSMIC_QA_TIMEBIN
tbMask=`awk 'BEGIN{ print lshift(1,'$tb'); }'`
n=$COSMIC_QA_NUM_EVENTS
nC=$COSMIC_QA_NUM_COR_CHAN
nP=$COSMIC_QA_NUM_PED_EVENTS
cut=$COSMIC_QA_CUT_SHORT
skip=$COSMIC_QA_SKIP_COR

# make peds
pedfile=fgtCosmicQA.$$.$RANDOM.$RANDOM.ped.txt
pedfile=simpleFgtCosmicQA.ped.txt

echo Making peds
echo root4star -b -q $PED_SRC/makeCosmicPeds.C\(\"$datafile\",\"$pedfile\",$nP,$tbMask,$cut\) 
root4star -b -q $PED_SRC/makeCosmicPeds.C\(\"$datafile\",\"$pedfile\",$nP,$tbMask,$cut\)

quadname=( $qname1 $qname2 $qname3 )

#for i in 2; do
for i in 0 1 2; do
    i2=$(( $i + 1 ))

    echo '--->' Quad $quadname[$i2]
    echo root4star -b -q \
        $QA_SRC/fgtCosmicQA.C\(\"$datafile\",\"$pedfile\",$i,0,\"$runname\",\"$quadname[$i2]\",$n,$tb,$cut,$skip\)
    root4star -b -q \
        $QA_SRC/fgtCosmicQA.C\(\"$datafile\",\"$pedfile\",$i,0,\"$runname\",\"$quadname[$i2]\",$n,$tb,$cut,$skip\)

done

#ls *.pdf
#rm $pedfile
