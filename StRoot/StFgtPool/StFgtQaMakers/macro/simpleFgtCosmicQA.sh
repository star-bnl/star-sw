#!/bin/zsh

# Usage: <filename.sfs> <runname> <quadname1> <quadname2> <quadname3> 

# can also set time bin via enviromental variable COSMIC_QA_TIMEBIN to
# the correct bit mask value

# can also set the number of events using enviromental variable COSMIC_QA_NUM_EVENTS
COSMIC_QA_TIMEBIN=${COSMIC_QA_TIMEBIN:=4}
COSMIC_QA_NUM_EVENTS=${COSMIC_QA_NUM_EVENTS:=-1}

# example
# COSMIC_QA_NUM_EVENTS=199 COSMIC_QA_TIMEBIN=4 ./simpleFgtCosmicQA.sh ../data/010_011_013_test_9 test_9 010 011 013

if [ $# -ne 5 ]; then
        echo 'Usage: <filename.sfs> <runname> <quadname1> <quadname2> <quadname3>'
        exit 127;
fi

# find the macros

QA_SRC=.
if [ ! -e $QA_SRC/simpleFgtCosmicQA.C ]; then
    QA_SRC=StRoot/StFgtQaMakers/macros

    if [ ! -e $QA_SRC/simpleFgtCosmicQA.C ]; then
        echo "Cannot fine file simpleFgtCosmicQA.C"
        exit 127;
    fi
fi

PED_SRC=.
if [ ! -e $PED_SRC/makeCosmicPeds.C ]; then
    PED_SRC=StRoot/StFgtPedMaker/macro

    if [ ! -e $PED_SRC/makeCosmicPeds.C ]; then
        echo "Cannot fine file makeCosmicPeds.C"
        exit 127;
    fi
fi

datafile=$1
runname=$2.tb$COSMIC_QA_TIMEBIN
quadname1=$3
quadname2=$4
quadname3=$5

# make the name shorter
tb=$COSMIC_QA_TIMEBIN
tbMask=`awk 'BEGIN{ print lshift(1,'$tb'); }'`
n=$COSMIC_QA_NUM_EVENTS

# make peds
pedfile=simpleFgtCosmicQA.$$.$RANDOM.$RANDOM.ped.txt

echo Making peds
echo root4star -b -q StRoot/StFgtPedMaker/macro/makeCosmicPeds.C\(\"$datafile\",\"$pedfile\",$n,$tbMask\) 
root4star -b -q StRoot/StFgtPedMaker/macro/makeCosmicPeds.C\(\"$datafile\",\"$pedfile\",$n,$tbMask\)

echo First quad
echo root4star -b -q \
    StRoot/StFgtQaMakers/macros/simpleFgtCosmicQA.C\(\"$datafile\",\"$pedfile\",0,0,\"$runname\",\"$quadname1\",$n,$tb\)
root4star -b -q \
    StRoot/StFgtQaMakers/macros/simpleFgtCosmicQA.C\(\"$datafile\",\"$pedfile\",0,0,\"$runname\",\"$quadname1\",$n,$tb\)

echo Second quad
echo root4star -b -q \
     StRoot/StFgtQaMakers/macros/simpleFgtCosmicQA.C\(\"$datafile\",\"$pedfile\",1,0,\"$runname\",\"$quadname2\",$n,$tb\)
root4star -b -q \
     StRoot/StFgtQaMakers/macros/simpleFgtCosmicQA.C\(\"$datafile\",\"$pedfile\",1,0,\"$runname\",\"$quadname2\",$n,$tb\)

echo Third quad
echo root4star -b -q \
     StRoot/StFgtQaMakers/macros/simpleFgtCosmicQA.C\(\"$datafile\",\"$pedfile\",2,0,\"$runname\",\"$quadname3\",$n,$tb\)
root4star -b -q \
     StRoot/StFgtQaMakers/macros/simpleFgtCosmicQA.C\(\"$datafile\",\"$pedfile\",2,0,\"$runname\",\"$quadname3\",$n,$tb\)

rm $pedfile
