#!/bin/bash
inputfile=$1
mult=$2
parttype=$3

outputname=`basename $inputname .root`_$mult.root
outputnameEta=`basename $inputname .root`_${mult}Eta.root
root -b -q "StRoot/StiEvaluator/macros/CreateResolutionHistos.C(\"$inputfile\",\"$mult\",\"$parttype\")"
root -b -q "StRoot/StiEvaluator/macros/CreatePsFile.C(\"$inputfile\")"

