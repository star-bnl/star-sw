#!/bin/sh

if [ ! -e $1 ]
then
    echo "tune <minimcFile> <outdir>"
fi

if [ ! -e $2 ]
then
    echo "tune <minimcFile> <outdir>"
fi


root4star -q "StRoot/StiEvaluator/macros/ResolutionEvaluator.C(\"$1\",\"$2\",true,true)"
root4star -q "StRoot/StiEvaluator/macros/FillHistos.C(\"$1\",\"out.root\",\"$2/\")"

root -b -q "StRoot/StiEvaluator/macros/SaveEff.C(\"$2/out.root\",\"accMcPtDistributionHiAll\",\"matchedMcPtDistributionHiAll\",\"All_Hi.root\")"
root -b -q "StRoot/StiEvaluator/macros/SaveEff.C(\"$2/out.root\",\"accMcPtDistributionLoAll\",\"matchedMcPtDistributionLoAll\",\"All_Lo.root\")"
convert -antialias -rotate 90 -size 400x300 All_Hi.ps $2/All_Hi.jpg
convert -antialias -rotate 90 -size 400x300 All_Lo.ps $2/All_Lo.jpg

root4star -q "StRoot/StiEvaluator/macros/DCAPlot.C(\"$1\",\"$2\")"
convert -antialias -rotate 90 -size 400x300 dcaGl.ps $2/dcaGl.jpg
convert -antialias -rotate 90 -size 400x300 dcaPr.ps $2/dcaPr.jpg
