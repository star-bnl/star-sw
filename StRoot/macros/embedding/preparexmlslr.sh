#!/bin/sh

#1. copy this script to the embedding working directory for a specific request.
#2. edit the options for $script first according to simulation request page, run this script at PDSF first, 
#   generate the xml script for embedding job submission at PDSF.
#3. submit the job at PDSF, produce one fset there, usually the fset ID# starts from "100".
#4. obtain the number of events and the CPU hours per fset with the fsetstat.sh script.
#5. calculate the number of fsets according to the requested statistics
#6. edit the three options for Cori accordingly for full embedding production
#7. run this script again at Cori, generate the .slr script for SLURM job submission.
#8. submit the SLURM job for the full production

if [[ $HOST =~ "cori" ]] ; then
 module load python/3.5-anaconda
 cp StRoot/macros/embedding/cori/farmerQAmonitor.ipynb ./
 ln -s StRoot/macros/embedding/fsetstat.sh
 ln -s StRoot/macros/embedding/cori/resumecori.sh
 script="python StRoot/macros/embedding/cori/prepEmbedTaskList.py"

#EDIT the following three options before use this script at Cori farm
 fset="-fSetRange 101-109"
 cpuh="-fSetCPUHours 1321"
 outp="-outPath /global/cscratch1/sd/$USER/embedding"

else
 ln -s StRoot/macros/embedding/submitxml.sh
 ln -s StRoot/macros/embedding/findfailed.sh
 ln -s StRoot/macros/embedding/fsetstat.sh
 script="perl StRoot/macros/embedding/get_embedding_xml.pl"
 fset=""
 cpuh=""
 outp=""
fi

#EDIT the following options according to the embedding request page
$script \
 -trg AuAu200_production_2011 -production P11id -lib SL11d_embed \
 -mixer StRoot/macros/embedding/bfcMixer_Tpx.C -prodname P11idAuAu200 \
 -r 20172901 \
 -geantid 10018 -particle Lambda -mode FlatPt \
 -trigger 350003 -trigger 350013 -trigger 350023 -trigger 350033 -trigger 350043 \
 -z 30.0 -vrcut 2.0 \
 -ymin -0.5 -ymax 0.5 -ptmin 0 -ptmax 5.0 \
 -mult 0.05 \
 -local \
 -daq /global/projecta/projectdirs/starprod/daq/2011/2011_auau200 \
 -tag /global/projecta/projectdirs/starprod/tags/2011_auau200 \
 $fset \
 $cpuh \
 $outp

