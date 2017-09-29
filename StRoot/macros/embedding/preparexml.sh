#!/bin/sh

#Please copy this file to the working directory for embedding submission

if [[ $HOST =~ "cori" ]] ; then
 module load python/3.5-anaconda
 cp StRoot/macros/embedding/cori/farmerQAmonitor.ipynb ./
fi

#use perl script at PDSF, and python script at Cori
#perl StRoot/macros/embedding/get_embedding_xml.pl \
python StRoot/macros/embedding/cori/prepEmbedTaskList.py \
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
 -fSetRange 100-109 \
 -fSetCPUHours 1321 \
 -outPath /global/cscratch1/sd/zhux/embedding

#The last three lines (fSetRange, fSetCPUHours, outPath) are specific to the Cori script, comment them out for PDSF.
