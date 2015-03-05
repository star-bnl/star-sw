#!/bin/csh
# XML formatting redone to conform to the input/output problem in mid/late 2013 - Kevin Adkins
# break full file list into 20 file lists
set filelist = `ls /gpfs01/star/subsysg/EMC2/bemcTrees2012/pp200Trees/bemcCalibTree_*.root`
set numfiles = $#filelist

set myShell =  '# ! /bin/csh' ;

echo $numfiles

set div = 20;

set i = 1;

while ($i <= $numfiles)
    @ ind = ($i % $div);
    set filename = /gpfs01/star/i_uky/jkadkins/bemcTrees2012/mipTrees/logs/miplist_${ind}.list
    if ($i <= $div) then
	echo "$filelist[$i]" > ${filename}
    else
	echo "$filelist[$i]" >> ${filename}
    endif
    @ i++;
end

set j = 0;

while ($j < 20)
    set listname = /gpfs01/star/i_uky/jkadkins/bemcTrees2012/mipTrees/logs/miplist_${j}.list
    set outname = mip_${j}.root
    set outdir = /gpfs01/star/i_uky/jkadkins/bemcTrees2012/mipTrees/
    set logname = /gpfs01/star/i_uky/jkadkins/bemcTrees2012/mipTrees/logs/miplist_${j}.log
    set logdir = /gpfs01/star/i_uky/jkadkins/bemcTrees2012/mipTrees/logs/
    set errname = /gpfs01/star/i_uky/jkadkins/bemcTrees2012/mipTrees/logs/miplist_${j}.err
    set xmlname = /gpfs01/star/i_uky/jkadkins/bemcTrees2012/mipTrees/logs/mipjob_${j}.xml
    echo  \<\?xml version=\"1.0\" encoding=\"utf-8\" \?\> > ${xmlname}
    echo  \<job name=\"mipjob\" simulateSubmission = \"false\"\> >> ${xmlname}
    echo  \<command\> >> ${xmlname}
    echo  root4star -b -q RunMipHistogramMaker.C\\\(1e8,\\\"${listname}\\\",\\\"${outname}\\\"\\\) \&gt\; \&amp\; mipLog_${j}.log >> ${xmlname}
    echo  \<\/command\> >> ${xmlname}
    echo \<SandBox installer=\"ZIP\"\> >> ${xmlname}
    echo \<Package name=\"mipAnalysisCode\"\> >> ${xmlname}
    echo \<File\>file\:\/star\/u\/jkadkins\/bemcCalib2012\/StRoot\/StEmcPool\/StEmcOfflineCalibrationMaker\/macros\/RunMipHistogramMaker.C \<\/File\> >> ${xmlname}
    echo \<File\>file\:\/star\/u\/jkadkins\/bemcCalib2012\/.sl64\_gcc447 \<\/File\> >> ${xmlname}
    echo \<\/Package\> >> ${xmlname}
    echo \<\/SandBox\> >> ${xmlname}
    echo \<stdout URL=\"file:${logname}\"\/\> >> ${xmlname} 
    echo \<stderr URL=\"file:${errname}\"\/\> >> ${xmlname}
    echo \<output fromScratch=\"\*.root\" toURL=\"file:${outdir}\"\/\> >> ${xmlname}
    echo \<output fromScratch=\"\*.log\" toURL=\"file:${logdir}\"\/\> >> ${xmlname}
    echo  \<\/job\> >> ${xmlname}

    star-submit ${xmlname}

    @ j++;
end
