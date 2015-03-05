#!/bin/csh

# break full file list into 200 file lists

set filelist = `ls /gpfs01/star/subsysg/EMC2/bemcTrees2012/pp200Trees/bemcCalibTree_*.root`
set numfiles = $#filelist

set myShell =  '# ! /bin/csh' ;

echo $numfiles

set div = 200;

set i = 1;

while ($i <= $numfiles)
    @ ind = $i % $div;
    set filename = /gpfs01/star/i_uky/jkadkins/bemcTrees2012/electronTrees/logs/electronlist_${ind}.list
    if ($i <= $div) then
	echo "$filelist[$i]" > ${filename}
    else
	echo "$filelist[$i]" >> ${filename}
    endif
    @ i++;
end

set j = 0;

while ($j < 200)
    set listname = /gpfs01/star/i_uky/jkadkins/bemcTrees2012/electronTrees/logs/electronlist_${j}.list
    set outname = electron_${j}.root
    set outdir = /gpfs01/star/i_uky/jkadkins/bemcTrees2012/electronTrees/
    set logname = /gpfs01/star/i_uky/jkadkins/bemcTrees2012/electronTrees/logs/electronlist_${j}.log
    set logdir = /gpfs01/star/i_uky/jkadkins/bemcTrees2012/electronTrees/logs/
    set errname = /gpfs01/star/i_uky/jkadkins/bemcTrees2012/electronTrees/logs/electronlist_${j}.err
    set xmlname = /gpfs01/star/i_uky/jkadkins/bemcTrees2012/electronTrees/logs/electronjob_${j}.xml
    set mipFile = mip.gains
    set geantFile = geant_fits.root
    echo \<\?xml version=\"1.0\" encoding=\"utf-8\" \?\> > ${xmlname}
    echo \<job name=\"elecjob\" simulateSubmission = \"false\"\> >> ${xmlname}
    echo \<command\> >> ${xmlname}
    echo root4star -b -q RunElectronAnalysis.C\\\(1e6,\\\"${listname}\\\",\\\"${mipFile}\\\",\\\"${geantFile}\\\",\\\"${outname}\\\"\\\) \&gt\; \&amp\; electronLog_${j}.log >> ${xmlname}
    echo \<\/command\> >> ${xmlname}
    echo \<SandBox installer=\"ZIP\"\> >> ${xmlname}
    echo \<Package name=\"electronAnalysisCode\"\> >> ${xmlname}
    echo \<File\>file\:\/star\/u\/jkadkins\/bemcCalib2012\/StRoot\/StEmcPool\/StEmcOfflineCalibrationMaker\/macros\/RunElectronAnalysis.C \<\/File\> >> ${xmlname}
    echo \<File\>file\:\/star\/u\/jkadkins\/bemcCalib2012\/StRoot\/StEmcPool\/StEmcOfflineCalibrationMaker\/macros\/mip.gains \<\/File\> >> ${xmlname}
    echo \<File\>file\:\/star\/u\/jkadkins\/bemcCalib2012\/StRoot\/StEmcPool\/StEmcOfflineCalibrationMaker\/macros\/geant_fits.root \<\/File\> >> ${xmlname}
    echo \<File\>file\:\/star\/u\/jkadkins\/bemcCalib2012\/.sl64\_gcc447 \<\/File\> >> ${xmlname}
    echo \<\/Package\> >> ${xmlname}
    echo \<\/SandBox\> >> ${xmlname}
    echo \<stdout URL=\"file:${logname}\"\/\> >> ${xmlname} 
    echo \<stderr URL=\"file:${errname}\"\/\> >> ${xmlname}
    echo \<output fromScratch=\"\electron\*.root\" toURL=\"file:${outdir}\"\/\> >> ${xmlname}
    echo \<output fromScratch=\"\*.log\" toURL=\"file:${logdir}\"\/\> >> ${xmlname}
    echo  \<\/job\> >> ${xmlname}

    star-submit ${xmlname}

    @ j++;
end
