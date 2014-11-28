#!/bin/csh

# break full file list into 20 file lists

#set filelist = `ls /star/data07/EMC2/trees/bemctree_*.root`
set filelist = `ls /star/data05/scratch/aohlson/bemcTrees2009/trees/bemctree_*.root`
set numfiles = $#filelist

set myShell =  '# ! /bin/csh' ;

echo $numfiles

set div = 20;

set i = 1;

while ($i <= $numfiles)
    @ ind = ($i % $div);
    set filename = ./miplists/miplist_${ind}.list
    if ($i <= $div) then
	echo "$filelist[$i]" > ${filename}
    else
	echo "$filelist[$i]" >> ${filename}
    endif
    @ i++;
end

set j = 0;

while ($j < 20)
    set listname = /star/u/aohlson/bemcCalib2009/miplists/miplist_${j}.list
    set outname = /star/data07/EMC2/mip09/mip_${j}.root
    set logname = /star/u/aohlson/bemcCalib2009/miplists/miplist_${j}.log
    set errname = /star/u/aohlson/bemcCalib2009/miplists/miplist_${j}.err
    set xmlname = /star/u/aohlson/bemcCalib2009/miplists/mipjob_${j}.xml
    echo  \<\?xml version=\"1.0\" encoding=\"utf-8\" \?\> > ${xmlname}
    echo  \<job name=\"mipjob\" simulateSubmission = \"false\"\> >> ${xmlname}
    echo  \<command\> >> ${xmlname}
    echo  cd /star/u/aohlson/bemcCalib2009/ >> ${xmlname}
    echo  root4star -b -q mip_histogram_maker.C\\\(\\\"${listname}\\\",\\\"${outname}\\\"\\\) >> ${xmlname}
    echo  \<\/command\> >> ${xmlname}
    echo  \<stdout URL=\"file:${logname}\"\/\> >> ${xmlname}
    echo  \<stderr URL=\"file:${errname}\"\/\> >> ${xmlname}
    echo  \<\/job\> >> ${xmlname}

    star-submit ${xmlname}

    @ j++;
end
