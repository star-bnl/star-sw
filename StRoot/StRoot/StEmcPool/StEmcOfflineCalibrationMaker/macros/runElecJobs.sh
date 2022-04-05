#!/bin/csh

# break full file list into 20 file lists


set filelist = `ls /star/data05/scratch/aohlson/bemcTrees2009/trees/bemctree_*.root`
set numfiles = $#filelist

set myShell =  '# ! /bin/csh' ;

echo $numfiles

set div = 20;

set i = 1;

while ($i <= $numfiles)
    @ ind = $i % $div;
    set filename = ./eleclists/eleclist_${ind}.list
    if ($i <= $div) then
	echo "$filelist[$i]" > ${filename}
    else
	echo "$filelist[$i]" >> ${filename}
    endif
    @ i++;
end

set j = 0;

while ($j < 20)
    set listname = /star/u/aohlson/bemcCalib2009_new/eleclists/eleclist_${j}.list
    set outname = /star/data07/EMC2/elec09/elec_${j}.root
    set logname = /star/u/aohlson/bemcCalib2009_new/eleclists/eleclist_${j}.log
    set errname = /star/u/aohlson/bemcCalib2009_new/eleclists/eleclist_${j}.err
    set xmlname = /star/u/aohlson/bemcCalib2009_new/eleclists/elecjob_${j}.xml
    echo  \<\?xml version=\"1.0\" encoding=\"utf-8\" \?\> > ${xmlname}
    echo  \<job name=\"elecjob\" simulateSubmission = \"false\"\> >> ${xmlname}
    echo  \<command\> >> ${xmlname}
    echo  cd /star/u/aohlson/bemcCalib2009_new/ >> ${xmlname}
    echo  root4star -b -q electron_tree_maker.C\\\(\\\"${listname}\\\",\\\"${outname}\\\"\\\) >> ${xmlname}
    echo  \<\/command\> >> ${xmlname}
    echo  \<stdout URL=\"file:${logname}\"\/\> >> ${xmlname}
    echo  \<stderr URL=\"file:${errname}\"\/\> >> ${xmlname}
    echo  \<\/job\> >> ${xmlname}

    star-submit ${xmlname}

    @ j++;
end
