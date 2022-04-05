#!/bin/csh



set j = 0;

while ($j < 20)
    set inname = /star/data07/EMC2/elec09/elec_${j}.root
    set outname = /star/data07/EMC2/elec09/finalelec_${j}.root
    set logname = /star/u/aohlson/bemcCalib2009_new/finalelec/eleclog_${j}.log
    set errname = /star/u/aohlson/bemcCalib2009_new/finalelec/elecerr_${j}.err
    set xmlname = /star/u/aohlson/bemcCalib2009_new/finalelec/elecjob_${j}.xml
    echo  \<\?xml version=\"1.0\" encoding=\"utf-8\" \?\> > ${xmlname}
    echo  \<job name=\"elecjob\" simulateSubmission = \"false\"\> >> ${xmlname}
    echo  \<command\> >> ${xmlname}
    echo  cd /star/u/aohlson/bemcCalib2009_new/ >> ${xmlname}
    echo  root4star -b -q electron_master_alt.C\\\(\\\"${inname}\\\",\\\"${outname}\\\"\\\) >> ${xmlname}
    echo  \<\/command\> >> ${xmlname}
    echo  \<stdout URL=\"file:${logname}\"\/\> >> ${xmlname}
    echo  \<stderr URL=\"file:${errname}\"\/\> >> ${xmlname}
    echo  \<\/job\> >> ${xmlname}


    star-submit ${xmlname}

    @ j++;
end


