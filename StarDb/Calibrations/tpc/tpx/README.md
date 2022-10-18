copy from /RTS/conf/tpx/tpx_gains*  and $CVSROOT/online/RTS/src/TPX_SUPPORT/tpx_gains.txt

cvs log $STAR/online/RTS/src/TPX_SUPPORT/tpx_gains.txt > tpx.log
 awk '{\
       if ($1 == "revision") printf("cvs co -r %s -p online/RTS/src/TPX_SUPPORT/tpx_gains.txt >",$2);\
       if ($1 == "Version") printf("tpc_gains.txt.%08i.%06i\n",$4,$5);\
       }' tpx.log | tee co.csh
csh -x co.csh 

foreach itpcRTS (`ls -1d itpc_gains.txt.*[A-Z]*`)
  foreach itpcCVS (`ls -1d itpc_gains.txt.20[1-2]*`)
    diff -s ${itpcRTS} ${itpcCVS} >& /dev/null
    if ($?) continue;
    echo "${itpcRTS} and ${itpcCVS} are matched"
    mv ${itpcCVS} ${itpcCVS}.matched
    ln -s ${itpcRTS} ${itpcCVS}
    break;
  end
end
