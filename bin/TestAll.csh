#! /usr/bin/env tcsh 
set list = "";
set domain = `hostname -d`
switch ($domain) 
  case "*local":
  case "*starp.bnl.gov":
    set list = "gcc gcc521 gcc631 gcc7 gcc8";
    breaksw
  case "*bnl.gov":
#    set list = "gcc482 gcc492";
    set list = "gcc"
    breaksw
endsw
foreach gcc (${list})
  foreach opt (debug opt)
    set bits = "64b";
    if ($gcc == "gcc") set bits = "32b 64b";
    foreach bit (${bits})
      if ($opt == "debug") then 
        unsetenv NODEBUG
      else      
        setenv NODEBUG yes
      endif
      setup ${gcc}
      setup ${bit}
      starver ${STAR_LEVEL}
      foreach proc (RC MC)
        set dir = "${proc}_${gcc}_${bit}_${opt}"
        if (! -d ${dir}) mkdir ${dir}
        cd ${dir}
	ls -1d *B.log
	if (! $?) continue
	if (${proc} == "RC") then
          root.exe -q -b -x 'bfc.C(1000,"P2019a,-hitfilt,mtd,btof,BEmcChkStat,CorrY,OSpaceZ2,OGridLeakFull,evout,NoHistos,noTags,noRunco,StiCA,picoWrite,PicoVtxVpdOrDefault","/net/l401/data/scratch1/daq/2019/083/20083024/hlt_20083024_12_02_000.daq")' >& hlt_20083024_12_02_000B.log &
	else 
          root.exe -q -b -x 'bfc.C(100,"genIn,MC.2019,McTpcAna,-bbcSim,StiCA,-hitfilt,geantOut,evout,vmc,VMCAlignment,CorrY,OSpaceZ2,OGridLeakFull,-useXgeom,NoHistos,noTags,noRunco,ZCut5cm,RunG.1","/net/l401/data/scratch2/fisyak/simu/Hijing/AuAu200/hijingAuAu200_1_1000.gener.root",0,"hijingAuAu200_1_1000.root")' >& hijingAuAu200_1_1000B.log &
        endif
        cd -;
      end 
    end
  end
end
