#! /usr/bin/env tcsh 
FPE_OFF
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
  set opts = "debug opt"
  if ($gcc == "gcc8" || gcc == "gcc631") set opts = "debug opt opt3"
  foreach opt ($opts)
    set bits = "64b";
    if ($gcc == "gcc") set bits = "32b 64b";
    foreach bit (${bits})
      foreach proc (XC RC) # MC)
	switch ($opt) 
	case "debug";
          setup nodebug
          breaksw
        case "opt3":
	   setup -O3
           breaksw
        default:
	   setup nodebug
           breaksw
        endsw
        if ($opt == "debug") then 
          unsetenv NODEBUG
        else if ($opt == "opt3") 
          setenv NODEBUG yes
        endif
        setup ${gcc}
        setup ${bit}
        starver ${STAR_LEVEL}
        set dir = ${proc}_${STAR_HOST_SYS}_${STAR_LEVEL}; echo "dir = $dir"
        if (! -d ${dir}) mkdir ${dir}
	ls -1d ${dir}/*B.log
	if (! $?) continue
        cd ${dir}
	switch ($proc)
	case "RC":
          root.exe -q -b -x 'bfc.C(1000,"P2019a,-hitfilt,mtd,btof,BEmcChkStat,CorrY,OSpaceZ2,OGridLeakFull,evout,NoHistos,noTags,noRunco,StiCA,picoWrite,PicoVtxVpdOrDefault","/net/l401/data/scratch1/daq/2019/083/20083024/hlt_20083024_12_02_000.daq")' >& hlt_20083024_12_02_000B.log &
	  breaksw
	case "XC":
	  root.exe -q -b -x 'bfc.C(1000,"P2019a,-hitfilt,mtd,btof,BEmcChkStat,CorrY,OSpaceZ2,OGridLeakFull,evout,NoHistos,noTags,noRunco,Stx,KFVertex,VFMinuitX,picoWrite,PicoVtxVpdOrDefault","/net/l401/data/scratch1/daq/2019/083/20083024/hlt_20083024_12_02_000.daq")' >& hlt_20083024_12_02_000B.log &
	
	  breaksw
	case "MC":
          root.exe -q -b -x 'bfc.C(100,"genIn,MC.2019,McTpcAna,-bbcSim,StiCA,-hitfilt,geantOut,evout,vmc,VMCAlignment,CorrY,OSpaceZ2,OGridLeakFull,-useXgeom,NoHistos,noTags,noRunco,ZCut5cm,RunG.1","/net/l401/data/scratch2/fisyak/simu/Hijing/AuAu200/hijingAuAu200_1_1000.gener.root",0,"hijingAuAu200_1_1000.root")' >& hijingAuAu200_1_1000B.log &
	  breaksw
	case default:
	  breaksw
        endsw
	if (${proc} == "RC") then
	else 
        endif
        cd -;
      end 
    end
  end
end
