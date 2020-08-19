#! /usr/bin/env tcsh 
set list = "";
set domain = `hostname -d`
switch ($domain) 
  case "*local":
  case "*starp.bnl.gov":
    set list = "gcc485 gcc531 gcc/6.3 gcc631 gcc7 gcc8 gcc9 gcc/10.1.0";
    breaksw
  case "*bnl.gov":
#    set list = "gcc482 gcc492 gcc/6.3 gcc/10";
    set list = "gcc"
    breaksw
endsw
foreach gcc (${list})
  set gccv = `echo ${gcc} | sed -e 's/\///g'`;
  set opts = "debug opt"
#  if ($gcc == "gcc8" || gcc == "gcc631") set opts = "debug opt opt3"
  foreach opt ($opts)
    set bits = "64b";
    if ($gcc == "gcc") set bits = "32b 64b";
    foreach bit (${bits})
      unsetenv NODEBUG
      if ($opt == "opt")  setenv NODEBUG YES 
      if ($opt == "opt3") setenv NODEBUG -O3 
      setup ${gcc}
      setup ${bit}
      starver ${STAR_LEVEL}
      set log = build.${gccv}.${opt}.${bit}.`date +%m%d%y:%H%M.`${HOST}.log;
#      printenv > ${log};
      time cons -k >>& ${log} &
    end
  end
end
