#! /usr/bin/env tcsh 
set list = "";
switch ($HOSTNAME) 
  case "*local":
  case "*starp.bnl.gov":
    set list = "gcc521 gcc621";
    breaksw
  case "*bnl.gov":
#    set list = "gcc482 gcc492";
    set list = "gcc"
    breaksw
endsw
foreach gcc (${list})
  foreach opt (debug opt)
    set bits = "32b 64b";
    if ($gcc == "gcc621") set bits = "64b";
    foreach bit (${bits})
      if ($opt == "debug") then 
        unsetenv NODEBUG
      else      
        setenv NODEBUG yes
      endif
      setup ${gcc}
      setup ${bit}
      starver ${STAR_LEVEL}
      time cons -k >& build.${gcc}.${opt}.${bit}.`date +%m%d%y:%H%M`.log &
    end
  end
end
