#! /usr/bin/env tcsh 
set list = "";
switch ($HOSTNAME) 
  case "*local":
    set list = "gcc521";
    breaksw
  case "*bnl.gov":
    set list = "gcc482 gcc492";
    breaksw
endsw
foreach gcc (${list})
  foreach opt (debug opt)
    foreach bit (32b 64b)
      if ($opt == "debug") then 
        unsetenv NODEBUG
      else      
        setenv NODEBUG yes
      endif
      setup ${gcc}
      setup ${bit}
      starver ${STAR_LEVEL}
      time cons -k >& build.${gcc}.${opt}.${bit}.`date +%Y%m%d%y:%H%M`.log &
    end
  end
end
