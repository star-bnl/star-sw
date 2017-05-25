#! /usr/bin/env tcsh 
foreach gcc (gcc482 gcc492 gcc521)
  foreach opt (debug opt)
    foreach bit (32b 64b)
      if ($opt == "debug") then 
        unsetenv NODEBUG
      else      
        setenv NODEBUG yes
      endif
      setup ${gcc}
      setup ${bit}
      time cons -k >& build.${gcc}.${opt}.${bit}.`date +%Y%m%d%y:%H%M`.log &
    end
  end
end
