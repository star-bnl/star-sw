#! /usr/bin/env tcsh 
foreach gcc (gcc gcc492)
  foreach opt (debug opt)
    foreach bit (32b 64b)
      if ($opt == "debug") then 
        unsetenv NODEBUG
      else      
        setenv NODEBUG yes
      endif
      setup ${gcc}
      setup ${bit}
      time cons -k >& build.${gcc}.${opt}.${bit}.log &
    end
  end
end
