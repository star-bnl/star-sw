#! /usr/bin/env tcsh 
set list = "";
set domain = `hostname -d`
if (! $?ROOT_VERSION) setenv $ROOT_VERSION ""
foreach root (root5 root6)
  switch ($domain) 
    case "*local":
    case "*starp.bnl.gov":
	set list = "gcc gcc631"; # gcc531 gcc/6.3 gcc7 gcc8 gcc9 gcc/10.1.0";
    breaksw
    case "*bnl.gov":
    #    set list = "gcc482 gcc492 gcc/6.3 gcc/10";
	set list = "gcc" #  gcc/6.3  gcc/10.1.0"
    breaksw
  endsw
  set gcc_list = ${list}
  if (${root} == "root5")   set gcc_list = "${list} gcc/10"
  foreach gcc (${gcc_list})
    set gccv = `echo ${gcc} | sed -e 's/\///g'`;
    set opts = "debug opt"
#   if ($gcc == "gcc8" || gcc == "gcc631") set opts = "debug opt opt3"
    foreach opt ($opts)
      set bits = "64b";
      if ($gcc == "gcc" && $root == "root5") set bits = "32b 64b";
      foreach bit (${bits})
        unsetenv NODEBUG
        if ($opt == "opt")  setenv NODEBUG YES 
        if ($opt == "opt3") setenv NODEBUG -O3 
        setup ${gcc}
        setup ${bit}
        setup ${root}
        starver ${STAR_LEVEL}
	which rootcint
	if ($?) continue;
        set log = build.${STAR_HOST_SYS}.${opt}.`date +%m%d%y:%H%M.`${HOST}.${root}.log;
	echo "Build ${log}"
#       printenv > ${log};
       time cons -k >>& ${log} &
      end
    end
  end
end 
