#! /usr/bin/env tcsh
# source $STAR/unsetupDEV2.csh

foreach trig (AuAu_200_production_2014 AuAu_200_production_low_2014 AuAu_200_production_mid_2014) 
    @ d =  75
    while ( $d < 168)  
      set day = ${d};
      if (${d} < 99) set day = "0"${d};
#      set cmd = 'get_file_list.pl -keys path,filename -cond production=P17id,trgsetupname='${trig}',filename~st_physics,filetype=daq_reco_MuDst,daynumber='${day}',storage!=hpss -limit 0 | tee '$trig'_'${day}'.list';
      set cmd = 'get_file_list.pl -keys path,filename -cond production=P16id,trgsetupname='${trig}',filename~st_physics,filetype=daq_reco_MuDst,daynumber='${day}',storage!=hpss -limit 0 | tee '$trig'_'${day}'.list';
      echo "$cmd";
#      $cmd;
      @ d++ 
    end
end
