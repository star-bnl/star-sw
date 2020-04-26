#! /usr/local/bin/tcsh -f
#get_file_list.pl -delim "/" -keys 'path,basename,events,size' -cond 'filename~st_physics_adc,filetype=online_daq,trgsetupname=production_'$1'_2020'  -limit 0 
foreach trig (11p5GeV_2020 13p5GeV_fixedTarget_2020 19p5GeV_fixedTarget_2020 31p2GeV_fixedTarget_2020 5p75GeV_fixedTarget_2020 7p3GeV_fixedTarget_2020 9p2GeV_2020 9p2GeV_2020b 9p8GeV_fixedTarget_2020) 
  get_file_list.pl -delim '/' -keys 'path,basename' -cond 'filename~st_physics_adc,filetype=online_daq,trgsetupname=production_'${trig}',tpx=1'  -limit 0  | tee  ${trig}.list
end



