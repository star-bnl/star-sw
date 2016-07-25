#get_file_list.pl -keys 'path,filename,events' -cond 'filetype=online_daq,runnumber[]12032056-12180055,trgsetupname=CosmicLocalClock,storage=HPSS' -limit 0
# get_file_list.pl -delim "#" -all -keys 'magscale,path,basename,events' -cond 'filetype=online_daq,trgsetupname=CosmicLocalClock' -limit 0 | tee daq_full.list
#get_no_events.csh st_tofcosmic_12179074_raw_1010001.daq 
get_file_list.pl -keys 'events' -cond 'filename='$1 -limit 1
