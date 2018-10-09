#for 2014 HFT
#./getFiles.pl -n st_physics_adc -t online_daq -s =HPSS -g "trgsetupname=AuAu_200_production_high_2014||AuAu_200_production_mid_2014||AuAu_200_production_low_2014||AuAu_200_production_2014,runnumber[]15107005-15167014,size>0" > allhft.daq.list

#for 2016 st_physics HFT
./getFiles.pl -n st_physics_adc -t online_daq -s =HPSS -g "trgsetupname=AuAu_200_production_2016||AuAu200_production2_2016,size>0" > allhftphys.daq.list
