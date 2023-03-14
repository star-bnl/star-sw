#! /bin/tcsh -f
set D = `basename ${PWD}`
set dist = ~/work/Histograms/${D}
if (! -d ${dist}) mkdir ${dist}
#foreach d (9p2GeVc_2020 9p8GeV_fixedTarget_2020 AuAu200GeV_2019 dAu200GeV_2021 OO_200GeV_2021 pp500GeV_2022 ps_OO_200GeV_2021)
foreach d (`ls -1d *GeV*`)
  if (-r ${dist}/${d}.root) continue;
  cd ${d}
  ls -1d All*_2.root
  if (! $?) then
    hadd ${dist}/${d}.root All*.root >& hadd.log &
  else
    cp -p  All*_1.root ${dist}/${d}.root
  endif
  cd -;
end
#  100GeV_fixedTarget_2021     100GeV_fixedTarget_2021.root   
#  11p5GeV_2020		    11p5GeV_2020.root	       
#  13p5GeV_fixedTarget_2020    13p5GeV_fixedTarget_2020.root  
#  14p5GeV_2019		    14p5GeV_2019.root	       
#  17p3GeV_2021		    17p3GeV_2021.root	       
#  19GeV_2019		    19GeV_2019.root		       
#  19p5GeV_fixedTarget_2020    19p5GeV_fixedTarget_2020.root  
#  26p5GeV_fixedTarget_2020    26p5GeV_fixedTarget_2020.root  
#  26p5GeV_fixedTarget_2021    26p5GeV_fixedTarget_2021.root  
#  31GeV_fixedTarget_2019	    31GeV_fixedTarget_2019.root    
#  31p2GeV_fixedTarget_2020    31p2GeV_fixedTarget_2020.root  
#  3p85GeV_fixedTarget_2019    3p85GeV_fixedTarget_2019.root  
#  3p85GeV_fixedTarget_2021    3p85GeV_fixedTarget_2021.root  
#  44p5GeV_fixedTarget_2021    44p5GeV_fixedTarget_2021.root  
#  4p59GeV_fixedTarget_2019    4p59GeV_fixedTarget_2019.root  
#  5p75GeV_fixedTarget_2020    5p75GeV_fixedTarget_2020.root  
#  70GeV_fixedTarget_2021	    70GeV_fixedTarget_2021.root    
#  7.3GeV_fixedTarget_2019	    7.3GeV_fixedTarget_2019.root   
#  7p3GeV_fixedTarget_2020	    7p3GeV_fixedTarget_2020.root   
#  7p7GeV_2019		    7p7GeV_2019.root	       
#  7p7GeV_2020		    7p7GeV_2020.root	       
#  7p7GeV_2021		    7p7GeV_2021.root	       
#  9p2GeV_2019		    9p2GeV_2019.root	       
#  9p2GeV_2020		    9p2GeV_2020.root	       
#  9p2GeVb_2020		    9p2GeVb_2020.root	       
#  9p2GeVc_2020		    9p2GeVc_2020.root	       
#  9p8GeV_fixedTarget_2020	    9p8GeV_fixedTarget_2020.root   
#  AuAu200GeV_2019		    dAu200GeV_2021.root	       
#  dAu200GeV_2021		         
#  OO_200GeV_2021		    OO_200GeV_2021.root	       
#  pp500GeV_2022		    pp500GeV_2022.root	           
#  ps_OO_200GeV_2021	    ps_OO_200GeV_2021.root
#  
#  #  Sum
# foreach y (2019 2020 2021)
#   hadd FXT_${y}.root [0-9]*fixedTarget_${y}.root >&  FXT_${y}.log &
#   hadd COL_${y}.root [0-9]*GeV_${y}.root >&  COL_${y}.log &
# end 
#
#



























