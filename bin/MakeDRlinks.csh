#! /bin/tcsh -f
foreach f (`ls -1d /gpfs01/star/data100/GRID/CalibrationOutput/production_19GeV_2019_DEV/picoDst/*.picoDst.root` )
  set d = `echo ${f} | awk -F_ '{print $6}'`;
  mkdir ${d}
  cd ${d}
  ln -s ${f} .
  cd -
end
# EoD
 foreach d (`ls -1d 2* | cut -c3-5 | sort -u`)
   mkdir ${d}; mv 20${d}* ${d}
 end
