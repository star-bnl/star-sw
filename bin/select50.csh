#! /usr//bin/env tcsh
foreach f (`ls -1d *2020*.list`)
  awk 'BEGIN{n=0;}{n++; if(n%50==1) print $0".daq /gpfs01/star"$0".daq"}' ${f} |  awk 'sub("/gpfs01/star/home/starsink/raw","/gpfs01/star")' > ${f}.selected
end
