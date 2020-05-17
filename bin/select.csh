#! /usr//bin/env tcsh
foreach f (`ls -1d *2020*.list`)
  awk 'BEGIN{n=0;}{n++; if(n%50==1) print $0}' ${f} > ${f}.selected
end
