#!/usr/bin/env tcsh
foreach d (`ls -1d Sti*`)
  cd ${d}; lsf ~/bin/Nightlies${d}.xml; cd -
end
# E O D
