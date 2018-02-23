#!/bin/csh
# demoloop.csh - Sample loop script
set j = ${1}
while ( ${j} <= ${2} )
  2016AuAu200MuDst.pl ${j} | tee ${j}.listO
  @ j++
end
