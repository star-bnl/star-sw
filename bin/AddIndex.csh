#! /usr/local/bin/tcsh -f
foreach d (`find . -type d`)
  echo "dir = ${d}"
  if (-r ${d}/index.html || -r ${d}/index.php) continue;
  echo "Make ${d}/index.php"
  cp /afs/rhic.bnl.gov/star/users/fisyak/WWW/star/index.php  ${d}/index.php 
end
