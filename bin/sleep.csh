#! /usr/local/bin/tcsh -f
set h = `hostname | cut -c6-7`;
#if ("$h" == "46") then
while (1)
  sleep 20m
  ls -l 
end
#else 
#  exit 0
#endif
# last line

