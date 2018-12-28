@ count = 0
foreach f (`ls -1d sched*.csh`)
  if (! -r  ${f}.log) then
    csh -x ${f} >& ${f}.log &
    @ count++;  echo "count $count";
    if ($count > 20) break;
  endif
end
============
foreach d (`ls -1d *AuAu*`)
   echo ${d}; ls -1d ${d}/*/*/*/*.root | wc -l
end

foreach d (`ls -1d 1??`)
   echo "run ${d}"; ls -1d ${d}/*/*.root | wc -l
end
