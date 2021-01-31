@ count = 0
foreach f (`ls -1d sched*.csh`)
  set log = ${f}.log
  if (! -r ${log}) then	
    @ count++;  echo "count $count";
    if ($count > 10) then 
	break;
    endif
    csh -x ${f} >& ${log} &
  endif
end

