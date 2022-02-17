foreach f (`ls -1d *.daq`)
  set d = `echo ${f} | awk -F_ '{printf("%03i/%i\n",($4/1000)%100,$4)}'`
  echo "${f} => ${d}"
  if (! -d ${d}) mkdir -p ../${d}
  echo "mv ${f} ../${d}/"
  mv ${f} ../${d}/
end
