foreach r ( `ls -1d *.event.root | awk -F_ '{print $2}' | sort -u` )
  echo $r
  ls -1d /hlt/cephfs/daq/2019/???/${r}
  if (! $?) continue
  echo "mv ${r} to Zombie"
  mv *${r}* Zombie
end
