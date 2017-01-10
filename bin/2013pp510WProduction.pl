#!/usr/bin/env perl
my @www = qw(
============================================= Summurizing =========================
# find 0?? -name "*B.log" -cmin +10 | tee Terminated.log
 egrep -l '(This is the end of ROOT -- Goodbye|StCloseFileOnTerminate::Notify : Terminating)' 1*/*/*B.log  | tee Terminated.log
 sed -e 's/B\.log/\.MuDst\.root/' Terminated.log | tee Terminated.MuDst.list
 root.exe -q -b 'Recover.C("@Terminated.MuDst.list")' | tee Recover.Terminated.MuDst.list

 grep Zombie Recover.Terminated.MuDst.list 
 foreach f (`grep Zombie Recover.Terminated.MuDst.list | awk '{print $3}'`)
   set b = `basename ${f} B.log`; set d = `dirname ${f}`; mv ${d}/${b}* Zombie/
 end
 grep Zombie Recover.Terminated.MuDst.list 
 foreach f (`grep Zombie Recover.Terminated.MuDst.list | awk '{print $3}'`)
   set b = `basename ${f} B.log`; set d = `dirname ${f}`; mv ${d}/${b}* Zombie/
 end
 foreach b ( `grep is\ Zombie *Chain | sed -e 's/:/ /' | awk '{print $2}' | sed -e 's/.MuDst.root//'` )
   mv ${b}* Zombie
 end
 foreach b (`grep has\ no\ key *Chain | awk '{print $5}' | sed -e 's/.MuDst.root//'`)
   mv ${b}* Zombie
 end
cat Terminated.log | xargs 2013pp510WProduction.pl | tee 2013pp510WProduction.log
renameF.pl
===================== Clean Up ===========================================================
foreach q (`ls -1d ???`) 
  echo ${q}
  ls -1d ${q}*Done*
  if ($?) then
    foreach d (`ls -1d ${q}/14*`)
      cd $d; pwd; CheckMuDstRepeation.pl; cd -;
    end
  endif
end
================================================================================
cd ~/work/daq/2013W
2013pp510W.pl | tee LostAndDone.list
grep Done LostAndDone.list | awk '{print $2}' | xargs rm
================================================================================
cd /gpfs01/star/scratch/fisyak/daq/2013
find . -depth -type d -empty -print
find . -depth -type d -empty -print -exec rmdir {} \;
============================== Count events ==================================================
#  ls -1d ${f}*.Done
#  if ($?) root.exe -q -b 'Chain.C+("MuDst","'${f}'/*/*.MuDst.root")' >& ${f}.Chain &
#  if (! -r ${f}.Chain) 
#foreach d (`ls -1d /gpfs01/star/daq/2013/???`)
foreach f (`ls -1d ???`) 
  echo ${f}
  ls -1d ${f}*Done*
  if ($?) then
    echo ${f}; 
    if (-r ${f}.Chain) rm ${f}.Chain
    if (-r ${f}) root.exe -q -b 'Chain.C+("MuDst","'${f}'/*/*.MuDst.root")' >& ${f}.Chain &
  endif
end

================================================================================
foreach f ( `grep -l total *Chain` )
  echo ${f}; set n = `grep total ${f} | awk '{print $6}'`; mv ${f} ${f}.${n}.events
end

foreach f ( `ls -1d *events`)
  set d = `echo $f | awk -F\. '{print $1}'`;
  if (! -r /gpfs01/star/daq/2013/${d}) mv ${f} ${f}.Done
end

ls -1d *Chain* | awk -F\. 'BEGIN{n=0}{n+=$3}END{print n/1e6}'
============================= SubmitJobs.csh  ===================================================
touch daq_2013pp510W.log
foreach q (`ls -1d /gpfs01/star/daq/2013/???/14* `)
  set r = `basename $q`
  set dd = `dirname $q`
  set day = `basename $dd`
  set d = $day/$r
  if (! -d $d) mkdir -p $d
  cd $d; echo $d; 
  daq_2013pp510W.pl 1 >>& daq_2013pp510W.log
  cd -
end
foreach q (`ls -1d /gpfs01/star/daq/2013/???/14* `)
  set r = `basename $q`
  set dd = `dirname $q`
  set day = `basename $dd`
  set d = $day/$r
  if (! -d $d) mkdir -p $d
  cd $d; echo $d; 
  daq_2013pp510W.pl 1 ?
  if ($? && $? != 255) then
    lsf ~/xml/daq_2013pp510W.xml; 
  endif
  cd -
end
================================= Count
ln -s ../.sl64_gcc492 .
foreach f ( `ls -1d * | grep -v Chain` )
  root.exe -q -b 'Chain.C+("PicoDst","'${f}'/*.picoDst.root")' >& ${f}.Chain &
end
foreach f (`ls -1d 1* | egrep -v '(\.)'`)
  root.exe -q -b 'Chain.C+("MuDst","'${f}'/*/*.MuDst.root")' >& ${f}.Chain &
end
  
==================== to HPSS ============================================================
cd 079 
dir */*B.log */sched* */*xml */*bla.root */paw*
foreach f (`ls -1d  */*.log`) 
  gzip ${f}
end
2013W2hpss.pl | hsi >& 2013W2hpssB.log &

foreach d (`ls -1d */*/*.log | awk -F\/ '{print $1}' | sort -u`)
  cd ${d}
  foreach f (`ls -1d  */*.log`) 
    gzip ${f}
  end
  2013W2hpss.pl | hsi >& 2013W2hpssB.log &
  cd -;
end



foreach f (`ls -1d *Done`)
  set d = `echo ${f} | awk -F. '{print $1}'`; echo ${d}
  test -r ${d}/2013W2hpss.log && grep 'ending HSI session' ${d}/2013W2hpss.log
  if (! $?) mv ${f} ${f}.onHPSS
end

================================================================================

set f = 15131008; root.exe -q -b 'Chain.C+("MuDst","'${f}'/*.MuDst.root")' >& ${f}.Chain &
set f = 076; root.exe -q -b 'Chain.C+("MuDst","'${f}'/*/*.MuDst.root")' >& ${f}.Chain &

================================================================================
foreach d ( `ls -1d 0??/* | grep -v Chain` )
  cd $d; PicoDST.pl; if ($? == 0)  /afs/rhic.bnl.gov/star/packages/.DEV2/scripts/star-submit ~/xml/PicoDST.xml; cd -;
end
================================================================================
foreach d (`ls -1d 1*/* | grep -v Chain`)
  cd ${d};
  foreach f (` grep -l 'StCloseFileOnTerminate::Notify' *.log`)
    set b = `basename ${f} .log`; mv ${b}.log ${b}.log_Terminated; mv ${b}.picoDst.root ${b}.picoDst.root_Terminated
er  end
  cd -;
end
================================================================================
Resubmit: clean */sched* */*xml */*bla.root */paw*
foreach d (`ls -1d 1*`)
 cd $d; echo $d; daq_2013pp510W.pl ; cd -
end
dir */*bla.root | awk '{print $9}' | awk -F\/ '{print $1}' | sort -u | xargs
rm */*bla.root
foreach d (`ls -1d ???/14*`)
  cd $d; echo $d; 
  daq_2013pp510W.pl 1
  if ($?) then
    lsf ~/xml/daq_2013pp510W.xml; 
  endif
  cd -
end
================================================================================
foreach d (`ls -1d /gpfs01/star/scratch/fisyak/daq/2013/*/* | sed -e 's/\/gpfs01\/star\/scratch\/fisyak\/daq\/2013\///'`)
   if (! -r $d) mkdir -p $d
end

=================== Resubmit hold =============================================================
rm hold
touch hold
foreach f ( `condq | grep -w H | awk '{print $9}'` )
ls -1d */*/${f} >> hold
end
foreach dir (` awk -F\/ '{print $1"/"$2}' hold | sort -u`)
  set l = ` grep ${dir} hold | awk -F_ '{print $2}' | awk -F\. '{print $1}' | xargs | sed -e 's/ /,/g'`
  cd ${dir}; 
  set xml = `ls -1dtr *.xml | tail -1`
  lsf -r ${l} ${xml}
  cd -
end
condq | grep -w H | awk '{print $1}' | xargs condor_rm

================================================================================
cd /gpfs01/star/scratch/fisyak/daq/2013
foreach d ( `ls -1d ???` )
set n = `ls -1d ${d}/*/*.daq | wc -l`; echo "${d} => ${n}"
end
================================================================================
);
if ($#ARGV < 0) {
  print "Usage $0 list_of_log_files\n";
  exit 0;
}
foreach my $file (@ARGV) {
  if (! -r $file) {next;}
  open(IN,"$file") or die "Can't open $file";
  my $N = 0;
  my $Ast = 0;
  my $Cpu = 0;
  my $Ast2 = 0;
  my $Cpu2 = 0;
  my @ww = split ("_",$file);
  my $firstF = $ww[$#ww-1];
  my $lastF  = $ww[$#ww]; $lastF =~ s/B\.log//;
  #print "firstF = $firstF, lastF = $lastF\n";
  my $Total = -1;
  #Total events processed :164 and not completed: 0
  my $firstEv = 999999999;
  my $EoF = 0;
  my $lastEv  = -1;
  my $line;
  while ($line = <IN>) {
    if ($line =~ /StEOF/) {$EoF = 1;}
    if ($line =~ /Total events processed :/ && $EoF) {
#      print $line;
      my ($dum,$dum,$dum,$string,$dum) = split(":",$line);
#      print "$string\n";
      ($Total) = split(" ",$string);
#     print "Total = $Total\n";
    }
    last if $line =~ /Disk quota exceeded/;
    next if $line !~ /Done with Event/;
#    print "$line";
    $line =~ s/.*\[no\.//;
    $line =~ s/\/run.*Real Time =//;
    $line =~ s/seconds Cpu Time = //; 
    $line =~ s/ seconds//;# print "$line\n";
    my @w = split ' ', $line;
    my $evt = $w[0];# print "event = $evt\n"; 
    my $ast = $w[1];# print "ast = $ast\n";
    my $cpu = $w[2];# print "cpu = $cpu\n";
    if ($evt < $firstEv) {$firstEv = $evt;}
    if ($evt > $lastEv)  {$lastEv  = $evt;}
    $N++;
    $Ast += $ast;
    $Cpu += $cpu;
    $Ast2 += $ast*$ast;
    $Cpu2 += $cpu*$cpu;
#    last;
  }
  close(IN);
#  print "N = $N, Total = $Total\n";
  if ($N > 0) {
    $Ast = $Ast/$N;
    $Cpu = $Cpu/$N;
    $Ast2 = $Ast2/$N;
    $Cpu2 = $Cpu2/$N;
    $Ast2 = sqrt ($Ast2 - $Ast*$Ast);
    $Cpu2 = sqrt ($Cpu2 - $Cpu*$Cpu);
    print "$file => F = $firstEv; L = $lastEv; N = $N; Ast = $Ast +/- $Ast2; Cpu = $Cpu +/- $Cpu2\n";
  } else {
    if ($Total == 0) {
      print "$file => F = $firstF; L = $lastF;\n";
    }
  }
}

