#!/usr/bin/perl -w
$SIG{CHLD} = "IGNORE";

my %runs = ();
open INPUT, "goodRunsEmcNoFtpcdAu2003_perday.txt";
while(<INPUT>){
   my @stuff = split;
   next unless ($stuff[0]=~m/^4/);
   $runs{$stuff[0]} = 1;
}
close INPUT;

for my $string (sort keys %runs) {
  my $runday = substr($string, 2,2);
  open OUT,">job_${runday}.xml" or die;
  print OUT <<EOF;
<?xml version="1.0" encoding="utf-8" ?>
 <job simulateSubmission="false" maxFilesPerProcess="20" filesPerHour="10">
  <command>
   stardev
   cd /star/u/russcher/StEvent2MyEvent/
   root4star -b -q /star/u/russcher/StEvent2MyEvent/doPhoton.C\\(1000000,\\"\$FILELIST\\",\\"\$SCRATCH/run_${runday}_\$JOBID\\",\\"real\\",\\"dAu\\",50,\\"/dev/null\\"\\)
  </command>
 <stdout URL="file:/star/u/russcher/StEvent2MyEvent/out/${runday}_\$JOBID.out" />
 <stderr URL="file:/star/u/russcher/StEvent2MyEvent/err/${runday}_\$JOBID.err" />
 <input URL="catalog:star.bnl.gov?production=P04if,runnumber=${string},trgsetupname=dAuCombined||UPCCombined,runtype=physics,filename~physics,filetype=daq_reco_MuDst,available=1,emc=1,tpc=1,storage!=HPSS" nFiles="all" preferStorage="local" />
 <output  fromScratch="*.root" toURL="file:/star/data01/pwg/highpt/russcher/dAu/UPCCombined/" />
</job>
EOF

close OUT;

system "star-submit job_${runday}.xml";


}
