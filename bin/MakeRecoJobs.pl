#!/usr/bin/env perl
use File::Basename;
use Cwd;
use Env;
my $DIR = Cwd::cwd();
print "ARGV $#ARGV\n"; 
if ($#ARGV < 1) {
  print "Usage: $0 files='glob' chain='chain' Nevents='Nevents' macro='macro' limit='10'\n";
  print "for example\n$0 files='/star/data03/daq/2005/1??/st_physic*.daq' chain='tpt'\n";
  exit 0;
}
my %ARG = (macro => 'bfc.C',
	   exec  => 'root.exe',
	   files => '',
	   Nevents => '9999',
	   chain => 'DbV20050816 pp2005 -svt_daq -SvtHit -SvtVtx -SvtClu -SvtSeqAdj -SvtCluAnal -svtD beamLine Xi2 Kink2 CMuDst OShortR OSpaceZ2 OGridLeak3D',
	   version => '.DEV2',
	   limit => '9999'
	  );
while (@ARGV) {
  $_ = shift @ARGV;
  if ($_ =~ /=/) { my($key, $val) = /([^=]*)=(.*)/; $ARG{$key} = $val;}
}
if ($ARG{macro} eq 'prepass.C') {
  $ARG{Nevents} = '';
  $ARG{chain} = '';
}
while (my ($key,$value) = each %ARG) {
  print  "$key=$value\n";
}
my $glob = $ARG{files}; print "glob = $glob\n";
my $exec = $ARG{exec}; print "exec = $exec\n";
my $chain = $ARG{chain}; print "chain = $chain\n";
my $macro = $ARG{macro}; print "macro = $macro\n";
my $nevents = $ARG{Nevents}; print "nevents = $nevents\n";
my $limit =  $ARG{limit}; print "limit = $limit\n";
my @Fiels = ();
if (-r $glob) {
  open (In,"$glob") or "Can't open $glob";
  while (my $line = <In>) {
    next if $line =~ /^ /;
    push @Files, $line;
  }
} else {
 @Files = glob $glob; 
}
print "No. of Files = $#Files\n";
if ($chain eq 'tpt') {
  $chain = "DbV20050816 pp2005 -svt_daq -SvtHit -SvtVtx -SvtClu -SvtSeqAdj -SvtCluAnal -svtD beamLine Xi2 Kink2 CMuDst OShortR OSpaceZ2 OGridLeak3D";
} elsif ($chain eq 'ittf') {
  $chain = "DbV20050816 pp2005a ITTF OSpaceZ2 OGridLeak3D";
} elsif ($chain eq 'TpcOnly') {
  $chain = "P2005,tofDat,logger,ITTF,ssddat,spt,-SvtIt,-SsdIt,clearmem,pmdRaw,OShortR,OSpaceZ2,-dstout";
} elsif ($chain eq 'TpcSvtSsd') {
  $chain = "P2005,tofDat,logger,ITTF,ssddat,spt,SvtIt,SsdIt,clearmem,pmdRaw,OShortR,OSpaceZ2,-dstout";
} elsif ($chain eq 'dEdxRunVII') {
  $chain = "T2007,IAna,VFMinuit,Idst,-svtIT,-ssdIT,ITTF,Corr5,EvOutOnly";
#  $chain = "T2007,dEdxY2,analysis,Corr5,VFMinuit,ITTF,-SsdIt,-SvtIt,OSpaceZ2,-dstout,EvOutOnly";
} elsif ($chain eq 'SpcChgCalG' or chain eq 'EbyE') {
  $chain = "SpcChgCalG B2007 ITTF VFMinuit -SvtIT -SsdIT";
} elsif ($chain eq 'dEdx') {
  $chain = "pgf77,mysql,T2007,IAna,VFMinuit,Idst,-svtIT,-ssdIT,ITTF,Corr5,EvOutOnly,-dstout,-dEdxY2";
#  $chain = "P2007,MakeEvent,ITTF,-SsdIt,-SvtIt,-dstout,-hitfilt,EvOutOnly,-dEdxY2";
}

my $XML = "jobs.xml";
open (XML,">$XML") or die "Can't open $XML";
#
print XML '<?xml version="1.0" encoding="utf-8" ?> 
<job name="recoJob" maxFilesPerProcess="1" filesPerHour="0.1" simulateSubmission="false" fileListSyntax="paths">
     <command>csh -x $INPUTFILE0 </command>
     <stdout URL="file:' . $DIR . '/shed$JOBID.log" />
';

my $job = 0;
foreach my $file (@Files) {
  my ($dev, $ino, $mode, $nlink, $uid, $gid, $rdev, $size, $atime, $mtime, $ctime, $blksize, $blocks ) = stat $file;
#  if ($size < 100000000) {print "$file is too small : $size skip it\n"; next;}
  my ($name, $path, $suffix) = fileparse($file,'\.(fz.*|daq|nt)'); print "name = $name, path = $path, suffix = $suffix\n";
  $name =~ s/\./_/g;
  my $F = $file;
  if ($nevents =~ /,/) {
    $name .= "_" . $nevents;
    $name =~ s/,/_/;
  }
  if ($F =~ /\.nt$/) {
    my $Z = $name . ".fz";
    $F .= "; gfile o $Z";
  }
  $SCRIPT = $name . ".csh";
  next if -r $SCRIPT;
  open (OUT,">$SCRIPT") or die "Can't open $SCRIPT";
  print "Create $SCRIPT\n";
  print XML "<input URL=\"file:" . $DIR . "/" .  $SCRIPT ."\" />\n";
  open (OUT,">$SCRIPT") or die "Can't open $SCRIPT";

#  print OUT "#! /usr/local/bin/tcsh -f\n";
  print OUT "#!/bin/tcsh -v\n";
  print OUT "cd $DIR\n";
  print OUT "starver " . $ARG{version} . "\n";
  my $log = $name . "B.log"; 
  my $cmd = "test ! -r " . $log . " && ";
#  $cmd .= "$exec -q -b Load.C \'" . $macro;
  $cmd .= "$exec -q -b \'" . $macro;
#  $cmd .= "root4star -q -b \'" . $macro;
  $name .= ".event.root";
  if ($macro eq "bfc.C") {
#    $cmd .= "+";
    if ($chain =~ /LanaDV/) {
      $cmd .= "(" . $nevents . ',"' . $chain . '","' . $F . '")' . "'";
    } else {
      $cmd .= "(" . $nevents . ',"' . $chain . '","' . $F . '","' . $name .'")' . "'";
    }
  } else {
    $cmd .= "(";
    if ($nevents) {$cmd .= $nevents . ",";}
    if ($chain)   {$cmd .= '"' . $chain . '",'}
#    if ($macro eq 'TpcRS.C') {
#      $cmd .= '"' . $F . '","")' . "'";
#    } else {
      $cmd .= '"' . $F . '")' . "'";
#    }
  }
  $cmd .= ' >& ' . $log;
#  print "$cmd\n";
  print OUT "$cmd\n";
  close (OUT);
  $job++;
  last if ($job >= $limit);
}
print XML '
</job>
';
close (XML);
