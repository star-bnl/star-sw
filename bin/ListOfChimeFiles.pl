#! /usr/bin/env perl
my %Clusters = ('iut2' => 'bh1.uits.indiana.edu',
		'iu'   => 'atlas.iu.edu',
		'bu'   => 'atlasgrid.bu.edu',
		'uc1'  => 'grid01.uchicago.edu',
		'jazz' => 'jglobus.lcrc.anl.gov',
		'pdsf' => 'pdsfgrid1.nersc.gov',
		'pdsfgrid3' => 'pdsfgrid3.nersc.gov',
		'bnl'  => 'spider.usatlas.bnl.gov',
		'bnl01'=> 'atlasgrid01.usatlas.bnl.gov',
		'smu'  => 'mcfarm.physics.smu.edu',
		'ou1'  => 'boomer1.oscer.ou.edu',
		'ufl'  => 'ufgrid01.phys.ufl.edu');
my %WorkDirs = ('bh1.uits.indiana.edu' => '/N/ivdgl/atlas_scratch_space/fisyak',
		'pdsfgrid1.nersc.gov'  => '/auto/atlas/scratch/fisyak',
		'atlasgrid.bu.edu'     => '/atlasgrid2/atlas_scratch/fisyak');
my $hostKey = 'iut2';
if ($#ARGV > -1) {$hostKey = $ARGV[0];}
print "run $0 for $hostKey\n";
my $host = $Clusters{$hostKey}; print "Host : $host\n";
my $disk = $WorkDirs{$host};
my $list = `globus-job-run $host /bin/ls -l $disk`;
#print "$list";
my @lines = split /^/m, $list;
foreach my $line (@lines) {
  my @words = split ' ', $line;
  my $subdir = $words[8];
  next if $subdir !~ /recon/;
  my $dir = $disk . "/" . $subdir;# print "dir = $dir \n";
  my $cmd = "globus-job-run " . $host . " /bin/ls -l " . $dir;
  print "$cmd\n";
  my $listdir = `$cmd`;
  print "$listdir";
}
exit 0;
