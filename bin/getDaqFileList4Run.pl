#! /usr/bin/env perl
use File::Basename;
#High lumi run list, average zdc rate=330kHz, 20 runs
my @runs = qw(
	       13087009
	       13094081
	       13096060
	       13097021
	       13098062
	       13099026
	       13099056
	       13099058
	       13077081
	       13078001
	       13078002
	       13078003
	       13078004
	       13078006
	       13078007
	       13078009
	       13078011
	       13078012
	       13078014
	       13078057
	       13078058
	       13078060
	       13078063
	       13078070
	       13080015
	       13091012
	       13091011
	       13091009
	    );
foreach my $run (@runs) {
  my $cmd = "get_file_list.pl -keys 'path,filename' -cond runnumber=" . $run . ",filetype=online_daq,storage=hpss,filename~st_W_13 -limit 0 -delim \"/\"";
#  print "$cmd\n";
  my @files = `$cmd`;
#  print "@files\n";
  foreach my $f (@files) {
    print "$f";
  }
}
my $low = join('|', @low);
