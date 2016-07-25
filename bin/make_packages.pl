#!/usr/bin/env perl
use File::Basename;
my @sys_ignore = qw ( 
		     alpha_dux40   alpha_osf20   alpha_osf32    
		     hp700_ux101   hp700_ux90   alpha-dec-osf2.0
		     pmax_ul43   
		     rs_aix32   rs_aix41   rs_aix43   
		     sgi_52   sgi_64   mips-sgi-irix5.2
		     sun4c_411   sun4m_53   sun4m_54   sun4x_55 
		    );
my $TOPDIR = "/afs/cern.ch/asis/packages";
my @list_dir = ();
my @dirs  = glob $TOPDIR . "/*/*/*";
foreach my $dir (@dirs) {
# skip unsupported plaforms
  next if !-d $dir;
  my @words = split '/', $dir;
  my $i = $#words;
  my $sys = $words[$i];
  my $vers = $words[$i-1];
  (my $pk   = $vers) =~ s/-.*//;
#  print "dir = $dir $i sys =  $sys  vers = $vers pk = $pk\n";
  foreach my $s (@sys_ignore) {
    if ($s eq $sys) {
#      print "skip $dir\n";
      goto DO;
    }
  }
  goto SKIP;
#  my $n = $#list_dir; #print "n = $n \n";
#  for (my $j = $n; $j>=0; $j--) {
#    my $f = $list_dir[$j]; #print "j = $j f = $f\n";
#    my @words1 = split '/', $f;
#    my $i1 = $#words1;
#    my $sys1 = $words1[$i1];
#    my $vers1 = $words1[$i1-1];
#    (my $pk1   = $vers1) =~ s/-.*//;
#    if ($pk1 eq $pk and $sys eq $sys1) {
#      print "replace $f by $dir\n";
#      $list_dir[$j] = $dir;
#      goto SKIP;
#    }
#  }
 DO:
  my $ldir = $dir; 
  $ldir =~ s|cern\.ch/asis|rhic/\.asis|g;
  push @list_dir, $ldir;
 SKIP:
}
foreach my $d (@list_dir) {
  print "$d\n";
}
