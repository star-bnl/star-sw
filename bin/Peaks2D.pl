#!/opt/star/bin/perl
my $ngroup = 3;
for (my $sec = 1; $sec <= 24; $sec++) {
  for (my $row = 1; $row <= 45; $row++) {
    #    for (my $token = 0; $token <= 110; $token++) {
    foreach my $file qw(st_laser_14158028.ClnoWB.root st_laser_14161023.ClnoWB.root) {
      my $f = $file;
      $f =~ s/\.root//;
      $f .= sprintf(".Fit.g%i.LandauIFreQ.",$ngroup)
	  . sprintf("sec%02i",$sec)
	    . sprintf(".r%i.root",$row);
#      print "$f\n";
      if (-r $f) {next;}
      print "string:$file:$sec:$row\n";
#      exit 0;
      }
#    } 
  }
}
