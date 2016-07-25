#! /usr/bin/env perl
use File::Basename;
use Cwd;
my $pwd = cwd();
#____________________________________________________________
sub PrintHash($$) {
  my $env = shift; # print "Call PrintHash\n";
  my $prefix = shift;
  foreach my $key ( sort keys %$env ) {
    if (ref($env->{$key}) eq "HASH") {
      PrintHash($env->{$key}, $prefix . $key . "}->{");
    }
    else {
      if ($env->{$key}) {print $prefix, $key, "}= \t$env->{$key}\n";}
    }
  }
}
#________________________________________________________________________________
my @Y2010Runs = ('AuAu200FF' => {'first' => 11001005, 'last' => 11035026},
		 'AuAu200RF' => {'first' => 11035061, 'last' => 11077018},
		 'AuAu62RF'  => {'first' => 11078018, 'last' => 11098056},
		 'AuAu39RF'  => {'first' => 11099002, 'last' => 11112067},
		 'AuAu7RF'   => {'first' => 11114027, 'last' => 11147025},
		 'AuAu11RF'  => {'first' => 11148001, 'last' => 11159020}
);
my $def = {@Y2010Runs};
#PrintHash($def,"\$def->{");
my @Files = glob "st_physics* trackMateFile*";
foreach my $file (@Files) {
  my $f = $file;
  $f =~ s/trackMateFile//;
  $f =~ s/st_physics_//;
  $f =~ s/adc_//;
  my ($run,$dum1,$dum2) = split '_',$f;
  print "$file => $f => $run\n";
  foreach my $key (sort keys %$def) {
#    print "$key => $def->{$key}->{first} and $def->{$key}->{last}\n";
    if ($run >= $def->{$key}->{first} and $run <= $def->{$key}->{last}) {
      print "Move $file to $key\n";
      if (! -d $key) {mkdir $key};
      chdir($key);
      unlink($file);
      symlink("../" . $file, $file) or die "Can't create symlink to $file";
      chdir("../");
      goto Last;
    }
  }
  print "File $file does not match to any run periods\n";
 Last:
}
#{
#  foreach my $key( sort keys %$def) {
#    if ( defined( $ARG{$key} ) ) {
#      print "Reset $key = $def->{$key} to $ARG{$key}\n" unless ($param::quiet);
#      my @defs = ($key => $ARG{$key});
#      push ( @param::defaults, @defs );
#    }
#  }
#}
