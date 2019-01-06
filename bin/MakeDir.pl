#! /usr/bin/env perl
use strict;
my @ver_list = qw(SL18h TFG19a);
my @gcc_list = ();
my @bit_list = ();
my @opt_list = ();
foreach my $ver (@ver_list) { 
  if ($ver eq "SL18h") {@gcc_list = qw(gcc485);}
  else                 {@gcc_list = qw(gcc485 gcc531 gcc631 gcc7);}
  print "ver = $ver for @ver_list make @gcc_list\n";
  foreach my $gcc (@gcc_list) {
    print "\tgcc = $gcc for @gcc_list\n";
    if ($ver == "TFG19a" && ($gcc != "gcc485" && $gcc != "gcc531")) {@bit_list = qw(64b);}
    else                                                            {@bit_list = qw(32b 64b);}
    foreach my $bit (@bit_list) {
      print "\t \tbit = $bit for @bit_list\n";
      if ($bit == "64b" && $ver == "SL19h") {@opt_list = qw(debug);}
      else                                  {@opt_list = qw(debug opt);}
      foreach my $opt (@opt_list) { 
	print "\t \t \topt = $opt for @opt_list\n";
	my $dir = $ver . "_" . $gcc;
	if ($bit == "64b") {$dir .= "_x8664";}
	if ($opt ne "debug") {$dir .= "_opt";}
	print "\t\t\t\t$dir \n";
	`mkdir -p $dir`;
      }
    }
  }
}

