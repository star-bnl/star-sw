#! /usr/local/bin/perl
#
my $file = @ARGV[0];
my $line;
open (INPUT, $file);
while ($line = <INPUT>) {
  if (grep(/ChainFlags/,$line) && grep (/\[k[A-Z]/,$line) && grep (/\=/,$line)) {
    if (grep (/kTRUE/,$line))  {
      $line =~ s/ChainFlags/SetOption/g;
      $line =~ s/\=//g; $line =~ s/kTRUE//g;
    }
    else {
      if (grep (/kFALSE/,$line)) {
	$line =~ s/ChainFlags/SetOptionOff/g;
	$line =~ s/\=//g; $line =~ s/kFALSE//g;
      }
      else {print "Something wrong ", $line;}
    }
      
  }
  print $line;
}

exit(0);
# last line
