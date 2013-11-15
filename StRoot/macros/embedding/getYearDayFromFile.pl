#!/usr/local/bin/perl -w

use Getopt::Long ;

my $day = 0;
my $help = 0;
my $runnum = 0;
my $year = 0;

GetOptions (
    'day' => \$day,     # Return day
    'help' => \$help ,  # Show help messages
    'run' => \$runnum,  # Return runnumber
    'year' => \$year    # Return year (default)
);

my $usage = q(
    Usage: getYearDayFromFile.pl [-d] [-y] [-h] [input file name]

    -d or --day     Return day extracted from filename
    -h or --help    Show this messages and exit
    -r or --run     Return the runnumber from filename
    -y or --year    Return year extracted from filename (default)


);

#----------------------------------------------------------------------------------------------------
# Print help
#----------------------------------------------------------------------------------------------------
if( $help )
{
  print($usage);
  exit(0);
}

#----------------------------------------------------------------------------------------------------
# Exit here if no input
#----------------------------------------------------------------------------------------------------
if ( $#ARGV + 1 == 0 ){
  print("No input argument. See usage below.\n\n");
  print($usage);
  exit(0);
}

$fname = shift @ARGV ;
#$fname =~ /_(\d)(\d\d\d)\d\d\d_raw/;

#----------------------------------------------------------------------------------------------------
# Get run number
#----------------------------------------------------------------------------------------------------
$fname =~ /_(\d.*)_raw/;
$run    = $1; # Run number
$length = length($run); # Length of run number
#print "$run, n=$length\n";

$output = 0 ;
if( $length == 8 ) { # run >= 10
  $fname =~ /_(\d\d)(\d\d\d)\d\d\d_raw/;

  if( $year ){
    $output = $1 ;
  }
  else{
    $output = $2 ;
  }

#  print "Run>=10: $year\n";
}
else{ # run < 10
  $fname =~ /_(\d)(\d\d\d)\d\d\d_raw/;

  if( $year ){
    $output = $1 ;
  }
  else{
    $output = $2 ;
  }

#  print "Run<10: $year\n";
}

if( $year ){
  $output += 1999 ; # -1 + 2000
}

if( $runnum ) {
print "$run"
}
else {
print "$output";
}

