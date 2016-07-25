#! /usr/local/bin/perl
#
#printf ("//Transformation of  %s to %s\n",$file,$newfile);
my $tag = ` grep -l Collection.h *.*`;
my @Files = split /^/m, $tag;
for my $file (sort @Files){
  next if $file =~ /StTriggerDetectorCollection/;
  open (INPUT, $file) || die "Cannot open $file";
  chomp $file;
  my $new_file = $file . ".tmp";
  open (OUTPUT, ">$new_file") || die "Cannot open $new_file";
  while ($line = <INPUT>) {
    if ($line =~ /\#include/ && !($line =~ /StTriggerDetectorCollection/) &&
	($line =~ /Collection/ || $line =~ /StVecPtr/)) {
      $line =~ s/StTrackCollection/StGlobalTrackCollection/g;
      $line =~ s/Collection//g;
      $line =~ s/VecPtr//g;
      $line =~ s/Vec//g;
      ;}
    if (! $line =~ /typedef/) {
      $line =~ s/StVecCtbCounter/StVecPtrCtbCounter/g;
      $line =~ s/StTrackIterator/StGlobalTrackIterator/g;
      $line =~ s/StTrackCollection/StGlobalTrackCollection/g;
      $line =~ s/StVecVpdCounter/StVecPtrVpdCounter/g;
      $line =~ s/StVecZdcSegment/StVecPtrZdcSegment/g;
    }
    print OUTPUT $line;
  }
  close(INPUT);
  close(OUTPUT);
}
exit(0);
# last line
