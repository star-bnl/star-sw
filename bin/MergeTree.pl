#! /usr/bin/env perl
use Cwd;
use File::Basename;
use lib "/afs/rhic/star/users/fisyak/public/.dev";
use JobSubmit;
use FileList;
my $dir =  File::Basename::basename(Cwd::cwd());
my $jobs = 0;
#print "tagslist : @tagslist\n";
foreach my $tag (@tagslist) {
  print "tag = $tag\n";
  my $list = "";
  my $no_of_files = 0;
  foreach my $key ( sort keys %runs ) {
    next if $runs{$key} !~ $tag;
    my $run  = $key;
    if (BadRun($run)) {next;}
    my $file = glob $key . "*.root";
    next if !$file;
#    print "$tag => {",$key, "}= \t",$runs{$key}, "files =", $file ,"\n";
    $list .= " "; $list .= $file;
  }
  next if ! $list;
  submit("flow" . $tag,$list,"MergeTree.C",0);
}
