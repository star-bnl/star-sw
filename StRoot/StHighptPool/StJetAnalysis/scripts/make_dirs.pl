#!/usr/bin/perl
use Time::Local;
use File::Copy;

print STDOUT "executing make_runs.pl \n";

#take a snapshot of the macros on afs, copy them to nfs so that we don't have token issues
copy("/afs/rhic/star/highpt/upsilon/common/macros/TestFilter.C",
     "/star/data22/MuDST/upsilon/common/macros/TestFilter.C");
copy("/afs/rhic/star/highpt/upsilon/common/macros/RunChainMerger.C",
     "/star/data22/MuDST/upsilon/common/macros/RunChainMerger.C");

for my $centrality (qw(central minbias)) {
#for my $centrality (qw(minbias)) {
  print STDOUT "$centrality";
  mkdir "$centrality",0775 unless (-d $centrality);
  chdir $centrality;
  for my $bfield (qw(FullField HalfField ReversedFullField ReversedHalfField)) {
  #for my $bfield (qw(ReversedFullField)) {
    print STDOUT "\t $bfield\n";
    mkdir "$bfield",0775 unless (-d $bfield);
    chdir $bfield;
    #drop in links to macros, libraries
    symlink "/star/data22/MuDST/upsilon/common/macros/TestFilter.C", "TestFilter.C";
    symlink "/star/data22/MuDST/upsilon/common/macros/RunChainMerger.C", "RunChainMerger.C";
    symlink "/star/data22/MuDST/upsilon/common/.i386_redhat61/", ".i386_redhat61";
    symlink "/star/data22/MuDST/upsilon/common/.share/", ".share";
    
    mkdir "runs", 0775;
    chdir "runs";
    print `pwd`,"\n";
    
    my $dir = sprintf qq(/star/data27/MuDST/Common/%s/%s/runs),$centrality, $bfield;
    print STDOUT "Look in directory $dir \n";
    opendir DIR,$dir or die;
    my @files = grep {/.MuDst.root/} readdir DIR;
    closedir DIR;
    my $found=0;
    for my $file (@files) {
      ++$found;
      # Now drop in symbolic links from Frank's area
      #print STDOUT "Found file $file in dir \n";
      symlink "${dir}/${file}","${file}";
    }
    print STDOUT "Created $found sym-links \n";
    
    chdir "../../";
  }
  chdir "../"
}
