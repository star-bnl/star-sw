#!/usr/bin/perl
use Time::Local;
use File::Copy;

print STDOUT "executing make_runs.pl \n";

#take a snapshot of the macros on afs, copy them to nfs so that we don't have token issues
copy("/afs/rhic.bnl.gov/star/users/mmiller/code/highpt/StRoot/JetFinder/macros/TestFilter.C",
     "/star/data22/MuDST/upsilon/jets/macros/TestFilter.C");
copy("/afs/rhic.bnl.gov/star/users/mmiller/code/highpt/StRoot/JetFinder/macros/RunChainMerger.C",
     "/star/data22/MuDST/upsilon/jets/macros/RunChainMerger.C");

#for my $centrality (qw(central minbias)) {
#for my $centrality (qw(minbias)) {

my %cens = (
	    #central=>[qw(productionCentral productionCentral1200 productionCentral600 Central central)],
	    #minbias=>[qw(ProductionMinBias MinBias MinBiasVertex MinBiasVtx)],
	    ppMinBias=>[qw(ppMinBias)],
	   );
for my $centrality (sort keys %cens) {
  
  print STDOUT "$centrality";
  mkdir "$centrality",0775 unless (-d $centrality);
  chdir $centrality;
  
  for my $bfield (qw(FullField HalfField ReversedFullField ReversedHalfField)) {
  #for my $bfield (qw(ReversedFullField)) {
    
    print STDOUT "\t $bfield\n";
    mkdir "$bfield",0775 unless (-d $bfield);
    chdir $bfield;
    #drop in links to macros, libraries
    symlink "/star/data22/MuDST/upsilon/jets/macros/TestFilter.C", "TestFilter.C";
    symlink "/star/data22/MuDST/upsilon/jets/macros/RunChainMerger.C", "RunChainMerger.C";
    symlink "/star/data22/MuDST/upsilon/jets/.i386_linux24/", ".i386_linux24";
    symlink "/star/data22/MuDST/upsilon/jets/.share/", ".share";
    
    mkdir "runs", 0775;
    chdir "runs";
    print `pwd`,"\n";
    
    my $dir = "";
    for my $trig (@{$cens{$centrality}}) {
      for my $i (1..28) {
	
	#Change these productions in the future
	for my $production (qw(P02ge)) { 
	  my $dir = sprintf qq(/star/data%02d/reco/%s/%s/%s/2001),$i,$trig,$bfield,$production;
	  
	  print STDOUT "Look in directory $dir \n";
	  
	  opendir DIR,"$dir" or next;
	  my @files = grep {/\d+/} readdir DIR;
	  for my $file (@files) {
	    #push @dirs,"${dir}/${file}";
	    my $newdir = "${dir}/${file}";
	    print STDOUT "newdir=\t ${newdir} \n";
	    
	    opendir NEWDIR,$newdir or die;
	    my @files2 = grep {/.MuDst.root/} readdir NEWDIR;
	    closedir DIR;
	    my $found=0;
	    for my $file2 (@files2) {
	      ++$found;
	      # Now drop in symbolic links from Frank's area
	      #print STDOUT "Found file $file in dir \n";
	      symlink "${newdir}/${file2}","${file2}";
	    }
	    print STDOUT "Created $found sym-links \n";
	    
	  }
	}
      }
    }
        
    chdir "../../";
  }
  chdir "../"
}
