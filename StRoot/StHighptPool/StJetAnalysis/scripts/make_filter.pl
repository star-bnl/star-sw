#!/usr/bin/perl -w
use File::stat;
use strict;
open OUT,">do_bsubs.tcsh" or die;
print OUT '#!/bin/tcsh',"\n";
for my $centrality (qw(central minbias ppMinBias)) {
#for my $centrality (qw(minbias)) { # just for testing
  print STDERR "$centrality\n";
  next unless (-d $centrality);
  chdir "$centrality";
  print OUT "cd $centrality\n";
  #print OUT "echo $centrality\n";
  for my $type (qw(ReversedFullField FullField HalfField ReversedHalfField)) {
  #for my $type (qw(ReversedFullField)) { #just for testing
    print STDERR "$type\n";
    next unless (-d $type);
    chdir "$type";
    print OUT "cd $type\n";
    print OUT "echo $type\n";
    my @checkdirs = qw(./ ./take1);
    #Ok, this is wherever my files are that I don't want recreated
    push @checkdirs,"./";
    
    my %filedone = ();
    for my $dir (@checkdirs) {
      print STDERR "checking $dir";
      opendir DIR,$dir;
      my @files = grep {/MuDst.root_has/} readdir DIR;
      closedir DIR;
      my $nFilesDone=0;
      for my $file (@files) {
	my $original = $file;
	my ($run,$raw) = $file =~ /st_physics_(\d+)_raw_(\d+)/;
	my $fileString = "st_physics_${run}_raw_${raw}";
	#print STDERR "file is $original adding string $fileString \n";
	++$nFilesDone;
	$filedone{$fileString} = 1;
      }
      print STDERR "Found $nFilesDone files already filtered \n";
      print STDERR "\ndone\n";
    }

    #print STDERR "Look at contents of hash \n";
    #my $nfiles=0;
    #for my $file (sort keys %filedone) {
      #++$nfiles;
      #print STDERR "key $nfiles is $file \n";
    #}
    
    my @dirs = ("runs");
    my %filem = ();
    
    my %runraw =();
    for my $dir (@dirs) {
      print STDERR "checking dir $dir\n";
      opendir (DIR,$dir) or die;
      my @files = grep {/.MuDst.root$/} readdir DIR;
      closedir DIR;
      my $nFiles=0;
      my $maxFiles=10;
      for my $file (@files) {
	
	#next if $nFiles>$maxFiles; #temporary, just for testing
	
	my ($run,$raw) = $file =~ /st_physics_(\d+)_raw_(\d+)/;
	my $stat = stat("$dir/$file");
	next unless ($stat && $stat->size);
	++$nFiles;
	(my $newfile = $file) =~ s/.MuDst.root//g;
	#print STDERR "search for $newfile in checkdirs \n";
	next if ($filedone{"${newfile}"});
	#print STDERR "file not found \n";
	my $div25 = int($raw/25);
	$runraw{$run}{$div25}{$file} = $dir;
	#    $filem{$file} = $stat->mtime;
      }
      
    }
    
    for my $run (sort keys %runraw) {
      for my $type (sort keys %{$runraw{$run}}) {
	open OUT2,">dofilter_${run}_${type}.tcsh" or die;
	select OUT2;
	print <<EOF;
#!/bin/tcsh
setenv GROUP_DIR /afs/rhic.bnl.gov/rhstar/group

if ( -r  \$\{GROUP_DIR\}/star_login.csh ) then
            source \$\{GROUP_DIR\}/star_login.csh 
endif

if ( -r  \$\{GROUP_DIR\}/star_cshrc.csh ) then
        source \$\{GROUP_DIR\}/star_cshrc.csh 
endif
setenv NODEBUG yes
source \$\{GROUP_DIR\}/.starver SL02e
EOF


      LOOPFILE: for my $file(sort keys 
			     %{$runraw{$run}{$type}}) {

	  my $dir = $runraw{$run}{$type}{$file};  
	  #    my ($run,$raw) = $file =~ /st_physics_(\d+)_raw_(\d+)/;

	  (my $newfile = $file) =~ s/.event.root//g;
	  print <<EOF; 
root4star -b -q 'TestFilter.C("runs/","$file","$file")'
EOF
	}

	close OUT2;
	chmod 0775,"dofilter_${run}_${type}.tcsh";
	print OUT "bsub -u miller\@star.physics.yale.edu -q star_cas_short -L/bin/tcsh -J ${type}_${run}str -o ./${run}_${type}.log ./dofilter_${run}_${type}.tcsh\n";
#	print STDERR "found ${run} ${type}\n";
      }
    }
    print OUT "cd ../\n";
    chdir "../";
  }
  print OUT "cd ../\n";
  chdir "../";
}
close OUT;
chmod 0775,"do_bsubs.tcsh";

