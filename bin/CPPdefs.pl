#!/usr/bin/env perl 
use File::Find;
use Cwd;
use File::Path;
my $DirPath = cwd();
find_sources($DirPath);
sub find_sources {
  @main::search_files   = ();
  if (-d $_[0]) {
    &File::Find::find(\&wanted, @_);
    for my $dir (@_) {
      if ( -d $dir ) { 
	my $i = 0;
#	for ($i=0;$i<=$#main::search_files;$i++) {
#	   $main::search_files[$i] = cutdir($dir, $main::search_files[$i]);
#	}
      }
    }
  }
}
sub wanted {
  #  print "wanted ",$_,"\n";
  my ($dev,$ino,$mode,$nlink,$uid,$gid) = lstat($_);
  if ( -d _ &&
       (/^CVS$/ ||
	/^macros$/ ||
	/^html$/ ||
	/^idl$/ ||
	/^exa$/ ||
	/^inc$/ ||
	/^doc$/ ||
	/^run$/ ||
	/^examples$/ ||
	/^local$/ ||
	/^hold$/ ||
	/^wrk$/)
     ) {$File::Find::prune = 1; return;}
  if (
      /^.*\.c$/       ||
      /^.*\.cc$/      ||
      /^.*\.cxx$/     ||
      /^.*\.cpp$/     ||
      /^.*\.g$/       ||
      /^.*\.age$/     ||
#for STAR      /^.*\.f$/       ||
      /^.*\.cdf$/       ||
      /^.*\.F$/) { 
#    print " $File::Find::name\n";
    LookForHash($File::Find::name);
    push @main::search_files,  $File::Find::name; 
  }
}
sub LookForHash($) {
  my ($file) = @_;
  open (In,$file) or die "Can't open $file";
  while (my $line = <In>) {
    next if $line !~ /^\s*#/;
    next if $line =~ /#include/;
    next if $line =~ /^\s*#\s*include/;
    next if $line =~ /^\s*#\s*endif/;
    next if $line =~ /__linux__/;
      chomp($line);
    print $line, " -- ", $file,"\n" ;
  }
  close (In);
}
