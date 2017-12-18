#!/usr/bin/env perl
use Env;
use File::Find ();
use File::Basename;
use Cwd;
my $debug = 1;
my $dir = cwd();
&File::Find::find (\&wanted_S,$dir);
#________________________________________________________________________________
sub wanted_S {
    ($dev,$ino,$mode,$nlink,$uid,$gid) = lstat($_);
    my $name = $File::Find::name; 
#    print "name = $name\n"                                   if ($debug);
    if ( ! -r $name ) {
      print "$name unreadable\n" if ($debug);
      next;
    }
    if ( -f $name) {
#      print "$name file\n" if ($debug);
      next;
    }
    if ( -l $name) {
      print "$name link\n" if ($debug);
    }
    if ( -d $name) {
      print "$name dir\n" if ($debug);
    }
#     if (-e _ && /\.\d+$/) {    
# 	print "Version $name\n"                              if ($debug);
# 	my $so = $_; $so =~ s/\.\d*$//;
# 	my $link = $so;
# 	if (-l $link) {
# 	    # print "$link\n";
# 	    my $file = readlink $link;      
# 	    print "Found Link = $link -> $file\n"            if ($debug);
	    
# 	    my $vers = $file; $vers =~ s/$so\.//; 
# 	    print "so = $so vers = $vers\n"                  if ($debug);
	    
# 	    #      chop ($file);
# 	    $so .= ".[0-9]*";  print "so = $so\n"            if ($debug);
# 	    my @So = glob $so; print "So = @So\n"            if ($debug);
# 	    my $keep = 0;
# 	    foreach my $s (@So) {
# 		my $v = $s; $v =~ s/$link\.//; 
# 		print "s = $s v = $v vers = $vers\n"         if ($debug);
# 		next if !$v;
# 		if ($vers == $v) {
# 		    print "keep \t$file => \t$link\n"        if ($debug);
# 		    next;
# 		}
# 		my $diff = $vers - $v - $KEEP;
# 		if ($diff lt 0) {
# 		    print "keep \t$file => \t$link / $s : $vers - $v - $KEEP = $diff < 0\n" if ($debug);
# 		    next;
# 		}
# 		print "Delete $s \n";
# 		unlink $s or die "Could not remove $s (check token or access)\n";
# 	    }
# 	}
#     } else {
# 	# we are in a sub-dir
# 	# `/bin/ls -l $name`;
# 	# chomp($pwd = `pwd`);
# 	# print "$name (1)\n";
# 	if ($debug){
# 	    if ( $pdir ne $dir ){
# 		chomp($pwd = `pwd`);
# 		# print "$dir $pwd\n";
# 		$pdir = $dir;
# 	    }
# 	    &CheckAndRemove(1,"$pwd/",$_);
# 	}
#     }
}



