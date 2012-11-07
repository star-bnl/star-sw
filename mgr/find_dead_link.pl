#!/usr/bin/env perl
use Env;
use File::Find;
if (defined($AFS)) {$File::Find::dont_use_nlink;}
#require "find.pl";
my $dir = ".";
if ($#ARGV > -1) {$dir = $ARGV[0];}
print "Find dead link in $dir\n";
#&find (\&wanted,$dir);
&File::Find::find({wanted => \&wanted, follow => 1}, $dir);
my $line = "";
sub wanted {
  if (-l and not -e) {
    $line .= " " . $File::Find::name;
    print "bogus link: $File::Find::name ==> remove it : $line\n"; 
#    system("rm $File::Find::name") or die "Can't remove $File::Find::name";
  }
}
