#!/usr/bin/env perl
use File::Basename;
use Cwd;
my @dbservers = qw(robinson db01 db02 db03 db04 db05 db06);
foreach my $db (@dbservers) {
  my $dbfile = $db . ".xml";
  my $port = "3306";
  if ($db ne "robinson") {$port = "3316";}
  open (OUT, ">$dbfile") or die "Can't open $dbfile";
  print OUT 
'<StDbServer>
<server> ' .$db. ' </server>
<host> ' .$db. '.star.bnl.gov </host>
<port> ' . $port . '  </port>
<socket> /tmp/mysql.3316.sock </socket>
</StDbServer>
';
  $ENV{STDB_SERVERS} = ".";
  my $cmd = "test -r dbServers.xml && rm dbServers.xml; ln -s " . $dbfile . " dbServers.xml; root4star -q -b Db.C >& " . $db . ".log";
  print "$cmd\n";
  system($cmd);
}
