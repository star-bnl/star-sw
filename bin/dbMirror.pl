#!/opt/star/bin/perl


use Getopt::Std;
use DBI;

$debugOn=1;


getopts('i:o:d:');

my $inputServer=$opt_i;
my $outputServer=$opt_o;
my $database=$opt_d;

if((!$inputServer or !$outputServer or !$database)){ Usage();};

my $dbhost;
my $port;

    @tmpSplit=split /\:/,$inputServer,2;
    $dbhost=$tmpSplit[0];
    $port=$tmpSplit[1];

print "\n";
print "Will Mirror $database from Server=$inputServer to Server=$outputServer\n";
print "\n";

$cmd = qq{ /opt/star/bin/mysqldump --host=$dbhost --port=$port --opt } .
       qq{ $database  > /tmp/$database.sql };

    print $cmd,"\n";

$out=`$cmd`;

    @tmpSplit=split /\:/,$outputServer,2;
    $dbhost=$tmpSplit[0];
    $port=$tmpSplit[1];

$cmd = qq { cat /tmp/$database.sql | }. 
       qq { /opt/star/bin/mysql -h $dbhost --port=$port -C $database};

    print $cmd,"\n";

$out=`$cmd`;

$cmd = qq { rm /tmp/$database.sql };

$out=`$cmd`;


##########################################################################
sub Usage() {

    print "\n";
print "****  Usage   :: dbMirror.pl -i inputServer -o outputServer -d database\n";
    print "****  Purpose :: replicate a database fro source to mirror\n";

    print "                 -i inputserver\n";
    print "                 -o outputserver\n";
    print "                 -d database to be mirrored\n";

    exit;
}



