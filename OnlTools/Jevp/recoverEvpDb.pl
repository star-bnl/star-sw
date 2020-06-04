#!/usr/bin/perl


my $filename = shift;
open(my $fh, '<', $filename) or die("usage:  recoverEvpDb.pl filename_of_runs.txt");

chdir "/home/evpops/cvs" or die("can't chgdir to /home/evps/cvs");

while(my $row = <$fh>) {
    chomp $row;
    
    if ( $row !~ /^\/a\// ) {
	$row = "/a/${row}";
    }

    print "Update $row\n";
    `OnlTools/Jevp/launch JevpServerMain -updatedb -file $row`;
}
