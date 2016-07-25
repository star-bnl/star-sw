#!/opt/star/bin/perl

@statfiles = `ls -1 /afs/rhic/star/packages/saved_statistics/* /afs/rhic/star/group/statistics/*`;
foreach $filename ( @statfiles ) {
    open( FILE, "<$filename" ) or die "Can't open $filename";
    while (<FILE>) {
      break if /EOF/;
        ($user, $dum1, $host, $dum2, $dum3, $level, $dum4, $ver, $dum5, $wk, $mo, $dy, $time, $zone, $yr) = 
            split / /;
        $level =~ s/STAR_LEVEL=//;
        $ver =~ s/STAR_VERSION=//;
        $host =~ s/\..*//;
        $host =~ s/rcrs[0-9]+/rcrsN/;
        $host =~ s/rsgi[0-9]+/rsgiN/;
        $host =~ s/rcas[0-9]+/rcasN/;
        $host =~ s/starlx[0-9]+/starlxN/;
        $host =~ s/pdsflx[0-9]+/pdsflxN/;
        $host =~ s/bonner-pcs[0-9]+/bonner-pcsN/;
#	print "$user $host $level $ver $mo $dy $time $zone $yr\n";
#        print "User $user, level $level\n";
        $levels{"$level"}++;
        $versions{"$ver"}++;
        $users{"$user"}++;
        $hosts{"$host"}++;
        $userlev{"$user-$level"}++;
    }
}
print "-----\n";
foreach $lv ( sort keys %levels ) {
    print "Level $lv ".$levels{$lv}."\n";
}
print "-----\n";
foreach $vr ( sort keys %versions ) {
    print "Version $vr ".$versions{$vr}."\n";
}
print "-----\n";
foreach $us ( sort keys %users ) {
    print "User $us ".$users{$us}." .dev ".$userlev{"$us-.dev"}." dev ".$userlev{"$us-dev"}." new ".$userlev{"$us-new"}." pro ".$userlev{"$us-pro"}." old ".$userlev{"$us-old"}."\n";
}
print "-----\n";
foreach $hs ( sort keys %hosts ) {
    print "Host $hs ".$hosts{$hs}."\n";
}
