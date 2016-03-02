#!/opt/star/bin/perl
#
# $Id: swstat.pl,v 1.3 2000/07/26 14:41:57 wenaus Exp $
#
# $Log: swstat.pl,v $
# Revision 1.3  2000/07/26 14:41:57  wenaus
# duvall management cron job
#
# Revision 1.2  1999/11/22 18:18:30  wenaus
# Clean up
#
# Revision 1.1  1999/09/16 16:37:10  wenaus
# cron script to assemble software/machine/site usage statistics
#
#
######################################################################
#
# swstat.pl
#
# Author: T. Wenaus
#
# Build software and machine usage statistics from logs
#
# Usage: swstat.pl in a cron job
#

package swstat;

use strict;

my $debugOn=0;
my %rcfNodes = (
                'rcasN' => 0,
                'rcf' => 0,
                'rcrsN' => 0,
                'rcrsuser' => 0,
                'rlnx01' => 0,
                'rlnx02' => 0,
                'rlnx03' => 0,
                'rlnxsp' => 0,
                'rlnxasis' => 0,
                'rsunasis' => 0,
                'rmds00' => 0,
                'rmds03' => 0,
                'rmine02' => 0,
                'rmine03' => 0,
                'rsgiN' => 0,
                'rsun00' => 0,
                'atlas00' => 0,
                'play1' => 0,
                'play2' => 0,
                'play3' => 0,
                'play7' => 0,
                'play8' => 0,
                'play10' => 0,
                'play11' => 0,
                'play14' => 0,
                'rdev' => 0,
                'www2' => 0,
                'rftpexp' => 0
                );
my %starSunNodes = (
                    'sol' => 0,
                    'onlsun1' => 0,
                    'garland' => 0,
                    'daqman' => 0
                    );
my %starLinuxNodes = (
                      'duvall' => 0,
                      'coburn' => 0,
                      'robinson' => 0,
                      'pickford' => 0,
                      'midler' => 0,
                      'connery' => 0,
                      'onllinux1' => 0,
                      'onllinux2' => 0,
                      'onllinux3' => 0,
                      'onllinux4' => 0
                      );
my %deskNodes = (
                 'vega' => 0,
                 'elvis' => 0,
                 'bogart' => 0,
                 'monroe' => 0,
                 'lemmon' => 0,
                 'sellers' => 0,
                 'soliton' => 0,
                 'astaire' => 0,
                 'monet' => 0,
                 'io' => 0,
                 'galaxy' => 0,
                 'charm' => 0,
                 'richpc' => 0,
                 'depardieu' => 0,
                 'streisand' => 0,
                 'rigel' => 0
                 );
my %starHpNodes = (
                   'flynn' => 0,
                   'hpplus01' => 0,
                   'hpplus03' => 0
                   );

my %usatlasNodes = (
                    'linuxN' => 0
                    );

my %starLblNodes = (
                    'starlxN' => 0,
                    'pdsflxN' => 0,
                    'starsu00' => 0,
                    'pdsfsu00' => 0,
                    'starlbl5' => 0,
                    'rncsv1' => 0,
                    'rncus1' => 0,
                    'rncus3' => 0,
                    'rncsmb' => 0,
                    'rnchp3' => 0,
                    'sseos' => 0
                    );                 
my %starRiceNodes = (
                     'bonner-pcsN' => 0,
                     'bonner-pc19' => 0,
                     'bonner-lap1' => 0
                     );
my %starIndianaNodes = (
                        'pc-star1' => 0,
                        'pc-star2' => 0,
                        'pc-star3' => 0,
                        'pc-star4' => 0,
                        'pc-star5' => 0
                        );
my $versions = `cd /afs/rhic/star/packages; ls -1 | grep SL`;
$versions =~ s/\n/ /g;
$versions .= "DEV00 .DEV";
#@statfiles = ( 'SL99b', 'SL99d', 'SL99e', 'SL99f', 'SL99g', 'SL99h');
my @statfiles = split (/ /,$versions);
my %stattypes = ( 'star' => 'env',
                  'root4star' => 'root'
                  );
my %levels, my %versions, my %users, my %hosts, my %userlev, my %hostLoc;
my $rcfNodeCount, my $starSunNodeCount, my $starLinuxNodeCount, my $starHpNodeCount, my $usatlasNodeCount,
    my $starLblNodeCount, my $deskNodeCount, my $starRiceNodeCount, my $starIndianaNodeCount;
foreach my $type ( sort keys %stattypes ) {
    foreach my $e ( keys %starHpNodes ) { $starHpNodes{$e} = 0; }
    
    foreach my $file ( @statfiles ) {
        my $filename = "/afs/rhic/star/group/statistics/$type$file";
        open( FILE, "<$filename" ) or next;
        print "Opened $filename\n" if $debugOn;
        while (<FILE>) {
            if ( substr($_,0,5) eq 'unset' ) {next}
            if ( substr($_,0,3) eq 'EOF' ) {next}
            $_ =~ s/asked for/in/;
            my ($user, $dum1, $host, $dum2, $level, $dum4, $ver, $wk, $mo, $dy, $time, $zone, $yr) = 
                split / /;
            if ( ! ($user =~ m/^[a-z]+/) ) {next}  # a few with junk characters
            $level =~ s/STAR_LEVEL=//;
            if ( $level ne '.dev' && $level ne 'dev' && $level ne 'new' && $level ne 'pro' && $level ne 'old' && substr($level,0,2) ne 'SL' && $level ne 'DEV00' && $level ne '.DEV' ) {next}
            $ver =~ s/STAR_V[A-Z]+N=//;
            $host =~ s/\..*//;
            $host =~ s/rcrs[0-9]+/rcrsN/;
            $host =~ s/^linux[0-9][0-9][0-9]/linuxN/;
            $host =~ s/rsgi[0-9]+/rsgiN/;
            $host =~ s/rcas[0-9]+/rcasN/;
            $host =~ s/starlx[0-9]+/starlxN/;
            $host =~ s/pdsflx[0-9]+/pdsflxN/;
            $host =~ s/bonner-pcs[0-9]+/bonner-pcsN/;
            if ($level eq '.DEV' ) {$level = '.dev'}
            if ($level eq 'DEV00' ) {$level = 'dev'}
            if ($level eq 'DEV' ) {$level = 'dev'}
            if (substr($level,0,2) ne 'SL') {$levels{$level}++}
            $versions{$ver}++;
            $users{$user}++;
            $hosts{$host}++;
            $userlev{"$user-$level"}++;
            
            $hostLoc{$host} = '?';
            if ( exists( $rcfNodes{$host} ) ) {$rcfNodeCount++; $hostLoc{$host}='RCF'};
            if ( exists( $starSunNodes{$host} ) ) {$starSunNodeCount++,$hostLoc{$host}='BNL Sun';};
            if ( exists( $starLinuxNodes{$host} ) ) {$starLinuxNodeCount++;$hostLoc{$host}='BNL Linux'};
            if ( exists( $starHpNodes{$host} ) ) {$starHpNodeCount++;$hostLoc{$host}='HP'};
            if ( exists( $usatlasNodes{$host} ) ) {$usatlasNodeCount++;$hostLoc{$host}='USAtlas'};
            if ( exists( $starLblNodes{$host} ) ) {$starLblNodeCount++;$hostLoc{$host}='LBL'};
            if ( exists( $deskNodes{$host} ) ) {$deskNodeCount++;$hostLoc{$host}='Desktop'};
            if ( exists( $starRiceNodes{$host} ) ) {$starRiceNodeCount++;$hostLoc{$host}='Rice'};
            if ( exists( $starIndianaNodes{$host} ) ) {$starIndianaNodeCount++;$hostLoc{$host}='Indiana'};
        }
    }
    
    my $pfx = $stattypes{$type};
    open (FSITES,">/star/starlib/doc/www/html/comp-nfs/swstat-".$pfx."sites.txt");
    printf(FSITES "RCF       %6d\n",$rcfNodeCount);
    printf(FSITES "BNL Sun   %6d\n",$starSunNodeCount);
    printf(FSITES "BNL Linux %6d\n",$starLinuxNodeCount);
    printf(FSITES "Desktop   %6d\n",$deskNodeCount);
    printf(FSITES "US ATLAS  %6d\n",$usatlasNodeCount);
    printf(FSITES "HP        %6d\n",$starHpNodeCount);
    printf(FSITES "LBL       %6d\n",$starLblNodeCount);
    printf(FSITES "Rice      %6d\n",$starRiceNodeCount);
    printf(FSITES "Indiana   %6d\n",$starIndianaNodeCount);
    close(FSITES);
    open (FLEV,">/star/starlib/doc/www/html/comp-nfs/swstat-".$pfx."level.txt");
    foreach my $lv ( sort keys %levels ) {
        printf(FLEV "Level %4s %6d\n",$lv,$levels{$lv});
    }
    close(FLEV);
    open (FVER,">/star/starlib/doc/www/html/comp-nfs/swstat-".$pfx."ver.txt");
    foreach my $vr ( sort keys %versions ) {
        printf(FVER "Version %6s %6d\n",$vr,$versions{$vr});
    }
    close(FVER);
    open (FUSERS,">/star/starlib/doc/www/html/comp-nfs/swstat-".$pfx."users.txt");
    my $nUsers = 0;
    my $nUsers10 = 0;
    my $nUsers50 = 0;
    my $nUsers100 = 0;
    my $us;
    foreach $us ( sort keys %users ) {
        printf(FUSERS "%-8s %5d  .dev %-5d dev %-5d new %-5d pro %-5d old %-5d\n",
               $us,$users{$us},$userlev{"$us-.dev"},$userlev{"$us-dev"},
               $userlev{"$us-new"},$userlev{"$us-pro"},$userlev{"$us-old"});
        $nUsers++;
        $nUsers10++ if ( $users{$us} > 10 );
        $nUsers50++ if ( $users{$us} > 50 );
        $nUsers100++ if ( $users{$us} > 100 );
    }
    print FUSERS "\nTotal users: $nUsers    >10 uses: $nUsers10  >50: $nUsers50  >100: $nUsers100\n";
    close(FUSERS);
    open (FHOSTS,">/star/starlib/doc/www/html/comp-nfs/swstat-".$pfx."hosts.txt");
    foreach my $hs ( sort keys %hosts ) {
        printf(FHOSTS "Host %-15s %6d %s\n",$hs,$hosts{$hs},$hostLoc{$hs});
    }
    close(FHOSTS);
}
