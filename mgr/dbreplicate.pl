#!/opt/star/bin/perl
#
# $Id: dbreplicate.pl,v 1.1 1999/09/21 12:29:36 wenaus Exp $
#
######################################################################
#
# dbreplicate.pl
#
# T. Wenaus 9/99
#
# Manages MySQL DB replication. For use in a cron job.
#
# Usage:    dbreplicate.pl 
#
# $Log: dbreplicate.pl,v $
# Revision 1.1  1999/09/21 12:29:36  wenaus
# Database replicator
#
#
use lib "/star/u2d/wenaus/datadb";
require "dbsetup.pl";

$debugOn = 0;

@replicationHosts = (
                     'db1.star.bnl.gov'
                     );

#$host = `hostname`;
#chomp $host;
#$hostOK=0;
#foreach $h ( @replicationHosts ) {
#    if ( $host eq $h ) {$hostOK=1}
#}
#if ( ! $hostOK ) {
#    print "Can only be run from replication hosts\n";
#    exit;
#}

## Databases to replicate
%dbToReplicate = (
               'system_data' => 1,
               'params' => 1,
               'mysql' => 1,
               'Calibrations' => 1,
               'Calibrations_tpc' => 1,
               'Conditions' => 1,
               'Conditions_tpc' => 1,
               'Configurations' => 1,
               'Configurations_daq' => 1,
               'Geometry' => 1,
               'Geometry_tpc' => 1,
               'RunParams' => 1,
               'RunParams_tpc' => 1,
               'StarDb' => 1
               );

$backupDir = '/star/sol4/duvall/archive/mysql';

foreach $repHost ( @replicationHosts ) {
    foreach $db ( keys %dbToReplicate ) {
        ## Replicate the desired databases
        ($dy,$mo,$yr) = (localtime())[3,4,5];
        $mo++;
        $yr=$yr+1900;
        $cdate = sprintf("%4.4d%2.2d%2.2d",$yr,$mo,$dy);
        $fname = "$backupDir/$db-$cdate.sql.gz";
        if ( ! -e $fname ) {
            print "Today's backup $fname does not exist\n";
        } else {
            ## Drop and recreate
            $cmd = "/usr/local/mysql/bin/mysql --host=$repHost --execute='drop database if exists $db; create database $db'";
            $out = `$cmd`;
            ## Populate it
            $cmd = "gunzip -c $fname | /usr/local/mysql/bin/mysql --host=$repHost -C $db";
            print "$cmd\n" if $debugOn;
            $out = `$cmd`;
            if ( $out ne '' ) {print $out}
            ## For mysql DB, flush privileges
            if ( $db eq 'mysql' ) {
                `/usr/local/mysql/bin/mysql --host=$repHost --execute='flush privileges' mysql`
                }
        }
    }
}
