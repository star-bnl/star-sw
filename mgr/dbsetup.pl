#!/opt/star/bin/perl -w
#
# $Id: dbsetup.pl,v 1.2 1999/07/07 13:22:11 wenaus Exp $
#
# $Log: dbsetup.pl,v $
# Revision 1.2  1999/07/07 13:22:11  wenaus
# incorporate run log
#
# Revision 1.1  1999/06/25 15:17:00  wenaus
# Add scripts for managing prod DB and SW guide
#
#
######################################################################
#
# dbsetup.pl
#
# T. Wenaus 5/99
#
# Production database setup routines
#

use DBI;

$dbuser="root";
$dbpass="octopus3";
$dbname="";
if ($dbuser eq 'root') {
    $dbname="system_data";
} else {
    $dbname=$dbuser."_data";
}

# DAQ format info:
#   http://daq.star.bnl.gov/~daq/DOC/mz_fmt_defaults.html
# DAQ run type numbering: test=0, ped=1, gain=2, physics=3
$runTypeList="'unknown','test','ped','gain','physics'";
%runTypes = ("" => "",
             "test" => "Test",
             "ped" => "Pedestal",
             "gain" => "Gain",
             "physics" => "Physics"
             );

# DAQ gain mode numbering: seesaw=0, linear=1, log=2, corrected=3
$gainModeList="'unknown','seesaw','linear','log','corrected','uncorrected'";
%gainModes = (
              "" => "",
              "seesaw" => "Seesaw",
              "linear" => "Linear",
              "log" => "Logarithmic",
              "corrected" => "Corrected",
              "uncorrected" => "Uncorrected"
              );

# DAQ ped mode numbering: 0=off, 1=on
$pedModeList="'unknown','off','on'";


# DAQ event types
$eventTypeList="'unknown','BEGR','ENDR','DATA','SLOW'";

# Bank types supported by DAQ
$bankTypeList="'TPC','EMC','SMD','SVT','FTP','TOF','RIC','L3_','TRG','BEG','END','PED','CAL','SLO'";

$sectorList="'01','02','03','04','05','06','07','08','09','10','11','12','13','14','15','16','17','18','19','20','21','22','23','24'";

# Triggers:
# 1. cosmic ray
# 2. TPC pulser
# 3. TPC laser
# 4. Collision = M(CTB)>4
# 5. ZDC = ZDCE.ZDCW
# 6. central = M>8.ZDC
$trigTypeList="'cosmic','pad-pulser','fee-pulser','laser','collision','zdc','central','ped'";
%trigTypes = (
              "" => "",
              "ped" => "Pedestal",
              "cosmic" => "Cosmic",
              "pad-pulser" => "Pad plane pulser",
              "fee-pulser" => "FEE pulser",
              "laser" => "Laser",
              "zdc" => "ZDC = ZDCE.ZDCW",
              "collision" => "Collision = M(CTB)>4",
              "central" => "Central = M>8.ZDC"
              );

# Processing stages
$procStages="'unknown','gen','mchit','mcdigi','daq','reco','analysis'";

# Tables
$DataFileT = "DataFile";
$FileLocationT = "FileLocation";
$DataSetT = "DataSet";
$SubsetT = "Subset";
$DataDirT = "DataDir";
$RunT = "Run";
$EventT = "Event";
$RunFileT = "RunFile";

######################
sub StDbConnect {
    $dbh = DBI->connect("dbi:mysql:$dbname", $dbuser, $dbpass)
        || die "Cannot connect to db server $DBI::errstr\n";
}

######################
sub StDbDisconnect {
    $dbh = $dbh->disconnect() || die "Disconnect failure $DBI::errstr\n";
}

1;
