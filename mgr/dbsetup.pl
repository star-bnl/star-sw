#!/opt/star/bin/perl -w
#
# $Id: dbsetup.pl,v 1.4 1999/07/25 16:12:43 wenaus Exp $
#
# $Log: dbsetup.pl,v $
# Revision 1.4  1999/07/25 16:12:43  wenaus
# Allow for remote DB access
#
# Revision 1.3  1999/07/21 09:19:08  wenaus
# Add STAR environment variables
#
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
use File::Basename;
use lib "/star/u2d/wenaus/datadb";
use lib "/afs/rhic/star/group";

## STAR environment setup
$STAR_ROOT="/afs/rhic/star";
$GROUP_DIR="$STAR_ROOT/group";
$STAR_PATH="$STAR_ROOT/packages";
if ( ! $STAR_LEVEL ) {$STAR_LEVEL="pro"}
$STAR_VERSION = (fileparse(readlink("$STAR_PATH/$STAR_LEVEL")))[0];
$STAR="$STAR_PATH/$STAR_VERSION";
require "STAR_SYS.pl";  # define $STAR_HOST_SYS
$STAR_LIB="$STAR/.$STAR_HOST_SYS/lib";
$MINE_LIB=".$STAR_HOST_SYS/lib";
$STAR_BIN="$STAR/.$STAR_HOST_SYS/bin";
$STAR_MGR="$STAR/mgr";
$STAR_PAMS="$STAR/pams";
$STAR_DATA="$STAR_ROOT/data";
$STAR_PARAMS="$STAR/params";
$STAR_CALIB="$STAR_ROOT/calib";
$STAR_PROD="$STAR/prod";
$STAR_CVSROOT="$STAR_PATH/repository";
$ROOT_LEVEL='';
if ( -e "$STAR/mgr/ROOT_LEVEL" ) {$ROOT_LEVEL=`cat $STAR/mgr/ROOT_LEVEL`}
$CERN_LEVEL='';
if ( -e "$STAR/mgr/CERN_LEVEL" ) {$CERN_LEVEL=`cat $STAR/mgr/CERN_LEVEL`}
$STAR_PATH="/usr/afsws/bin:/usr/afsws/etc:/opt/star/bin:/usr/sue/bin:/usr/local/bin:$GROUP_DIR:$STAR_MGR:$STAR_BIN";
$CERN="/cern";
$CERN_ROOT="$CERN/$CERN_LEVEL";

my $systype = substr($STAR_HOST_SYS,0,4);
if ( $systype eq 'sun4' ) {
} elsif ( $systype eq 'i386' ) {
    $STAR_PATH.=":/usr/local/bin/ddd";
    $LD_LIBRARY_PATH="/usr/lib:/usr/local/lib:$MINE_LIB:$STAR_LIB:/usr/dt/lib:/usr/openwin/lib";
} else {
    $LD_LIBRARY_PATH="/opt/SUNWspro/lib:/usr/openwin/lib:/usr/dt/lib:/usr/local/lib:/afs/rhic/star/packages/ObjectSpace/2.0m/lib:$MINE_LIB:$STAR_LIB:/usr/lib";
}
$LD_LIBRARY_PATH.=":/usr/ccs/lib:/opt/star/lib";

## Database setup
$dbuser="root";
$dbpass="";
$dbname="";
if ($dbuser eq 'root') {
    $dbname="system_data:duvall.star.bnl.gov";
} else {
    $dbname="".$dbuser."_data:duvall.star.bnl.gov";
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
