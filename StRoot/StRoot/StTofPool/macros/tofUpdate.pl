#!/usr/bin/perl  
#
#MPD
#Script changes three values and
#keeps the other values intact and
#inserts all with new time stamp
#040407
#
use Getopt::Std;
use DBI;


#for debugging
# DBI->trace(1);

$dbUser='';
$dbPort='';
$dbPort2='';

#
#-> connect to DB
#

$dbh = DBI->connect("DBI:mysql:Geometry_tof:robinson.star.bnl.gov",$dbuser)

    || die "Cannot connect to server $DBI::errstr\n";

#define/assign query 
$querycount = "select count(*) from tofSlatGeom";
	print $querycount,"\n";
#prepare then execute
$countsth=$dbh->prepare($querycount);
$countsth->execute;

#use the following when debugging turn off when running
#if(!$countsth->execute){ die "SQL syntax is wrong " ; };
$count = $countsth->fetchrow_array();

print $count " rows in tofSlatGeom \n+++++++++++++++++++++++++++\n\n";


$querysel = "select dataID,
		    nodeID, 
		    elementID, 
		    flavor, 
		    numRows,
		    schemaID,
		    deactive,
		    ieta,
		    z,
		    z_min,
		    z_max,
		    cosang,
		    r,
		    eta_min, 
		    eta_max,
		    eta,
		    iphi, 
		    phi, 
		    phi_min, 
		    phi_max, 
		    trayId 
		from tofSlatGeom
		where beginTime = '2002-11-05 00:00:00' ";

$selsth=$dbh->prepare($querysel);
$selsth->execute;

#This is the cursor - fetching one row at a time
while ( ( $dataID, 
	  $dnodeID, 
	  $delementID, 
	  $dflavor, 
	  $dnumRows, 
	  $dschemaID, 
	  $ddeactive, 
	  $dieta, 
	  $dz, 
	  $dz_min, 
	  $dz_max, 
	  $dcosang, 
	  $dr, 
	  $deta_min, 
	  $deta_max, 
	  $deta, 
	  $diphi, 
	  $dphi, 
	  $dphi_min, 
	  $dphi_max, 
 	  $dtrayId ) = $selsth ->fetchrow_array) 
{

      print "ELEMENT ID IS ", $delementID, "; LINE NUMBER IS ", $dataID, "\n++++++++++++++++++++++++++++++++++++\n";
	print "old phi = ", $dphi, "\n";  

     $nphi = $dphi + ((2 * 3.14159265)*(6/360));

	print "updated phi = ", $nphi, "\n\n";  

	print "old phi_max = ", $dphi_max, "\n";  

     $nphi_max = $dphi_max + ((2 * 3.14159265)*(6/360));

	print "updated phi_max = ", $nphi_max, "\n\n";  

	print "old phi_min = ", $dphi_min, "\n";  

     $nphi_min = $dphi_min + ((2 * 3.14159265)*(6/360));

	print "updated phi_min = ", $nphi_min, "\n\n";  

	$queryInsert= "insert into tofSlatGeom set
		    nodeID = $dnodeID, 
		    elementID = $delementID, 
		    beginTime = '2004-04-07 00:00:00',
		    flavor = \'$dflavor\', 
		    numRows = $dnumRows,
		    schemaID = $dschemaID,
		    deactive = $ddeactive,
		    ieta = $dieta,
		    z = $dz,
		    z_min = $dz_min,
		    z_max = $dz_max,
		    cosang = $dcosang,
		    r = $dr,
		    eta_min = $deta_min, 
		    eta_max = $deta_max,
		    eta = $deta,
		    iphi = $diphi, 
		    phi = $nphi, 
		    phi_min = $nphi_min, 
		    phi_max = $nphi_max, 
		    trayId = $dtrayId";
		
print $queryInsert,"\n\n";
	
	$sthIn=$dbh->prepare($queryInsert);
	$sthIn->execute;
}

$selsth->finish();
$sthIn->finish();
$countsth->finish();

$dbh->disconnect();
exit(0);

 
