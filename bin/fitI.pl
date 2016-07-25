#! /usr/bin/env perl
use File::Basename;
use Sys::Hostname;
use Cwd;
my $rootfile = $ARGV[0];
exit if ! $rootfile;
my @histGF = ();
my @histGP = ();
my $all = 0;
if (! $all ) {
  @histGF = qw (  SecRow3C SecRow3A Charge3I Charge3O Charge3IB Charge3OB ChargeSum3 ChargeSum3B Phi3 Phi3D);
#		  ChargeA2I ChargeA2O ChargeA2IB ChargeA2OB);
  @histGP = qw ( TPoints70B TPointsB TPointsBU TPoints70BU); #Charge3I Charge3O
} else {
  @histGF = qw (  SecRow3C SecRow3A 
		 Charge3I Charge3O
		 SecRow3C
		 PressureC     
		 PressureTC     
		 TimeC         
		 Z3C 
		 Z3OC           
		 Z3OW Z3OWC
		 dXdEC
		 nitrogenPressureP  inputGasTemperatureP outputGasTemperatureP
		 flowRateExhaustP flowRateRecirculationP
 		 percentMethaneInP percentMethaneInPA percentMethaneInPC
		 ppmWaterOutP ppmWaterOutPA ppmWaterOutPC
		 MultiplicityPI MultiplicityPO ETA3
		);
  @histGP = qw (TPoints70B TPointsB
		 SecRow3 Pressure PressureT  Time Z3 Z3O dXdE PointsB Points70B Points70BU TPointsBU 
		 Pads3 Pads3C
		 Tbins3 Tbins3C
		 PadsTbins3 PadsTbins3C
 		 eP70B ePz eN70B eNz
 		 piP70B piPz piN70B piNz
 		 protonP70B protonPz protonN70B protonNz
 		 kaonP70B kaonPz kaonN70B kaonNz
 		 muonP70B muonPz muonN70B muonNz
 		 deuteronP70B deuteronPz deuteronN70B deuteronNz
 		 eP70BT ePzT eN70BT eNzT
 		 piP70BT piPzT piN70BT piNzT
 		 protonP70BT protonPzT protonN70BT protonNzT
 		 kaonP70BT kaonPzT kaonN70BT kaonNzT
 		 muonP70BT muonPzT muonN70BT muonNzT
 		 deuteronP70BT deuteronPzT deuteronN70BT deuteronNzT
		);
}
#		 SecRow3A
#		 PressureA
#		 TimeP
#		 Z3A
#		 Z3OA 
#		 dCharge3C
#          dXdEA 
# inputGasTemperatureP
#my @histGP = qw (SecRow3 Pressure Time Z3 Z3O dXdE PointsB Points70B TPoints70B TPointsB);
# 		 eP70B ePz eN70B eNz
# 		 piP70B piPz piN70B piNz
# 		 protonP70B protonPz protonN70B protonNz
# 		 kaonP70B kaonPz kaonN70B kaonNz
# 		 muonP70B muonPz muonN70B muonNz
# 		 deuteronP70B deuteronPz deuteronN70B deuteronNz
#		 Z3OC           
#		 dXdE
#		 dXdEA
#		 dXdEC
#		 eP70B ePz eN70B eNz
#		 piP70B piPz piN70B piNz
#		 protonP70B protonPz protonN70B protonNz
#		 kaonP70B kaonPz kaonN70B kaonNz
#		 muonP70B muonPz muonN70B muonNz
#		 deuteronP70B deuteronPz deuteronN70B deuteronNz
#		 TPoints      
#		 TPoints70     
#		 MultiplicityPI
#		 MultiplicityPO
#		 MulRow
#		 MulRowC
#		 MulRowB
#		 MultiplicityPI
#		 MultiplicityPO
#		 MulRow
#		 MulRowC
#		 MulRowB
#		 PointsB
#		 Points
#		 Points70B
#		 Points70
#		 Adc3Ie       
#		 Adc3Oe       
#		 Adc3Iproton  
#		 Adc3Oproton  
#		 Adc3Ikaon    
#		 Adc3Okaon    
#		 Adc3Ipi      
#		 Adc3Opi      
#		 Adc3Imu      
#		 Adc3Omu      
#		 Adc3Ideuteron
#		 Adc3Odeuteron
#		 AdcIZP        
#		 AdcOZP        
#		 AdcIZN        
#		 AdcOZN        
#dEdxFit.C+("SecRow3","G2","",1,1,0,0,5,0)
#		 Adc3Ie       
#		 Adc3Oe       
#		 Adc3Iproton  
#		 Adc3Oproton  
#		 Adc3Ikaon    
#		 Adc3Okaon    
#		 Adc3Ipi      
#		 Adc3Opi      
#		 Adc3Imu      
#		 Adc3Omu      
#		 Adc3Ideuteron
#		 Adc3Odeuteron
#		 AdcIZP        
#		 AdcOZP        
#		 AdcIZN        
#		 AdcOZN        
#		);
##		 TPoints60B    
#		 TPoints60     
#		 AdcIZP AdcIZN AdcOZP AdcOZN 
#		 Adc3I
#		 Adc3O
#		 Adc3IC
#		 Adc3OC
#		 AdcI
#		 AdcO
#		 AdcIC
#		 AdcOC
#		 Adc3Ie
#		 Adc3Oe
#		 Adc3Ipi
#		 Adc3Opi
#		 Adc3IK
#		 Adc3OK
#		 Adc3Ip
#		 Adc3Op
#		 Adc3Id
#		 Adc3Od
#);
my @opt = qw (GP GF);
foreach my $fitype (@opt) {
  my @histos = ();
  if ($fitype eq 'GP') {@histos = @histGP;}
  elsif ($fitype eq 'GF')  {@histos = @histGF;}
  else {next;}
  foreach my $hist (@histos) {
    my $dir = File::Basename::dirname($rootfile);
    my $fil = File::Basename::basename($rootfile);
    my $newfilew = $dir . "/" . $hist . $fitype . $fil;
    print "$rootfile -> $hist => $fitype new file: $newfilew\n";
    next if -r $newfilew;
    my $log = $newfilew;
    $log =~ s/\.root/\.log/;
    my $cmd = "";
#    my $cmd = "bsub -o " . $log ." -q star_cas_short ";
#    my $cmd = "bsub -o " . $log ." -q star_cas_prod ";
    $cmd .= "root.exe -q -b lBichsel.C " . $rootfile;
    #  $cmd .= " '/afs/rhic/star/users/fisyak/.dev/dEdxFit.C+(\"";
    $cmd .= " 'dEdxFit.C+(\"";
    $cmd .= $hist;
    $cmd .= "\",\"" . $fitype . "\")' | tee " . $log;
#  if ($fitype eq "GP") {$cmd .= "\")'";}
#  else {$cmd .= "\",\"G2\",\"\",1,1,0,0,5,0)'";} 
      print "job: $jobs : $cmd \n";
    my $flag = `$cmd`;
  }
}
exit 0;
