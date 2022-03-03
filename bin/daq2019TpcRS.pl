#! /usr/bin/env perl
use File::Basename;
use Cwd;
use Env;
my $debug = 0;
my $pwd = cwd();
#my $Trigger =  File::Basename::basename($pwd); $Trigger =~ s/daq_//;
#my $Trigger =  File::Basename::basename($pwd); $Trigger =~ s/TpcRS_/daq_/; $Trigger =~ s/\..*//; print "Trigger = $Trigger\n" if ($debug);
#my $glob = "/net/l401/data/scratch1/daq/2020/" . $Trigger . "/st_physics_adc*.daq";  print "glob = $glob\n" if ($debug);
#my $glob = "/net/l401/data/scratch1/fisyak/Tpc/TpcRS/" . $Trigger . "/st_physics_adc*.MuDst.root";  print "glob = $glob\n" if ($debug);
my $Trigger =  File::Basename::basename($pwd); 
$Trigger =~ s/TpcRS_//; 
$Trigger =~ s/_.*//; 
print "Trigger = $Trigger\n" if ($debug);
my $fNo = 0;
#my $glob = "../" . $Trigger . "/*.MuDst.root"; print "glob = $glob\n" if ($debug);
my $glob = "../daq_" . $Trigger . "/*.MuDst.root"; print "glob = $glob\n" if ($debug);
my @globs = glob $glob; print "globs = @globs\n" if ($debug);
my %Runs19GeV = (
	    "st_physics_adc_20057003_raw_4000002" =>  "230",
	    "st_physics_adc_20057049_raw_6500002" =>  "134",
	    "st_physics_adc_20059061_raw_3500011" =>  "132",
	    "st_physics_adc_20060004_raw_3500012" =>  "197",
	    "st_physics_adc_20060018_raw_1000004" =>  "178",
	    "st_physics_adc_20060019_raw_6000011" =>  "174",
	    "st_physics_adc_20060045_raw_5500013" =>  "200",
	    "st_physics_adc_20060069_raw_4500002" =>  "614",
	    "st_physics_adc_20061003_raw_1000004" =>  "307",
	    "st_physics_adc_20061009_raw_2000004" =>  "357",
	    "st_physics_adc_20061018_raw_7000003" =>  "68",
	    "st_physics_adc_20061031_raw_3000003" =>  "267",
	    "st_physics_adc_20061043_raw_3000013" =>  "798",
	    "st_physics_adc_20061045_raw_4500004" =>  "748",
	    "st_physics_adc_20062006_raw_3500004" =>  "781",
	    "st_physics_adc_20062019_raw_7000004" =>  "168",
	    "st_physics_adc_20062048_raw_1500005" =>  "481",
	    "st_physics_adc_20063012_raw_5500003" =>  "79",
	    "st_physics_adc_20063029_raw_7500002" =>  "482",
	    "st_physics_adc_20063043_raw_7000003" =>  "905",
	    "st_physics_adc_20063057_raw_2000003" =>  "893",
	    "st_physics_adc_20065008_raw_1500017" =>  "163",
	    "st_physics_adc_20065061_raw_6000002" =>  "83",
	    "st_physics_adc_20066003_raw_1000004" =>  "197",
	    "st_physics_adc_20066003_raw_1500004" =>  "199",
	    "st_physics_adc_20066003_raw_2000003" =>  "203",
	    "st_physics_adc_20066003_raw_2500003" =>  "194",
	    "st_physics_adc_20066003_raw_3000003" =>  "297",
	    "st_physics_adc_20066003_raw_3500003" =>  "295",
	    "st_physics_adc_20066003_raw_4000003" =>  "292",
	    "st_physics_adc_20066003_raw_4500003" =>  "302",
	    "st_physics_adc_20066003_raw_5000004" =>  "200",
	    "st_physics_adc_20066003_raw_5500003" =>  "292",
	    "st_physics_adc_20066003_raw_6000002" =>  "297",
	    "st_physics_adc_20066003_raw_6500002" =>  "193",
	    "st_physics_adc_20066003_raw_7000002" =>  "197",
	    "st_physics_adc_20066003_raw_7500002" =>  "202",
	    "st_physics_adc_20066006_raw_4500002" =>  "267",
	    "st_physics_adc_20066025_raw_1500005" =>  "505",
	    "st_physics_adc_20066074_raw_7000003" =>  "882",
	    "st_physics_adc_20067002_raw_3500002" =>  "901",
	    "st_physics_adc_20067013_raw_3500003" =>  "481",
	    "st_physics_adc_20067039_raw_6000012" =>  "332",
	    "st_physics_adc_20068003_raw_5500003" =>  "734",
	    "st_physics_adc_20068013_raw_1500003" =>  "617",
	    "st_physics_adc_20068035_raw_1500004" =>  "446",
	    "st_physics_adc_20068056_raw_6500012" =>  "786",
	    "st_physics_adc_20068066_raw_2500003" =>  "788",
	    "st_physics_adc_20069021_raw_7000014" =>  "579",
	    "st_physics_adc_20069037_raw_5500002" =>  "948",
	    "st_physics_adc_20069061_raw_4000004" =>  "1018",
	    "st_physics_adc_20070004_raw_6500004" =>  "859",
	    "st_physics_adc_20070037_raw_1500003" =>  "403",
	    "st_physics_adc_20071011_raw_1500002" =>  "972",
	    "st_physics_adc_20071021_raw_2000002" =>  "943",
	    "st_physics_adc_20071038_raw_6500002" =>  "482",
	    "st_physics_adc_20071054_raw_3000003" =>  "1070",
	    "st_physics_adc_20072006_raw_2000003" =>  "1014",
	    "st_physics_adc_20072014_raw_7500003" =>  "435",
	    "st_physics_adc_20073003_raw_4000003" =>  "494",
	    "st_physics_adc_20073015_raw_6500003" =>  "890",
	    "st_physics_adc_20073019_raw_1500004" =>  "809",
	    "st_physics_adc_20075018_raw_1000004" =>  "1138",
	    "st_physics_adc_20075040_raw_6500005" =>  "1100",
	    "st_physics_adc_20075049_raw_7500002" =>  "1109",
	    "st_physics_adc_20075058_raw_5500004" =>  "1033",
	    "st_physics_adc_20075064_raw_7500003" =>  "1128",
	    "st_physics_adc_20076005_raw_7500011" =>  "1082",
	    "st_physics_adc_20076012_raw_1000004" =>  "808",
	    "st_physics_adc_20076022_raw_1500013" =>  "352",
	    "st_physics_adc_20076029_raw_6000003" =>  "1138",
	    "st_physics_adc_20076033_raw_3000003" =>  "487",
	    "st_physics_adc_20076046_raw_3000003" =>  "1020",
	    "st_physics_adc_20076056_raw_1500003" =>  "917",
	    "st_physics_adc_20076063_raw_7500011" =>  "568",
	    "st_physics_adc_20077019_raw_7500002" =>  "1146",
	    "st_physics_adc_20078012_raw_2500003" =>  "151",
	    "st_physics_adc_20078030_raw_1000005" =>  "506",
	    "st_physics_adc_20078048_raw_2500003" =>  "201",
	    "st_physics_adc_20078065_raw_3000003" =>  "715",
	    "st_physics_adc_20079008_raw_5000003" =>  "625",
	    "st_physics_adc_20079021_raw_1500004" =>  "992",
	    "st_physics_adc_20080003_raw_1000004" =>  "1036",
	    "st_physics_adc_20080022_raw_1000004" =>  "135",
	    "st_physics_adc_20080031_raw_1500004" =>  "1153",
	    "st_physics_adc_20081013_raw_4500003" =>  "1124",
	    "st_physics_adc_20081026_raw_0000004" =>  "170",
	    "st_physics_adc_20082003_raw_5500013" =>  "562",
	    "st_physics_adc_20082017_raw_4500003" =>  "1174",
	    "st_physics_adc_20082031_raw_3000002" =>  "819",
	    "st_physics_adc_20082048_raw_1500004" =>  "1089",
	    "st_physics_adc_20082058_raw_4500003" =>  "576",
	    "st_physics_adc_20083003_raw_3500003" =>  "814",
	    "st_physics_adc_20083026_raw_1500004" =>  "1107",
	    "st_physics_adc_20083072_raw_6500003" =>  "1003",
	    "st_physics_adc_20084004_raw_5500003" =>  "854",
	    "st_physics_adc_20084017_raw_1500015" =>  "117",
	    "st_physics_adc_20085001_raw_3500003" =>  "1136",
	    "st_physics_adc_20085010_raw_4000012" =>  "595",
	    "st_physics_adc_20085050_raw_3000003" =>  "1176",
	    "st_physics_adc_20086008_raw_1000004" =>  "990",
	    "st_physics_adc_20087005_raw_7000003" =>  "102",
	    "st_physics_adc_20087007_raw_0000005" =>  "604",
	    "st_physics_adc_20087007_raw_2000004" =>  "123",
	    "st_physics_adc_20087007_raw_2500003" =>  "120",
	    "st_physics_adc_20087007_raw_3000002" =>  "185",
	    "st_physics_adc_20087007_raw_3500002" =>  "183",
	    "st_physics_adc_20087007_raw_4000004" =>  "181",
	    "st_physics_adc_20087007_raw_4500003" =>  "183",
	    "st_physics_adc_20087007_raw_5500003" =>  "182",
	    "st_physics_adc_20087007_raw_6000002" =>  "185",
	    "st_physics_adc_20087007_raw_7500002" =>  "124",
	    "st_physics_adc_20087018_raw_3000003" =>  "936",
	    "st_physics_adc_20087022_raw_3500012" =>  "675",
	    "st_physics_adc_20088002_raw_4500003" =>  "1174",
	    "st_physics_adc_20088014_raw_1000004" =>  "386",
	    "st_physics_adc_20088034_raw_1000003" =>  "1107",
	    "st_physics_adc_20088038_raw_7500002" =>  "1157",
	    "st_physics_adc_20089013_raw_5500010" =>  "755",
	    "st_physics_adc_20089017_raw_1500003" =>  "803",
	    "st_physics_adc_20089029_raw_1500004" =>  "1107",
	    "st_physics_adc_20090007_raw_6500003" =>  "340",
	    "st_physics_adc_20090023_raw_2000003" =>  "934",
	    "st_physics_adc_20090047_raw_5000003" =>  "732",
	    "st_physics_adc_20091010_raw_7500002" =>  "1160",
	    "st_physics_adc_20092004_raw_4500003" =>  "501",
	    "st_physics_adc_20092014_raw_6500003" =>  "503",
	    "st_physics_adc_20092029_raw_5000003" =>  "507",
	    "st_physics_adc_20092037_raw_5000003" =>  "210",
	    "st_physics_adc_20093003_raw_2000003" =>  "559",
	    "st_physics_adc_20093036_raw_5000003" =>  "854"
	   );
foreach my $file (@globs) {
  #  my $b = File::Basename::basename($file",".daq"); 
  my $b = File::Basename::basename($file,".MuDst.root"); 
  print "$b\n" if ($debug);
  my $i1 =   1;
  my $Nmax =  1000;#0;# 00;
  if ($Trigger eq '19GeV') {
    $N = $Runs19GeV{$b}; print "$b = > $N\n" if ($debug);
    if ($N > $Nmax) {$N = $Nmax;}
  }
  my $step = 50;
  for (my $i = $i1; $i <= $N; $i += $step) {
    my $f = $i;
    my $l = $i + $step - 1;
    if ($l > $N) {$l = $N;}
    my $filel = $b . "_" . $f . "_" . $l;
       my $mufile = $filel . ".MuDst.root";
       if (-r $mufile) {next;}
       my $pifile = $filel . ".picoDst.root";
       if (-r $pifile) {next;}
       my $blafile = $filel . ".event.root";
       if (-r $blafile) {next;}
    print "string:$file:$f:$l\n";
    $fNo++;
  }
}
if (! $fNo) {die "Don't have input files\n";}
