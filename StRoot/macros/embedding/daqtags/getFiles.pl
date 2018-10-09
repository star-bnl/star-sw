#!/usr/bin/perl


use FileHandle;
use Getopt::Std;

getopts('p:n:t:s:g:l:c:o:kh');

if($opt_h){ help_message(); exit(); };
my $prod=$opt_p;
my $fname=$opt_n;
my $ftype=$opt_t;
my $stype=$opt_s;
my $gen=$opt_g;
my $lim=$opt_l;
my $outf=$opt_o;
my $complist=$opt_c;
my $simulate=$opt_k;

my $getCmd=qq{ get_file_list.pl -keys \"path,filename\" -delim \"/\" -cond \"};


if($prod) { $getCmd=$getCmd."production=".$prod.","; };
if($fname){ $getCmd=$getCmd."filename~".$fname.","; };
if($ftype){ $getCmd=$getCmd."filetype=".$ftype.","; };
if($stype){ $getCmd=$getCmd."storage".$stype.","; };
if($gen)  {$getCmd=$getCmd.$gen."," ; };

$getCmd=$getCmd."sanity=1\" -distinct -limit ";

if($lim){ $getCmd=$getCmd.$lim; 
	}else { $getCmd=$getCmd."0"; }
#
#
#if($outf){
#    $getCmd=$getCmd." >> ".$outf;
#}
#
#

if($simulate){ 

print $getCmd,"\n";

exit; 

}


my @result = `$getCmd`;

my @clist=();
my $k=0;
if($complist){
    open(INFILE,"< $complist");
    while(<INFILE>){
	chomp;
	$clist[$k]=$_;
        $clist[$k]=~s/\n//;
        $k++;
#    @clist=`cat $complist`;
    }
    close(INFILE);
} 



for($i=0;$i<=$#result;$i++){
   print $result[$i];#,"\n";
    if($complist){
	for($j=0;$j<=$#clist;$j++){
#            if($i==1){
#                $clist[$j]=~s/\n//;
#                $clist[$j]="st_physics_adc_".$clist[$j];
#		print $j," ",$clist[$j],"\n";
#	    }
	    if($result[$i]=~m/$clist[$j]/){
#		print $i," ",$result[$i];
                print $result[$i];
                last;
	    }
    
	}
    }
}

#
#if($complist){
#    filterData($complist,$outf);
#}


#sub filterData($complist,$outf){



sub help_message(){

    print "My Help Message\n";

}
