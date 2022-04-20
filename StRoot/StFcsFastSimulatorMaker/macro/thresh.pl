#!/usr/bin/perl
#
#  Server and Port depend on run year:
#   Year : ongoing  heston:3501
#          20xx     db04:3400+year-1

if (@ARGV != 1) {
    printf("thresh.pl run\n");
    exit(0);
}

$run = @ARGV[0];
$year = int(($run - 273000)/1000000); # October 1 rollover date

if($run > 22274000){ # This condition will change each year!
    $server = "heston.star.bnl.gov";
    $port = 3501;
}else{
    $server = "db04.star.bnl.gov";
    $port = 3400+$year-1;
}

$query = "select dictHash from run where idx_rn=$run";
$hash = `mysql -h $server --port=$port Conditions_rts -s -e "$query"`;
#printf "hash=$hash";

sub get {
    $thresh = $_[0]; 
    $query = "select distinct object from dict where hash=$hash and label='$thresh'";
    $obj   = `mysql -h $server --port=$port Conditions_rts -s -e "$query"`;
    $query = "select distinct idx from dict where hash=$hash and label='$thresh'";
    $idx   = `mysql -h $server --port=$port Conditions_rts -s -e "$query"`;
    $query = "select distinct reg from dict where hash=$hash and label='$thresh'";
    $reg   = `mysql -h $server --port=$port Conditions_rts -s -e "$query"`;
    $query = "select distinct value from dict where hash=$hash and label='$thresh'";
    $value = `mysql -h $server --port=$port Conditions_rts -s -e "$query"`;
    $query = "select distinct defaultvalue from dict where hash=$hash and label='$thresh'";
    $dfault = `mysql -h $server --port=$port Conditions_rts -s -e "$query"`;
    chomp $value; chomp $idx; chomp $obj; chomp $dfault; chomp $reg;
    $v = $value;
    if ($v eq -1) {$v = $dfault;}
    printf("%s %s %s %s %s %s \n",$obj,$idx,$reg,$thresh,$v);
}

&get("FCS_PHTTHR"); 
&get("FCS_EM-HERATIO-THR"); 
&get("FCS_HAD-HERATIO-THR"); 
&get("FCS_EMTHR2");   
&get("FCS_EMTHR1");   
&get("FCS_EMTHR0");   
&get("FCS_ELETHR2");  
&get("FCS_ELETHR1");  
&get("FCS_ELETHR0");  
&get("FCS_HADTHR2");  
&get("FCS_HADTHR1");  
&get("FCS_HADTHR0");  
&get("FCS_JPATHR2");  
&get("FCS_JPATHR1");  
&get("FCS_JPATHR0");  
&get("FCS_JPBCTHR2"); 
&get("FCS_JPBCTHR1"); 
&get("FCS_JPBCTHR0"); 
&get("FCS_JPBCTHRD"); 
&get("FCS_JPDETHR2"); 
&get("FCS_JPDETHR1"); 
&get("FCS_JPDETHR0"); 
&get("FCS_JPDETHRD"); 
&get("FCS_EHTTHR");   
&get("FCS_HHTTHR");
&get("FCS_ETOTTHR");
&get("FCS_HTOTTHR");
