#!/usr/bin/env perl
# http://drupal.star.bnl.gov/STAR/comp/db/onlinedb/online-sever-port-map/
# Run/Year	       NODE	Port
my @Runs = (
    'Run1'        => {node => 'dbbak.starp.bnl.gov', port => '3400'},
    'Run2' 	  => {node => 'dbbak.starp.bnl.gov', port => '3401'},
    'Run3' 	  => {node => 'dbbak.starp.bnl.gov', port => '3402'},
    'Run4' 	  => {node => 'dbbak.starp.bnl.gov', port => '3403'},
    'Run5' 	  => {node => 'dbbak.starp.bnl.gov', port => '3404'},
    'Run6' 	  => {node => 'dbbak.starp.bnl.gov', port => '3405'},
    'Run7' 	  => {node => 'dbbak.starp.bnl.gov', port => '3406'},
    'Run8' 	  => {node => 'dbbak.starp.bnl.gov', port => '3407'},
    'Run9' 	  => {node => 'dbbak.starp.bnl.gov', port => '3408'},
    'CURRENT'     => {node => 'onldb.starp.bnl.gov', port => '3501'}
	   );
my $Run = 'CURRENT'; print "$#ARGV : #ARGV\n";
if ($#ARGV >= 0) {
  $Run = $ARGV[0];
}
my $def = {@Runs};
print "Run = $Run; node = $def->{$Run}->{node}; port = $def->{$Run}->{port}\n";
my $cmd = "mysql -h $def->{$Run}->{node}  -P $def->{$Run}->{port}";
$cmd .= " -e "; 

#analysis --> K0
#default_format --> L0
#asic_seq_lo --> ALTRO_seq
#asic_thr_lo --> ALTRO_thr
#time_bin_lo --> K1
#time_bin_hi --> L1
#clust_charge_lo --> K2
#cl_write --> L2

$cmd .= "'select asic_thr_lo as ALTRO_thr,asic_seq_lo as ALTRO_seq,analysis as K1, time_bin_lo as K2, clust_charge_lo as K3, default_format as L1, time_bin_hi as L2, cl_write as L3, min(a.beginTIme),min(a.idx_rn),max(a.idx_rn),b.glb_setup_name from Conditions_rts.dets a, Conditions_rts.run b where a.idx_det=20 and a.idx_rn=b.idx_rn and b.glb_setup_name like \"%roduction%\" group by asic_seq_hi,asic_seq_lo,asic_thr_hi,asic_thr_lo,glb_setup_name order by min(a.idx_rn)'";
print "$cmd\n";
$flag = system($cmd);
