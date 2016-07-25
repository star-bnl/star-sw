{
 use File::Basename;
 use Sys::Hostname;
 use Cwd;
 use File::Find();
 #__________________
 sub submit($$$$) {
   my $tag = shift;
   my $tagf = $tag . ".root";
   if (-r  $tagf) { return 0;}
   my $list = shift;
   my @lists = split ' ',$list;
   my $macro = shift;
   if ($macro eq '') {$macro = "Hadd.C";}
   my $splitjob = shift;
   if ($splitjob eq '') {$splitjob = 0;}
   $no_of_files = $#lists;
#   print "no_of_files = $no_of_files target file = $tagf from $list\n";
   my $queue = "gen_henp_atlas";
   my $id = `id`;  chomp($id);
   my $hostname = `hostname`; chomp($hostname);
   if ($hostname =~ /^acas/) {
     if ($id =~ /fisyak/) { $queue = "at_cas";}
   }
   else {
     $queue = "star_cas_short";
#     $queue = "star_cas";
       }
#   print "use queue = $queue\n";
   if ($no_of_files == 0) {
     symlink($list,$tagf);
   }
   else {
#     print "no_of_files = $no_of_files\n";
     if ($splitjob == 0) {$splitjob = $no_of_files+1;}
     my $ii = int $no_of_files/$splitjob; 
     my $nojb = $ii+1;
     for (my $jb = 1; $jb <= $nojb; $jb++) {
       $list = "";
       for (my $i = 0; $i<$splitjob; $i++) {
	 my $j = $splitjob*($jb -1) + $i;
	 if ($j > $no_of_files) {goto ENDL;}
	 $list .= " " . $lists[$j];
       }
     ENDL:
       if ($list ne "") {
	 my $tagff = $tag;
	 if ($jb > 1) {$tagff .= "_" . $jb;}
	 my $log = $tagff . ".log";
	 $tagff .= ".root";
	 if (-r $tagff) {
	   print "File: $tagff has been done\n";
	 }
	 else {
	   my $cmd = "";
#	   $cmd .= "bsub -u fisyak@bnl.gov -q  " . $queue;
	   $cmd .= " root.exe -q -b " . $list;
	   $cmd .= " '/afs/rhic/star/users/fisyak/.dev/" . $macro . "(\"";
	   $cmd .= $tagff . "\")' >& $log";
	   print "job: $jb : $cmd \n";
	   my $flag = `$cmd`;
	 }
       }
     }
   }
 }
}
1;
