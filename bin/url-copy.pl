#! /usr/bin/env perl
my @ListOfRanges = qw( 
		      201 202 203 204 205 206 207 208 209 210 
		      212 213 214 215 216 217 218 219 220 
		      221 222 223 224 225 226 227 228 229 
		      232 234 237 241 244 246 247 249 250 
		      251 252 253 254 255 256 257 258 259 
		      260 261 262 263 264 265 266 267 268 269 270 
		      271 272 273 274 275 276 277 278 279 280 
		      281 282 283 284 285 286 287 288 289 290 
		      291 292 293 294 295 296 297 298 299 300 
		      301 302 303 304 305 306 307 308 309 310 
		      311 312 313 314 315 316 317 318 319 320 
		      321 322 323 324 325 326 327 328 329 330 
		      331 332 333 334 335 336 337 343 344 346 349 350 
		      354 355 356 357 358 
		      363 364 368 
		      376 
		      381 384 385 388 
		      391 392 394 395 397 400 
		      401 402 403 404 405 406 407 408 409 410 
		      411 413 414 415 417 418 419 420 
		      421 422 423 424 428 429 439 440 
		      441 444 445 446 447 448 449 450 
		      480 
		      486 
		      492 496 499 
		      502 
		      520 
		      521 522 523 524 525 526 527 528 529 530 
		      531 532 533 534 535 536 538 539 540 
		      543 544 545 548 549 550 
		      551 552 553 554 555 556 557 558 559 560 
		      561 562 563 564 565 566 567 568 569 570 
		      571 572 573 574 575 576 577 597 600 
		      601 603 604 605 606 
		      612 632 639 680 
		      681 682 683 684 685 686 697 698 699 700 
		      701 702 703 
		      739 740 
		      742 743 744 745 746 750 
		      751 752 753 754 755 757 
		      764 765 766 767 768 769 
		      773 778 779 780 
		      781 782 
		      803 804 805 806 807 808 809 810 
		      811 812 813 814 815 816 817 818 819 820 
		      821 822 823 824 825 826 827 828 829 830 
		      831 832 833 834 835 836 837 838 839 840 
		      841 842 843 844 845 846 847 848 849 850 
		      851 852 853 854 855 856 857 858 859 860 
		      861 862 863 864 865 866 867 868 869 870 
		      871 872 873 874 875 876 877 878 879 880 
		      881 882 883 884 885 886 887 888 889 890 
		      891 892 893 894 895 896 897 898 899 900 
		      901 902 903);
my $from = "gsiftp://bh1.uits.indiana.edu//N/ivdgl/atlas_scratch_space/fisyak/reconx.";
my $to   = "gsiftp://spider.usatlas.bnl.gov/";
my $mydist = "/usatlas/projects/fisyak/2002.lumi02/data";#"/usatlas/data04/fisyak/chimera";
my $dist = "/usatlas/data04/fisyak/Production/6.0.4/ntuple";
my @exts = qw(hbook log);# histo.hbook);
foreach my $range (@ListOfRanges) {
  my ($i1,$i2) = split '-', $range; 
  if ($range !~ /-/) {$i1 = $range; $i2 = $i1;} 
  print "i1 = $i1 and i2 = $i2\n";
  for (my $i = $i1; $i <= $i2; $i++) {
    my $partition = $i;
    if    ($partition <   10) {$partition = "000" . $partition;}
    elsif ($partition <  100) {$partition = "00"  . $partition;}
    elsif ($partition < 1000) {$partition = "0"   . $partition;}
    print "partition = $partition\n";
    foreach my $ext (@exts) {
      my $file = "dc1.002002.lumi02.recon.010._" . 
	$partition . ".hlt.pythia_jet_55." .  
	  $ext;
      my $in  = $from . $partition . "/" . $file;
      my $File = $dist . "/" . $file;
      if (-r $File) {print "File: $File exists\n"; last;}
      $File = $mydist . "/" . $file;
      if (-r $File) {print "File: $File exists\n"; next;}
      my $out = $to                 . $File;
      my $cmd = "globus-url-copy " . $in . " " . $out;
      print "$cmd\n";
      system($cmd);
    }
  }
}			  
