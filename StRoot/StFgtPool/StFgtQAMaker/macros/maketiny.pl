#!/usr/bin/perl

$dir=$ARGV[0];
print maketiny $dir;

opendir DIR,"$dir" or die;
@files = readdir DIR;
closedir DIR;

foreach $file (@files){
    if($file =~ /.png/){
	if($file =~ /.tiny./){
	}else{
	    $tfile = $file;
	    $tfile =~ s/.png/.tiny.png/g;
	    if(-e $file){}
	    else{
		$cmd = "convert $dir/$file -equalize -geometry 100x100 $dir/$tfile";
		print $cmd,"\n";
		$out = `$cmd`;
	    }
	}
    }
}
