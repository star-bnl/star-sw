#!/opt/star/bin/perl -w
#MoveJob.pl

#use strict;
use File::Copy;

my $workdir = "/afs/rhic.bnl.gov/star/doc/www/html/tmp/csh/";

my $target;
my $source;
my $workfile;
my $ctime;     #list creation time
my $i;         #file num
my $j;         #move str num
my %mvstr;     #move strings
my %mvfiles;   #move file names
my @wf;        #work files
my $extended=0;

my($SELF)="MoveJob";

&scanfiles();

#&print_menu();
print 
    "\nType '?' for help.\n\n",
    "$SELF> ";
chomp($actn = <STDIN>);

while( $actn ne "q"){  
    $actn =~ s/\s+/ /g; 
    # we will allow commands separated by ";"
    @items = split(";",$actn);

    foreach $actn (@items){
	@nums = split(" ",$actn);

      ACTIONS:
	if( defined($nums[0]) ){
	    if( ($nums[0] eq "l") or ($nums[0] eq "list")){
		#show the list of files with contents
		&scanfiles();

	    } elsif( (($nums[0] eq "d") or ($nums[0] eq "del")) and (defined($fnum = $nums[1])) ){
		#delete command
		if( $#nums >= 2 ){
		    $i = 2;
		    while( defined($nums[$i]) ){
			&del_str("$fnum"." "."$nums[$i++]");
		    }
		} elsif( defined($mvfiles{$fnum}) ){
		    #delete entire file cmnd
		    &question("delete file : $mvfiles{$fnum} ? [yes] : ");
		    if( ($actn eq "yes\n") or ($actn eq "\n") ){
			if ( unlink("$mvfiles{$fnum}") ){
			    print "deleted $mvfiles{$fnum}\n";
			} else {
			    print "Failed to delete $mvfiles{$fnum}\n";
			}
		    }
		}
    
	    } elsif( (($nums[0] eq "m") or ($nums[0] eq "move")) and defined($fnum = $nums[1]) ){
		#move command
		if( $#nums >= 2 ){
		    #move selected cmnd
		    $i = 2;
		    while( defined($nums[$i]) ){
			&move_str("$fnum"." "."$nums[$i++]","one");
		    }
		} elsif( defined($mvfiles{$fnum}) ){
		    # move entire file cmnd
		    &question("move $mvfiles{$fnum} ? [yes] : ");
		    if( ($actn eq "yes\n") or ($actn eq "\n") ){
			@all = keys %mvstr;
			if ( $#all == -1){
			    print "Please, use eXtend first, then move\n";
			} else {
			    foreach $key (keys(%mvstr)){
				@nums = split(" ",$key);
				if( $nums[0] == $fnum ){
				    &move_str("$fnum"." "."$nums[1]","all");
				}
			    }
			    # Auto-delete the file
			    $nums[0] = "d";
			    $nums[1] = $fnum;
			    goto ACTIONS;
			}
		    }
		}    

	    } elsif( ($nums[0] eq "?") or ($nums[0] eq "help") or ($nums[0] eq "h") ){
		# show help
		&print_menu();

	    } elsif( ($nums[0] eq "ex") or ($nums[0] eq "e") ){
		# show example
		print
		    "\nUsing commands example:\n\n",
		    "$SELF> move 2           move ALL jobs from file 2\n",
		    "$SELF> move 2 2         move from file 2 ONLY job 2\n",
		    "$SELF> move 2 1 2 3     move from file 2 jobs 1, 2 AND 3\n",
		    "$SELF> del 2            delete(unlink) file 2\n",
		    "$SELF> del 2 2          delete from file 2 ONLY job 2\n",
		    "$SELF> del 2 1 2 3      delete from file 2 jobs 1, 2 AND 3\n";

	    } elsif( ($nums[0] eq "xtend") or ($nums[0] eq "x") ){
		$extended = ! $extended;
		print "eXtended display is now ".($extended?"ON":"OFF")."\n";
	    }
	    
	}
    }
    print "\n$SELF> ";
    chomp($actn = <STDIN>);
}

#subs
#=======================================
sub question {
    my($q) = @_;

    do {
	print "$q";
	$actn = <STDIN>;
    } while( ($actn !~ /no/i ) and ($actn ne "\n") and ($actn !~ /yes/i) );

}     
    

#=======================================
sub del_str {
    my($key) = @_;
    my $fnum;
    my @nums;
    if( defined($mvstr{$key}) ){
	@nums = split(" ",$key);
	$fnum = $nums[0];
	&question("delete $mvstr{$key} ? [yes] : ");
	if( ($actn eq "\n") or ($actn eq "yes\n") ){
	    print
		"deleted $mvstr{$key}\n";
	    delete $mvstr{$key};
	    open(TF,">$mvfiles{$fnum}") or warn "cannot open file : $!\n";
	    foreach $key (keys(%mvstr)){
		@nums = split(" ",$key);
		if( $nums[0]==$fnum ){
		    print TF "$mvstr{$key}\n";
		}
	    }
	    close(TF);
	}
    }
    
}
#========================================
sub move_str{
    my ($key,$status) = @_;
    if( defined($mvstr{$key}) ){
	@str = split(" ",$mvstr{$key});
	if( $status eq "one" ){
	    &question("move $str[0] to $str[1] ? [yes] : ");
	}	
	if( ($status eq "all") or ($actn eq "\n") or ($actn eq "yes\n") ){
	    if( copy($str[0], $str[1]) ){
		print "copied $str[0] to $str[1]\n";
	    } else {
		warn  "cannot copy file : $!\n";
	    }
	    
	}
    }
}
#========================================
sub print_menu{
    print 
	"\n$SELF commands:\n",
	"?         (h)       Display this text\n",
	"help      (h)       Synonim for '?'\n",
	"list      (l)       Show list of files\n",
	"move X    (m X)     Move all jobs from file X\n",
	"move X Y  (m X Y)   Move job Y from file X\n",
	"del X     (d X)     Delete file X\n",
	"del X Y   (d X Y)   Delete Y job from file X\n",    
	"xtend     (x)       eXtended display (list files to be moved)\n",
	"ex        (e)       Example\n",
	"command1 ; command2 ; command3 ... for multiple commands\n",
	"quit      (q)       Quit $SELF\n\n";
}
#========================================
sub scanfiles{
    print
	"\n";
    $i = 0;
    undef(@wf);
    undef(%mvstr);
    undef(%mvfiles);
    my ($arch_dir,$match);

    @wf = glob("$workdir*.ml");
    foreach $workfile ( @wf ){  
	if( open(FI,$workfile) ){ 
	    $mvfiles{$i} = $workfile;
	    $workfile =~ /.*\/(\d*)\.ml/;
	    $ctime = localtime($1);
	    print 
		$i." : $workfile      $ctime\n";
	    $j=0;       

	    if ( $extended ){
		while( defined($str=<FI>) ){   
		    undef(@str);     
		    @str = split(" ","$str");
		    $target = $str[1];
		    if( $str[0] =~ /(.*archive\/)(.*st_physics.*)/ ){
			$arch_dir = $1;
			$match = $2;
		    } elsif( $str[0] =~ /(.*archive\/)(.*st_zerobias.*)/ ){
			$arch_dir = $1;
			$match = $2;
		    } elsif( $str[0] =~ /(.*archive\/)(.*st_express.*)/ ){
			$arch_dir = $1;
			$match = $2;
		    } elsif( $str[0] =~ /(.*archive\/)(.*st_fast.*)/ ){
			$arch_dir = $1;
			$match = $2;
		    }

		    if( -e $target && defined($match) ){
			@all = glob($arch_dir.$match);
			if ($#all != -1){
			    $source = $all[0];
			    if( -e $source ){
				chomp($source);
				$movestr = $source."   ".$target; 
				$mvstr{"$i"." "."$j"} = $movestr;
				print "\t$i"." $j"." $movestr\n";
				$j++;
			    }
			}
		    } else {
			print "\t$target not found or pattern is not st_physics for $str[0]\n";}
		    }	
	    }
	} else {
	    warn "cannot open $workfile : $!\n";
	}
	$i++;
    }    
}
