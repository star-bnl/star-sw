#! /opt/star/bin/perl -w

# checks cvs status of files in library directories
# pmj 9/12/99

#-----------------------------------------------------------------

use File::Find;
use Tie::IxHash;

#=================================================================

# list of directories to search
@search_directories = ("StRoot", "pams");


# list of directories to exclude from error reporting
@no_error_report = ("StRoot/St_base", "StRoot/StChain", "StRoot/StIOMaker", "StRoot/StBFChain", "StRoot/St_io_Maker", "StRoot/StTreeMaker");

#-----------------------------------------------------------------
$now = localtime;

%directories  = ();
tie %directories, "Tie::IxHash";
#-----------------------------------------------------------------
# run through all directories, put array of strings into hash whose
# key is a directory name

$star = $ENV{STAR};

foreach $top_dir ( @search_directories ){
  $full_dir = "$star/$top_dir";
  find (\&scan_directories, $full_dir);
}
#-----------------------------------------------------------------

# now print has contents to file

# get filenames

$star_level = $ENV{STAR_LEVEL};
$output_string = "cvs_check.$star_level.";

($sec,$min,$hour,$mday,$mon) = localtime;
undef $sec;
$mon++;
#foreach $int ( $mon,$mday,$hour,$min ){
foreach $int ( $mon,$mday ) {
  $int < 10 and $int = '0'.$int;
  $output_string .= $int;
}

$outfile_all = "/afs/rhic.bnl.gov/star/doc/www/comp/prod/" . $output_string . "\.all\.html";
$outfile_error = "/afs/rhic.bnl.gov/star/doc/www/comp/prod/" . $output_string . "\.error\.html";

&beginAlHtml();

 print HTML "<center><h3>Time = $now\n</h3></center>";
 

#-----------------------------------------------------------------

foreach $dir (keys %directories){

  print HTML "<h4>$dir\n</h4>";

  foreach $file ( keys %{$directories{$dir}} ){

    $name = $directories{$dir}->{$file}->{name};
    $cvs_status = $directories{$dir}->{$file}->{cvs_status};
    $working_rev = $directories{$dir}->{$file}->{working_rev};
    $working_rev_date = $directories{$dir}->{$file}->{working_rev_date};
    $repo_rev = $directories{$dir}->{$file}->{repository_rev};

    print HTML "<p><font color=red> $name:</font><font size=\"-1\">status=$cvs_status; working rev=$working_rev; working rev date=$working_rev_date; repository rev=$repo_rev\n</font></p>";

  }
}

&endHtml();

&beginErHtml();

print HTML "<center><h3>Time = $now\n</h3></center>";

@no_error_report and do{

  print HTML "<h3>Excluded directories:\n<h3>";
  foreach $dir (@no_error_report){
    print HTML "<h5><font color=red> $star/$dir\n</font></h5>";
  }
};


$n_error_total = 0;

$no_error_string = join ' ',@no_error_report;

  print HTML "<h3>Directories being checked:\n<h3>"; 

foreach $dir (keys %directories){

  $n_error = 0;
 
  print HTML "<h5>$dir\n</h5>";

  foreach $file ( keys %{$directories{$dir}} ){

    $name = $directories{$dir}->{$file}->{name};
    $cvs_status = $directories{$dir}->{$file}->{cvs_status};
    $working_rev = $directories{$dir}->{$file}->{working_rev};
    $working_rev_date = $directories{$dir}->{$file}->{working_rev_date};
    $repo_rev = $directories{$dir}->{$file}->{repository_rev};


  # this directory excluded from error reporting?

    ($test_dir = $dir) =~ s/$star\///;
    $no_error_string =~ /$test_dir/ and next;

    # file not up to date or otherwise peculiar?

    (($cvs_status ne 'Up-to-date') or ($working_rev ne $repo_rev)) and do{
      $n_error or print HTML "<h4><font color=red>$dir</font>\n</h4>";
      $n_error++;
      $n_error_total++;

    print HTML "<p><font color=red>$name:</font><font size=\"-1\"> status=$cvs_status; working rev=$working_rev; working rev date=$working_rev_date; repository rev=$repo_rev\n</font></p>";

    };

  }
}

&endHtml();
##=================================================================
#defined $n_error_total or print ERROR "No delinquent files found\n";
#=================================================================

sub scan_directories{

  my $this_dir = $File::Find::dir;

  # forget about CVS directories
  $this_dir =~ /CVS$/ and return;

  # forget about StRoot/html directory (nothing committed)
  $this_dir =~ /StRoot\/html$/ and do{
    $File::Find::prune = 1;
    return;
  };

  my $this_file = $File::Find::name;

  # not a plain file? forget it.
  ! -f $this_file and return;

  # emacs garbage: forget it
  $this_file =~ /\#$/ and return;

  # other files to ignore
  $this_file =~ /\.consign/ and return;

  # get cvs status
  @output = `cvs status $this_file`;
  
  # parse output, put into hash
  foreach $line ( @output ){

    chomp $line;

    $line =~ /^=/ and next;
    $line =~ /^$/ and next;

    $line =~ /File:\s+([\w\.]+)\s+Status:\s+([\w- ]+)/ and do{
      $directories{$this_dir}->{$this_file}->{name} = $1;
      $directories{$this_dir}->{$this_file}->{cvs_status} = $2;
      next;
    };

    $line =~ /Working revision:\s+([0-9\.]+)\s+(.*)/ and do{
      $directories{$this_dir}->{$this_file}->{working_rev} = $1;
      $directories{$this_dir}->{$this_file}->{working_rev_date} = $2;
      next;
    };

    $line =~ /Working revision:\s+(.*)/ and do{
      $directories{$this_dir}->{$this_file}->{working_rev} = $1;
      $directories{$this_dir}->{$this_file}->{working_rev_date} = "none";
      next;
    };

    $line =~ /Repository revision:\s+([0-9\.]+)/ and do{
      $directories{$this_dir}->{$this_file}->{repository_rev} = $1;
      last;
    };

    $line =~ /Repository revision:\s+(.*)/ and do{
      $directories{$this_dir}->{$this_file}->{repository_rev} = $1;
      last;
    };

  }

}


##=================================================================
sub beginAlHtml {

open (HTML, ">$outfile_all") or die "Cannot open file $outfile_all";

print HTML "<html>\n";
print HTML "  <head>\n";
print HTML "          <title>CVS Status of DEV Library</title>\n";
print HTML "  </head>\n";
print HTML "  <body BGCOLOR=\"#ccffff\">\n"; 
print HTML " <center>   <h1>CVS status for all files in dev library\n</h1></center>";
print HTML "<a href=\"http://duvall.star.bnl.gov/cgi-bin/didenko/cvsQuery.pl?show=yes\"><h3>Back</h3></a>\n";

}
 
##=================================================================
sub beginErHtml {

open (HTML, ">$outfile_error") or die "Cannot open file $outfile_error";

print HTML "<html>\n";
print HTML "  <head>\n";
print HTML "          <title>CVS Errors of DEV Library</title>\n";
print HTML "  </head>\n";
print HTML "  <body BGCOLOR=\"#ccffff\">\n"; 
print HTML " <center>     <h1>CVS status for files with CVS errors\n</h1></center>";

print HTML "<a href=\"http://duvall.star.bnl.gov/cgi-bin/didenko/cvsErrQuery.pl?show=yes\"><h3>Back </h3></a>\n";
}

##====================================================================

sub endHtml {

  my $Date = `date`;

print HTML "</TABLE>\n";
print HTML "     <h5>\n";
print HTML "      <address><a href=\"mailto:didenko\@bnl.gov\">Lidia Didenko</a></address>\n";
print HTML "<!-- Created: Wed 15 Dec  05:29:25 MET 1999 -->\n";
print HTML "<!-- hhmts start -->\n";
print HTML "Last modified: $Date\n";
print HTML "<!-- hhmts end -->\n";
print HTML "  </body>\n";
print HTML "</html>\n";
close HTML;
}



