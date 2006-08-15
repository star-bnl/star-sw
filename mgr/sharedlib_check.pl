#! /opt/star/bin/perl -w
#
# checks cvs revisions in shared libraries
# pmj 11/12/99

#-----------------------------------------------------------------

use File::Find;
use Tie::IxHash;

#=================================================================
#$star = $ENV{STAR};
$star_level = $ENV{STAR_LEVEL};
$star_lib = $ENV{STAR_LIB};
$star_sys = $ENV{STAR_SYS};

#-----------------------------------------------------------------
$now = localtime;

%cvs_revision  = ();
tie %cvs_revision, "Tie::IxHash";
#-----------------------------------------------------------------
# run through all directories, put array of strings into hash whose
# key is a directory name

find (\&scan_directories, $star_lib);
#-----------------------------------------------------------------

# now print has contents to file

# get filename

$output_string = "$star_level.$star_sys.";

($sec,$min,$hour,$mday,$mon) = localtime;
undef $sec;
$mon++;
foreach $int ( $mon,$mday ){
  $int < 10 and $int = '0'.$int;
  $output_string .= $int;
}

$outfile = "/afs/rhic.bnl.gov/star/doc/www/comp/prod/$output_string\.html";

#print "STAR_SYS = ",$star_sys, "\n";
 
if($star_sys =~ /i386_redhat61/) {

&beginLiHtml();
 }
elsif($star_sys =~ /sun4x/)  {

 &beginSlHtml();
 }


 print HTML "<center><h3>Time = $now</h3></center>\n";

#-----------------------------------------------------------------

foreach $file (keys %cvs_revision ){

  print HTML "<h4>$file\n</h4>";

  foreach $name ( keys %{$cvs_revision{$file}} ){

    $rev = $cvs_revision{$file}->{$name}->{revision};
    $date_time = $cvs_revision{$file}->{$name}->{date_time};
    
    print HTML "<p><font color=red>$name:</font> <font size=\"-1\">revision=$rev; date/time = $date_time</font>\n</p>";

  }
}
&endHtml();

#=================================================================
sub scan_directories{

  my $this_file = $File::Find::name;

  # not of type .so? forget it.
  $this_file !~ /\.so$/ and return;

  print "Working on file $this_file \n";

  # get Id strings
  @output = `strings $this_file | grep Id`;
  
  # parse output, put into hash
  foreach $line ( @output ){

    chomp $line;

    $line =~ /\\s+([\w\.,]+)\s+([\d\.]+)\s+(.*)Exp/ and do{
      $rev = $2;
      $date_time = $3;
      ($name = $1) =~ s/,v$//;

      $cvs_revision{$this_file}->{$name}->{revision} = $rev;
      $cvs_revision{$this_file}->{$name}->{date_time} = $date_time;
      next;
    };
  }

}

##===================================================================
sub beginLiHtml {

open (HTML, ">$outfile") or die "Cannot open file $outfile";

print HTML "<html>\n";
print HTML "  <head>\n";
print HTML "          <title>CVS revision content of dev library</title>\n";
print HTML "  </head>\n";
print HTML "  <body BGCOLOR=\"#ccffff\">\n"; 
print HTML " <center> <h1>CVS Revision Content for Shared Libraries in DEV on RedHat61</h1></center>\n";
print HTML "<a href=\"http://duvall.star.bnl.gov/cgi-bin/didenko/cvsShLinux.pl?show=yes\"><h3>Back</h3></a>\n";
print HTML "<p>";
}

##=============================================================================
sub beginSlHtml {

open (HTML, ">$outfile") or die "Cannot open file $outfile";

print HTML "<html>\n";
print HTML "  <head>\n";
print HTML "          <title>CVS revision content of dev library</title>\n";
print HTML "  </head>\n";
print HTML "  <body BGCOLOR=\"#ccffff\">\n"; 
print HTML " <center> <h1>CVS Revision Content for Shared Libraries in DEV on Solaris</h1></center>\n";
print HTML "<a href=\"http://duvall.star.bnl.gov/cgi-bin/didenko/cvsShSolaris.pl?show=yes\"><h3>Back</h3></a>\n";
print HTML "<p>";
}

##=====================================================================

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
