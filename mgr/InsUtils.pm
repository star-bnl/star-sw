
#
# This simple module was added to regroup a bunch of utility
# routines for the Insure++ build. 
# Put there WebStyles, routines or anything which would be
# common to several scripts (so we don't have to re-write
# it and we would have everything centralized)
#

package InsUtils;
require 5.000;
use Exporter;

@ISA = (Exporter);
@EXPORT=qw(IUbody IUcmt IUhead IUtrail IUGetRef IUl2pre IUresource);


# variables subject to export. So far, we will be using
# functions to return their value
# HTML body attributes
$INSU::BODYATTR="bgcolor=cornsilk text=black link=navy vlink=maroon alink=tomato";
# HTML font declaration
$INSU::FONT="<basefont face=\"verdana,arial,helvetica,sans-serif\">";



# Internal variables only
$PRGM="InsUtils ::";
$AUTHOR="Jerome LAURET";
$EMAIL="jlauret\@bnl.gov";
$VERSION="V01-000";


# Although I am forseing only the use if INSUhead, flexibility
# of 2 extra routines added.
sub IUbody {    
    "<BODY $INSU::BODYATTR>\n$INSU::FONT\n";
}
sub IUcmt {
    my($tmp);
    $tmp  = "<!-- Listing auto-generated on ".localtime()." -->\n";
    $tmp .= "<!-- $PRGM version $VERSION -->\n";
    $tmp .= "<!-- Script written by $AUTHOR $EMAIL  -->\n";
}
# Both head and trail may later include templates
# or style sheets etc ...
sub IUhead
{
    my($title)=@_;
    my($tmp);
    $tmp  = "<HTML>\n<HEAD><TITLE>$title</TITLE></HEAD>\n";
    $tmp .= &IUbody().&IUcmt();
    $tmp .= "<H1 ALIGN=\"center\">$title</H1>\n";
    $tmp .= "<H4 ALIGN=\"center\">Formatted on ".localtime()."</H4>\n";
    $tmp;
}
sub IUtrail
{
    "</BODY>\n</HTML>\n";  
}


# Format line to something suitable to a pre tag.
# Some character needs HTML escaping.
sub IUl2pre
{
    my($line)=@_;
    $line =~ s/&/&amp;/g;
    $line =~ s/</&lt;/g;
    $line =~ s/>/&gt;/g;
    $line;
}

# This routine converts a line into a string suitable
# for NAME and HREF relative document reference
sub IUGetRef
{
    my($line)=@_;
    $line =~ s/[\.\[\]:\(\)]/_/g;
    $line =~ s/\s//g;
    $line;
}


# Creates an Insure++ .psrc file in the current directory
# Argument is the file to redirect the output to.
# Arg2 : message to display.
sub IUresource
{
    my($ofile,$mess)=@_;

    if(! -e ".psrc"){
	open(FO,">.psrc") || 
	    die "$PRGM Could not open any file in W mode in the current tree\n";
	print FO qq~
insure++.summarize bugs
insure++.symbol_table off
insure++.stack_internal on
insure++.leak_combine none
insure++.leak_search off
insure++.leak_sort size
insure++.leak_sweep off
insure++.leak_trace off
insure++.summarize leaks outstanding
insure++.report_file $ofile
    ~;
	close(FO); 
	print "$mess\n";
    }
}





