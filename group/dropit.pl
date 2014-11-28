#!/usr/bin/env perl
#  $Id: dropit.pl,v 1.1.1.1 2013/08/22 01:27:48 fisyak Exp $
#  $Log: dropit.pl,v $
#  Revision 1.1.1.1  2013/08/22 01:27:48  fisyak
#  Freeze
#
#  Revision 1.4  2012/04/02 22:48:17  jeromel
#  Some indent for readability
#
#  Revision 1.3  2004/12/14 20:50:13  jeromel
#  Few more fully specified path execs
#
#  Revision 1.2  2004/11/25 23:11:09  jeromel
#  Added a csh version to dropit command and copied dropit -> dropit.csh
#
#  Revision 1.10  2003/06/24 17:27:54  jeromel
#  Restored the perl version for now ...
#
#  Revision 1.8  2001/04/13 13:32:24  jeromel
#  ROOT excluded from drop candidate.
#
#  Revision 1.7  2001/03/29 23:48:02  perev
#  HP related fixes
#
#  Revision 1.6  2000/07/14 13:07:09  fisyak
#  Try to reduce dependence on NFS
#
#  Revision 1.5  2000/06/16 18:34:04  fisyak
#  Remove nonexisting directories from path
#
#  Revision 1.4  2000/05/15 21:10:38  fisyak
#  Change /opt/star/bin to /usr/bin/env perl
#
#  Revision 1.3  2000/05/02 22:45:50  fisyak
#  Replace shell version by perl one
#
#  Revision 1.2  1998/03/26 16:41:31  fisyak
#  Add STAR_LEVELS
#
#  Revision 1.1  1998/03/23 02:29:14  fisyak
#  Fix group start-up
#
#  Revision 1.2  1998/02/22 02:07:49  fisyak
#  use ftul parameter definitions
#
#  Revision 1.1  1998/02/17 18:06:47  fisyak
#  Add dropit for PATH
#
# dropit [-i input_delimiter] [-d output_delimiter] [-p string] [substring] 
#      DESCRIPTION
#          dropit's intended purpose is to remove program library names
#          from a PATH shell variable value, which has colon separated
#          fields.  dropit is usable in sh, ksh, and csh shell script
#          and source files.
#
#          If the -i option is not specified, the input separator is a
#          colon.  If the -d option is not specified, the output
#          separator is a colon.  If the -p option is not specified,
#          the value of the PATH shell environment variable is used.
#
#          Option value meanings are:
#
#             input_delimiter
#                       input separator
#
#             output_delimiter
#                       output separator
#
#             string    the string containing colon separated fields to
#                       be operated on
#
#             substring a string which if it is a substring of a field
#                       causes the field to be omitted
#
#

my $FS=":";
my $DS=":";
my $string= $ENV{PATH};    # default value
my @string=();
my @substring=();

# parse arguments
for ( my $i=0; $i<scalar(@ARGV); $i++){
    my $test = @ARGV[$i];
    ##print $i, $test, "\n";
    if ($test =~ /^-/  && @ARGV[$i+1] =~ /^-/) {next;}    
    if ($test eq '-c') {next;}
    if ($test eq '-i') {$FS = @ARGV[++$i]; next;}      
    if ($test eq '-d') {$DS = @ARGV[++$i]; next;}     
    if ($test eq '-p') {push @string,@ARGV[++$i]; next; } 
    if ($test =~ /^-/) {next;}    
    push @substring, $test;
 }

if (scalar(@string)) {$string = join $FS,@string ;}

my $ostring = $string;
my @words = split $FS, $string;
my @newwords = ();

# remove words containing substring
foreach my $word (@words) {
    foreach my $s (@substring) {
	if ($word =~ m/$s/) {
	    goto NEXT;
	}
    }
    push @newwords, $word;
  NEXT:
}

# remove duplications
@words = ();
foreach my $word (@newwords) {# print "word = $word\n";
    foreach my $w (@words) {
	# print "word $word and w = $w\n";
	goto LAST if $w eq $word;
    }
    #  print "Check $word\n";
    #$word =~ /lsf/ || 

    ## JL 2012 - Hidden undocumented business logic in a script 
    ## aimed to perform a generic task (???)
    if ( $word =~ /scratch/ || $word =~ /ROOT/){
	push @words, $word; 
	next;
    }
    if ( (!( -d $word)) and $word !~ /^\./ ) {next;}
    
    push @words, $word;
    # print "                    add $word\n";
  LAST:
}

$string = join $DS,@words;
if ($string) {  print "$string\n";}
else         {  print "$ostring\n";}
