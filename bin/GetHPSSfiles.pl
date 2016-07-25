#!/usr/bin/perl -w
#
# setenv PERL5LIB /afs/usatlas.bnl.gov/project/magda/current:/afs/usatlas.bnl.gov/opt/i386_redhat72/lib/perl5/site_perl:/afs/usatlas.bnl.gov/opt/i386_redhat72/lib/perl5/vendor/5.6.1
# setenv PATH /afs/usatlas.bnl.gov/opt/i386_redhat72/bin:$PATH
for(my $i=5601; $i<=6000; $i++) {
    my $filename = "dc1.002001.lumi10.0".$i.".hlt.pythia_jet_25.zebra";
    print "$filename\n";;
    next if -r $filename;
    my $findfile = `magda_findfile $filename --location=usatlasrftp`; 
    print  "$findfile\n";
    if( $findfile =~ m/^LFN:\/\/atlas.org\/$filename/g ) {
	   print "magda_getfile $filename\n";
	   my $magdagetfile = `magda_getfile $filename --local`;
	   print "$magdagetfile\n";
       }
}


exit 0;
