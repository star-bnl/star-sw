#!/usr/bin/perl

while(<>) {
    next if ( ! /ClassDef/ );
    s/^\s+ClassDef\(//;
    s/\)[\s\S]+$//;
    ($class,$ver) = split /,/ ;
    if( $ver > 0 ) { 
	@classes = ( @classes, $class );
    }
}



#print "// Instructions to the ROOTCINT dictionary generator.\n";
print "#pragma link off all globals;\n"  ;
print "#pragma link off all classes;\n"  ;
print "#pragma link off all functions;\n";
#print "\n\n";
print "#ifdef __CINT__\n";
#print "// list of classes:\n";
@sorted = sort @classes;
foreach $class ( @sorted ) {
    print "#pragma link C++ class " . $class . "+;\n";
}
#print "\n\n";
print "#endif\n";
