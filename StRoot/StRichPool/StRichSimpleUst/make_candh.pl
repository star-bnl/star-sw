#!/usr/bin/perl -w
use strict;

while (@ARGV) {
my @names = ();
my @types = ();
my $class = shift @ARGV;
$class =~s/\..*$//g;
open IN,"${class}.temp" or die $!;
open H,">${class}.h" or die $!;
open C,">${class}.cxx" or die $!;
while (<IN>) {
  chomp;

  my ($type,$name) = split;
  if (defined($name)) {
    $type=~s/\s+//g;
    $name =~ s/\;$//;
    $name=~s/\s+//g;
    push @names,$name;
    push @types,$type;
  }



}
close IN;
select H;
my $classname;
($classname = $class)=~s/([A-Z])/_$1/g;
$classname =~ s/^_//;
$classname = uc($classname);
print<<EOF;
\#ifndef ${classname}_HH
\#define ${classname}_HH
\#include "TObject.h"

class ${class}: public TObject {
private:
EOF
for my $i (0..$#names) {
  local $,=" ";
  local $\="\n";
  print "   ",$types[$i],$names[$i],";";
}
print <<EOF;
public:
     ${class}();
     ${class}(${class}&)\;
\~${class}(){/*noop*/}
EOF

  for my $i (0..$#names) {
    my $lname = $names[$i];
    my $type = $types[$i];
    my $uname = ucfirst($lname);
    print "    $type Get${uname}() const { return ${lname};}\n";
    print "    void Set${uname}(${type} q) {${lname} = q;}\n";
    print "\n";

  }
print <<EOF;
    ClassDef(${class},1)
};
\#endif
EOF

close H;
select C;

print <<EOF;
\#include "${class}.h"

ClassImp(${class})
${class}::${class}() {
EOF
  for my $name (@names) {
    print "    ${name} = -999;\n"; 
  }
print <<EOF;
}
${class}::${class}(${class}& in) {
EOF
  for my $name (@names) {
    print "    ${name} = in.Get\u${name}();\n"; 
  }
print "}";
close C;

}




