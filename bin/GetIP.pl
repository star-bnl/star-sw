#!/usr/bin/env perl
my $node = shift;# print "node = $node\n";
exit 0 if ! $node;
my $n1 = shift; $n1 = -1 if ! $n1;
my $n2 = shift; $n2 = -1 if ! $n2;
my $suffix = shift;
#print "node = $node n1 = $n1 n2 = $n2\n";
for (my $i = $n1; $i <= $n2; $i++) {
  my $Node = $node;
  if ($i >= 0) {
#    if ($i < 100) {$Node .= "0";}
    if ($i <  10) {$Node .= "0";}
    $Node .= $i . $suffix;
  } 
  my @list = `nslookup -sil $Node`;#  print "list = @list\n";
  my $get = 0;
  my $address = 0;
  foreach my $line (@list) {
    next if ! $line;
    if ($line =~ $Node) {$get = 1;}
    if ($get and $line =~ /Address/) {
      my @words = split / /, $line;# print $line;
      $address = $words[1];
      chomp($address);
    }
  }
  print "$address # $Node\n";
}
exit 0;
