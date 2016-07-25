#!/usr/bin/env perl
use File::Basename;
use Cwd;
if ($#ARGV < 0) {
  print "Usage: $0 cons_log_file
search for 'error: extra `;''
and replace the last ';'
\n";
  exit 0;
}
my $logFile = $ARGV[0];
open (IN,"$logFile") or die "Can't open $logFile";
my $oldfile = "";
my @lines = ();
while (my $line = <IN>) {
  next if $line !~ /error: extra `;'/;
  my ($file,$l,$e) = split ':', $line;
  $file =~ s|.*obj/||;
  if (! $oldfile ) { @lines = (); $oldfile = $file;}
  if ($oldfile eq $file) {push @lines, $l; next;}
  edit($oldfile,@lines);
  @lines = (); $oldfile = $file;
}
close(IN);
if ($oldfile and $#lines >= 0) {edit($oldfile,@lines);}
#________________________________________________________________________________
sub edit() {
  my ($file,@lines) = @_;
  if ($#lines < 0) {return;}
  open (in, "$file") or die "Can't open $file";
  my $output = File::Basename::basename($file);
  open (out,">$output") or die "Can't open $output";
  my $i = 0;
  my $min = 999999999;
  my $max = 0;
  my $iok = 0;
  foreach my $l (@lines) {
    if ($l < $min) {$min = $l;}
    if ($l > $max) {$max = $l;}
  }
  while (my $line = <in>) {
    $i++;
    if ($i >= $min and $i <= $max) {
      foreach  my $l (@lines) {
	if ($i eq $l) {
	  $line =~ s/;\s*$/\n/;
	}
      }
    }
    print out $line;
  }
  close(out);
  close(in);
  my $backup = $file . ".BAK";
  rename $file, $backup;
  rename $output, $file;
  print "diff -u $backup $file\n";
  system("diff -u $backup $file");
}
