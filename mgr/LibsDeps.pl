#!/usr/bin/env perl
use strict;
use File::Basename;
if ($#ARGV < 0) {
  print "Usage $0 list_of_libraries\n";
  print '$0 $STAR_LIB/*.so $ROOTSYS/lib/*.so $OPTSTAR/lib/libPythia6.so $OPTSTAR/lib/liblog4cxx.so /usr/lib/libmysqlclient.so',"\n";
  exit 0;
}
my @root_exe_libs = qw(
		       libCore.so
		       libCint.so
		       libHist.so
		       libGraf.so
		       libGraf3d.so
		       libGpad.so
		       libTree.so
		       libMatrix.so
		       libRint.so
		      ); # list of libs already in root.exe
my $debug = 0;
my $root_exe_libs = join '|', @root_exe_libs; print "ignore dependencies from $root_exe_libs\n" if $debug;
my @LoH = ();
my $newtarget = 0;
my $tag;
my $dummy;
my $LastLevel = 0;
foreach my $Lib (@ARGV) { 
  my $lib = File::Basename::basename($Lib);
  print "lib = $lib\n" if $debug;
  next if $lib eq 'libSt_base.so';
#  my $cmd = "nm  --demangle --extern-only  --undefined-only " . $Lib;
#  my @listU = `$cmd`;
#  print "listU = $#listU ==>\n" if $debug > 1;
#  $cmd = "nm  --demangle --extern-only  --defined-only " . $Lib;
#  my @listD = `$cmd`;
#  print "listD = $#listD ==>\n" if $debug > 1;
#  my $code = getSymbolsList(@listD);
#  my $miss = getSymbolsList(@listU);
  my $cmd = "nm  --demangle --extern-only " . $Lib;
  my @list = `$cmd`;
  my ($code,$miss) = (); 
  foreach my $line (@list) {
    next if $line =~ /__gnu_cxx|GLIBC|GCC|ROOT| __bss_|__cxa_| __dynamic|__cxxabiv1|std\:\:operator/;
    $line =~ s/typeinfo for|vtable for //;
    my @words = split ' ', $line;
    if ($debug > 2) {
      my $i = 0;
      foreach my $w (@words) {print "$i $w\n"; $i++;}
    }
    next if $words[0] ne 'U' and $words[1] ne 'T' and $words[1] ne 'W' and $words[1] ne 'V' and $words[1] ne 'D';
    shift @words if $words[0] ne 'U';
    my $symbol = $words[1]; print "$symbol =>" if $debug > 2;
    $symbol =~ s/:.*//;     print "$symbol\n"  if $debug > 2;
    $symbol =~ s/\(.*//;    print "$symbol\n"  if $debug > 2;
    $symbol =~ s/\**$//;    print "$symbol\n"  if $debug > 2;
    next if $symbol =~ /^(void|int|unsigned|float|double|char|std)/;
    next if $symbol eq '_fini';
    next if $symbol =~ /^G__/;
    next if $symbol =~ /typeinfo/;
    next if $symbol =~ /vtable/;
    next if $symbol =~ /^_/;
    next if $symbol =~ /operator/;
    next if $symbol =~ /^BF_/ or $symbol =~ /^BIO_/ or $symbol =~ /^ERR_/;
    next if $symbol =~ /^RSA_/ or $symbol =~ /^RAND_/ or $symbol =~ /^PEM_/ or $symbol eq 'G';
    next if $symbol =~ /^dladdr/;
    next if $symbol =~ /gufld_$/;
    next if ! $symbol;
    print $line if $debug > 1;
    if ($words[0] eq 'U' or $words[0] eq 'W') {$miss = add($miss,$symbol); next;}
    if ($words[0] eq 'T' or $words[0] eq 'D' or $words[0] eq 'V') {$code = add($code,$symbol); next;} #$words[0] eq 'V' or
#   if ($words[0] eq 'D') {$data = add($data,$symbol); next;}
  }
  # clean up
  my @miss = split '\|', $miss;
  my @code = split '\|', $code;
  my @Miss = ();
  
  foreach my $m  (@miss) {
    foreach my $c (@code) {
      if ($m eq $c) {goto NEXT;}
    }
    push @Miss, $m;
  NEXT:
  }
  my $rec = {};
  $rec->{lib}  = $lib;
  $rec->{code} = $code;
#  $rec->{data} = $data;
  $rec->{miss} = join '|', @Miss;
  $rec->{deps} = "";
#  @($rec->{ref})  = ();
  push @LoH, $rec;
}
if ($debug) {
  foreach my $rec (@LoH) {
    print "$rec->{lib} \n=> code |$rec->{code}| \n\n=> miss  |$rec->{miss}|\n\n=>deps |$rec->{deps}|\n";
  }
}
if ($debug) {print "LoH = "; foreach  my $rec (@LoH) {print "\t$rec->{lib}";} print "\n";}
my @L = sort compMiss @LoH;
if ($debug) {print "L = "; foreach  my $rec (@L) {print "\t$rec->{lib}";} print "\n";}
my %H = ();
foreach my $rec (@L) {
  my $lib =  $rec->{lib};
  next if $lib =~ '^St' and $lib =~ '^lib';
  my @miss = split '\|', $rec->{miss};
  foreach my $slave (@LoH) {
    next if $rec eq $slave;
    next if ! $slave->{code};
    foreach my $miss (@miss) {
      next if ! $miss;
      my $match = "";
      my @Miss = ();
      foreach my $src (split '\|', $slave->{code}) {
	if ($src eq $miss) {$match = $src;} 
	else {push @Miss, $src;}
      }
      next if ! $match;
      
      $rec->{deps} = add($rec->{deps},$slave->{lib});
#      print "match found $match \tAdd deps: $rec->{deps}\n" if $debug;
      print "match found $match for $rec->{lib} in $slave->{lib}\n" if $debug;
    }
  }
# clean up trivial libs
  my @deps = split '\|', $rec->{deps};
  my @Deps = ();
  foreach my $d (@deps) {
    next if $root_exe_libs =~ /$d/;
    push @Deps, $d;
  }
  my $Deps = join '|', @Deps;
  $rec->{deps} = $Deps;
  #
  #  if ($debug) {
  print "$rec->{lib}:";
  my @deps = split '\|', $rec->{deps};
  foreach my $d (@deps) {
    next if $root_exe_libs =~ /$d/;
    print "\t$d";
  }
  print "\n";
  #}
  $H{$rec->{lib}} = $rec;
}
##print "============================= sort ======================\n" if $debug;
##my @L = sort comp @LoH;
##foreach my $rec (@L) {
##  if ($debug) {
##    print "$rec->{lib} ===> $H{$rec->{lib}}->{deps}\n";
##    print "$rec->{lib} ===>";
##    my @deps = split '\|', $rec->{deps};
##    foreach my $d (@deps) {print "\t$d";}
##    print "\n";
##  }
##  my $Deps = deps($rec);
##  my @Deps = split '\|', $Deps;
##  print "$rec->{lib} ";
##  foreach my $d (@Deps) {print "\t$d";}
##  print "\n";
##}
#________________________________________________________________________________
sub getSymbolsList() {
  my $SymbolsList = "";
  foreach my $line (@_) {
    next if $line =~ /__gnu_cxx|GLIBC|GCC|ROOT| __bss_|__cxa_| __dynamic|__cxxabiv1|std\:\:operator/;
    $line =~ s/typeinfo for|vtable for //;
    my @words = split ' ', $line;
    if ($debug > 2) {
      my $i = 0;
      foreach my $w (@words) {print "$i $w\n"; $i++;}
    }
    shift @words if $words[0] ne 'U';
    my $symbol = $words[1]; print "$symbol =>" if $debug > 2;
    $symbol =~ s/:.*//;     print "$symbol\n"  if $debug > 2;
    $symbol =~ s/\(.*//;    print "$symbol\n"  if $debug > 2;
    $symbol =~ s/\**$//;    print "$symbol\n"  if $debug > 2;
    next if $symbol =~ /^(void|int|unsigned|float|double|char|std)/;
    next if $symbol eq '_fini';
    next if $symbol =~ /^G__/;
    next if $symbol =~ /typeinfo/;
    next if $symbol =~ /vtable/;
    next if $symbol =~ /^_/;
    next if $symbol =~ /operator/;
    next if $symbol =~ /^BF_/ or $symbol =~ /^BIO_/ or $symbol =~ /^ERR_/;
    next if $symbol =~ /^RSA_/ or $symbol =~ /^RAND_/ or $symbol =~ /^PEM_/ or $symbol eq 'G';
    next if $symbol =~ /^dladdr/;
    next if $symbol =~ /gufld_$/;
    next if ! $symbol;
    print $line if $debug > 1;
    $SymbolsList =  add($SymbolsList,$symbol)
  }
  return $SymbolsList;
}
#________________________________________________________________________________
sub deps($) {
  my $rec = $_[0];
  my $Deps =  $rec->{deps}; print "$rec->{lib} \tDeps = $Deps\n" if $debug;
  my @src = split '\|', $Deps;
  if ($#src <=  0) {return $Deps;}
  $Deps = "";
  foreach my $s (@src) {
    next if ! $s;
    my $ref = $H{$s};
    die if ! $ref;
#    $debug = 2;
    my $D = deps($ref);    print "$ref->{lib} \t$D\n" if $debug;
    if ($D) {
      $Deps = add($Deps,$D); print "$ref->{lib} \ts = $s \tDeps = $Deps\n" if $debug;
    }
    $Deps = add($Deps,$s); print "$ref->{lib} \ts = $s \tDeps = $Deps\n" if $debug;
#    $debug = 0;
  }
  return $Deps;
}
#________________________________________________________________________________
sub comp($$) {
  my ($ref1,$ref2) = @_;
#  if (! $ref1->{deps} and ! $ref2->{deps}) {return    0;}
#  if (  $ref1->{deps} and ! $ref2->{deps}) {return    1;}
#  if (! $ref1->{deps} and   $ref2->{deps}) {return   -1;}
#  if (  $ref1->{deps} =~ /$ref2->{lib}/)   {return   -1;}
#  if (  $ref2->{deps} =~ /$ref1->{lib}/)   {return    1;}
#  return 0;
  my @src1 = split '\|', $ref1->{deps};
  my @src2 = split '\|', $ref2->{deps};
  return $#src1 <=> $#src2;
}
#________________________________________________________________________________
sub compMiss($$) {
  my ($ref1,$ref2) = @_;
  my @src1 = split '\|', $ref1->{miss};
  my @src2 = split '\|', $ref2->{miss};
  return $#src2 <=> $#src1;
}
#________________________________________________________________________________
sub add($$) {
  my ($list,$item) = @_; print "$list + $item =>" if $debug > 2;
  if ($item) {
    if (! $list ) {$list = $item;}
    else {
      my @src = split '\|', $list;
      foreach my $s (@src) {if ($s eq $item) {return $list;}      }
      $list .= "|"; $list .= $item;
      print " $list\n" if $debug > 2;
    }
  }
  return $list;
}
