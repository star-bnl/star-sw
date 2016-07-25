#!/usr/bin/env perl
use CPAN;
#autobundle, clean, install, make, recompile, test
#    perl -MCPAN::FirstTime -e 'CPAN::FirstTime::init("MyConfig.pm")'
#    perl -MCPAN -e 'CPAN::Shell->install("Bundle::DBI")'
#    perl -MCPAN -e 'CPAN::Shell->install("Bundle::DBD::mysql")'
# install everything that is outdated on my disk:
#perl -MCPAN -e 'CPAN::Shell->install(CPAN::Shell->r)'
  
## install my favorite programs if necessary: #Net::FTP MD5 Data::Dumper)){
my @Modules = qw(Bundle::libnet
		 MD5
		 Bundle::CPAN
		 Bundle::DBI
		 Bundle::DBD::mysql
		 Bundle::Apache
		 Proc::ProcessTable
		 Mail::Mailer
		 File::PathConvert
		 XML::Parser
		 Time::HiRes
		 Heap
		 Date::Parse
		 Date::Calc
		 PDL
#		 GD d
#		 GIFgraph
#		 Graph
);
for $mod (@Modules){
  CPAN::Shell->install($mod);
#  my $obj = CPAN::Shell->expand('Module',$mod);
#  $obj->install;
}

## list all modules on my disk that have no VERSION number
#for $mod (CPAN::Shell->expand("Module","/./")){
#  next unless $mod->inst_file;
#  # MakeMaker convention for undefined $VERSION:
#  next unless $mod->inst_version eq "undef";
#  print "No VERSION in ", $mod->id, "\n";
#}

# find out which distribution on CPAN contains a module:
#print CPAN::Shell->expand("Module","Apache::Constants")->cpan_file
