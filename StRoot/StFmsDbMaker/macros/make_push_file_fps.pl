#!/usr/bin/perl

#Purpose of this script is to generate a push_fpsPedSV2017 file so that when you run it adds all the relevant values to the database

use strict;
use warnings;

my $file_push = "push_fpsPedSV2017";
open( my $fh, '>', $file_push ) or die "Could not open file '$file_push' for writing: $!\n";

my $file_dir="fps_good_physics_ped/";
opendir my $dh, $file_dir or die "Could not open '$file_dir' for reading: $!\n";

while( my $file_name = readdir $dh )
{
    if( $file_name eq "." or $file_name eq ".."){next;}
    my $command = "root4star -b -q ";
    my $program = "fpsPedSV2017_db.C";
    my $options = "'(\"writedb\",\"\",\"${file_dir}${file_name}\")'";
    print $fh $command, $program, $options,"\n";
}

closedir $dh;
close $fh;

