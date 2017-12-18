#!/usr/bin/perl

# Create index.html with a list of all files.
# Written by Ilya Zverev, licensed WTFPL.

use strict;
use POSIX qw(strftime);
use HTML::Template;
use File::Basename;
use Getopt::Long;
use Cwd;
 
my $ignore_files;# = '\.(pl)$';
my $ignore_dirs;
my $process_subfolders;
my $hide_readme; # files starting with '.' are always skipped
my $start_from = '.';
my $verbose;
my $help;
my $not_root;

my $date_fmt = '%d-%m-%Y';
my $index_file = 'index.html';
my $desc_file = '.description';
my $readme_file = 'README';
my $tmpl_file;

GetOptions('h|help' => \$help,
           'n|hide' => \$hide_readme,
           'r|recursive' => \$process_subfolders,
           'd|root=s' => \$start_from,
           'i|index=s' => \$index_file,
           't|template=s' => \$tmpl_file,
           'p|parent' => \$not_root,
           'v|verbose' => \$verbose,
           ) || usage();

if( $help ) {
  usage();
}

my $cwd = cwd();
$cwd =~ s#^(.+)[/\\]##;

my $template = <<TMPL;
<html><head>
<title>List of files in <tmpl_var path></title>
<style>body {font-family: Arial, sans-serif;} .mg {padding-left: 16px;} .r {text-align: right;} .sp {padding-bottom: 8px;} .f {margin-top: 2em; font-size: 9pt; font-style: italic;}</style>
</head>
<body>
<h1><tmpl_var path></h1>
<tmpl_if readme><div class="readme"><pre><tmpl_var readme></pre></div></tmpl_if>
<table cellspacing="2" cellpadding="0">
<tmpl_unless root>
<tr><td colspan="4" class="sp"><b><a href="../$index_file">Parent directory</a></b></td></tr>
</tmpl_unless>
<tmpl_loop dirs>
<tr><td colspan="3"><b><a href="<tmpl_var name escape=url>/$index_file"><tmpl_var name></a></b></td><td class="mg"><tmpl_var desc></td></tr>
</tmpl_loop>
<tr><td colspan="3" class="sp"></td></tr>
<tmpl_loop files>
<tr><td><a href="<tmpl_var name escape=url>"><tmpl_var name></a></td><td class="mg r"><tmpl_var size></td><td class="mg"><tmpl_var date></td><td class="mg"><tmpl_var desc></td></tr>
</tmpl_loop>
</table>
<tmpl_if nothing>
<p><i>Nothing here.</i></p>
</tmpl_if>
<p class="f">Created with <a href="https://gist.github.com/3555260">recursive_index.pl</a> on <tmpl_var date>.</p>
</body>
</html>
TMPL

my $tmpl = $tmpl_file ? HTML::Template->new(filename => $tmpl_file, die_on_bad_params => 0)
                      : HTML::Template->new(scalarref => \$template, die_on_bad_params => 0);
process_dir($start_from, $not_root ? 0 : 1);

sub process_dir {
    my ($dir, $top) = @_;
    my $dh;
    if( !opendir($dh, $dir) ) {
        warn "Failed to open directory $dir: $!";
        return;
    }
    my @contents = sort grep { !/^\./ } readdir $dh;
    closedir $dh;
    print STDERR "Processing $dir\n" if $verbose;
    
    my $presdir = $dir;
    $presdir =~ s#^\.(/.+)?#$cwd$1#;
    $tmpl->param(path => $presdir);
    $tmpl->param(root => $top);
    $tmpl->param(date => strftime($date_fmt, localtime));
    
    if( open README, "<$dir/$readme_file" ) {
        $tmpl->param(readme => do { local $/; <README> });
        close README;
    }
    
    my %desc;
    if( open DESC, "<$dir/$desc_file" ) {
        while(<DESC>) {
            chomp;
            $desc{$1} = $2 if /^([^:]+?)\s*:\s*(.+?)\s*$/;
        }
        close DESC;
    }
    
    my @dirs;
    my @files;
    for (grep { -d "$dir/$_" } @contents) {
        next if $ignore_dirs && /$ignore_dirs/o;
        push @dirs, {name => $_, desc => $desc{$_}};
    }
    for (grep { -f "$dir/$_" } @contents) {
        next if $_ eq $index_file;
        next if $hide_readme && ($_ eq $desc_file || $_ eq $readme_file);
        next if $ignore_files && /$ignore_files/o;
        my $size = -s "$dir/$_";
        if( $size >= 1024*1024*1024*20 ) { $size = (int($size/1024/1024/1024 + 0.5)).' G'; }
        elsif( $size >= 1024*1024*10 ) { $size = (int($size/1024/1024 + 0.5)).' M'; }
        elsif( $size >= 1024*10 ) { $size = (int($size/1024 + 0.5)).' K'; }
        else { $size .= ''; } # ' B'
        push @files, {
            name => $_,
            size => $size,
            date => strftime($date_fmt, localtime((stat "$dir/$_")[9])),
            desc => $desc{$_}
        };
    }
    $tmpl->param(nothing => 1) if scalar(@dirs) + scalar(@files) == 0;
    $tmpl->param(dirs => \@dirs);
    $tmpl->param(files => \@files);
    
    if( !open(IDX, ">$dir/$index_file") ) {
        warn "Cannot create $dir/$index_file: $!";
        return;
    }
    print IDX $tmpl->output();
    close IDX;
    $tmpl->clear_params();
    
    if( $process_subfolders ) {
        process_dir("$dir/$_", 0) for grep {-d "$dir/$_"} @contents;
    }
}

sub usage {
    my $prog = basename($0);
    print STDERR <<EOF;
This script generates directory index using HTML template.
usage: $prog [-h] [-d root] [-r] [-p] [-t tmpl] [-i index] [-n] [-v]
 -h       : print this help message and exit.
 -d root  : root directory to parse.
 -p       : print link to parent in root directory.
 -r       : process subfolders recursively.
 -t tmpl  : template file (default is provided).
 -i index : name of the index file (default=$index_file).
 -n       : hide README files.
 -v       : show debug messages.
README files are included in the page (HTML tags are not escaped).
The format for .description files is "file: description".
All files with names beginning with a dot are skipped.
Templating system is HTML::Template, see example in the source.
 
EOF
    exit;
}
