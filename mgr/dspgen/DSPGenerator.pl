# -*- Perl -*-

# Copyright (C) 1999, Presenter.com
# This program is placed in the public domain, under the
# "Artistic License".  See the Open Source document
# http://www.opensource.org/licenses/artistic-license.html
# for more info.

# This program allows us to generate a "makefile-only" DSP file that
# contains all of the source files used in a project, as well as the
# Conscript file.  It is used by Cons to keep the DSP file up-to-date
# wrt to Conscript file.
package DSPGenerator;

sub new { 
    my $self = {};
    my $type = shift;
    my @tmp = @_;
    bless $self;
    $self->init(@tmp);
    return $self;
}

# initializer
sub init {
    my $self = shift;

    # we take either a reference to a hash, or
    # a list of key => value pairs
    if (ref($_[0]) eq "HASH") {
      %{$self} = %{$_[0]};
    }
    else {
      %{$self} = @_;
    }


    # if no name was given, derive one from the dspfile
    if ($self->{DSPFILE} && !$self->{NAME}) {
      ($self->{NAME} = $self->{DSPFILE}) =~ s/\.dsp$//i;
    }
    elsif (!$self->{NAME}) {
      $self->{NAME} = "Unnamed";
    }
    # name the dsp file from the name if none was given.
    $self->{DSPFILE} = $self->{NAME}.".dsp" if !$self->{DSPFILE};

    $self->{CONSCRIPT} = "Conscript" if !$self->{CONSCRIPT};
    $self->{TGT} = $self->{NAME} if (!$self->{TGT});
}

sub PrintHeader {
  my $self = shift;
  my $name = $self->{NAME};
  
  print OUTFILE "# Microsoft Developer Studio Project File - Name=\"$name\" - Package Owner=<4>\n";
  print OUTFILE "# Microsoft Developer Studio Generated Build File, Format Version 6.00\n";
  print OUTFILE "# ** DO NOT EDIT **\n";
  print OUTFILE "\n";
  print OUTFILE "# TARGTYPE \"Win32 (x86) External Target\" 0x0106\n";
  print OUTFILE "\n";
  print OUTFILE "CFG=$name - Win32 Debug\n";
  print OUTFILE "!MESSAGE This is not a valid makefile. To build this project using NMAKE,\n";
  print OUTFILE "!MESSAGE use the Export Makefile command and run\n";
  print OUTFILE "!MESSAGE \n";
  print OUTFILE "!MESSAGE NMAKE /f \"$name.mak\".\n";
  print OUTFILE "!MESSAGE \n";
  print OUTFILE "!MESSAGE You can specify a configuration when running NMAKE\n";
  print OUTFILE "!MESSAGE by defining the macro CFG on the command line. For example:\n";
  print OUTFILE "!MESSAGE \n";
  print OUTFILE "!MESSAGE NMAKE /f \"$name.mak\" CFG=\"$name - Win32 Debug\"\n";
  print OUTFILE "!MESSAGE \n";
  print OUTFILE "!MESSAGE Possible choices for configuration are:\n";
  print OUTFILE "!MESSAGE \n";
  print OUTFILE "!MESSAGE \"$name - Win32 Release\" (based on \"Win32 (x86) External Target\")\n";
  print OUTFILE "!MESSAGE \"$name - Win32 Debug\" (based on \"Win32 (x86) External Target\")\n"; 
  print OUTFILE "!MESSAGE \n\n";
}

sub PrintProject {
  my $self = shift;
  my $name = $self->{NAME};
  print OUTFILE "# Begin Project\n";
  print OUTFILE "# PROP AllowPerConfigDependencies 0\n";
  print OUTFILE "# PROP Scc_ProjName \"\"\n";
  print OUTFILE "# PROP Scc_LocalPath \"\"\n";
  print OUTFILE "\n";
  
  foreach ("Release", "Debug") {
    if ($_ eq "Release") {
      print OUTFILE "!IF  \"\$(CFG)\" == \"$name - Win32 $_\"\n\n";
      $self->{TGT} =~ s,[/\\]debug[/\\],/release/,ig; #fixup path
    }
    else {
      print OUTFILE "!ELSEIF  \"\$(CFG)\" == \"$name - Win32 $_\"\n\n";
      $self->{TGT} =~ s,[/\\]release[/\\],/debug/,ig; #fixup path
    }
    $self->{TGT} =~ s,\\,/,g;
    my $outdir = $self->{TGT};
    $outdir =~ s,[\\/][^\\/]+$,,; # get base path
    print OUTFILE "# PROP BASE Use_MFC 0\n";
    print OUTFILE "# PROP BASE Use_Debug_Libraries ".($_ eq "Debug" ? 1 : 0)."\n";
    print OUTFILE "# PROP BASE Output_Dir \"$outdir\"\n";
    print OUTFILE "# PROP BASE Intermediate_Dir \"$outdir\"\n";
    if ($self->{TGT} eq $self->{NAME}) {
      print OUTFILE "# PROP BASE Cmd_Line \"perl ".$self->{CONSPATH}." -t -- ".($_ eq "Debug" ? "-debug" : "-release")." \"\n";
    }
    else {
      print OUTFILE "# PROP BASE Cmd_Line \"perl ".$self->{CONSPATH}." -t -- -msdev ".$self->{TGT}."\"\n";
    }
    print OUTFILE "# PROP BASE Rebuild_Opt \"-all\"\n";
    print OUTFILE "# PROP BASE Target_File \"".$self->{TGT}."\"\n";
    print OUTFILE "# PROP BASE Bsc_Name \"\"\n";
    print OUTFILE "# PROP BASE Target_Dir \"\"\n";
    
    print OUTFILE "# PROP Use_MFC 0\n";
    print OUTFILE "# PROP Use_Debug_Libraries ".($_ eq "Debug" ? 1 : 0)."\n";
    print OUTFILE "# PROP Output_Dir \"$outdir\"\n";
    print OUTFILE "# PROP Intermediate_Dir \"$outdir\"\n";
    if ($self->{TGT} eq $self->{NAME}) {
      print OUTFILE "# PROP Cmd_Line \"perl ".$self->{CONSPATH}." -t -- ".($_ eq "Debug" ? "-debug" : "-release")." \"\n";
    }
    else {
      print OUTFILE "# PROP Cmd_Line \"perl ".$self->{CONSPATH}." -t -- -msdev ".$self->{TGT}."\"\n";
    }
    print OUTFILE "# PROP Rebuild_Opt \"-all\"\n";
    print OUTFILE "# PROP Target_File \"".$self->{TGT}."\"\n";
    print OUTFILE "# PROP Bsc_Name \"\"\n";
    print OUTFILE "# PROP Target_Dir \"\"\n\n";
  }
  print OUTFILE "!ENDIF\n\n";
  print OUTFILE "# Begin Target\n\n";
  print OUTFILE "# Name \"$name - Win32 Release\"\n";
  print OUTFILE "# Name \"$name - Win32 Debug\"\n\n";
  print OUTFILE "!IF  \"\$(CFG)\" == \"$name - Win32 Release\"\n\n";
  print OUTFILE "!ELSEIF  \"\$(CFG)\" == \"$name - Win32 Debug\"\n\n";
  print OUTFILE "!ENDIF \n\n";
  $self->PrintSourceFiles();
  print OUTFILE "# End Target\n";
  print OUTFILE "# End Project\n";
  
}

sub PrintSourceFiles {
  my $self = shift;
  my %files = ();
  my %categories = (
		    " Source Files" => "cpp|c|cxx|l|y|def|odl|idl|hpj|bat",
		    "Header Files" => "h|hpp|hxx|hm|inl",
		    "Local Headers" => "h|hpp|hxx|hm|inl",
		    "Resource Files" => "r|rc|ico|cur|bmp|dlg|rc2|rct|bin|cnt|rtf|gif|jpg|jpeg|jpe",
		    "Other Files" => "",
		   );
  
  $files{" Source Files"} = \@{$self->{SRCS}};
  $files{"Header Files"} = \@{$self->{INCS}};
  $files{"Local Headers"} = \@{$self->{LOCALINCS}};
  $files{"Resource Files"} = \@{$self->{RESOURCES}};
  $files{"Other Files"} = \@{$self->{MISC}};

  my @others = ($self->{CONSTRUCT},@{$self->{OTHERFILES}});

  foreach $type (sort keys %categories) {
    next if !@{$files{$type}}; # skip empty groups
    print OUTFILE "# Begin Group \"$type\"\n\n";
    my $typelist = $categories{$type};
    $typelist =~ s/\|/;/g;
    print OUTFILE "# PROP Default_Filter \"$typelist\"\n";
    my $file;
    
    foreach $file (@{$files{$type}}) {
      $file =~ s|/|\\|g;	# convert slashes
      
      print OUTFILE "# Begin Source File\n\n";
      print OUTFILE "SOURCE=\"$file\"\n";
      print OUTFILE "# End Source File\n";
    }
    print OUTFILE "# End Group\n";    
  }

  # add the Conscript file outside of the groups
  print OUTFILE "# Begin Source File\n\n";
  print OUTFILE "SOURCE=".$self->{CONSCRIPT}."\n";
  print OUTFILE "# End Source File\n";
}

sub Build {
  my $self = shift;
  open (OUTFILE, ">".$self->{DSPFILE}) || die "Unable to write to DSP file '".$self->{DSPFILE}."': $!\n";
  $self->PrintHeader();
  $self->PrintProject();
  close (OUTFILE);

  open (OUTFILE, ">".$self->{TOUCH}) || die "Unable to touch DSP file '".$self->{TOUCH}."': $!\n";
  print OUTFILE "Look in ".$self->{DSPFILE}." for the real dsp file.\n";
  close (OUTFILE);
}

package DSWGenerator;

sub new { 
    my $self = {};
    my $type = shift;
    my @tmp = @_;
    bless $self;
    $self->init(@tmp);
    return $self;
}

# initializer
sub init {
    my $self = shift;

    # we take either a reference to a hash, or
    # a list of key => value pairs
    if (ref($_[0]) eq "HASH") {
      %{$self} = %{$_[0]};
    }
    else {
      %{$self} = @_;
    }

    # if no name was given, derive one from the dspfile
    if ($self->{DSPFILE} && !$self->{NAME}) {
      ($self->{NAME} = $self->{DSPFILE}) =~ s/\.dsp$//i;
    }
    elsif (!$self->{NAME}) {
      $self->{NAME} = "Unnamed";
    }
    # name the dsp file from the name if none was given.
    $self->{DSPFILE} = $self->{NAME}.".dsp" if !$self->{DSPFILE};
}

sub Parse {
  my $self = shift;
  my $dsw = $self->{DSWFILE};

  return if (!-f $dsw);

  $self->{db} = {};

  open(DSW, "<$dsw") || die "$0: Unable to read $dsw: $!\n";
  while(<DSW>) {
    next if !m/^Project:/;
    m/"(.*)"=(.*) - .*/;
    my $name = $1;
    my $path = $2;
    my $dsppath = $path;

    # We eliminate cruft by checking to see if there is a Conscript
    # file in the directory pointed at by the DSP file name.  If not,
    # then we assume that the directory has been blown away, or isn't
    # a Cons controlled directory, and therefore isn't to be included
    # in the top level DSW file.
    $dsppath =~ s,[/\\][^/\\]+\.dsp$,,i;
    if (-f "$dsppath/Conscript") {
      $self->{db}->{$name} = $path;
    }
  }
  close DSW;
}

sub Emit {
  my $self = shift;
  my $dsw = $self->{DSWFILE};
  
  open(DSW, ">$dsw") || die "$0: Unable to write $dsw: $!\n";
  # print the header.
  print DSW "Microsoft Developer Studio Workspace File, Format Version 6.00\n";
  print DSW "# WARNING: DO NOT EDIT OR DELETE THIS WORKSPACE FILE!\n";
  print DSW "\n";
  print DSW "###############################################################################\n";

  # print the db
  foreach $name (sort keys %{$self->{db}}) {
    print DSW "\n";
    print DSW "Project: \"".$name."\"=".$self->{db}->{$name}." - Package Owner=<4>\n\n";
    print DSW "Package=<5>\n";
    print DSW "{{{\n";
    print DSW "}}}\n\n";
    print DSW "Package=<4>\n";
    print DSW "{{{\n";
    print DSW "}}}\n\n";
    print DSW "###############################################################################\n";
  }

  # print the trailer
  print DSW "\nGlobal:\n\n";
  print DSW "Package=<5>\n";
  print DSW "{{{\n";
  print DSW "}}}\n\n";
  print DSW "Package=<3>\n";
  print DSW "{{{\n";
  print DSW "}}}\n\n";
  print DSW "###############################################################################\n";

  close DSW;
}

sub Build {
  my $self = shift;

  # load the existing dsw file, if any.
  $self->Parse();

  # insert the current project
  $self->{db}->{$self->{NAME}} = $self->{DSPFILE};
  
  # write it out again.
  $self->Emit();
}

package main;

while (@ARGV) {
  my $flag = shift @ARGV;
  my $value = shift @ARGV;
  if ($flag eq "-srcs") {
    @SRCS = split(/\|/,$value);
  }
  elsif ($flag eq "-incs") {
    @INCS = split(/\|/,$value);
  }
  elsif ($flag eq "-localincs") {
    @LOCALINCS = split(/\|/,$value);
  }
  elsif ($flag eq "-resources") {
    @RESOURCES = split(/\|/,$value);
  }
  elsif ($flag eq "-misc") {
    @MISC = split(/\|/,$value);
  }
  elsif ($flag eq "-tgt") {
    $TGT = $value;
  }
  elsif ($flag eq "-conscript") {
    $CONSCRIPT = $value;
  }
  elsif ($flag eq "-name") {
    $NAME = $value;
  }
  elsif ($flag eq "-dspfile") {
    $DSPFILE = $value;
  }
  elsif ($flag eq "-dswfile") {
    $DSWFILE = $value;
  }
  elsif ($flag eq "-cons") {
    $CONSPATH = $value;
  }
  elsif ($flag eq "-touch") {
    $TOUCH = $value;
  }
  else {
    die "$0: Error parsing command line!\n";
  }
}

%hash = ( SRCS => \@SRCS,
	  INCS => \@INCS,
	  LOCALINCS => \@LOCALINCS,
	  RESOURCES => \@RESOURCES,
	  MISC => \@MISC,
	  CONSCRIPT => $CONSCRIPT,
	  NAME => $NAME,
	  TOUCH => $TOUCH,
	  DSPFILE => $DSPFILE,
	  CONSPATH => $CONSPATH,
	  DSWFILE => $DSWFILE,
	);

$hash{TGT} = $TGT if $TGT;

my $dsp = new DSPGenerator( \%hash );
$dsp->Build();
my $dsw = new DSWGenerator( \%hash );
$dsw->Build();

exit(0);
