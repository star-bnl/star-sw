###################################
##### Additional Cons Methods #####
###################################
use Cwd;

sub cons::ProjectFile {
    my $env = shift;
    my $dspgen = $env->{DSPGEN};
    my %opts = @_;
    my $dswfile = "#AllProjects.dsw";
    die "%DSPGEN must be set when calling ProjectFile" if !$dspgen;
    
    $opts{CONSCRIPT} = "Conscript" if !$opts{CONSCRIPT};
    my $defname = $dir::cwd->path;
    $defname =~ s,.*[/\\](.)(.*),\U$1\E$2,;
    $opts{NAME} = $defname if !$opts{NAME};
    ($opts{NAME}=$opts{DSPFILE})=~ s/.dsp//i if (!$opts{NAME} && $opts{DSPFILE});
    $opts{DSPFILE}=$opts{NAME}.".dsp" if ($opts{NAME} && !$opts{DSPFILE});
    
    my $dsptarget = $opts{DSPFILE};
    my $dsppath = $opts{DSPFILE};
    if ($dsppath !~ m,[\#\\/],) {
	# if we didn't specify any path for this,
	# then we need to make it relative to where the source dir is.
	$dsppath = "!".$dsppath;
    }
    
    # make the target also depend on the dspgen script
    $env->Depends($dsptarget,$dspgen);
    
    # make the top level dsw file depend on the dsp files.
    $env->Depends($dswfile,$dsptarget);
    
    # find the "real" location of the script.
    $dspgen = $dir::cwd->lookup($dspgen)->path;   
    my $perl = $^X;
    my $cmd = "$perl $dspgen -srcs \"".join('|',@{$opts{SRCS}})."\" ".
      "-incs \"".join('|',@{$opts{INCS}})."\" ".
	"-localincs \"".join('|',@{$opts{LOCALINCS}})."\" ".
	  "-resources \"".join('|',@{$opts{RESOURCES}})."\" ".
	    "-misc \"".join('|',@{$opts{MISC}})."\" ".
	      "-conscript \"".$opts{CONSCRIPT}."\" ".
		"-name \"".$opts{NAME}."\" ".
		  "-touch \"".$dir::cwd->lookup($dsptarget)->path."\" ".
		    "-dspfile \"".$dir::cwd->lookup($dsppath)->path."\" ".
		      "-dswfile \"".$dir::cwd->lookup($dswfile)->path."\" ".
			"-cons \"".$param::conspath."\" ";

     if ($opts{TGT}) {
	 $cmd .= "-tgt \"".$opts{TGT}."\" ";
     }
    
    $env->Command($dsptarget, $opts{CONSCRIPT}, $cmd);
}

sub cons::ResourceLibrary {
    my $env = shift;
    my %args = @_;
    my $target = file::addsuffix($args{NAME},'.dll');
    my $rc = file::addsuffix($args{RC},'.rc'); # the ".rc" file
    my $res = $rc;
    $res =~ s/\.rc$/.res/;
    my @sources = @{$args{RESOURCES}};
    
    $env = $env->clone(PDBFLAGS => '',
		       DLLFLAGS => '/dll /noentry', # this gets referenced in %LDFLAGS
		       IMPLIB => '', # no implib for resource only dlls
		       DEFINES => $env->{DEFINES}.' /D "_WINDOWS" /D "_AFXDLL"',
		      );
    $env->Depends($res, @sources);
    $env->LinkedModule($target,$rc);
    $env->Install($args{BIN},$target);
    if ($args{INCLUDE} && @{$args{INCS}}) {
	$env->Install($args{INCLUDE}."/".$args{NAME},@{$args{INCS}});
    }
    if (!$args{NOPROJECT}) {
	$env->ProjectFile(RESOURCES => [ $args{RC}, @sources ],
			  INCS      => $args{INCS},
			  LOCALINCS => $args{LOCALINCS},
			  TGT       => $args{BIN}."/$target",
			  MISC	=> $args{MISC},
			  SRCS	=> $args{SRCS},
			 );
    }
}

sub cons::SharedLibrary {
    my $env = shift;
    my %args = @_;
    my $target = file::addsuffix($args{NAME},'.dll');
    my $base = $args{NAME};
    $env->WindowsModule(SUFFIX => 'dll',
			ANCILLARY => 'pdb lib exp',
			BUILDER => "build::command::link",
			TARGET => $target,
			SRCS => $args{SRCS},
			ADDENV => [PDBFLAGS => "/Zi /Fd\"%>:d\\$base.pdb\"",
				   DLLFLAGS => '/dll', # this gets referenced in %LDFLAGS
				   IMPLIB => '/implib:%>:b.lib', # this goes into LINKCOM
				   DEFINES => $env->{DEFINES}.' /D "_WINDOWS" /D "_AFXDLL"',
				  ],
		       );
    $env->Install($args{BIN},($target, $base.".pdb"));
    $env->Install($args{LIB},($base.".lib", $base.".exp"));
    
    # make sure the dll is installed whenever the lib is.
    $env->Depends($args{LIB}."/".$base.".lib",$args{BIN}."/".$target);
    
    # make sure the pdb is installed whenever the lib is.
    $env->Depends($args{LIB}."/".$base.".lib",$args{BIN}."/".$base.".pdb");
    
    if ($args{INCLUDE} && @{$args{INCS}}) {
	$env->Install($args{INCLUDE}."/".$args{NAME},@{$args{INCS}});
    }
    if (!$args{NOPROJECT}) {
	$env->ProjectFile(SRCS      => $args{SRCS},
			  INCS      => $args{INCS},
			  LOCALINCS => $args{LOCALINCS},
			  TGT       => $args{BIN}."/$target",
			  MISC	=> $args{MISC},
			  RESOURCES	=> $args{RESOURCES},
			 );
    }
}

sub cons::MFCSharedLibrary {
    my $env = shift;
    my %args = @_;
    my $target = file::addsuffix($args{NAME},'.dll');
    my $base = $args{NAME};
    $env->WindowsModule(SUFFIX => 'dll',
			ANCILLARY => 'pdb lib exp',
			BUILDER => "build::command::link",
			TARGET => $target,
			SRCS => $args{SRCS},
			ADDENV => [PDBFLAGS => "/Zi /Fd\"%>:d\\$base.pdb\"",
				   DLLFLAGS => '/dll', # this gets referenced in %LDFLAGS
				   IMPLIB => '/implib:%>:b.lib', # this goes into LINKCOM
				   DEFINES => $env->{DEFINES}.' /D "_WINDOWS" /D "_AFXDLL" /D "_WINDLL" /D "_AFXEXT"',
				   THREADFLAGS => (($BUILDTYPE eq 'debug') ? '/MDd' : '/MD'),
				   SUBSYSTEM => "/subsystem:windows",
				  ],
		       );
    $env->Install($args{BIN},($target, $base.".pdb"));
    $env->Install($args{LIB},($base.".lib", $base.".exp"));
    
    # make sure the dll is installed whenever the lib is.
    $env->Depends($args{LIB}."/".$base.".lib",$args{BIN}."/".$target);
    
    # make sure the pdb is installed whenever the lib is.
    $env->Depends($args{LIB}."/".$base.".lib",$args{BIN}."/".$base.".pdb");
    
    if ($args{INCLUDE} && @{$args{INCS}}) {
	$env->Install($args{INCLUDE}."/".$args{NAME},@{$args{INCS}});
    }
    if (!$args{NOPROJECT}) {
	$env->ProjectFile(SRCS      => $args{SRCS},
			  INCS      => $args{INCS},
			  LOCALINCS => $args{LOCALINCS},
			  TGT       => $args{BIN}."/$target",
			  MISC	=> $args{MISC},
			  RESOURCES	=> $args{RESOURCES},
			 );
    }
}

sub cons::StaticLibrary {
    my $env = shift;
    my %args = @_;
    my $target = file::addsuffix($args{NAME},'.lib');
    my $base = $args{NAME};
    # the vtune compiler ignores the -Fd option, so no PDB file is generated for .obj files.
    $env->WindowsModule(SUFFIX => 'lib',
			ANCILLARY => (($param::compiler eq 'vtune') ? '' : 'pdb'),
			BUILDER => "build::command::library",
			TARGET => $target,
			SRCS => $args{SRCS},
			ADDENV => [
				   PDBFLAGS => "/Zi /Fd\"%>:d\\$base.pdb\"",
				   DEFINES => $env->{DEFINES}.' /D "_LIB"',
				  ],
		       );
    if ($param::compiler eq 'vtune') { # VTune doesn't create pdb files for static libs -- debug info is embedded
	$env->Install($args{LIB},($target));
    }
    else {
	# make sure the pdb is installed whenever the lib is.
	$env->Depends($args{LIB}."/".$base.".lib",$args{LIB}."/".$base.".pdb");
	$env->Install($args{LIB},($target, $base.'.pdb'));
    }
    if ($args{INCLUDE} && @{$args{INCS}}) {
	$env->Install($args{INCLUDE}."/".$args{NAME},@{$args{INCS}});
    }
    if (!$args{NOPROJECT}) {
	$env->ProjectFile(SRCS      => $args{SRCS},
			  INCS      => $args{INCS},
			  LOCALINCS => $args{LOCALINCS},
			  TGT       => $args{LIB}."/$target",
			  MISC	=> $args{MISC},
			  RESOURCES	=> $args{RESOURCES},
			 );
    }
}

sub cons::WindowsProgram {
    my $env = shift;
    my %args = @_;
    my $target = $env->_subst(file::addsuffix($args{NAME},'.exe'));
    my $base = $args{NAME};
    my $defines = $env->{DEFINES}.' /D "_WINDOWS"';

    # add in DLL define if the DLL version of MFC is desired.
    $defines .= ' /D "_AFXDLL"' if $env->{THREADFLAGS} =~ m,/MD,;

    $env->WindowsModule(SUFFIX => 'exe',
			ANCILLARY => 'pdb',
			BUILDER => "build::command::link",
			TARGET => $target,
			SRCS => $args{SRCS},
			ADDENV => [
				   PDBFLAGS => "/Zi /Fd\"%>:d\\${base}PDB.pdb\"",
				   SUBSYSTEM => "/subsystem:windows",
				   DEFINES =>  $defines,
				  ],
		       );
    $env->Install($args{BIN},($target, $base.".pdb"));
    
    # make sure the pdb is installed whenever the exe is.
    $env->Depends($args{BIN}."/".$target,$args{BIN}."/".$base.".pdb");
    
    if ($args{INCLUDE} && @{$args{INCS}}) {
	$env->Install($args{INCLUDE}."/".$args{NAME},@{$args{INCS}});
    }
    if (!$args{NOPROJECT}) {
	$env->ProjectFile(SRCS      => $args{SRCS},
			  INCS      => $args{INCS},
			  LOCALINCS => $args{LOCALINCS},
			  TGT       => $args{BIN}."/$target",
			  MISC	=> $args{MISC},
			  RESOURCES	=> $args{RESOURCES},
			 );
    }
}

sub cons::ConsoleProgram {
    my $env = shift;
    my %args = @_;
    my $target = $env->_subst(file::addsuffix($args{NAME},'.exe'));
    my $base = $args{NAME};
    $env->WindowsModule(SUFFIX => 'exe',
			ANCILLARY => 'pdb',
			BUILDER => "build::command::link",
			TARGET => $target,
			SRCS => $args{SRCS},
			ADDENV => [
				   PDBFLAGS => "/Zi /Fd%>:d\\${base}PDB.pdb",
				   SUBSYSTEM => "/subsystem:console",
				  ],
		       );
    $env->Install($args{BIN},($target, $base.".pdb"));
    
    # make sure the pdb is installed whenever the exe is.
    $env->Depends($args{BIN}."/".$target,$args{BIN}."/".$base.".pdb");
    
    if ($args{INCLUDE} && @{$args{INCS}}) {
	$env->Install($args{INCLUDE}."/".$args{NAME},@{$args{INCS}});
    }
    if (!$args{NOPROJECT}) {
	$env->ProjectFile(SRCS      => $args{SRCS},
			  INCS      => $args{INCS},
			  LOCALINCS => $args{LOCALINCS},
			  TGT       => $args{BIN}."/$target",
			  MISC	=> $args{MISC},
			  RESOURCES	=> $args{RESOURCES},
			 );
    }
}


# Like "_Object", but it is a multi-target that generates the .pch file too.
sub cons::PCHObject {
    my $env = shift;
    my $src = $dir::cwd->lookup(shift);
    my $obj = $src->{dir}->lookup($src->base . $env->{OBJMAP}{$src->suffix});
    my $pch = $src->{dir}->lookup($src->base . ".pch");
    
    my $pchopts = $env->{PCHOPTS};
    $pchopts =~ s,/Yu%PCHHEADER,,; # Get rid of "using" this PCH, although we don't need to.
    $env = $env->clone( PCHOPTS => "$pchopts /Yc%PCHHEADER" );
    $env = $env->_resolve($obj);
    
    return ($env->_Object($src,$obj));
}

sub cons::WindowsModule {
    my $env = shift;
    my %opts = @_;
    my $suffix = $opts{SUFFIX};
    my $module = file::addsuffix($opts{TARGET},$suffix);
    my @suffices = split(/\s+/,$opts{ANCILLARY});
    my @sources = ();
    my $pchsource = "";
    my @ancillary = ();
    my $base = $module;
    $base=~s/\.$suffix$//i;
    @ancillary = map("$base.$_", @suffices) if @suffices;
    
    # Separate the PCH source file from the list of sources,
    # since it gets compiled differently.
    if ($env->{PCHSOURCE}) {
	foreach (@{$opts{SRCS}}) {
	    push (@sources,$_) if ($_ !~ m/$env->{PCHSOURCE}/ || $pchsource);
	    $pchsource = $_ if (m/$env->{PCHSOURCE}/ && !$pchsource);
	} 
    }
    else {
	@sources = @{$opts{SRCS}};
    }
    
    # A multiple-target build command, since the windows
    # linker makes all kinds of files.
    my @tgts = ();
    if (@ancillary) {		# only do this if we have at least one file (the first of which should be a PDB).
	@tgts = map($dir::cwd->lookup($_), ($module, @ancillary));
	$tgts[1]->{precious} = 1; # tell it not to nuke the pdb file before "building" it.
    }
    else {
	@tgts = ($dir::cwd->lookup($module));
    }
    
    my $pchopts = $env->{PCHOPTS};
    $pchopts .= "/Fp\"%>:d\\$base.pch\" /Yu%PCHHEADER" if $pchsource;
    $env = $env->clone(@{$opts{ADDENV}},
		       PCHOPTS => $pchopts);
    
    $env = $env->_resolve($tgts[0]);
    my $builder = $opts{BUILDER};
    $builder = find $builder($env, $env->{LINKCOM});
    my $multi = build::multiple->new($builder, \@tgts);
    
    my @pchfiles = ();
    my @objfiles = $env->_Objects(map($dir::cwd->lookup($_), @sources));
    
    if ($pchsource) {
	@pchfiles = $env->PCHObject($pchsource);
	# make all the output obj files dependent upon
	# the PCH obj file, so it gets built first.
    foreach (@objfiles) {
      push(@{$_->{dep}},$pchfiles[0]);
    }
  }

  for $tgt (@tgts) {
    $tgt->bind($multi, @objfiles, @pchfiles);
  }
}

1;
