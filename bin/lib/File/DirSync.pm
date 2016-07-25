package File::DirSync;

use strict;
use Exporter;
use Fcntl qw(O_CREAT O_RDONLY O_WRONLY O_EXCL);
use Carp qw(croak);

use vars qw( $VERSION @ISA $PROC );
$VERSION = '1.22';
@ISA = qw(Exporter);
$PROC = join " ", $0, @ARGV;

# Whether or not symlinks are supported
use constant HAS_SYMLINKS => ($^O !~ /Win32/i) || 0;

# Wallclock percent spent sleeping
use constant GENTLE_PERCENT_DEFAULT => 50;
use constant GENTLE_PERCENT_MIN     =>  0;
use constant GENTLE_PERCENT_MAX     => 99;

# Disk Operations to run without sleeping
use constant GENTLE_OPS_DEFAULT     => 1_000;
use constant GENTLE_OPS_MIN         => 10;
use constant GENTLE_OPS_MAX         => 20_000_000;

# Automatically increase maxops by GENTLE_CHEWINCFACTOR
# whenever realtime spent syncing is under GENTLE_CHEWMINTIME
# or sleeptime is under GENTLE_SLEEPMINTIME.
use constant GENTLE_CHEWINCFACTOR   => 0.25;
use constant GENTLE_CHEWMINTIME     => 2;
use constant GENTLE_SLEEPMINTIME    => 2;
use constant GENTLE_SLEEPMAXTIME    => 600;

# Number of bytes that can read and write to and from a local file in one syscall
use constant BUFSIZE => 8192;

# Number of bytes written to consider as one "disk op"
use constant BLKSIZE => 1024;

# Number of ops to be considered for each iteration during a file copy.
# (This counts both the read and write ops for the buffer.)
use constant BUFFER_OPS => BUFSIZE / BLKSIZE * 2;

sub new {
  my $class = shift;
  my $self = shift || {};
  $self->{only} ||= [];
  $| = 1 if $self->{verbose};
  bless $self, $class;
  return $self;
}

sub proctitle {
  my $self = shift;
  $self->{proctitle} ||= shift || $0;
  return $self->{proctitle};
}

sub gentle {
  my $self = shift;

  $self->{_gentle_percent} = shift || GENTLE_PERCENT_DEFAULT;
  $self->{_gentle_percent} = GENTLE_PERCENT_MIN if $self->{_gentle_percent} < GENTLE_PERCENT_MIN;
  $self->{_gentle_percent} = GENTLE_PERCENT_MAX if $self->{_gentle_percent} > GENTLE_PERCENT_MAX;

  $self->{_gentle_maxops} = shift || GENTLE_OPS_DEFAULT;
  $self->{_gentle_maxops} = GENTLE_OPS_MIN if $self->{_gentle_maxops} < GENTLE_OPS_MIN;
  $self->{_gentle_maxops} = GENTLE_OPS_MAX if $self->{_gentle_maxops} > GENTLE_OPS_MAX;

  $self->{_gentle_started} = time;
  $self->{_gentle_ops} = 0;

  return $self->{_gentle_percent};
}

sub _op {
  my $self = shift;
  if (($self->{_gentle_ops} += (shift || 1) ) >= $self->{_gentle_maxops}) {
    # Reached maximum operations
    my $elapsed = time - $self->{_gentle_started};
    my $delay = int ($elapsed / (100/$self->{_gentle_percent} - 1)) || 1;
    if ($self->{_gentle_maxops} < GENTLE_OPS_MAX and
        $elapsed < GENTLE_CHEWMINTIME ||
        $delay < GENTLE_SLEEPMINTIME) {
      $self->{_gentle_maxops} += int ($self->{_gentle_maxops} * GENTLE_CHEWINCFACTOR);
      $self->{_gentle_maxops} = GENTLE_OPS_MAX if $self->{_gentle_maxops} > GENTLE_OPS_MAX;
    } elsif ($delay > GENTLE_SLEEPMAXTIME) {
      $self->{_gentle_maxops} -= int ($self->{_gentle_maxops} * GENTLE_CHEWINCFACTOR);
      $delay = GENTLE_SLEEPMAXTIME;
    }
    my $prevproc = $0;
    $0 = "$self->{proctitle} - [$self->{_gentle_percent}% gentle on $self->{_gentle_maxops} ops]: SLEEPING $delay UNTIL: ".scalar(localtime (time() + $delay)) if $self->{proctitle};
    sleep $delay;
    $0 = $prevproc;
    $self->{_gentle_started} = time;
    $self->{_gentle_ops} = 0;
  }
  return 1;
}

sub rebuild {
  my $self = shift;
  my $dir = shift || $self->{src};

  croak 'Source directory must be specified: $obj->rebuild($directory) or define $obj->src($directory)'
    unless defined $dir;

  # Remove trailing / if accidently supplied
  $dir =~ s%/$%%;
  -d $dir or
    croak 'Source must be a directory';

  if (@{ $self->{only} }) {
    foreach my $only (@{ $self->{only} }) {
      if ($only =~ /^$dir/) {
        $self->_rebuild( $only );
      } else {
        croak "$only is not a subdirectory of $dir";
      }
      local $self->{localmode} = 1;
      while ($only =~ s%/[^/]*$%% && $only =~ /^$dir/) {
        $self->_rebuild( $only );
      }
    }
  } else {
    $self->_rebuild( $dir );
  }
  $0 = $PROC if $self->{proctitle};
  print "Rebuild cache complete.\n" if $self->{verbose};
}

sub _rebuild {
  my $self = shift;
  my $dir = shift;

  # Hack to snab a scoped file handle.
  my $handle = do { local *FH; };
  $dir = $1 if $dir =~ m%^(.*)$%;
  $self->_op if $self->{_gentle_percent};
  return unless opendir($handle, $dir);
  $0 = "$self->{proctitle} - rebuild: $dir" if $self->{proctitle};
  $self->_op if $self->{_gentle_percent};
  my $current = (lstat $dir)[9];
  my $most_current = $current;
  my $node;
  my $skew = $self->{maxskew};
  if (defined $skew) {
    $skew += time;
    if ($current > $skew) {
      $most_current = $current = $skew;
    }
  }
  $self->_op if $self->{_gentle_percent};
  while (defined ($node = readdir($handle))) {
    next if $node =~ /^\.\.?$/;
    next if $self->{ignore}->{$node};
    my $path = "$dir/$node";
    # Recurse into directories to make sure they
    # are updated before comparing time stamps
    $self->_op if $self->{_gentle_percent};
    !$self->{localmode} && !-l $path && -d _ && $self->_rebuild( $path );
    my $this_stamp = (lstat $path)[9];
    next if -l _;
    if (defined $skew) {
      $self->_op if $self->{_gentle_percent};
      if ($this_stamp > $skew and !-l $path) {
        print "Clock skew detected [$path] ".($this_stamp-$skew)." seconds in the future? Repairing...\n" if $self->{verbose};
        utime($skew, $skew, $path);
        $this_stamp = $skew;
      }
    }
    if ($this_stamp > $most_current) {
      print "Found a newer node [$path]\n" if $self->{verbose};
      $most_current = $this_stamp;
    }
  }
  closedir($handle);
  if ($most_current > $current) {
    print "Adjusting [$dir]...\n" if $self->{verbose};
    $most_current = $1 if $most_current =~ /^(\d+)$/;
    $self->_op if $self->{_gentle_percent};
    utime($most_current, $most_current, $dir);
  }
  return;
}

sub tracking {
  my $self = shift;
  if (@_) {
    if (shift) {
      $self->{_tracking} = {
        removed => [],
        updated => [],
        skipped => [],
        failed  => [],
      };
    } else {
      delete $self->{_tracking};
    }
  }
  return ($self->{_tracking} ? 1 : 0);
}

sub dirsync {
  my $self = shift;
  my $src = shift || $self->{src};
  my $dst = shift || $self->{dst};
  croak 'Source and destination directories must be specified: $obj->dirsync($source_directory, $destination_directory) or specify $obj->to($source_directory) and $obj->src($destination_directory)'
    unless (defined $src) && (defined $dst);

  # Remove trailing / if accidently supplied
  $src =~ s%/$%%;
  -d $src or
    croak 'Source must be a directory';
  # Remove trailing / if accidently supplied
  $dst =~ s%/$%%;
  my $upper_dst = $dst;
  $upper_dst =~ s%/?[^/]+$%%;
  if ($upper_dst && !-d $upper_dst) {
    croak "Destination root [$upper_dst] must exist: Aborting dirsync";
  }
  $self->_dirsync( $src, $dst );
  $0 = $PROC if $self->{proctitle};
  return;
}

sub _dirsync {
  my $self = shift;
  my $src = shift;
  my $dst = shift;

  $self->_op(2) if $self->{_gentle_percent};
  my $when_dst = (lstat $dst)[9];
  my $size_dst = -s _;
  my @stat_src = lstat $src;
  my $when_src = $stat_src[9];
  my $size_src = $stat_src[7];

  if (HAS_SYMLINKS) {
    # Symlink Check must be first because
    # I could not figure out how to preserve
    # timestamps (without root privileges).
    if (-l _) {
      # Source is a symlink
      $self->_op(2) if $self->{_gentle_percent};
      my $point = readlink($src);
      if (-l $dst) {
        # Dest is a symlink, too
        if ($point eq (readlink $dst)) {
          # Symlinks match, nothing to do.
          $self->_op if $self->{_gentle_percent};
          return;
        }
        # Remove incorrect symlink
        print "$dst: Removing symlink\n" if $self->{verbose};
        unlink $dst or warn "$dst: Failed to remove symlink: $!\n";
        $self->_op(2) if $self->{_gentle_percent};
      }
      if (-d $dst) {
        # Wipe directory
        print "$dst: Removing tree\n" if $self->{verbose};
        $self->rmtree($dst) or warn "$dst: Failed to rmtree!\n";
      } elsif (-e $dst) {
        # Regular file (or something else) needs to go
        print "$dst: Removing\n" if $self->{verbose};
        unlink $dst or warn "$dst: Failed to purge: $!\n";
      }
      if (-l $dst || -e $dst) {
        warn "$dst: Still exists after wipe?!!!\n";
      }
      $point = $1 if $point =~ /^(.+)$/; # Taint
      # Point to the same place that $src points to
      print "$dst -> $point\n" if $self->{verbose};
      symlink $point, $dst or warn "$dst: Failed to create symlink: $!\n";
      $self->_op(5) if $self->{_gentle_percent};
      return;
    }
  }

  if ($self->{nocache} && -d _) {
    $size_dst = -1;
  }
  # Short circuit and kick out the common case:
  # Nothing to do if the timestamp and size match
  if ( defined ( $when_src && $when_dst && $size_src && $size_dst) &&
       $when_src == $when_dst && $size_src == $size_dst ) {
    push @{ $self->{_tracking}->{skipped} }, $dst if $self->{_tracking};
    return;
  }

  # Regular File Check
  if (-f _) {
    # Source is a plain file
    if (-l $dst) {
      # Dest is a symlink
      print "$dst: Removing symlink\n" if $self->{verbose};
      unlink $dst or warn "$dst: Failed to remove symlink: $!\n";
      $self->_op if $self->{_gentle_percent};
    } elsif (-d _) {
      # Wipe directory
      print "$dst: Removing tree\n" if $self->{verbose};
      $self->rmtree($dst) or warn "$dst: Failed to rmtree: $!\n";
    }
    $self->_op if $self->{_gentle_percent};
    $0 = "$self->{proctitle} - copying: $src => $dst" if $self->{proctitle};
    if ($self->copy($src, $dst)) {
      print "$dst: Updated\n" if $self->{verbose};
      push @{ $self->{_tracking}->{updated} }, $dst if $self->{_tracking};
    } else {
      warn "$dst: Failed to copy: $!\n";
    }
    if (!-e $dst) {
      warn "$dst: Never created?!!!\n";
      push @{ $self->{_tracking}->{failed} }, $dst if $self->{_tracking};
      $self->_op if $self->{_gentle_percent};
      return;
    }
    # Force permissions to match the source
    chmod( $stat_src[2] & 0777, $dst) or warn "$dst: Failed to chmod: $!\n";
    # Force user and group ownership to match the source
    chown( $stat_src[4], $stat_src[5], $dst) or warn "$dst: Failed to chown: $!\n";
    # Force timestamp to match the source.
    utime $when_src, $when_src, $dst or warn "$dst: Failed to utime: $!\n";
    $self->_op(4) if $self->{_gentle_percent};
    return;
  }

  # Missing Check
  if (!-e _) {
    # The source does not exist
    # The destination must also not exist
    print "$dst: Removing\n" if $self->{verbose};
    $0 = "$self->{proctitle} - removing: $dst" if $self->{proctitle};
    if ( $self->rmtree($dst) ) {
      push @{ $self->{_tracking}->{removed} }, $dst if $self->{_tracking};
    } else {
      push @{ $self->{_tracking}->{failed} }, $dst if $self->{_tracking};
      warn "$dst: Failed to rmtree!\n";
    }
    return;
  }

  # Finally, the recursive Directory Check
  if (-d _) {
    # Source is a directory
    if (-l $dst) {
      # Dest is a symlink
      print "$dst: Removing symlink\n" if $self->{verbose};
      unlink $dst or warn "$dst: Failed to remove symlink: $!\n";
      $self->_op if $self->{_gentle_percent};
    }
    if (-f $dst) {
      # Dest is a plain file
      # It must be wiped
      print "$dst: Removing file\n" if $self->{verbose};
      if ( unlink($dst) ) {
        push @{ $self->{_tracking}->{removed} }, $dst if $self->{_tracking};
      } else {
        push @{ $self->{_tracking}->{failed} }, $dst if $self->{_tracking};
        warn "$dst: Failed to unlink file: $!\n";
      }
      $self->_op if $self->{_gentle_percent};
    }
    if (!-d $dst) {
      if ( mkdir $dst, 0755 ) {
        push @{ $self->{_tracking}->{updated} }, $dst if $self->{_tracking};
      } else {
        push @{ $self->{_tracking}->{failed} }, $dst if $self->{_tracking};
        warn "$dst: Failed to mkdir: $!\n";
      }
      $self->_op if $self->{_gentle_percent};
    }
    -d $dst or warn "$dst: Destination directory cannot exist?\n";
    $self->_op(4) if $self->{_gentle_percent};

    # If nocache() was not specified, then it is okay
    # skip this directory if the timestamps match.
    if (!$self->{nocache}) {
      # (The directory sizes do not really matter.)
      # If the timestamps are the same, nothing to do
      # because rebuild() will ensure that the directory
      # timestamp is the most recent within its
      # entire descent.
      if ( defined ( $when_src && $when_dst) &&
           $when_src == $when_dst ) {
        push @{ $self->{_tracking}->{skipped} }, $dst if $self->{_tracking};
        return;
      }
    }

    print "$dst: Scanning...\n" if $self->{verbose};

    # I know the source is a directory.
    # I know the destination is also a directory
    # which has a different timestamp than the
    # source.  All nodes within both directories
    # must be scanned and updated accordingly.

    my ($handle, $node, %nodes);

    $handle = do { local *FH; };
    $0 = "$self->{proctitle} - src: $src" if $self->{proctitle};
    return unless opendir($handle, $src);
    while (defined ($node = readdir($handle))) {
      next if $node =~ /^\.\.?$/;
      next if $self->{ignore}->{$node};
      next if ($self->{localmode} &&
               !-l "$src/$node" &&
               -d _);
      $nodes{$node} = 1;
      $self->_op if $self->{_gentle_percent};
    }
    closedir($handle);

    $handle = do { local *FH; };
    $0 = "$self->{proctitle} - dst: $dst" if $self->{proctitle};
    return unless opendir($handle, $dst);
    while (defined ($node = readdir($handle))) {
      next if $node =~ /^\.\.?$/;
      next if $self->{ignore}->{$node};
      next if ($self->{localmode} &&
               !-l "$src/$node" &&
               -d _);
      $nodes{$node} = 1;
      $self->_op if $self->{_gentle_percent};
    }
    closedir($handle);

    $0 = "$self->{proctitle} - syncing: $src => $dst" if $self->{proctitle};
    # %nodes is now a union set of all nodes
    # in both the source and destination.
    # Recursively call myself for each node.
    foreach $node (keys %nodes) {
      $self->_dirsync("$src/$node", "$dst/$node");
    }
    # Force permissions to match the source
    chmod( $stat_src[2] & 0777, $dst) or warn "$dst: Failed to chmod: $!\n";
    # Force user and group ownership to match the source
    chown( $stat_src[4], $stat_src[5], $dst) or warn "$dst: Failed to chown: $!\n";
    # Force timestamp to match the source.
    utime $when_src, $when_src, $dst or warn "$dst: Failed to utime: $!\n";
    $self->_op(5) if $self->{_gentle_percent};
    return;
  }

  print "$src: Unimplemented weird type of file! Skipping...\n" if $self->{verbose};
}

sub only {
  my $self = shift;
  push (@{ $self->{only} }, @_);
}

sub maxskew {
  my $self = shift;
  $self->{maxskew} = shift || 0;
}

sub dst {
  my $self = shift;
  $self->{dst} = shift;
}

sub src {
  my $self = shift;
  $self->{src} = shift;
}

sub ignore {
  my $self = shift;
  $self->{ignore} ||= {};
  # Load ignore into a hash
  foreach my $node (@_) {
    $self->{ignore}->{$node} = 1;
  }
}

sub lockfile {
  my $self = shift;
  my $lockfile = shift or return;
  open (LOCK, ">$lockfile") or return;
  if (!flock(LOCK, 6)) { # (LOCK_EX | LOCK_NB)
    print "Skipping due to concurrent process already running.\n" if $self->{verbose};
    exit;
  }
}

sub verbose {
  my $self = shift;
  if (@_) {
    $self->{verbose} = shift;
  }
  return $self->{verbose};
}

sub localmode {
  my $self = shift;
  if (@_) {
    $self->{localmode} = shift;
  }
  return $self->{localmode};
}

sub nocache {
  my $self = shift;
  if (@_) {
    $self->{nocache} = shift;
  }
  return $self->{nocache};
}


sub entries_updated {
  my $self = shift;
  return () unless ( ref $self->{_tracking} eq 'HASH' );
  return @{ $self->{_tracking}->{updated} };
}

sub entries_removed {
  my $self = shift;
  return () unless ( ref $self->{_tracking} eq 'HASH' );
  return @{ $self->{_tracking}->{removed} };
}

sub entries_skipped {
  my $self = shift;
  return () unless ( ref $self->{_tracking} eq 'HASH' );
  return @{ $self->{_tracking}->{skipped} };
}

sub entries_failed {
  my $self = shift;
  return () unless ( ref $self->{_tracking} eq 'HASH' );
  return @{ $self->{_tracking}->{failed} };
}

sub rmtree {
  my $self = shift;
  my $restore = {};
  foreach my $node (@_) {
    $self->_op if $self->{_gentle_percent};
    my (undef,undef,$mode) = lstat $node;
    if (-d _) {
      my @files = ();
      if (opendir my $d, $node) {
        @files = readdir $d;
        closedir $d;
      } else {
        unless ($mode & 0200) {
          # Make directory writable
          chmod 0777, $node or warn "$node: Failed to chmod: $!\n";
          $self->_op(2) if $self->{_gentle_percent};
          # Try to opendir one last time
          if (opendir my $d, $node) {
            @files = readdir $d;
            closedir $d;
          } else {
            warn "$node: Failed to opendir: $!\n";
          }
        }
      }
      $self->rmtree( map { "$node/$_" } grep !/^\.\.?$/, @files );
      rmdir $node;
      $self->_op(3 + scalar(@files)) if $self->{_gentle_percent};
    } else {
      if (!unlink $node and lstat $node) {
        # Tried to unlink it but it still exists
        my $dir = $node;
        $dir =~ s%[^/]*$%.%;
        my (undef,undef, $dmode) = lstat $dir;
        unless ($dmode & 0200) {
          # Make directory writable
          chmod 0777, $dir or warn "$dir: Failed to chmod: $!\n";
          $self->_op if $self->{_gentle_percent};
        }
        # Try one last time to remove
        unlink $node or warn "$node: Failed to unlink: $!\n";
        $self->_op(4) if $self->{_gentle_percent};
        # Don't forget to restore this guy back to how he was
        $restore->{$dir} = $dmode & 07777 unless exists $restore->{$dir};
      } else {
        $self->_op if $self->{_gentle_percent};
      }
    }
  }
  foreach my $dir (keys %{ $restore }) {
    chmod $restore->{$dir}, $dir;
  }
  $self->_op(1 + scalar(keys %{ $restore }) ) if $self->{_gentle_percent};
  return $_[0] && !lstat $_[0];
}

# Create copy of $src as $dst in one atomic operation.
# (The $dst file will never be partial.)
sub copy {
  my $self = shift;
  my $src = shift;
  my $dst = shift;
  my $temp_dst = $dst;
  $temp_dst =~ s%/([^/]+)$%/.\#$1.dirsync.tmp%;
  my $errno = 0;
  if (sysopen FROM, $src, O_RDONLY) {
    if (sysopen TO, $temp_dst, O_WRONLY | O_CREAT | O_EXCL, 0600) {
      my $buffer;
      while (sysread(FROM, $buffer, BUFSIZE)) {
        $self->_op(BUFFER_OPS) if $self->{_gentle_percent};
        if (!syswrite(TO, $buffer, length $buffer)) {
          $errno = $!;
          last;
        }
      }
      close TO;
    } else {
      $errno = $!;
    }
    close FROM;
  } else {
    $errno = $!;
  }
  # XXX - Should we consider this operation as many thousands of ops?
  # XXX - Depending on fs type, such as reiserfs, could this grind?
  if (!$errno and !rename $temp_dst, $dst) {
    $errno = $!;
  }
  $self->_op(6) if $self->{_gentle_percent};
  if ($errno) {
    unlink $temp_dst;
    $! = $errno;
    return undef;
  }
  return 1;
}

1;
__END__

=head1 NAME

File::DirSync - Syncronize two directories rapidly

$Id: DirSync.pm,v 1.2 2015/03/10 14:28:48 fisyak Exp $

=head1 SYNOPSIS

  use File::DirSync;

  my $dirsync = new File::DirSync {
    verbose => 1,
    nocache => 1,
    localmode => 1,
  };

  $dirsync->src("/remote/home/www");
  $dirsync->dst("/home/www");
  $dirsync->ignore("CVS");

  $dirsync->rebuild();

  #  and / or

  $dirsync->dirsync();

=head1 DESCRIPTION

File::DirSync will make two directories exactly the same. The goal
is to perform this syncronization process as quickly as possible
with as few stats and reads and writes as possible.  It usually
can perform the syncronization process within a few milliseconds -
even for gigabytes or more of information.

Much like File::Copy::copy, one is designated as the source and the
other as the destination, but this works for directories too.  It
will ensure the entire file structure within the descent of the
destination matches that of the source.  It will copy files, update
time stamps, adjust symlinks, and remove files and directories as
required to force consistency.

The algorithm used to keep the directory structures consistent is
a dirsync cache stored within the source structure.  This cache is
stored within the timestamp information of the directory nodes.
No additional checksum files or separate status configurations
are required nor created.  So it will not affect any files or
symlinks within the source_directory nor its descent.

=head1 METHODS

=head2 new( [ { properties... } ] )

Instantiate a new object to prepare for the rebuild and/or dirsync
mirroring process.

  $dirsync = new File::DirSync;

Key/value pairs in a property hash may optionally be specified
as well if desired as demonstrated in the SYNOPSIS above.  The
default property hash is as follows:

  $dirsync = new File::DirSync {
    verbose => 0,
    nocache => 0,
    localmode => 0,
    src => undef,
    dst => undef,
  };

=head2 src( <source_directory> )

Specify the source_directory to be used as the default for
the rebuild() method if none is specified.  This also sets
the default source_directory for the dirsync() method if
none is specified.

=head2 dst( <destination_directory> )

Specify the destination_directory to be used as the default
for the dirsync() method of none is specified.

=head2 rebuild( [ <source_directory> ] )

In order to run most efficiently, a source cache should be built
prior to the dirsync process.  That is what this method does.
If no <source_directory> is specified, you must have already
set the value through the src() method or by passing it as a
value to the "src" property to the new() method.  Unfortunately,
write access to <source_directory> is required for this method.

  $dirsync->rebuild( $from );

This may take from a few seconds to a few minutes depending on
the number of nodes within its directory descent.  For best
performance, it is recommended to execute this rebuild on the
computer actually storing the files on its local drive.  If it
must be across NFS or other remote protocol, try to avoid
rebuilding on a machine with much latency from the machine
with the actual files, or it may take an unusually long time.

=head2 dirsync( [ <source_directory> [ , <destination_directory> ] ] )

Copy everything from <source_directory> to <destination_directory>.
If no <source_directory> or <destination_directory> are specified,
you must have already set the values through the src() or dst()
methods or by passing it to the "src" or "dst" properties to new().
Files and directories within <destination_directory> that do not
exist in <source_directory> will be removed.  New nodes put within
<source_directory> since the last dirsync() will be mirrored to
<destination_directory> retaining permission modes and timestamps.
Write access to <destination_directory> is required.  Read-only
access to <source_directory> is sufficient since it will not be
modifed by this method.

  $dirsync->dirsync( $from, $to );

The rebuild() method should have been run on <source_directory>
prior to using dirsync() for maximum efficiency.  If not, then use
the nocache() setting to force dirsync() to mirror the entire
<source_directory> regardless of the dirsync source cache.

=head2 only( <source> [, <source> ...] )

If you are sure nothing has changed within source_directory
except for <source>, you can specify a file or directory
using this method.

  $dirsync->only( "$from/htdocs" );

However, the cache will still be built all the way up to the
source_directory.  This only() node must always be a subdirectory
or a file within source_directory.  This option only applies to
the rebuild() method and is ignored for the dirsync() method.
This method may be used multiple times to rebuild several nodes.
It may also be passed a list of nodes.  If this method is not
called before rebuild() is, then the entire directory structure
of source_directory and its descent will be rebuilt.

=head2 maxskew( [ future_seconds ] )

In order to avoid corrupting directory time stamps into the
future, you can specify a maximum future_seconds that you will
permit a node in the <source> directory to be modified.

  $dirsync->maxskew( 7200 );

If the maxskew method is not called, then no corrections to
the files or directories will be made.  If no argument is
passed, then future_seconds is assumed to be 0, meaning "now"
is considered to be the farthest into the future that a file
should be allowed to be modified.

=head2 ignore( <node> )

Avoid recursing into directories named <node> within
source_directory.  It may be called multiple times to ignore
several directory names.

  $dirsync->ignore("CVS");

This method applies to both the rebuild() process and the
dirsync() process.

=head2 lockfile( <lockfile> )

If this option is used, <lockfile> will be used to
ensure that only one dirsync process is running at
a time.  If another process is concurrently running,
this process will immediately abort without doing
anything.  If <lockfile> does not exist, it will be
created.  This might be useful say for a cron that
runs dirsync every minute, but just in case it takes
longer than a minute to finish the dirsync process.
It would be a waste of resources to have multiple
simultaneous dirsync processes all attempting to
dirsync the same files.  The default is to always
dirsync.

=head2 verbose( [ <0_or_1> ] )

  $dirsync->verbose( 1 );

Read verbose setting or turn verbose off or on.
Default is off.

=head2 localmode( [ <0_or_1> ] )

Read or set local directory only mode to avoid
recursing into the directory descent.

  $dirsync->localmode( 1 );

Default is to perform the action recursively
by descending into all subdirectories of
source_directory.

=head2 nocache( [ <0_or_1> ] )

When mirroring from source_directory to destination_directory,
do not assume the rebuild() method has been run on the source
already to rebuild the dirsync cache.  All files will be
mirrored.

  $dirsync->nocache( 1 );

If enabled, it will significantly degrade the performance
of the mirroring process.  The default is 0 - assume that
rebuild() has already rebuilt the source cache.

=head2 gentle( [ <percent> [, <ops> ] ] )

Specify gentleness for all disk operations.
This is useful for those servers with very busy disk drives
and you need to slow down the sync process in order to allow
other processes the io slices they demand.
The <percent> is the realtime percentage of time you wish to
be sleeping instead of doing anything on the hard drive,
i.e., a low value (1) will spend most of the time working
and a high value (99) will spend most of the time sleeping.
The <ops> is the number of disk operations you wish to
perform in between each sleep interval.

  $dirsync->gentle( 25, 1_000 );

If gentle is called without arguments, then some default
"nice" values are set.
If gentle is not called at all, then it will process all disk
operations at full blast without sleeping at all.

=head2 proctitle( [ procname ] )

Enable proctitle mode which shows the current operation on the
process title.  If procname is specified, then it shows that
string in the "ps" listing.  Otherwise, the current $0 is used.
This is mostly for progress tracking for convenience purposes.

  $dirsync->proctitle( "SYNCING" );

Default is not to alter the process title at all.

=head2 tracking( [ <0_or_1> ] )

Enable or disable tracking mode.  Operation tracking is disabled
by default in order to reduce CPU and memory consumption.
See entries_* methods below for more details.

=head2 entries_updated()

Returns an array of all directories and files updated in the last
C<dirsync>, an empty list if it hasn't been run yet.

=head2 entries_removed()

Returns an array of all directories and files removed in the last
C<dirsync>, an empty list if it hasn't been run yet.

=head2 entries_skipped()

Returns an array of all directories and files that were skipped in the
last C<dirsync>, an empty list if it hasn't been run yet.

=head2 entries_failed()

Returns an array of all directories and files that failed in the last
C<dirsync>, an empty list if it hasn't been run yet.


=head1 TODO

Support for efficient incremental changes to large log files using
md5 checksum comparison on portions of or all of corresponding
parts of both the larger source and smaller destination files.
If no differences are found anywhere, including the very end of the
destination file, then simply append the end of the source to the
end of the destination until both files are identical again.
Avoid making a full copy of the source and especially avoid
writing the entire file since writes are so slow and plainful.

Support for hard linking the source files into the destination when
they both reside on the same device instead of making a full copy.

Generalized file manipulation routines to allow for easier
integration with third-party file management systems.

Support for FTP dirsync (both source and destination).

Support for Samba style sharing dirsync.

Support for VFS, HTTP/DAV, and other more standard remote
third-party file management.

Support for dereferencing symlinks instead of creating
matching symlinks in the destination.

=head1 BUGS

If the source or destination directory permission settings do not
provide write access, there may be problems trying to update nodes
within that directory.

If a source file is modified after, but within the same second, that
it is dirsynced to the destination and is exactly the same size, the
new version may not be updated to the destination.  The source will
need to be modified again or at least the timestamp changed after
the entire second has passed by.  A quick touch should do the trick.

It does not update timestamps on symlinks, because I couldn't
figure out how to do it without dinking with the system clock. :-/
If anyone knows a better way, just let the author know.

Only plain files, directories, and symlinks are supported at this
time.  Special files, (including mknod), pipe files, and socket files
will be ignored.

If a destination node is modified, added, or removed, it is not
guaranteed to revert to the source unless its corresponding node
within the source tree is also modified.  To ensure syncronization
to a destination that may have been modifed, a rebuild() will also
need to be performed on the destination tree as well as the source.
This bug does not apply when using { nocache => 1} however.

Win32 PLATFORM: Removing or renaming a node from the source tree
does NOT modify the timestamp of the directory containing that node
for some reason (see test case t/110_behave.t).  Thus, this change
cannot be detected and stored in the source rebuild() cache.  The
workaround for renaming a file is to modify the contents of the
new file in some way or make sure at least the modified timestamp
gets updated.  The workaround for removing a file, (which also
works for renaming a file), is to manually update the timestamp
of the directory where the node used to reside:

  perl -e "utime time,time,q{.}"

Then the rebuild() cache can detect and propagate the changes
to the destination.  The other workaround is to disable the
rebuild() cache (nocache => 1) although the dirsync() process
will generally take longer.

=head1 AUTHOR

Rob Brown, bbb@cpan.org

=head1 COPYRIGHT

Copyright (C) 2002-2007, Rob Brown, bbb@cpan.org

All rights reserved.

This may be copied, modified, and distributed under the same
terms as Perl itself.

=head1 SEE ALSO

L<dirsync(1)>,
L<File::Copy(3)>,
L<perl(1)>

=cut
