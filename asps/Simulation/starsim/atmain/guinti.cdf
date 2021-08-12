* $Id: guinti.cdf,v 1.1.1.1 2004/01/12 23:49:38 potekhin Exp $
* $Log: guinti.cdf,v $
* Revision 1.1.1.1  2004/01/12 23:49:38  potekhin
*
* Revision 1.1  2001/02/27 10:14:52  nevski
*  first working release
*
******************************************************************************
*               G U I N T I    C D F
******************************************************************************
>Name GUINTI

>Menu USER
>Guidance
User defined menu for GSTAR

>Command INPUT
>Parameters
TYPE 'Type of event input' C D='TX' R='FZ,TX,FZTX,TXOLD,TXOTX,FZTXO'
INFILE1 'First input file name' C D=' '
+
INFILE2 'Second input file name (must be TX)' C D=' '
>Guidance
Opens one or two files of events as input to Geant. If read types FZ
(Zebra) or TX (Star new text format) are given, only one file of the given
type is opened using the first input filename. If read type FZTX or
TXOTX is given, two files are opened and merged within Geant. For
the FZTX option, the first filename must be an FZ file and the second
filename a TX file (new text format). For the TXOTX, the first file
must be an old format text file, and the second a TX file.

In the FZTX and TXOTX options, if the tracks and vertices of the two files
are to be disentangled again downstream of Geant, it is the user's
responsibility to ensure that they are labelled in a unique way. One
simple way to do this is to make all vertex processes in the TX file
negative.

If a file of the given type is open at the time of this call, it will be
closed before further action is taken.

A TXO ("old text") file should consist of one line containing the word
"event", number of  particles (N) and the event number, followed by exactly
N lines each containing geant particle id and px,py,pz of the particle.
In a TX ("new text") file each line starts with a keyword each containing:

 GENER:  version  east_z  east_a  west_z  west_a  sqrts b_max
 EVENT:  n_event  n_track  n_vertex
 TRACK:  ge_pid  px  py  pz  LabelTr  StartVx  StopVx  eg_pid
 VERTEX: vx  vy  vz   LabelVx  eg_proc  parent

In both cases reading is free formatted, case sensetive.


>Action AGSUSER
*----------------------------------------------------------------------------

>Command OUTPUT
>Parameters
ACTION  'Open or close output file' C D=' ' R='O,C'
+
OUTFILE 'FZ output file name' C D='geant.fzd'
>Guidance
Opens/Closes an FZ output file.
>Action AGSUSER
*----------------------------------------------------------------------------

>Command PHASESPACE
>Parameters
+
NTRACK  'Number of tracks per event' I D=100
ID      'Geant Particle ID'       I D=8
PTLOW   'Lower limit of pT'       R D=0.
PTHIGH  'Upper limit of pT'       R D=1.
YLOW    'Lower limit of rapidity' R D=-1.
YHIGH   'Upper limit of rapidity' R D=1.
PHILOW  'Lower limit of phi'      R D=0.
PHIHIGH 'Upper limit of phi'      R D=6.283
>Guidance

Generates flat phase space in place of input file of events
(uniform in pT and rapidity).
Parameters are Geant PID, lower and upper bounds of rapidity interval,
lower and upper bounds of pT interval, and number of tracks per
event. Full azimuthal interval is used (0<phi<two pi radians).

If no parameters are given, old values existing in the program are preserved.
Initial limits are 0<pT<10000, -10<y<+10, 0<phi<2pi.

>Action AGSUSER
*----------------------------------------------------------------------------

>Command MOMENTUMBIN
>Parameters
+
NTRACK 'Number of tracks per event' I D=100
ID     'Geant Particle ID' I D=8
PXLOW  'Lower limit of px' R D=-1.
PXHIGH 'Upper limit of px' R D=1.
PYLOW  'Lower limit of py' R D=-1.
PYHIGH 'Upper limit of py' R D=1.
PZLOW  'Lower limit of pz' R D=-1.
PZHIGH 'Upper limit of pz' R D=1.
>Guidance

Generates uniform distribution within given 3-momentum bin in place of
input file of events.
Parameters are Geant PID, lower and upper bounds
of px, py and pz, and number of tracks per event.

If no parameters are given, 100 pi+ will be generated per event,
distributed uniformly in the interval -1<px<1, -1<py<1, -1<pz<1 GeV.
If px_high is less than px_low, then px_high will be set equal to px_low
(i.e. px will have the same value for all tracks),
and similarly for py and pz.
>Action AGSUSER
*----------------------------------------------------------------------------

>Command SKIP
>Parameters
NEVENT 'Number of input events to skip' I D=0
+
NSUBEVENT 'Number of subevents to skip in first processed event' I D=0
SUBRAN1 'First random seed at start of first processed subevent' I D=0
SUBRAN2 'Second random seed at start of first processed subevent' I D=0
>Guidance

Skips the next NEVENT events of input event file. If NSUBEVENT>0,
skips first NSUBEVENT subevents in first processed event, using random
seeds SUBRAN1 and SUBRAN2. This allows the user to set the seeds to
randomize the target position correctly at the beginning of the desired
event and then skip directly to any subevent.
>Action AGSUSER
*----------------------------------------------------------------------------

>Command UDECAY
>Parameters
PIDPARENT 'Geant PID of parent' I D=11
RIN 'Inner limit of radial interval for uniform decay' R D=10.
ROUT 'Outer limit of radial interval for uniform decay' R D=100.
+
PID1 'Geant PID of first decay daughter'  I D=0
PID2 'Geant PID of second decay daughter' I D=0
PID3 'Geant PID of third decay daughter'  I D=0
>Guidance
Initializes parameters for the decay of a particle with roughly uniform
probability along its trajectory between the cylindrical surfaces
R=RIN and R=ROUT. This can be used to enhance statistics for the
efficiency studies of the reconstruction of decays.
Since the path length to the generated vertex and the parent
4-momentum are known, the appropriate weighting factor for
the vertex due to the lifetime of the parent can be calculated.
Decay is generated according to the parent branchings.
If needed, they can be modified by user with SPART command.
>Action AGSUSER
*----------------------------------------------------------------------------

>Command SPARTSTAR
>Parameters
>Guidance
 Obsolete command. Use GEANT/CONTROL/SPART instead
>Action AGSUSER
*----------------------------------------------------------------------------

>Command GFDK
>Parameters
+
IPART 'Geant PID' I D=0
>Guidance
Writes out decay modes for particle id IPART.
>Action AGSUSER
*----------------------------------------------------------------------------

>Command SECONDARIES
>Parameters
SCND 'secondaraies treatment flag'  I D=1  R=0:2
>Guidance
Controls the way how secondary particles are treated:
 0 - secondaries ignored;
 1 - secondaries are put in jstak only (standard)
 2 - some secondaries are saved in KINE bank.
     This concerns decay products of particles, already existing in KINE.
     Other products (than decay) of these particles are saved in KINE
     only if they are produced in specially named media, which should
     be different from the medium, where the parent was born.
>Action  AGSUSER
*----------------------------------------------------------------------------

>Command VXYZ
>Parameters
VX ' primary vertex x ' R D=0 R=-1000:1000
VY ' primary vertex y ' R D=0 R=-1000:1000
VZ ' primary vertex z ' R D=0 R=-1000:1000
>Guidance
Displaces an average position of the interactions vertex in (Vx,Vy,Vz)
>Action  AGSUSER
*----------------------------------------------------------------------------

>Command VSIG
>Parameters
VSIGT 'transverse vertex spread'        R D=0 R=0:1000
VSIGZ 'longitudinal vertex spread'      R D=0 R=0:1000
>Guidance
Defines sigma if the generated vertex spread both in transverse and
longitudinal(along the beam axis) direction.
>Action  AGSUSER
*----------------------------------------------------------------------------

>Command SUBEVENT
>Parameters
NSUB 'number of final state tracks per sub-event'  I D=200      R=0:10000000
+
Ntrk_max 'maximum of tracks per single KINE bank ' I D=64000    R=0:10000000
Nhit_max 'maximum length of a single HITS bank'    I D=10000000 R=0:10000000
>Guidance
Controls splitting of full events into a number of smaller sub-events
in the GEANT simulation phase.
>Action  AGSUSER
*----------------------------------------------------------------------------

>Command SHADOW
>Parameters
SHDO 'tracking flag for dense materials'  I D=1  R=0:1
>Guidance
Flag to set tracking threshold in dense materials very high.
This will prevent showering in magnets and other dense objects,
but they will still block unphysical tracks
>Action  AGSUSER
*----------------------------------------------------------------------------

>Command SENSECUTE
>Parameters
CUTE 'electron tracking cut'              R D=-1 R=-1.0:10.0
>Guidance
Changing electron tracking cut in sensitive gases one can control
delta-electron production in detector itself.
>Action  AGSUSER
*----------------------------------------------------------------------------


