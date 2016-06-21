* $Id: agxinit.cdf,v 1.5 2016/06/21 14:23:19 jwebb Exp $
* $Log: agxinit.cdf,v $
* Revision 1.5  2016/06/21 14:23:19  jwebb
* Retire unused DB / Zebra interface.
*
* Revision 1.4  2012/07/19 21:33:14  jwebb
* Added KUIP interface to configuration parameters of the filters.
*
* Revision 1.3  2011/10/20 23:07:19  jwebb
* Added capability to provide a slope in the x,y vertex.
*
* Revision 1.2  2009/04/09 22:33:21  perev
* gfilter added
*
* Revision 1.1.1.1  2004/01/12 23:49:38  potekhin
*
*
* Revision 1.12  2003/08/23 15:59:05  nevski
* add bug report feature
*
* Revision 1.11  2003/06/13 15:30:01  nevski
* gfile help corrected
*
* Revision 1.10  2003/06/12 09:27:34  nevski
* fpu control introduced
*
* Revision 1.9  2003/05/07 17:04:05  nevski
* make draw bank persistent
*
* Revision 1.8  2002/11/15 18:46:16  nevski
* add interface to control GCALOR
*
* Revision 1.7  2001/12/17 23:37:54  nevski
* tflt command added
*
* Revision 1.6  2001/10/28 15:04:00  nevski
* help updated
*
* Revision 1.5  2001/07/26 16:07:21  nevski
* *** empty log message ***
*
* Revision 1.4  2001/06/15 15:17:30  nevski
* merging cmz cvs differences
*
* Revision 1.3  2001/06/08 17:16:59  nevski
* MySQL interface enabled
*
* Revision 1.1  2001/02/27 10:14:48  nevski
*  first working release
*
******************************************************************************
*               A G X I N I T    C D F
******************************************************************************
>Name AGXINIT
>Menu AGUSER
>guidance
Advanced Geant Interface

    ********************************************************************
    *                                                                  *
    *        A D V A N C E D   G E A N T   I N T E R F A C E           *
    *                                                                  *
    *                      04-Jan-00 hot news:                         *
    *                                                                  *
    *                          CONTROL                                 *
    * Kuip command "ON ERROR GOTO label" will now react on:            *
    *      - End_of_DATA on P stream, Write_Error on O stream          *
    *      - time_left less than defined by GTIME command argument     *
    * On GHIST command some standard histogram handling is introduced  *
    *        with automatic histogram dump when EXITing the program.   *
    * Print control is now fully consistent with SLUG-DICE-ATRECON     *
    * Since 97a CERNLIB release FILL attribute has to be 0, otherwise  *
    * edges drawn in black/white obscure most of the GEANT drawings !  *
    * GEANT and PAW memory may be increased at the invocation time -   *
    * start with -h option to get more information how to use switches *
    * "-b filename" option now available for batch mode.               *
    * To get complete description of AGI commands in printable format  *
    *              do:  MANUAL AGUSER MAN.TEX LATEX                    *
    *                                                                  *
    *                       GEANT & PHYSICS:                           *
    * GCALOR avalable for calorimeter simulations on command: HADR 6   *
    *        It needs bertaf.dat and xsneut95.dat from /cern/95a/lib   *
    * Electron physics (Bremsstraghlung, Pair production) updated      *
    * Absorption length calculation for mixtures corrected in GEANT.   *
    * Explicit NCOPY parameter is now allowed in the POSITION operator *
    * Binning refined: - for BIT option all 2**Nbit values are used,   *
    *                    unknown elements are supposed to be integer   *
    *                  - for BIN option the interval is centered       *
    * Protection against errenious hit limits (bug in GFIPAR on SUN)   *
    * Tracking in MANY volumes is corrected - no missing hits anymore  *
    *                                                                  *
    *                       Data STRUCTURING:                          *
    * RbGET counting request now returns again the correct number      *
    *       of banks in a chain, as it is described in SOFT-NO-002     *
    * CMZ KEEP sequences (car-format) are recognised as REPLACE macros *
    * GENZ package added - LGNFIND,GNZGET* calls are available         *
    * USE operator has an OPERation NEXT, allowing bank chain scanning *
    * Schema evolution is supported by USE operator - see help USE     *
    *                                                                  *
    ********************************************************************

>Command ACTIONS
>parameters
+
Option 'program actions allowed'   C    D='*'
>Guidance
This command can be used to overwrite the default program actions,
derived from the nature of input data.

List of program actions, allowed in the run, is :
 K - convertion GENZ banks to KINE
 S - Geant simulations
 D - Hit digitization
 R - reconstruction
 T - test beam data analysis.
 O - automatic output at the end of TRIG command
 * - all above is allowed
>Action AGXUSER
*-----------------------------------------------------------------------------
>Command VERSIONS
>parameters
+
Option 'program, component or data versions'   C    D='*'
Value  'optional parameter value'              I    D=0
>Guidance
This command can be used to steer version dependance,
which cannot be derived from the nature of input data.

List of available program version :

 ATLAS,STAF,DENS - Different packing versions for REBANK, affects
                   the number of user banks per single ZEBRA bank
 BATCH           - no errors are tolarated in I/O commands (gexec,gfile)
 INTER           - return to normal (interactive) error handling
 FZLEN           - max length of output FZ file in Mwords (512 is 2GB)
 GTRACK          - generic keys to control tracking modes.
 RZ95, RZ96      - Different key formats in RZ files (affects RZ/FILE)
                   (now recognised automatically

For the moment the first key switches  GUSCNTR in GTRACK,
the second one switches additional call to GTMEDI in case of MANY;
The third one (3 digits) modifies logic of GTXXXX routines.
Default setting (all 0s) corresponds to what is believed
today (05.10.00) to be the best choice - purely 99 cernlib
tracking apart from diff=(Vout-Vnext) which is taken as abs(diff).

>Action AGXUSER
*-----------------------------------------------------------------------------
>Command RANLUX
>parameters
+
Seed 'initial seed' I        D='123456789'
Lux  'luxury level' I R=-1:4 D='1'
N1   'first part of sequence number'   I R=0:999999999 D=0
N2   'second part of sequence number'  I R=0:999999999 D=0
>guidance
Switch all random number generators known to the program
from GRNDM (geant) to RANLUX (V115) and initialise it.

For negative Lux switch back to GRNDM (default status)

>Action AGXUSER
*-----------------------------------------------------------------------------
>Command GDUMP
>Parameters
PATH   'Path to the selected sub-branch of banks' C D='/DETM'
+
OPTION 'Dump option (F,H,C,U or 1)'                   C D=' '
>guidance
Dumps the content of a selected sub-branch of ZEBRA banks,
addressed by the PATH.

Possible options are:
 F - dump in a file instead of the terminal. The bank name is used as
     the file name with .sgml extension
 H - convert the dump file in HTML formated set of files with
     apropriate hyperlinks beween them.
     Each bank in the structure is described in a separate file
     with the name of the bank and its top level bank as the filename
     and with .html extension.
 C - dump only numbers without variable names and comments. This may
     be usefull for an output intended to be read by another program.
 U - dump also banks which have no documenation (normally they are skipped),
 1 - dump also long banks (more than 1000 word - normally they are skipped).

>Action AGXUSER
*-----------------------------------------------------------------------------
>Command MODE
>Parameters
Detector 'detector subsystem name' C D='ALL'
+
Flag  'control flag name' C
* R='PRIN,DEBU,GEOM,HIST,GRAP,SIMU,DIGI,RECO,MFLD,ANAL,BACK'
Value 'flag value'        I
>Guidance
Set control flags for a given detector subsystem (or for ALL of them).
Possible flags and their default values are:
       PNOW  (0)             - print level for current event
       PRIN  (0)             - normal print level
       DEBU  (0)             - debug print level
       GEOM  (1)             - geometry version
       HIST  (1)             - system histogram flag
       GRAP  (1)             - system graphics level
       SIMU  (1)             - store GEANT hits flag
       DIGI  (1)             - digitisation flag
       RECO  (1)             - reconstruction flag
       MFLD  (1)             - magnetic field flag
       ANAL  (0)             - user analysis level
       BACK  (0)             - number of pile-up bunchs to select
                               (relative to the trigger one)

To change default values use GSFLAG command.

>Action AGXUSER
*-----------------------------------------------------------------------------
>Command DETP
>Parameters
Detector 'detector subsystem name' C
+
NAME  'name of the selected bank or of a variable in the bank' C
Value 'value of the selector or new value of the variable    ' R
>Guidance

 DETP command provides a way to modify the content of parameter banks.

When a USE operator is called for a bank for the first time,
it checks wether the bank name and the value of its selector
coincides with the one mentioned in the DETP command for the same detector.
If this is the case, corresponding variables in the bank are replaced
by the new value. Selector name and value is provided in brackets
after the bank name. If no name (only a value) is provided,
this refers to the selector in the USE operator itself.

 Only one DETP command per detector is kept in the program.
The next command with the same detector name overwrites the previous one.
On the other hand, any number of banks and their variable
can be changed by a single command (which can be extended
over several lines following KUIP rules).
When data are read from P stream, they are stripped out of
old DETP commands.

 Typing rules for  DETP parameters are the following:
 - the bank selector value may be given in () or with a = sign.
 - variable to change should have a trailing = sign.
 - command is not case sensitive.

 Example.
 DETP CALO cgeo=1  rmin=200  rmax=300
 DETP CALO cgeo(1).rmin=200  rmax=300
 DETP CALO cgeo(version=1).rmin=200  rmax=300

DETP command may be generated also from the code using two
subroutines:  AgDETPnew (Detector) and AgDETPadd (Name,Values,Nval).

 Example. The previous command may be generated by:
 Call AgDETP new ('CALO')
 Call AgDETP add ('cgeo(version=1).rmin=',200,1)
 Call AgDETP add ('rmax=',300,1)

>Action AGXUSER
*-----------------------------------------------------------------------------
>Command GVERTEX
>Parameters
X_vertex  'x of the generated vertex' R D=0
Y_vertex  'y of the generated vertex' R D=0
Z_vertex  'z of the generated vertex' R D=0
>Guidance
Defines the average position of the simulated vertex at z=0.  To provide a
z-dependent offset, use the gslope command.
>Action AGXUSER
*-----------------------------------------------------------------------------
>Command GSPREAD
>Parameters
X_sigma  'x-spread of the generated vertex' R D=0
Y_sigma  'y-spread of the generated vertex' R D=0
Z_sigma  'z-spread of the generated vertex' R D=0
>Guidance
Defines the spread of the simulated vertex position.
>Action AGXUSER
*-----------------------------------------------------------------------------
>Command GSLOPE
>Parameters
X 'z-dependence of the beamline' R D=0
Y 'z-dependence of the beamline' R D=0
>Guidance
Provides the slope of the beamline vs z in the x and y directions. 

Example:

   aguser/gspread 0.02 0.02 25.0
   aguser/gvertex 0.01 0.10  1.0
   aguser/gslope  2.5E-3 1.0E-6 

   implements a beamline constraint

   x = 0.01 + (2.5E-3)*z
   y = 0.10 + (1.0E-6)*z
>Action AGXUSER
*-----------------------------------------------------------------------------
>Command GSFLAG
>Parameters
Flag  'AGI control flag' C D='PRIN' R='PRIN,PNOW,GRAP,HIST,SIMU,MFLD'
+
Value 'flag value      ' I D=1
>Guidance
Sets default value for control flags. These flags will be used to
provide a default value for all new detectors DETE (MODE) bank.

Flags mentioned here are only a subset of all flags,
available in the MODE command.

>Action AGXUSER
*-----------------------------------------------------------------------------
>Command GFLAG
>Parameters
Flag  'AGI control flag' C D='PRIN' R='PRIN,PNOW,GRAP,HIST,SIMU,MFLD'
+
Value 'flag value      ' I D=1
>Guidance
Sets default value for control flags - same as GSFLAG

>Action AGXUSER
*-----------------------------------------------------------------------------
>Command GDEBUG
>Parameters
Flag 'hard debug level'  I  D=1
+
Itest 'random number print flag'  I  D=0
>Guidance
Set geant IDEBUG flag to value more than 1 for hard debugging
and ITEST flag for random number printouts.

>Action AGXUSER
*-----------------------------------------------------------------------------
>Command GAUTO
>Parameters
Option 'automatic tracking parameter computation'  C  D='ON' R='ON,OFF,0,1'
>Guidance
Set or reset flag for automatic tracking medium parameter computation by GEANT.
Equivalent to the standard GEANT AUTO flag (which also works now).
>Action AGXUSER
*-----------------------------------------------------------------------------
>Command GPRINT
>Parameters
NAME   'Object Name' C
>Guidance
Prints selected GEANT object (hits, digits, sets,
KINE or GENZ tracks, Vertices, Particles, Materials, Media,
Volumes, Rotation matrices)
using its name rather than the numeric ID.

 optional additional parameters are :
   SET    'Set(subsystem) name'        C  D='*'
   DET    '(sensitive) detector name'  C  D='*'
 or:
   NUM    'track/vertex number'        I  D=0
>Action AGXUSER
*-----------------------------------------------------------------------------
>Command GPHITS
>Parameters
+
CSET  'subsystem (set) name' C D='*'
CDET  'User detector name'   C D='*'
>Guidance
Prints hits using CALL AGPDIGI(cset+H,cdet).
Unlike the internal GEANT numbering, volume numbering starts from 1.
Pseudo-divisions are decoded into their original real values.
>Action AGXUSER
*-----------------------------------------------------------------------------
>Command GPDIGI
>Parameters
+
CSET  'subsystem (set) name' C D='*'
CDET  'User detector name'   C D='*'
>Guidance
Prints digits using CALL AGPDIGI(cset+D,cdet).
Unlike the internal GEANT numbering, volume numbering starts from 1.
Pseudo-divisions are decoded into their original real values.
>Action AGXUSER
*-----------------------------------------------------------------------------
>Command GSTOP
>Parameters
ABANDON 'electron stopping mode' I D=0
>Guidance
Low energy electrons and positrons, which have no chance to
reach the volume boundary, may be stopped using 2 different criteria.
There is no description currently available how it is done.
>Action AGXUSER
*-----------------------------------------------------------------------------
>Command GKINE
>Parameters
NTRACK  'Number of tracks per event, -1 for tape input ' I D=1  R=-10:10000
ID      'Particle ID or number of events to skip on input'  I
+
PTLOW   'Lower limit of pT'          R         R=0.:100000.
PTHIGH  'Upper limit of pT'          R         R=0.:100000.
YLOW    'Lower limit of pseudorapidity'    R         R=-10.:10.
YHIGH   'Upper limit of pseudorapidity'    R         R=-10.:10.
PHILOW  'Lower limit of Phi'         R         R=-10.:10.
PHIHIGH 'Upper limit of Phi'         R         R=-10.:10.
ZLOW    'Lower limit of Z of Vertex' R
ZHIGH   'Upper limit of Z of vertex' R
option  'choise of Geant or PDG particle ID' C D='G'  R='G,P,E'
>Guidance
Generates particles with flat phase space distribution
(instead of input events from a file) or to provide a particle filter
parameters for external generator input.
.
Mandatory parameters are number of tracks per event and Geant particle ID for
particle in-line generation. Following optional parameters are
lower and upper bounds of pT, pseudorapidity and azimouth anlge intervals.
If no parameters are given, a single muon will be generated per event,
distributed uniformly in the interval 0<pT<10 GeV, -10<y<10, 0<phi<2pi.
.
To switch to PDG particle ID instead of the GEANT one, the options P or
E can be used (PDG particle ID may also be negative).
In this case user can switch from the transverse momentum range
definition (option "P", Pt range) to total energy range definition
(option "E", Energy range).

If NTRACK equal -1 events are read from P input stream instead of
been generated. In this case the second parameter is the initial
event number.
.
Other parameters are use as a filter to select particles fed into
simulations. When used as filter, GKINE command should preceed
file openning command.

>Action AGXUSER
*-----------------------------------------------------------------------------
>Command TFLT
>Parameters
YLOW    'Lower limit of pseudorapidity'    R         R=-10.:10.
YHIGH   'Upper limit of pseudorapidity'    R         R=-10.:10.
+
PHILOW  'Lower limit of Phi'               R         R=-10.:10.
PHIHIGH 'Upper limit of Phi'               R         R=-10.:10.

>Guidance
Install a filter on input particle parameters. Particles read on input
with their rapidity [phi] outside these limits will not be loaded into
GEANT KINE bank

>Action AGXUSER
*-----------------------------------------------------------------------------
>Command GMOMENTUM
>Parameters
NTRACK  'Number of tracks per event, -1 for tape input ' I D=1  R=-10:10000
ID      'Geant Particle ID, first event for tape input'  I D=5  R=0:1000
+
PxLOW   'Lower limit of x-momentum'  R         R=-10000:10000
PxHIGH  'Upper limit of x-momentum'  R         R=-10000:10000
PyLOW   'Lower limit of y-momentum'  R         R=-10000:10000
PyHIGH  'Upper limit of y-momentum'  R         R=-10000:10000
PzLOW   'Lower limit of z-momentum'  R         R=-10000:10000
PzHIGH  'Upper limit of z-momentum'  R         R=-10000:10000
ZLOW    'Lower limit of Z of Vertex' R         R=-10000:10000
ZHIGH   'Upper limit of Z of vertex' R         R=-10000:10000
option  'choise of Geant or PDG particle ID' C D='G'  R='G,P,E'
>Guidance
 Generates particles in a given momentum bin
or to provide a particle filter
parameters for external generator input.
.
Mandatory parameters are number of tracks per event and Geant particle ID for
particle in-line generation, or -1 and first event number for
input event stream.
Following optional parameters are
lower and upper bounds of Px, Py and Pz.
If no parameters are given, a single muon will be generated per event,
distributed uniformely in the interval -1<Px,Py,Px<1 GeV.
.
To switch to PDG particle ID instead of the GEANT one, the options P or
E can be used.
.
If NTRACK equal -1 events are read from P input stream instead of
been generated. In this case the second parameter is the initial
event number.
Other parameters are use as a filter to select particles fed into
simulations.
>Action AGXUSER
*-----------------------------------------------------------------------------
>Command GFILE
>Parameters
+
streamtype  'I/O stream and data type combined in one word'  C  D='P'
file            'name of the file'                   C  D='ZEBRA'
sets            'list of data sets to read/write'    C  D='*'
>Guidance
Open an input or output data file with events.

 Stream. Different I/O streams are:
  P      - physics events input stream
  B      - background events to pile up on top of physics events,
  I      - general input data (no geometry record in the beginning),
  O      - output data.

 Data types may be:
 ' ' - Standard GENZ Zebra format with Lrec=8100 (default),
  Z  - Standard GEANT FZ format with Lrec=900 words,
  L  - Records include Length (Fortran-type OPEN used),
  F  - Fatmen catalog used to access Zebra tapes,
  N  - special Colomn Wise Ntuple with GENZ-type events for input,
  U  - user defined data type. In this case user has to supply his own
       routines AGUSREAD(ier) and to link it dinamically using gexec.
       The input file is opened by default on unit 20 as FORMATTED.
       For a user-specific file openning user has to provide his own
       routine AGUSOPEN(file) and to link it dinamically using gexec.
  1  - read only till the first ZEBRA End_of_file
       (default is a multy-file input stream),
  C  - do not merge splitted or piled-up subevents, keep them in a chain
       (by default they are automatically merged in a single event).
  S  - Special event, i.e. First resulting pile-up event is saved 
       as a permanent pile-up HIT bank.
       Such event, saved once with a maximum pile-up needed, will be added on
       top of normal mixed pile-up events to get huge pile-up in a fast way.
  M  - Multiple try to bypass latency of mass storage. Do not use this option
       if you are not sure the file is there.
  W  - Wildcard numbers, i.e. all digits within the filename are sequentially
       incremented to find valid filenames. The path part is not modified. 
  T  - LisT driven input, i.e. this is the lisT if input files to loop over. 

File: is the name of the data file.
It may include directory path and wildcard characters.
Default extensions for the default file name are P, B and O.

 Sets: list of data sets to be read or written:	
 G - GEANT detector geometry plus DETM bank
     (unless they are already loaded or created)
 E - GENZ RUNT/EVNT banks with parton level information,
 K - Geant KINE/VERT/HEAD information,
 H - HIT banks,
 D - DIGI banks,
 R - reconstruction banks,
 * - all above (default, NO SIMULATION will be done!).

 Note:
 (1) GEANT simulations are allowed ONLY if the input does not contain
hits or digits (i.e. simulations are disallowed by the default list!).
 (2) Internal data structure description (rz-database) is always updated
automatically on P stream.
To force a manual update use STRUCTURES command with 2 blank arguments.
To prevent the rz-database update, use I-input stream with sequential
TRIG comand.
To update include files use the default STRUCTURES commands.

>Action AGXUSER
*-----------------------------------------------------------------------------
>Command GHIST
>Parameters
+
file         'name of the file'               C  D='atlas.his'
directory    'rz-directory for n-tuples'      C  D='SLUGRZ'
unit         'logical unit number'            I  D=33
LRECL        'Record length in words'         I  D=1024
NRECP        'Maximum record allocation'      I  D=1024

>Guidance
Open a histogram output file. This file is used to keep
disk resident N-tuples and to save all histograms at the end
of run.
Default parameters allows maximum file length about 4 Gbytes
(30 times more then a standard HBOOK file). Note, that most
UNIX systems still limit the filesize below 2Gbytes.

 Before opening a disk resident N-tuple, user should do
 the following directory setting:

    Call RZCDIR('//SLUGRZ',' ')
    Call  HCDIR('//SLUGRZ',' ')

 At the end of the run you should do EXIT command to finish GEANT -
 this will do the actual histogram saving.

>Action AGXUSER
*-----------------------------------------------------------------------------
>Command GBACK
>parameters
+
Nbefor  'number of bunchs to add prior to the trigger one' I D=0
Nafter  'number of bunchs to add after the trigger one'    I D=0
BgMult  'Average pile-up multiplicity of bunch crossing'   R D=23
BgTime  'Time between bunch crossings (in ns)'             R D=25
BGSkip  'average number of skept events for randomizing'   R D=1

>Guidance
Secondary event stream 'B' use used to put pile-up pre-simulated
events on top of a physics event.
Background data should be in GEANT or GENZ format (non splitted).
Interaction time, vertex and track numbers of each secondary event
are updated upon read.

>Action AGXUSER
*-----------------------------------------------------------------------------
>Command GRFILE
>parameters
+
file    'geometry file name'       C  D='geom.rz'
inout   'Input or Output command'  C  D='OUT'      R='IN,OUT'
unit    'Unit number'              I  D=30
>Guidance
read in or write out (default) a geometry file in rz-format.

>Action AGXUSER
*-----------------------------------------------------------------------------
>Command GCLOSE
>Parameters
+
Option 'call GPHYSI after geometry closing' C
>Guidance
Close GEANT geometry building phase. Should be done before any graphics
and/or simulations are started, otherwise ZEBRA memory problems may arise.
If any parameter is given, GPHYSI is also called to calculate cross-sections.
In DEBUG OFF mode the output of GPHYSI is rederected into LUN 99.

>Action AGXUSER
*-----------------------------------------------------------------------------
>Command GDROP
>Parameters
+
Option 'bank drop choice' C D='*'
>Guidance
Drops a selected bank tree or a set of bank trees according to the
following list:
 P - Particle structure PART
 A - material structure MATE
 M - medium structure TMED
 - - drops all three above mentioned structures

 V - Volume structures VOLUM and GPAR, Rotation matrices
 S - sets, hits and digits SETS, HITS and DIGI
 D - detector description bank DETM
 R - event raw data RAWD and reconstruction bank RECB
 E - the whole short-range event division
 * - drops all listed above except for the first three.

>Action AGXUSER
*-----------------------------------------------------------------------------
>Command GEXEC
>Parameters
file        'names of files or directories'         C

>Guidance
Compile, link as one shared library, load and execute a user code,
stored in selected source files, in a directory or in a set of directories.
The resulting shared library is named by the first name in the list.
A subroutine with the same name as the library is executed,
unless the library contains a name_init or name_start entry,
which have the precedence.

>Action AGXUSER
*-----------------------------------------------------------------------------
>Command GMAKE
>Parameters
+
library     'additional keyword parameters for make'   C  D=' '

>Guidance
supply optional parameters for the make procedure, may be with
keywords (examples: debug  LIB_PATH=... LIBS=...)

>Action AGXUSER
*-----------------------------------------------------------------------------
>Command GSTAT
>Parameters
+
key   'name of the selected histogram'  C   D='ALL'
>Guidance
 Book a standard set of the GEANT control histogram:
 TIME -  Time per event
 SIZE -  Space used in IXDIV per event
 MULT -  Total number of tracks per event
 NTRA -  Long life tracks per event
 STAK -  Maximum stack size per event
>Action AGXUSER
*-----------------------------------------------------------------------------
>Command STRUCTURES
>Parameters
+
system  'name of the system whose structures should be dumped' C  D=' '
type    'type of the output'                                   C  D='def'

>Guidance

Produce a definition file with data structure description and
update the documentation database (detm.rz) in accordance with the
structures currenly loaded in the program.

System name argument is interpreted in the same way as the argument in
the UNIX 'ls' command, for example:
 - no name at all produces a single output file (detmsys)
with all structure descriptions in it;
 - '*' produces a set of system-based output files (*sys)
with its related structure description;
 - sys/name produces a single structure definition.

Type of the output may be

  def   - AGI preprocessor input file (default),
  idl   - CORBA interface definitions language,
  other - internal table format

The detmsys.def output may be automatically read by the AGI parser so that user
can get access to structure description with +cde or +include statements.
CORBA idl file should be processed by STIC compiler to produce .inc and .h
files. Internal definition file is directly fed to table access module.

>Action AGXUSER
*-----------------------------------------------------------------------------
>Command TABLES
>Parameters
+
system   'name of the system to be converted into table'  C  D=' '
dataset  'output directory'                               C  D=' '

>Guidance
Makes AGI structures, which belong to a particular AGI system, visible as
StaF tables in the requested dataset. Apropriate STAF table descriptions
is derived from AGI description and actual table adresses are mapped to AGI
structures. Default value means "all system".
A subset of structures can also be convertes into tables
by defining the path to them.

By default, all tables are created in /dui/Run, but
destination can be redirectded to another dataset.
If the dataset does not exist yet, it will be created
(but not the whole path!)

>Action AGXUSER
*-----------------------------------------------------------------------------
>Command TABCLEAR
>Parameters
+
dataset  'directory to clear'                       C  D='.'

>Guidance
Clear (reset row conters) all tables in the selected directory and below it.

>Action AGXUSER
*-----------------------------------------------------------------------------
>Command ONFAULT
>Parameters
+
fault   'name of the arithmetic fault signal flag'         C D='IDO'
counter 'number of faults to catch '                       I D=1
handler 'name of an optional user error handler routine'   C D=' '

>Guidance.

Catch certain number of arithmetic faults of selected type and print
tracing diagnostic. To provide a meaningful diagnostic, user code should
be compiled with '-g' option (unfortunatly, not all of the cernlib
routines are compiled with it!).

 - fault parameter:

List of possible fault types is computer dependant.
Here we describe the HPUX version only, which can detect:
(I) - illegal instruction, (D) - division by 0,
(O) - floating overflow, (U) - floating underflow,  and (X) - inexact numbers.
Last two happens very often and should not be normally considered as errors.
In addition (*) subsitutes all five flags, (+) means do not alter flags other
then mentioned in the command, which otherwise are reset to IGNORE.
If a fault type was never mentioned in any ONFAULT commands and they all
had +'s, corresponding error handler is still activated with the default
behaviour defined in libm.a.

 - counter meaning:

A positive counter sets the number of extended error messages to be printed,
the rest is counted in the common block /agerrorcount/ nnum(5),mmax(5),
but not reported. Error handler is permanently activated.

A zero counter forces the program to simulate a kuip break after the error
message is printed. If this happens in a macro, executed on a kuip prompt,
the kuip will issue the prompt again. If this happens in macro started in
a command line, program will stop.

If the counter is negative, corresponding faults are completely ignored
(not even counted).

 - handler:

If defined, a user handler routine is called instead of the
standard CERNLIB tracing routine.

>Action AGXUSER
*-----------------------------------------------------------------------------
>Command ABEND
>Parameters
>Guidance
 abort the program - the fastest possible exit
>Action AGXUSER
*-----------------------------------------------------------------------------
>Command REBANK
>Guidance

Bank access mechanism is implemeted as it is described in the
Atlas note SOFT-NO-002. The only correction to the note is the call
to the REBANK itself, which now has an additional parameter Ia:

          call REBANK (Path,IDN,Npar,Link*,Ia*)

The returned value of Ia contains the displacement in the bank,
for a single raw request. Remember that this routine is not intended
for general usage and should be avoided - use FILL/USE operators or
RBSTORE/RBCOPY, RBGET/RBPUT routines instead.
*-----------------------------------------------------------------------------
>Command FILL
>Guidance

FILL operatore is fully described in the AGI manual (Atlas note SOFT-NO-14).
Here we provide some additional details and helpful hints.

 Although names of the variables in AGI structures are limited to 12
characters, DZDOC suports only 8 characters in the documentation.
That means that in the structure defintion, produced by AGI (see also
STRUCTURES and TABLES commands), all names will be TRUNCATED to 8 symbols.
To avoid this complification, user is advised to use in structures
variable names not longer than 8 charactes.

>Action AGXUSER
*-----------------------------------------------------------------------------
>Command USE
>Guidance

Here we provide some general guidance in addition to the Atlas note
SOFT-NO-014, for using USE operator in the reconstruction code.

 the complete format of the USE operator is

 USE Path [ variable=value ] [ OPER={DELETE/UNIQUE/NEXT/ZERO} ] [ STAT=istat ]

All fields apart from Path are optional.

  Path : is the Unix-like path to the selected bank or a chain of banks
         It may include after bank names integer indeces
         If omitted, default value for index is supposed to be 1.

         The Path may be absolute, i.e. starting from /DETM or /RECB, in
         which case the selected bank becomes the Current Working Directory
         or relative, i.e. starting from the current working directory,
         selected by the previous USE operator.
         Module names in the begining of the path are recognized as
         the absolute path in DETM tree.

  OPER:  Different operations are :
   DELETE - deletes the selected bank from the tree AFTER copying its
            content in the corresponding structure
   ZERO   - reset the bank content to ZERO (agaim AFTER coping)
   UNIQUE - moves the selected bank to the first position in the chain
            and drops all other banks in this chain making a unique
            version available for the further analysis.
   NEXT   - select the next bank in the chain without searching the
            path again. If STAT option is used, this is done only
            if the status control variable is OK [0], otherwise
            a normal search in the path is done, thus allowing
            to select a starting bank and its descendent in the
            same USE

  STAT=ISTAT returns in ISTAT the bank access status (0 if OK).
             Initial value of ISTAT should be defined with a DATA statement.

If structure description in the program is different from the one in ZEBRA
memory, local copy is filled correctly using documentation. New variables,
not present in the bank, are left intact.

 Example: suppose a bank has been created by the following module:
 --------
      module    somegeo is a system
      author    me
      created   today
      integer   iprin
      structure MFLG { version, int Bfield(2,3), RmaxInn, ZmaxInn ,char title}
      fill MFLG(1)                  !  system data
          version =1                   !  bank version
          Bfield  = {1,2; 4,5; 11,12}  !  some field value
          RmaxInn = 100                !  max rad
          ZmaxInn = 200                !  max len
          title   = 'abcdef'           !  some 4-letter text
      end

 Then you try to read it with the following subroutine:
 -----
      subroutine sometest
      integer   iprin,istat/0/,i
      structure MFLG { int version, char title, int aaa,
                       Bfield(3,2), RmaxInn, ZmaxInn }
      begin
      mflg_aaa  = 999
      use  SOMEGEO/MFLG
      print *, ' version, aaa, ZmaxInn, title, Bfield: '
      print *,mflg_version,mflg_aaa,mflg_ZmaxInn,' ',mflg_title,mflg_Bfield
      end

 Result will be the following:
 ------
 Schema evolution for Bpath=/DETM/SOME/MFLG* bank=MFLG:
  version, aaa, ZmaxInn, title, Bfield:
   2  999    200.000 abcd    1.00000    2.00000  0.    4.00000    5.00000  0.

>Action AGXUSER
*-----------------------------------------------------------------------------
*>Command DBSET
*>Parameters
*+
*user   'username for database connection: "reader" or "writer"'  C D='reader'
*passwd 'password for database connection, required for writing'  C D=' '
*server 'ip-name of computer running database server' C D='atlassw1.phy.bnl.gov'
*dbname 'name of the database to use (for writing use "test_NOVA")' C D='NOVA'

*>Guidance.
*should provide a guidance
*>Action DBUSER
**-----------------------------------------------------------------------------
*>Command DBLS
*>Parameters
*+
*directory 'directory (=system) name for listing' C D='ATLSGEO'
*
*>Guidance.
*should provide a guidance
*>Action DBUSER
*-----------------------------------------------------------------------------
>Command GENZ
>Guidance
                  *************************
                  *  preliminary version  *
                  *  send your comments   *
                  *       to Pavel        *
                  *************************

GENZ is a package for the handling of the events, sets of particles
and partons, as output by physics event generators.
It has been written for by Robert DeWolf in 1990-1992 and is
fully described in the GENZ manual (1).
The version, described here, maintains the complete functionality
of the original package, but reflects the nature of its usage
and the experience within ATLAS.



GENZ provides tools to access any component of generator
events from  user's code.
Particle data, identities, 4-momenta, vertex position, etc., as well as
the relational information between particles, is readily available.

GENZ provides also a means of reading and writing generator events
stored in the HEPEVT common block (2).

In addition, GENZ provides an a nice event dumping utility for listing
the current event to the standard output.


GENZ was designed as a slave package, hence it is initialised
by the framework itself within avalable ZEBRA memory.
All input/output is also initialised and performed by the
framework itself (see GFILE commands)

Following the general logic, all input events are connected
to a single primary link, with the identification preserved
inside banks, so the first parameter in each call (ILINK)
in never used.

However, to preserve code re-usabilty, we still keep ALL
parameters as they are described in the original manual.

*** Attention **** The only ATLAS-specific change in parameters,
which differs from the GENZ manual, concerns subroutine GNZGETP,
where an extra parameter (VRTX) was added on request from Daniel.

 The subroutine calls that are of interest to GENZ users are given here.
 -----------------------------------------------------------------------

 CALL GNZPRIN(ILINK,LEVEL)

The contents of the current GENE/GENP banks are printed
in a nice format, making this a useful event dump utility.
The contents of the GENR bank are printed if there has been
a new run since the last GNZPRIN call.


 CALL GNZTOHC(IRET)

 Information in the GENZ banks is translated into HEPEVT common block


 Get Run Information

 CALL GNZGETR(ILINK,JIDGN,IGRUN,IGTIM,IGDAT,CMACH,CGENE,VRGEN,IDATG,VRGNZ,VRZEB)

 Return global run information from the GENR bank in link set ILINK.

 Input Parameters
 ILINK  Link set specifier - not used

 Output Parameters
 JIDGN  Original generator run's job ID
 IGRUN  run number as specified in the original generator run
 IGTIM  time of the generator run as hhmmss
 IGDAT  date of the generator run as yymmdd
 CMACH  generator run platform (CHAR*4)
 CGENE  generator name (CHAR*4)
 VRGEN  generator version as vv.ssss (REAL*4)
 IDATG  date of the generator version as yymmdd
 VRGNZ  GENZ version (REAL*4)
 VRGNZ  Zebra version (REAL*4)


 Get Gate Information:

 GNZGETG(ILINK,NEVEN)

 Return information on the current gate (that is , the current GENE
 linear structure) in link set ILINK.

 Input Parameters
 ILINK - Link set specifier - not used

 Output Parameters
 NEVEN - Number of events in the current gate


 Get Event Information:

 GNZGETE(ILINK,IDN,NPART,IRUN,IEVT,CGNAM,VERT,IWTFL,WEIGH)

 Return information on the IDN'th event in the present gate in link set ILINK

 Input Parameters
 ILINK - Link set specifier - not used
 IDN   - ID of the GENE bank (Zebra IDN of the bank in the GENE
        linear structure).

 Output Parameters
 NPART - Numbers of particles or partons in event record
 IRUN  - Run number from generator run
 IEVT  - Event number from generator run
 CGNAM - Generator name (CHARACTER*4)
 VERT  - 4-vector given the primary vertex position of the event
 IWTFL - weight flag
 WEIGH - event weight



 Get Particle Information:

 GNZGETP(ILINK,IDN,IP,ISTAT,IDPDG,P,AMASS,MOTH,TIME,IDAU1,VERT)

Return information from the particle record specified by ILINK,IDN,IP.
If particle record does not exist, (or if ISTAT = 0),
returned ISTAT is equal to zero.

 Input Parameters
 ILINK Link set specifier - not used

 IDN - ID of the GENE bank
 IP  - particle record to read.

 Output Parameters
 ISTAT    - particle status
 IDPDG    - particle code (Particle Data Group code)
 P(4)     - 4-momentum of particle (in GeV )
 AMASS    -  mass of particle (in GeV/$c~{2$)
 MOTH(2)  - pointers to mother records
 TIME     - Start time of particle relative to interaction,in seconds.
 IDAU1    - pointer to first daughter
 VRTX(4)  - production vertex and time


 Get vertex information ???:

 CALL GNZGETV(ILINK,IDN,IP,VSTAR,IMVRT,PRET,AROT,ROTMA)

 Obtain the start point of a particle specified by ILINK,IDN,IP.
 This is found by tracing back from particle IP until zero time

 The index of the particle decaying at the returned vertex point is
returned in IMVRT.
This may not be the immeditate mother particle of IP,
which can be found by calling GNZGETP.
It is rather the first particle in the ancestor traceback.
IMVRT is also used as an error flag for bad input argument values
and other errors.
In these cases, there is always accompanying printout.

 The remaining output parameters are of interest to users who
have set a (uniform) magnetic field using GNZPARR.  ???
PRET is the 3-momentum of particle IP rotated according to the
deflections of its ancestor particles in the field.
AROT is the accumulated angle of rotation in radians about the magnetic field
axis up to but not including IP's flight.  ROTMA returns the rotation matrix
that can be used to rotate 3-vectors, for instance using the utility GNZROTA.

 Input Parameters
 ILINK   Link set specifier - not used
 IDN   - ID of event in the gate
 IP    - particle member number

 Output Parameters
 VSTAR start vertex position and time, x,y,z,t. This is returned
       relative to the interaction point in metres and seconds.
 IMVRT index of particle decaying at VSTAR (see notes)
        = 0   IP decays within TVMI of primary vertex
        = -1  Error condition
 PRET(3) 3-momentum of particle IP accounting for rotation in B-field
 AROT   angle of rotation in radians about field axis
 ROTMA(3,3)  rotation matrix for rotation in B-field


 Get Daughter Information

 GNZGETD(ILINK,IDN,IP,NDAUG,IDAUG)

 Return in array IDAUG up to NDAUG indices of daughter particles for particle
 IP in event IDN in link set ILINK.

 Input Parameters
 ILINK - Link set specifier - not used
 IDN   - IDN of the desired GENE bank
 IP    - number of the given mother particle
 NDAUG - Maximum number of daughters to return in IDAUG

 Output Parameters
 NDAUG Number of daughters returned in array IDAUG
   = 0  No daughters found
 IDAUG Array containing the member numbers of the daughter particles

 (1) R.Dewolf. GENZ -- Generated Event Handling using Zebra
 atlasinfo.cern.ch/Atlas/GROUPS/SOFTWARE/DOCUMENTS/GENZ_MANUAL/genz_manual.html

 (2) T. Sjostrand et al.,
 CERN Yellow Report 89-08, v.3, p.327.

 ps:  HCTOGNZ,  GNZPUTE,  GNZPUTG,  GNZPUTP,  GNZPUTV,  GNZPUTM,
      can be readily done available if requested

>Action AGXUSER
*-----------------------------------------------------------------------------
>COMMAND HIGZ

>Parameters
+
IGH 'Graphic workstation type' I D=1

>GUIDANCE 
Open a HIGZ window (if it was not opened initially).

If program was started without a graphic window (using -w 0),
it is still possible to open it later. 
In this case window can be used by DZDOC or GEANT graphics.
HBOOK package still remains in AlphaNumeric mode.
For all possible workstation types see appendix B to HIGGZ manual
(for X11  1 to 10 are used).

>Action AGXUSER
*-----------------------------------------------------------------------------
>COMMAND DRAWONETREE
>GUIDANCE
Draw tree below a selected bank. All banks belonging to the
tree will be actually lifted in memory each with three data
words. Word 1, 2, 3 are the number of data words, links and
structural links as described in the documentation, a -1 indicates
a variable number. A global title may be given which appears
on the front page of the document.
Output option:
               'P' or ' ' PostScript file (default)
               'L'        Latex file
               'H'        HTML (hypertext) file
               'Q'        quiet i.e. generate no output files
               'C'        check consistency
               'S'        suppress confirmation
               'M' Put as many down banks as posibble on one picture
                   (Per default 2nd level are only drawn if all fit)

>PARAMETERS
CHBSBK 'Hollerith Id of selected bank' C
CHBSUP 'Hollerith Id of its up-bank' C
+
CHMETA 'Name of temp plot file' C D=' '
CHSGML 'Name of temp text-file' C D=' '
CHPOST 'Name of PostScript file' C D=' '
CHOPT  'Option (P=PostScript L=Latex) ' C D=' '
CTITLE 'Global title' C D='ZEBRA-Datastructures'
>ACTION DZEDRW

*-----------------------------------------------------------------------------
>COMMAND FPU_CONTROL

>Parameters
+
mask  'FPU control word, octal'  I  D=1577

>GUIDANCE 

Show or change the Intel Processor Control Word using the fortran interface
( Courtesy of Anders Waananen and Jorgen Beck Hansen ):

 FPUGETCW(CW)    : Get the FPU control word in CW
 FPUSETCW(CW)    : Set the FPU control word from CW
 FPUSTACK(FPUTAG,FPUSTATUS,FPUUSED,FPUOVERFLOW) : where
     FPUTAG      : FPU TAG
     FPUSTATUS   : FPU status word
     FPUUSED     : Number of stack registers used. 0 is OK.
     FPUOVERFLOW : 0 = OK, 1 = Stack overflow occurred (FPUUSED>8).

The Intel FPU Control word allows to get the program interupted on various
arithmetics conditions. By default, all interrupts are masked. Resetting 
of some of the mask bits allows for more vigorous arithmetics testing. 
We recommend to set the Control Word at least to 1563 to react on 
Zero Division and Overflow.
If no parameters are given, the FPU Control word and FPU Stack Status
as shown above are printed. 
As calling FPUSTACK affects the FPU control word by setting 
the lowest 7 bits incidentally disabling all interrupts (Why?),
we get CW before and then re-set it after FPUSTACK is called.

More details can be found in  /usr/include/fpu_control.h or running
"info -f g77 Trouble Missing Floating" or in Intel reference manuals.
The following is an abstract from fpu_control.

 The hardware default is 0x037f (o1577), we recommend 0x0373 (o1563)

 * Mask bit: 1 means no interrupt              default   recommeded
 * 0     IM: Invalid operation mask              (1)         (1)
 * 1     DM: Denormalized operand mask           (1)         (1)
 * 2     ZM: Zero-divide mask                    (1)         (0)
 * 3     OM: Overflow mask                       (1)         (0)
 * 4     UM: Underflow mask                      (1)         (1)
 * 5     PM: Precision (inexact result) mask     (1)         (1)
 * 6-7     : reserved                           (01)        (01)
 * 8-9   PC: Precision control:                 (11)        (11)
 *       11 - round to extended precision
 *       10 - round to double precision
 *       00 - round to single precision
 * 10-11 RC: Rounding control                   (00)        (00)
 *       00 - rounding to nearest
 *       01 - rounding down (toward - infinity)
 *       10 - rounding up (toward + infinity)
 *       11 - rounding toward zero
 * 12    IC: Infinity control for 8087 and 80287 (0)         (0)
 * 13-15     reserved                           (00)        (00)     

>Action AGXUSER
*-----------------------------------------------------------------------------
>COMMAND BUG_REPORT
>Parameters
>GUIDANCE 
 send a bug report to nevski@cern.ch
>ACTION AGXUSER
*-----------------------------------------------------------------------------
* Leave in for backwards compatability
**>Command GFILTER
**>Parameters
**filterName 'User defined filtername' C
**>Guidance
**
** GFILTER command provides a way to install user defined filter.
** This filter is defined on StMCFilter class. It has two rejection
**  methods. One for EG (Event Generator) and G3 (End of GEANT3 event)
** Before this command, library containing filter must be loaded 
** Example.
** GEXEC $STAR_LIB)/StMCFilter.so
** GFILTER  myFilterName
**
**>Action AGXUSER
*-----------------------------------------------------------------------------
>Menu AgFilter
>Guidance
*-----------------------------------------------------------------------------
>Command GFILTER
>Parameters
filterName 'User defined filtername' C
>Guidance

 GFILTER command provides a way to install user defined filter.
 This filter is defined on StMCFilter class. It has two rejection
 methods. One for EG (Event Generator) and G3 (End of GEANT3 event)
 Before this command, library containing filter must be loaded 
 Example.
 GEXEC $STAR_LIB)/StMCFilter.so
 GFILTER  myFilterName

>Action AGXUSER
*-----------------------------------------------------------------------------
>Command GCONFIG
>Parameters
filterKey 'User defined filter cut' C
filterVal 'Value of the cut'        R
>Guidance

The GFILTCFG command passes a key and a value to the filter's parseConfig
method, if it is defined.  The parseConfig method may then set various
cuts in the filter according to the key/value pair which is provided.

>Action AGXUSER

