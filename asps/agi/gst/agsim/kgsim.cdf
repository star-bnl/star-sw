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
    *                      04-Sep-98 hot news:                         *
    *                                                                  *
    *                          CONTROL                                 *
    * Kuip command "ON ERROR GOTO label" will now react on:            *
    *      - End_of_DATA on P stream, Write_Error on O stream          *
    *      - time_left less than defined by GTIME command argument     *
    * On GHIST command some standard histogram handling is introduced  *
    *        with automatic histogram dump when EXITing the program.   *
    * Print control is now fully consistent with SLUG-DICE-ATRECON     *
    * CERNLIB 97a release is now the default library version.          *
    * Be aware that it requires the FILL attribute to be 0, otherwise  *
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
Defines an average vertex position for standalone particle generation.
>Action AGXUSER
*-----------------------------------------------------------------------------
>Command GSPREAD
>Parameters
X_sigma  'x-spread of the generated vertex' R D=0
Y_sigma  'y-spread of the generated vertex' R D=0
Z_sigma  'z-spread of the generated vertex' R D=0
>Guidance
Defines the vertex spread for standalone particle generation.
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
+
SET    'Set(subsystem) name'        C  D='*'
DET    '(sensitive) detector name'  C  D='*'
>Guidance
Prints selected GEANT object (hits, digits, sets,
KINE tracks, Vertices, Particles, Materials, Media,
Volumes, Rotation matrices)
using its name rather than the numeric ID.
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
ID      'Particle ID or number of events to skip on input'  I   R=0:100000
+
PTLOW   'Lower limit of pT'          R         R=0.:100000.
PTHIGH  'Upper limit of pT'          R         R=0.:100000.
YLOW    'Lower limit of rapidity'    R         R=-10.:10.
YHIGH   'Upper limit of rapidity'    R         R=-10.:10.
PHILOW  'Lower limit of Phi'         R         R=-10.:10.
PHIHIGH 'Upper limit of Phi'         R         R=-10.:10.
ZLOW    'Lower limit of Z of Vertex' R
ZHIGH   'Upper limit of Z of vertex' R
option  'choise of Geant or PDG particle ID' C D='G'  R='G,P,E'
>Guidance
Generates particles with flat phase space distribution
(instead of input events from a file) or to provide a particle filter
parameters for external generator input.
 
 Parameters are number of tracks per event, Geant particle ID for
particle in-line generation. Following optional parameters are
lower and upper bounds of pT, rapidity and azimouth anlge intervals.
If no parameters are given, a single muon will be generated per event,
distributed uniformly in the interval 0<pT<10 GeV, -10<y<10, 0<phi<2pi.
 
 To switch to PDG particle ID instead of the GEANT one, the options P or
E can be used.
 
  If NTRACK equal -1 events are read from P input stream instead of
been generated. In this case the second parameter is the initial
event number.
Other parameters are use as a filter to select particles fed into
simulations. When used as filter, GKINE command should preceed
file openning.
 
>Action AGXUSER
*-----------------------------------------------------------------------------
>Command GMOMENTUM
>Parameters
NTRACK  'Number of tracks per event, -1 for tape input ' I D=1  R=-10:10000
ID      'Geant Particle ID, first event for tape input'  I D=5  R=0:1000
+
PxLOW   'Lower limit of pT'          R         R=-10000:10000
PxHIGH  'Upper limit of pT'          R         R=-10000:10000
PyLOW   'Lower limit of rapidity'    R         R=-10000:10000
PyHIGH  'Upper limit of rapidity'    R         R=-10000:10000
PzLOW   'Lower limit of Phi'         R         R=-10000:10000
PzHIGH  'Upper limit of Phi'         R         R=-10000:10000
ZLOW    'Lower limit of Z of Vertex' R         R=-10000:10000
ZHIGH   'Upper limit of Z of vertex' R         R=-10000:10000
option  'choise of Geant or PDG particle ID' C D='G'  R='G,P,E'
>Guidance
 Generates particles in a given momentum bin
or to provide a particle filter
parameters for external generator input.
 
 Parameters are number of tracks per event, Geant particle ID for
particle in-line generation, or -1 and first event number for
input event stream.
 Following optional parameters are
lower and upper bounds of Px, Py and Pz.
If no parameters are given, a single muon will be generated per event,
distributed uniformely in the interval -1<Px,Py,Px<1 GeV.
 
To switch to PDG particle ID instead of the GEANT one, the options P or
E can be used.
 
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
Modif           'filename modification mode'         C  D=' '
num             'numbers for filename modification'  I
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
 S - data structure description (both include files and rz-database) is
     updated automatically as it is done with the default STRUCTURES commands,
 * - all above (default).
 
Note that GEANT simulations are allowed ONLY if the input does not contain
hits or digits (i.e. disallowed by default list!).
 
Modif (to be implemented on request):
  The way how the * wildcard character is replaced using the
  numbers which follow this parameter.
  Possible choice is LOOP or LIST (see software note 008).
>Action AGXUSER
*-----------------------------------------------------------------------------
>Command GHIST
>Parameters
+
file         'name of the file'               C  D='atlas.his'
directory    'rz-directory for n-tuples'      C  D='SLUGRZ'
unit         'logical unit number'            I  D=33
 
>Guidance
Open a histogram output file. This file is used to keep
disk resident N-tuples and to save all histograms at the end
of run.
 
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
>Command GEXEC
>Parameters
file        'name of the *.g file'                  C
+
library     'additional library path and libraies'  C  D=' '
 
>Guidance
compile, link as a shared library and execute a user code,
stored in a source file with extention .g, .f, .F, .c  or .cc.
The file may contain several subroutines.
The one which has the same name as the file is executed,
unless the file contains a file_init or file_start entry,
which have the precedence.
 
>Action AGXUSER
*-----------------------------------------------------------------------------
>Command GMAKE
>Parameters
source      'default path to the module source'        C  D='.'
+
name        'name of the makefile '                    C  D=' '
library     'additional keyword parameters for make'   C  D=' '
 
>Guidance
Redefine default path to the source for GEXEC command,
as well as the name of the makefile to be executed by GEXEC.
Additional parameters for the make procedure may be supplied with
their keywords (examples: LIB_PATH=... LIBS=...)
 
>Action AGXUSER
*-----------------------------------------------------------------------------
>Command VERSIONS
>parameters
+
Option 'program, component or data versions'   C    D='*'
>Guidance
This command can be used to steer version dependance,
which cannot be derived from the nature of input data.
 
List of known program version :
 
 RZ95, RZ96      - Different key formats in RZ files (affects RZ/FILE)
 ATLAS,STAF,DENS - Different packing versions for REBANK, affects
                   the number of user banks per single ZEBRA bank
 
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
 - no name at all produces a single output file (detmsys) with all structure descriptions in it;
 - '*' produces a set of system-based output files (*sys) with its related structure description;
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
 
Generates flat phase space in place of input file of events.
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
PID1 'Geant PID of first decay daughter' I D=5
PID2 'Geant PID of second decay daughter' I D=4
+
PID3 'Geant PID of third decay daughter, if present' I D=0
>Guidance
Initializes parameters for the decay of a particle with uniform
probability along its trajectory between the cylindrical surfaces
R=RIN and R=ROUT. This can be used to enhance statistics for the
efficiency studies of the reconstruction of decays. A new particle is
defined, having PIDNEW=PIDPARENT+200, and which does not have any
decay modes defined in Geant. When particle PIDPARENT appears in the
input list of kinematics, its pid is replaced by PIDNEW. It is
propogated from its origin through the surface R=ROUT, and then a decay
point is chosen uniformly along its trajectory between RIN and
ROUT. GSTAR explicitly creates a new vertex at this decay point, with
100% branching ratio for decay into the two or three daughters
defined, with kinematics defined appropriately (three-body decay
assumes scalar particles). Since the path length to the generated
vertex and the parent 4-momentum are known, the appropriate weighting
factor for the vertex due to the lifetime of the parent can be
calculated.
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
 
 
