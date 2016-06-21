      SUBROUTINE AGXINIT
      CHARACTER*1 BS1
      PARAMETER (BS1='\\')
      PARAMETER (MGUIDL=199)
      CHARACTER*80 GUID
      COMMON /KCGUID/ GUID(MGUIDL)
      EXTERNAL AGXUSER
      EXTERNAL DZEDRW

      CALL KUCMD(' ','AGUSER','C')
      GUID(1)='Advanced Geant Interface'
      GUID(2)='    *****************************************************
     +***************'
      GUID(3)='    *                                                   '
     +//'               *'
      GUID(4)='    *        A D V A N C E D   G E A N T   I N T E R F A'
     +//' C E           *'
      GUID(5)='    *                                                   '
     +//'               *'
      GUID(6)='    *                      04-Jan-00 hot news:          '
     +//'               *'
      GUID(7)='    *                                                   '
     +//'               *'
      GUID(8)='    *                          CONTROL                  '
     +//'               *'
      GUID(9)='    * Kuip command "ON ERROR GOTO label" will now react o
     +n:            *'
      GUID(10)='    *      - End_of_DATA on P stream, Write_Error on O s
     +tream          *'
      GUID(11)='    *      - time_left less than defined by GTIME comman
     +d argument     *'
      GUID(12)='    * On GHIST command some standard histogram handling'
     +//' is introduced  *'
      GUID(13)='    *        with automatic histogram dump when EXITing'
     +//' the program.   *'
      GUID(14)='    * Print control is now fully consistent with SLUG-DI
     +CE-ATRECON     *'
      GUID(15)='    * Since 97a CERNLIB release FILL attribute has to be
     + 0, otherwise  *'
      GUID(16)='    * edges drawn in black/white obscure most of the GEA
     +NT drawings !  *'
      GUID(17)='    * GEANT and PAW memory may be increased at the invoc
     +ation time -   *'
      GUID(18)='    * start with -h option to get more information how t
     +o use switches *'
      GUID(19)='    * "-b filename" option now available for batch mode.
     +               *'
      GUID(20)='    * To get complete description of AGI commands in pri
     +ntable format  *'
      GUID(21)='    *              do:  MANUAL AGUSER MAN.TEX LATEX    '
     +//'                *'
      GUID(22)='    *                                                  '
     +//'                *'
      GUID(23)='    *                       GEANT & PHYSICS:           '
     +//'                *'
      GUID(24)='    * GCALOR avalable for calorimeter simulations on com
     +mand: HADR 6   *'
      GUID(25)='    *        It needs bertaf.dat and xsneut95.dat from /
     +cern/95a/lib   *'
      GUID(26)='    * Electron physics (Bremsstraghlung, Pair production
     +) updated      *'
      GUID(27)='    * Absorption length calculation for mixtures correct
     +ed in GEANT.   *'
      GUID(28)='    * Explicit NCOPY parameter is now allowed in the POS
     +ITION operator *'
      GUID(29)='    * Binning refined: - for BIT option all 2**Nbit valu
     +es are used,   *'
      GUID(30)='    *                    unknown elements are supposed t
     +o be integer   *'
      GUID(31)='    *                  - for BIN option the interval is'
     +//' centered       *'
      GUID(32)='    * Protection against errenious hit limits (bug in GF
     +IPAR on SUN)   *'
      GUID(33)='    * Tracking in MANY volumes is corrected - no missing
     + hits anymore  *'
      GUID(34)='    *                                                  '
     +//'                *'
      GUID(35)='    *                       Data STRUCTURING:          '
     +//'                *'
      GUID(36)='    * RbGET counting request now returns again the corre
     +ct number      *'
      GUID(37)='    *       of banks in a chain, as it is described in S
     +OFT-NO-002     *'
      GUID(38)='    * CMZ KEEP sequences (car-format) are recognised as'
     +//' REPLACE macros *'
      GUID(39)='    * GENZ package added - LGNFIND,GNZGET* calls are ava
     +ilable         *'
      GUID(40)='    * USE operator has an OPERation NEXT, allowing bank'
     +//' chain scanning *'
      GUID(41)='    * Schema evolution is supported by USE operator - se
     +e help USE     *'
      GUID(42)='    *                                                  '
     +//'                *'
      GUID(43)='    ****************************************************
     +****************'
      CALL KUGUID('AGUSER',GUID,43,'S')

      CALL KUCMD('AGUSER',' ','SW')

      CALL KUCMD(' ','ACTIONS','C')
      CALL KUPAR('ACTIONS','Option','program actions allowed','OO','S')
      CALL KUPVAL('ACTIONS','Option',0,0.,'*','D')
      GUID(1)='This command can be used to overwrite the default program
     + actions,'
      GUID(2)='derived from the nature of input data.'
      GUID(3)='List of program actions, allowed in the run, is :'
      GUID(4)=' K - convertion GENZ banks to KINE'
      GUID(5)=' S - Geant simulations'
      GUID(6)=' D - Hit digitization'
      GUID(7)=' R - reconstruction'
      GUID(8)=' T - test beam data analysis.'
      GUID(9)=' O - automatic output at the end of TRIG command'
      GUID(10)=' * - all above is allowed'
      CALL KUGUID('ACTIONS',GUID,10,'S')
      CALL KUACT('ACTIONS',AGXUSER)

      CALL KUCMD(' ','VERSIONS','C')
      CALL KUPAR('VERSIONS','Option',
     +'program, component or data versions','OO','S')
      CALL KUPVAL('VERSIONS','Option',0,0.,'*','D')
      CALL KUPAR('VERSIONS','Value','optional parameter value','IO','S')
      CALL KUPVAL('VERSIONS','Value',0,0.,' ','D')
      GUID(1)='This command can be used to steer version dependance,'
      GUID(2)='which cannot be derived from the nature of input data.'
      GUID(3)='List of available program version :'
      GUID(4)=' ATLAS,STAF,DENS - Different packing versions for REBANK,
     + affects'
      GUID(5)='                   the number of user banks per single ZE
     +BRA bank'
      GUID(6)=' BATCH           - no errors are tolarated in I/O command
     +s (gexec,gfile)'
      GUID(7)=' INTER           - return to normal (interactive) error h
     +andling'
      GUID(8)=' FZLEN           - max length of output FZ file in Mwords
     + (512 is 2GB)'
      GUID(9)=' GTRACK          - generic keys to control tracking modes
     +.'
      GUID(10)=' RZ95, RZ96      - Different key formats in RZ files (af
     +fects RZ/FILE)'
      GUID(11)='                   (now recognised automatically'
      GUID(12)='For the moment the first key switches  GUSCNTR in GTRACK
     +,'
      GUID(13)='the second one switches additional call to GTMEDI in cas
     +e of MANY;'
      GUID(14)='The third one (3 digits) modifies logic of GTXXXX routin
     +es.'
      GUID(15)='Default setting (all 0s) corresponds to what is believed
     +'
      GUID(16)='today (05.10.00) to be the best choice - purely 99 cernl
     +ib'
      GUID(17)='tracking apart from diff=(Vout-Vnext) which is taken as'
     +//' abs(diff).'
      CALL KUGUID('VERSIONS',GUID,17,'S')
      CALL KUACT('VERSIONS',AGXUSER)

      CALL KUCMD(' ','RANLUX','C')
      CALL KUPAR('RANLUX','Seed','initial seed','IO','S')
      CALL KUPVAL('RANLUX','Seed',123456789,0.,' ','D')
      CALL KUPAR('RANLUX','Lux','luxury level','IO','S')
      CALL KUPVAL('RANLUX','Lux',1,0.,' ','D')
      CALL KUPVAL('RANLUX','Lux',-1,0.,' ','L')
      CALL KUPVAL('RANLUX','Lux',4,0.,' ','H')
      CALL KUPAR('RANLUX','N1','first part of sequence number','IO','S')
      CALL KUPVAL('RANLUX','N1',0,0.,' ','D')
      CALL KUPVAL('RANLUX','N1',0,0.,' ','L')
      CALL KUPVAL('RANLUX','N1',999999999,0.,' ','H')
      CALL KUPAR('RANLUX','N2','second part of sequence number','IO',
     +'S')
      CALL KUPVAL('RANLUX','N2',0,0.,' ','D')
      CALL KUPVAL('RANLUX','N2',0,0.,' ','L')
      CALL KUPVAL('RANLUX','N2',999999999,0.,' ','H')
      GUID(1)='Switch all random number generators known to the program'
      GUID(2)='from GRNDM (geant) to RANLUX (V115) and initialise it.'
      GUID(3)='For negative Lux switch back to GRNDM (default status)'
      CALL KUGUID('RANLUX',GUID,3,'S')
      CALL KUACT('RANLUX',AGXUSER)

      CALL KUCMD(' ','GDUMP','C')
      CALL KUPAR('GDUMP','PATH',
     +'Path to the selected sub-branch of banks','C','S')
      CALL KUPVAL('GDUMP','PATH',0,0.,'/DETM','D')
      CALL KUPAR('GDUMP','OPTION','Dump option (F,H,C,U or 1)','OO','S')
      CALL KUPVAL('GDUMP','OPTION',0,0.,' ','D')
      GUID(1)='Dumps the content of a selected sub-branch of ZEBRA banks
     +,'
      GUID(2)='addressed by the PATH.'
      GUID(3)='Possible options are:'
      GUID(4)=' F - dump in a file instead of the terminal. The bank nam
     +e is used as'
      GUID(5)='     the file name with .sgml extension'
      GUID(6)=' H - convert the dump file in HTML formated set of files'
     +//' with'
      GUID(7)='     apropriate hyperlinks beween them.'
      GUID(8)='     Each bank in the structure is described in a separat
     +e file'
      GUID(9)='     with the name of the bank and its top level bank as'
     +//' the filename'
      GUID(10)='     and with .html extension.'
      GUID(11)=' C - dump only numbers without variable names and commen
     +ts. This may'
      GUID(12)='     be usefull for an output intended to be read by ano
     +ther program.'
      GUID(13)=' U - dump also banks which have no documenation (normall
     +y they are skipped),'
      GUID(14)=' 1 - dump also long banks (more than 1000 word - normall
     +y they are skipped).'
      CALL KUGUID('GDUMP',GUID,14,'S')
      CALL KUACT('GDUMP',AGXUSER)

      CALL KUCMD(' ','MODE','C')
      CALL KUPAR('MODE','Detector','detector subsystem name','C','S')
      CALL KUPVAL('MODE','Detector',0,0.,'ALL','D')
      CALL KUPAR('MODE','Flag','control flag name','CO','S')
      CALL KUPAR('MODE','Value','flag value','IO','S')
      GUID(1)='Set control flags for a given detector subsystem (or for'
     +//' ALL of them).'
      GUID(2)='Possible flags and their default values are:'
      GUID(3)='       PNOW  (0)             - print level for current ev
     +ent'
      GUID(4)='       PRIN  (0)             - normal print level'
      GUID(5)='       DEBU  (0)             - debug print level'
      GUID(6)='       GEOM  (1)             - geometry version'
      GUID(7)='       HIST  (1)             - system histogram flag'
      GUID(8)='       GRAP  (1)             - system graphics level'
      GUID(9)='       SIMU  (1)             - store GEANT hits flag'
      GUID(10)='       DIGI  (1)             - digitisation flag'
      GUID(11)='       RECO  (1)             - reconstruction flag'
      GUID(12)='       MFLD  (1)             - magnetic field flag'
      GUID(13)='       ANAL  (0)             - user analysis level'
      GUID(14)='       BACK  (0)             - number of pile-up bunchs'
     +//' to select'
      GUID(15)='                               (relative to the trigger'
     +//' one)'
      GUID(16)='To change default values use GSFLAG command.'
      CALL KUGUID('MODE',GUID,16,'S')
      CALL KUACT('MODE',AGXUSER)

      CALL KUCMD(' ','DETP','C')
      CALL KUPAR('DETP','Detector','detector subsystem name','C','S')
      CALL KUPAR('DETP','NAME',
     +'name of the selected bank or of a variable in the bank','CO','S')
      CALL KUPAR('DETP','Value',
     +'value of the selector or new value of the variable','RO','S')
      GUID(1)=' DETP command provides a way to modify the content of par
     +ameter banks.'
      GUID(2)='When a USE operator is called for a bank for the first ti
     +me,'
      GUID(3)='it checks wether the bank name and the value of its selec
     +tor'
      GUID(4)='coincides with the one mentioned in the DETP command for'
     +//' the same detector.'
      GUID(5)='If this is the case, corresponding variables in the bank'
     +//' are replaced'
      GUID(6)='by the new value. Selector name and value is provided in'
     +//' brackets'
      GUID(7)='after the bank name. If no name (only a value) is provide
     +d,'
      GUID(8)='this refers to the selector in the USE operator itself.'
      GUID(9)=' Only one DETP command per detector is kept in the progra
     +m.'
      GUID(10)='The next command with the same detector name overwrites'
     +//' the previous one.'
      GUID(11)='On the other hand, any number of banks and their variabl
     +e'
      GUID(12)='can be changed by a single command (which can be extende
     +d'
      GUID(13)='over several lines following KUIP rules).'
      GUID(14)='When data are read from P stream, they are stripped out'
     +//' of'
      GUID(15)='old DETP commands.'
      GUID(16)=' Typing rules for  DETP parameters are the following:'
      GUID(17)=' - the bank selector value may be given in () or with a'
     +//' = sign.'
      GUID(18)=' - variable to change should have a trailing = sign.'
      GUID(19)=' - command is not case sensitive.'
      GUID(20)=' Example.'
      GUID(21)=' DETP CALO cgeo=1  rmin=200  rmax=300'
      GUID(22)=' DETP CALO cgeo(1).rmin=200  rmax=300'
      GUID(23)=' DETP CALO cgeo(version=1).rmin=200  rmax=300'
      GUID(24)='DETP command may be generated also from the code using t
     +wo'
      GUID(25)='subroutines:  AgDETPnew (Detector) and AgDETPadd (Name,V
     +alues,Nval).'
      GUID(26)=' Example. The previous command may be generated by:'
      GUID(27)=' Call AgDETP new (''CALO'')'
      GUID(28)=' Call AgDETP add (''cgeo(version=1).rmin='',200,1)'
      GUID(29)=' Call AgDETP add (''rmax='',300,1)'
      CALL KUGUID('DETP',GUID,29,'S')
      CALL KUACT('DETP',AGXUSER)

      CALL KUCMD(' ','GVERTEX','C')
      CALL KUPAR('GVERTEX','X_vertex','x of the generated vertex','R',
     +'S')
      CALL KUPVAL('GVERTEX','X_vertex',0,0.,' ','D')
      CALL KUPAR('GVERTEX','Y_vertex','y of the generated vertex','R',
     +'S')
      CALL KUPVAL('GVERTEX','Y_vertex',0,0.,' ','D')
      CALL KUPAR('GVERTEX','Z_vertex','z of the generated vertex','R',
     +'S')
      CALL KUPVAL('GVERTEX','Z_vertex',0,0.,' ','D')
      GUID(1)='Defines the average position of the simulated vertex at z
     +=0.  To provide a'
      GUID(2)='z-dependent offset, use the gslope command.'
      CALL KUGUID('GVERTEX',GUID,2,'S')
      CALL KUACT('GVERTEX',AGXUSER)

      CALL KUCMD(' ','GSPREAD','C')
      CALL KUPAR('GSPREAD','X_sigma','x-spread of the generated vertex',
     +'R','S')
      CALL KUPVAL('GSPREAD','X_sigma',0,0.,' ','D')
      CALL KUPAR('GSPREAD','Y_sigma','y-spread of the generated vertex',
     +'R','S')
      CALL KUPVAL('GSPREAD','Y_sigma',0,0.,' ','D')
      CALL KUPAR('GSPREAD','Z_sigma','z-spread of the generated vertex',
     +'R','S')
      CALL KUPVAL('GSPREAD','Z_sigma',0,0.,' ','D')
      GUID(1)='Defines the spread of the simulated vertex position.'
      CALL KUGUID('GSPREAD',GUID,1,'S')
      CALL KUACT('GSPREAD',AGXUSER)

      CALL KUCMD(' ','GSLOPE','C')
      CALL KUPAR('GSLOPE','X','z-dependence of the beamline','R','S')
      CALL KUPVAL('GSLOPE','X',0,0.,' ','D')
      CALL KUPAR('GSLOPE','Y','z-dependence of the beamline','R','S')
      CALL KUPVAL('GSLOPE','Y',0,0.,' ','D')
      GUID(1)='Provides the slope of the beamline vs z in the x and y di
     +rections.'
      GUID(2)='Example:'
      GUID(3)='   aguser/gspread 0.02 0.02 25.0'
      GUID(4)='   aguser/gvertex 0.01 0.10  1.0'
      GUID(5)='   aguser/gslope  2.5E-3 1.0E-6'
      GUID(6)='   implements a beamline constraint'
      GUID(7)='   x = 0.01 + (2.5E-3)*z'
      GUID(8)='   y = 0.10 + (1.0E-6)*z'
      CALL KUGUID('GSLOPE',GUID,8,'S')
      CALL KUACT('GSLOPE',AGXUSER)

      CALL KUCMD(' ','GSFLAG','C')
      CALL KUPAR('GSFLAG','Flag','AGI control flag','O','S')
      CALL KUPVAL('GSFLAG','Flag',0,0.,'PRIN','D')
      CALL KUPVAL('GSFLAG','Flag',0,0.,'PRIN,PNOW,GRAP,HIST,SIMU,MFLD',
     +'V')
      CALL KUPAR('GSFLAG','Value','flag value','IO','S')
      CALL KUPVAL('GSFLAG','Value',1,0.,' ','D')
      GUID(1)='Sets default value for control flags. These flags will be
     + used to'
      GUID(2)='provide a default value for all new detectors DETE (MODE)
     + bank.'
      GUID(3)='Flags mentioned here are only a subset of all flags,'
      GUID(4)='available in the MODE command.'
      CALL KUGUID('GSFLAG',GUID,4,'S')
      CALL KUACT('GSFLAG',AGXUSER)

      CALL KUCMD(' ','GFLAG','C')
      CALL KUPAR('GFLAG','Flag','AGI control flag','O','S')
      CALL KUPVAL('GFLAG','Flag',0,0.,'PRIN','D')
      CALL KUPVAL('GFLAG','Flag',0,0.,'PRIN,PNOW,GRAP,HIST,SIMU,MFLD',
     +'V')
      CALL KUPAR('GFLAG','Value','flag value','IO','S')
      CALL KUPVAL('GFLAG','Value',1,0.,' ','D')
      GUID(1)='Sets default value for control flags - same as GSFLAG'
      CALL KUGUID('GFLAG',GUID,1,'S')
      CALL KUACT('GFLAG',AGXUSER)

      CALL KUCMD(' ','GDEBUG','C')
      CALL KUPAR('GDEBUG','Flag','hard debug level','I','S')
      CALL KUPVAL('GDEBUG','Flag',1,0.,' ','D')
      CALL KUPAR('GDEBUG','Itest','random number print flag','IO','S')
      CALL KUPVAL('GDEBUG','Itest',0,0.,' ','D')
      GUID(1)='Set geant IDEBUG flag to value more than 1 for hard debug
     +ging'
      GUID(2)='and ITEST flag for random number printouts.'
      CALL KUGUID('GDEBUG',GUID,2,'S')
      CALL KUACT('GDEBUG',AGXUSER)

      CALL KUCMD(' ','GAUTO','C')
      CALL KUPAR('GAUTO','Option',
     +'automatic tracking parameter computation','O','S')
      CALL KUPVAL('GAUTO','Option',0,0.,'ON','D')
      CALL KUPVAL('GAUTO','Option',0,0.,'ON,OFF,0,1','V')
      GUID(1)='Set or reset flag for automatic tracking medium parameter
     + computation by GEANT.'
      GUID(2)='Equivalent to the standard GEANT AUTO flag (which also wo
     +rks now).'
      CALL KUGUID('GAUTO',GUID,2,'S')
      CALL KUACT('GAUTO',AGXUSER)

      CALL KUCMD(' ','GPRINT','C')
      CALL KUPAR('GPRINT','NAME','Object Name','C','S')
      GUID(1)='Prints selected GEANT object (hits, digits, sets,'
      GUID(2)='KINE or GENZ tracks, Vertices, Particles, Materials, Medi
     +a,'
      GUID(3)='Volumes, Rotation matrices)'
      GUID(4)='using its name rather than the numeric ID.'
      GUID(5)=' optional additional parameters are :'
      GUID(6)='   SET    ''Set(subsystem) name''        C  D=''*'''
      GUID(7)='   DET    ''(sensitive) detector name''  C  D=''*'''
      GUID(8)=' or:'
      GUID(9)='   NUM    ''track/vertex number''        I  D=0'
      CALL KUGUID('GPRINT',GUID,9,'S')
      CALL KUACT('GPRINT',AGXUSER)

      CALL KUCMD(' ','GPHITS','C')
      CALL KUPAR('GPHITS','CSET','subsystem (set) name','CO','S')
      CALL KUPVAL('GPHITS','CSET',0,0.,'*','D')
      CALL KUPAR('GPHITS','CDET','User detector name','CO','S')
      CALL KUPVAL('GPHITS','CDET',0,0.,'*','D')
      GUID(1)='Prints hits using CALL AGPDIGI(cset+H,cdet).'
      GUID(2)='Unlike the internal GEANT numbering, volume numbering sta
     +rts from 1.'
      GUID(3)='Pseudo-divisions are decoded into their original real val
     +ues.'
      CALL KUGUID('GPHITS',GUID,3,'S')
      CALL KUACT('GPHITS',AGXUSER)

      CALL KUCMD(' ','GPDIGI','C')
      CALL KUPAR('GPDIGI','CSET','subsystem (set) name','CO','S')
      CALL KUPVAL('GPDIGI','CSET',0,0.,'*','D')
      CALL KUPAR('GPDIGI','CDET','User detector name','CO','S')
      CALL KUPVAL('GPDIGI','CDET',0,0.,'*','D')
      GUID(1)='Prints digits using CALL AGPDIGI(cset+D,cdet).'
      GUID(2)='Unlike the internal GEANT numbering, volume numbering sta
     +rts from 1.'
      GUID(3)='Pseudo-divisions are decoded into their original real val
     +ues.'
      CALL KUGUID('GPDIGI',GUID,3,'S')
      CALL KUACT('GPDIGI',AGXUSER)

      CALL KUCMD(' ','GSTOP','C')
      CALL KUPAR('GSTOP','ABANDON','electron stopping mode','I','S')
      CALL KUPVAL('GSTOP','ABANDON',0,0.,' ','D')
      GUID(1)='Low energy electrons and positrons, which have no chance'
     +//' to'
      GUID(2)='reach the volume boundary, may be stopped using 2 differe
     +nt criteria.'
      GUID(3)='There is no description currently available how it is don
     +e.'
      CALL KUGUID('GSTOP',GUID,3,'S')
      CALL KUACT('GSTOP',AGXUSER)

      CALL KUCMD(' ','GKINE','C')
      CALL KUPAR('GKINE','NTRACK',
     +'Number of tracks per event, -1 for tape input','I','S')
      CALL KUPVAL('GKINE','NTRACK',1,0.,' ','D')
      CALL KUPVAL('GKINE','NTRACK',-10,0.,' ','L')
      CALL KUPVAL('GKINE','NTRACK',10000,0.,' ','H')
      CALL KUPAR('GKINE','ID',
     +'Particle ID or number of events to skip on input','I','S')
      CALL KUPAR('GKINE','PTLOW','Lower limit of pT','RO','S')
      CALL KUPVAL('GKINE','PTLOW',0,0.,' ','L')
      CALL KUPVAL('GKINE','PTLOW',0,100000.,' ','H')
      CALL KUPAR('GKINE','PTHIGH','Upper limit of pT','RO','S')
      CALL KUPVAL('GKINE','PTHIGH',0,0.,' ','L')
      CALL KUPVAL('GKINE','PTHIGH',0,100000.,' ','H')
      CALL KUPAR('GKINE','YLOW','Lower limit of pseudorapidity','RO',
     +'S')
      CALL KUPVAL('GKINE','YLOW',0,-10.,' ','L')
      CALL KUPVAL('GKINE','YLOW',0,10.,' ','H')
      CALL KUPAR('GKINE','YHIGH','Upper limit of pseudorapidity','RO',
     +'S')
      CALL KUPVAL('GKINE','YHIGH',0,-10.,' ','L')
      CALL KUPVAL('GKINE','YHIGH',0,10.,' ','H')
      CALL KUPAR('GKINE','PHILOW','Lower limit of Phi','RO','S')
      CALL KUPVAL('GKINE','PHILOW',0,-10.,' ','L')
      CALL KUPVAL('GKINE','PHILOW',0,10.,' ','H')
      CALL KUPAR('GKINE','PHIHIGH','Upper limit of Phi','RO','S')
      CALL KUPVAL('GKINE','PHIHIGH',0,-10.,' ','L')
      CALL KUPVAL('GKINE','PHIHIGH',0,10.,' ','H')
      CALL KUPAR('GKINE','ZLOW','Lower limit of Z of Vertex','RO','S')
      CALL KUPAR('GKINE','ZHIGH','Upper limit of Z of vertex','RO','S')
      CALL KUPAR('GKINE','option','choise of Geant or PDG particle ID',
     +'OO','S')
      CALL KUPVAL('GKINE','option',0,0.,'G','D')
      CALL KUPVAL('GKINE','option',0,0.,'G,P,E','V')
      GUID(1)='Generates particles with flat phase space distribution'
      GUID(2)='(instead of input events from a file) or to provide a par
     +ticle filter'
      GUID(3)='parameters for external generator input.'
      GUID(4)=' '
      GUID(5)='Mandatory parameters are number of tracks per event and G
     +eant particle ID for'
      GUID(6)='particle in-line generation. Following optional parameter
     +s are'
      GUID(7)='lower and upper bounds of pT, pseudorapidity and azimouth
     + anlge intervals.'
      GUID(8)='If no parameters are given, a single muon will be generat
     +ed per event,'
      GUID(9)='distributed uniformly in the interval 0<pT<10 GeV, -10<y<
     +10, 0<phi<2pi.'
      GUID(10)=' '
      GUID(11)='To switch to PDG particle ID instead of the GEANT one, t
     +he options P or'
      GUID(12)='E can be used (PDG particle ID may also be negative).'
      GUID(13)='In this case user can switch from the transverse momentu
     +m range'
      GUID(14)='definition (option "P", Pt range) to total energy range'
     +//' definition'
      GUID(15)='(option "E", Energy range).'
      GUID(16)='If NTRACK equal -1 events are read from P input stream i
     +nstead of'
      GUID(17)='been generated. In this case the second parameter is the
     + initial'
      GUID(18)='event number.'
      GUID(19)=' '
      GUID(20)='Other parameters are use as a filter to select particles
     + fed into'
      GUID(21)='simulations. When used as filter, GKINE command should p
     +receed'
      GUID(22)='file openning command.'
      CALL KUGUID('GKINE',GUID,22,'S')
      CALL KUACT('GKINE',AGXUSER)

      CALL KUCMD(' ','TFLT','C')
      CALL KUPAR('TFLT','YLOW','Lower limit of pseudorapidity','R','S')
      CALL KUPVAL('TFLT','YLOW',0,-10.,' ','L')
      CALL KUPVAL('TFLT','YLOW',0,10.,' ','H')
      CALL KUPAR('TFLT','YHIGH','Upper limit of pseudorapidity','R','S')
      CALL KUPVAL('TFLT','YHIGH',0,-10.,' ','L')
      CALL KUPVAL('TFLT','YHIGH',0,10.,' ','H')
      CALL KUPAR('TFLT','PHILOW','Lower limit of Phi','RO','S')
      CALL KUPVAL('TFLT','PHILOW',0,-10.,' ','L')
      CALL KUPVAL('TFLT','PHILOW',0,10.,' ','H')
      CALL KUPAR('TFLT','PHIHIGH','Upper limit of Phi','RO','S')
      CALL KUPVAL('TFLT','PHIHIGH',0,-10.,' ','L')
      CALL KUPVAL('TFLT','PHIHIGH',0,10.,' ','H')
      GUID(1)='Install a filter on input particle parameters. Particles'
     +//' read on input'
      GUID(2)='with their rapidity [phi] outside these limits will not b
     +e loaded into'
      GUID(3)='GEANT KINE bank'
      CALL KUGUID('TFLT',GUID,3,'S')
      CALL KUACT('TFLT',AGXUSER)

      CALL KUCMD(' ','GMOMENTUM','C')
      CALL KUPAR('GMOMENTUM','NTRACK',
     +'Number of tracks per event, -1 for tape input','I','S')
      CALL KUPVAL('GMOMENTUM','NTRACK',1,0.,' ','D')
      CALL KUPVAL('GMOMENTUM','NTRACK',-10,0.,' ','L')
      CALL KUPVAL('GMOMENTUM','NTRACK',10000,0.,' ','H')
      CALL KUPAR('GMOMENTUM','ID',
     +'Geant Particle ID, first event for tape input','I','S')
      CALL KUPVAL('GMOMENTUM','ID',5,0.,' ','D')
      CALL KUPVAL('GMOMENTUM','ID',0,0.,' ','L')
      CALL KUPVAL('GMOMENTUM','ID',1000,0.,' ','H')
      CALL KUPAR('GMOMENTUM','PxLOW','Lower limit of x-momentum','RO',
     +'S')
      CALL KUPVAL('GMOMENTUM','PxLOW',0,-10000.,' ','L')
      CALL KUPVAL('GMOMENTUM','PxLOW',0,10000.,' ','H')
      CALL KUPAR('GMOMENTUM','PxHIGH','Upper limit of x-momentum','RO',
     +'S')
      CALL KUPVAL('GMOMENTUM','PxHIGH',0,-10000.,' ','L')
      CALL KUPVAL('GMOMENTUM','PxHIGH',0,10000.,' ','H')
      CALL KUPAR('GMOMENTUM','PyLOW','Lower limit of y-momentum','RO',
     +'S')
      CALL KUPVAL('GMOMENTUM','PyLOW',0,-10000.,' ','L')
      CALL KUPVAL('GMOMENTUM','PyLOW',0,10000.,' ','H')
      CALL KUPAR('GMOMENTUM','PyHIGH','Upper limit of y-momentum','RO',
     +'S')
      CALL KUPVAL('GMOMENTUM','PyHIGH',0,-10000.,' ','L')
      CALL KUPVAL('GMOMENTUM','PyHIGH',0,10000.,' ','H')
      CALL KUPAR('GMOMENTUM','PzLOW','Lower limit of z-momentum','RO',
     +'S')
      CALL KUPVAL('GMOMENTUM','PzLOW',0,-10000.,' ','L')
      CALL KUPVAL('GMOMENTUM','PzLOW',0,10000.,' ','H')
      CALL KUPAR('GMOMENTUM','PzHIGH','Upper limit of z-momentum','RO',
     +'S')
      CALL KUPVAL('GMOMENTUM','PzHIGH',0,-10000.,' ','L')
      CALL KUPVAL('GMOMENTUM','PzHIGH',0,10000.,' ','H')
      CALL KUPAR('GMOMENTUM','ZLOW','Lower limit of Z of Vertex','RO',
     +'S')
      CALL KUPVAL('GMOMENTUM','ZLOW',0,-10000.,' ','L')
      CALL KUPVAL('GMOMENTUM','ZLOW',0,10000.,' ','H')
      CALL KUPAR('GMOMENTUM','ZHIGH','Upper limit of Z of vertex','RO',
     +'S')
      CALL KUPVAL('GMOMENTUM','ZHIGH',0,-10000.,' ','L')
      CALL KUPVAL('GMOMENTUM','ZHIGH',0,10000.,' ','H')
      CALL KUPAR('GMOMENTUM','option',
     +'choise of Geant or PDG particle ID','OO','S')
      CALL KUPVAL('GMOMENTUM','option',0,0.,'G','D')
      CALL KUPVAL('GMOMENTUM','option',0,0.,'G,P,E','V')
      GUID(1)=' Generates particles in a given momentum bin'
      GUID(2)='or to provide a particle filter'
      GUID(3)='parameters for external generator input.'
      GUID(4)=' '
      GUID(5)='Mandatory parameters are number of tracks per event and G
     +eant particle ID for'
      GUID(6)='particle in-line generation, or -1 and first event number
     + for'
      GUID(7)='input event stream.'
      GUID(8)='Following optional parameters are'
      GUID(9)='lower and upper bounds of Px, Py and Pz.'
      GUID(10)='If no parameters are given, a single muon will be genera
     +ted per event,'
      GUID(11)='distributed uniformely in the interval -1<Px,Py,Px<1 GeV
     +.'
      GUID(12)=' '
      GUID(13)='To switch to PDG particle ID instead of the GEANT one, t
     +he options P or'
      GUID(14)='E can be used.'
      GUID(15)=' '
      GUID(16)='If NTRACK equal -1 events are read from P input stream i
     +nstead of'
      GUID(17)='been generated. In this case the second parameter is the
     + initial'
      GUID(18)='event number.'
      GUID(19)='Other parameters are use as a filter to select particles
     + fed into'
      GUID(20)='simulations.'
      CALL KUGUID('GMOMENTUM',GUID,20,'S')
      CALL KUACT('GMOMENTUM',AGXUSER)

      CALL KUCMD(' ','GFILE','C')
      CALL KUPAR('GFILE','streamtype',
     +'I/O stream and data type combined in one word','CO','S')
      CALL KUPVAL('GFILE','streamtype',0,0.,'P','D')
      CALL KUPAR('GFILE','file','name of the file','CO','S')
      CALL KUPVAL('GFILE','file',0,0.,'ZEBRA','D')
      CALL KUPAR('GFILE','sets','list of data sets to read/write','CO',
     +'S')
      CALL KUPVAL('GFILE','sets',0,0.,'*','D')
      GUID(1)='Open an input or output data file with events.'
      GUID(2)=' Stream. Different I/O streams are:'
      GUID(3)='  P      - physics events input stream'
      GUID(4)='  B      - background events to pile up on top of physics
     + events,'
      GUID(5)='  I      - general input data (no geometry record in the'
     +//' beginning),'
      GUID(6)='  O      - output data.'
      GUID(7)=' Data types may be:'
      GUID(8)=' '' '' - Standard GENZ Zebra format with Lrec=8100 (defau
     +lt),'
      GUID(9)='  Z  - Standard GEANT FZ format with Lrec=900 words,'
      GUID(10)='  L  - Records include Length (Fortran-type OPEN used),'
      GUID(11)='  F  - Fatmen catalog used to access Zebra tapes,'
      GUID(12)='  N  - special Colomn Wise Ntuple with GENZ-type events'
     +//' for input,'
      GUID(13)='  U  - user defined data type. In this case user has to'
     +//' supply his own'
      GUID(14)='       routines AGUSREAD(ier) and to link it dinamically
     + using gexec.'
      GUID(15)='       The input file is opened by default on unit 20 as
     + FORMATTED.'
      GUID(16)='       For a user-specific file openning user has to pro
     +vide his own'
      GUID(17)='       routine AGUSOPEN(file) and to link it dinamically
     + using gexec.'
      GUID(18)='  1  - read only till the first ZEBRA End_of_file'
      GUID(19)='       (default is a multy-file input stream),'
      GUID(20)='  C  - do not merge splitted or piled-up subevents, keep
     + them in a chain'
      GUID(21)='       (by default they are automatically merged in a si
     +ngle event).'
      GUID(22)='  S  - Special event, i.e. First resulting pile-up event
     + is saved'
      GUID(23)='       as a permanent pile-up HIT bank.'
      GUID(24)='       Such event, saved once with a maximum pile-up nee
     +ded, will be added on'
      GUID(25)='       top of normal mixed pile-up events to get huge pi
     +le-up in a fast way.'
      GUID(26)='  M  - Multiple try to bypass latency of mass storage. D
     +o not use this option'
      GUID(27)='       if you are not sure the file is there.'
      GUID(28)='  W  - Wildcard numbers, i.e. all digits within the file
     +name are sequentially'
      GUID(29)='       incremented to find valid filenames. The path par
     +t is not modified.'
      GUID(30)='  T  - LisT driven input, i.e. this is the lisT if input
     + files to loop over.'
      GUID(31)='File: is the name of the data file.'
      GUID(32)='It may include directory path and wildcard characters.'
      GUID(33)='Default extensions for the default file name are P, B an
     +d O.'
      GUID(34)=' Sets: list of data sets to be read or written:'
      GUID(35)=' G - GEANT detector geometry plus DETM bank'
      GUID(36)='     (unless they are already loaded or created)'
      GUID(37)=' E - GENZ RUNT/EVNT banks with parton level information,
     +'
      GUID(38)=' K - Geant KINE/VERT/HEAD information,'
      GUID(39)=' H - HIT banks,'
      GUID(40)=' D - DIGI banks,'
      GUID(41)=' R - reconstruction banks,'
      GUID(42)=' * - all above (default, NO SIMULATION will be done!).'
      GUID(43)=' Note:'
      GUID(44)=' (1) GEANT simulations are allowed ONLY if the input doe
     +s not contain'
      GUID(45)='hits or digits (i.e. simulations are disallowed by the d
     +efault list!).'
      GUID(46)=' (2) Internal data structure description (rz-database) i
     +s always updated'
      GUID(47)='automatically on P stream.'
      GUID(48)='To force a manual update use STRUCTURES command with 2 b
     +lank arguments.'
      GUID(49)='To prevent the rz-database update, use I-input stream wi
     +th sequential'
      GUID(50)='TRIG comand.'
      GUID(51)='To update include files use the default STRUCTURES comma
     +nds.'
      CALL KUGUID('GFILE',GUID,51,'S')
      CALL KUACT('GFILE',AGXUSER)

      CALL KUCMD(' ','GHIST','C')
      CALL KUPAR('GHIST','file','name of the file','CO','S')
      CALL KUPVAL('GHIST','file',0,0.,'atlas.his','D')
      CALL KUPAR('GHIST','directory','rz-directory for n-tuples','CO',
     +'S')
      CALL KUPVAL('GHIST','directory',0,0.,'SLUGRZ','D')
      CALL KUPAR('GHIST','unit','logical unit number','IO','S')
      CALL KUPVAL('GHIST','unit',33,0.,' ','D')
      CALL KUPAR('GHIST','LRECL','Record length in words','IO','S')
      CALL KUPVAL('GHIST','LRECL',1024,0.,' ','D')
      CALL KUPAR('GHIST','NRECP','Maximum record allocation','IO','S')
      CALL KUPVAL('GHIST','NRECP',1024,0.,' ','D')
      GUID(1)='Open a histogram output file. This file is used to keep'
      GUID(2)='disk resident N-tuples and to save all histograms at the'
     +//' end'
      GUID(3)='of run.'
      GUID(4)='Default parameters allows maximum file length about 4 Gby
     +tes'
      GUID(5)='(30 times more then a standard HBOOK file). Note, that mo
     +st'
      GUID(6)='UNIX systems still limit the filesize below 2Gbytes.'
      GUID(7)=' Before opening a disk resident N-tuple, user should do'
      GUID(8)=' the following directory setting:'
      GUID(9)='    Call RZCDIR(''//SLUGRZ'','' '')'
      GUID(10)='    Call  HCDIR(''//SLUGRZ'','' '')'
      GUID(11)=' At the end of the run you should do EXIT command to fin
     +ish GEANT -'
      GUID(12)=' this will do the actual histogram saving.'
      CALL KUGUID('GHIST',GUID,12,'S')
      CALL KUACT('GHIST',AGXUSER)

      CALL KUCMD(' ','GBACK','C')
      CALL KUPAR('GBACK','Nbefor',
     +'number of bunchs to add prior to the trigger one','IO','S')
      CALL KUPVAL('GBACK','Nbefor',0,0.,' ','D')
      CALL KUPAR('GBACK','Nafter',
     +'number of bunchs to add after the trigger one','IO','S')
      CALL KUPVAL('GBACK','Nafter',0,0.,' ','D')
      CALL KUPAR('GBACK','BgMult',
     +'Average pile-up multiplicity of bunch crossing','RO','S')
      CALL KUPVAL('GBACK','BgMult',0,23.,' ','D')
      CALL KUPAR('GBACK','BgTime',
     +'Time between bunch crossings (in ns)','RO','S')
      CALL KUPVAL('GBACK','BgTime',0,25.,' ','D')
      CALL KUPAR('GBACK','BGSkip',
     +'average number of skept events for randomizing','RO','S')
      CALL KUPVAL('GBACK','BGSkip',0,1.,' ','D')
      GUID(1)='Secondary event stream ''B'' use used to put pile-up pre-
     +simulated'
      GUID(2)='events on top of a physics event.'
      GUID(3)='Background data should be in GEANT or GENZ format (non sp
     +litted).'
      GUID(4)='Interaction time, vertex and track numbers of each second
     +ary event'
      GUID(5)='are updated upon read.'
      CALL KUGUID('GBACK',GUID,5,'S')
      CALL KUACT('GBACK',AGXUSER)

      CALL KUCMD(' ','GRFILE','C')
      CALL KUPAR('GRFILE','file','geometry file name','CO','S')
      CALL KUPVAL('GRFILE','file',0,0.,'geom.rz','D')
      CALL KUPAR('GRFILE','inout','Input or Output command','OO','S')
      CALL KUPVAL('GRFILE','inout',0,0.,'OUT','D')
      CALL KUPVAL('GRFILE','inout',0,0.,'IN,OUT','V')
      CALL KUPAR('GRFILE','unit','Unit number','IO','S')
      CALL KUPVAL('GRFILE','unit',30,0.,' ','D')
      GUID(1)='read in or write out (default) a geometry file in rz-form
     +at.'
      CALL KUGUID('GRFILE',GUID,1,'S')
      CALL KUACT('GRFILE',AGXUSER)

      CALL KUCMD(' ','GCLOSE','C')
      CALL KUPAR('GCLOSE','Option','call GPHYSI after geometry closing',
     +'OO','S')
      GUID(1)='Close GEANT geometry building phase. Should be done befor
     +e any graphics'
      GUID(2)='and/or simulations are started, otherwise ZEBRA memory pr
     +oblems may arise.'
      GUID(3)='If any parameter is given, GPHYSI is also called to calcu
     +late cross-sections.'
      GUID(4)='In DEBUG OFF mode the output of GPHYSI is rederected into
     + LUN 99.'
      CALL KUGUID('GCLOSE',GUID,4,'S')
      CALL KUACT('GCLOSE',AGXUSER)

      CALL KUCMD(' ','GDROP','C')
      CALL KUPAR('GDROP','Option','bank drop choice','OO','S')
      CALL KUPVAL('GDROP','Option',0,0.,'*','D')
      GUID(1)='Drops a selected bank tree or a set of bank trees accordi
     +ng to the'
      GUID(2)='following list:'
      GUID(3)=' P - Particle structure PART'
      GUID(4)=' A - material structure MATE'
      GUID(5)=' M - medium structure TMED'
      GUID(6)=' - - drops all three above mentioned structures'
      GUID(7)=' V - Volume structures VOLUM and GPAR, Rotation matrices'
      GUID(8)=' S - sets, hits and digits SETS, HITS and DIGI'
      GUID(9)=' D - detector description bank DETM'
      GUID(10)=' R - event raw data RAWD and reconstruction bank RECB'
      GUID(11)=' E - the whole short-range event division'
      GUID(12)=' * - drops all listed above except for the first three.'
      CALL KUGUID('GDROP',GUID,12,'S')
      CALL KUACT('GDROP',AGXUSER)

      CALL KUCMD(' ','GEXEC','C')
      CALL KUPAR('GEXEC','file','names of files or directories','C','S')
      GUID(1)='Compile, link as one shared library, load and execute a u
     +ser code,'
      GUID(2)='stored in selected source files, in a directory or in a s
     +et of directories.'
      GUID(3)='The resulting shared library is named by the first name i
     +n the list.'
      GUID(4)='A subroutine with the same name as the library is execute
     +d,'
      GUID(5)='unless the library contains a name_init or name_start ent
     +ry,'
      GUID(6)='which have the precedence.'
      CALL KUGUID('GEXEC',GUID,6,'S')
      CALL KUACT('GEXEC',AGXUSER)

      CALL KUCMD(' ','GMAKE','C')
      CALL KUPAR('GMAKE','library',
     +'additional keyword parameters for make','CO','S')
      CALL KUPVAL('GMAKE','library',0,0.,' ','D')
      GUID(1)='supply optional parameters for the make procedure, may be
     + with'
      GUID(2)='keywords (examples: debug  LIB_PATH=... LIBS=...)'
      CALL KUGUID('GMAKE',GUID,2,'S')
      CALL KUACT('GMAKE',AGXUSER)

      CALL KUCMD(' ','GSTAT','C')
      CALL KUPAR('GSTAT','key','name of the selected histogram','CO',
     +'S')
      CALL KUPVAL('GSTAT','key',0,0.,'ALL','D')
      GUID(1)=' Book a standard set of the GEANT control histogram:'
      GUID(2)=' TIME -  Time per event'
      GUID(3)=' SIZE -  Space used in IXDIV per event'
      GUID(4)=' MULT -  Total number of tracks per event'
      GUID(5)=' NTRA -  Long life tracks per event'
      GUID(6)=' STAK -  Maximum stack size per event'
      CALL KUGUID('GSTAT',GUID,6,'S')
      CALL KUACT('GSTAT',AGXUSER)

      CALL KUCMD(' ','STRUCTURES','C')
      CALL KUPAR('STRUCTURES','system',
     +'name of the system whose structures should be dumped','CO','S')
      CALL KUPVAL('STRUCTURES','system',0,0.,' ','D')
      CALL KUPAR('STRUCTURES','type','type of the output','CO','S')
      CALL KUPVAL('STRUCTURES','type',0,0.,'def','D')
      GUID(1)='Produce a definition file with data structure description
     + and'
      GUID(2)='update the documentation database (detm.rz) in accordance
     + with the'
      GUID(3)='structures currenly loaded in the program.'
      GUID(4)='System name argument is interpreted in the same way as th
     +e argument in'
      GUID(5)='the UNIX ''ls'' command, for example:'
      GUID(6)=' - no name at all produces a single output file (detmsys)
     +'
      GUID(7)='with all structure descriptions in it;'
      GUID(8)=' - ''*'' produces a set of system-based output files (*sy
     +s)'
      GUID(9)='with its related structure description;'
      GUID(10)=' - sys/name produces a single structure definition.'
      GUID(11)='Type of the output may be'
      GUID(12)='  def   - AGI preprocessor input file (default),'
      GUID(13)='  idl   - CORBA interface definitions language,'
      GUID(14)='  other - internal table format'
      GUID(15)='The detmsys.def output may be automatically read by the'
     +//' AGI parser so that user'
      GUID(16)='can get access to structure description with +cde or +in
     +clude statements.'
      GUID(17)='CORBA idl file should be processed by STIC compiler to p
     +roduce .inc and .h'
      GUID(18)='files. Internal definition file is directly fed to table
     + access module.'
      CALL KUGUID('STRUCTURES',GUID,18,'S')
      CALL KUACT('STRUCTURES',AGXUSER)

      CALL KUCMD(' ','TABLES','C')
      CALL KUPAR('TABLES','system',
     +'name of the system to be converted into table','CO','S')
      CALL KUPVAL('TABLES','system',0,0.,' ','D')
      CALL KUPAR('TABLES','dataset','output directory','CO','S')
      CALL KUPVAL('TABLES','dataset',0,0.,' ','D')
      GUID(1)='Makes AGI structures, which belong to a particular AGI sy
     +stem, visible as'
      GUID(2)='StaF tables in the requested dataset. Apropriate STAF tab
     +le descriptions'
      GUID(3)='is derived from AGI description and actual table adresses
     + are mapped to AGI'
      GUID(4)='structures. Default value means "all system".'
      GUID(5)='A subset of structures can also be convertes into tables'
      GUID(6)='by defining the path to them.'
      GUID(7)='By default, all tables are created in /dui/Run, but'
      GUID(8)='destination can be redirectded to another dataset.'
      GUID(9)='If the dataset does not exist yet, it will be created'
      GUID(10)='(but not the whole path!)'
      CALL KUGUID('TABLES',GUID,10,'S')
      CALL KUACT('TABLES',AGXUSER)

      CALL KUCMD(' ','TABCLEAR','C')
      CALL KUPAR('TABCLEAR','dataset','directory to clear','CO','S')
      CALL KUPVAL('TABCLEAR','dataset',0,0.,'.','D')
      GUID(1)='Clear (reset row conters) all tables in the selected dire
     +ctory and below it.'
      CALL KUGUID('TABCLEAR',GUID,1,'S')
      CALL KUACT('TABCLEAR',AGXUSER)

      CALL KUCMD(' ','ONFAULT','C')
      CALL KUPAR('ONFAULT','fault',
     +'name of the arithmetic fault signal flag','CO','S')
      CALL KUPVAL('ONFAULT','fault',0,0.,'IDO','D')
      CALL KUPAR('ONFAULT','counter','number of faults to catch','IO',
     +'S')
      CALL KUPVAL('ONFAULT','counter',1,0.,' ','D')
      CALL KUPAR('ONFAULT','handler',
     +'name of an optional user error handler routine','CO','S')
      CALL KUPVAL('ONFAULT','handler',0,0.,' ','D')
      GUID(1)='Catch certain number of arithmetic faults of selected typ
     +e and print'
      GUID(2)='tracing diagnostic. To provide a meaningful diagnostic, u
     +ser code should'
      GUID(3)='be compiled with ''-g'' option (unfortunatly, not all of'
     +//' the cernlib'
      GUID(4)='routines are compiled with it!).'
      GUID(5)=' - fault parameter:'
      GUID(6)='List of possible fault types is computer dependant.'
      GUID(7)='Here we describe the HPUX version only, which can detect:
     +'
      GUID(8)='(I) - illegal instruction, (D) - division by 0,'
      GUID(9)='(O) - floating overflow, (U) - floating underflow,  and (
     +X) - inexact numbers.'
      GUID(10)='Last two happens very often and should not be normally c
     +onsidered as errors.'
      GUID(11)='In addition (*) subsitutes all five flags, (+) means do'
     +//' not alter flags other'
      GUID(12)='then mentioned in the command, which otherwise are reset
     + to IGNORE.'
      GUID(13)='If a fault type was never mentioned in any ONFAULT comma
     +nds and they all'
      GUID(14)='had +''s, corresponding error handler is still activated
     + with the default'
      GUID(15)='behaviour defined in libm.a.'
      GUID(16)=' - counter meaning:'
      GUID(17)='A positive counter sets the number of extended error mes
     +sages to be printed,'
      GUID(18)='the rest is counted in the common block /agerrorcount/ n
     +num(5),mmax(5),'
      GUID(19)='but not reported. Error handler is permanently activated
     +.'
      GUID(20)='A zero counter forces the program to simulate a kuip bre
     +ak after the error'
      GUID(21)='message is printed. If this happens in a macro, executed
     + on a kuip prompt,'
      GUID(22)='the kuip will issue the prompt again. If this happens in
     + macro started in'
      GUID(23)='a command line, program will stop.'
      GUID(24)='If the counter is negative, corresponding faults are com
     +pletely ignored'
      GUID(25)='(not even counted).'
      GUID(26)=' - handler:'
      GUID(27)='If defined, a user handler routine is called instead of'
     +//' the'
      GUID(28)='standard CERNLIB tracing routine.'
      CALL KUGUID('ONFAULT',GUID,28,'S')
      CALL KUACT('ONFAULT',AGXUSER)

      CALL KUCMD(' ','ABEND','C')
      GUID(1)=' abort the program - the fastest possible exit'
      CALL KUGUID('ABEND',GUID,1,'S')
      CALL KUACT('ABEND',AGXUSER)

      CALL KUCMD(' ','REBANK','C')
      GUID(1)='Bank access mechanism is implemeted as it is described in
     + the'
      GUID(2)='Atlas note SOFT-NO-002. The only correction to the note i
     +s the call'
      GUID(3)='to the REBANK itself, which now has an additional paramet
     +er Ia:'
      GUID(4)='          call REBANK (Path,IDN,Npar,Link*,Ia*)'
      GUID(5)='The returned value of Ia contains the displacement in the
     + bank,'
      GUID(6)='for a single raw request. Remember that this routine is n
     +ot intended'
      GUID(7)='for general usage and should be avoided - use FILL/USE op
     +erators or'
      GUID(8)='RBSTORE/RBCOPY, RBGET/RBPUT routines instead.'
      CALL KUGUID('REBANK',GUID,8,'S')

      CALL KUCMD(' ','FILL','C')
      GUID(1)='FILL operatore is fully described in the AGI manual (Atla
     +s note SOFT-NO-14).'
      GUID(2)='Here we provide some additional details and helpful hints
     +.'
      GUID(3)=' Although names of the variables in AGI structures are li
     +mited to 12'
      GUID(4)='characters, DZDOC suports only 8 characters in the docume
     +ntation.'
      GUID(5)='That means that in the structure defintion, produced by A
     +GI (see also'
      GUID(6)='STRUCTURES and TABLES commands), all names will be TRUNCA
     +TED to 8 symbols.'
      GUID(7)='To avoid this complification, user is advised to use in s
     +tructures'
      GUID(8)='variable names not longer than 8 charactes.'
      CALL KUGUID('FILL',GUID,8,'S')
      CALL KUACT('FILL',AGXUSER)

      CALL KUCMD(' ','USE','C')
      GUID(1)='Here we provide some general guidance in addition to the'
     +//' Atlas note'
      GUID(2)='SOFT-NO-014, for using USE operator in the reconstruction
     + code.'
      GUID(3)=' the complete format of the USE operator is'
      GUID(4)=' USE Path [ variable=value ] [ OPER={DELETE/UNIQUE/NEXT/Z
     +ERO} ] [ STAT=istat ]'
      GUID(5)='All fields apart from Path are optional.'
      GUID(6)='  Path : is the Unix-like path to the selected bank or a'
     +//' chain of banks'
      GUID(7)='         It may include after bank names integer indeces'
      GUID(8)='         If omitted, default value for index is supposed'
     +//' to be 1.'
      GUID(9)='         The Path may be absolute, i.e. starting from /DE
     +TM or /RECB, in'
      GUID(10)='         which case the selected bank becomes the Curren
     +t Working Directory'
      GUID(11)='         or relative, i.e. starting from the current wor
     +king directory,'
      GUID(12)='         selected by the previous USE operator.'
      GUID(13)='         Module names in the begining of the path are re
     +cognized as'
      GUID(14)='         the absolute path in DETM tree.'
      GUID(15)='  OPER:  Different operations are :'
      GUID(16)='   DELETE - deletes the selected bank from the tree AFTE
     +R copying its'
      GUID(17)='            content in the corresponding structure'
      GUID(18)='   ZERO   - reset the bank content to ZERO (agaim AFTER'
     +//' coping)'
      GUID(19)='   UNIQUE - moves the selected bank to the first positio
     +n in the chain'
      GUID(20)='            and drops all other banks in this chain maki
     +ng a unique'
      GUID(21)='            version available for the further analysis.'
      GUID(22)='   NEXT   - select the next bank in the chain without se
     +arching the'
      GUID(23)='            path again. If STAT option is used, this is'
     +//' done only'
      GUID(24)='            if the status control variable is OK [0], ot
     +herwise'
      GUID(25)='            a normal search in the path is done, thus al
     +lowing'
      GUID(26)='            to select a starting bank and its descendent
     + in the'
      GUID(27)='            same USE'
      GUID(28)='  STAT=ISTAT returns in ISTAT the bank access status (0'
     +//' if OK).'
      GUID(29)='             Initial value of ISTAT should be defined wi
     +th a DATA statement.'
      GUID(30)='If structure description in the program is different fro
     +m the one in ZEBRA'
      GUID(31)='memory, local copy is filled correctly using documentati
     +on. New variables,'
      GUID(32)='not present in the bank, are left intact.'
      GUID(33)=' Example: suppose a bank has been created by the followi
     +ng module:'
      GUID(34)=' --------'
      GUID(35)='      module    somegeo is a system'
      GUID(36)='      author    me'
      GUID(37)='      created   today'
      GUID(38)='      integer   iprin'
      GUID(39)='      structure MFLG { version, int Bfield(2,3), RmaxInn
     +, ZmaxInn ,char title}'
      GUID(40)='      fill MFLG(1)                  !  system data'
      GUID(41)='          version =1                   !  bank version'
      GUID(42)='          Bfield  = {1,2; 4,5; 11,12}  !  some field val
     +ue'
      GUID(43)='          RmaxInn = 100                !  max rad'
      GUID(44)='          ZmaxInn = 200                !  max len'
      GUID(45)='          title   = ''abcdef''           !  some 4-lette
     +r text'
      GUID(46)='      end'
      GUID(47)=' Then you try to read it with the following subroutine:'
      GUID(48)=' -----'
      GUID(49)='      subroutine sometest'
      GUID(50)='      integer   iprin,istat/0/,i'
      GUID(51)='      structure MFLG { int version, char title, int aaa,
     +'
      GUID(52)='                       Bfield(3,2), RmaxInn, ZmaxInn }'
      GUID(53)='      begin'
      GUID(54)='      mflg_aaa  = 999'
      GUID(55)='      use  SOMEGEO/MFLG'
      GUID(56)='      print *, '' version, aaa, ZmaxInn, title, Bfield:'
     +//' '''
      GUID(57)='      print *,mflg_version,mflg_aaa,mflg_ZmaxInn,'' '',m
     +flg_title,mflg_Bfield'
      GUID(58)='      end'
      GUID(59)=' Result will be the following:'
      GUID(60)=' ------'
      GUID(61)=' Schema evolution for Bpath=/DETM/SOME/MFLG* bank=MFLG:'
      GUID(62)='  version, aaa, ZmaxInn, title, Bfield:'
      GUID(63)='   2  999    200.000 abcd    1.00000    2.00000  0.    4
     +.00000    5.00000  0.'
      CALL KUGUID('USE',GUID,63,'S')
      CALL KUACT('USE',AGXUSER)

      CALL KUCMD(' ','GENZ','C')
      GUID(1)='                  *************************'
      GUID(2)='                  *  preliminary version  *'
      GUID(3)='                  *  send your comments   *'
      GUID(4)='                  *       to Pavel        *'
      GUID(5)='                  *************************'
      GUID(6)='GENZ is a package for the handling of the events, sets of
     + particles'
      GUID(7)='and partons, as output by physics event generators.'
      GUID(8)='It has been written for by Robert DeWolf in 1990-1992 and
     + is'
      GUID(9)='fully described in the GENZ manual (1).'
      GUID(10)='The version, described here, maintains the complete func
     +tionality'
      GUID(11)='of the original package, but reflects the nature of its'
     +//' usage'
      GUID(12)='and the experience within ATLAS.'
      GUID(13)='GENZ provides tools to access any component of generator
     +'
      GUID(14)='events from  user''s code.'
      GUID(15)='Particle data, identities, 4-momenta, vertex position, e
     +tc., as well as'
      GUID(16)='the relational information between particles, is readily
     + available.'
      GUID(17)='GENZ provides also a means of reading and writing genera
     +tor events'
      GUID(18)='stored in the HEPEVT common block (2).'
      GUID(19)='In addition, GENZ provides an a nice event dumping utili
     +ty for listing'
      GUID(20)='the current event to the standard output.'
      GUID(21)='GENZ was designed as a slave package, hence it is initia
     +lised'
      GUID(22)='by the framework itself within avalable ZEBRA memory.'
      GUID(23)='All input/output is also initialised and performed by th
     +e'
      GUID(24)='framework itself (see GFILE commands)'
      GUID(25)='Following the general logic, all input events are connec
     +ted'
      GUID(26)='to a single primary link, with the identification preser
     +ved'
      GUID(27)='inside banks, so the first parameter in each call (ILINK
     +)'
      GUID(28)='in never used.'
      GUID(29)='However, to preserve code re-usabilty, we still keep ALL
     +'
      GUID(30)='parameters as they are described in the original manual.
     +'
      GUID(31)='which differs from the GENZ manual, concerns subroutine'
     +//' GNZGETP,'
      GUID(32)='where an extra parameter (VRTX) was added on request fro
     +m Daniel.'
      GUID(33)=' The subroutine calls that are of interest to GENZ users
     + are given here.'
      GUID(34)=' -------------------------------------------------------
     +----------------'
      GUID(35)=' CALL GNZPRIN(ILINK,LEVEL)'
      GUID(36)='The contents of the current GENE/GENP banks are printed'
      GUID(37)='in a nice format, making this a useful event dump utilit
     +y.'
      GUID(38)='The contents of the GENR bank are printed if there has b
     +een'
      GUID(39)='a new run since the last GNZPRIN call.'
      GUID(40)=' CALL GNZTOHC(IRET)'
      GUID(41)=' Information in the GENZ banks is translated into HEPEVT
     + common block'
      GUID(42)=' Get Run Information'
      GUID(43)=' CALL GNZGETR(ILINK,JIDGN,IGRUN,IGTIM,IGDAT,CMACH,CGENE,
     +VRGEN,IDATG,VRGNZ,VRZEB)'
      GUID(44)=' Return global run information from the GENR bank in lin
     +k set ILINK.'
      GUID(45)=' Input Parameters'
      GUID(46)=' ILINK  Link set specifier - not used'
      GUID(47)=' Output Parameters'
      GUID(48)=' JIDGN  Original generator run''s job ID'
      GUID(49)=' IGRUN  run number as specified in the original generato
     +r run'
      GUID(50)=' IGTIM  time of the generator run as hhmmss'
      GUID(51)=' IGDAT  date of the generator run as yymmdd'
      GUID(52)=' CMACH  generator run platform (CHAR*4)'
      GUID(53)=' CGENE  generator name (CHAR*4)'
      GUID(54)=' VRGEN  generator version as vv.ssss (REAL*4)'
      GUID(55)=' IDATG  date of the generator version as yymmdd'
      GUID(56)=' VRGNZ  GENZ version (REAL*4)'
      GUID(57)=' VRGNZ  Zebra version (REAL*4)'
      GUID(58)=' Get Gate Information:'
      GUID(59)=' GNZGETG(ILINK,NEVEN)'
      GUID(60)=' Return information on the current gate (that is , the c
     +urrent GENE'
      GUID(61)=' linear structure) in link set ILINK.'
      GUID(62)=' Input Parameters'
      GUID(63)=' ILINK - Link set specifier - not used'
      GUID(64)=' Output Parameters'
      GUID(65)=' NEVEN - Number of events in the current gate'
      GUID(66)=' Get Event Information:'
      GUID(67)=' GNZGETE(ILINK,IDN,NPART,IRUN,IEVT,CGNAM,VERT,IWTFL,WEIG
     +H)'
      GUID(68)=' Return information on the IDN''th event in the present'
     +//' gate in link set ILINK'
      GUID(69)=' Input Parameters'
      GUID(70)=' ILINK - Link set specifier - not used'
      GUID(71)=' IDN   - ID of the GENE bank (Zebra IDN of the bank in t
     +he GENE'
      GUID(72)='        linear structure).'
      GUID(73)=' Output Parameters'
      GUID(74)=' NPART - Numbers of particles or partons in event record
     +'
      GUID(75)=' IRUN  - Run number from generator run'
      GUID(76)=' IEVT  - Event number from generator run'
      GUID(77)=' CGNAM - Generator name (CHARACTER*4)'
      GUID(78)=' VERT  - 4-vector given the primary vertex position of t
     +he event'
      GUID(79)=' IWTFL - weight flag'
      GUID(80)=' WEIGH - event weight'
      GUID(81)=' Get Particle Information:'
      GUID(82)=' GNZGETP(ILINK,IDN,IP,ISTAT,IDPDG,P,AMASS,MOTH,TIME,IDAU
     +1,VERT)'
      GUID(83)='Return information from the particle record specified by
     + ILINK,IDN,IP.'
      GUID(84)='If particle record does not exist, (or if ISTAT = 0),'
      GUID(85)='returned ISTAT is equal to zero.'
      GUID(86)=' Input Parameters'
      GUID(87)=' ILINK Link set specifier - not used'
      GUID(88)=' IDN - ID of the GENE bank'
      GUID(89)=' IP  - particle record to read.'
      GUID(90)=' Output Parameters'
      GUID(91)=' ISTAT    - particle status'
      GUID(92)=' IDPDG    - particle code (Particle Data Group code)'
      GUID(93)=' P(4)     - 4-momentum of particle (in GeV )'
      GUID(94)=' AMASS    -  mass of particle (in GeV/$c~{2$)'
      GUID(95)=' MOTH(2)  - pointers to mother records'
      GUID(96)=' TIME     - Start time of particle relative to interacti
     +on,in seconds.'
      GUID(97)=' IDAU1    - pointer to first daughter'
      GUID(98)=' VRTX(4)  - production vertex and time'
      GUID(99)=' Get vertex information ???:'
      GUID(100)=' CALL GNZGETV(ILINK,IDN,IP,VSTAR,IMVRT,PRET,AROT,ROTMA)
     +'
      GUID(101)=' Obtain the start point of a particle specified by ILIN
     +K,IDN,IP.'
      GUID(102)=' This is found by tracing back from particle IP until z
     +ero time'
      GUID(103)=' The index of the particle decaying at the returned ver
     +tex point is'
      GUID(104)='returned in IMVRT.'
      GUID(105)='This may not be the immeditate mother particle of IP,'
      GUID(106)='which can be found by calling GNZGETP.'
      GUID(107)='It is rather the first particle in the ancestor traceba
     +ck.'
      GUID(108)='IMVRT is also used as an error flag for bad input argum
     +ent values'
      GUID(109)='and other errors.'
      GUID(110)='In these cases, there is always accompanying printout.'
      GUID(111)=' The remaining output parameters are of interest to use
     +rs who'
      GUID(112)='have set a (uniform) magnetic field using GNZPARR.  ???
     +'
      GUID(113)='PRET is the 3-momentum of particle IP rotated according
     + to the'
      GUID(114)='deflections of its ancestor particles in the field.'
      GUID(115)='AROT is the accumulated angle of rotation in radians ab
     +out the magnetic field'
      GUID(116)='axis up to but not including IP''s flight.  ROTMA retur
     +ns the rotation matrix'
      GUID(117)='that can be used to rotate 3-vectors, for instance usin
     +g the utility GNZROTA.'
      GUID(118)=' Input Parameters'
      GUID(119)=' ILINK   Link set specifier - not used'
      GUID(120)=' IDN   - ID of event in the gate'
      GUID(121)=' IP    - particle member number'
      GUID(122)=' Output Parameters'
      GUID(123)=' VSTAR start vertex position and time, x,y,z,t. This is
     + returned'
      GUID(124)='       relative to the interaction point in metres and'
     +//' seconds.'
      GUID(125)=' IMVRT index of particle decaying at VSTAR (see notes)'
      GUID(126)='        = 0   IP decays within TVMI of primary vertex'
      GUID(127)='        = -1  Error condition'
      GUID(128)=' PRET(3) 3-momentum of particle IP accounting for rotat
     +ion in B-field'
      GUID(129)=' AROT   angle of rotation in radians about field axis'
      GUID(130)=' ROTMA(3,3)  rotation matrix for rotation in B-field'
      GUID(131)=' Get Daughter Information'
      GUID(132)=' GNZGETD(ILINK,IDN,IP,NDAUG,IDAUG)'
      GUID(133)=' Return in array IDAUG up to NDAUG indices of daughter'
     +//' particles for particle'
      GUID(134)=' IP in event IDN in link set ILINK.'
      GUID(135)=' Input Parameters'
      GUID(136)=' ILINK - Link set specifier - not used'
      GUID(137)=' IDN   - IDN of the desired GENE bank'
      GUID(138)=' IP    - number of the given mother particle'
      GUID(139)=' NDAUG - Maximum number of daughters to return in IDAUG
     +'
      GUID(140)=' Output Parameters'
      GUID(141)=' NDAUG Number of daughters returned in array IDAUG'
      GUID(142)='   = 0  No daughters found'
      GUID(143)=' IDAUG Array containing the member numbers of the daugh
     +ter particles'
      GUID(144)=' (1) R.Dewolf. GENZ -- Generated Event Handling using Z
     +ebra'
      GUID(145)=' atlasinfo.cern.ch/Atlas/GROUPS/SOFTWARE/DOCUMENTS/GENZ
     +_MANUAL/genz_manual.html'
      GUID(146)=' (2) T. Sjostrand et al.,'
      GUID(147)=' CERN Yellow Report 89-08, v.3, p.327.'
      GUID(148)=' ps:  HCTOGNZ,  GNZPUTE,  GNZPUTG,  GNZPUTP,  GNZPUTV,'
     +//'  GNZPUTM,'
      GUID(149)='      can be readily done available if requested'
      CALL KUGUID('GENZ',GUID,149,'S')
      CALL KUACT('GENZ',AGXUSER)

      CALL KUCMD(' ','HIGZ','C')
      CALL KUPAR('HIGZ','IGH','Graphic workstation type','IO','S')
      CALL KUPVAL('HIGZ','IGH',1,0.,' ','D')
      GUID(1)='Open a HIGZ window (if it was not opened initially).'
      GUID(2)='If program was started without a graphic window (using -w
     + 0),'
      GUID(3)='it is still possible to open it later.'
      GUID(4)='In this case window can be used by DZDOC or GEANT graphic
     +s.'
      GUID(5)='HBOOK package still remains in AlphaNumeric mode.'
      GUID(6)='For all possible workstation types see appendix B to HIGG
     +Z manual'
      GUID(7)='(for X11  1 to 10 are used).'
      CALL KUGUID('HIGZ',GUID,7,'S')
      CALL KUACT('HIGZ',AGXUSER)

      CALL KUCMD(' ','DRAWONETREE','C')
      CALL KUPAR('DRAWONETREE','CHBSBK','Hollerith Id of selected bank',
     +'C','S')
      CALL KUPAR('DRAWONETREE','CHBSUP','Hollerith Id of its up-bank',
     +'C','S')
      CALL KUPAR('DRAWONETREE','CHMETA','Name of temp plot file','CO',
     +'S')
      CALL KUPVAL('DRAWONETREE','CHMETA',0,0.,' ','D')
      CALL KUPAR('DRAWONETREE','CHSGML','Name of temp text-file','CO',
     +'S')
      CALL KUPVAL('DRAWONETREE','CHSGML',0,0.,' ','D')
      CALL KUPAR('DRAWONETREE','CHPOST','Name of PostScript file','CO',
     +'S')
      CALL KUPVAL('DRAWONETREE','CHPOST',0,0.,' ','D')
      CALL KUPAR('DRAWONETREE','CHOPT','Option (P=PostScript L=Latex)',
     +'OO','S')
      CALL KUPVAL('DRAWONETREE','CHOPT',0,0.,' ','D')
      CALL KUPAR('DRAWONETREE','CTITLE','Global title','CO','S')
      CALL KUPVAL('DRAWONETREE','CTITLE',0,0.,'ZEBRA-Datastructures',
     +'D')
      GUID(1)='Draw tree below a selected bank. All banks belonging to t
     +he'
      GUID(2)='tree will be actually lifted in memory each with three da
     +ta'
      GUID(3)='words. Word 1, 2, 3 are the number of data words, links a
     +nd'
      GUID(4)='structural links as described in the documentation, a -1'
     +//' indicates'
      GUID(5)='a variable number. A global title may be given which appe
     +ars'
      GUID(6)='on the front page of the document.'
      GUID(7)='Output option:'
      GUID(8)='               ''P'' or '' '' PostScript file (default)'
      GUID(9)='               ''L''        Latex file'
      GUID(10)='               ''H''        HTML (hypertext) file'
      GUID(11)='               ''Q''        quiet i.e. generate no outpu
     +t files'
      GUID(12)='               ''C''        check consistency'
      GUID(13)='               ''S''        suppress confirmation'
      GUID(14)='               ''M'' Put as many down banks as posibble'
     +//' on one picture'
      GUID(15)='                   (Per default 2nd level are only drawn
     + if all fit)'
      CALL KUGUID('DRAWONETREE',GUID,15,'S')
      CALL KUACT('DRAWONETREE',DZEDRW)

      CALL KUCMD(' ','FPU_CONTROL','C')
      CALL KUPAR('FPU_CONTROL','mask','FPU control word, octal','IO',
     +'S')
      CALL KUPVAL('FPU_CONTROL','mask',1577,0.,' ','D')
      GUID(1)='Show or change the Intel Processor Control Word using the
     + fortran interface'
      GUID(2)='( Courtesy of Anders Waananen and Jorgen Beck Hansen ):'
      GUID(3)=' FPUGETCW(CW)    : Get the FPU control word in CW'
      GUID(4)=' FPUSETCW(CW)    : Set the FPU control word from CW'
      GUID(5)=' FPUSTACK(FPUTAG,FPUSTATUS,FPUUSED,FPUOVERFLOW) : where'
      GUID(6)='     FPUTAG      : FPU TAG'
      GUID(7)='     FPUSTATUS   : FPU status word'
      GUID(8)='     FPUUSED     : Number of stack registers used. 0 is O
     +K.'
      GUID(9)='     FPUOVERFLOW : 0 = OK, 1 = Stack overflow occurred (F
     +PUUSED>8).'
      GUID(10)='The Intel FPU Control word allows to get the program int
     +erupted on various'
      GUID(11)='arithmetics conditions. By default, all interrupts are m
     +asked. Resetting'
      GUID(12)='of some of the mask bits allows for more vigorous arithm
     +etics testing.'
      GUID(13)='We recommend to set the Control Word at least to 1563 to
     + react on'
      GUID(14)='Zero Division and Overflow.'
      GUID(15)='If no parameters are given, the FPU Control word and FPU
     + Stack Status'
      GUID(16)='as shown above are printed.'
      GUID(17)='As calling FPUSTACK affects the FPU control word by sett
     +ing'
      GUID(18)='the lowest 7 bits incidentally disabling all interrupts'
     +//' (Why?),'
      GUID(19)='we get CW before and then re-set it after FPUSTACK is ca
     +lled.'
      GUID(20)='More details can be found in  /usr/include/fpu_control.h
     + or running'
      GUID(21)='"info -f g77 Trouble Missing Floating" or in Intel refer
     +ence manuals.'
      GUID(22)='The following is an abstract from fpu_control.'
      GUID(23)=' The hardware default is 0x037f (o1577), we recommend 0x
     +0373 (o1563)'
      GUID(24)=' * Mask bit: 1 means no interrupt              default '
     +//'  recommeded'
      GUID(25)=' * 0     IM: Invalid operation mask              (1)   '
     +//'      (1)'
      GUID(26)=' * 1     DM: Denormalized operand mask           (1)   '
     +//'      (1)'
      GUID(27)=' * 2     ZM: Zero-divide mask                    (1)   '
     +//'      (0)'
      GUID(28)=' * 3     OM: Overflow mask                       (1)   '
     +//'      (0)'
      GUID(29)=' * 4     UM: Underflow mask                      (1)   '
     +//'      (1)'
      GUID(30)=' * 5     PM: Precision (inexact result) mask     (1)   '
     +//'      (1)'
      GUID(31)=' * 6-7     : reserved                           (01)   '
     +//'     (01)'
      GUID(32)=' * 8-9   PC: Precision control:                 (11)   '
     +//'     (11)'
      GUID(33)=' *       11 - round to extended precision'
      GUID(34)=' *       10 - round to double precision'
      GUID(35)=' *       00 - round to single precision'
      GUID(36)=' * 10-11 RC: Rounding control                   (00)   '
     +//'     (00)'
      GUID(37)=' *       00 - rounding to nearest'
      GUID(38)=' *       01 - rounding down (toward - infinity)'
      GUID(39)=' *       10 - rounding up (toward + infinity)'
      GUID(40)=' *       11 - rounding toward zero'
      GUID(41)=' * 12    IC: Infinity control for 8087 and 80287 (0)   '
     +//'      (0)'
      GUID(42)=' * 13-15     reserved                           (00)   '
     +//'     (00)'
      CALL KUGUID('FPU_CONTROL',GUID,42,'S')
      CALL KUACT('FPU_CONTROL',AGXUSER)

      CALL KUCMD(' ','BUG_REPORT','C')
      GUID(1)=' send a bug report to nevski@cern.ch'
      CALL KUGUID('BUG_REPORT',GUID,1,'S')
      CALL KUACT('BUG_REPORT',AGXUSER)

      CALL KUCMD(' ','AgFilter','C')

      CALL KUCMD('AgFilter',' ','SW')

      CALL KUCMD(' ','GFILTER','C')
      CALL KUPAR('GFILTER','filterName','User defined filtername','C',
     +'S')
      GUID(1)=' GFILTER command provides a way to install user defined f
     +ilter.'
      GUID(2)=' This filter is defined on StMCFilter class. It has two r
     +ejection'
      GUID(3)=' methods. One for EG (Event Generator) and G3 (End of GEA
     +NT3 event)'
      GUID(4)=' Before this command, library containing filter must be l
     +oaded'
      GUID(5)=' Example.'
      GUID(6)=' GEXEC $STAR_LIB)/StMCFilter.so'
      GUID(7)=' GFILTER  myFilterName'
      CALL KUGUID('GFILTER',GUID,7,'S')
      CALL KUACT('GFILTER',AGXUSER)

      CALL KUCMD(' ','GCONFIG','C')
      CALL KUPAR('GCONFIG','filterKey','User defined filter cut','C',
     +'S')
      CALL KUPAR('GCONFIG','filterVal','Value of the cut','R','S')
      GUID(1)='The GFILTCFG command passes a key and a value to the filt
     +er''s parseConfig'
      GUID(2)='method, if it is defined.  The parseConfig method may the
     +n set various'
      GUID(3)='cuts in the filter according to the key/value pair which'
     +//' is provided.'
      CALL KUGUID('GCONFIG',GUID,3,'S')
      CALL KUACT('GCONFIG',AGXUSER)

      CALL KUCMD('..',' ','SW')

      CALL KUCMD('..',' ','SW')

      CALL KUCMD('/',' ','SW')

      END
