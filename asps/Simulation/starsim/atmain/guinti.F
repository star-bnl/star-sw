      SUBROUTINE GUINTI
      CHARACTER*1 BS1
      PARAMETER (BS1='\\')
      PARAMETER (MGUIDL=199)
      CHARACTER*80 GUID
      COMMON /KCGUID/ GUID(MGUIDL)
      EXTERNAL AGSUSER

      CALL KUCMD(' ','USER','C')
      GUID(1)='User defined menu for GSTAR'
      CALL KUGUID('USER',GUID,1,'S')

      CALL KUCMD('USER',' ','SW')

      CALL KUCMD(' ','INPUT','C')
      CALL KUPAR('INPUT','TYPE','Type of event input','O','S')
      CALL KUPVAL('INPUT','TYPE',0,0.,'TX','D')
      CALL KUPVAL('INPUT','TYPE',0,0.,'FZ,TX,FZTX,TXOLD,TXOTX,FZTXO',
     +'V')
      CALL KUPAR('INPUT','INFILE1','First input file name','C','S')
      CALL KUPVAL('INPUT','INFILE1',0,0.,' ','D')
      CALL KUPAR('INPUT','INFILE2',
     +'Second input file name (must be TX)','CO','S')
      CALL KUPVAL('INPUT','INFILE2',0,0.,' ','D')
      GUID(1)='Opens one or two files of events as input to Geant. If re
     +ad types FZ'
      GUID(2)='(Zebra) or TX (Star new text format) are given, only one'
     +//' file of the given'
      GUID(3)='type is opened using the first input filename. If read ty
     +pe FZTX or'
      GUID(4)='TXOTX is given, two files are opened and merged within Ge
     +ant. For'
      GUID(5)='the FZTX option, the first filename must be an FZ file an
     +d the second'
      GUID(6)='filename a TX file (new text format). For the TXOTX, the'
     +//' first file'
      GUID(7)='must be an old format text file, and the second a TX file
     +.'
      GUID(8)='In the FZTX and TXOTX options, if the tracks and vertices
     + of the two files'
      GUID(9)='are to be disentangled again downstream of Geant, it is t
     +he user''s'
      GUID(10)='responsibility to ensure that they are labelled in a uni
     +que way. One'
      GUID(11)='simple way to do this is to make all vertex processes in
     + the TX file'
      GUID(12)='negative.'
      GUID(13)='If a file of the given type is open at the time of this'
     +//' call, it will be'
      GUID(14)='closed before further action is taken.'
      GUID(15)='A TXO ("old text") file should consist of one line conta
     +ining the word'
      GUID(16)='"event", number of  particles (N) and the event number,'
     +//' followed by exactly'
      GUID(17)='N lines each containing geant particle id and px,py,pz o
     +f the particle.'
      GUID(18)='In a TX ("new text") file each line starts with a keywor
     +d each containing:'
      GUID(19)=' GENER:  version  east_z  east_a  west_z  west_a  sqrts'
     +//' b_max'
      GUID(20)=' EVENT:  n_event  n_track  n_vertex'
      GUID(21)=' TRACK:  ge_pid  px  py  pz  LabelTr  StartVx  StopVx  e
     +g_pid'
      GUID(22)=' VERTEX: vx  vy  vz   LabelVx  eg_proc  parent'
      GUID(23)='In both cases reading is free formatted, case sensetive.
     +'
      CALL KUGUID('INPUT',GUID,23,'S')
      CALL KUACT('INPUT',AGSUSER)

      CALL KUCMD(' ','OUTPUT','C')
      CALL KUPAR('OUTPUT','ACTION','Open or close output file','O','S')
      CALL KUPVAL('OUTPUT','ACTION',0,0.,' ','D')
      CALL KUPVAL('OUTPUT','ACTION',0,0.,'O,C','V')
      CALL KUPAR('OUTPUT','OUTFILE','FZ output file name','CO','S')
      CALL KUPVAL('OUTPUT','OUTFILE',0,0.,'geant.fzd','D')
      GUID(1)='Opens/Closes an FZ output file.'
      CALL KUGUID('OUTPUT',GUID,1,'S')
      CALL KUACT('OUTPUT',AGSUSER)

      CALL KUCMD(' ','PHASESPACE','C')
      CALL KUPAR('PHASESPACE','NTRACK','Number of tracks per event',
     +'IO','S')
      CALL KUPVAL('PHASESPACE','NTRACK',100,0.,' ','D')
      CALL KUPAR('PHASESPACE','ID','Geant Particle ID','IO','S')
      CALL KUPVAL('PHASESPACE','ID',8,0.,' ','D')
      CALL KUPAR('PHASESPACE','PTLOW','Lower limit of pT','RO','S')
      CALL KUPVAL('PHASESPACE','PTLOW',0,0.,' ','D')
      CALL KUPAR('PHASESPACE','PTHIGH','Upper limit of pT','RO','S')
      CALL KUPVAL('PHASESPACE','PTHIGH',0,1.,' ','D')
      CALL KUPAR('PHASESPACE','YLOW','Lower limit of rapidity','RO','S')
      CALL KUPVAL('PHASESPACE','YLOW',0,-1.,' ','D')
      CALL KUPAR('PHASESPACE','YHIGH','Upper limit of rapidity','RO',
     +'S')
      CALL KUPVAL('PHASESPACE','YHIGH',0,1.,' ','D')
      CALL KUPAR('PHASESPACE','PHILOW','Lower limit of phi','RO','S')
      CALL KUPVAL('PHASESPACE','PHILOW',0,0.,' ','D')
      CALL KUPAR('PHASESPACE','PHIHIGH','Upper limit of phi','RO','S')
      CALL KUPVAL('PHASESPACE','PHIHIGH',0,6.283,' ','D')
      GUID(1)='Generates flat phase space in place of input file of even
     +ts'
      GUID(2)='(uniform in pT and rapidity).'
      GUID(3)='Parameters are Geant PID, lower and upper bounds of rapid
     +ity interval,'
      GUID(4)='lower and upper bounds of pT interval, and number of trac
     +ks per'
      GUID(5)='event. Full azimuthal interval is used (0<phi<two pi radi
     +ans).'
      GUID(6)='If no parameters are given, old values existing in the pr
     +ogram are preserved.'
      GUID(7)='Initial limits are 0<pT<10000, -10<y<+10, 0<phi<2pi.'
      CALL KUGUID('PHASESPACE',GUID,7,'S')
      CALL KUACT('PHASESPACE',AGSUSER)

      CALL KUCMD(' ','MOMENTUMBIN','C')
      CALL KUPAR('MOMENTUMBIN','NTRACK','Number of tracks per event',
     +'IO','S')
      CALL KUPVAL('MOMENTUMBIN','NTRACK',100,0.,' ','D')
      CALL KUPAR('MOMENTUMBIN','ID','Geant Particle ID','IO','S')
      CALL KUPVAL('MOMENTUMBIN','ID',8,0.,' ','D')
      CALL KUPAR('MOMENTUMBIN','PXLOW','Lower limit of px','RO','S')
      CALL KUPVAL('MOMENTUMBIN','PXLOW',0,-1.,' ','D')
      CALL KUPAR('MOMENTUMBIN','PXHIGH','Upper limit of px','RO','S')
      CALL KUPVAL('MOMENTUMBIN','PXHIGH',0,1.,' ','D')
      CALL KUPAR('MOMENTUMBIN','PYLOW','Lower limit of py','RO','S')
      CALL KUPVAL('MOMENTUMBIN','PYLOW',0,-1.,' ','D')
      CALL KUPAR('MOMENTUMBIN','PYHIGH','Upper limit of py','RO','S')
      CALL KUPVAL('MOMENTUMBIN','PYHIGH',0,1.,' ','D')
      CALL KUPAR('MOMENTUMBIN','PZLOW','Lower limit of pz','RO','S')
      CALL KUPVAL('MOMENTUMBIN','PZLOW',0,-1.,' ','D')
      CALL KUPAR('MOMENTUMBIN','PZHIGH','Upper limit of pz','RO','S')
      CALL KUPVAL('MOMENTUMBIN','PZHIGH',0,1.,' ','D')
      GUID(1)='Generates uniform distribution within given 3-momentum bi
     +n in place of'
      GUID(2)='input file of events.'
      GUID(3)='Parameters are Geant PID, lower and upper bounds'
      GUID(4)='of px, py and pz, and number of tracks per event.'
      GUID(5)='If no parameters are given, 100 pi+ will be generated per
     + event,'
      GUID(6)='distributed uniformly in the interval -1<px<1, -1<py<1, -
     +1<pz<1 GeV.'
      GUID(7)='If px_high is less than px_low, then px_high will be set'
     +//' equal to px_low'
      GUID(8)='(i.e. px will have the same value for all tracks),'
      GUID(9)='and similarly for py and pz.'
      CALL KUGUID('MOMENTUMBIN',GUID,9,'S')
      CALL KUACT('MOMENTUMBIN',AGSUSER)

      CALL KUCMD(' ','SKIP','C')
      CALL KUPAR('SKIP','NEVENT','Number of input events to skip','I',
     +'S')
      CALL KUPVAL('SKIP','NEVENT',0,0.,' ','D')
      CALL KUPAR('SKIP','NSUBEVENT',
     +'Number of subevents to skip in first processed event','IO','S')
      CALL KUPVAL('SKIP','NSUBEVENT',0,0.,' ','D')
      CALL KUPAR('SKIP','SUBRAN1',
     +'First random seed at start of first processed subevent','IO','S')
      CALL KUPVAL('SKIP','SUBRAN1',0,0.,' ','D')
      CALL KUPAR('SKIP','SUBRAN2',
     +'Second random seed at start of first processed subevent','IO',
     +'S')
      CALL KUPVAL('SKIP','SUBRAN2',0,0.,' ','D')
      GUID(1)='Skips the next NEVENT events of input event file. If NSUB
     +EVENT>0,'
      GUID(2)='skips first NSUBEVENT subevents in first processed event,
     + using random'
      GUID(3)='seeds SUBRAN1 and SUBRAN2. This allows the user to set th
     +e seeds to'
      GUID(4)='randomize the target position correctly at the beginning'
     +//' of the desired'
      GUID(5)='event and then skip directly to any subevent.'
      CALL KUGUID('SKIP',GUID,5,'S')
      CALL KUACT('SKIP',AGSUSER)

      CALL KUCMD(' ','UDECAY','C')
      CALL KUPAR('UDECAY','PIDPARENT','Geant PID of parent','I','S')
      CALL KUPVAL('UDECAY','PIDPARENT',11,0.,' ','D')
      CALL KUPAR('UDECAY','RIN',
     +'Inner limit of radial interval for uniform decay','R','S')
      CALL KUPVAL('UDECAY','RIN',0,10.,' ','D')
      CALL KUPAR('UDECAY','ROUT',
     +'Outer limit of radial interval for uniform decay','R','S')
      CALL KUPVAL('UDECAY','ROUT',0,100.,' ','D')
      CALL KUPAR('UDECAY','PID1','Geant PID of first decay daughter',
     +'IO','S')
      CALL KUPVAL('UDECAY','PID1',0,0.,' ','D')
      CALL KUPAR('UDECAY','PID2','Geant PID of second decay daughter',
     +'IO','S')
      CALL KUPVAL('UDECAY','PID2',0,0.,' ','D')
      CALL KUPAR('UDECAY','PID3','Geant PID of third decay daughter',
     +'IO','S')
      CALL KUPVAL('UDECAY','PID3',0,0.,' ','D')
      GUID(1)='Initializes parameters for the decay of a particle with r
     +oughly uniform'
      GUID(2)='probability along its trajectory between the cylindrical'
     +//' surfaces'
      GUID(3)='R=RIN and R=ROUT. This can be used to enhance statistics'
     +//' for the'
      GUID(4)='efficiency studies of the reconstruction of decays.'
      GUID(5)='Since the path length to the generated vertex and the par
     +ent'
      GUID(6)='4-momentum are known, the appropriate weighting factor fo
     +r'
      GUID(7)='the vertex due to the lifetime of the parent can be calcu
     +lated.'
      GUID(8)='Decay is generated according to the parent branchings.'
      GUID(9)='If needed, they can be modified by user with SPART comman
     +d.'
      CALL KUGUID('UDECAY',GUID,9,'S')
      CALL KUACT('UDECAY',AGSUSER)

      CALL KUCMD(' ','SPARTSTAR','C')
      GUID(1)=' Obsolete command. Use GEANT/CONTROL/SPART instead'
      CALL KUGUID('SPARTSTAR',GUID,1,'S')
      CALL KUACT('SPARTSTAR',AGSUSER)

      CALL KUCMD(' ','GFDK','C')
      CALL KUPAR('GFDK','IPART','Geant PID','IO','S')
      CALL KUPVAL('GFDK','IPART',0,0.,' ','D')
      GUID(1)='Writes out decay modes for particle id IPART.'
      CALL KUGUID('GFDK',GUID,1,'S')
      CALL KUACT('GFDK',AGSUSER)

      CALL KUCMD(' ','SECONDARIES','C')
      CALL KUPAR('SECONDARIES','SCND','secondaraies treatment flag','I',
     +'S')
      CALL KUPVAL('SECONDARIES','SCND',1,0.,' ','D')
      CALL KUPVAL('SECONDARIES','SCND',0,0.,' ','L')
      CALL KUPVAL('SECONDARIES','SCND',2,0.,' ','H')
      GUID(1)='Controls the way how secondary particles are treated:'
      GUID(2)=' 0 - secondaries ignored;'
      GUID(3)=' 1 - secondaries are put in jstak only (standard)'
      GUID(4)=' 2 - some secondaries are saved in KINE bank.'
      GUID(5)='     This concerns decay products of particles, already e
     +xisting in KINE.'
      GUID(6)='     Other products (than decay) of these particles are s
     +aved in KINE'
      GUID(7)='     only if they are produced in specially named media,'
     +//' which should'
      GUID(8)='     be different from the medium, where the parent was b
     +orn.'
      CALL KUGUID('SECONDARIES',GUID,8,'S')
      CALL KUACT('SECONDARIES',AGSUSER)

      CALL KUCMD(' ','VXYZ','C')
      CALL KUPAR('VXYZ','VX',' primary vertex x','R','S')
      CALL KUPVAL('VXYZ','VX',0,0.,' ','D')
      CALL KUPVAL('VXYZ','VX',0,-1000.,' ','L')
      CALL KUPVAL('VXYZ','VX',0,1000.,' ','H')
      CALL KUPAR('VXYZ','VY',' primary vertex y','R','S')
      CALL KUPVAL('VXYZ','VY',0,0.,' ','D')
      CALL KUPVAL('VXYZ','VY',0,-1000.,' ','L')
      CALL KUPVAL('VXYZ','VY',0,1000.,' ','H')
      CALL KUPAR('VXYZ','VZ',' primary vertex z','R','S')
      CALL KUPVAL('VXYZ','VZ',0,0.,' ','D')
      CALL KUPVAL('VXYZ','VZ',0,-1000.,' ','L')
      CALL KUPVAL('VXYZ','VZ',0,1000.,' ','H')
      GUID(1)='Displaces an average position of the interactions vertex'
     +//' in (Vx,Vy,Vz)'
      CALL KUGUID('VXYZ',GUID,1,'S')
      CALL KUACT('VXYZ',AGSUSER)

      CALL KUCMD(' ','VSIG','C')
      CALL KUPAR('VSIG','VSIGT','transverse vertex spread','R','S')
      CALL KUPVAL('VSIG','VSIGT',0,0.,' ','D')
      CALL KUPVAL('VSIG','VSIGT',0,0.,' ','L')
      CALL KUPVAL('VSIG','VSIGT',0,1000.,' ','H')
      CALL KUPAR('VSIG','VSIGZ','longitudinal vertex spread','R','S')
      CALL KUPVAL('VSIG','VSIGZ',0,0.,' ','D')
      CALL KUPVAL('VSIG','VSIGZ',0,0.,' ','L')
      CALL KUPVAL('VSIG','VSIGZ',0,1000.,' ','H')
      GUID(1)='Defines sigma if the generated vertex spread both in tran
     +sverse and'
      GUID(2)='longitudinal(along the beam axis) direction.'
      CALL KUGUID('VSIG',GUID,2,'S')
      CALL KUACT('VSIG',AGSUSER)

      CALL KUCMD(' ','SUBEVENT','C')
      CALL KUPAR('SUBEVENT','NSUB',
     +'number of final state tracks per sub-event','I','S')
      CALL KUPVAL('SUBEVENT','NSUB',200,0.,' ','D')
      CALL KUPVAL('SUBEVENT','NSUB',0,0.,' ','L')
      CALL KUPVAL('SUBEVENT','NSUB',10000000,0.,' ','H')
      CALL KUPAR('SUBEVENT','Ntrk_max',
     +'maximum of tracks per single KINE bank','IO','S')
      CALL KUPVAL('SUBEVENT','Ntrk_max',64000,0.,' ','D')
      CALL KUPVAL('SUBEVENT','Ntrk_max',0,0.,' ','L')
      CALL KUPVAL('SUBEVENT','Ntrk_max',10000000,0.,' ','H')
      CALL KUPAR('SUBEVENT','Nhit_max',
     +'maximum length of a single HITS bank','IO','S')
      CALL KUPVAL('SUBEVENT','Nhit_max',10000000,0.,' ','D')
      CALL KUPVAL('SUBEVENT','Nhit_max',0,0.,' ','L')
      CALL KUPVAL('SUBEVENT','Nhit_max',10000000,0.,' ','H')
      GUID(1)='Controls splitting of full events into a number of smalle
     +r sub-events'
      GUID(2)='in the GEANT simulation phase.'
      CALL KUGUID('SUBEVENT',GUID,2,'S')
      CALL KUACT('SUBEVENT',AGSUSER)

      CALL KUCMD(' ','SHADOW','C')
      CALL KUPAR('SHADOW','SHDO','tracking flag for dense materials',
     +'I','S')
      CALL KUPVAL('SHADOW','SHDO',1,0.,' ','D')
      CALL KUPVAL('SHADOW','SHDO',0,0.,' ','L')
      CALL KUPVAL('SHADOW','SHDO',1,0.,' ','H')
      GUID(1)='Flag to set tracking threshold in dense materials very hi
     +gh.'
      GUID(2)='This will prevent showering in magnets and other dense ob
     +jects,'
      GUID(3)='but they will still block unphysical tracks'
      CALL KUGUID('SHADOW',GUID,3,'S')
      CALL KUACT('SHADOW',AGSUSER)

      CALL KUCMD(' ','SENSECUTE','C')
      CALL KUPAR('SENSECUTE','CUTE','electron tracking cut','R','S')
      CALL KUPVAL('SENSECUTE','CUTE',0,-1.,' ','D')
      CALL KUPVAL('SENSECUTE','CUTE',0,-1.0,' ','L')
      CALL KUPVAL('SENSECUTE','CUTE',0,10.0,' ','H')
      GUID(1)='Changing electron tracking cut in sensitive gases one can
     + control'
      GUID(2)='delta-electron production in detector itself.'
      CALL KUGUID('SENSECUTE',GUID,2,'S')
      CALL KUACT('SENSECUTE',AGSUSER)

      CALL KUCMD('..',' ','SW')

      CALL KUCMD('/',' ','SW')

      END
