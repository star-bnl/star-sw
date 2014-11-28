      MODULE MUTDGEO4    is the geometry of the STAR MTD, WMRPC Version
      +CDE,agecom.
      +CDE,gcunit.
      Author W.J. Llope
      Created    21 July 2010
      CONTENT MUTD,MTMT,MTMF,MTEB,MTTF,MTTT,MTRA,MTBP,MTTP,MCHL,
      MCAL,MCHS,MCAS,MIGS,MIGG
      STRUCTURE MTDG { Version,Config}
      STRUCTURE MTDD { Rmin,Rmax,dZmother,BackLegR,BemcElectBoxdX,BemcElectBoxdY,
            BemcElectBoxdZ3,BemcElectBoxdZ5,Rgap,MtdMotherdX,MtdMotherdY,MtdMotherdZ3,MtdMotherdZ5,
            MtdTraydX,MtdTraydY,MtdTraydZ,MtdBPlate,MtdTPlate,MtdIGstackdY,MtdIGstackdZ,
            MtdIGstackNgaps,MtdIGstackGapThick,MtdIGstackThinGlassThick}
      INTEGER kTrayConfig,iphi,iBL,Ntrayplaced,igap,ngaps

      REAL thisphi,thisx,chandim,thish


      <W> ; ('Creating 12/19/2012 AgML version of mutdgeo4');



      FILL MTDG		! Muon system configuration parameters
      Version = 1 !  version number
      Config = 1 !  =1 single backleg (Run11), =2 full system (Run13)
      ENDFILL
      FILL MTDD		! Muon system geometry parameters
      Rmin = 364.25 !  integration radius lower
      Rmax = 415.0 !  integration radius upper
      dZmother = 300.0 !  integration half length in Z
      BackLegR = 365.0 !  radius of a backleg at phi-center
      BemcElectBoxdX = 11.035 !  BEMC electronics box half-length X (height)
      BemcElectBoxdY = 30.65 !  BEMC electronics box half-length Y (width)
      BemcElectBoxdZ5 = 290.0 !  BEMC electronics box half-length Z (length 5-pack)
      BemcElectBoxdZ3 = 200.0 !  BEMC electronics box half-length Z (length 3-pack)
      Rgap = 3.81 !  Distance top of Bemc box to underside of lowest tray
      MtdMotherdX = 5.5 !  Tray Mother half-length X (height)
      MtdMotherdY = 32.0 !  Tray Mother half-length Y (width)
      MtdMotherdZ5 = 285.0 !  Tray Mother half-length Z (length long)
      MtdMotherdZ3 = 195.0 !  Tray Mother half-length Z (length short)
      MtdTraydX = 2.4956 !  Tray half-height
      MtdTraydY = 33.765 !  Tray half-width
      MtdTraydZ = 54.005 !  Tray half-length
      MtdBPlate = 0.3175 !  Tray bottom plate thickness
      MtdTPlate = 0.2286 !  Tray top plate thickness
      MtdIGstackdY = 26.1 !  WMRPC inner glass stack volume half-length Y
      MtdIGstackdZ = 43.5 !  WMRPC inner glass stack volume half-length Y
      MtdIGstackNgaps = 5.0 !  WMPRC number of gas gaps
      MtdIGstackGapThick = 0.025 !  WMRPC gas thickness (0.25mm)
      MtdIGstackThinGlassThick = 0.07 !  WMRPC thin glass thickness (0.7mm)
      ENDFILL
      use MTDG
      use MTDD
      Prin0 MTDG_Version;
      (' MuTD: MTDG_Version = ', F5.3)
      Prin0 MTDG_Config;
      (' MuTD: MTDG_Config = ', F5.3)
      kTrayConfig = NINT(MTDG_Config)
      ngaps   = MTDD_MtdIGstackNgaps
      COMPONENT H A=1 Z=1 W=0.90*2*1./102.+0.+0.05*10*1./58.
      COMPONENT C A=12 Z=6 W=0.90*2*12./102.+0.+0.05*4*12./58.
      COMPONENT F A=19 Z=9 W=0.90*4*19./102.+0.05*6*19./146.+0.
      COMPONENT S A=32 Z=16 W=0.+0.05*1*32./146.+0.
      MIXTURE RPCgas dens=4.55E-3
      COMPONENT Si A=28 Z=14 W=1.
      COMPONENT O A=16 Z=8 W=2.
      MIXTURE Glass dens=2.5
      CREATE MUTD
      POSITION MUTD in CAVE 
      ! ---------------------------------------------------------------------------------- MUTD
      BLOCK MUTD   is the muon detector mother
      MATERIAL Air
      MEDIUM Standard
      ATTRIBUTE MUTD seen=0 colo=1
      SHAPE tube rmin=MTDD_Rmin rmax=MTDD_Rmax,
            dz=MTDD_dZmother
      Ntrayplaced     = 0
      CREATE MTMT
      IF ( kTrayConfig.eq.4 ) THEN
      thisphi = 156
      POSITION MTMT z=0 alphaz=150 
      Ntrayplaced = Ntrayplaced + 3
      ELSEIF ( kTrayConfig.eq.5 ) THEN
      CREATE MTMF
      DO iBL = 1, 30
      thisphi = 90.0 + (iBL-1)*(-12.0)
      IF ( iBL.ne.8.and.iBL.ne.9.and.iBL.ne.23.and.iBL.ne.24 ) THEN
      IF ( iBL.ge.12.and.iBL.le.20 ) THEN
      POSITION MTMT z=0 alphaz=thisphi 
      Ntrayplaced = Ntrayplaced + 3
      ELSE
      POSITION MTMF z=0 alphaz=thisphi 
      Ntrayplaced = Ntrayplaced + 5
      ENDIF
      ENDIF
      END DO
      ELSEIF ( kTrayConfig.eq.12 ) THEN
      CREATE MTMF
      DO iBL = 26, 28
      thisphi = 90.0 + (iBL-1)*(-12.0)
      IF ( iBL.eq.26 ) THEN
      POSITION MTMT z=0 alphaz=thisphi 
      Ntrayplaced = Ntrayplaced + 3
      ELSE
      POSITION MTMF z=0 alphaz=thisphi 
      Ntrayplaced = Ntrayplaced + 5
      ENDIF
      END DO
      ELSEIF ( kTrayConfig.eq.13 ) THEN
      <W> kTrayConfig;  ('Creating MTD geometry with kTrayConfig=', I3);
      CREATE MTMF
      DO iBL = 1, 30
      thisphi = 90.0 + (iBL-1)*(-12.0)
      IF ( (iBL.ne.8.and.iBL.ne.9.and.iBL.ne.23.and.iBL.ne.24) ) THEN
      IF ( iBL.le.10.or.iBL.ge.22 ) THEN
      POSITION MTMF z=0 alphaz=thisphi 
      Ntrayplaced = Ntrayplaced + 5
      ENDIF
      ENDIF
      END DO
      ENDIF
      ENDBLOCK
      ! ---------------------------------------------------------------------------------- MTMF
      BLOCK MTMF   is the backleg mother that encloses five trays
      MATERIAL Air
      MEDIUM Standard
      ATTRIBUTE MTMF seen=1 colo=6
      SHAPE tubs phi1=-6.0 phi2=6.0
      thisx      = MTDD_BackLegR + MTDD_BemcElectBoxdX
      CREATE MTEB dz=MTDD_BemcElectBoxdZ5
      POSITION MTEB x=thisx 
      thisx      = MTDD_BackLegR + 2.*(MTDD_BemcElectBoxdX + MTDD_Rgap) + MTDD_MtdMotherdX
      CREATE MTTF dz=MTDD_MtdMotherdZ5
      POSITION MTTF x=thisx 
      ENDBLOCK
      ! ---------------------------------------------------------------------------------- MTMT
      BLOCK MTMT   is the backleg mother that encloses three trays
      MATERIAL Air
      MEDIUM Standard
      ATTRIBUTE MTMT seen=1 colo=4
      SHAPE tubs phi1=-6.0 phi2=6.0
      thisx      = MTDD_BackLegR + MTDD_BemcElectBoxdX
      CREATE MTEB dz=MTDD_BemcElectBoxdZ3
      POSITION MTEB x=thisx 
      thisx      = MTDD_BackLegR + 2.*(MTDD_BemcElectBoxdX + MTDD_Rgap) + MTDD_MtdMotherdX
      CREATE MTTT dz=MTDD_MtdMotherdZ3
      POSITION MTTT x=thisx 
      ENDBLOCK
      ! ---------------------------------------------------------------------------------- MTEB
      BLOCK MTEB   is the bemc electronics box
      MATERIAL Air
      MEDIUM Standard
      ATTRIBUTE MTEB seen=1 colo=1
      SHAPE box dx=MTDD_BemcElectBoxdX dy=MTDD_BemcElectBoxdY
      ENDBLOCK
      ! ---------------------------------------------------------------------------------- MTTF
      BLOCK MTTF   is the MTD11 5-tray group mother
      MATERIAL Air
      MEDIUM Standard
      ATTRIBUTE MTTF seen=1 colo=3
      SHAPE box dx=MTDD_MtdMotherdX dy=MTDD_MtdMotherdY
      CREATE MTRA
      POSITION MTRA x=MTDD_MtdTraydX z=-174 
      POSITION MTRA x=-MTDD_MtdTraydX z=-87 
      POSITION MTRA x=MTDD_MtdTraydX z=0 
      POSITION MTRA x=-MTDD_MtdTraydX z=87 
      POSITION MTRA x=MTDD_MtdTraydX z=174 
      ENDBLOCK
      ! ---------------------------------------------------------------------------------- MTTT
      BLOCK MTTT   is the MTD11 3-tray group mother
      MATERIAL Air
      MEDIUM Standard
      ATTRIBUTE MTTT seen=1 colo=3
      SHAPE box dx=MTDD_MtdMotherdX dy=MTDD_MtdMotherdY
      CREATE MTRA
      POSITION MTRA x=-MTDD_MtdTraydX z=-87 
      CREATE MTRA
      POSITION MTRA x=MTDD_MtdTraydX z=0 
      CREATE MTRA
      POSITION MTRA x=-MTDD_MtdTraydX z=87 
      ENDBLOCK
      ! ---------------------------------------------------------------------------------- MTRA
      BLOCK MTRA   is an MTD11-style tray
      MATERIAL RPCgas
      MEDIUM Standard
      ATTRIBUTE MTRA seen=1 colo=2
      SHAPE box dx=MTDD_MtdTraydX dy=MTDD_MtdTraydY,
            dz=MTDD_MtdTraydZ
      CREATE MTBP dx=MTDD_MtdBPlate/2.
      POSITION MTBP x=-MTDD_MtdTraydX+(MTDD_MtdBPlate/2.) 
      CREATE MTTP dx=MTDD_MtdTPlate/2.
      POSITION MTTP x=MTDD_MtdTraydX-(MTDD_MtdTPlate/2.) 
      chandim = MTDD_MtdTraydX - MTDD_MtdBPlate/2. - MTDD_MtdBPlate/2.
      CREATE MCHL dz=MTDD_MtdTraydZ-2.*chandim dx=chandim dy=chandim
      POSITION MCHL x=-MTDD_MtdTraydX+MTDD_MtdBPlate+chandim y=MTDD_MtdTraydY-chandim  _
	z=0 
      POSITION MCHL x=-MTDD_MtdTraydX+MTDD_MtdBPlate+chandim  _
	y=-MTDD_MtdTraydY+chandim z=0 alphaz=180 
      CREATE MCHS dz=chandim dx=chandim dy=MTDD_MtdTraydY
      POSITION MCHS x=-MTDD_MtdTraydX+MTDD_MtdBPlate+chandim y=0  _
	z=MTDD_MtdTraydZ-chandim 
      POSITION MCHS x=-MTDD_MtdTraydX+MTDD_MtdBPlate+chandim y=0  _
	z=-MTDD_MtdTraydZ+chandim alphay=180 
      thish   =        MTDD_MtdIGstackNgaps*MTDD_MtdIGstackGapThick + (MTDD_MtdIGstackNgaps _
            - 1.0)*MTDD_MtdIGstackThinGlassThick
      CREATE MIGS dz=MTDD_MtdIGstackdZ dx=thish/2.0 dy=MTDD_MtdIGstackdY
      POSITION MIGS x=0 y=0 z=0 
      ENDBLOCK
      ! ---------------------------------------------------------------------------------- MIGS
      BLOCK MIGS   is the inner glass stack
      ATTRIBUTE MIGS seen=1 colo=4
      MATERIAL Glass
      SHAPE box
      CREATE MIGG dz=MTDD_MtdIGstackdZ dx=MTDD_MtdIGstackGapThick/2.0 dy=MTDD_MtdIGstackdY
      IF ( ngaps.eq.6 ) THEN
      POSITION MIGG x=-2.5*MTDD_MtdIGstackThinGlassThick-2.5*MTDD_MtdIGstackGapThick 
      POSITION MIGG x=-1.5*MTDD_MtdIGstackThinGlassThick-1.5*MTDD_MtdIGstackGapThick 
      POSITION MIGG x=-0.5*MTDD_MtdIGstackThinGlassThick-0.5*MTDD_MtdIGstackGapThick 
      POSITION MIGG x= 0.5*MTDD_MtdIGstackThinGlassThick+0.5*MTDD_MtdIGstackGapThick 
      POSITION MIGG x= 1.5*MTDD_MtdIGstackThinGlassThick+1.5*MTDD_MtdIGstackGapThick 
      POSITION MIGG x= 2.5*MTDD_MtdIGstackThinGlassThick+2.5*MTDD_MtdIGstackGapThick 
      ELSEIF ( ngaps.eq.5 ) THEN
      POSITION MIGG x=-2.0*MTDD_MtdIGstackThinGlassThick-2.0*MTDD_MtdIGstackGapThick 
      POSITION MIGG x=-1.0*MTDD_MtdIGstackThinGlassThick-1.0*MTDD_MtdIGstackGapThick 
      POSITION MIGG x= 0.0*MTDD_MtdIGstackThinGlassThick+0.0*MTDD_MtdIGstackGapThick 
      POSITION MIGG x= 1.0*MTDD_MtdIGstackThinGlassThick+1.0*MTDD_MtdIGstackGapThick 
      POSITION MIGG x= 2.0*MTDD_MtdIGstackThinGlassThick+2.0*MTDD_MtdIGstackGapThick 
      ENDIF
      ENDBLOCK
      ! ---------------------------------------------------------------------------------- MIGG
      BLOCK MIGG   is a single gas gap
      ATTRIBUTE MIGG seen=1 colo=7
      MATERIAL RPCgas
      SHAPE box
      HITS MIGG x:.01:S y:.01: z:.01: ptot:18:(0,100) cx:10: cy:10: cz:10: sleng:.1:(0,500),
            tof:16: step:.01: eloss:16:
      ENDBLOCK
      ! ---------------------------------------------------------------------------------- MTBP
      BLOCK MTBP   is the MTD11 bottom plate
      MATERIAL Aluminium
      MEDIUM Standard
      ATTRIBUTE MTBP seen=1 colo=1
      SHAPE box
      ENDBLOCK
      ! ---------------------------------------------------------------------------------- MTTP
      BLOCK MTTP   is the MTD11 top plate
      MATERIAL Aluminium
      MEDIUM Standard
      ATTRIBUTE MTTP seen=1 colo=1
      SHAPE box
      ENDBLOCK
      ! ---------------------------------------------------------------------------------- MCHL
      BLOCK MCHL   is the MTD11 architectural channel long side
      MATERIAL Aluminium
      MEDIUM Standard
      ATTRIBUTE MCHL seen=1 colo=1
      SHAPE box
      CREATE MCAL dz=MTDD_MtdTraydZ-2.*chandim dx=chandim-0.3175 dy=chandim-0.3175/2.
      POSITION MCAL x=0 y=0.3175/2. z=0 
      ENDBLOCK
      ! ---------------------------------------------------------------------------------- MCAL
      BLOCK MCAL   is the air in the MTD11 architectural channel long side
      MATERIAL Air
      MEDIUM Standard
      ATTRIBUTE MCAL seen=1 colo=7
      SHAPE box
      ENDBLOCK
      ! ---------------------------------------------------------------------------------- MCHS
      BLOCK MCHS   is the MTD11 architectural channel short side
      MATERIAL Aluminium
      MEDIUM Standard
      ATTRIBUTE MCHS seen=1 colo=1
      SHAPE box
      CREATE MCAS dz=chandim-0.3175/2. dx=chandim-0.3175 dy=MTDD_MtdTraydY
      POSITION MCAS x=0 y=0 z=+0.3175/2. 
      ENDBLOCK
      ! ---------------------------------------------------------------------------------- MCAS
      BLOCK MCAS   is the air in the MTD11 architectural channel short side
      MATERIAL Air
      MEDIUM Standard
      ATTRIBUTE MCAS seen=1 colo=7
      SHAPE box
      ENDBLOCK
      <W> '===>> 	[End AgML Module MUTDGEO4]	 <<==='; (A32,/,/);
      END! Module MUTDGEO4
