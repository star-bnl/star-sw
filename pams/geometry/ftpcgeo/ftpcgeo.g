******************************************************************************
Module   FTPCGEO  is the geometry of the Forward TPC in STAR
Author   Michael Konrad
Created  18-Okt-96
******************************************************************************
+CDE,AGECOM,GCONST,GCUNIT.
*
	Content FTPC, FIAL, FMPT, FOAL, FDUM, FGAS, FSEN, FSEC,
		FIFR, FKWI, FFSL, FFCE, FROC, FROM, FROA,
		FROG, FROE, FRCC, FRPB, FRSB, FSER, FSEI,
		FSDR, FSIL, FSFL, FSSB, FSSP, FSPH, FSTB,
		FSTP, FFRA, FFRB, FFRC, FFRD, FFRE, FFRF, 
		FFRG, FFRH, FFRI, FFRJ, FFRK, FFRL, FFRM, 
		FFRN, FFRO, FFRP, FFRQ 
*
	structure FTPG { Version, Rinnermost, Routermost,
                         RGasOut, Zstart, totLen, LayLen,
                         Hitlay, DrInAlLay1, DrInAlLay2,
                         DrInIsoLay, DzKaptonW, DrIFR, DzIFR}
*
	structure FFCC {Version, StiLeng, StiDia, StiRpos, RiThick,
                        RiDr, RiGap, BarLeng, BarWidt, BarThik}
*
	structure FRBD {Version, Phi1, Phi2, ModLeng, GlassDr, 
                        AlDr, PcbDr, ElektrDr, CoolDr, PhibarDr, 
                        PhibarTh, SidebarDr, SidebarWi}
*
	structure FSSD {Version, EringRmax, EringTh, EringWTh, 
                        DringDr, DringWi, IringRmax, IringTh, 
                        FringDr, FringWi, SbarWi, SbarTh,
                        SplaLe, SplaWi, SplaTh, SplaDia, TbarWi, 
                        TbarHe, TplaWi, TplaTh}
*
	Integer k,n
	Real position, temp1, temp2, temp3
	Integer Agexist
*
* ----------------------------------------------------------------------------
   Fill FTPG	    ! basic FTPC data
	Version		= 1	! geometry Version
	Rinnermost	= 7.5   ! innermost radius of envelope
	Routermost	= 36.5	! outermost radius of envelope
	RGasOut		= 30.5  ! outer radius of the gas-volume
	Zstart		= 150	! distance from the interaction point
	totLen		= 120	! overall length
        LayLen          = 0.1   ! thickness of the sensitive Layer
        Hitlay          = 10    ! # of padrows in one FTPC : 10
	DrInAlLay1	= 0.05  ! thickness of inner Al-Layer of inner Tube
	DrInAlLay2	= 0.05  ! thickness of outer Al-Layer of inner Tube
	DrInIsoLay	= 0.4	! thickness of plastic insulation of inner tube
	DzKaptonW	= 0.02	! thickness of a double kapton-windows
	DrIFR		= 1.15	! thickness (r) of inner flange ring
	DzIFR		= 0.4	! thickness (z) of inner flange ring
   Endfill
*
   Fill FFCC	    ! Field-Cage Caps data
	Version		= 1	! geometry Version
	StiLeng		= 21	! lenth of the ceramic-holders
	StiDia		= .8	! diameter of ceramic-holders
	StiRpos		= 19.25	! r-pos. of ceramic-holders
	RiThick		= .06	! thickness of the FC rings
	RiDr		= 1	! width (r) if the FC rings
	RiGap		= .3	! gap between two FC rings
	BarLeng		= 29	! length of stabilizer bar for FC 
	BarWidt		= 2	! width of stabilizer bar for FC
	BarThik		= .8	! thickness of stabilizer bar for FC  
   Endfill
*
   Fill FRBD           ! Readout Barrel Design
	Version		= 1	! geometry Version
	Phi1		= 0	! lower bound of phi
	Phi2		= 60	! upper bound of phi
	ModLeng		= 16.6	! length (z) of the module
	GlassDr		= .03	! glass thickness
	AlDr		= .05	! Al layer thickness
	PcbDr		= .1	! PC board thickness
	ElektrDr	= .05	! electronics thickness
	CoolDr		= .1	! cooling plate thickness
	PhibarDr	= 1.5	! height (r) of the phibar
	PhibarTh	= .8	! thickness (z) of the phibar
	SidebarDr	= 1.9	! Sidebar Thickness
	SidebarWi	= 2.2	! Sidebar Width
Endfill
*
   Fill FSSD           ! Support Structure Design
	Version		= 1	! geometry Version
	EringRmax	= 36.5	! endring outer radius
	EringTh		= 6.7	! endring thickness (z) 
	EringWTh	= .9	! wall thickness of the endring
	DringDr		= 1.75	! distance ring thickness (r)
	DringWi		= 2.35	! distance ring width (z)
	IringRmax	= 36.5	! intersection ring outer radius
	IringTh		= .9	! intersection ring thickness (z) 
	FringDr		= 1.75	! flange ring thickness (r)
	FringWi		= 1.9	! flange ring width (z)
	SbarWi		= 6.1	! side bar width (phi)
	SbarTh		= 1.7	! side bar thickness (r)
	SplaLe		= 20.4	! side plate length (z)
	SplaWi		= 18.0	! side plate width (phi)
	SplaTh		= .5	! side plate thickness (r)
	SplaDia		= 16.0	! side plate hole diameter
	TbarWi		= 1.7	! top bar width (phi)
	TbarHe		= 1.7	! top bar height (r)
	TplaWi		= 6.4	! top plate width (phi)
	TplaTh		= .2	! top plate thickness (r)
Endfill
*
      Use  FTPG  version=1
      Use  FFCC  version=1
      Use  FRBD  version=1
      Use  FSSD  version=1
*
	position=ftpg_Zstart+ftpg_totLen/2

      create FTPC 
      if (agexist('SVTT') != 0) then
         position FTPC in SVTT z=position konly='MANY'
	 position FTPC in SVTT z=-position ThetaZ=180 konly='MANY'
      else
         position FTPC in CAVE z=position konly='MANY'
	 position FTPC in CAVE z=-position ThetaZ=180 konly='MANY' 
      endif
*  
* ----------------------------------------------------------------------------
Block FTPC is the Forward TPC mother (needed for standalong test only)
      Material  Air
      Medium    Standard
      Attribute FTPC   seen=1   colo=1
      shape     TUBE  Rmin=ftpg_Rinnermost, 
                           Rmax=ftpg_Routermost, 
                           Dz=ftpg_totLen/2 
*
      Create and position FIAL 
      Create and position FMPT 
      Create and position FOAL
      Create and position FDUM
      Create and position FIFR z=(ftpg_totLen/2)-(ftpg_DzIFR/2)-ftpg_DzKaptonW
               position FIFR z=-((ftpg_totLen/2)-(ftpg_DzIFR/2)-ftpg_DzKaptonW)
      Create and position FKWI z=(ftpg_totLen/2)-ftpg_DzKaptonW/2
               position FKWI z=-((ftpg_totLen/2)-ftpg_DzKaptonW/2)
*
* Start here with the Readout Chambers
      temp3=(ftpg_totLen/2)-fssd_EringTh-fssd_DringWi-frbd_ModLeng/2
      Create FROC
      Do n=1,5
         Do k=1,6
            position FROC  AlphaZ=k*60+30*mod(n-1,2) z=temp3-21.3*(n-1)
         EndDo
      EndDo
*
* Start here with the Support Structure
      Create and position FSER z=(ftpg_totLen/2)-fssd_EringTh/2
                 position FSER z=-(ftpg_totLen/2)+fssd_EringTh/2
      Create and position FSDR z=(ftpg_totLen/2)-fssd_EringTh-fssd_DringWi/2
                 position FSDR z=-(ftpg_totLen/2)+fssd_EringTh+fssd_DringWi/2
*
      Do k=1,4
         temp3=(ftpg_totLen/2)-fssd_EringTh-k*21.3
         Create and position FSIL z=temp3
         Create and position FSFL z=temp3+fssd_IringTh/2+fssd_FringWi/2
                    position FSFL z=temp3-fssd_IringTh/2-fssd_FringWi/2
      EndDo
      Create and position FSSB AlphaZ=19.2 _
                          x=(fssd_EringRmax-fssd_SbarTh/2)*cos(pi*19.2/180.) _
                          y=(fssd_EringRmax-fssd_SbarTh/2)*sin(pi*19.2/180.)
      position FSSB AlphaZ=19.2 _
                          x=-(fssd_EringRmax-fssd_SbarTh/2)*cos(pi*19.2/180.) _
                          y=-(fssd_EringRmax-fssd_SbarTh/2)*sin(pi*19.2/180.)
      position FSSB AlphaZ=-19.2 _
                          x=(fssd_EringRmax-fssd_SbarTh/2)*cos(pi*19.2/180.) _
                          y=-(fssd_EringRmax-fssd_SbarTh/2)*sin(pi*19.2/180.)
      position FSSB AlphaZ=-19.2 _
                          x=-(fssd_EringRmax-fssd_SbarTh/2)*cos(pi*19.2/180.) _
                          y=(fssd_EringRmax-fssd_SbarTh/2)*sin(pi*19.2/180.)
*
      Do k=1,5
      Create and position FSSP z=(ftpg_totLen/2)-fssd_EringTh-(k-.5)*21.3  _
                               x=fssd_EringRmax-fssd_SbarTh/2 y=0
      Create and position FSSP z=(ftpg_totLen/2)-fssd_EringTh-(k-.5)*21.3  _
                               x=-fssd_EringRmax+fssd_SbarTh/2 y=0
      EndDo
*
      Create and position FSTB x=-fssd_TbarWi/2-fssd_TplaWi/2 _
                               y=fssd_EringRmax-fssd_TbarHe/2
                 position FSTB x=+fssd_TbarWi/2+fssd_TplaWi/2 _
                               y=fssd_EringRmax-fssd_TbarHe/2
                 position FSTB x=-fssd_TbarWi/2-fssd_TplaWi/2 _
                               y=-fssd_EringRmax+fssd_TbarHe/2
                 position FSTB x=+fssd_TbarWi/2+fssd_TplaWi/2 _
                               y=-fssd_EringRmax+fssd_TbarHe/2
*
      Create and position FSTP x=0 y=fssd_EringRmax-fssd_TplaTh/2
                 position FSTP x=0 y=-fssd_EringRmax+fssd_TplaTh/2
Endblock
* ----------------------------------------------------------------------------
Block FIAL is the inner AL-tube of the FTPC 
	Material Aluminium
	Attribute FIAL seen=1 colo=2
	shape	TUBE Rmin=ftpg_Rinnermost,
                     Rmax=ftpg_Rinnermost+ftpg_DrInAlLay1,
		     Dz=(ftpg_totLen/2)-ftpg_DzIFR-ftpg_DzKaptonW
Endblock
* ----------------------------------------------------------------------------
Block FMPT is the insulating plastic tube of the drift-electrode
	Material POLYETHYLENE 
	Attribute FMPT seen=1 colo=3
	shape   TUBE Rmin=ftpg_Rinnermost+ftpg_DrInAlLay1,
                     Rmax=ftpg_Rinnermost+ftpg_DrInAlLay1+ftpg_DrInIsoLay,
		     Dz=(ftpg_totLen/2)-ftpg_DzIFR-ftpg_DzKaptonW
Endblock
* ----------------------------------------------------------------------------
Block FOAL is the Al drift-electrode
	Material Aluminium 
	Attribute FMPT seen=1 colo=2
	temp1=ftpg_Rinnermost+ftpg_DrInAlLay1+ _
              ftpg_DrInIsoLay+ftpg_DrInAlLay2
	shape   TUBE Rmin=ftpg_Rinnermost+ftpg_DrInAlLay1+ftpg_DrInIsoLay,
                     Rmax=temp1,
		     Dz=(ftpg_totLen/2)-ftpg_DzIFR-ftpg_DzKaptonW
Endblock
* ----------------------------------------------------------------------------
Block FDUM is a dummy volume to provide the correct hitplane-numbering
      Material  Argon_gas
      attribute FINR   seen=0  colo=7
      temp1=ftpg_Rinnermost+ftpg_DrInAlLay1+ _
              ftpg_DrInIsoLay+ftpg_DrInAlLay2
      Shape     TUBE   Rmin=temp1,
                       Rmax=ftpg_RGasOut,
                       Dz=ftpg_totLen/2-ftpg_DzKaptonW
      Create and position FGAS
Endblock
* ----------------------------------------------------------------------------
Block FGAS is the FTPC gas volume
      Material  Argon_gas
      attribute FINR   seen=1  colo=7
      temp1=ftpg_Rinnermost+ftpg_DrInAlLay1+ _
              ftpg_DrInIsoLay+ftpg_DrInAlLay2
      Shape     TUBE   Rmin=temp1,
                       Rmax=ftpg_RGasOut,
                       Dz=ftpg_totLen/2-ftpg_DzKaptonW
	create FSEN
*
* temp1 is length of the gas-volume 
	temp1=ftpg_totLen-2*(ftpg_DzKaptonW)
*
	Do k=1,nint(ftpg_Hitlay)
           position FSEN z = temp1*k/(ftpg_Hitlay+1.)-temp1/2.
	EndDo 
*
* FIFR must be called a second time; else 'overrruled' by FGAS
* PN:  why the first one was needed ?
      Create and position FIFR z=(ftpg_totLen/2)-(ftpg_DzIFR/2)-ftpg_DzKaptonW
               position FIFR z=-((ftpg_totLen/2)-(ftpg_DzIFR/2)-ftpg_DzKaptonW)
*
* Start here to built the Fieldcage in the Gas-Volume
*
	create FFSL
	position FFSL ort=yzx AlphaZ=90  z=(ftpg_totLen/2)-5 y=ffcc_StiRpos 
	position FFSL ort=yzx AlphaZ=210 z=(ftpg_totLen/2)-5 _
                         x=-ffcc_StiRpos*cos(pi/6.) y=-ffcc_StiRpos*sin(pi/6.)
	position FFSL ort=yzx AlphaZ=-30 z=(ftpg_totLen/2)-5 _
                         x=ffcc_StiRpos*cos(pi/6.)  y=-ffcc_StiRpos*sin(pi/6.)
*
	position FFSL ort=yzx AlphaZ=90  z=-ftpg_totLen/2+5 y=ffcc_StiRpos 
	position FFSL ort=yzx AlphaZ=210 z=-ftpg_totLen/2+5 _
                         x=-ffcc_StiRpos*cos(pi/6.) y=-ffcc_StiRpos*sin(pi/6.)
	position FFSL ort=yzx AlphaZ=-30 z=-ftpg_totLen/2+5 _
                         x=ffcc_StiRpos*cos(pi/6.)  y=-ffcc_StiRpos*sin(pi/6.)
*
	create FFCE
	temp1 = ftpg_Rinnermost+ftpg_DrInAlLay1+ftpg_DrInIsoLay+ _
                ftpg_DrInAlLay2
	temp2 = ((ftpg_RGasOut-temp1)/2)+temp1
	position FFCE in FGAS _
                 z=(ftpg_totLen/2+ffcc_StiDia/2+ffcc_BarWidt/2)-5 y=temp2
	position FFCE in FGAS AlphaZ=-60 _
                 z=(ftpg_totLen/2+ffcc_StiDia/2+ffcc_BarWidt/2)-5 _
                 x=-temp2*cos(pi/6.) y=-temp2*sin(pi/6.)
	position FFCE in FGAS AlphaZ=60 _
                 z=(ftpg_totLen/2+ffcc_StiDia/2+ffcc_BarWidt/2)-5 _
                 x=temp2*cos(pi/6.) y=-temp2*sin(pi/6.)
*
* Start here to position the FC Rings
*
	create FFRA
	position FFRA in FGAS z=(ftpg_totLen/2)-5-ffcc_StiDia/2
	position FFRA in FGAS z=-ftpg_totLen/2+5+ffcc_StiDia/2
*
	create FFRB
	position FFRB in FGAS z=(ftpg_totLen/2)-5-ffcc_StiDia/2
	position FFRB in FGAS z=-ftpg_totLen/2+5+ffcc_StiDia/2
*
	create FFRC
	position FFRC in FGAS z=(ftpg_totLen/2)-5-ffcc_StiDia/2
	position FFRC in FGAS z=-ftpg_totLen/2+5+ffcc_StiDia/2
*
	create FFRD
	position FFRD in FGAS z=(ftpg_totLen/2)-5-ffcc_StiDia/2
	position FFRD in FGAS z=-ftpg_totLen/2+5+ffcc_StiDia/2
*
	create FFRE
	position FFRE in FGAS z=(ftpg_totLen/2)-5-ffcc_StiDia/2
	position FFRE in FGAS z=-ftpg_totLen/2+5+ffcc_StiDia/2
*
	create FFRF
	position FFRF in FGAS z=(ftpg_totLen/2)-5-ffcc_StiDia/2
	position FFRF in FGAS z=-ftpg_totLen/2+5+ffcc_StiDia/2
*
	create FFRG
	position FFRG in FGAS z=(ftpg_totLen/2)-5-ffcc_StiDia/2
	position FFRG in FGAS z=-ftpg_totLen/2+5+ffcc_StiDia/2
*
	create FFRH
	position FFRH in FGAS z=(ftpg_totLen/2)-5-ffcc_StiDia/2
	position FFRH in FGAS z=-ftpg_totLen/2+5+ffcc_StiDia/2
*
	create FFRI
	position FFRI in FGAS z=(ftpg_totLen/2)-5-ffcc_StiDia/2
	position FFRI in FGAS z=-ftpg_totLen/2+5+ffcc_StiDia/2
*
	create FFRJ
	position FFRJ in FGAS z=(ftpg_totLen/2)-5-ffcc_StiDia/2
	position FFRJ in FGAS z=-ftpg_totLen/2+5+ffcc_StiDia/2
*
	create FFRK
	position FFRK in FGAS z=(ftpg_totLen/2)-5-ffcc_StiDia/2
	position FFRK in FGAS z=-ftpg_totLen/2+5+ffcc_StiDia/2
*
	create FFRL
	position FFRL in FGAS z=(ftpg_totLen/2)-5-ffcc_StiDia/2
	position FFRL in FGAS z=-ftpg_totLen/2+5+ffcc_StiDia/2
*
	create FFRM
	position FFRM in FGAS z=(ftpg_totLen/2)-5-ffcc_StiDia/2
	position FFRM in FGAS z=-ftpg_totLen/2+5+ffcc_StiDia/2
*
	create FFRN
	position FFRN in FGAS z=(ftpg_totLen/2)-5-ffcc_StiDia/2
	position FFRN in FGAS z=-ftpg_totLen/2+5+ffcc_StiDia/2
*
	create FFRO
	position FFRO in FGAS z=(ftpg_totLen/2)-5-ffcc_StiDia/2
	position FFRO in FGAS z=-ftpg_totLen/2+5+ffcc_StiDia/2
*
	create FFRP
	position FFRP in FGAS z=(ftpg_totLen/2)-5-ffcc_StiDia/2
	position FFRP in FGAS z=-ftpg_totLen/2+5+ffcc_StiDia/2
*
	create FFRQ
	position FFRQ in FGAS z=(ftpg_totLen/2)-5-ffcc_StiDia/2
	position FFRQ in FGAS z=-ftpg_totLen/2+5+ffcc_StiDia/2
*
 endblock
* ----------------------------------------------------------------------------
Block FSEN is the sensetive gas volume
      Material  Argon_gas
      Medium    sensitive  ISVOL=1
      attribute FSEN   seen=1  colo=4
*      attribute FSEN   seen=0  colo=0
      temp1=ftpg_Rinnermost+ftpg_DrInAlLay1+ _
            ftpg_DrInIsoLay+ftpg_DrInAlLay2
      Shape     TUBE   Rmin=temp1,
                       Rmax=ftpg_RGasOut,
		       Dz=ftpg_LayLen/2
      Create    FSEC 
endblock
* ----------------------------------------------------------------------------
Block FSEC is a sensetive gas sector
      SHAPE     TUBE
      HITS      FSEN   xx:16:SHX(-50,50)   yy:16:(-50,50)     zz:32:(-370,370),
                       px:16:(-100,100)    py:16:(-100,100)   pz:16:(-100,100),
                       Slen:16:(0,1.e4)    Tof:16:(0,1.e-6)   Step:16:(0,100),
                       SHTN:16:            Elos:32:(0,1)
endblock
* ----------------------------------------------------------------------------
Block FIFR is the Al inner flange ring
	Material Aluminium 
	Attribute FMPT seen=1 colo=2
	shape   TUBE Rmin=ftpg_Rinnermost,
                     Rmax=ftpg_Rinnermost+ftpg_DrIFR,
		     Dz=ftpg_DzIFR/2
Endblock
* ----------------------------------------------------------------------------
Block FKWI is the double Kapton window
	Material  MYLAR
	Attribute FMPT seen=1 colo=3
	shape   TUBE Rmin=ftpg_Rinnermost,
                     Rmax=ftpg_RGasOut,
		     Dz=ftpg_DzKaptonW/2
Endblock
* ----------------------------------------------------------------------------
* ----------------------------------------------------------------------------
Block FFSL is ceramic holder for fieldcage rings
	Material  PYREX A=20.719  Z=10.307  Dens=2.23  RadL=12.6  AbsL=50.7
	Attribute FFSL seen=1 colo=6
	shape   TUBE Rmin=0,
                     Rmax=ffcc_StiDia/2,
		     Dz=ffcc_StiLeng/2
Endblock
* ----------------------------------------------------------------------------
Block FFCE is the Fildcage Enhanced Support Structure
	Material Aluminium 
	Attribute FFCE seen=1 colo=2
	temp1 = ftpg_Rinnermost+ftpg_DrInAlLay1+ftpg_DrInIsoLay+ _
                ftpg_DrInAlLay2
	shape   BOX Dx=ffcc_BarThik/2 Dy=(ftpg_RGasOut-temp1)/2 _
                    Dz=ffcc_BarWidt/2
Endblock
* ----------------------------------------------------------------------------
* ----------------------------------------------------------------------------
Block FROC is one Module of the Readout Chamber
	Material  Air
	Attribute FROC seen=0 colo=0
	temp1=ftpg_RGasOut+frbd_GlassDr+frbd_AlDr+frbd_PcbDr+ _
              frbd_ElektrDr+frbd_CoolDr+1.2*frbd_SidebarDr
	shape TUBS Rmin=ftpg_RGasOut,
                   Rmax=temp1,
                   Dz=frbd_ModLeng/2 Phi1=frbd_Phi1 Phi2=frbd_Phi2
	Create and position FROM in FROC
	Create and position FROA in FROC
	Create and position FROG in FROC
	Create and position FROE in FROC
	Create and position FRCC in FROC
	Create and position FRPB in FROC
	temp1=ftpg_RGasOut+frbd_GlassDr+frbd_AlDr+frbd_PcbDr+ _
              frbd_ElektrDr+frbd_CoolDr+frbd_SidebarDr
	Create and position FRSB in FROC x=temp1-frbd_SidebarDr/2 _
                   y=frbd_SidebarWi/2 
Endblock
*
* ----------------------------------------------------------------------------
Block FROM is one Module of the Readout Chamber
	Material  PYREX A=20.719  Z=10.307  Dens=2.23  RadL=12.6  AbsL=50.7
	Attribute FROM seen=1 colo=6
	shape TUBS Rmin=ftpg_RGasOut Rmax=ftpg_RGasOut+frbd_GlassDr,
                   Dz=frbd_ModLeng/2 Phi1=frbd_Phi1 Phi2=frbd_Phi2
Endblock
* ----------------------------------------------------------------------------
Block FROA is the Al-Layer of the Readout Chamber
	Material Aluminium
	Attribute FROA seen=1 colo=2
	shape TUBS Rmin=ftpg_RGasOut+frbd_GlassDr, 
                   Rmax=ftpg_RGasOut+frbd_GlassDr+frbd_AlDr,
                   Dz=frbd_ModLeng/2 Phi1=frbd_Phi1 Phi2=frbd_Phi2
Endblock
* ----------------------------------------------------------------------------
Block FROG is the G10 Layer of the Readout Chamber (PC-Board)
*     G10 is about 60% SiO2 and 40% epoxy
         Component Si  A=28.08  Z=14   W=0.6*1*28./60.
         Component O   A=16     Z=8    W=0.6*2*16./60.
         Component C   A=12     Z=6    W=0.4*8*12./174.
         Component H   A=1      Z=1    W=0.4*14*1./174.
         Component O   A=16     Z=8    W=0.4*4*16./174.
      Mixture   G10    Dens=1.7
 	Material G10 
	Attribute FROG seen=1 colo=3
	shape TUBS Rmin=ftpg_RGasOut+frbd_GlassDr+frbd_AlDr, 
                   Rmax=ftpg_RGasOut+frbd_GlassDr+frbd_AlDr+frbd_PcbDr,
                   Dz=frbd_ModLeng/2 Phi1=frbd_Phi1 Phi2=frbd_Phi2
Endblock
* ----------------------------------------------------------------------------
Block FROE is the Electronics Layer of the Readout Chamber
	Material G10 
	Attribute FROE seen=1 colo=3
	temp2=ftpg_RGasOut+frbd_GlassDr+frbd_AlDr+frbd_PcbDr+frbd_ElektrDr
	shape TUBS Rmin=ftpg_RGasOut+frbd_GlassDr+frbd_AlDr+frbd_PcbDr, 
                   Rmax=temp2,
                   Dz=frbd_ModLeng/2 Phi1=frbd_Phi1 Phi2=frbd_Phi2
Endblock
* ----------------------------------------------------------------------------
Block FRCC is the Copper Cooling Layer of the Readout Chamber
	Material Copper 
	Attribute FRCC seen=1 colo=2
	temp1=ftpg_RGasOut+frbd_GlassDr+frbd_AlDr+frbd_PcbDr+ _
              frbd_ElektrDr 
	temp2=ftpg_RGasOut+frbd_GlassDr+frbd_AlDr+frbd_PcbDr+ _
              frbd_ElektrDr+frbd_CoolDr
	shape TUBS Rmin=temp1,
                   Rmax=temp2,
                   Dz=frbd_ModLeng/2 Phi1=frbd_Phi1 Phi2=frbd_Phi2
Endblock
* ----------------------------------------------------------------------------
Block FRPB is the Phi Bar of the Readout Chamber
	Material Aluminium
	Attribute FRPB seen=1 colo=2
	temp1=ftpg_RGasOut+frbd_GlassDr+frbd_AlDr+frbd_PcbDr+ _
              frbd_ElektrDr+frbd_CoolDr 
	temp2=ftpg_RGasOut+frbd_GlassDr+frbd_AlDr+frbd_PcbDr+ _
              frbd_ElektrDr+frbd_CoolDr+frbd_PhibarDr
	shape TUBS Rmin=temp1,
                   Rmax=temp2,
                   Dz=frbd_PhibarTh/2 Phi1=frbd_Phi1 Phi2=frbd_Phi2
Endblock
* ----------------------------------------------------------------------------
Block FRSB is the Side Bar of the Readout Chamber
	Material Aluminium
	Attribute FRSB seen=1 colo=2
	shape BOX Dx=frbd_SidebarDr/2 Dy=frbd_SidebarWi/2 Dz=frbd_ModLeng/2
Endblock
* ----------------------------------------------------------------------------
* ----------------------------------------------------------------------------
Block FSER is the Support End Ring
	Material Aluminium
	Attribute FSER seen=1 colo=2
	shape TUBE Rmin=ftpg_RGasOut Rmax=fssd_EringRmax Dz=fssd_EringTh/2
	Create and position FSEI in FSER
Endblock
* ----------------------------------------------------------------------------
Block FSEI is the inner Part of the Support And Ring
	Material Air
	Attribute FSER seen=1 colo=1
	shape TUBE Rmin=ftpg_RGasOut+fssd_EringWTh,
                   Rmax=fssd_EringRmax-fssd_EringWTh,
                   Dz=fssd_EringTh/2-fssd_EringWTh
Endblock
* ----------------------------------------------------------------------------
Block FSDR is the Distance Ring between S. End Rings and first Module
	Material Aluminium
	Attribute FSER seen=1 colo=2
	shape TUBE Rmin=ftpg_RGasOut, 
                   Rmax=ftpg_RGasOut+fssd_DringDr, 
                   Dz=fssd_DringWi/2
Endblock
* ----------------------------------------------------------------------------
Block FSIL is the Inner Support Ring 
	Material Aluminium
	Attribute FSIL seen=1 colo=2
	shape TUBE Rmin=ftpg_RGasOut,
                   Rmax=fssd_IringRmax,
                   Dz=fssd_IringTh/2
*	Create and position FSFL in FSIL z=fssd_IringTh/2+fssd_FringWi/2
*	position FSFL in FSIL z=-fssd_IringTh/2-fssd_FringWi/2
Endblock
* ----------------------------------------------------------------------------
Block FSFL is the Flange Ring
	Material Aluminium
	Attribute FSFL seen=1 colo=2
	shape TUBE Rmin=ftpg_RGasOut,
                   Rmax=ftpg_RGasOut+fssd_FringDr,
                   Dz=fssd_FringWi/2
Endblock
* ----------------------------------------------------------------------------
Block FSSB is the Side Bar
	Material Aluminium
	Attribute FSSB seen=1 colo=2
	shape BOX Dx=fssd_SbarTh/2 Dy=fssd_SbarWi/2 Dz=ftpg_totLen/2
Endblock
* ----------------------------------------------------------------------------
Block FSSP is the Side Plate
	Material Aluminium
	Attribute FSSP seen=1 colo=2
	shape BOX Dx=fssd_SplaTh/2 Dy=fssd_SplaWi/2 Dz=fssd_SplaLe/2
	Create and position FSPH in FSSP AlphaY=90
Endblock
* ----------------------------------------------------------------------------
Block FSPH is the Hole in the Side Plate
	Material Air
	Attribute FSPH seen=1 colo=1
	shape TUBE Rmin=0 Rmax=fssd_SplaDia/2 Dz=fssd_SplaTh/2
Endblock
* ----------------------------------------------------------------------------
Block FSTB is the Top Bar
	Material Aluminium
	Attribute FSTB seen=1 colo=2
	shape Box Dx=fssd_TbarWi/2 Dy=fssd_TbarHe/2 Dz=ftpg_totLen/2
Endblock
* ----------------------------------------------------------------------------
Block FSTP is the Top Plate
	Material Aluminium
	Attribute FSTP seen=1 colo=2
	shape Box Dx=fssd_TplaWi/2 Dy=fssd_TplaTh/2 Dz=ftpg_totLen/2
Endblock
* ----------------------------------------------------------------------------
*
*
*
* ----------------------------------------------------------------------------
* ----------------------------------------------------------------------------
Block FFRA is outermost FC Ring
	Material Aluminium 
	Attribute FFRA seen=1 colo=2
* 17 Rings -> 18 gaps
	temp1 = (ftpg_RGasOut-(ftpg_Rinnermost+ftpg_DrInAlLay1+ _
                ftpg_DrInIsoLay+ftpg_DrInAlLay2))/18
	temp2 = ftpg_Rinnermost+ftpg_DrInAlLay1+ftpg_DrInIsoLay+ _
                ftpg_DrInAlLay2
	shape   TUBE Rmin=temp2+17*temp1-ffcc_RiDr/2,
                     Rmax=temp2+17*temp1+ffcc_RiDr/2,
		     Dz=ffcc_RiThick/2
Endblock
* ----------------------------------------------------------------------------
Block FFRB 
	Material Aluminium 
	Attribute FFRB seen=1 colo=2
* 17 Rings -> 18 gaps
	temp1 = (ftpg_RGasOut-(ftpg_Rinnermost+ftpg_DrInAlLay1+ _
                ftpg_DrInIsoLay+ftpg_DrInAlLay2))/18
	temp2 = ftpg_Rinnermost+ftpg_DrInAlLay1+ftpg_DrInIsoLay+ _
                ftpg_DrInAlLay2
	shape   TUBE Rmin=temp2+16*temp1-ffcc_RiDr/2,
                     Rmax=temp2+16*temp1+ffcc_RiDr/2,
		     Dz=ffcc_RiThick/2
Endblock
* ----------------------------------------------------------------------------
Block FFRC 
	Material Aluminium 
	Attribute FFRC seen=1 colo=2
* 17 Rings -> 18 gaps
	temp1 = (ftpg_RGasOut-(ftpg_Rinnermost+ftpg_DrInAlLay1+ _
                ftpg_DrInIsoLay+ftpg_DrInAlLay2))/18
	temp2 = ftpg_Rinnermost+ftpg_DrInAlLay1+ftpg_DrInIsoLay+ _
                ftpg_DrInAlLay2
	shape   TUBE Rmin=temp2+15*temp1-ffcc_RiDr/2,
                     Rmax=temp2+15*temp1+ffcc_RiDr/2,
		     Dz=ffcc_RiThick/2
Endblock
* ----------------------------------------------------------------------------
Block FFRD 
	Material Aluminium 
	Attribute FFRD seen=1 colo=2
* 17 Rings -> 18 gaps
	temp1 = (ftpg_RGasOut-(ftpg_Rinnermost+ftpg_DrInAlLay1+ _
                ftpg_DrInIsoLay+ftpg_DrInAlLay2))/18
	temp2 = ftpg_Rinnermost+ftpg_DrInAlLay1+ftpg_DrInIsoLay+ _
                ftpg_DrInAlLay2
	shape   TUBE Rmin=temp2+14*temp1-ffcc_RiDr/2,
                     Rmax=temp2+14*temp1+ffcc_RiDr/2,
		     Dz=ffcc_RiThick/2
Endblock
* ----------------------------------------------------------------------------
Block FFRE 
	Material Aluminium 
	Attribute FFRE seen=1 colo=2
* 17 Rings -> 18 gaps
	temp1 = (ftpg_RGasOut-(ftpg_Rinnermost+ftpg_DrInAlLay1+ _
                ftpg_DrInIsoLay+ftpg_DrInAlLay2))/18
	temp2 = ftpg_Rinnermost+ftpg_DrInAlLay1+ftpg_DrInIsoLay+ _
                ftpg_DrInAlLay2
	shape   TUBE Rmin=temp2+13*temp1-ffcc_RiDr/2,
                     Rmax=temp2+13*temp1+ffcc_RiDr/2,
		     Dz=ffcc_RiThick/2
Endblock

* ----------------------------------------------------------------------------
Block FFRF 
	Material Aluminium 
	Attribute FFRF seen=1 colo=2
* 17 Rings -> 18 gaps
	temp1 = (ftpg_RGasOut-(ftpg_Rinnermost+ftpg_DrInAlLay1+ _
                ftpg_DrInIsoLay+ftpg_DrInAlLay2))/18
	temp2 = ftpg_Rinnermost+ftpg_DrInAlLay1+ftpg_DrInIsoLay+ _
                ftpg_DrInAlLay2
	shape   TUBE Rmin=temp2+12*temp1-ffcc_RiDr/2,
                     Rmax=temp2+12*temp1+ffcc_RiDr/2,
		     Dz=ffcc_RiThick/2
Endblock
* ----------------------------------------------------------------------------
Block FFRG 
	Material Aluminium 
	Attribute FFRG seen=1 colo=2
* 17 Rings -> 18 gaps
	temp1 = (ftpg_RGasOut-(ftpg_Rinnermost+ftpg_DrInAlLay1+ _
                ftpg_DrInIsoLay+ftpg_DrInAlLay2))/18
	temp2 = ftpg_Rinnermost+ftpg_DrInAlLay1+ftpg_DrInIsoLay+ _
                ftpg_DrInAlLay2
	shape   TUBE Rmin=temp2+11*temp1-ffcc_RiDr/2,
                     Rmax=temp2+11*temp1+ffcc_RiDr/2,
		     Dz=ffcc_RiThick/2
Endblock
* ----------------------------------------------------------------------------
Block FFRH 
	Material Aluminium 
	Attribute FFRH seen=1 colo=2
* 17 Rings -> 18 gaps
	temp1 = (ftpg_RGasOut-(ftpg_Rinnermost+ftpg_DrInAlLay1+ _
                ftpg_DrInIsoLay+ftpg_DrInAlLay2))/18
	temp2 = ftpg_Rinnermost+ftpg_DrInAlLay1+ftpg_DrInIsoLay+ _
                ftpg_DrInAlLay2
	shape   TUBE Rmin=temp2+10*temp1-ffcc_RiDr/2,
                     Rmax=temp2+10*temp1+ffcc_RiDr/2,
		     Dz=ffcc_RiThick/2
Endblock
* ----------------------------------------------------------------------------
Block FFRI 
	Material Aluminium 
	Attribute FFRI seen=1 colo=2
* 17 Rings -> 18 gaps
	temp1 = (ftpg_RGasOut-(ftpg_Rinnermost+ftpg_DrInAlLay1+ _
                ftpg_DrInIsoLay+ftpg_DrInAlLay2))/18
	temp2 = ftpg_Rinnermost+ftpg_DrInAlLay1+ftpg_DrInIsoLay+ _
                ftpg_DrInAlLay2
	shape   TUBE Rmin=temp2+9*temp1-ffcc_RiDr/2,
                     Rmax=temp2+9*temp1+ffcc_RiDr/2,
		     Dz=ffcc_RiThick/2
Endblock
* ----------------------------------------------------------------------------
Block FFRJ 
	Material Aluminium 
	Attribute FFRJ seen=1 colo=2
* 17 Rings -> 18 gaps
	temp1 = (ftpg_RGasOut-(ftpg_Rinnermost+ftpg_DrInAlLay1+ _
                ftpg_DrInIsoLay+ftpg_DrInAlLay2))/18
	temp2 = ftpg_Rinnermost+ftpg_DrInAlLay1+ftpg_DrInIsoLay+ _
                ftpg_DrInAlLay2
	shape   TUBE Rmin=temp2+8*temp1-ffcc_RiDr/2,
                     Rmax=temp2+8*temp1+ffcc_RiDr/2,
		     Dz=ffcc_RiThick/2
Endblock
* ----------------------------------------------------------------------------
Block FFRK 
	Material Aluminium 
	Attribute FFRK seen=1 colo=2
* 17 Rings -> 18 gaps
	temp1 = (ftpg_RGasOut-(ftpg_Rinnermost+ftpg_DrInAlLay1+ _
                ftpg_DrInIsoLay+ftpg_DrInAlLay2))/18
	temp2 = ftpg_Rinnermost+ftpg_DrInAlLay1+ftpg_DrInIsoLay+ _
                ftpg_DrInAlLay2
	shape   TUBE Rmin=temp2+7*temp1-ffcc_RiDr/2,
                     Rmax=temp2+7*temp1+ffcc_RiDr/2,
		     Dz=ffcc_RiThick/2
Endblock
* ----------------------------------------------------------------------------
Block FFRL 
	Material Aluminium 
	Attribute FFRL seen=1 colo=2
* 17 Rings -> 18 gaps
	temp1 = (ftpg_RGasOut-(ftpg_Rinnermost+ftpg_DrInAlLay1+ _
                ftpg_DrInIsoLay+ftpg_DrInAlLay2))/18
	temp2 = ftpg_Rinnermost+ftpg_DrInAlLay1+ftpg_DrInIsoLay+ _
                ftpg_DrInAlLay2
	shape   TUBE Rmin=temp2+6*temp1-ffcc_RiDr/2,
                     Rmax=temp2+6*temp1+ffcc_RiDr/2,
		     Dz=ffcc_RiThick/2
Endblock
* ----------------------------------------------------------------------------
Block FFRM 
	Material Aluminium 
	Attribute FFRM seen=1 colo=2
* 17 Rings -> 18 gaps
	temp1 = (ftpg_RGasOut-(ftpg_Rinnermost+ftpg_DrInAlLay1+ _
                ftpg_DrInIsoLay+ftpg_DrInAlLay2))/18
	temp2 = ftpg_Rinnermost+ftpg_DrInAlLay1+ftpg_DrInIsoLay+ _
                ftpg_DrInAlLay2
	shape   TUBE Rmin=temp2+5*temp1-ffcc_RiDr/2,
                     Rmax=temp2+5*temp1+ffcc_RiDr/2,
		     Dz=ffcc_RiThick/2
Endblock
* ----------------------------------------------------------------------------
Block FFRN 
	Material Aluminium 
	Attribute FFRN seen=1 colo=2
* 17 Rings -> 18 gaps
	temp1 = (ftpg_RGasOut-(ftpg_Rinnermost+ftpg_DrInAlLay1+ _
                ftpg_DrInIsoLay+ftpg_DrInAlLay2))/18
	temp2 = ftpg_Rinnermost+ftpg_DrInAlLay1+ftpg_DrInIsoLay+ _
                ftpg_DrInAlLay2
	shape   TUBE Rmin=temp2+4*temp1-ffcc_RiDr/2,
                     Rmax=temp2+4*temp1+ffcc_RiDr/2,
		     Dz=ffcc_RiThick/2
Endblock
* ----------------------------------------------------------------------------
Block FFRO 
	Material Aluminium 
	Attribute FFRO seen=1 colo=2
* 17 Rings -> 18 gaps
	temp1 = (ftpg_RGasOut-(ftpg_Rinnermost+ftpg_DrInAlLay1+ _
                ftpg_DrInIsoLay+ftpg_DrInAlLay2))/18
	temp2 = ftpg_Rinnermost+ftpg_DrInAlLay1+ftpg_DrInIsoLay+ _
                ftpg_DrInAlLay2
	shape   TUBE Rmin=temp2+3*temp1-ffcc_RiDr/2,
                     Rmax=temp2+3*temp1+ffcc_RiDr/2,
		     Dz=ffcc_RiThick/2
Endblock
* ----------------------------------------------------------------------------
Block FFRP 
	Material Aluminium 
	Attribute FFRP seen=1 colo=2
* 17 Rings -> 18 gaps
	temp1 = (ftpg_RGasOut-(ftpg_Rinnermost+ftpg_DrInAlLay1+ _
                ftpg_DrInIsoLay+ftpg_DrInAlLay2))/18
	temp2 = ftpg_Rinnermost+ftpg_DrInAlLay1+ftpg_DrInIsoLay+ _
                ftpg_DrInAlLay2
	shape   TUBE Rmin=temp2+2*temp1-ffcc_RiDr/2,
                     Rmax=temp2+2*temp1+ffcc_RiDr/2,
		     Dz=ffcc_RiThick/2
Endblock
* ----------------------------------------------------------------------------
Block FFRQ 
	Material Aluminium 
	Attribute FFRQ seen=1 colo=2
* 17 Rings -> 18 gaps
	temp1 = (ftpg_RGasOut-(ftpg_Rinnermost+ftpg_DrInAlLay1+ _
                ftpg_DrInIsoLay+ftpg_DrInAlLay2))/18
	temp2 = ftpg_Rinnermost+ftpg_DrInAlLay1+ftpg_DrInIsoLay+ _
                ftpg_DrInAlLay2
	shape   TUBE Rmin=temp2+1*temp1-ffcc_RiDr/2,
                     Rmax=temp2+1*temp1+ffcc_RiDr/2,
		     Dz=ffcc_RiThick/2
Endblock
* ----------------------------------------------------------------------------
* ----------------------------------------------------------------------------
    END






