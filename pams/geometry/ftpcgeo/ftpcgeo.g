******************************************************************************
Module   FTPCGEO  is the geometry of the Forward TPC in STAR
* Original version:
* Author   Michael Konrad
* Created  18-Okt-96
  Author   Andreas Schuettauf
  Created  03-Apr-98
* modification:
* PN. 06/08/98: truncate variable in structures to 8 letters maximum
*               otherwise they can not be converted into tables
*  Modified 10-July-98
*  Author   Holm Hummler, Janet Seyboth
*           Position FSEN in correct order
*  Modified 15-July-98
*  Author   Janet Seyboth
*           removed unused definitions
*           declare FROM  konly='MANY'
*           declare FGAS  konly='ONLY'
* PN. 27-07-98 : FROM position corrected, content declared MANY
******************************************************************************
+CDE,AGECOM,GCONST,GCUNIT.
*
	Content FTPC, FIAL, FMPT, FOAL, FGAS, FSEN, FSEC,
		FIFR, FKWI, FFSL, FFCE, FROS, FROM, FROB, 
		FROE, FROL, FROP, FROT, FREL, FRCC, FRCE,
		FSER, FSRA, FSRB, FSPG, FSPI, FSSM, FSRI, FSBA,
		FPAD, FFRA
*
	structure FTPG { Version, RinnerMs, RouterMs,
                         RGasOut, RRoM, RElCard,  RCooPlm, RCooPle, 
                         Zstart, totLen, LayLen, Hitlay, DrInAlL1,
                         DrInAlL2, DrInIsoL, DzKapton, DrIFR, 
                         DzIFR, DzEr, DzRoM, DzSuRa, DzSuRb, DzSmPR, 
                         DzBiPR, MSRDZ, SERHole, RISRing, ISRingDZ,
                         SBSRDx,  SBSRDy, SBSRDz, GasVolDz}
*
	structure FFCC {Version, StiLeng, StiDia, StiRpos, RiThick,
                        RiDr, RiGap, BarLeng, BarWidt, BarThik}
*
	structure FRBD {Version, Phi1, Phi2, Phi3, Phi4, Phi5, Phi6,
                        Phi7, Phi8, Phi9, Phi10, Phi11, Phi12, Phi13,
                        XRoM, YRoM, ZRoM, RaHol, XEHol, YEHol, XLHol, 
                        YLHol, BOffset, ZOffB, ModLeng, ElectrDX, 
                        ElectrDY,ElectrDZ, CoolPlDX, CoolPlDY, CoolPlDZ, 
                        EClPlDX, EClPlDY, EClPlDZ, CakeHIR, CakeHOR, 
                        CakeHWZ, BoxHX, BoxHY, BoxHZ, EBoxHX, EBoxHY, 
                        EBoxHZ, LBoxHX, LBoxHY, LBoxHZ}
*
	structure FSSD {Version, EringRmn, EringRmx, EringDZ, 
                        OEringDZ, ERPosZ, MEringRm, MEringDZ, 
                        ERPolyRm, TrapR, PolyR, PolyDZ, PolyIR, 
                        PolyOR, TrapX1, TrapX2, TrapDY, TrapDZ,
                        PGonPDZ, SBSDy}
*
	Integer  k,n,jj,ww,gg,hh,Iring
        Integer  krueck
        Integer  iflaga(5),iflagb(5)
        Data     iflaga /0,1,1,2,2/
        Data     iflagb /0,0,1,1,2/
	Real     position, temp1, temp2, temp3
        Real     z1,z2
	Real     frob_x1
	Integer  Agexist
*
* ----------------------------------------------------------------------------

   Fill FTPG	    ! basic FTPC data
	Version		= 1	! geometry Version
	RinnerMs	= 7.55  ! innermost radius of envelope
	RouterMs	= 36.4	! outermost radius of envelope
	RGasOut		= 30.6  ! outer radius of the gas-volume
        RRoM            = 29.42 ! outer radius for one readout module in a ring
        RElCard         = 32.4  ! outer radius for the electronic card  
        RCooPlm         = 33.7  ! outer radius for the cooling plate middle
        RCooPle         = 36.0  ! outer radius for the cooling plate ends
	Zstart		= 150	! distance from the interaction point
	totLen		= 119	! overall length
        LayLen          = 2.0   ! thickness of the sensitive Layer
        Hitlay          = 10    ! # of padrows in one FTPC : 10
	DrInAlL1	= 0.05  ! thickness of inner Al-Layer of inner Tube
	DrInAlL2	= 0.05  ! thickness of outer Al-Layer of inner Tube
	DrInIsoL	= 0.4	! thickness of plastic insulation of inner tube
	DzKapton	= 0.02	! thickness of a double kapton-windows
	DrIFR		= 1.15	! thickness (r) of inner flange ring
	DzIFR		= 0.4	! thickness (z) of inner flange ring
        DzER            = 10.35 ! thickness (z) of Endring
        DzRoM           = 21.3  ! Distance of one Readout module Ring to coor. 
        DzSuRa          = 10.65 ! Distance to inner(a) Support Ring from coor. 
        DzSuRb          = 31.95 ! Distance to outer(b) Support Ring from coor.
        DzSmPR          = 8.5   ! Distance Small between to Pad Rows
        DzBiPR          = 12.80 ! Distance Big between to Pad Rows  
        MSRDZ           = 4.1   ! half length of main support ring
        SERHole         = 0.5   ! half Support End Ring Hole length Z
        RISRing         = 30.8  ! outer Radius for innner Support Ring
        ISRingDZ        = 3.1   ! Half length for inner Support Ring
        SBSRDx          = 0.7   ! Half width of Stabil. Block for Supp.Ring 
        SBSRDy          = 2.8   ! Half thick. of Stabil. Block for Supp.Ring 
        SBSRDz          = 3.1   ! Half length of Stabil. Block for Supp.Ring
        GasVolDz        = 59.5   ! Half length of active volume
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
	Phi1		= 0	! phi 1
	Phi2		= 30	! phi 2
        Phi3            = 60    ! phi 3
        Phi4            = 90    ! phi 4  
        Phi5            = 120   ! phi 5
        Phi6            = 150   ! phi 6
        Phi7            = 180   ! phi 7
        Phi8            = 210   ! phi 8
        Phi9            = 240   ! phi 9
        Phi10           = 270   ! phi 10
        Phi11           = 300   ! phi 11
        Phi12           = 330   ! phi 12
        Phi13           = 360   ! phi 13 
        XRoM            = 32.6  ! X lenght of one Readout Module 
        YRoM            = 5.77  ! Y lenght of one Readout Module 
        ZRoM            = 15.1  ! Z lenght of one Readout Module 
        RaHol           = 29.508! Radius for the circle cut in RoM
        XEHol           = 15.8  ! X length end hole in RoM  
        YEHol           = 0.275 ! Y length end hole in RoM  
        XLHol           = 7.05  ! X length long hole in RoM  
        YLHol           = 0.275 ! Y length long hole in RoM  
        BOffset         = 11.92 ! Box Offset
        ZOffB           = 3.15  ! Z Offset for Box hole
	ModLeng		= 16.6	! length (z) of the module
	ElectrDX	= 12.7	! electronics width
	ElectrDY	= 0.5	! electronics thickness
	ElectrDZ	= 8.5	! electronics length
	CoolPlDX	= 12.7	! cooling plate width
        CoolPlDY	= 0.025	! cooling plate thickness
        CoolPlDZ	= 6.6	! cooling plate length
        EClPlDX	        = 1.8	! cooling end plates width
        EClPlDY	        = 0.025	! cooling end plates thickness
        EClPlDZ	        = 6.6	! cooling end plates length
        CakeHIR         = 25.5  ! Cake Hole Inner Radius
        CakeHOR         = 30.5  ! Cake Hole Outer Radius
        CakeHWZ         = 7.55  ! Half Cake Hole Width in Z    
        BoxHX           = 2.88  ! Half Box Hole Length in X
        BoxHY           = 2.845 ! Half Box Hole Length in Y 
        BoxHZ           = 2.95  ! Half Box Hole Length in Z
        EBoxHX          = 0.5   ! Half End Box Hole Length in X
        EBoxHY          = 2.626 ! Half End Box Hole Length in Y 
        EBoxHZ          = 7.55  ! Half End Box Hole Length in Z
        LBoxHX          = 15.3  ! Half Long Box Hole Length in X
        LBoxHY          = 2.628 ! Half Long Box Hole Length in Y 
        LBoxHZ          = 0.5   ! Half Long Box Hole Length in Z        
   Endfill
*
*
   Fill FSSD           ! Support Structure Design
	Version		= 1	! geometry Version
        EringRmn        = 30.6  ! endring inner radius
	EringRmx	= 36.4	! endring outer radius
	EringDZ		= 5.175	! half endring thickness (z) 
        OEringDZ        = 0.5	! outer endring thickness (z) 
        ERPosZ          = 4.675 ! Position of inner and outer Ring in End Ring
	MEringRm	= 31.1	! endring medium outer radius        
        MEringDZ	= 4.175	! medium endring thickness (z) 
        ERPolyRm        = 31.75 ! End ring Polygon max. radius
        TrapR           = 35.823! Trapezoid Radius 
        PolyR           = 33.791! Polygon Radius
        PolyDz          = 7.05  ! half Polygon length in Z
        PolyIR          = 0.0   ! Polygon Inner Radius  
        PolyOR          = 1.515 ! Polygon Outer Radius
        TrapX1          = 0.4585! half small lenght of trapezoid in X
        TrapX2          = 2.625 ! half large lenght of trapezoid in X
        TrapDY          = 7.05  ! half hight of trapezoid in Y
        TrapDZ          = 0.576 ! half width of trapezoid in Z 
        PGonPDZ         = 3.6   ! Position of Polygon in Supp. Struc FSSM (z)
        SBSDy           = 33.6  ! Position for Stabi. Blocks in (y) 
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
      shape     TUBE     Rmin=ftpg_RinnerMs, 
                         Rmax=ftpg_RouterMs, 
                         Dz=ftpg_totLen/2 

      Create and position FIAL 
      Create and position FMPT 
      Create and position FOAL
      Create and position FPAD 
      Create and position FGAS
      Create and position FIFR z=(ftpg_totLen/2)-(ftpg_DzIFR/2)-ftpg_DzKapton
               position FIFR z=-((ftpg_totLen/2)-(ftpg_DzIFR/2)-ftpg_DzKapton)
      Create and position FKWI z=(ftpg_totLen/2)-ftpg_DzKapton/2
               position FKWI z=-((ftpg_totLen/2)-ftpg_DzKapton/2)

*
*
*
* Start here with the Support Structure
*

*
* End Ring's 2*
*
      Create and position FSER z=(ftpg_totLen/2.)-(ftpg_DzEr/2.)
                 position FSER AlphaX=180 z=-((ftpg_totLen/2.)-(ftpg_DzEr/2.))

*
* Main support Ring's  4*
*

       Create and position FSSM z=ftpg_DzSuRb 
       Create and position FSSM z=ftpg_DzSuRa
       Create and position FSSM z=-(ftpg_DzSuRa)
       Create and position FSSM z=-(ftpg_DzSuRb)

 
               position FKWI z=-((ftpg_totLen/2)-ftpg_DzKapton/2)
*
* Start here with the Readout Chambers
*

      temp3=(ftpg_totLen/2)-(ftpg_DzEr)-((frbd_ZRoM-2.)/2.)

      Create FROS

      Do n=1,5
             position FROS z=temp3-((ftpg_DzRoM)*(n-1))             
      EndDo
*
*
*

Endblock

* ----------------------------------------------------------------------------

Block FIAL is the inner AL-tube of the FTPC 
	Material Aluminium
	Attribute FIAL seen=1 colo=2
	shape	TUBE Rmin=ftpg_RinnerMs,
                     Rmax=ftpg_RinnerMs+ftpg_DrInAlL1,
		     Dz=(ftpg_totLen/2)-ftpg_DzIFR-ftpg_DzKapton
Endblock
* ----------------------------------------------------------------------------
Block FMPT is the insulating plastic tube of the drift-electrode
	Material POLYETHYLENE 
	Attribute FMPT seen=1 colo=3
	shape   TUBE Rmin=ftpg_RinnerMs+ftpg_DrInAlL1,
                     Rmax=ftpg_RinnerMs+ftpg_DrInAlL1+ftpg_DrInIsoL,
		     Dz=(ftpg_totLen/2)-ftpg_DzIFR-ftpg_DzKapton
Endblock
* ----------------------------------------------------------------------------
Block FOAL is the Al drift-electrode
	Material Aluminium 
	Attribute FMPT seen=1 colo=2
	temp1=ftpg_RinnerMs+ftpg_DrInAlL1+ _
              ftpg_DrInIsoL+ftpg_DrInAlL2
	shape   TUBE Rmin=ftpg_RinnerMs+ftpg_DrInAlL1+ftpg_DrInIsoL,
                     Rmax=temp1,
		     Dz=(ftpg_totLen/2)-ftpg_DzIFR-ftpg_DzKapton
Endblock
* ----------------------------------------------------------------------------
Block FGAS is the FTPC gas volume
      Material  Argon_gas
      attribute FGAS   seen=1  colo=7
      temp1=ftpg_RinnerMs+ftpg_DrInAlL1+ _
              ftpg_DrInIsoL+ftpg_DrInAlL2
      Shape     TUBE   Rmin=temp1,
                       Rmax=ftpg_RGasOut,
                       Dz=ftpg_totLen/2-ftpg_DzKapton
*
* temp1 is length of the gas-volume 

	temp1=ftpg_totLen-2*(ftpg_DzKapton)
* 
    create FSEN
    Do k=1,nint(ftpg_Hitlay)/2
       krueck=nint(ftpg_Hitlay)/2+1-k
       position FSEN z = -((ftpg_DzSmPR/2.)+(ftpg_DzSmPR*iflagb(krueck))_
                         +(ftpg_DzBiPR*iflaga(krueck)))
    EndDO
    Do k=1,nint(ftpg_Hitlay)/2
       position FSEN z = (ftpg_DzSmPR/2.)+(ftpg_DzSmPR*iflagb(k))_
                         +(ftpg_DzBiPR*iflaga(k))
    EndDO

*
* Start here to built the Fieldcage in the Gas-Volume
*

	create FFSL
	position FFSL ort=yzx AlphaZ=frbd_Phi4 _
                        z=(ftpg_totLen/2)-5_
                        y=ffcc_StiRpos 
	position FFSL ort=yzx AlphaZ=frbd_Phi8 _
                        z=(ftpg_totLen/2)-5 _
                        x=-ffcc_StiRpos*cos(pi/6.)_
                        y=-ffcc_StiRpos*sin(pi/6.)
	position FFSL ort=yzx AlphaZ=-(frbd_Phi2) _
                        z=(ftpg_totLen/2)-5 _
                        x=ffcc_StiRpos*cos(pi/6.) _
                        y=-ffcc_StiRpos*sin(pi/6.)
*
	position FFSL ort=yzx AlphaZ= frbd_Phi4 _
                        z=-ftpg_totLen/2+5 _
                        y=ffcc_StiRpos 
	position FFSL ort=yzx AlphaZ=frbd_Phi8 _
                        z=-ftpg_totLen/2+5 _
                        x=-ffcc_StiRpos*cos(pi/6.)_
                        y=-ffcc_StiRpos*sin(pi/6.)
	position FFSL ort=yzx AlphaZ=-(frbd_Phi2)_
                        z=-ftpg_totLen/2+5 _
                        x=ffcc_StiRpos*cos(pi/6.)_
                        y=-ffcc_StiRpos*sin(pi/6.)
*
	create FFCE

	temp1 = ftpg_RinnerMs+ftpg_DrInAlL1+ftpg_DrInIsoL+ _
                ftpg_DrInAlL2
	temp2 = ((ftpg_RGasOut-temp1)/2)+temp1

	position FFCE in FGAS _
                 z=(ftpg_totLen/2+ffcc_StiDia/2+ffcc_BarWidt/2)-5_
                 y=temp2
	position FFCE in FGAS AlphaZ=-(frbd_Phi3) _
                 z=(ftpg_totLen/2+ffcc_StiDia/2+ffcc_BarWidt/2)-5 _
                 x=-temp2*cos(pi/6.) y=-temp2*sin(pi/6.)
	position FFCE in FGAS AlphaZ=frbd_Phi3 _
                 z=(ftpg_totLen/2+ffcc_StiDia/2+ffcc_BarWidt/2)-5 _
                 x=temp2*cos(pi/6.)_
                 y=-temp2*sin(pi/6.)
*
* Start here to position the FC Rings
*
        do Iring=17,1,-1
   	  create FFRA
	  position FFRA in FGAS z=(ftpg_totLen/2)-5-ffcc_StiDia/2
	  position FFRA in FGAS z=-ftpg_totLen/2+5+ffcc_StiDia/2
        enddo
*
Endblock
* ----------------------------------------------------------------------------
Block FSEN is the sensitive gas volume
      Material  Argon_gas
      Medium    sensitive  ISVOL=1
      attribute FSEN   seen=1  colo=4
*     attribute FSEN   seen=0  colo=0

      temp1=ftpg_RinnerMs+ftpg_DrInAlL1+ _
            ftpg_DrInIsoL+ftpg_DrInAlL2

      Shape     TUBE   Rmin=temp1,
                       Rmax=ftpg_RGasOut,
		       Dz=ftpg_LayLen/2
      Create    FSEC 
Endblock
* ----------------------------------------------------------------------------
Block FSEC is a sensitive gas sector
*
      Shape     division Iaxis=2  Ndiv=6 C0=30 
*
*  may be: ... cx:0.01: cy:0.01: cz:0.01: ptot:16:(-100,100) instead of px,py,pz
*
      HITS      FSEC   xx:16:SHX(-50,50)   yy:16:(-50,50)     zz:32:(-370,370),
                       px:16:(-100,100)    py:16:(-100,100)   pz:16:(-100,100),
                       Slen:16:(0,1.e4)    Tof:16:(0,1.e-6)   Step:16:(0,100),
                       SHTN:16:            Elos:32:(0,1)
Endblock
* ----------------------------------------------------------------------------
Block FIFR is the Al inner flange ring
	Material Aluminium 
	Attribute FMPT seen=1 colo=2
	shape   TUBE Rmin=ftpg_RinnerMs,
                     Rmax=ftpg_RinnerMs+ftpg_DrIFR,
		     Dz=ftpg_DzIFR/2
Endblock
* ----------------------------------------------------------------------------
Block FKWI is the double Kapton window
	Material  MYLAR
	Attribute FMPT seen=1 colo=3
	shape   TUBE Rmin=ftpg_RinnerMs,
                     Rmax=ftpg_RGasOut,
		     Dz=ftpg_DzKapton/2
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

	temp1 = ftpg_RinnerMs+ftpg_DrInAlL1+ftpg_DrInIsoL+ _
                ftpg_DrInAlL2

	shape   BOX Dx=ffcc_BarThik/2 _
                    Dy=(ftpg_RGasOut-temp1)/2 _
                    Dz=ffcc_BarWidt/2
Endblock
* ----------------------------------------------------------------------------
Block FROS is one Ring of Readout Modules in the support Structure  
	Material Air
	Attribute FROS seen=0 colo=1
*
*	deg = (pi/180.)
* PN:   degrad is Geant standard, described in the manual
*
        shape  TUBE Rmin=ftpg_RinnerMs Rmax=ftpg_RouterMs Dz=ftpg_RinnerMs

*
* Create different modules for the readout ring
*
           Create FROM
           Create FREL
           Create FRCC
           Create FRCE
* 
* Do Loop position Readoutmodules, electronic plus cooling
*
*  'position IN FROS' should be avoided - default is the current block !
*
* PN: this creates the same geometry, but is a bit simpler:
*     check with:         dcut fros z 0 10 10 .25 .25
*     sin and cos are difined over full solid angle, not just in 0-90
*

*
* Support wedge in Readoutring consist out of two parts
*
* First Trapezoid
*
           Create FROT           

           Do gg=frbd_Phi2,frbd_Phi12,frbd_Phi3
              Position FROT  ort=yzx alphaz=gg _
                 y=-fssd_TrapR*sin(degrad*gg)_
                 x=-fssd_TrapR*cos(degrad*gg)      
           EndDo
*
* Second Triangle
*
           Create FROP

           Do hh=frbd_Phi1,frbd_Phi11,frbd_Phi3
              Position FROP  AlphaZ=hh _
                 x=fssd_PolyR*(sin(degrad*hh))_
                 y=fssd_PolyR*(cos(degrad*hh))
           EndDo

*
*End of support wedge in readoutring  
*

           Do ww=frbd_Phi2,frbd_Phi12,frbd_Phi3 
*
              Position FROM  AlphaZ=ww+180 _
                             x=-29.42*sin(degrad*ww)_
                             y=ftpg_RRoM*cos(degrad*ww)_
                             konly='MANY'
* PN:         position is done relative to the volume CENTER
              Position FREL  AlphaZ=ww _
                             x=-(ftpg_RElCard+frbd_ElectrDY)*sin(degrad*ww) _
                             y= (ftpg_RElCard+frbd_ElectrDY)*cos(degrad*ww) 
              Position FRCC  AlphaZ=ww _
                             x=-ftpg_RCooPlm*sin(degrad*ww) _
                             y= ftpg_RCooPlm*cos(degrad*ww)
* PN:         left and right wings of the copper
              Position FRCE  AlphaZ=(ww+22) _
                             x=-ftpg_RCooPle*sin(degrad*(ww+30-6.5)) _
                             y= ftpg_RCooPle*cos(degrad*(ww+30-6.5))

              Position FRCE  AlphaZ=(ww-22) _
                             x=-ftpg_RCooPle*sin(degrad*(ww-30+6.5)) _
                             y= ftpg_RCooPle*cos(degrad*(ww-30+6.5))

            EndDO

*
*End of one complete Readout Ring 
*
Endblock
* ----------------------------------------------------------------------------
Block FROM is one Module of the Readout Chamber
	Material  Aluminium
	Attribute FROM seen=1 colo=6
	shape BOX Dx=frbd_XRoM/2.,
                  Dy=frbd_YRoM/2.,
                  Dz=frbd_ZRoM/2.

* PN: default is position in current block, 'IN FROM' not needed

        Create and position FROE  x=frbd_XEHol y=frbd_YEHol      konly='MANY'
        Create and position FROE  x=-(frbd_XEHol) y=frbd_YEHol   konly='MANY'

        Create and position FROL  z=frbd_XLHol  y=frbd_YLHol     konly='MANY'
        Create and position FROL  z=-(frbd_XLHol)  y=frbd_YLHol  konly='MANY'
        
        frob_x1 = -(frbd_BOffset)

        Do jj=1,5
         Create and position FROB  AlphaY=frbd_Phi1 x=frob_x1_ 
                     z=frbd_ZOffB      konly='MANY'
         Create and position FROB  AlphaY=frbd_Phi1 x=frob_x1_
                     z=-(frbd_ZOffB)   konly='MANY'
         frob_x1 = frob_x1 + 5.96 
        EndDo
         
Endblock
* ----------------------------------------------------------------------------
Block FROB are the Box Holes in the Readout Chamber
	Material Air
	Attribute FROB seen=1 colo=1
	shape BOX Dx=frbd_BoxHX,
                  Dy=frbd_BoxHY,
                  Dz=frbd_BoxHZ
Endblock
* ----------------------------------------------------------------------------
Block FROE are the End Box Holes in the Readout Chamber
	Material Air
	Attribute FROE seen=1 colo=3
	shape BOX Dx=frbd_EBoxHX,
                  Dy=frbd_EBoxHY,
                  Dz=frbd_EBoxHZ
Endblock
* ----------------------------------------------------------------------------
Block FROL are the Length side Box Holes in the Readout Chamber
	Material Air
	Attribute FROL seen=1 colo=2
	Shape BOX Dx=frbd_LBoxHX,
                  Dy=frbd_LBoxHY,
                  Dz=frbd_LBoxHZ
Endblock
* ----------------------------------------------------------------------------
Block FROP are the Polygon part of the support bar
	Material Aluminium
	Attribute FROP seen=1 colo=1
	Shape PGON Phi1=frbd_Phi2,
                   Dphi=frbd_Phi13,
                   Nz=2,
                   Npdv=3,
                   zi ={ -(fssd_PolyDZ), fssd_PolyDZ},
                   rmn={ fssd_PolyIR, fssd_PolyIR},
                   rmx={ fssd_PolyOR, fssd_PolyOR}
Endblock
* ----------------------------------------------------------------------------
Block FROT  are the Trapezoid part of the support bar
	Material Aluminium
	Attribute FROT seen=1 colo=1
	Shape TRD1 Dx1 = fssd_TrapX1,
                   Dx2 = fssd_TrapX2,
                   Dy  = fssd_TrapDY,
                   Dz  = fssd_TrapDZ
Endblock
* ----------------------------------------------------------------------------
Block FREL is the Electronics Layer of the Readout Chamber
*     G10 is about 60% SiO2 and 40% epoxy
         Component Si  A=28.08  Z=14   W=0.6*1*28./60.
         Component O   A=16     Z=8    W=0.6*2*16./60.
         Component C   A=12     Z=6    W=0.4*8*12./174.
         Component H   A=1      Z=1    W=0.4*14*1./174.
         Component O   A=16     Z=8    W=0.4*4*16./174.
        Mixture   G10    Dens=1.7
	Material G10 
	Attribute FREL seen=1 colo=3
	shape BOX Dx=frbd_ElectrDX,
                  Dy=frbd_ElectrDY,
                  Dz=frbd_ElectrDZ
Endblock
* ----------------------------------------------------------------------------
Block FRCC is the Copper Cooling Layer of the Readout Chamber (Middle)
	Material Copper 
	Attribute FRCC seen=1 colo=2
	shape BOX Dx=frbd_CoolPlDX,
                  Dy=frbd_CoolPlDy,
                  Dz=frbd_CoolPlDz
Endblock

* ----------------------------------------------------------------------------
Block FRCE is the Copper Cooling Layer of the Readout Chamber (Ends)
	Material Copper 
	Attribute FRCE seen=1 colo=2
	shape BOX Dx=frbd_EClPlDX,
                  Dy=frbd_EClPlDY, 
                  Dz=frbd_EClPlDZ

Endblock
* ----------------------------------------------------------------------------
Block FSER is the Support End Ring
	Material Air
	Attribute FSER seen=1 colo=2
	shape TUBE Rmin= fssd_EringRmn,
                   Rmax= fssd_EringRmx,
                   Dz=fssd_EringDZ

	Create and position FSRA in FSER z=fssd_ERPosZ
	Create and position FSRB in FSER z=0.
	Create and position FSPG in FSER z=-(fssd_ERPosZ)

Endblock
* ----------------------------------------------------------------------------
Block FSRA is the outer Support End Ring
	Material Aluminium
	Attribute FSRA seen=1 colo=2
	shape TUBE Rmin=fssd_EringRmn,
                   Rmax=fssd_EringRmx,
                   Dz=fssd_OEringDZ
Endblock
* ----------------------------------------------------------------------------
Block FSRB is the medium Support End Ring
	Material Aluminium
	Attribute FSRB seen=1 colo=2
	shape TUBE Rmin=fssd_EringRmn,
                   Rmax=fssd_MEringRm,
                   Dz=fssd_MEringDZ
Endblock
* ----------------------------------------------------------------------------
Block FSPG is the inner Support End Ring and the outer support Rings 
	Material Aluminium
	Attribute FSPG seen=1 colo=2
        z1 = -0.5 
        z2 = 0.5
        Shape     PGON  Phi1=frbd_Phi4,
                        Dphi=frbd_Phi13,
                        Nz=2,  Npdv=6,
                        zi ={ z1, z2},
                        rmn={ fssd_PolyIR, fssd_PolyIR},
                        rmx={ fssd_ERPolyRm, fssd_ERPolyRm}
      
       Create and position FSPI in FSPG
Endblock
* ----------------------------------------------------------------------------
Block FSPI is the Hole of the inner Support End Ring
	Material Air
	Attribute FSPI seen=1 colo=1
	Shape TUBE Rmin=ftpg_RinnerMs,
                   Rmax=ftpg_RGasOut,
                   Dz=ftpg_SERHole
Endblock
* ----------------------------------------------------------------------------
Block FSSM is the main Support Stucture Module 
	Material Air
	Attribute FSSM seen=0 colo=1

* PN:   standard degrad should be used - nobody knows what phi7 is
*       (and it may be changed with time !)
*	deg = (pi/frbd_Phi7)
*
	Shape TUBE Rmin=ftpg_RinnerMs,
                   Rmax = ftpg_RouterMs,
                   Dz=ftpg_MSRDZ

        Create and position FSPG in FSSM z=fssd_PGonPDZ
        Create and position FSRI in FSSM
        Create and position FSPG in FSSM z=-(fssd_PGonPDZ)

        Create and position FSBA in FSSM AlphaZ=frbd_Phi1 y=fssd_SBSDy
        Create and position FSBA in FSSM AlphaZ=frbd_Phi1 y=-(fssd_SBSDy)
        
        Create and position FSBA in FSSM AlphaZ=frbd_Phi11 _
                             x=fssd_SBSDy*cos(frbd_Phi2*degrad) _
                             y=fssd_SBSDy*sin(frbd_Phi2*degrad)
        Create and position FSBA in FSSM AlphaZ=frbd_Phi3 _
                             x=-(fssd_SBSDy*cos(frbd_Phi2*degrad)) _
                             y=fssd_SBSDy*sin(frbd_Phi2*degrad)
        Create and position FSBA in FSSM AlphaZ=frbd_Phi5 _
                             x=-(fssd_SBSDy*cos(frbd_Phi2*degrad)) _
                             y=-(fssd_SBSDy*sin(frbd_Phi2*degrad))
        Create and position FSBA in FSSM AlphaZ=frbd_Phi9 _
                             x=(fssd_SBSDy*cos(frbd_Phi2*degrad)) _
                             y=-(fssd_SBSDy*sin(frbd_Phi2*degrad))

Endblock
* ----------------------------------------------------------------------------
Block FSRI is the inner Support Ring
	Material Aluminium
	Attribute FSRI seen=1 colo=1
	shape TUBE Rmin=ftpg_RGasOut,
                   Rmax=ftpg_RISRing,
                   Dz=ftpg_ISRingDZ
Endblock

* ----------------------------------------------------------------------------
Block FSBA are the Stabilizer Block for the  inner Support Ring
	Material Aluminium
	Attribute FSBA seen=1 colo=1
	shape BOX  Dx=ftpg_SBSRDx,
                   Dy=ftpg_SBSRDy,
                   Dz=ftpg_SBSRDz
Endblock

* ----------------------------------------------------------------------------

Block FPAD is the Pad plane of the FTPC 
*BendFlex consist of 65 % polyester 35% glas
*         Component Si  A=28.08  Z=14   W=0.6*1*28./60.
*         Component O   A=16     Z=8    W=0.6*2*16./60.
*         Component C   A=12     Z=6    W=0.4*8*12./174.
*         Component H   A=1      Z=1    W=0.4*14*1./174.
*         Component O   A=16     Z=8    W=0.4*4*16./174.
*        Mixture  Bendflex    Dens=1.7
*	Material Bendflex
        Material MYLAR
	Attribute FPAD seen=1 colo=2
	shape TUBE Rmin=ftpg_RGasOut-0.25, 
                   Rmax=ftpg_RGasOut, 
                   Dz=ftpg_GasVolDz
Endblock
*
* ----------------------------------------------------------------------------
Block FFRA is outermost FC Ring
	Material Aluminium 
	Attribute FFRA seen=1 colo=2
* 17 Rings -> 18 gaps
	temp1 = (ftpg_RGasOut-(ftpg_RinnerMs+ftpg_DrInAlL1+ _
                ftpg_DrInIsoL+ftpg_DrInAlL2))/18
	temp2 = ftpg_RinnerMs+ftpg_DrInAlL1+ftpg_DrInIsoL+ _
                ftpg_DrInAlL2
	shape   TUBE Rmin=temp2+Iring*temp1-ffcc_RiDr/2,
                     Rmax=temp2+Iring*temp1+ffcc_RiDr/2,
		     Dz=ffcc_RiThick/2
Endblock
* ----------------------------------------------------------------------------
 
      END
