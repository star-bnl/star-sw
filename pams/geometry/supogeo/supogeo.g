******************************************************************************
Module   SUPOGEO  is the geometry of the Forward TPC supports in STAR
Author   Holm Huemmler
Created  27-Okt-99
* modification:
******************************************************************************
+CDE,AGECOM,GCONST,GCUNIT.
*
	Content SUPO, SUPL, SUPH, SLRL, SLWL, SLHD, SLXW, SLEN, SLFX, SLBL,
		SHRL, SHPT, SHBR, SHBK, SHFX, SHST 
*
	structure SMAI {Version, RInner, ROuter, ZMin, ZMax, PhiMid,
                        FixHei, FixWid, FixThk}
	structure SSLO {Version, PhiMin, PhiMax, 
                        RailLen, RailWIn, RailWOut, RailHei,
                        WallWid, WallHei, HeadThk, HeadHei,
                        XWalThk, XWal1Pos, XWal2Pos,
                        EndThk, EndHei, BoltPos, BoltOff, BoltRad}
	structure SSHI {Version, PhiMin, PhiMax, XDist, YDist,
                        RailWid, RailHei, PlatThk, PlatHei, 
			BarLen, BarOffZ, BarHei, TopWid,
			BlocHei, BlocLen, BoltOff,
			StabWid, StabThk, Stab1Z, Stab2Z}
*
	Integer  Agexist
	Real pos, angle
*
* ----------------------------------------------------------------------------

*
   Fill SMAI	    !  FTPC support envelope geometry 
	Version		= 1	! geometry Version
	RInner		= 36.4	! envelope inner radius 
	ROuter		= 47.5	! envelope outer radius
	ZMin		= 230.69! envelope minimum z value
	ZMax		= 259.7	! envelope maximum z value
	PhiMid		= 30	! angle of support center positions
	FixHei		= 10.22	! Height of support fixture plate (on TPC)
	FixWid		= 3.79	! Width of support fixture plate 
	FixThk		= 2.04	! Thickness of support fixture plate
   Endfill
*
   Fill SSLO	    ! lower FTPC support geometry 
	Version		= 1	! geometry Version
	PhiMin		= -6	! envelope start angle
	PhiMax		= 11	! envelope end angle
	RailLen		= 24.5	! support rail length
	RailWIn		= 4.07	! support rail inner width
	RailWOut	= 7.03	! support rail outer width
	RailHei		= 2.96	! support rail height
	WallWid		= 0.78	! support wall thickness
	WallHei		= 4.46	! support wall height
        HeadThk		= 2.68	! support head thickness
        HeadHei		= 7.82	! support head height
	XWalThk		= 0.82	! cross support wall thickness
	XWal1Pos	= 0.55	! 1st cross support wall pos (to env center)
	XWal2Pos	= 7.02	! 2nd cross support wall pos (to env center)
        EndThk		= 2.6	! support end block thickness
        EndHei		= 1.49	! support end block height
        BoltPos		= 44.9	! support bolt radial position
        BoltOff		= 1.54	! support bolt offset from fixture center
        BoltRad		= 1.2	! effective support bolt radius
   Endfill
*
   Fill SSHI	    ! upper FTPC support geometry 
	Version		= 1	! geometry Version
	PhiMin		= 38	! envelope start angle
	PhiMax		= 65	! envelope end angle
	XDist		= 34.19	! distance of support to beam axis in x
	YDist		= 16.39	! distance of support bottom to axis in y
	RailWid		= 1.75	! Width of upper FTPC support rail
	RailHei		= 1.93	! Height of support rail 
	PlatThk		= 0.41	! Thickness of support plate
	PlatHei		= 8.69	! Height of support plate
	BarLen		= 10.58	! Length of support top bar
	BarOffZ		= 3.22	! Offset of bar from support base in z
	BarHei		= 0.81	! Height of support top bar
	TopWid		= 2.58	! Width of support top part
	BlocHei		= 2.74	! Height of support top block
	BlocLen		= 5.5	! Length of support top block
	BoltOff		= 3.26	! support bolt offset from fixture center
	StabWid		= 1.15	! Width of support stabilizers
	StabThk		= 0.41	! Thickness of support stabilizers
	Stab1Z		= 8.32	! z-position of first stabilizer
	Stab2Z		= 13.58	! z-position of second stabilizer
Endfill
*
*
*
	pos=(smai_ZMin+smai_ZMax)/2

	create   SUPO

        position SUPO in CAVE z=pos             konly='MANY'
	position SUPO in CAVE z=-pos ThetaZ=180 konly='MANY' 
*  
* ----------------------------------------------------------------------------
Block SUPO is the FTPC support mother volume

      Material  Air
      Medium    Standard
      Attribute SUPO     seen=0   colo=1
      shape     TUBE     Rmin=smai_RInner, 
                         Rmax=smai_ROuter, 
                         Dz=(smai_ZMax-smai_ZMin)/2

      Create   SUPL
      position SUPL             PhiX=-smai_PhiMid  PhiY=-smai_PhiMid+90
      position SUPL ThetaX=270  PhiX=smai_PhiMid   PhiY=smai_PhiMid+90
      Create   SUPH
      position SUPH
      position SUPH ThetaX=270

Endblock

* ----------------------------------------------------------------------------

Block SUPL is the lower FTPC support mother volume

      shape    TUBS     Rmin=smai_RInner, 
                        Rmax=smai_ROuter, 
                        Dz=(smai_ZMax-smai_ZMin)/2,
                        Phi1=-sslo_PhiMax,
                        Phi2=-sslo_PhiMin
      Create   SLRL
      position SLRL ORT=YZX x=(smai_RInner+sslo_RailHei/2),
                    z=(smai_ZMax-smai_ZMin-sslo_RailLen)/2
      Create   SLWL 
      position SLWL x=(smai_RInner+sslo_RailHei+sslo_WallHei/2),
                    y=(sslo_RailWOut/2-sslo_WallWid/2),
                    z=(smai_ZMax-smai_ZMin-sslo_RailLen+sslo_HeadThk)/2
      position SLWL x=(smai_RInner+sslo_RailHei+sslo_WallHei/2),
                    y=-(sslo_RailWOut/2-sslo_WallWid/2),
                    z=(smai_ZMax-smai_ZMin-sslo_RailLen+sslo_HeadThk)/2
      Create   SLHD
      position SLHD x=(smai_RInner+sslo_RailHei+sslo_HeadHei/2),
                    z=(smai_ZMax-smai_ZMin+sslo_HeadThk)/2-sslo_RailLen
      Create   SLXW
      position SLXW x=(smai_RInner+sslo_RailHei+sslo_WallHei/2),
                    z=sslo_XWal1Pos
      position SLXW x=(smai_RInner+sslo_RailHei+sslo_WallHei/2),
                    z=sslo_XWal2Pos
      Create   SLEN
      position SLEN x=(smai_RInner+sslo_RailHei+sslo_EndHei/2),
                    z=(smai_ZMax-smai_ZMin-sslo_EndThk)/2
      Create   SLFX
      position SLFX x=sslo_BoltPos, y=-sslo_BoltOff,
                    z=(smai_ZMin-smai_ZMax+smai_FixThk)/2
      Create   SLBL
      position SLBL x=sslo_BoltPos,
                    z=(smai_FixThk-sslo_RailLen)/2
Endblock

* ----------------------------------------------------------------------------

Block SLRL is the lower FTPC support rail

      Material  aluminium
      Attribute SLRL    seen=1   colo=2
      shape     TRD1    Dx1=sslo_RailWIn/2,
                        Dx2=sslo_RailWOut/2,
                        Dy=sslo_RailLen/2,
                        Dz=sslo_RailHei/2
Endblock

* ----------------------------------------------------------------------------

Block SLWL is the lower FTPC support side wall

      Material  aluminium
      Attribute SLRL    seen=1   colo=2
      shape     BOX     Dx=sslo_WallHei/2,
                        Dy=sslo_WallWid/2,
                        Dz=sslo_RailLen/2-sslo_HeadThk/2
Endblock

* ----------------------------------------------------------------------------

Block SLHD is the lower FTPC support head plate (mounted to TPC)

      Material  aluminium
      Attribute SLHD   seen=1   colo=2
      shape     BOX     Dx=sslo_HeadHei/2,
                        Dy=sslo_RailWOut/2,
                        Dz=sslo_HeadThk/2

Endblock

* ----------------------------------------------------------------------------

Block SLXW is the lower FTPC support cross wall

      Material  aluminium
      Attribute SLXW    seen=1   colo=2
      shape     BOX     Dx=sslo_WallHei/2,
                        Dy=sslo_RailWOut/2-sslo_WallWid,
                        Dz=sslo_XWalThk/2

Endblock

* ----------------------------------------------------------------------------

Block SLEN is the lower FTPC support end block


      Material  aluminium
      Attribute SLEN    seen=1   colo=2
      shape     BOX     Dx=sslo_EndHei/2,
                        Dy=sslo_RailWOut/2-sslo_WallWid,
                        Dz=sslo_EndThk/2

Endblock

* ----------------------------------------------------------------------------

Block SLFX is the lower FTPC support fixture plate

      Material  aluminium
      Attribute SLFX    seen=1   colo=2
      shape     BOX     Dx=smai_FixWid/2,
                        Dy=smai_FixHei/2,
                        Dz=smai_FixThk/2

Endblock

* ----------------------------------------------------------------------------

Block SLBL is the lower FTPC support bolt

      Material  aluminium
      Attribute SLBL    seen=1   colo=2
      shape     TUBE    Rmin=0,
                        Rmax=sslo_BoltRad,
                        Dz=(smai_ZMax-smai_ZMin-sslo_RailLen-smai_FixThk)/2

Endblock

* ----------------------------------------------------------------------------

Block SUPH is the upper FTPC support mother volume

      shape     TUBS     Rmin=smai_RInner, 
                         Rmax=smai_ROuter, 
                         Dz=(smai_ZMax-smai_ZMin)/2,
                         Phi1=90-sshi_PhiMax,
                         Phi2=90-sshi_PhiMin

      Create   SHRL
      position SHRL x=sshi_XDist+sshi_PlatThk/2-sshi_RailWid/2,
                    y=sshi_YDist+sshi_RailHei/2
      Create   SHPT
      position SHPT ORT=XYZ AlphaY=90 x=sshi_XDist,
                    y=sshi_YDist+sshi_RailHei+sshi_PlatHei/2,
                    z=-sshi_BarOffZ
      Create   SHBR
      position SHBR x=sshi_XDist,
                    y=sshi_YDist+sshi_RailHei+sshi_PlatHei+sshi_BarHei/2,
                    z=(smai_ZMin-smai_ZMax)/2+sshi_BarOffZ+sshi_BarLen/2
      Create   SHBK
      pos=sshi_YDist+sshi_RailHei+sshi_PlatHei+sshi_BarHei
      position SHBK x=sshi_XDist,
                    y=pos+sshi_BlocHei/2,
                    z=(smai_ZMin-smai_ZMax)/2+smai_FixThk+sshi_BlocLen/2
      Create   SHFX
      position SHFX Alphaz=45,
                    x=sshi_XDist-sshi_BoltOff*sin(degrad*45),
                    y=pos+sshi_BlocHei/2+sshi_BoltOff*cos(degrad*45),
                    z=(smai_ZMin-smai_ZMax)/2+smai_FixThk/2
      Create   SHST
      position SHST x=sshi_XDist-sshi_PlatThk/2-sshi_StabWid/2,
                    y=sshi_YDist+sshi_RailHei+sshi_PlatHei/2,
                    z=(smai_ZMin-smai_ZMax)/2+sshi_Stab1Z
      position SHST x=sshi_XDist+sshi_PlatThk/2+sshi_StabWid/2,
                    y=sshi_YDist+sshi_RailHei+sshi_PlatHei/2,
                    z=(smai_ZMin-smai_ZMax)/2+sshi_Stab1Z
      position SHST x=sshi_XDist-sshi_PlatThk/2-sshi_StabWid/2,
                    y=sshi_YDist+sshi_RailHei+sshi_PlatHei/2,
                    z=(smai_ZMin-smai_ZMax)/2+sshi_Stab2Z
      position SHST x=sshi_XDist+sshi_PlatThk/2+sshi_StabWid/2,
                    y=sshi_YDist+sshi_RailHei+sshi_PlatHei/2,
                    z=(smai_ZMin-smai_ZMax)/2+sshi_Stab2Z

Endblock

* ----------------------------------------------------------------------------

Block SHRL is the upper FTPC support rail

      Material  aluminium
      Attribute SHRL    seen=1   colo=2
      shape     BOX     Dx=sshi_RailWid/2,
                        Dy=sshi_RailHei/2,
                        Dz=(smai_ZMax-smai_ZMin)/2

Endblock

* ----------------------------------------------------------------------------

Block SHPT is the upper FTPC support main plate

      pos=((smai_ZMax-smai_ZMin)/2-(sshi_BarOffZ+sshi_BarLen/2))
      angle=atan(pos/sshi_PlatHei)

      Material  aluminium
      Attribute SHPT    seen=1   colo=2
      shape     TRAP    Dz=sshi_PlatThk/2,
                        Thet=0,
                        Phi=0,
                        H1=sshi_PlatHei/2,
                        Bl1=(smai_ZMax-smai_ZMin)/2,
			Tl1=sshi_BarLen/2,
			Alp1=angle/degrad,
                        H2=sshi_PlatHei/2,
                        Bl2=(smai_ZMax-smai_ZMin)/2,
			Tl2=sshi_BarLen/2,
			Alp2=angle/degrad

Endblock

* ----------------------------------------------------------------------------

Block SHBR is the upper FTPC support top bar

      Material  aluminium
      Attribute SHBR   seen=1   colo=2
      shape     BOX     Dx=sshi_TopWid/2,
                        Dy=sshi_BarHei/2,
                        Dz=sshi_BarLen/2

Endblock

* ----------------------------------------------------------------------------

Block SHBK is the upper FTPC support top block


      Material  aluminium
      Attribute SHBK   seen=1   colo=2
      shape     BOX     Dx=sshi_TopWid/2,
                        Dy=sshi_BlocHei/2,
                        Dz=sshi_BlocLen/2

Endblock

* ----------------------------------------------------------------------------

Block SHFX is the upper FTPC support fixture plate

*     G10 is about 60% SiO2 and 40% epoxy
         Component Si  A=28.08  Z=14   W=0.6*1*28./60.
         Component O   A=16     Z=8    W=0.6*2*16./60.
         Component C   A=12     Z=6    W=0.4*8*12./174.
         Component H   A=1      Z=1    W=0.4*14*1./174.
         Component O   A=16     Z=8    W=0.4*4*16./174.
      Mixture   G10    Dens=1.7
      Attribute SHFX   seen=1   colo=3
      shape     BOX     Dx=smai_FixWid/2,
                        Dy=smai_FixHei/2,
                        Dz=smai_FixThk/2

Endblock

* ----------------------------------------------------------------------------

Block SHST are the upper FTPC support stabilizers

      Material  aluminium
      Attribute SHBK   seen=1   colo=2
      shape     BOX     Dx=sshi_StabWid/2,
                        Dy=sshi_PlatHei/2,
                        Dz=sshi_StabThk/2

Endblock

* ----------------------------------------------------------------------------
 
      END
