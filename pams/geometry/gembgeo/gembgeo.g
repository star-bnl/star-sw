* $Id: gembgeo.g,v 1.1 2005/06/28 16:16:24 potekhin Exp $
*
* $Log: gembgeo.g,v $
* Revision 1.1  2005/06/28 16:16:24  potekhin
* Gem barrel tracker code created by Nikolai and Gerrit.
* Only active in the tag IST1 for now. Tested by Maxim.
*
******************************************************************
      module GEMBGEO is the geometry of GEMs in front of the TPC
      Author   NNS
      Created  Sept 25, 2004

*     Modified by GvN, May 18, 2005
*     Added mothervolumes etc.
*     Code review by Maxim Potekhin, June 28, 2005, CVS tags added
*
*    Notes:
*    Two layers of GEM strip detectors (check gem_pattern)
*    First (or Inner)  -- R ~ 34 cm
*    Second (or Outer) -- R ~ 40 cm
*
* -----------------------------------------------------------
+CDE,AGECOM,GCUNIT,GCONST.

      integer n, k
      real ungl, angl

      Content GMBO,GMVI,GMDI,GMVO,GMDO,GMMI,GMGI,GMMO,GMGO

      Structure GMMG {Version, Rin,  Rout,  TotalLength}
      structure GEMV {layer,   dx,   dy,    dz,
                      rpos,    phi,  nsd,   nz, alpha, 
                      frth,    rdth, mlrth, sgasth}


*Begin
*-----------------------------------------------------------------
      Fill GMMG			! Mother volume data
         Version     = 1	! Version
         Rin         = 28.0	! Inner radius
         Rout        = 42.0	! Outer radius
         TotalLength = 260.0    ! Maximum length of the detector
      EndFill
*-----------------------------------------------------------------
      Fill GEMV         ! GEM strip Det system 
	layer = 1       ! inner layer
	dx = 5.5        ! one detector dx ( I , for O - x2) org=5.0cm org2=5.5
	dy = 0.5        ! one detector dy (thickness)
	dz = 5.0        ! one detector dz ( I , for O - x2)
	rpos=33.0       ! r position I layer org=30.0
	phi=18.94737    ! angle in phi
	nsd=19          ! n of ladens in phi
	nz=16           ! n of ladens in z
	alpha=10.0      ! inner layer shift angle org=8.0
	frth=0.35       ! frame thickness
	rdth=0.25       ! read-out plate thickness
	mlrth=0.04      ! mylar window thickness
	sgasth=0.3      ! sensive gas thickness
      EndFill

      Fill GEMV         ! GEM strip Det system 
	layer = 2       ! outer layer
	rpos=40.0       ! r position O layer org=38.0
	phi=13.84615    ! angle step in phi
	nsd=26          ! n of ladens in phi
	nz=24           ! n of ladens in z
	alpha=10.0      ! outer layer rotation angle org=5.0
      EndFill
*-----------------------------------------------------------------

*     G10 is about 60% SiO2 and 40% epoxy (stolen from ftpcgeo.g)
      Component Si  A=28.08  Z=14   W=0.6*1*28./60.
      Component O   A=16     Z=8    W=0.6*2*16./60.
      Component C   A=12     Z=6    W=0.4*8*12./174.
      Component H   A=1      Z=1    W=0.4*14*1./174.
      Component O   A=16     Z=8    W=0.4*4*16./174.
      Mixture   G10    Dens=1.7


*     mixture gas C20: Ar/CO2 80:20 by volume
      Component Ar    A=40  Z=18 W=8
      Component C     A=12  Z=6  W=1
      Component O     A=16  Z=8  W=2
      Mixture   c20   Dens=0.8*0.00166+0.2*0.001977  " g/cm**3 "

*-----------------------------------------------------------------
      USE GMMG

************************* MOTHER ************************

      Create   GMBO
      Position GMBO in CAVE


*-----------------------------------------------------------------
Block GMBO is the mother of the GEMB detector
      Material  Air
      Attribute GMBO  Seen=0  colo=6

      Shape TUBE Rmin = GMMG_Rin             _
                 Rmax = GMMG_Rout            _
                 Dz   = GMMG_TotalLength/2.0

     Use GEMV layer=1
       do n = 1, GEMV_nz
         do k = 1, GEMV_nsd

           ungl = (k-1)*GEMV_phi
           angl = (90+ungl)*degrad

           Create and Position GMVI in GMBO  x=GEMV_rpos*cos(angl),
                                             y=GEMV_rpos*sin(angl),
                                             z=(-(GEMV_nz-1)+2.*(n-1))*(GEMV_dz+2.*GEMV_frth),
                                             ThetaX=90, ThetaY=90, ThetaZ=0,
                                             PhiX=ungl+GEMV_alpha, PhiY=90+ungl+GEMV_alpha, PhiZ=0
         enddo
       enddo


     Use GEMV layer=2
       do n = 1, GEMV_nz
         do k = 1, GEMV_nsd

           ungl = (k-1)*GEMV_phi
           angl = (90+ungl)*degrad

           Create and Position GMVI in GMBO  x=GEMV_rpos*cos(angl),
                                             y=GEMV_rpos*sin(angl),
                                             z=(-(GEMV_nz-1)+2.*(n-1))*(GEMV_dz+2.*GEMV_frth),
                                             ThetaX=90, ThetaY=90, ThetaZ=0,
                                             PhiX=ungl+GEMV_alpha, PhiY=90+ungl+GEMV_alpha, PhiZ=0
         enddo
       enddo

EndBlock
*-----------------------------------------------------------------
Block GMVI is a GEM strip detector, layer 1

    Material G10
    Attribute GMVI SEEN=1   colo=2

    Shape BOX dx=GEMV_dx+GEMV_frth  dy=GEMV_dy  dz=GEMV_dz+GEMV_frth

    Create and Position GMMI x=0. y=-GEMV_rdth/2. z=0.

endblock
*-----------------------------------------------------------------
Block GMMI is a GEM strip detector, mylar staff, layer 1

    Material Mylar
    Attribute GMMI SEEN=1   colo=3

    Shape BOX dx=GEMV_dx  dy=GEMV_dy-GEMV_rdth/2.  dz=GEMV_dz

    Create and Position GMGI x=0. y=0. z=0.

EndBlock
*-----------------------------------------------------------------
Block GMGI is a GEM strip detector filled with gas, layer 1

    Material c20
    Attribute GMGI SEEN=1   colo=3

    Shape BOX dx=GEMV_dx  dy=GEMV_dy-GEMV_rdth/2.-GEMV_mlrth  dz=GEMV_dz

    Create and Position GMDI x=0. y=0. z=0.

EndBlock
*-----------------------------------------------------------------
Block GMDI is the sensitive volume of the GEM strip detector, layer 1

    Material c20
    Material Sensitive Isvol=1
    Attribute GMDI SEEN=1   colo=4

    Shape BOX dx = GEMV_dx         _
              dy = GEMV_sgasth/2.0 _ 
              dz = GEMV_dz


    HITS    GMDI   Z:.001:S  Y:.001:   X:.001:     Ptot:16:(0,100),
                   cx:10:    cy:10:    cz:10:      Sleng:16:(0,500),
                   ToF:16:(0,1.e-6)    Step:.01:   Eloss:16:(0,0.001)

    !HITS  GMDI   zz:.00001:S  Yy:.00001:   Xx:.00001:     Etot:16:(0,100),
    !px:16:(-60,60)    py:16:(-60,60)    pz:16:(-60,60),
    !z:.00001:  y:.00001:   x:.00001:,
    !Eloss:32:(0,0.01) Step:16:(0,1.)

EndBlock
*------------------------------------------------------------------------------
end

*------------------------------------------------------------------------------
