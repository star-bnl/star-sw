* $Id: shldgeo.g,v 1.1 2005/08/16 00:57:23 potekhin Exp $
*
* $Log: shldgeo.g,v $
* Revision 1.1  2005/08/16 00:57:23  potekhin
* New source: the upstream shielding
*
******************************************************************************
Module SHLDGEO is the shielding
  Created  15-Aug-2005 
  Author   Maxim Potekhin
*
******************************************************************************
+CDE,AGECOM,GCUNIT.
*
      Content  SHLD, SHBS, SHLS, SHBI, SHFI, SHOL, QONE, QTWO, QTHR
*
      Structure SHLG {  Version,     Z,
                            dx,     dy,     dz,
                         baseX,  baseLevel,  baseZ,
                        baseDx, baseDy,
                         slabX,  slabZ,
                        slabDy, slabDz, fiDz, fiDy, hole}

      Structure SHLQ { Version,
                       Q0,
                       Q1, ri1, ro1, dz1,
                       Q2, ri2, ro2, dz2,
                       Q3, ri3, ro3, dz3
                     }
*
*    local variable for section positioning
      Real    Yslab, Width, Yfi
*
* -----------------------------------------------------------------------------
*
   Fill SHLG    !  Shielding Geometry Data
      Version   = 1       ! geometry version  
      Z         = 1920    ! position of the shielding
      dx        = 120     ! half-x dimension
      dy        = 145     ! half-y dimension
      dz        = 100     ! half-z dimension

      baseX     =  30     ! base position
      baseLevel =-145     ! base position on the floor
      baseZ     =  0      ! base position

      baseDx    =  40     ! base half-x dimension
      baseDy    =  30     ! base half-y dimension

      slabX     =   0     ! slab position
      slabZ     = -30     ! slab position

      slabDy    =  40     ! slab half-y dimension
      slabDz    =  30     ! slab half-z dimension

      fiDz      =  50     ! forward iron slab half-thickness
      fiDy      =  57     ! half-height

      hole      =  10     ! beam hole half-size
   EndFill
* -----------------------------------------------------------------------------
   Fill SHLQ    !  Quadrupole Geometry Data
      Version   =   1     ! geometry version  
      Q0        = 2485.26 ! offset point that corresponds to 1505.92 in CAD notation, end of D0
*
      Q1        = 88.59   ! offset 1
      ri1       =  6.355  ! inner 1
      ro1       =  18.28  ! outer 1
      dz1       = 170.92  ! full length 1
*
      Q2        = 324.90  ! offset 2
      ri2       =  6.355  ! inner 2
      ro2       =  18.28  ! outer 2
      dz2       = 444.02  ! full length 2
*
      Q3        = 823.94  ! offset 3
      ri3       =  6.355  ! inner 3
      ro3       =  18.28  ! outer 3
      dz3       = 399.55  ! full length 3
   EndFill


* = = = = = = = = = = = =


      USE      SHLG
      Create   SHLD
      Position SHLD in CAVE z=shlg_Z

      Create QONE
      Position QONE in CAVE z=shlq_q0+shlq_q1+shlq_dz1/2.0

      Create QTWO
      Position QTWO in CAVE z=shlq_q0+shlq_q2+shlq_dz2/2.0

      Create QTHR
      Position QTHR in CAVE z=shlq_q0+shlq_q3+shlq_dz3/2.0

*
* -----------------------------------------------------------------------------
Block SHLD is the shield mother volume in the STAR cave 
      Material  Air
      Medium    Standard
      Attribute SHLD   Seen=1  colo=2

      SHAPE     BOX    dx=shlg_dx dy=shlg_dy dz=shlg_dz

      Create    SHBS

      Position  SHBS x=+shlg_dx-shlg_baseDx y=shlg_baseLevel+shlg_baseDy z=0
      Position  SHBS x=-shlg_dx+shlg_baseDx y=shlg_baseLevel+shlg_baseDy z=0

      Yslab = shlg_baseLevel+2.0*shlg_baseDy+shlg_slabDy

      Create    SHLS
      Position  SHLS x= shlg_slabX y=Yslab z=-shlg_dz+shlg_slabDz

      Create    SHBI
      Position  SHBI x=0.0 y=Yslab z=shlg_slabDz

      Yfi = shlg_baseLevel+2.0*shlg_baseDy+2.0*shlg_slabDy+shlg_fiDy

      Create    SHFI
      Position  SHFI x=0.0 y=Yfi z=-shlg_dz+shlg_fiDz

*
EndBlock
* -----------------------------------------------------------------------------
Block SHBS is the shield base
      Attribute SHBS   Seen=1  colo=3

      component Si  Z=14 A=28.08  W=1
      component O2  Z=8  A=16     W=2
      Mixture   ShieldConc  dens=2.5    " PDG: absl=67.4/2.5 radl=10.7
      Medium    Standard

      SHAPE     BOX    dx=shlg_baseDx dy=shlg_baseDy dz=shlg_dz
EndBlock

* -----------------------------------------------------------------------------
Block SHLS is the lateral slab
      Material  ShieldConc
      Attribute SHBS   Seen=1  colo=3

      Shape BOX dx=shlg_dx dy=shlg_slabDy dz=shlg_slabDz
EndBlock
* -----------------------------------------------------------------------------
Block SHBI is the back iron slab
      Material  Iron
      Attribute SHBI   Seen=1  colo=1

      Shape BOX dx=shlg_dx dy=shlg_slabDy dz=shlg_Dz-shlg_slabDz
EndBlock
* -----------------------------------------------------------------------------
Block SHFI is the forward iron slab
      Material  Iron
      Attribute SHFI   Seen=1  colo=1

      Shape BOX dx=shlg_dx dy=shlg_fiDy dz=shlg_fiDz
      Create SHOL
      Position SHOL y=-shlg_fiDy+shlg_hole

EndBlock
* -----------------------------------------------------------------------------
Block SHOL is the hole in the forward iron slab
      Material  Air
      Attribute SHOL   Seen=1  colo=6

      Shape BOX dx=shlg_hole dy=shlg_hole dz=shlg_fiDz
EndBlock
* -----------------------------------------------------------------------------
Block QONE is the Q1 yoke
      Material  Iron
      Attribute QONE   Seen=1  colo=1

      Shape TUBE Rmin=shlq_ri1 Rmax=shlq_ro1 dZ=shlq_dz1/2.0
EndBlock
* -----------------------------------------------------------------------------
Block QTWO is the Q2 yoke
      Material  Iron
      Attribute QTWO   Seen=1  colo=1

      Shape TUBE Rmin=shlq_ri2 Rmax=shlq_ro2 dZ=shlq_dz2/2.0
EndBlock
* -----------------------------------------------------------------------------
Block QTHR is the Q3 yoke
      Material  Iron
      Attribute QTHR   Seen=1  colo=1

      Shape TUBE Rmin=shlq_ri3 Rmax=shlq_ro3 dZ=shlq_dz3/2.0
EndBlock

* -----------------------------------------------------------------------------
     END
*
*

