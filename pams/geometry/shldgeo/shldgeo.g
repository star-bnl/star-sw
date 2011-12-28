* $Id: shldgeo.g,v 1.5 2011/02/28 16:28:59 jwebb Exp $
*
* $Log: shldgeo.g,v $
* Revision 1.5  2011/02/28 16:28:59  jwebb
* Cosmetic changed required for AgML syntax matching.
*
* Revision 1.4  2006/01/18 23:17:59  potekhin
* Flush out small changes in the beam shield geometry
* (very simple at the moment) as we aren't doing further development right now.
*
* Revision 1.3  2005/09/02 18:23:21  potekhin
* Separated magnets into a separate branch, quadgeo.
*
* Revision 1.2  2005/09/02 16:30:30  potekhin
* Waypoint check-in -- before I separate the magnets into a separatefile,
* I want to have this code saved in CVS
*
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
      Content  SHLD, SHBS, SHLS, SHBI, SHFI, SHOL, SFLR
*
      Structure SHLG {  Version ,     Z,
                            dx,      dy,       dz,
                     baseLevel,   baseZ,   baseDx,  baseDy,
                         slabX,   slabZ,   slabDy,  slabDz,
                          fiDz,    fiDy,     holeX,  holeY,
                      floorThk,floorLen,floorWidth, floorPos }


*
*    local variable for section positioning
      Real    Yslab, ShieldHalfHeight, Yfi
*
* -----------------------------------------------------------------------------
*
   Fill SHLG    !  Shielding Geometry Data
      Version   = 1       ! geometry version  
      Z         = 1750    ! position of the shielding
      dx        = 170     ! half-x dimension
      dy        = 150     ! half-y dimension
      dz        = 100     ! half-z dimension

      baseLevel =-125     ! base position on the floor
      baseZ     =  0      ! base position

      baseDx    =  60     ! base half-x dimension
      baseDy    =  30     ! base half-y dimension

      slabX     =   0     ! slab position
      slabZ     = -30     ! slab position

      slabDy    =  30     ! slab half-y dimension
      slabDz    =  30     ! slab half-z dimension

      fiDz      =  47     ! forward iron slab half-thickness
      fiDy      =  55     ! half-height

      holeX     =  20     ! beam hole half-size in X
      holeY     =  10     ! beam hole half-size in Y
      floorThk  =  70     ! Concrete floor thickness
      floorLen  =  3900   ! Concrete floor length
      floorWidth=  340    ! Concrete floor width
      floorPos  =  2800   ! Concrete floor z-position
   EndFill

* = = = = = = = = = = = =

      USE      SHLG

      ShieldHalfHeight=(shlg_baseDy+shlg_slabDy+shlg_fiDy)

      Create   SHLD
      Position SHLD in CAVE x=0.0 y=shlg_baseLevel+ShieldHalfHeight z=+shlg_Z
      Position SHLD in CAVE x=0.0 y=shlg_baseLevel+ShieldHalfHeight z=-shlg_Z thetaZ=180

      Create SFLR
      Position SFLR in CAVE x=0 y=shlg_baseLevel-0.5*shlg_floorThk z=+shlg_floorPos
      Position SFLR in CAVE x=0 y=shlg_baseLevel-0.5*shlg_floorThk z=-shlg_floorPos thetaZ=180

*
* -----------------------------------------------------------------------------
Block SHLD is the shield mother volume in the STAR cave 
      Material  Air
      Medium    Standard
      Attribute SHLD   Seen=1  colo=2

      SHAPE     BOX    dx=shlg_dx dy=ShieldHalfHeight dz=shlg_dz

      Create    SHBS

      Position  SHBS x=+shlg_dx-shlg_baseDx y=-ShieldHalfHeight+shlg_baseDy z=0
      Position  SHBS x=-shlg_dx+shlg_baseDx y=-ShieldHalfHeight+shlg_baseDy z=0

      Yslab = -ShieldHalfHeight+2.0*shlg_baseDy+shlg_slabDy

      Create    SHLS
      Position  SHLS x= shlg_slabX y=Yslab z=-shlg_dz+shlg_slabDz

      Create    SHBI
      Position  SHBI x=0.0 y=Yslab z=shlg_slabDz

      Yfi = -ShieldHalfHeight+2.0*(shlg_baseDy+shlg_slabDy)+shlg_fiDy

      Create    SHFI
      Position  SHFI x=0.0 y=Yfi z=-shlg_dz+shlg_fiDz

*
EndBlock
* -----------------------------------------------------------------------------
Block SFLR is the floor
      Attribute SFLR   Seen=1  colo=3

      component Si  Z=14 A=28.08  W=1
      component O2  Z=8  A=16     W=2
      Mixture   ShieldConc  dens=2.5    " PDG: absl=67.4/2.5 radl=10.7
      Medium    Standard

      Shape BOX dx=shlg_floorWidth/2.0 dy=shlg_floorThk/2.0 dz=shlg_floorLen/2.0
EndBlock
* -----------------------------------------------------------------------------
Block SHBS is the shield base
      Attribute SHBS   Seen=1  colo=3

      component Si  Z=14 A=28.08  W=1
      component O2  Z=8  A=16     W=2
      Mixture   BaseConc  dens=2.5    " PDG: absl=67.4/2.5 radl=10.7
      Medium    Standard

      SHAPE     BOX    dx=shlg_baseDx dy=shlg_baseDy dz=shlg_dz
EndBlock

* -----------------------------------------------------------------------------
Block SHLS is the lateral slab
      Material  BaseConc
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
      Position SHOL y=-shlg_fiDy+shlg_holeY

EndBlock
* -----------------------------------------------------------------------------
Block SHOL is the hole in the forward iron slab
      Material  Air
      Attribute SHOL   Seen=1  colo=6

      Shape BOX dx=shlg_holeX dy=shlg_holeY dz=shlg_fiDz
EndBlock
* -----------------------------------------------------------------------------
     END
*
*

