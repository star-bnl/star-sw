* $Id: pixlgeo.g,v 1.2 2003/10/10 23:09:56 potekhin Exp $
* $Log: pixlgeo.g,v $
* Revision 1.2  2003/10/10 23:09:56  potekhin
* A more functional version with one sixth of all
* ladders properly populated. Still a template for
* more development. Data is structured fro easier
* editing by Kai and others.
*
******************************************************************************
Module PIXLGEO is the geometry of the STAR pixel detector
  Created  10/09/03
  Author   Maxim Potekhin
******************************************************************************
+CDE,AGECOM,GCUNIT.
*
      real angle,raddeg
      integer i

      Content  PXMO, PLAD
*
      Structure PIXG {Ladder, Rin, Rout, TotalLength,
                      LadderWidth,LadderThk,r,a,aOffset}
*
* -----------------------------------------------------------------------------
*
   Fill PIXG              ! Pixel detector data
      Ladder     =  1     ! ladder index
      Rin        =  1.4   ! Inner radius
      Rout       =  5.65  ! Outer radius
      TotalLength=  16.0  ! Overal length of the detector
*
      LadderWidth=  2.00  ! Ladder Width
      LadderThk  =  0.01  ! Ladder Thickness
*
      r          =  5.40  ! 1st ladder radius
      a          =  0.0   ! 1st ladder angle
*
      aOffset    =  0.0   ! Angular offset
   EndFill

   Fill PIXG              ! Pixel detector data
      Ladder     =  2     ! ladder index
      r          =  5.05  ! 2nd ladder radius
      a          =  20.0  ! 2nd ladder angle
   EndFill

   Fill PIXG              ! Pixel detector data
      Ladder     =  3     ! ladder index
      r          =  4.70  ! 3rd ladder radius
      a          =  40.0  ! 3rd ladder angle
   EndFill

   Fill PIXG              ! Pixel detector data
      Ladder     =  4     ! ladder index
      r          =  1.50  ! 4th ladder radius
      a          =  60.0  ! 4th ladder angle
   EndFill


*
*
   USE      PIXG  
*
      Create   PXMO
      Position PXMO in SVTT

* -----------------------------------------------------------------------------
Block PXMO is the mother of the pixel detector volumes
      Material  Air
      Attribute PXMO  Seen=1  colo=6
      Shape TUBE Rmin=PIXG_Rin Rmax=PIXG_Rout Dz=PIXG_TotalLength/2.0


      raddeg=3.14159265/180.0

      do i=1,4
         USE PIXG Ladder=i
         angle=PIXG_aOffset+PIXG_a
         Create and Position PLAD x=PIXG_r*cos(angle*raddeg) y=PIXG_r*sin(angle*raddeg) _
         z=0.0 AlphaZ=90.0+angle
     enddo


endblock
* -----------------------------------------------------------------------------
Block PLAD is the ladder
      Material  Air
      Attribute PLAD   Seen=1  colo=4
      Shape BOX dX=PIXG_LadderWidth/2.0 dY=PIXG_LadderThk/2.0 Dz=PIXG_TotalLength/2.0
endblock
*
      END

