* $Id: pixlgeo.g,v 1.3 2003/10/13 17:15:53 potekhin Exp $
* $Log: pixlgeo.g,v $
* Revision 1.3  2003/10/13 17:15:53  potekhin
* Added more tuning paremters for a more
* precise geometry definition, and a lot
* of documentation to facilitate the
* development by the pixel group members,
* if necessary
*
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
      real angle,anglePos,angleCorr,raddeg
      integer nLadder,nSector

      Content  PXMO, PLAD
*
      Structure PIXG {Ladder, Rin, Rout, TotalLength,
                      LadderWidth,LadderThk,r,a,pOffset,aOffset}
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
      pOffset    =  0.0   ! Position offset (shift)
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
      pOffset    =  -0.4  ! Position offset (shift, cm)
      aOffset    =  25.0  ! Angular offset  (degrees)
   EndFill

******************************************************
      USE      PIXG  
*
      raddeg=3.14159265/180.0

      Create   PXMO
      Position PXMO in SVTT
* -----------------------------------------------------------------------------
Block PXMO is the mother of the pixel detector volumes
      Material  Air
      Attribute PXMO  Seen=1  colo=6
      Shape TUBE Rmin=PIXG_Rin Rmax=PIXG_Rout Dz=PIXG_TotalLength/2.0

* The sector is defined as a group of 4 ladders, we
* have a total of 6 sectors placed with Phi rotational symmetry

      do nSector=1,6 ! Outer loop, populate sectors
       do nLadder=1,4 ! Inner loop, create ladders

         USE PIXG Ladder=nLadder ! simple loop over ladders, and their data structures

* The anglePos defines the POSITION of the center of the ladder
* in space, along the lines of x=r*cos(...), y=r*sin(...)

* Individual ladders can be individually tuned by using
* the aOffset parameter (angular offset), and the pOffset
* (position offset), which is the individual lateral
* displacement. What follows is trivial arithmetic:

         angle    = PIXG_aOffset + PIXG_a + 60.0*(nSector-1)

* Above, we hardcode the 60 degree rotation angle
* between adjacent sectors, this may stay or be replaced later

         angleCorr= atan(PIXG_pOffset/PIXG_r)  ! extra lateral correction

* have to correct and convert to radians:
         anglePos = angle*raddeg+angleCorr

         Create and Position PLAD x=PIXG_r*cos(anglePos) y=PIXG_r*sin(anglePos) _
         z=0.0 AlphaZ=90.0+angle
* Note that in the previous line, the rotation of the ladder around the
* Z axis does not include the lateral correction, by design
       enddo
      enddo


endblock
* -----------------------------------------------------------------------------
Block PLAD is the ladder
      Material  Silicon
      Attribute PLAD   Seen=1  colo=4
      Shape BOX dX=PIXG_LadderWidth/2.0 dY=PIXG_LadderThk/2.0 Dz=PIXG_TotalLength/2.0
endblock
*
      END

