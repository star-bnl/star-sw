* $Id: mittgeo.g,v 1.1 2004/06/28 20:49:35 potekhin Exp $
* $Log: mittgeo.g,v $
* Revision 1.1  2004/06/28 20:49:35  potekhin
* The first (dummy) version of the MIT tracker, dubbed MITT,
* based on pixel ladders.
*
******************************************************************************
Module MITTGEO is the geometry of the outer pixel detector
  Created  06/28/04
  Author   Maxim Potekhin
******************************************************************************
+CDE,AGECOM,GCUNIT.
*
      real angle,anglePos,angleCorr,raddeg, cylrad, cyllen, cylthk
      integer nLadder,nSector, nExtraLadder

      Content  PXMO, PSEC, PLMO, PLAC, PLPS
*
      Structure PIXG {Ladder, Rin, Rout, Rout1, TotalLength,
                      LadderWidth,LadderThk,PassiveThk,ActiveThk,
                      r,a,pOffset,aOffset}
*
* -----------------------------------------------------------------------------
*
   Fill PIXG                   ! Pixel detector data
      Ladder     =  1          ! ladder index
      Rin        =  1.45       ! Inner radius
      Rout       =  5.65       ! Outer radius
      Rout1      =  25.65      ! Outer radius
      TotalLength=  16.0       ! Overal length of the detector
*
      LadderWidth=  2.00       ! Ladder Width
      LadderThk  =  0.0120     ! Total ladder Thickness
      PassiveThk =  0.0100     ! Passive silicon Thickness
      ActiveThk  =  0.0020     ! Active  silicon Thickness
*
      r          =  5.294      ! 1st ladder nominal radius
      a          =  0.0        ! 1st ladder nominal position angle
      aOffset    =  89.28      ! Angular offset
*
      pOffset    =  0.0        ! Position offset (shift)
   EndFill

   Fill PIXG                   ! Pixel detector data
      Ladder     =  2          ! ladder index
      r          =  4.862      ! 2nd ladder nominal radius
      a          =  20.27      ! 2nd ladder nominal position angle
      aOffset    =  88.31      ! Angular offset
   EndFill

   Fill PIXG                   ! Pixel detector data
      Ladder     =  3          ! ladder index
      r          =  4.391      ! 3rd ladder radius
      a          =  42.62      ! 3rd ladder nominal position angle
      aOffset    =  87.01      ! Angular offset
   EndFill

   Fill PIXG                   ! Pixel detector data
      Ladder     =  4          ! ladder index
      r          =  1.595      ! 4th ladder nominal radius
      a          =  79.51      ! 4th ladder nominal position angle
      aOffset    =  70.15      ! Angular offset
   EndFill

******************************************************
      USE      PIXG  
*
      raddeg=3.14159265/180.0

      Create   PXMO
      Position PXMO in CAVE
*      Position PXMO in SVTT
* -----------------------------------------------------------------------------
Block PXMO is the mother of the pixel detector volumes
      Material  Air
      Attribute PXMO  Seen=1  colo=6

      Shape TUBE Rmin=PIXG_Rin Rmax=PIXG_Rout1 Dz=PIXG_TotalLength/2.0

* The "sector" is defined as a group of 4 ladders, we
* have a total of 6 overlapping sectors placed with rotational symmetry

      Create PSEC
      do nSector=1,6 ! need the "MANY" option as they do overlap
          Position PSEC AlphaZ=60.0*(nSector-1) konly='MANY'
      enddo

      cylthk=0.01
      cylrad=10.0
      cyllen=16.0

      nExtraLadder=30
      do nLadder=1,nExtraladder
         angle=(360.0/nExtraLadder)*nLadder

         anglePos = angle*raddeg

         Create and Position PLMO x=cylrad*cos(anglePos) y=cylrad*sin(anglePos) _
         z=0.0 AlphaZ=angle-80.0
       enddo


*      Create and Position PLAY
*      cylrad=20.0
*      Create and Position PLAY

endblock
* -----------------------------------------------------------------------------
Block PSEC is a group of ladders
      Material  Air
      Attribute PSEC   Seen=1  colo=5
      Shape TUBS  Rmin=PIXG_Rin Rmax=PIXG_Rout Dz=PIXG_TotalLength/2.0 Phi1=-11.0 Phi2=118.0


       do nLadder=1,4 ! Inner loop, create ladders inside the sector

         USE PIXG Ladder=nLadder ! index the ladder data structures
         angle = PIXG_a

* Individual ladders can be individually tilted by using
* the aOffset parameter (angular offset), and the pOffset
* (position offset), which is the individual lateral
* displacement. (Optional and maybe obsoleted soon: angleCorr= atan(PIXG_pOffset/PIXG_r))

* The anglePos defines the POSITION of the center of the ladder
* in space, along the lines of x=r*cos(...), y=r*sin(...)
* have to correct and convert to radians:

         anglePos = angle*raddeg               !  +angleCorr  see above cpmment

         Create and Position PLMO x=PIXG_r*cos(anglePos) y=PIXG_r*sin(anglePos) _
         z=0.0 AlphaZ=-PIXG_aOffset+angle

       enddo

endblock
*
* -----------------------------------------------------------------------------
Block PLMO is the mother of the silicon ladder
      Material  Air
      Attribute PLMO   Seen=1  colo=4
      Shape BOX dX=PIXG_LadderWidth/2.0 dY=PIXG_LadderThk/2.0 Dz=PIXG_TotalLength/2.0

      Create and position PLAC y=-PIXG_LadderThk/2.0+PIXG_ActiveThk/2.0
      Create and position PLPS y=-PIXG_LadderThk/2.0+PIXG_ActiveThk+PIXG_PassiveThk/2.0

endblock
*
* -----------------------------------------------------------------------------
Block PLAC is the active layer of the ladder
      Material  Silicon
      Material  Sensitive  Isvol=1
      Attribute PLAC   Seen=1  colo=4

      Shape BOX dX=PIXG_LadderWidth/2.0 dY=PIXG_ActiveThk/2.0 Dz=PIXG_TotalLength/2.0

      call      GSTPAR (%Imed,'STRA',1.)

      HITS    PLAC   Z:.001:S  Y:.001:   X:.001:     Ptot:16:(0,100),
                     cx:10:    cy:10:    cz:10:      Sleng:16:(0,500),
                     ToF:16:(0,1.e-6)    Step:.01:   Eloss:16:(0,0.001) 
endblock
* -----------------------------------------------------------------------------
Block PLPS is the passive layer of the ladder
      Material  Silicon
      Attribute PLPS   Seen=1  colo=2
      Shape BOX dX=PIXG_LadderWidth/2.0 dY=PIXG_PassiveThk/2.0 Dz=PIXG_TotalLength/2.0
endblock
*
* -----------------------------------------------------------------------------
*Block PLAY is a cylindrical layer
*      Material  Silicon
*      Material  Sensitive  Isvol=1
*      Attribute PLAY   Seen=1  colo=4
*
*      Shape TUBE Rmin=cylrad Rmax=cylrad+cylthk Dz=cyllen/2.0

*      HITS    PLAY   X:.01:S  Y:.01:   Z:.01:     Ptot:16:(0,100),
*                     cx:10:    cy:10:    cz:10:      Sleng:16:(0,500),
*                     ToF:16:(0,1.e-6)    Step:.01:   Eloss:16:(0,0.001) 

*endblock

      END

