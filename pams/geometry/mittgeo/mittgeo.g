* $Id: mittgeo.g,v 1.2 2004/06/28 22:49:34 potekhin Exp $
* $Log: mittgeo.g,v $
* Revision 1.2  2004/06/28 22:49:34  potekhin
* First useful cut of the new detector description, with two extra layers
* of silicon ladders in place
*
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
      real angle,anglePos,angleCorr,raddeg
      integer nl,ly,nSector

      Content  MIMO, MSEC, MLMO, MLAC, MLPS
*
      Structure MITG {Layer, nLadder, Rin, Rout, TotalLength,
                      LadderWidth,LadderThk,PassiveThk,ActiveThk,
                      r,a,pOffset,aOffset}
*
* -----------------------------------------------------------------------------
*
   Fill MITG                   ! Pixel detector data
      Layer      =  1          ! layer index
      nLadder    =  30         ! ladder count
      Rin        =  5.65       ! Inner radius
      Rout       =  25.65      ! Outer radius
      TotalLength=  16.0       ! Overal length of the detector
*
      LadderWidth=  2.00       ! Ladder Width
      LadderThk  =  0.0120     ! Total ladder Thickness
      PassiveThk =  0.0100     ! Passive silicon Thickness
      ActiveThk  =  0.0020     ! Active  silicon Thickness
*
      r          =  10.00      ! 1st ladder nominal radius
      a          =  0.0        ! 1st ladder nominal position angle
      aOffset    =  89.28      ! Angular offset
      pOffset    =  0.0        ! Position offset (shift)
   EndFill
*
   Fill MITG                   ! Pixel detector data
      Layer      =  2          ! layer index
      nLadder    =  60         ! ladder count
      Rin        =  5.65       ! Inner radius
      Rout       =  25.65      ! Outer radius
      TotalLength=  16.0       ! Overal length of the detector
*
      LadderWidth=  2.00       ! Ladder Width
      LadderThk  =  0.0120     ! Total ladder Thickness
      PassiveThk =  0.0100     ! Passive silicon Thickness
      ActiveThk  =  0.0020     ! Active  silicon Thickness
*
      r          =  20.00      ! 2nd ladder nominal radius
      a          =  0.0        ! 2nd ladder nominal position angle
      aOffset    =  89.28      ! Angular offset
      pOffset    =  0.0        ! Position offset (shift)
   EndFill
******************************************************

      USE      MITG  
*
      raddeg=3.14159265/180.0

      Create   MIMO
      Position MIMO in CAVE
* -----------------------------------------------------------------------------
Block MIMO is the mother of the MIT detector volumes
      Material  Air
      Attribute MIMO  Seen=1  colo=6

      Shape TUBE Rmin=MITG_Rin Rmax=MITG_Rout Dz=MITG_TotalLength/2.0


      do ly=1,2
      USE MITG Layer=ly
      do nl=1,MITG_nLadder
         angle=(360.0/MITG_nLadder)*nl

         anglePos = angle*raddeg

         Create and Position MLMO x=MITG_r*cos(anglePos) y=MITG_r*sin(anglePos) _
         z=0.0 AlphaZ=angle-80.0
      enddo
      enddo

endblock
* -----------------------------------------------------------------------------
Block MLMO is the mother of the silicon ladder
      Material  Air
      Attribute MLMO   Seen=1  colo=4
      Shape BOX dX=MITG_LadderWidth/2.0 dY=MITG_LadderThk/2.0 Dz=MITG_TotalLength/2.0

      Create and position MLAC y=-MITG_LadderThk/2.0+MITG_ActiveThk/2.0
      Create and position MLPS y=-MITG_LadderThk/2.0+MITG_ActiveThk+MITG_PassiveThk/2.0

endblock
*
* -----------------------------------------------------------------------------
Block MLAC is the active layer of the ladder
      Material  Silicon
      Material  Sensitive  Isvol=1
      Attribute MLAC   Seen=1  colo=4

      Shape BOX dX=MITG_LadderWidth/2.0 dY=MITG_ActiveThk/2.0 Dz=MITG_TotalLength/2.0

      call      GSTPAR (%Imed,'STRA',1.)

      HITS    MLAC   Z:.001:S  Y:.001:   X:.001:     Ptot:16:(0,100),
                     cx:10:    cy:10:    cz:10:      Sleng:16:(0,500),
                     ToF:16:(0,1.e-6)    Step:.01:   Eloss:16:(0,0.001) 
endblock
* -----------------------------------------------------------------------------
Block MLPS is the passive layer of the ladder
      Material  Silicon
      Attribute MLPS   Seen=1  colo=2
      Shape BOX dX=MITG_LadderWidth/2.0 dY=MITG_PassiveThk/2.0 Dz=MITG_TotalLength/2.0
endblock
*

      END

