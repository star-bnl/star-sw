* $Id: istbgeo.g,v 1.3 2004/12/07 00:44:45 potekhin Exp $
* $Log: istbgeo.g,v $
* Revision 1.3  2004/12/07 00:44:45  potekhin
* The prior naming convention led to a clash in the name space,
* which was implicit due to automatic volume enumeration and
* reference counting, under certain conditions. This resulted
* in a crash. I modified the names, and created a new structure
* to describe the attributed of the mother volume in a more
* reasonable way.
*
* Revision 1.2  2004/07/22 22:48:24  potekhin
* Corrected a typo in comment
*
* Revision 1.1  2004/07/15 16:27:50  potekhin
* A properly configured version of the outer pixel
* barrel detector with 3 layers, initial cut
*
******************************************************************************
Module ISTBGEO is the geometry of the outer barrel pixel detector
  Created  07/15/04
  Author   Maxim Potekhin
******************************************************************************
+CDE,AGECOM,GCUNIT.
*
      real angle,anglePos,angleCorr,raddeg
      integer nl,ly,nSector

      Content   IBMO, IBLM, IBAL, IBPL
*

      Structure ISMG {Version, Rin, Rout, TotalLength}

      Structure ISBG {Layer, nLadder, Length,
                      LadderWidth,LadderThk,PassiveThk,ActiveThk,
                      r,a,pOffset,aOffset}
*
* -----------------------------------------------------------------------------
*
   Fill ISMG                   ! Mother volume data
      Version    =  1          ! version
      Rin        =  6.0        ! Inner radius
      Rout       =  20.0       ! Outer radius
      TotalLength=  52.0       ! Overal length of the detector
   EndFill
* -----------------------------------------------------------------------------
*
   Fill ISBG                   ! Pixel detector data
      Layer      =  1          ! layer index
      nLadder    =  11         ! ladder count
      Length     =  28.0       ! Overal length of the detector
*
      LadderWidth=  4.0        ! Ladder Width
      LadderThk  =  0.0120     ! Total ladder Thickness
      PassiveThk =  0.0100     ! Passive silicon Thickness
      ActiveThk  =  0.0020     ! Active  silicon Thickness
*
      r          =  7.0        ! 1st ladder nominal radius
      a          =  0.0        ! 1st ladder nominal position angle
      aOffset    =  81.0       ! Angular offset
      pOffset    =  0.0        ! Position offset (shift)
   EndFill
*
   Fill ISBG                   ! Pixel detector data
      Layer      =  2          ! layer index
      nLadder    =  19         ! ladder count
      Length     =  40.0       ! Overal length of the detector
*
      LadderWidth=  4.0        ! Ladder Width
      LadderThk  =  0.0120     ! Total ladder Thickness
      PassiveThk =  0.0100     ! Passive silicon Thickness
      ActiveThk  =  0.0020     ! Active  silicon Thickness
*
      r          =  12.0       ! 2nd ladder nominal radius
      a          =  0.0        ! 2nd ladder nominal position angle
      aOffset    =  81.0       ! Angular offset
      pOffset    =  0.0        ! Position offset (shift)
   EndFill
*
   Fill ISBG                   ! Pixel detector data
      Layer      =  3          ! layer index
      nLadder    =  27         ! ladder count
      Length     =  52.0       ! Overal length of the detector
*
      LadderWidth=  4.0        ! Ladder Width
      LadderThk  =  0.0120     ! Total ladder Thickness
      PassiveThk =  0.0100     ! Passive silicon Thickness
      ActiveThk  =  0.0020     ! Active  silicon Thickness
*
      r          =  17.0       ! 2nd ladder nominal radius
      a          =  0.0        ! 2nd ladder nominal position angle
      aOffset    =  81.0       ! Angular offset
      pOffset    =  0.0        ! Position offset (shift)
   EndFill
******************************************************

      USE      ISMG
*      USE      ISBG
*
      raddeg=3.14159265/180.0

      Create   IBMO
      Position IBMO in CAVE
* -----------------------------------------------------------------------------
Block IBMO is the mother of the ISTB detector volumes
      Material  Air
      Attribute IBMO  Seen=1  colo=6

      Shape TUBE Rmin=ISMG_Rin Rmax=ISMG_Rout Dz=ISMG_TotalLength/2.0


      do ly=1,3
       USE ISBG Layer=ly
         do nl=1,ISBG_nLadder
           angle=(360.0/ISBG_nLadder)*nl

           anglePos = angle*raddeg

           Create and Position IBLM _
           x=ISBG_r*cos(anglePos) _
           y=ISBG_r*sin(anglePos) _
           z=0.0 _
           AlphaZ=angle-ISBG_aOffset
         enddo
      enddo

endblock
* -----------------------------------------------------------------------------
Block IBLM is the mother of the silicon ladder
      Material  Air
      Attribute IBLM   Seen=1  colo=4
      Shape BOX dX=ISBG_LadderWidth/2.0 dY=ISBG_LadderThk/2.0 Dz=ISBG_Length/2.0

      Create and position IBAL y=-ISBG_LadderThk/2.0+ISBG_ActiveThk/2.0
      Create and position IBPL y=-ISBG_LadderThk/2.0+ISBG_ActiveThk+ISBG_PassiveThk/2.0

endblock
*
* -----------------------------------------------------------------------------
Block IBAL is the active layer of the ladder
      Material  Silicon
      Material  Sensitive  Isvol=1
      Attribute IBAL   Seen=1  colo=4

      Shape BOX dX=ISBG_LadderWidth/2.0 dY=ISBG_ActiveThk/2.0 Dz=ISBG_Length/2.0

      call      GSTPAR (%Imed,'STRA',1.)

      HITS    IBAL   Z:.001:S  Y:.001:   X:.001:     Ptot:16:(0,100),
                     cx:10:    cy:10:    cz:10:      Sleng:16:(0,500),
                     ToF:16:(0,1.e-6)    Step:.01:   Eloss:16:(0,0.001) 
endblock
* -----------------------------------------------------------------------------
Block IBPL is the passive layer of the ladder
      Material  Silicon
      Attribute IBPL   Seen=1  colo=2
      Shape BOX dX=ISBG_LadderWidth/2.0 dY=ISBG_PassiveThk/2.0 Dz=ISBG_Length/2.0
endblock
*

      END

