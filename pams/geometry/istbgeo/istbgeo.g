* $Id: istbgeo.g,v 1.5 2005/01/18 17:38:10 potekhin Exp $
* $Log: istbgeo.g,v $
* Revision 1.5  2005/01/18 17:38:10  potekhin
* Removed the separate Active and Passive layers,
* as the sensor is apparently all active. Tested
* the hits structure.
*
* Revision 1.4  2005/01/06 02:02:23  potekhin
* Large addition to the code along the lines
* of Gerrit's design. Segmented silicon, AlN,
* chtips, new volume names
*
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
      real    angle, anglePos, angleCorr, trueR, raddeg
      integer nl,    ly,       nu

      Content   IBMO, IBLM, IBAM, IBSS, ISTP, ISSC
*

      Structure ISMG {Version, Rin, Rout, TotalLength}

      Structure ISBG {Layer,       nLadder,    nUnit,       Length,
                      LadderWidth, LadderThk,  Spacing,
                      SensorWidth, SensorThk,  SensorLngth, PassiveThk, ActiveThk,
                      r,           a,          pOffset,     aOffset}

      Structure ISAN {Version,  Thk, Length}

      Structure ISCG {Nummer,   W,   H,    Thk}

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
*--------------------------------------------------------
   Fill ISBG                   ! Pixel detector data
      Layer      =  1          ! layer index
      nLadder    =  11         ! ladder count
      nUnit      =  7          ! sensor units per ladder
      Length     =  28.0       ! Overal length of the detector

      LadderWidth=  6.0        ! Ladder Width
      LadderThk  =  0.3700     ! Includes two layers of Si, AlN plates and chips

      Spacing    =  0.1686     ! spacing between the wafers

      SensorWidth=  4.0        ! Sensor Width
      SensorLngth=  4.0        ! Sensor Length
      SensorThk  =  0.0300     ! Sensor Thickness

      r          =  7.0        ! 1st ladder nominal radius
      a          =  0.0        ! 1st ladder nominal position angle
      aOffset    =  81.0       ! Angular offset
      pOffset    =  2.0        ! Position offset (shift)
   EndFill

   Fill ISBG                   ! Pixel detector data
      Layer      =  2          ! layer index
      nLadder    =  19         ! ladder count
      nUnit      =  10         ! sensor units per ladder
      Length     =  40.0       ! Overal length of the detector

      r          =  12.0       ! 2nd ladder nominal radius
      a          =  0.0        ! 2nd ladder nominal position angle
      aOffset    =  81.0       ! Angular offset
      pOffset    =  2.0        ! Position offset (shift)
   EndFill

   Fill ISBG                   ! Pixel detector data
      Layer      =  3          ! layer index
      nLadder    =  27         ! ladder count
      nUnit      =  13         ! sensor units per ladder
      Length     =  52.0       ! Overal length of the detector

      r          =  17.0       ! 2nd ladder nominal radius
      a          =  0.0        ! 2nd ladder nominal position angle
      aOffset    =  81.0       ! Angular offset
      pOffset    =  2.0        ! Position offset (shift)
   EndFill
*--------------------------------------------------------
   Fill ISAN                   ! Aluminum Nitride Thermal Plate
      Version    =   1         ! May have a few
      Thk        =  0.0762     ! AlN Thickness
      Length     =  2.0        ! AlN length
   EndFill

   Fill ISAN                   ! Aluminum Nitride Thermal Plate
      Version    =   2         ! May have a few
      Thk        =  0.0762     ! AlN Thickness
      Length     =  1.0        ! AlN length
   EndFill

*--------------------------------------------------------
   Fill ISCG                   ! Barrel Silicon readout Chip Geometry, first approx.
      Nummer     =   1         ! We can have a few different chips
      W          =   0.5       ! Width
      H          =   0.5       ! Height
      Thk        =   0.07      ! Thickness
   EndFill
******************************************************

      USE      ISMG
*
      raddeg=3.14159265/180.0

      Create   IBMO
      Position IBMO in CAVE
* -----------------------------------------------------------------------------
Block IBMO is the mother of the ISTB detector
      Material  Air
      Attribute IBMO  Seen=1  colo=6

      Shape TUBE Rmin=ISMG_Rin Rmax=ISMG_Rout Dz=ISMG_TotalLength/2.0


      do ly=1,3               ! a loop over layers
         USE ISBG Layer=ly
         do nl=1,ISBG_nLadder ! inner loop over ladders

           angle=(360.0/ISBG_nLadder)*nl

* Individual ladders can be individually tilted by using
* the aOffset parameter (angular offset), and the pOffset
* (position offset), which is the individual lateral
* displacement.

           angleCorr= atan(ISBG_pOffset/ISBG_r)

* The anglePos defines the POSITION of the center of the ladder
* in space, along the lines of x=r*cos(...), y=r*sin(...)
* have to correct and convert to radians:

           anglePos = angle*raddeg - angleCorr ! see above comment
           trueR    = sqrt(ISBG_r**2+ISBG_pOffset**2)

           Create and Position IBAM x=trueR*cos(anglePos) y=trueR*sin(anglePos) _
                                    z=0.0 AlphaZ=angle-ISBG_aOffset

         enddo
      enddo

endblock
* -----------------------------------------------------------------------------
Block IBAM is the mother of the whole long ladder
      Attribute IBAM  Seen=0  colo=6

      Shape BOX dx=ISBG_LadderWidth/2.0 dy=ISBG_LadderThk/2.0 dz=ISBG_Length/2.0

      Create   IBLM

      do nu=1,ISBG_nUnit      
        Position IBLM z=-ISBG_Length/2.0+ISBG_SensorWidth*(nu-0.5)
      enddo

endblock
* -----------------------------------------------------------------------------
Block IBLM is the mother of the sensor assmebly

      Shape BOX dx=ISBG_LadderWidth/2.0 dy=ISBG_LadderThk/2.0 dz=ISBG_SensorLngth/2.0

      Create IBSS

      Position IBSS x=-0.5*(ISBG_LadderWidth-ISBG_SensorWidth) _
                    y=-ISBG_Spacing/2.0+ISBG_SensorThk/2.0 _
                    z=0.0

      Position IBSS x=-0.5*(ISBG_LadderWidth-ISBG_SensorWidth) _
                    y=+ISBG_Spacing/2.0-ISBG_SensorThk/2.0 _
                    z=0.0 AlphaZ=180

      Use ISAN Version=1
      Create and Position ISTP x=0.5*(ISBG_LadderWidth-ISBG_SensorWidth)+ISAN_Length/2.0

      Use ISAN Version=2
      Create   ISTP

      Position ISTP x=0.5*(ISBG_LadderWidth-ISBG_SensorWidth)+ISAN_Length/2.0 _
                               y=ISAN_Thk
      Position ISTP x=0.5*(ISBG_LadderWidth-ISBG_SensorWidth)+ISAN_Length/2.0 _
                               y=-ISAN_Thk

      Create   ISSC

      Position ISSC x= 0.5*(ISBG_LadderWidth-ISBG_SensorWidth)+ISCG_W/2.0 _
                    y=+0.5*ISCG_Thk+1.5*ISAN_Thk _
                    z=0.0

      Position ISSC x= 0.5*(ISBG_LadderWidth-ISBG_SensorWidth)+ISCG_W/2.0 _
                    y=-0.5*ISCG_Thk-1.5*ISAN_Thk _
                    z=0.0

endblock
*
* -----------------------------------------------------------------------------
Block IBSS is the Silicon Sensor
      Material  Silicon
      Material  Sensitive  Isvol=1

      Attribute IBSS  Seen=1  colo=5

      Shape BOX dX=ISBG_SensorWidth/2.0 dY=ISBG_SensorThk/2.0 dz=ISBG_SensorLngth/2.0

      HITS    IBSS   Z:.001:S  Y:.001:   X:.001:     Ptot:16:(0,100),
                     cx:10:    cy:10:    cz:10:      Sleng:16:(0,500),
                     ToF:16:(0,1.e-6)    Step:.01:   Eloss:16:(0,0.001) 

endblock
* -----------------------------------------------------------------------------
Block ISTP is the AlN Thermal Plate
      Attribute ISTP Seen=1  colo=6

      Component Al   A=27  Z=13  W=1
      Component N    A=14  Z=7   W=1
      Mixture   AlN  Dens=3.30

      Shape BOX dx=ISAN_Length/2.0 dy=ISAN_Thk/2.0 dz=ISBG_SensorLngth/2.0

endblock
* -----------------------------------------------------------------------------
Block ISSC is the readout Chip
      Material  Silicon  
      Attribute ISSC  Seen=1  colo=1

      Shape BOX dx=ISCG_W/2.0 dy=ISCG_Thk/2.0 dz=ISBG_SensorLngth/2.0

endblock
* -----------------------------------------------------------------------------


      END
