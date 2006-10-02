* $Id: istbgeo1.g,v 1.1 2006/10/02 21:50:18 potekhin Exp $
* $Log: istbgeo1.g,v $
* Revision 1.1  2006/10/02 21:50:18  potekhin
* A dignificantly updated version by Gerrit
*
*****************************************************************
Module ISTBGEO1 is the geometry of the Inner Silicon Tracker
  Created  10/02/06
  Author   Gerrit van Nieuwenhuizen
*****************************************************************
+CDE,AGECOM,GCUNIT.
* ---
      real    angle, anglePos, angleCorr, trueR, raddeg, Rlad
      integer nl,    ly,       nu,     cable
      REAL Mod_Thk

* ---
      Content   IBMO,IBLM,IBAM,IBSS,ISTP,ISKH,ISCL,ISSC,ISLA,ISLB,ISRI,ISCO,
                IBMY,IBMZ,IBMW,IBME

      Structure ISMG {Layer, Rin,            Rout,        TotalLength}
      Structure ISBG {Layer,   nLadder,        nUnit,       Length,
                      LadderWidth, LadderThk,  SensAThk,    Spacing,
                      SensorWidth, SensorThk,  SensorLngth,
                      r,a,         pParOffset, pPerOffset,  aOffset}
      Structure ISAN {Version,  W,H, Thk}
      Structure ISHY {Version,  W,H, Thk}
      Structure ISCU {Version,  W,H, Thk}
      Structure ISCG {Version,  W,H, Thk}
      Structure ISGA {Version,  W,H, Thk}
      Structure ISGB {Version,  W,H, Thk}
      Structure ISRR {Layer,    Rin, Rout, Length}
      Structure ISCC {Version,  Rin1, Rout1, Rin2, Rout2, Length}

* -------------------------------------------------------
   Fill ISMG                   ! Mother volume whole detector
      Layer      =  1          ! version
      Rin        =  11.00      ! Inner radius
      Rout       =  18.00      ! Outer radius
      TotalLength=  56.00      ! Overal length of the detector
   EndFill

   Fill ISMG                   ! Mother volume inner layer
      Layer      =  2          ! version
      Rin        =  11.00      ! Inner radius
      Rout       =  14.00      ! Outer radius
      TotalLength=  44.00      ! Overal length of the layer
   EndFill

   Fill ISMG                   ! Mother volume outer layer
      Layer      =  3          ! version
      Rin        =  15.00      ! Inner radius
      Rout       =  18.00      ! Outer radius
      TotalLength=  56.00      ! Overal length of the layer
   EndFill

*--------------------------------------------------------
   Fill ISBG                   ! Inner silicon tracker data
      Layer      =  1          ! layer index, layer 1 does not exist anymore
      nLadder    =  11         ! ladder count				
      nUnit      =  7          ! sensor units per ladder		
      Length     =  28.0       ! Overal length of the detector		
      r          =  7.0        ! 1st ladder nominal radius		
      a          =  0.0        ! 1st ladder nominal position angle
      aOffset    =  81.0       ! Angular offset				
      pParOffset =  0.0        ! Position offset parallel to the length of the module
      pPerOffset =  0.0        ! Position offset perpendicular to the length of the module
      LadderWidth=  4.0        ! Ladder Width
      LadderThk  =  0.4626     ! Ladder Thickness
      SensAThk  =   0.4282     ! Module Thickness
      SensorWidth=  4.0        ! Sensor Width 				
      SensorLngth=  4.0        ! Sensor Length 				
      SensorThk  =  0.0300     ! Sensor Thickness 			
   EndFill

   Fill ISBG                   ! Inner silicon tracker data
      Layer      =  2          ! layer index				
      nLadder    =  19         ! ladder count				
      nUnit      =  10         ! sensor units per ladder		
      Length     =  40.0       ! Overal length of the detector		
      r          =  12.0       ! 2nd ladder nominal radius		
      a          =  0.0        ! 2nd ladder nominal position angle	
      aOffset    =  81.0       ! Angular offset				
      pParOffset =  0.0        ! Position offset parallel to the length of the module
      pPerOffset =  0.0        ! Position offset perpendicular to the length of the module
   EndFill

   Fill ISBG                   ! Inner silicon tracker data
      Layer      =  3          ! layer index				
      nLadder    =  27         ! ladder count				
      nUnit      =  13         ! sensor units per ladder		
      Length     =  52.0       ! Overal length of the detector		
      r          =  17.0       ! 2nd ladder nominal radius		
      a          =  0.0        ! 2nd ladder nominal position angle	
      aOffset    =  81.0       ! Angular offset				
      pParOffset =  0.0        ! Position offset parallel to the length of the module
      pPerOffset =  0.0        ! Position offset perpendicular to the length of the module
   EndFill

*--------------------------------------------------------
   Fill ISAN                   ! Thermal conductive carbon foam plate
      Version    =  1          ! May have a few
      W          =  4.0        ! Width
      H          =  4.0        ! Length			
      Thk        =  0.1686     ! Thickness
   EndFill

*--------------------------------------------------------
   Fill ISHY                   ! Kapton hybrid
      Version    =  1          ! May have a few
      W          =  4.0        ! Width
      H          =  4.0        ! Length			
      Thk        =  0.0050     ! Thickness, 1mil Kapton, 1/2mil glue, 1/2mil kapton = 2mil kapton
   EndFill

*--------------------------------------------------------
   Fill ISCU                   ! Copper traces on hybrid
      Version    =  1          ! May have a few
      W          =  4.0        ! Width
      H          =  4.0        ! Length			
      Thk        =  0.0018     ! Thickness, 0.7mil Cu
   EndFill

*--------------------------------------------------------
   Fill ISCG                   ! Readout Chip Geometry, first approx.
      Version    =   1         ! We may have a few different chips
      W          =   0.8055    ! Width
      H          =   4.0       ! Height (height chip = 7100um, just make strip)
      Thk        =   0.0762    ! Thickness
   EndFill

*--------------------------------------------------------
   Fill ISGA                   ! Glue layer, same size as sensor
      Version    =  1          ! May have a few
      W          =  4.0        ! Width
      H          =  4.0        ! Length			
      Thk        =  0.0050     ! Thickness, use 50um
   EndFill

*--------------------------------------------------------
   Fill ISGB                   ! Glue layer, same size as chip
      Version    =  1          ! May have a few
      W          =  0.8055     ! Width
      H          =  4.0        ! Length			
      Thk        =  0.0050     ! Thickness, use 50um
   EndFill
* -------------------------------------------------------
   Fill ISRR                   ! Support ring for layer 1
      Layer      =   1          ! Layer  
      Rin        =   6.00      ! Inner radius
      Rout       =   9.00      ! Outer radius
      Length     =   2.00      ! Thickness of the ring
   EndFill

   Fill ISRR                   ! Support ring for layer 2
      Layer      =  2          ! Layer  
      Rin        =  11.00      ! Inner radius
      Rout       =  14.00      ! Outer radius
      Length     =   2.00      ! Thickness of the ring
   EndFill

   Fill ISRR                   ! Support ring for layer 3
      Layer      =  3          ! Layer  
      Rin        =  15.00      ! Inner radius
      Rout       =  18.00      ! Outer radius
      Length     =   2.00      ! Thickness of the ring
   EndFill
* -------------------------------------------------------
   Fill ISCC                   ! Interconnect between support rings 2 and 3
      Version    =   1         ! Version
      Rin1       =  13.00      ! Inner radius small
      Rout1      =  14.00      ! Outer radius small
      Rin2       =  15.00      ! Inner radius large
      Rout2      =  16.00      ! Outer radius large
      Length     =   4.00      ! Thickness of the ring
   EndFill
                                                                                                                                                                                             

* -------------------------------------------------------
      raddeg = 3.14159265/180.0
* -------------------------------------------------------
      USE ISMG Layer=1
      Create   IBMO
      Position IBMO in CAVE
* -------------------------------------------------------
Block IBMO is the mother of the ISTB detector
      Material  Air
      Attribute IBMO  Seen=0  colo=2

      Shape TUBE Rmin = ISMG_Rin             _
                 Rmax = ISMG_Rout            _
                 Dz   = ISMG_TotalLength/2.0

*     Inner IST layer
      USE ISMG Layer=2
      Create   IBMY
      Position IBMY x=0.0 y=0.0 z=0.0

*     Outer IST layer
      USE ISMG Layer=3
      Create   IBMZ
      Position IBMZ x=0.0 y=0.0 z=0.0

*     place the interconnects, east and west side
      USE ISBG Layer=2
      USE ISRR Layer=2
      USE ISCC Version=1
      Create   IBME
      Position IBME x =  0.0 _
                    y =  0.0 _
                    z =  1.0*(ISBG_Length/2.0+ISRR_Length+ISCC_Length/2.0)
      Create   IBMW
      Position IBMW x =  0.0 _
                    y =  0.0 _
                    z = -1.0*(ISBG_Length/2.0+ISRR_Length+ISCC_Length/2.0)

endblock

* -------------------------------------------------------
Block IBMY is the mother of the inner IST layer
     Material  Air
     Attribute IBMY  Seen=0  colo=2

     Shape TUBE Rmin = ISMG_Rin             _
                Rmax = ISMG_Rout            _
                Dz   = ISMG_TotalLength/2.0

* -- Some administration to calculate the thickness of a module
* -- Kept for (private) future use
     USE ISAN Version=1
     USE ISGA Version=1
     USE ISBG Layer=1
     USE ISGA Version=1
     USE ISHY Version=1
     USE ISCU Version=1
     USE ISGB Version=1
     USE ISCG Version=1
     Mod_Thk = 2.0*(ISAN_Thk/2.0+ISGA_Thk+ISBG_SensorThk+ISGA_Thk+ISHY_Thk+ _
                    ISCU_Thk+ISGB_Thk+ISCG_Thk)

     USE ISBG Layer=2
     write(*,*) '===>GEOINFO/istbgeo: Creating IST layer', ISBG_Layer
     do nl=1,ISBG_nLadder ! inner loop over ladders (which consist of sensors)

        angle = (360.0/ISBG_nLadder)*nl  ! Base tilt, to be further corrected

*       GvN Offset perpendicular to the length of the module/ladder
*       this to move the midpoint of the sensors back on the required
*       radius after the tilting
        Rlad = ISBG_r+ISBG_pPerOffset

*       Individual ladders can be individually tilted by using
*       the aOffset parameter (angular offset), and the pOffset
*       (position offset), which is the individual lateral
*       displacement.

        angleCorr = atan(ISBG_pParOffset/Rlad)

*       The anglePos defines the POSITION of the center of the ladder
*       in space, along the lines of x=r*cos(...), y=r*sin(...)
*       have to correct and convert to radians:

        anglePos = angle*raddeg - angleCorr       ! see above comment
        trueR    = sqrt(Rlad**2+ISBG_pParOffset**2)

        Create   IBAM
        Position IBAM x = trueR*cos(anglePos) _
                      y = trueR*sin(anglePos) _
                      z = 0.0                 _
                      AlphaZ = angle-ISBG_aOffset

      enddo

*     place 2 support rings at both ends of the ladders
      USE ISRR Layer=2
      Create   ISRI
      Position ISRI x =  0.0 _
                    y =  0.0 _
                    z =  1.0*(ISBG_Length/2.0+ISRR_Length/2.0)
      Position ISRI x =  0.0 _
                    y =  0.0 _
                    z = -1.0*(ISBG_Length/2.0+ISRR_Length/2.0)

endblock

* -------------------------------------------------------
Block IBMZ is the mother of the outer IST layer
     Material  Air
     Attribute IBMZ  Seen=0  colo=2

     Shape TUBE Rmin = ISMG_Rin             _
                Rmax = ISMG_Rout            _
                Dz   = ISMG_TotalLength/2.0

     USE ISBG Layer=3
     write(*,*) '===>GEOINFO/istbgeo: Creating IST layer', ISBG_Layer
     do nl=1,ISBG_nLadder ! inner loop over ladders (which consist of sensors)

        angle = (360.0/ISBG_nLadder)*nl  ! Base tilt, to be further corrected

*       GvN Offset perpendicular to the length of the module/ladder
*       this to move the midpoint of the sensors back on the required
*       radius after the tilting
        Rlad = ISBG_r+ISBG_pPerOffset

*       Individual ladders can be individually tilted by using
*       the aOffset parameter (angular offset), and the pOffset
*       (position offset), which is the individual lateral
*       displacement.

        angleCorr = atan(ISBG_pParOffset/Rlad)

*       The anglePos defines the POSITION of the center of the ladder
*       in space, along the lines of x=r*cos(...), y=r*sin(...)
*       have to correct and convert to radians:

        anglePos = angle*raddeg - angleCorr       ! see above comment
        trueR    = sqrt(Rlad**2+ISBG_pParOffset**2)

        Create   IBAM
        Position IBAM x = trueR*cos(anglePos) _
                      y = trueR*sin(anglePos) _
                      z = 0.0                 _
                      AlphaZ = angle-ISBG_aOffset

      enddo

*     place 2 support rings at both ends of the ladders
      USE ISRR Layer=3
      Create   ISRI
      Position ISRI x =  0.0 _
                    y =  0.0 _
                    z =  1.0*(ISBG_Length/2.0+ISRR_Length/2.0)
      Position ISRI x =  0.0 _
                    y =  0.0 _
                    z = -1.0*(ISBG_Length/2.0+ISRR_Length/2.0)

endblock

* -----------------------------------------------------------------------------
Block IBME is the mother of the interconnect of the IST on the WEST side
      Material Air
      Attribute IBME seen=0 colo=2

      Shape CONE Rmn1 = ISCC_Rin1       _
                 Rmx1 = ISCC_Rout1      _
                 Rmn2 = ISCC_Rin2       _
                 Rmx2 = ISCC_Rout2      _
                 Dz   = ISCC_Length/2.0

      USE ISBG Layer=2
      USE ISRR Layer=2
      USE ISCC Version=1
      Create   ISCO
      Position ISCO x =  0.0 _
                    y =  0.0 _
                    z =  0.0

endblock

* -----------------------------------------------------------------------------
Block IBMW is the mother of the interconnect of the IST on the EAST side
      Material Air
      Attribute IBMW seen=0 colo=2
      Shape CONE Rmn1 = ISCC_Rin2       _
                 Rmx1 = ISCC_Rout2      _
                 Rmn2 = ISCC_Rin1       _
                 Rmx2 = ISCC_Rout1      _
                 Dz   = ISCC_Length/2.0

      USE ISBG Layer=2
      USE ISRR Layer=2
      USE ISCC Version=1
      Create   ISCO
      Position ISCO x =  0.0 _
                    y =  0.0 _
                    z =  0.0 _
                    AlphaX = 180

endblock

* -----------------------------------------------------------------------------
Block IBAM is the mother of the whole long ladder
      Attribute IBAM  Seen=0  colo=2

      Shape BOX dx = ISBG_LadderWidth/2.0 _
                dy = ISBG_LadderThk/2.0   _
                dz = ISBG_Length/2.0

     USE ISHY Version=1
     Create ISKH
     Create ISCL
     Create IBLM
      do nu=1,ISBG_nUnit      
* --    IST modules
        Position IBLM x = 0.0 _
                      y = 0.0 _
                      z = -ISBG_Length/2.0+ISBG_SensorWidth*(nu-0.5)
* --    Dummy readout cables (Kapton+Copper) stacked on top of modules
* --    Note that the first part of the cable (the hybrid) is already part of the module
* --    daisy chain cables as: 5 sensors + 5 sensors             (for layer 2)
* --                           5 sensors + 5 sensors + 3 sensors (for layer 3)
        if (nu>5) then
          Position ISKH x =  0.0 _
                        y =  1.0*(Mod_Thk/2.0+ISHY_Thk/2.0) _
                        z = -ISBG_Length/2.0+ISBG_SensorWidth*(nu-0.5)
          Position ISCL x =  0.0 _
                        y =  1.0*(Mod_Thk/2.0+ISHY_Thk+ISCU_Thk/2.0) _
                        z = -ISBG_Length/2.0+ISBG_SensorWidth*(nu-0.5)
          Position ISKH x =  0.0 _
                        y = -1.0*(Mod_Thk/2.0+ISHY_Thk/2.0) _
                        z = -ISBG_Length/2.0+ISBG_SensorWidth*(nu-0.5)
          Position ISCL x =  0.0 _
                        y = -1.0*(Mod_Thk/2.0+ISHY_Thk+ISCU_Thk/2.0) _
                        z = -ISBG_Length/2.0+ISBG_SensorWidth*(nu-0.5)
        endif
        if (nu>10) then
          Position ISKH x =  0.0 _
                        y =  1.0*(Mod_Thk/2.0+ISHY_Thk+ISCU_Thk+ISHY_Thk/2.0) _
                        z = -ISBG_Length/2.0+ISBG_SensorWidth*(nu-0.5)
          Position ISCL x =  0.0 _
                        y =  1.0*(Mod_Thk/2.0+ISHY_Thk+ISCU_Thk+ISHY_Thk+ISCU_Thk/2.0) _
                        z = -ISBG_Length/2.0+ISBG_SensorWidth*(nu-0.5)
          Position ISKH x =  0.0 _
                        y = -1.0*(Mod_Thk/2.0+ISHY_Thk+ISCU_Thk+ISHY_Thk/2.0) _
                        z = -ISBG_Length/2.0+ISBG_SensorWidth*(nu-0.5)
          Position ISCL x =  0.0 _
                        y = -1.0*(Mod_Thk/2.0+ISHY_Thk+ISCU_Thk+ISHY_Thk+ISCU_Thk/2.0) _
                        z = -ISBG_Length/2.0+ISBG_SensorWidth*(nu-0.5)
        endif

      enddo

endblock

* -----------------------------------------------------------------------------
Block IBLM is the mother of the sensor assembly
      Attribute IBLM  Seen=0  colo=2

      Shape BOX dx = ISBG_LadderWidth/2.0 _
                dy = ISBG_SensAThk/2.0    _
                dz = ISBG_SensorLngth/2.0

* -- Start stacking all the parts of the module
* -- Note that the module are mirror symmetric wrt the x-z plane
* -- with exception of the readout chips (minimize average material!)

* -- Carbon foam
      Use ISAN Version = 1
      Create ISTP
      Position ISTP x =  0.0 _
                    y =  0.0 _
                    z =  0.0

* -- Glue layers between foam and silicon
      Use ISGA Version = 1
      Create ISLA
      Position ISLA x =  0.0                           _
                    y =  1.0*ISAN_Thk/2.0+ISGA_Thk/2.0 _
                    z =  0.0
      Position ISLA x =  0.0                           _
                    y = -1.0*ISAN_Thk/2.0-ISGA_Thk/2.0 _
                    z =  0.0

* -- Silicon Sensors
      Create   IBSS
      Position IBSS x =  0.0                                          _
                    y =  1.0*ISAN_Thk/2.0+ISGA_Thk+ISBG_SensorThk/2.0 _
                    z =  0.0
      Position IBSS x =  0.0                                          _
                    y = -1.0*ISAN_Thk/2.0-ISGA_Thk-ISBG_SensorThk/2.0 _
                    z =  0.0                                          _
                    AlphaZ = 180

* -- Glue layers between silicon and hybrid
      Use ISGA Version = 1
      Create ISLA
      Position ISLA x =  0.0                                                   _
                    y =  1.0*ISAN_Thk/2.0+ISGA_Thk+ISBG_SensorThk+ISGA_Thk/2.0 _
                    z =  0.0
      Position ISLA x =  0.0                                                   _
                    y = -1.0*ISAN_Thk/2.0-ISGA_Thk-ISBG_SensorThk-ISGA_Thk/2.0 _
                    z =  0.0

* -- Kapton hybrids
      Use ISHY Version = 1
      Create   ISKH
      Position ISKH x =  0.0                                                            _
                    y =  1.0*ISAN_Thk/2.0+ISGA_Thk+ISBG_SensorThk+ISGA_Thk+ISHY_Thk/2.0 _
                    z =  0.0
      Position ISKH x =  0.0                                                            _
                    y = -1.0*ISAN_Thk/2.0-ISGA_Thk-ISBG_SensorThk-ISGA_Thk-ISHY_Thk/2.0 _
                    z =  0.0

* -- Copper on the hybrid, 10 um total for couple of 5um lines at 50% coverage + ground plane
      Use ISCU Version = 1
      Create   ISCL
      Position ISCL x =  0.0                                                                     _
                    y =  1.0*ISAN_Thk/2.0+ISGA_Thk+ISBG_SensorThk+ISGA_Thk+ISHY_Thk+ISCU_Thk/2.0 _
                    z =  0.0
      Position ISCL x =  0.0                                                                     _
                    y = -1.0*ISAN_Thk/2.0-ISGA_Thk-ISBG_SensorThk-ISGA_Thk-ISHY_Thk-ISCU_Thk/2.0 _
                    z =  0.0

* -- Glue layers between hybrid and chip
      Use ISGB Version = 1
      Create   ISLB
      Position ISLB x =  1.0*ISBG_SensorWidth/2.0-ISCG_W/2.0-0.2                                          _
                    y =  1.0*ISAN_Thk/2.0+ISGA_Thk+ISBG_SensorThk+ISGA_Thk+ISHY_Thk+ISCU_Thk+ISGB_Thk/2.0 _
                    z =  0.0
      Position ISLB x = -1.0*ISBG_SensorWidth/2.0+ISCG_W/2.0+0.2                                          _
                    y = -1.0*ISAN_Thk/2.0-ISGA_Thk-ISBG_SensorThk-ISGA_Thk-ISHY_Thk-ISCU_Thk-ISGB_Thk/2.0 _
                    z =  0.0

* -- Readout chips, put at 2mm from the edge of the sensor
      Use ISCG Version = 1
      Create   ISSC
      Position ISSC x =  1.0*ISBG_SensorWidth/2.0-ISCG_W/2.0-0.2                                                   _
                    y =  1.0*ISAN_Thk/2.0+ISGA_Thk+ISBG_SensorThk+ISGA_Thk+ISHY_Thk+ISCU_Thk+ISGB_Thk+ISCG_Thk/2.0 _
                    z =  0.0
      Position ISSC x = -1.0*ISBG_SensorWidth/2.0+ISCG_W/2.0+0.2                                                   _
                    y = -1.0*ISAN_Thk/2.0-ISGA_Thk-ISBG_SensorThk-ISGA_Thk-ISHY_Thk-ISCU_Thk-ISGB_Thk-ISCG_Thk/2.0 _
                    z =  0.0
endblock

* -----------------------------------------------------------------------------
Block IBSS is the Silicon Sensor
      Material  Silicon
      Material  Sensitive  Isvol=1

      Attribute IBSS  Seen=1  colo=38

      Shape BOX dX = ISBG_SensorWidth/2.0 _
                dY = ISBG_SensorThk/2.0   _
                dz = ISBG_SensorLngth/2.0

      HITS    IBSS   Z:.001:S  Y:.001:   X:.001:     Ptot:16:(0,100),
                     cx:10:    cy:10:    cz:10:      Sleng:16:(0,500),
                     ToF:16:(0,1.e-6)    Step:.01:   Eloss:16:(0,0.001) 

endblock

* -----------------------------------------------------------------------------
Block ISKH is the Kapton hybrid
      Attribute ISKH Seen=1  colo=42

      ! Kapton   C22-H10-N2-O4
      Component C   A=12   Z=6   W=22.0*12.0/366.0
      Component H   A=1    Z=1   W=10.0*1.0/366.0
      Component N   A=14   Z=7   W=2.0*14.0/366.0
      Component O   A=16   Z=8   W=4.0*16.0/366.0
      Mixture   Kapton   Dens=1.42

      Shape BOX dx = ISHY_H/2.0   _
                dy = ISHY_Thk/2.0 _
                dz = ISHY_W/2.0

endblock

* -----------------------------------------------------------------------------
Block ISCL is the Copper layer on the Kapton hybrid
      Attribute ISCL Seen=1  colo=46

      Material Copper

      Shape BOX dx = ISCU_H/2.0   _
                dy = ISCU_Thk/2.0 _
                dz = ISCU_W/2.0

endblock

* -----------------------------------------------------------------------------
Block ISTP is the Carbon Foam
      Attribute ISTP Seen=1  colo=13

      ! Carbon foam density 0.24
      ! 0.1 comes from 4-5% of Carbon density for Carbon foam
      ! 0.24 is when we articially increase the density to squeeze into 1.686 mm
      !       between the sensors
      Component C   A=12   Z=6   W=0.05
      Component N   A=14   Z=7   W=0.95
      Mixture   CFoam   Dens=0.24

      Shape BOX dx = ISAN_H/2.0   _
                dy = ISAN_Thk/2.0 _
                dz = ISAN_W/2.0

endblock

* -----------------------------------------------------------------------------
Block ISSC is the readout Chip
      Attribute ISSC  Seen=1  colo=37

      Material  Silicon  

      Shape BOX dx = ISCG_W/2.0   _
                dy = ISCG_Thk/2.0 _
                dz = ISCG_H/2.0

endblock

* -----------------------------------------------------------------------------
Block ISLA is the large glue layer between Carbon foam and sensor (and sensor and hybrid)
      Attribute ISLA Seen=1 colo=11

      ! Epoxy   C8-H14-O4
      Component C   A=12   Z=6   W=8.0*12.0/174.0
      Component H   A=1    Z=1   W=14.0*1.0/174.0
      Component O   A=16   Z=8   W=4.0*16.0/174.0
      Mixture   Epoxy    Dens=1.3

      Shape BOX dx = ISGA_W/2.0   _
                dy = ISGA_Thk/2.0 _
                dz = ISGA_H/2.0

endblock

* -----------------------------------------------------------------------------
Block ISLB is the small glue layer between hybrid and chip
      Attribute ISLA Seen=1 colo=11

      ! Epoxy   C8-H14-O4
      Component C   A=12   Z=6   W=8.0*12.0/174.0
      Component H   A=1    Z=1   W=14.0*1.0/174.0
      Component O   A=16   Z=8   W=4.0*16.0/174.0
      Mixture   Epoxy    Dens=1.3

      Shape BOX dx = ISGB_W/2.0   _
                dy = ISGB_Thk/2.0 _
                dz = ISGB_H/2.0

endblock

* -----------------------------------------------------------------------------
Block ISRI is the support ring for the layers
      Attribute ISRA  Seen=1  colo=13
                                                                                                                                                                                             
      Component C   A=12   Z=6   W=0.05
      Component N   A=14   Z=7   W=0.95
      Mixture   CFoam   Dens=0.10
                                                                                                                                                                                             
      Shape TUBE Rmin = ISRR_Rin        _
                 Rmax = ISRR_Rout       _
                 Dz   = ISRR_Length/2.0
                                                                                                                                                                                             
endblock

* -----------------------------------------------------------------------------
Block ISCO is the interconnect between the support rings
      Attribute ISCO  Seen=1  colo=13
                                                                                                                                                                                             
      Component C   A=12   Z=6   W=0.05
      Component N   A=14   Z=7   W=0.95
      Mixture   CFoam   Dens=0.10
                                                                                                                                                                                             
      Shape CONE Rmn1 = ISCC_Rin1       _
                 Rmx1 = ISCC_Rout1      _
                 Rmn2 = ISCC_Rin2       _
                 Rmx2 = ISCC_Rout2      _
                 Dz   = ISCC_Length/2.0
                                                                                                                                                                                             
endblock
                                                                                                                                                                                             
* -------------------------------------------------------

      END
