*
* ***************************************************************
* March 10,    2008: Gerrit van Nieuwenhuizen
* Very simple post CD0 geometry
* 24 ladders with 12 sensors each at Radius=14cm
* Just active silicon filling up the whole circle at R=14
* Added some inactive material to mimic total material budget
* Also 'fixed' some things with versioning (ISVR removed)
*
*
*****************************************************************
Module ISTBGEO00 is the SIMPLIFIED geometry of the Inner Silicon Tracker
  Created  10 Mar 2008
  Author   Gerrit van Nieuwenhuizen
*****************************************************************
+CDE,AGECOM,GCUNIT.
* ---
      ! Local variables
      REAL    aangle, bangle, raddeg
      REAL    IniX,IniY
      INTEGER nl, ly, nu,  cable, istLayer
      REAL    Module_Thk, Ladder_Thk, Ladder_Width, Ladder_Length
      REAL    ModuleRadius

* ---
      ! Global stuff
      Content   IBMO,IBMY,IBAM,IBLM,IBSS,IBSP,ISMB
      Structure ISMG {Version, SubVersion, Rin, Rout}
      Structure ISBG {Layer, nLadder, nUnit, Radius, Tilt, Gap}
      Structure ISAN {Version,  Rmin, Length, Thk}
      Structure ISSS {Version, ActiveW, ActiveH, ActiveThk, PassiveThk}

* -------------------------------------------------------
   Fill ISMG                   ! Mother volume whole detector
      Version    =  1          ! Version, this goes to g2t_volume_id.g!!!
      SubVersion =  0          ! For future use with g2t_volume_id.g
      Rin        =  13.50      ! Inner radius
      Rout       =  15.00      ! Outer radius (Length = ladder length)
   EndFill
*--------------------------------------------------------
   Fill ISBG                   ! Inner Silicon Tracker parameters
      Layer       =  1         ! Layer index, just one layer left
      nLadder     =  24        ! Number of ladders in this layer
      nUnit       =  12        ! Number of sensors per ladder in this layer
      Radius      =  14.0      ! Radius centers of active silicon sensors
      Tilt        =  -90.0      ! Tilt angle of the ladders
      Gap         =  0.0000    ! Gaps between the sensors
   EndFill
*--------------------------------------------------------
   Fill ISAN                   ! Radiation length equivalent tube
      Version    =  1          ! Version
      Rmin       =  14.200     ! Inner radius (Length = ladder length)
      Thk        =  0.0636     ! Thickness (1% RL silicon)
   EndFill
*--------------------------------------------------------
   Fill ISSS                   ! Silicon Sensor
      Version    = 1           ! Version
      ActiveW    = 3.6823      ! Active Area Width
      ActiveH    = 4.0400      ! Active Area Height
      ActiveThk  = 0.0300      ! Active Area Thickness
      PassiveThk = 0.0936      ! Passive Area Thickness
   EndFill
* -------------------------------------------------------
      raddeg = 3.14159265/180.0
* -------------------------------------------------------
      USE ISMG Version=1
      
      ! Calculate thickness of modules and ladders
      ! to properly set the size of the mothervolumes
      Module_Thk = ISSS_ActiveThk+ISSS_PassiveThk
      ! Ladder thickness 
      Ladder_Thk = Module_Thk
      ! Ladder width
      Ladder_Width = ISSS_ActiveW
      ! Ladder length, no. module * module width
      Ladder_Length = ISBG_nUnit*ISSS_ActiveH

      Create   IBMO
      Position IBMO in CAVE

      write (*,*) '===>GEOINFO/istbgeo00 SIMPLE VERSION of IST!!! - IBMO - created'
* -------------------------------------------------------
Block IBMO is the mother of the ISTB detector
      Material  Air
      Attribute IBMO  Seen=0  colo=1
      Shape TUBE Rmin = ISMG_Rin        _
                 Rmax = ISMG_Rout       _
                 Dz   = Ladder_Length/2.0

*     Make one IST layer
      Create   IBMY
      Position IBMY

*     Make the radiation length equivalent tube
*      Create   ISMB
*      Position ISMB

endblock

* -------------------------------------------------------
Block IBMY is the mother of the IST layer
     Material  Air
     Attribute IBMY  Seen=0  colo=1 serial=istLayer

     Shape TUBE Rmin = ISMG_Rin        _
                Rmax = ISAN_Rmin       _
                Dz   = Ladder_Length/2.0

*    Radius at which the module has to end up to have the active volume
*    at the IST Radius
     ModuleRadius = ISBG_Radius+Module_Thk/2.0-ISSS_ActiveThk/2.0

     do nl=1,ISBG_nLadder ! inner loop over ladders (which consist of sensors)

        ! Rotate ladders so that they nicely cover the layer
        aangle = (360.0/ISBG_nLadder)*nl 
        ! And tilt a bit so that they can be tiled
        bangle = (aangle+ISBG_Tilt)*raddeg
        ! Vector from center of silicon sensor to center ladder volume
        IniX = 0.0
        IniY = 0.0
        Create   IBAM
        Position IBAM x = ModuleRadius*cos(aangle*raddeg)+(IniX*cos(bangle)-IniY*sin(bangle)) _
                      y = ModuleRadius*sin(aangle*raddeg)+(IniX*sin(bangle)+IniY*cos(bangle)) _
                      z = 0.0                 _
                      AlphaZ = aangle+ISBG_Tilt
      enddo

endblock

* -----------------------------------------------------------------------------
Block IBAM is the mother of the whole long ladder
      Attribute IBAM  Seen=0  colo=1

      Shape BOX dx = Ladder_Width/2.0 _
                dy = Ladder_Thk/2.0   _
                dz = Ladder_Length/2.0

      Create IBLM
      do nu=1,ISBG_nUnit      
        Position IBLM x = 0.0 _
                      y = 0.0 _
                      z = -1.0*Ladder_Length/2.0+ISSS_ActiveH*(nu-0.5)
      enddo

endblock

* -----------------------------------------------------------------------------
Block IBLM is the mother of the sensor assembly
      Attribute IBLM  Seen=0  colo=1 serial=istLayer

      Shape BOX dx = Ladder_Width/2.0 _
                dy = Module_Thk/2.0       _
                dz = ISSS_ActiveH/2.0


* --  Silicon Sensors
      Create   IBSS
      Position IBSS x =  0.0  _
                    y =  -1.0*Module_Thk/2.0+ISSS_ActiveThk/2.0  _
                    z =  0.0 
      Create   IBSP
      Position IBSP x =  0.0  _
                    y =  +1.0*Module_Thk/2.0-ISSS_PassiveThk/2.0  _
                    z =  0.0

endblock

* -----------------------------------------------------------------------------
Block IBSP is the Silicon Sensor Passive Area
      Attribute IBSP  Seen=1  colo=4
      Material  Silicon

      Shape BOX dx = ISSS_ActiveW/2.0 _
                dY = ISSS_PassiveThk/2.0   _
                dz = ISSS_ActiveH/2.0

endblock

* -----------------------------------------------------------------------------
Block IBSS is the Silicon Sensor Active Area
      Attribute IBSS  Seen=1  colo=2
      Material  Silicon
      Material  Sensitive  Isvol=1

      Shape BOX dx = ISSS_ActiveW/2.0 _
                dY = ISSS_ActiveThk/2.0   _
                dz = ISSS_ActiveH/2.0

      HITS    IBSS   Z:.001:S  Y:.001:   X:.001:     Ptot:16:(0,100),
                     cx:10:    cy:10:    cz:10:      Sleng:16:(0,500),
                     ToF:16:(0,1.e-6)    Step:.01:   Eloss:16:(0,0.001) 
endblock

* -----------------------------------------------------------------------------
*Block ISMB is the material budget equivalent tube
*      Attribute ISMB Seen=1  colo=4
*      Material Silicon
*
*      Shape TUBE Rmin = ISAN_Rmin          _
*                 Rmax = ISAN_Rmin+ISAN_Thk _
*                 Dz   = Ladder_Length/2.0
*
*endblock
*
* -----------------------------------------------------------------------------


      END
