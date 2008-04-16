*
******************************************************************************
Module PIXLGEO00 is the SIMPLIFIED pixel detector
  Created  03/13/08
  Author   Gerrit van Nieuwenhuizen
******************************************************************************
+CDE,AGECOM,GCUNIT.
*
      real    angle,anglePos,angleCorr,raddeg
      integer nLadder,nSector, nExtraLadder
      Real    ActiveRadius

      Content  PXMO, PSEC, PLMO, PLAC, PLPS, PXME
*
      Structure PXLV {int version, LadVer}

      Structure PXLD {version, TotalLength,
                      LadderThk,PassiveThk,ActiveThk, Rin, Rout}

      Structure PIXG {Ladder,LadderWidth,r,a,pOffset,aOffset}
      Structure PXBG {version, Length, Rin, Thk}
*
* -----------------------------------------------------------------------------
*
*
  Fill PXLV                    ! Pixel ladder data
      Version    =  1.0        ! config version
      LadVer=1.0               ! Ladder Version
  EndFill

  Fill PXLD                    ! Pixel ladder data
      Version    =  1.0        ! version
      TotalLength=  20.0       ! Overal length of the detector
      LadderThk  =  0.0300     ! Total ladder Thickness = 280um + 20um
      PassiveThk =  0.0280     ! Passive silicon Thickness
      ActiveThk  =  0.0020     ! Active  silicon Thickness
      Rin        =  2.4        ! Inner radius
      Rout       =  8.3        ! Outer radius
  EndFill

   Fill PIXG                   ! Pixel detector data
      Ladder     =  1          ! ladder index
      LadderWidth=  1.81912323 ! Ladder Width
      r          =  2.5        ! 1st ladder nominal radius
      a          =  100.       ! 1st ladder nominal position angle
      aOffset    =  90.0       ! Angular offset
      pOffset    =  0.0        ! Position offset (shift)
   EndFill

   Fill PIXG                   ! Pixel detector data
      Ladder     =  2          ! ladder index
      LadderWidth=  1.81912323 ! Ladder Width
      r          =  2.5        ! 2nd ladder nominal radius
      a          =  60.        ! 2nd ladder nominal position angle
      aOffset    =  90.0       ! Angular offset
   EndFill

   Fill PIXG                   ! Pixel detector data
      Ladder     =  3          ! ladder index
      LadderWidth=  1.81912323 ! Ladder Width
      r          =  2.5        ! 2nd ladder nominal radius
      a          =  20.        ! 2nd ladder nominal position angle
      aOffset    =  90.0       ! Angular offset
   EndFill


   Fill PIXG                   ! Pixel detector data
      Ladder     =  4          ! ladder index
      LadderWidth= 2.106176    ! Ladder Width
      r          =  8.0        ! 2nd ladder nominal radius
      a          =  105.       ! 2nd ladder nominal position angle
      aOffset    =  90.        ! Angular offset
   EndFill

   Fill PIXG                   ! Pixel detector data
      Ladder     =  5          ! ladder index
      LadderWidth= 2.106176    ! Ladder Width
      r          =  8.0        ! 3rd ladder radius
      a          =  90.        ! 3rd ladder nominal position angle
      aOffset    =  90.        ! Angular offset
   EndFill

   Fill PIXG                   ! Pixel detector data
      Ladder     =  6          ! ladder index
      LadderWidth= 2.106176    ! Ladder Width
      r          =  8.0        ! 4th ladder nominal radius
      a          =  75.        ! 4th ladder nominal position angle
      aOffset    =  90.        ! Angular offset
   EndFill


   Fill PIXG                   ! Pixel detector data
      Ladder     =  7          ! ladder index
      LadderWidth= 2.106176    ! Ladder Width
      r          =  8.0        ! 3rd ladder radius
      a          =  60.        ! 3rd ladder nominal position angle
      aOffset    =  90.        ! Angular offset
   EndFill


   Fill PIXG                   ! Pixel detector data
      Ladder     =  8          ! ladder index
      LadderWidth= 2.106176    ! Ladder Width
      r          =  8.0        ! 4th ladder nominal radius
      a          =  45.        ! 4th ladder nominal position angle
      aOffset    =  90.        ! Angular offset
   EndFill


   Fill PIXG                   ! Pixel detector data
      Ladder     =  9          ! ladder index
      LadderWidth= 2.106176    ! Ladder Width
      r          =  8.0        ! 3rd ladder radius
      a          =  30.        ! 3rd ladder nominal position angle
      aOffset    =  90.        ! Angular offset
   EndFill


  Fill PIXG                    ! Pixel detector data
      Ladder     =  10         ! ladder index
      LadderWidth= 2.106176    ! Ladder Width
      r          =  8.0        ! 4th ladder nominal radius
      a          =  15.        ! 4th ladder nominal position angle
      aOffset    =  90.        ! Angular offset
   EndFill


   Fill PIXG                   ! Pixel detector data
      Ladder     =  11         ! ladder index
      LadderWidth= 2.106176    ! Ladder Width
      r          =  8.0        ! 3rd ladder radius
      a          =  0.         ! 3rd ladder nominal position angle
      aOffset    =  90.        ! Angular offset
   EndFill


   Fill PXBG                   ! Beam Pipe Exoskeleton Data
      version    =  2          ! Version
      Length     =  48.0       ! Total Length
      Rin        =   8.5       ! Inner Radius
      Thk        =   0.0800    ! Thickness
   EndFill

* -----------------------------------------------------------------------------
      raddeg=3.14159265/180.0
* -----------------------------------------------------------------------------
      USE      PXLV
      USE      PXLD version=1
*

      write(*,*) '===>GEOINFO/pixlgeo00 SIMPLE VERSION of PIXEL!!! - PXMO - created'

      Create   PXMO
      Position PXMO in CAVE   Konly='ONLY'

*      Create   PXME
*      Position PXME in CAVE
* -----------------------------------------------------------------------------
Block PXMO is the mother of the pixel detector volumes
      Material  Air
      Attribute PXMO  Seen=0  colo=1

      Shape TUBE Rmin=PXLD_Rin           _
                 Rmax=PXLD_Rout          _
                 Dz=PXLD_TotalLength/2.0

* The "sector" is defined as a group of 5 ladders, we
* have a total of 6 overlapping sectors placed with rotational symmetry

      Create PSEC
      do nSector=1,3 ! need the "MANY" option as they do overlap
          Position PSEC AlphaZ=120.0*(nSector-1) _
                        konly='MANY'
      enddo

endblock
* -----------------------------------------------------------------------------
*Block PXME is the exoskeleton tube
*      Component C A=12 Z=6 W=1
*      Mixture   CFiber Dens=1.713     
*      Attribute PXME  Seen=1  colo=3
*
*      Shape TUBE Rmin=PXBG_Rin          _
*                 Rmax=PXBG_Rin+PXBG_Thk _
*                 Dz=PXBG_Length/2.0
*EndBlock
* -----------------------------------------------------------------------------
Block PSEC is a group of ladders
      Material  Air
      Attribute PSEC   Seen=0  colo=1
      Shape TUBS Rmin=PXLD_Rin           _
                 Rmax=PXLD_Rout          _
                 Dz=PXLD_TotalLength/2.0 _
                 Phi1=-11.0              _
                 Phi2=122.0


       do nLadder=1,11 ! Inner loop, create ladders inside the sector

         USE PIXG Ladder=nLadder ! index the ladder data structures
         angle = PIXG_a

* Individual ladders can be individually tilted by using
* the aOffset parameter (angular offset), and the pOffset
* (position offset), which is the individual lateral
* displacement. (Optional and maybe obsoleted soon: angleCorr= atan(PIXG_pOffset/PIXG_r))

* The anglePos defines the POSITION of the center of the ladder
* in space, along the lines of x=r*cos(...), y=r*sin(...)
* have to correct and convert to radians:

         anglePos = angle*raddeg               !  +angleCorr  see above comment

* Place middle of the active material at the desired radius
         ActiveRadius = PIXG_r+PXLD_LadderThk/2.0-PXLD_ActiveThk/2.0

* In case we do go with the pOffset, don't forget to correct the radius,
* to keep the surfaces at the nominal DCA to the beam

         Create   PLMO
         Position PLMO x=ActiveRadius*cos(anglePos) _
                       y=ActiveRadius*sin(anglePos) _
                       z=0.0                  _
                       AlphaZ=-PIXG_aOffset+angle

       enddo

endblock
*
* -----------------------------------------------------------------------------
Block PLMO is the mother of the silicon ladder
      Material  Air
      Attribute PLMO   Seen=1  colo=1
      Shape BOX dX=PIXG_LadderWidth/2.0 _
                dY=PXLD_LadderThk/2.0   _
                Dz=PXLD_TotalLength/2.0

      Create   PLAC
      Position PLAC y=-PXLD_LadderThk/2.0+PXLD_ActiveThk/2.0
      Create   PLPS
      Position PLPS y=-PXLD_LadderThk/2.0+PXLD_ActiveThk+PXLD_PassiveThk/2.0

endblock
*
* -----------------------------------------------------------------------------
Block PLAC is the active layer of the ladder
      Material  Silicon
      Material  Sensitive  Isvol=1
      Attribute PLAC   Seen=1  colo=2

      Shape BOX dX=PIXG_LadderWidth/2.0 _
                dY=PXLD_ActiveThk/2.0   _
                Dz=PXLD_TotalLength/2.0

      call      GSTPAR (%Imed,'STRA',1.)

      HITS    PLAC   Z:.00001:S  Y:.00001:   X:.00001:     Ptot:16:(0,100),
                     cx:10:    cy:10:    cz:10:      Sleng:16:(0,500),
                     ToF:16:(0,1.e-6)    Step:.01:   Eloss:16:(0,0.001) 
                   
endblock
* -----------------------------------------------------------------------------
Block PLPS is the passive layer of the ladder
      Material  Silicon
      Attribute PLPS   Seen=1  colo=4

      Shape BOX dX=PIXG_LadderWidth/2.0 _
                dY=PXLD_PassiveThk/2.0  _
                Dz=PXLD_TotalLength/2.0

endblock
* -----------------------------------------------------------------------------

      END

