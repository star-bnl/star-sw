*
* $Id: pixlgeo3.g,v 1.5 2007/03/21 21:10:16 potekhin Exp $
* $Log: pixlgeo3.g,v $
* Revision 1.5  2007/03/21 21:10:16  potekhin
* A new backward-compatible version with (a) refactoring of the data
* structures for better modification support (b) switchable
* "thick" active layer configuration (c) lots of small purely
* cosmetic changes
*
* Revision 1.4  2007/03/15 19:57:00  potekhin
* Added a useful diagnostic printout about the thicknesses
* of the Si layers (passive and active)
*
* Revision 1.3  2006/10/23 00:10:03  potekhin
* The opening angle of the sector needs to be increased (one ladder
* was sticking outside.
*
* Revision 1.2  2006/10/21 18:09:57  potekhin
* Decreased the radius of the "active" part of the detector as per
* Jim's instructions, employed the 'ONLY' version to insure we don't
* lose hits in case there is intersection with other volumes.
*
* Revision 1.1  2006/10/02 21:37:38  potekhin
* An updated version of the HFT geometry
*
* 
*
******************************************************************************
Module PIXLGEO3 is the the STAR pixel detector and beam pipe support
  Created  10/02/06
  Author   Andrew Rose
******************************************************************************
+CDE,AGECOM,GCUNIT.
*
      real angle,anglePos,angleCorr,raddeg
      integer nLadder,nSector, nExtraLadder

      Content  PXMO, PSEC, PLMO, PLAC, PLPS, PXBX
*
      Structure PXLV {int version, LadVer}

      Structure PXLD {version, TotalLength,
                      LadderWidth,LadderThk,PassiveThk,ActiveThk, Rin, Rout}

      Structure PIXG {Ladder,r,a,pOffset,aOffset}
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
      LadderWidth=  2.00       ! Ladder Width
      LadderThk  =  0.0240     ! Total ladder Thickness
      PassiveThk =  0.0220     ! Passive silicon Thickness
      ActiveThk  =  0.0020     ! Active  silicon Thickness
      Rin        =  2.4        ! Inner radius
      Rout       =  8.3        ! Outer radius
  EndFill

  Fill PXLD                    ! Pixel ladder data
      Version    =  2.0        ! version
      LadderWidth=  2.00       ! Ladder Width
      LadderThk  =  0.0240     ! Total ladder Thickness
      PassiveThk =  0.0120     ! Passive silicon Thickness
      ActiveThk  =  0.0120     ! Active  silicon Thickness
      Rin        =  2.4        ! Inner radius
      Rout       =  8.3        ! Outer radius
  EndFill

   Fill PIXG                   ! Pixel detector data
      Ladder     =  1          ! ladder index
      r          =  2.5        ! 1st ladder nominal radius
      a          =  100.       ! 1st ladder nominal position angle
      aOffset    =  103.       ! Angular offset
*
      pOffset    =  0.0        ! Position offset (shift)
   EndFill

   Fill PIXG                   ! Pixel detector data
      Ladder     =  2          ! ladder index
      r          =  2.5        ! 2nd ladder nominal radius
      a          =  60.        ! 2nd ladder nominal position angle
      aOffset    =  103.       ! Angular offset
   EndFill

   Fill PIXG                   ! Pixel detector data
      Ladder     =  3          ! ladder index
      r          =  2.5        ! 2nd ladder nominal radius
      a          =  20.        ! 2nd ladder nominal position angle
      aOffset    =  103.       ! Angular offset
   EndFill


   Fill PIXG                   ! Pixel detector data
      Ladder     =  4          ! ladder index
      r          =  6.5        ! 2nd ladder nominal radius
      a          =  105.       ! 2nd ladder nominal position angle
      aOffset    =  90.        ! Angular offset
   EndFill

   Fill PIXG                   ! Pixel detector data
      Ladder     =  5          ! ladder index
      r          =  7.5        ! 3rd ladder radius
      a          =  90.        ! 3rd ladder nominal position angle
      aOffset    =  90.        ! Angular offset
   EndFill

   Fill PIXG                   ! Pixel detector data
      Ladder     =  6          ! ladder index
      r          =  6.5        ! 4th ladder nominal radius
      a          =  75.        ! 4th ladder nominal position angle
      aOffset    =  90.        ! Angular offset
   EndFill


   Fill PIXG                   ! Pixel detector data
      Ladder     =  7          ! ladder index
      r          =  7.5        ! 3rd ladder radius
      a          =  60.        ! 3rd ladder nominal position angle
      aOffset    =  90.        ! Angular offset
   EndFill


   Fill PIXG                   ! Pixel detector data
      Ladder     =  8          ! ladder index
      r          =  6.5        ! 4th ladder nominal radius
      a          =  45.        ! 4th ladder nominal position angle
      aOffset    =  90.        ! Angular offset
   EndFill


   Fill PIXG                   ! Pixel detector data
      Ladder     =  9          ! ladder index
      r          =  7.5        ! 3rd ladder radius
      a          =  30.        ! 3rd ladder nominal position angle
      aOffset    =  90.        ! Angular offset
   EndFill


  Fill PIXG                    ! Pixel detector data
      Ladder     =  10         ! ladder index
      r          =  6.5        ! 4th ladder nominal radius
      a          =  15.        ! 4th ladder nominal position angle
      aOffset    =  90.        ! Angular offset
   EndFill


   Fill PIXG                   ! Pixel detector data
      Ladder     =  11         ! ladder index
      r          =  7.5        ! 3rd ladder radius
      a          =  0.         ! 3rd ladder nominal position angle
      aOffset    =  90.        ! Angular offset
   EndFill


   Fill PXBG                   ! Beam Pipe Exoskeleton Data
      version    =  2          ! Version
      Length     =  48.0       ! Total Length
      Rin        =   8.5       ! Inner Radius
      Thk        =   0.1       ! Thickness
   EndFill

******************************************************
      USE      PXLV
      USE      PXLD version=PXLV_LadVer
*
      write(*,*) 'This is the new pixel geometry, pixlgeo3. Ladder version is ', PXLV_LadVer
      raddeg=3.14159265/180.0

      write(*,*) '=======  Constructing the Pixel Detector with Beam Pipe Support ========'
      write(*,*) 'HFT thk: Passive, active, total ',PXLD_PassiveThk,',',PXLD_ActiveThk,',',PXLD_LadderThk
      Create   PXMO
      Position PXMO in CAVE   Konly='ONLY'

      Create   PXBX
      Position PXBX in CAVE
* -----------------------------------------------------------------------------
Block PXMO is the mother of the pixel detector volumes
      Material  Air
      Attribute PXMO  Seen=1  colo=6

      Shape TUBE Rmin=PXLD_Rin Rmax=PXLD_Rout Dz=PXLD_TotalLength/2.0

* The "sector" is defined as a group of 5 ladders, we
* have a total of 6 overlapping sectors placed with rotational symmetry

      Create PSEC
      do nSector=1,3 ! need the "MANY" option as they do overlap
          Position PSEC AlphaZ=120.0*(nSector-1) konly='MANY'
      enddo

endblock
* -----------------------------------------------------------------------------
Block PXBX is the exoskeleton of the beampipe
      Material  Berillium
      Attribute PXBX  Seen=1  colo=3

      Shape TUBE Rmin=PXBG_Rin Rmax=PXBG_Rin+PXBG_Thk Dz=PXBG_Length/2.0
EndBlock
* -----------------------------------------------------------------------------
Block PSEC is a group of ladders
      Material  Air
      Attribute PSEC   Seen=1  colo=5
      Shape TUBS  Rmin=PXLD_Rin Rmax=PXLD_Rout Dz=PXLD_TotalLength/2.0 Phi1=-11.0 Phi2=122.0


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

* In case we do go with the pOffset, don't forget to correct the radius,
* to keep the surfaces at the nominal DCA to the beam

         Create and Position PLMO x=PIXG_r*cos(anglePos) y=PIXG_r*sin(anglePos) _
         z=0.0 AlphaZ=-PIXG_aOffset+angle

       enddo

endblock
*
* -----------------------------------------------------------------------------
Block PLMO is the mother of the silicon ladder
      Material  Air
      Attribute PLMO   Seen=1  colo=4
      Shape BOX dX=PXLD_LadderWidth/2.0 dY=PXLD_LadderThk/2.0 Dz=PXLD_TotalLength/2.0

      Create and position PLAC y=-PXLD_LadderThk/2.0+PXLD_ActiveThk/2.0
      Create and position PLPS y=-PXLD_LadderThk/2.0+PXLD_ActiveThk+PXLD_PassiveThk/2.0

endblock
*
* -----------------------------------------------------------------------------
Block PLAC is the active layer of the ladder
      Material  Silicon
      Material  Sensitive  Isvol=1
      Attribute PLAC   Seen=1  colo=4

      Shape BOX dX=PXLD_LadderWidth/2.0 dY=PXLD_ActiveThk/2.0 Dz=PXLD_TotalLength/2.0

      call      GSTPAR (%Imed,'STRA',1.)

      HITS    PLAC   Z:.00001:S  Y:.00001:   X:.00001:     Ptot:16:(0,100),
                     cx:10:    cy:10:    cz:10:      Sleng:16:(0,500),
                     ToF:16:(0,1.e-6)    Step:.01:   Eloss:16:(0,0.001) 
                   
endblock
* -----------------------------------------------------------------------------
Block PLPS is the passive layer of the ladder
      Material  Silicon
      Attribute PLPS   Seen=1  colo=2
      Shape BOX dX=PXLD_LadderWidth/2.0 dY=PXLD_PassiveThk/2.0 Dz=PXLD_TotalLength/2.0
endblock

      END

