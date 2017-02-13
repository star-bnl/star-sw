******************************************************************************
Module PIXLGEO00 is the SIMPLIFIED pixel detector
  Created  03/13/08
  Author   Gerrit van Nieuwenhuizen
******************************************************************************
+CDE,AGECOM,GCUNIT.
*
      real    angle,anglePos,angleCorr,raddeg
      integer nLadder,nSector, nExtraLadder
      Real    LadderRadius, LadderThk, LadderWidth

      Content  PXMO, PXLA, PLMI, PLAC, PLPS, PXME
*
      Structure PXLD {version,   SubVersion,
                      TotalLength,
                      PassiveThk,  ActiveThk, 
                      LayerThk,  Rin,Rout}

      Structure PIXG {Layer, NoLadders, r,a,pOffset,aOffset}
      Structure PXBG {version, Length, Rin, Thk}
*
* -----------------------------------------------------------------------------
*
*
  Fill PXLD                    ! General pixel parameters
      Version    =  1.0        ! version
      SubVersion =  0.0        ! sub version
      TotalLength=  20.0       ! Overal length of the detector
      PassiveThk =  0.0280     ! Passive silicon Thickness
      ActiveThk  =  0.0020     ! Active  silicon Thickness
      Rin        =  2.5        ! Inner radius
      Rout       =  8.0        ! Outer radius
      LayerThk   =  0.5        ! Thickness of the layer mother volume
  EndFill

   Fill PIXG                   ! Inner layer parameters
      Layer      =  1          ! Layer index 
      NoLadders  =  10         ! Number of ladders
      r          =  2.5        ! 1st ladder nominal radius
      aOffset    = -90.0       ! Angular offset
      pOffset    =  0.0        ! Position offset (shift)
   EndFill

   Fill PIXG                   ! Outer layer parameters
      Layer      =  2          ! Layer index 
      NoLadders  =  30         ! Number of ladders
      r          =  8.0        ! Ladder radius
      aOffset    = -90.0       ! Angular offset
      pOffset    =  0.0        ! Position offset (shift)
   EndFill


*   Fill PXBG                   ! Beam Pipe Exoskeleton Data
*      version    =  2          ! Version
*      Length     =  48.0       ! Total Length
*      Rin        =   8.5       ! Inner Radius
*      Thk        =   0.0800    ! Thickness
*   EndFill

* -----------------------------------------------------------------------------
      raddeg=3.14159265/180.0
* -----------------------------------------------------------------------------
      USE      PXLD Version=1

      LadderThk = PXLD_ActiveThk + PXLD_PassiveThk

      write(*,*) '===>GEOINFO/pixlgeo00 SIMPLE VERSION of PIXEL!!! - PXMO - created'

      Create   PXMO
      Position PXMO in CAVE   Konly='ONLY'

*      Create   PXME
*      Position PXME in CAVE
* -----------------------------------------------------------------------------
Block PXMO is the mother of the pixel detector volumes
      Material  Air
      Attribute PXMO  Seen=0  colo=6

      Shape TUBE Rmin=PXLD_Rin-PXLD_LayerThk/2.0  _
                 Rmax=PXLD_Rout+PXLD_LayerThk/2.0 _
                 Dz=PXLD_TotalLength/2.0


*      Make inner layer
       USE PIXG Layer=1
       Create   PXLA
       Position PXLA
*      Make outer layer
       USE PIXG Layer=2
       Create   PXLA
       Position PXLA
endblock
* -----------------------------------------------------------------------------
Block PXLA is the mother of a layer
      Material Air
      Attribute PXLA Seen=0 colo=1
      Shape TUBE Rmin=PIXG_r-PXLD_LayerThk/2.0 _
                 Rmax=PIXG_r+PXLD_LayerThk/2.0 _
                 Dz=PXLD_TotalLength/2.0

*     Determine the radius of the ladder mother so that the active layer
*     ends up at the right radius
      LadderRadius = PIXG_r+LadderThk/2.0-PXLD_ActiveThk/2.0

*     Determine the width of the ladder so that the layer
*     will be hermetic
      LadderWidth = 2.0 * (PIXG_r-PXLD_ActiveThk/2.0) * _
                    tan(raddeg*360.0/PIXG_NoLadders/2.0)

      do nLadder=1,PIXG_NoLadders
         angle = (360.0/PIXG_NoLadders)*nLadder
         anglePos = angle*raddeg

         Create   PLMI
         Position PLMI x=LadderRadius*cos(anglePos) _
                       y=LadderRadius*sin(anglePos) _
                       z=0.0                  _
                       AlphaZ=angle+PIXG_aOffset
      enddo
endblock
* -----------------------------------------------------------------------------
Block PLMI is the mother of a silicon ladder
      Material  Air
      Attribute PLMI   Seen=0  colo=6
      Shape BOX dX=LadderWidth/2.0 _
                dY=LadderThk/2.0   _
                Dz=PXLD_TotalLength/2.0

      Create   PLAC
      Position PLAC x =  0.0 _
                    y = -1.0*LadderThk/2.0+PXLD_ActiveThk/2.0 _
                    z =  0.0
      Create   PLPS
      Position PLPS x =  0.0 _
                    y = +1.0*LadderThk/2.0-PXLD_PassiveThk/2.0 _
                    z =  0.0
endblock
* -----------------------------------------------------------------------------
Block PLPS is the passive layer of the ladder
      Material  Silicon
      Attribute PLPS   Seen=1  colo=4

      Shape BOX dX=LadderWidth/2.0      _
                dY=PXLD_PassiveThk/2.0  _
                Dz=PXLD_TotalLength/2.0
endblock
* -----------------------------------------------------------------------------
Block PLAC is the active layer of the ladder
      Material  Silicon
      Material  Sensitive  Isvol=1
      Attribute PLAC   Seen=1  colo=2

      Shape BOX dX=LadderWidth/2.0      _
                dY=PXLD_ActiveThk/2.0   _
                Dz=PXLD_TotalLength/2.0
                

*      call      GSTPAR (%Imed,'STRA',1.)

      HITS    PLAC   Z:.00001:S  Y:.00001:   X:.00001:     Ptot:16:(0,100),
                     cx:10:    cy:10:    cz:10:      Sleng:16:(0,500),
                     ToF:16:(0,1.e-6)    Step:.01:   Eloss:16:(0,0.001) 
                   
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

      END

