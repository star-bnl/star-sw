* $Id: scongeo.g,v 1.5 2011/03/11 00:05:51 jwebb Exp $
*
* $Log: scongeo.g,v $
* Revision 1.5  2011/03/11 00:05:51  jwebb
* An improved model of the SVT support cone... specifically the support rods.
* Previous geometry assumed solid carbon.  Now we implement a carbon-fiber nomex
* sandwich.
*
* Revision 1.4  2009/02/12 00:07:32  perev
* BugFix wrong array size for PCON
*
* Revision 1.3  2009/01/06 04:06:35  perev
* coneVer=3 for elliptic rods
*
* Revision 1.2  2008/12/30 19:40:26  perev
* Rods added
*
* Revision 1.1  2007/11/06 01:19:38  perev
* y2008 geo
*
* Support structures living before in SVTT moved into CAVE
*
*
*
*******************************************************************************
*
*
*******************************************************************************
Module  SCONGEO is Support structures from SVTT moved into CAVE:

   Author  Victor Perev
   Created 31 Oct 2007

*------------------------------------------------------------------------------
*  Division lines:                                                            *
*                 *********** separates major divisions                       *
*                 ----------- separates sub-divisions                         *
*                 - - - - - - separates sub-sub divisions                     *
+cde,AGECOM,GCONST,GCUNIT.
*
      Content          SCON,SCMY,SGRA,SBSP,SAKM,SCKM,SBMM,SBMI,SBMO,SMRD,
                       SBRL,SBRX,SBSR,SBCR,SROD,SROH,SRON,SROI
*
      structure SVTG { Version,   Nlayer,    RsizeMin,  RsizeMax,
                       ZsizeMax,  Angoff, SupportVer,   ConeVer,
                       ifMany, Nmin}
*
      structure SSUP { Version,   CabThk,    HosRmn,
                       HosRmx,    Nhoses,    WrpMyThk,  WrpAlThk,
                       GrphThk,   Cone1Zmn,  RodLen,    RodDist,
                       RodID,     RodOD,     RodIDx,    RodODx,
                       carbonShell, carbonDens, nomexDens,
		       Con1IdMn, Con3IdMn,
                       Con4IdMn, Con4IdMx,   Cone3zmx,  Cone4zmx,
                       BraThk,    ERJThk,    ERJWid,
                       ERJLen,    ERJzdis,   ERJ1x,     ERJ2x,
                       ERJ2y,     ERJrad,    ERJdia}
*
      structure SSUB { Version,   KMountId,  KMountOd,  KMntThk,
                       KMCutOd,   KMCutId,   KMCutOA,   KMCutOff,
                       SRingId,   SRingOd,   SRingThk,
                       SRCutPhi,  SRCutWid,  SRCutOut,  SRCutIn,
                       SRollId,   SRollOd,   SRollLen,  SWireLen,
                       MBlkHgh,   MBlkOWid,  MBlkOLen,  MBlkIWid,
                       MBlkILen,  MBlkORad,  MBlkIRad,  MRodDia}
*

      Integer        iLayer,s,side,ilad,iwaf,i,j
      Real           ladthk,cone_thk1,cone_thk2,roffset,RsizeMax,deg,rad,c0
      Real           cone_len,cone_sin,cone_cos,rmin,rmax,zmin,zmax,angle
      Real           xpos,ypos,zpos,clearance,rin,rou,elethk,tabLen,radmax
      Real           endrng_z,brack_z,screw_z,ir_rmin,ang,wafpckLen,dthk,radtilt
      Real           xbuf, phi, xbuf1, xbuf2
      Real           yPCB, A, CuThk, sq, tube_angle
      Real           radii(6), rad_cones_in(5),rad_cones_out(5), rad_offset, shield_phi(4)

      Real           trapY,ssidX,ssirY

      Integer        i_phi

*******************************************************************************
***************************
      shield_phi(1)=9.0
      shield_phi(2)=27.0
      shield_phi(3)=45.0
      shield_phi(4)=63.0
***************************
   Fill SVTG ! Basic SVT dimensions 
      Version   = 2          ! geometry version
      Nlayer    = 7          ! number of svt layers (was 7)
      RsizeMin  = 4.100      ! STV innermost radius
      RsizeMax  = 46.107     ! STV outermost radius
      ZsizeMax  = 270        ! SVT+FTPC length
      Angoff    = 0          ! angular offset x1 for slayer 2 x2 for slayer 3
      SupportVer= 1          ! versioning of the shield
      ConeVer=    1          ! versioning of the support cone
      ifMany    = 0          ! whether we use the geant MANY option
      Nmin      = 1          ! the index of the innermost layer
*
   Fill SSUP ! Support structures
      Version   = 1          ! geometry version
      CabThk    = 0.05       ! thickness of layer of cables on support cone
      HosRmn    = 0.75       ! inner radius of water hoses on support cone
      HosRmx    = 0.95       ! outer radius of water hoses on support cone
      Nhoses    = 10         ! number of water hoses
      WrpMyThk  = 0.10       ! thickness of mylar wrap around cone (guess)
      WrpAlThk  = 0.01       ! thickness of Al on mylar wrap (guess)
      GrphThk   = 0.16       ! support cone thickness
      Cone1zmn  = 52.23      ! Cone z min (parts 1,2,3,4 in increasing z)
      RodLen    = 110.8      ! Length of support rods
      RodDist   = 17.5       ! Distance of support rod od from beam axis
      RodID     = 2.5        ! ID of Carbon support rods (approx)
      RodOD     = 3.05       ! OD of Carbon support rods (approx)
      Con1IdMn  = 15.67      ! Minimum id of cone 1
      Con3IdMn  = 21.67      ! Minimum id of cone 3 (TBD)
      Con4IdMn  = 37.4       ! Minimum id of cone 4 (TBD)
      Con4IdMx  = 37.4       ! Maximum id of cone 4 (TBD)
      Cone3zmx  = 150.0      ! Maximum z of cone 3 (TBD)
      Cone4zmx  = 229.36     ! Maximum z of cone 4 (TBD)
      BraThk    = .2         ! thickness of Al brackets
      ERJThk    = .1         ! (z) thickness of end ring joining brackets
      ERJWid    = 2.07       ! (azimuthal) width of end ring joining brackets
      ERJLen    = 5.19       ! (radial) length of end ring joining brackets
      ERJzdis   = 2.0        ! dist from ladder ends to ERJ (guess)
      ERJ1x     = 0.31       ! ERJ screw 1 x position (radial)
      ERJ2x     = 1.15       ! ERJ screw 2 x position
      ERJ2y     = 0.72       ! ERJ screw 2 y position
      ERJrad    = 10.80      ! distance of ERJ center from beam axis
      ERJdia    = 0.17       ! ERJ screw diameter
*------------------------------------------------------------
* This is a copy of the above (base) version with one difference which is
* the thickness of the copper layer which models the cables attached to
* the cones. Based on the new measured data from Dave Lynn, we normalize the
* effective thickness of this approximation, such that it matches the actual
* cable weight.It's a factor of 4.35 compared to the previous number. That
* previous number's origin is now impossible to determine.
* NOTE: historically in the code this number, CabThk, is divided by 2.
* I will keep it the same way to more easily compare the old with the new.

   Fill SSUP ! Support structures
      Version   = 2          ! geometry version
      CabThk    = 0.21       ! thickness of layer of cables on support cone
      HosRmn    = 0.75       ! inner radius of water hoses on support cone
      HosRmx    = 0.95       ! outer radius of water hoses on support cone
      Nhoses    = 10         ! number of water hoses
      WrpMyThk  = 0.10       ! thickness of mylar wrap around cone (guess)
      WrpAlThk  = 0.01       ! thickness of Al on mylar wrap (guess)
      GrphThk   = 0.16       ! support cone thickness
      Cone1zmn  = 52.23      ! Cone z min (parts 1,2,3,4 in increasing z)
      RodLen    = 110.8      ! Length of support rods
      RodDist   = 17.5       ! Distance of support rod od from beam axis
      RodID     = 2.5        ! ID of Carbon support rods (approx)
      RodOD     = 3.05       ! OD of Carbon support rods (approx)
      Con1IdMn  = 15.67      ! Minimum id of cone 1
      Con3IdMn  = 21.67      ! Minimum id of cone 3 (TBD)
      Con4IdMn  = 37.4       ! Minimum id of cone 4 (TBD)
      Con4IdMx  = 37.4       ! Maximum id of cone 4 (TBD)
      Cone3zmx  = 150.0      ! Maximum z of cone 3 (TBD)
      Cone4zmx  = 229.36     ! Maximum z of cone 4 (TBD)
      BraThk    = .2         ! thickness of Al brackets
      ERJThk    = .1         ! (z) thickness of end ring joining brackets
      ERJWid    = 2.07       ! (azimuthal) width of end ring joining brackets
      ERJLen    = 5.19       ! (radial) length of end ring joining brackets
      ERJzdis   = 2.0        ! dist from ladder ends to ERJ (guess)
      ERJ1x     = 0.31       ! ERJ screw 1 x position (radial)
      ERJ2x     = 1.15       ! ERJ screw 2 x position
      ERJ2y     = 0.72       ! ERJ screw 2 y position
      ERJrad    = 10.80      ! distance of ERJ center from beam axis
      ERJdia    = 0.17       ! ERJ screw diameter
*------------------------------------------------------------
* This is a copy of the above 2nd version with one difference which is
* elliptic rodes instead of tubes

   Fill SSUP ! Support structures
      Version   = 3          ! geometry version
      RodLen    = 110.8      ! Length of support rods
      RodDist   = 17.5       ! Distance of support rod od from beam axis
      RodID     = 3.64       ! ID of Carbon support rods 
      RodOD     = 4.50       ! OD of Carbon support rods 
      RodIDx    = 8.72       ! ID of Carbon support rods 
      RodODx    = 9.58       ! OD of Carbon support rods 
*------------------------------------------------------------
   Fill SSUP ! Support structures
      Version   = 4          ! geometry version
      carbonShell = 0.04     ! 0.4mm carbon fiber shell
      carbonDens  = 1.78     ! 1.78 g/cm^3 is a typical carbon composite density
      nomexDens   = 0.048    ! Ballpark figure
*------------------------------------------------------------
   Fill SSUB ! beampipe support
      Version   = 1          ! geometry version
*
      KMountId  = 31.34      ! id of beampipe support kinematic mount
      KMountOd  = 38.96      ! od of beampipe support kinematic mount
      KMntThk   = 0.64       ! thickness of support kinematic mount
*
      KMCutOd   = 18.31      ! od of cutout in kinematic mount
      KMCutId   = 14         ! id of cutout in kinematic mount
      KMCutOA   = 38         ! opening angle of cutout
      KMCutOff  = 26.58      ! offset of cutout center from axis
*
      SRingId   = 8.47       ! id of beampipe support ring
      SRingOd   = 12.78      ! od of beampipe support ring
      SRingThk  = 0.64       ! thichkness of beampipe support ring
*
      SRCutPhi  = 38         ! support ring cutout angle to z-axis
      SRCutWid  = 3.63       ! support ring cutout width
      SRCutOut  = 5.08       ! support ring cutout depth
      SRCutIn   = 3.5        ! support ring cutout start
      SRollId   = 0.2        ! support roller Id
      SRollOd   = 0.62       ! support roller Od
      SRollLen  = 2.54       ! support roller length
      SWireLen  = 5.08       ! support roller axis length
*
      MBlkHgh   = 0.97       ! mounting block height
      MBlkOWid  = 2.54       ! outer mounting block width
      MBlkOLen  = 1.27       ! outer mounting block length
      MBlkIWid  = 3.175      ! inner mounting block width
      MBlkILen  = 1.27       ! inner mounting block length
      MBlkORad  = 17.4       ! outer mounting block at radius
      MBlkIRad  = 5.42       ! inner mounting block at radius
      MRodDia   = 0.32       ! mounting support rod diameter
*
   EndFill
*
   do i=1,4
     rad_cones_in(i) = 8.5+2.60*(i-1)
     rad_cones_out(i)=15.0+0.85*(i-1)
   enddo

*
*------------------------------  SHALL WE BEGIN? -------------------------------------------
      USE SVTG
      USE SSUP version=SVTG_ConeVer
      USE SSUB


* introduce common materials here
*
*     G10 is about 60% SiO2 and 40% epoxy (stolen from ftpcgeo.g)
        Component Si  A=28.08  Z=14   W=0.6*1*28./60.
        Component O   A=16     Z=8    W=0.6*2*16./60.
        Component C   A=12     Z=6    W=0.4*8*12./174.
        Component H   A=1      Z=1    W=0.4*14*1./174.
        Component O   A=16     Z=8    W=0.4*4*16./174.
      Mixture   G10   Dens=1.7

      Create and Position SCON in Cave
      Create and Position SCON in Cave ThetaZ=180
* 
* SVT support rods -- previously incorrectly described as Be,
* carbon compound in reality
      Create    SROD  "Support rod"
      Position  SROD  y = ssup_rodDist+ssup_rodOD/2
      Position  SROD  y =-ssup_rodDist-ssup_rodOD/2
*
* The beampipe support
      Create and Position SBSP  in CAVE z = (ssup_RodLen/2- ssub_KMntThk/2)
      Create and Position SBSP  in CAVE z= -(ssup_RodLen/2- ssub_KMntThk/2),
                                        ThetaZ=180


*******************************************************************************
*
* ****************************************************************************
*
* ****************************************************************************
*
Block SCON is the Silicon tracker supporting cone mother volume
* There is a graphite-epoxy cone SGRA (guess = carbon) 
* On top of that there are cones of plastic and copper for Twin-Ax STAP and STAC
* On top of that there are 10 water hoses SHOS, with water inside SHSW
* On top of that there is a layer of mylar SCMY
* On top of that there is a layer of aluminum SCAL
*
* The parts of the cones are labeled 1 through 4 as you go away from the 
* interaction point 
*
      cone_thk1=ssup_GrphThk
      cone_thk2=cone_thk1+ssup_CabThk+2*ssup_HosRmx
      cone_thk2=cone_thk2+ssup_WrpMyThk+ssup_WrpAlThk
      Material  Air
      attribute SCON    seen=0 colo=1
      SHAPE     PCON    Phi1=0  Dphi=360  Nz=7,
      zi ={ssup_Cone1zmn,
           ssup_Rodlen/2, 
           ssup_Rodlen/2, 
           ssup_Rodlen/2+ssup_GrphThk, 
           ssup_Rodlen/2+ssup_GrphThk, 
	   ssup_Cone3zmx, 
	   ssup_Cone4zmx},
      Rmx={ssup_Con1IdMn+cone_thk1,
           ssup_Con1IdMn+cone_thk1, 
           ssup_Con3IdMn+cone_thk1,
           ssup_Con3IdMn+cone_thk1,
           ssup_Con3IdMn+cone_thk2,
	   ssup_Con4IdMn+cone_thk2,
	   ssup_Con4IdMx+cone_thk2},
      Rmn={ssup_Con1IdMn,
           ssup_Con1IdMn, 
           ssup_Con1IdMn,
           ssup_Con1IdMn,
           ssup_Con3IdMn, 
	   ssup_Con4IdMn,
	   ssup_Con4IdMx}
*
      Create    SGRA  " graphite/epoxy support cone (lowest layer)"
      Position  SGRA

      Create    SCMY  " support cone mylar wrap"
      Position  SCMY

EndBlock
*
*------------------------------------------------------------------------------
*
Block SGRA is the graphite/epoxy support cone
      Material   Carbon
      Attribute SGRA   Seen=1   Colo=6
      SHAPE     PCON   Phi1=0   Dphi=360   Nz=5,
      zi ={ssup_Rodlen/2,
           ssup_Rodlen/2+ssup_GrphThk,
           ssup_Rodlen/2+ssup_GrphThk,
           ssup_Cone3zmx,
           ssup_Cone4zmx},
      Rmx={ssup_Con3IdMn+ssup_GrphThk,
           ssup_Con3IdMn+ssup_GrphThk,
           ssup_Con3IdMn+ssup_GrphThk,
           ssup_Con4IdMn+ssup_GrphThk,
           ssup_Con4IdMx+ssup_GrphThk},
      Rmn={ssup_Con1IdMn,
           ssup_Con1IdMn,
           ssup_Con3IdMn,
           ssup_Con4IdMn,
           ssup_Con4IdMx}
EndBlock
*
*******************************************************************************
*
Block SBSP is the beampipe support mother volume
      Material Air
      Attribute SBSP Seen=0 Colo=1
      Shape TUBE rmin=svtg_RSizeMin rmax=ssub_KMountOd/2,
                 dz=ssub_KMntThk/2+ssub_MBlkHgh
      Create SAKM  " aluminum kinematic mount "
      Position SAKM

      Create SBMM " beampipe support mounting mother volume "

      Create SBRL " beampipe support ceramic roller "
      Create SBRX " beampipe support roller axis "

      do i=-1,1,2
      do j=0,1
      phi=i*ssub_SRCutPhi+180*j

      xbuf1=(ssub_KMountOD/4.0+(svtg_RSizeMin+ssub_SRollOd)/2.0)
      Position SBMM AlphaZ=Phi,
                    x= xbuf1*sin(degrad*Phi),
                    y=-xbuf1*cos(degrad*Phi),
                    z=-ssub_KMntThk/2-ssub_MBlkHgh/2

      xbuf2=svtg_RSizeMin+ssub_SRollOd/2
      Position SBRL alphaZ=phi-90  ort=ZXY _
                    x= xbuf2*sin(degrad*Phi),
                    y=-xbuf2*cos(degrad*Phi),
                    z=ssub_SRingThk/2+ssub_SRollId/2

      Position SBRX alphaZ=phi-90  ort=ZXY _
                    x= xbuf2*sin(degrad*Phi),
                    y=-xbuf2*cos(degrad*Phi),
                    z=ssub_SRingThk/2+ssub_SRollId/2
      enddo
      enddo

      Create SBSR  " g10 beampipe support ring "
      Position SBSR
EndBlock
*
*------------------------------------------------------------------------------
*
Block SBMM is the beampipe support mounting mother volume
      Material  Air
      Attribute SBMM Seen=0 Colo=1
      Shape BOX dx=ssub_MBlkIWid/2,
                dy=(ssub_KMountOD/2-svtg_RSizeMin-ssub_SRollOd)/2,
                dz=ssub_MBlkHgh/2
      Create SMRD " beampipe support mounting rod "
      xbuf=-(ssub_MBlkORad+ssub_MBlkIRad)/2+svtg_RSizeMin+ssub_SRollOd
      Position SMRD AlphaX=90,
                    y=xbuf+(ssub_KMountOD/2-svtg_RSizeMin-ssub_SRollOd)/2

      Create SBMO " outer beampipe support mounting block "
      xbuf=-ssub_MBlkORad+svtg_RSizeMin+ssub_SRollOd
      Position SBMO y=xbuf+(ssub_KMountOD/2-svtg_RSizeMin-ssub_SRollOd)/2

      Create SBMI " inner beampipe support mounting block "
      xbuf=-ssub_MBlkIRad+svtg_RSizeMin+ssub_SRollOd
      Position SBMI y=xbuf+(ssub_KMountOD/2-svtg_RSizeMin-ssub_SRollOd)/2
EndBlock
*
*------------------------------------------------------------------------------
*
Block SMRD is the aluminum rod carrying the beampipe support
      Material  Aluminium
      Attribute SMRD Seen=1 Colo=2
      Shape TUBE rmin=0.0 rmax=ssub_MRodDia/2,
                  dz=(ssub_MBlkORad-ssub_MBlkIRad+ssub_MBlkOLen)/2
EndBlock
*
*------------------------------------------------------------------------------
*
Block SBMO is the outer beampipe support mounting block
      Material G10
      Attribute SBMO Seen=1 Colo=3
      Shape BOX dx=ssub_MBlkOWid/2,
                dy=ssub_MBlkOLen/2,
                dz=ssub_MBlkHgh/2
EndBlock
*
*
*------------------------------------------------------------------------------
*
Block SBMI is the inner beampipe support mounting block
      Material G10 
      Attribute SBMI Seen=1 Colo=3
      Shape BOX dx=ssub_MBlkIWid/2,
                dy=ssub_MBlkILen/2,
                dz=ssub_MBlkHgh/2
Endblock
*
*
* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
* 
Block SAKM is the beampipe support aluminum kinematic mount
      Material Aluminium
      Attribute SAKM Seen=1 Colo=2
      Shape     TUBE rmin=ssub_KMountId/2 rmax=ssub_KMountOd/2,
		     dz=ssub_KMntThk/2
      Create    SCKM  " cutout in kinematic mount "
      Position  SCKM y=ssub_KMCutOff               Konly='MANY'
      Position  SCKM y=-ssub_KMCutOff  AlphaZ=180  Konly='MANY'
Endblock



Block SCKM is the cutout in the aluminum kinematic mount
      Material Air
      Attribute SCKM Seen=1 Colo=1
      Shape TUBS rmin=ssub_KMCutId/2 rmax=ssub_KMCutOd/2,
                 dz=ssub_KMntThk/2,
                 phi1=270-ssub_KMCutOA,
                 phi2=270+ssub_KMCutOA
EndBlock
*
*------------------------------------------------------------------------------
*
Block SBRL is the ceramic roller supporting the beampipe
* approximate MACOR with PYREX, the ceramic used in the ftpc
      Material  PYREX A=20.719  Z=10.307  Dens=2.23  RadL=12.6  AbsL=50.7
      Attribute SBRL  Seen=1 Colo=6
      Shape     TUBE  rmin=ssub_SRollId/2  rmax=ssub_SRollOd/2,
                       dy=ssub_SRollLen/2
EndBlock
*
*------------------------------------------------------------------------------
*
Block SBRX is the stainless steel roller axis
      Material  Iron
      Attribute SBRX Seen=1 Colo=2
      Shape TUBE rmin=0.0 rmax=ssub_SRollId/2,
                  dz=ssub_SWireLen/2
EndBlock
*
*
*
Block SBSR is the beampipe support G10 ring

      Material G10
      Attribute SBSR Seen=1 Colo=3
      Shape TUBE rmin=ssub_SRingId/2  rmax=ssub_SRingOd/2,
                 dz=ssub_SRingThk/2
      Create SBCR " cutout in beampipe support ring "

      xbuf=ssub_SRCutIn+(ssub_SRCutOut-ssub_SRCutIn)/2
      do i=-1,1,2
      do j=0,1
         phi=i*ssub_SRCutPhi+180*j
         Position SBCR x=xbuf*sin(degrad*phi)  y=-xbuf*cos(degrad*phi),
                       AlphaZ=phi              Konly='MANY'
      enddo
      enddo

EndBlock
*
*
*
Block SBCR is the cutout in the beampipe support G10 ring
      Material Air
      Attribute SBCR Seen=1 Colo=1
      Shape BOX dx=ssub_SRCutWid/2,
                dy=(ssub_SRCutOut-ssub_SRCutIn)/2,
                dz=ssub_SRingThk/2
EndBlock
*
*------------------------------------------------------------------------------
*
Block SCMY is a mylar wrap around the support cone
      roffset=ssup_GrphThk+ssup_CabThk+2.0*ssup_HosRmx
      Component C5     A=12  Z=6  W=5
      Component H4     A=1   Z=1  W=4
      Component O2     A=16  Z=8  W=2
      Mixture   Mylar  Dens=1.39
      Attribute SCMY   Seen=1   Colo=3
      SHAPE     PCON   Phi1=0   Dphi=360  Nz=3,
      zi ={ssup_Rodlen/2+ssup_GrphThk,
           ssup_Cone3zmx,
           ssup_Cone4zmx},
      Rmx={ssup_Con3IdMn+roffset+ssup_WrpMyThk,
           ssup_Con4IdMn+roffset+ssup_WrpMyThk,
           ssup_Con4IdMx+roffset+ssup_WrpMyThk},
      Rmn={ssup_Con3IdMn+roffset,
           ssup_Con4IdMn+roffset,
           ssup_Con4IdMx+roffset}
EndBlock
*
* ****************************************************************************
*
Block SROD is the SVT Carbon composite support rod

      Material  Carbon
      IF (ssup_version>=4.00) THEN
         Material CarbonFiber dens=ssup_carbonDens
      ENDIF
      Attribute SROD  Seen=1  Colo=1
      Shape     ELTU   p1=ssup_RodODx/2,
                       p2=ssup_RodOD/2,
                       dz=ssup_RodLen/2

      IF (ssup_version>=4.00) THEN
      
         Create   SRON "The support rod nomex core"
         Position SRON

      Else

         Create   SROH "The support rod hole"
         Position SROH

      EndIF

endblock

*
* ****************************************************************************
*


BLOCK SRON Is the creamy nomex filling

      Component C      A=12  Z=6  W=5
      Component H      A=1   Z=1  W=8
      Component O      A=16  Z=8  W=2
      Mixture   Nomex  Dens=ssup_nomexDens
      Attribute SRON seen=1 colo=5

      Shape     ELTU p1=ssup_rododx/2-ssup_carbonShell,
                     p2=ssup_rodod /2-ssup_carbonShell

      Create   SROI "The support rod inner shell"
      Position SROI

ENDBLOCK

BLOCK SROI Is the inner carbon fiber shell

      Material CarbonFiber
      Attribute SROI seen=1 colo=1
      Shape ELTU p1=ssup_rodidx/2+ssup_carbonShell,
                 p2=ssup_rodid /2+ssup_carbonShell

      Create   SROH "The support rod hole"
      Position SROH

ENDBLOCK

Block SROH is the hole in SROD

      Material  Air
      Attribute SROH  Seen=1  Colo=3
      Shape     ELTU   p1=ssup_RodIDx/2,
                       p2=ssup_RodID/2,
                       dz=ssup_RodLen/2

endblock
      End

