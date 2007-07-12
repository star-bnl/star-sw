* $Id: svttgeo9.g,v 1.1 2007/07/12 20:13:24 potekhin Exp $
*
* $Log: svttgeo9.g,v $
* Revision 1.1  2007/07/12 20:13:24  potekhin
* Support cones were traditionally located in the
* SVT hierarchy of the geometry model; thus, even without the
* svt, we are using same prototype code and location
* in CVS; this cut is needed as the first approximation
* of the SVT-less configuration in year 2008 onward; previous
* version in svvttgeo5 still contained water hoses and copper
* on the cones which are really extraneous in this case,
* hence it's better to cut a new version.
*
*******************************************************************************
*
************** PLEASE SEE SVTTGEO5 FOR PRIOR CVS LOG MESSAGES *****************
*
*******************************************************************************
Module  SVTTGEO9  is the SVT-less geometry of the support cones of STAR without the SVT
*-->Based on the SVTTGEO5 code

   Author  Maxim Potekhin
   Created 12 July 2007

*------------------------------------------------------------------------------
*  Division lines:                                                            *
*                 *********** separates major divisions                       *
*                 ----------- separates sub-divisions                         *
*                 - - - - - - separates sub-sub divisions                     *
+cde,AGECOM,GCONST,GCUNIT.
*     
      Content          SVTT,SCON,SGRA,STAP,
                       SBRG,SOES,SOSM,SCRW,SIES,SISM,
                       SBSP,SAKM,SBMM,SMRD,SBMO,SBMI,SCKM,SBRL,SBRX,
                       SBSR,SBCR,SCMY
* SIRT, SBRM, SIRP, SROD, SBRI, SOER - left out of this cut
*
      structure SVTG { Version,   Nlayer,    RsizeMin,  RsizeMax,
		       ZsizeMax,  Angoff, SupportVer,   ConeVer,
                       ifMany, Nmin}
*     
      structure SWCA { Version,   Length,
                       WaferWid,  WaferLen,  WaferThk,  RohaThk,
                       WafCarWd,  WafCarTh,  WaferGap,
                       Drift,     strutlen}
*
      structure SSUP { Version,   CabThk,    HosRmn,
		       HosRmx,    Nhoses,    WrpMyThk,  WrpAlThk,    
                       GrphThk,   Cone1Zmn,  RodLen,    RodDist,
                       RodID,     RodOD,     Con1IdMn, Con3IdMn,  
                       Con4IdMn, Con4IdMx, Cone3zmx,  Cone4zmx,  
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
      structure SWAM { Version,   Zmin,      Len,       Rmin, 
                       Rmax,      TbrdThk,   WallThk} 
*
      structure SERG { Version,   IrngTrMx, IrngPrMn, OrngRmin, 
                       OrngRmax,  EndRngTh, EndRngZm}
*
      structure SELC { Version,   BeThk,     WatThk,    BeOThk,  
                       DyeThk,    DyeWid,    DyeSpc,    ElcaWid, 
                       AgPdThk,   GlassThk,  CabThk,    CabWid}
*
      Structure SVTL { Layer,    Nladder,  Nwafer,   Radius,
                       BareEdge, PcbLen,   PcbWidth, PcbThk, PcbGap }
*
*
      structure SSLD { Version,  SInRInn,  SInROut,  SInLen,
                       SSepRInn, SSepROut, SSepLen,
                       SOutRInn, SOutROut, SOutLen,
                       AlMeshId, AlMeshOd, AlMshThk, AlMshPos} 
*     
      structure SCBP { Layer,Len,Rmin1,Rmax1,Rmin2,Rmax2,Vol}
      structure SFEP { Layer,Len,Rmin1,Rmax1,Rmin2,Rmax2,Vol,VolPlast}
      structure SWCX { Layer,Length,dR, offset, rad, wall, rOffset}
      structure SOUP { Version,Length,Rout,dR,Phi1,Phi2,DiamOut, DiamIn}

      Integer        iLayer,s,side,ilad,iwaf,i,j
      Real           ladthk,cone_thk1,cone_thk2,roffset,RsizeMax,deg,rad,c0
      Real	     cone_len,cone_sin,cone_cos,rmin,rmax,zmin,zmax,angle
      Real           xpos,ypos,zpos,clearance,rin,rou,elethk,tabLen,radmax
      Real           endrng_z,brack_z,screw_z,ir_rmin,ang,wafpckLen,dthk,radtilt
      Real           xbuf, phi, xbuf1, xbuf2
      Real           yPCB, A, CuThk, sq, tube_angle
      Real           radii(6), rad_cones_in(5),rad_cones_out(5), rad_offset, shield_phi(4)
      Integer        i_phi

*******************************************************************************
* Turns out it's more convenient to store these here:
      radii(1)= 6.37
      radii(2)= 7.38
      radii(3)=10.38
      radii(4)=11.27
      radii(5)=14.19
      radii(6)=15.13
** Important: the radial offset -- we position by the center of the volume,
** hence half the wafer thicknes should be added. Remember to change it if
** it will ever be anyhting other than 300 um.
      rad_offset=0.015
***************************
      shield_phi(1)=9.0
      shield_phi(2)=27.0
      shield_phi(3)=45.0
      shield_phi(4)=63.0
***************************
   Fill SVTG ! Basic SVT dimensions 
      Version   = 2          ! geometry version
      Nlayer    = 7          ! number of svt layers (was 7)
      RsizeMin  = 4.006      ! STV innermost radius
      RsizeMax  = 46.107     ! STV outermost radius
      ZsizeMax  = 270        ! SVT+FTPC length
      Angoff    = 0          ! angular offset x1 for slayer 2 x2 for slayer 3
      SupportVer= 1          ! versioning of the shield
      ConeVer=    1          ! versioning of the support cone
      ifMany    = 0          ! whether we use the geant MANY option
      Nmin      = 1          ! the index of the innermost layer
*
   Fill SWCA ! Wafer Carrier
      Version   = 1          ! geometry version
      Length    = 56.04      ! ladder length
      WaferWid  = 6.305      ! wafer width
      WaferLen  = 6.305      ! wafer length
      WaferThk  = 0.0300     ! wafer thickness
      RoHaThk   = 0.0381     ! Roha cell plus glue thickness 
      WafCarWd  = 1.5        ! wafer carrier rails width
      WafCarTh  = 0.0300     ! wafer carrier thickness 
      WaferGap  = 0.05       ! width of the inter-wafer gap (was 0 in prev versions)
      Drift     = 1          ! drift direction
      strutlen  = 1.0        ! len (z) of strut between waf car. rails (approx)
*
   Fill SSUP ! Support structures
      Version   = 1          ! geometry version
      CabThk    = 0.05       ! thickness of layer of cables on support cone
      HosRmn    = 0.75       ! inner radius of water hoses on support cone
      HosRmx    = 0.95       ! outer radius of water hoses on support cone
      Nhoses    = 10         ! number of water hoses
      WrpMyThk  = 0.10       ! thickness of mylar wrap around cone (guess)
      WrpAlThk  = 0.01       ! thickness of Al on mylar wrap (guess)
      GrphThk   = 0.16	     ! support cone thickness
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
      GrphThk   = 0.16	     ! support cone thickness
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
*------------------------------------------------------------
   Fill SERG ! end rings
      Version   = 1          ! geometry version
      IrngTrMx  = 9.703      ! Inner end ring tube maximum radius 
      IrngPrMn  = 7.671      ! Inner end ring polygon minimum radius
      OrngRmin  = 11.900     ! Outer end ring minimum radius
      OrngRmax  = 13.805     ! Outer end ring maximum radius
      EndRngTh  = 0.2        ! End ring thickness
      EndRngZm  = 23.01      ! minimum z for end rings
*
   Fill SWAM ! water manifold
      Version  = 1           ! geometry version
      Zmin     = 33.9        ! minimum z for positioning water manifold
      Len      = 16.0        ! water manifold full length 
      Rmin     = 15.24       ! water manifold rmin (not including trans. brds)
      Rmax     = 16.83       ! water manifold rmax (not including trans. brds)
      TbrdThk  = 0.1         ! transition board thickness
      WallThk  = 0.1         ! water manifold wall thickness
*
   Fill SELC ! electronics carrier data
      Version  = 1           ! geometry version
      BeThk    = 0.0500      ! thickness of Berillium water channel walls
      WatThk   = 0.0750      ! thickness of water channel
      BeOThk   = 0.0500      ! thickness of Berrillia (BeO) substra
      DyeThk   = 0.0340      ! to give .11% of a radiation length of Si 
      DyeWid   = 0.3         ! width of ic chips (2 covers 0.3 of area)
      DyeSpc   = 0.1         ! space ic chips
      ElcaWid  = 2.0         ! electronics carrier width
      AgPdThk  = 0.0030      ! thickness of Ag-Pd conductor
      GlassThk = 0.0150      ! thickness of glass insulator
      CabThk   = 0.0033      ! Cu for cables of .23% rad len (weighted average)
      CabWid   = 0.6         ! cable width 
*
* important -- in the following, the 150 um radius correction
* corresponds to half thickness of the Silicon wafer,
* such that the surveyed data is translated into the middle
* of the volume. In the previous version, this number was 250 um
*
   Fill SVTL ! single layer data
      layer    = 1                    ! layer number
      Radius   = radii(1)+rad_offset  ! layer radius (center of wafer position)
      Nladder  = 4                    ! number of ladder
      Nwafer   = 4                    ! number of wafers
      BareEdge = 1.0                  ! the strip of bare Be which makes the PCB shorter
      PcbLen   = 14.9                 ! PCB Length
      PcbWidth = 6.3                  ! PCB Width
      PcbThk   = 0.1                  ! PCB Thickness -- should be 0.09, but we add extra for copper
      PcbGap   = 0.2                  ! Gap between the PCB and the Si Wafer

* Former analytical atempt:
*      PcbLen   = (swca_Length-SVTL_Nwafer*(swca_WaferLen+swca_WaferGap))/4.0 -SVTL_ BareEdge! PCB Length

*      
   Fill SVTL ! single layer parameters
      layer    = 2                    ! layer number
      Radius   = radii(2)+rad_offset  ! layer radius
      PcbLen   = 14.9                 ! PCB Length

*      
   Fill SVTL ! single layer parameters
      layer    = 3                   ! layer number
      Radius   = radii(3)+rad_offset ! layer radius
      Nladder  = 6                   ! number of ladder
      Nwafer   = 6                   ! number of wafers
      PcbLen   = 7.5                 ! PCB Length
*      
   Fill SVTL ! single layer parameters
      layer    = 4                   ! layer number
      Radius   = radii(4)+rad_offset ! layer radius
      PcbLen   = 7.5                 ! PCB Length
*      
   Fill SVTL ! single layer parameters
      layer    = 5                   ! layer number
      Radius   = radii(5)+rad_offset ! layer radius
      Nladder  = 8                   ! number of ladder
      Nwafer   = 7                   ! number of wafers
      PcbLen   = 4.4                 ! PCB Length
*      
   Fill SVTL ! single layer parameters
      layer    = 6                   ! layer number
      Radius   = radii(6)+rad_offset ! layer radius
      PcbLen   = 4.4                 ! PCB Length
*
   Fill SSLD ! shielding parameters
      version  = 1          ! geometry version
      SInRInn  = 5          ! inner shield cylinder, inner radius
      SInROut  = 5.008      ! inner shield cylinder, outer radius
      SInLen   = 53.5       ! inner shield cylinder, half length
      SSepRInn = 22         ! separation shield cylinder, inner radius
      SSepROut = 22.018     ! separation shield cylinder, outer radius
      SSepLen  = 55.4       ! separation shield cylinder, half length
      SOutRInn = 29.5       ! outer shield cylinder, inner radius
      SOutROut = 29.52      ! outer shield cylinder, outer radius
      SOutLen  = 65.4       ! outer shield cylinder, half length
      AlMeshId  = 9.7       ! Aluminum shield mesh inner diameter
      AlMeshOd  = 44        ! Aluminum shield mesh outer diameter
      AlMshThk  = 0.03      ! Aluminum shield mesh effective thickness
      AlMshPos  = 53.5      ! Aluminum shield mesh z position
   EndFill
***
* the following version of the shield will accomodate the PIXEL detector,
* so it needs to be slightly bigger
   Fill SSLD ! shielding parameters
      version  = 2          ! geometry version
      SInRInn  = 5.9        ! inner shield cylinder, inner radius
      SInROut  = 5.908      ! inner shield cylinder, outer radius
   EndFill
*
   do i=1,4
     rad_cones_in(i) = 8.5+2.60*(i-1)
     rad_cones_out(i)=15.0+0.85*(i-1)
   enddo

   Fill SCBP                ! Cabling
      Layer=1               ! Layer
      Len  =1.85            ! Length
      Rmin1=rad_cones_in(1) ! Min radius closer to wafers
      Rmin2=rad_cones_out(1)! Min radius further from wafers
      Vol  =7.24+3.21       ! Volume of copper, LV+HV cables
   EndFill
*
   Fill SCBP                ! Cabling
      Layer=2               ! Layer
      Rmin1=rad_cones_in(2) ! Min radius closer to wafers
      Rmin2=rad_cones_out(2)! Min radius further from wafers
      Vol  =15.54+5.7       ! Volume of copper, LV+HV cables
   EndFill
*
   Fill SCBP                ! Cabling
      Layer=3               ! Layer
      Rmin1=rad_cones_in(3) ! Min radius closer to wafers
      Rmin2=rad_cones_out(3)! Min radius further from wafers
      Vol  =4.05+2.02+3.67+1.69        ! Volume of copper, LV+HV cables -- 3+4 layers coalesce
   EndFill
*
*
   Fill SCBP                ! Cabling
      Layer=4               ! Layer (former 5th)
      Rmin1=rad_cones_in(4) ! Min radius closer to wafers
      Rmin2=rad_cones_out(4)! Min radius further from wafers
      Vol  =6.95+2.43       ! Volume of copper, LV+HV cables
   EndFill
***********************************************
   Fill SFEP                ! Water feed
      Layer=1               ! Layer
      Len  =1.85            ! Length
      Rmin1=0.5*(rad_cones_in(1) +rad_cones_in(2))    ! Min radius closer to wafers
      Rmin2=0.5*(rad_cones_out(1)+rad_cones_out(2))   ! Min radius further from wafers
      Vol  =16.0            ! Volume of water
      VolPlast  =38.4       ! Volume of plastic
   EndFill
*
   Fill SFEP                ! Water feed
      Layer=2               ! Layer
      Rmin1=0.5*(rad_cones_in(2) +rad_cones_in(3))    ! Min radius closer to wafers
      Rmin2=0.5*(rad_cones_out(2)+rad_cones_out(3))   ! Min radius further from wafers
      Vol  =24.0            ! Volume of water
      VolPlast  =57.6       ! Volume of plastic
   EndFill
*
   Fill SFEP                ! Water feed
      Layer=3               ! Layer
      Rmin1=0.5*(rad_cones_in(3) +rad_cones_in(4))    ! Min radius closer to wafers
      Rmin2=0.5*(rad_cones_out(3)+rad_cones_out(4))   ! Min radius further from wafers
      Vol  =32       ! Volume of water
      VolPlast  =76.8      ! Volume of plastic
   EndFill
*
***********************************************
   Fill SWCX                ! Segments of the water distribution pipes
      Layer=1               ! version
      Length =2.8           ! of the ring in the Z direction
      dR     =0.72          ! thickness of the mother layer
      rad    =0.2           ! inner plastic tube radius
      offset =-2.0          ! from the edge of the ladder support, inward
      rOffset=1.0           ! Radial offset
      wall   =0.16          ! thickness of the plastic pipe wall
   EndFill
*
   Fill SOUP                ! Mother of the outer shielding cage, parameters
      Version = 1           ! Version
      Length  = 82.5        ! Length
      Rout    = 19.41       ! Outer radius of the shield
      dR      = 0.711       ! Diameter of the tubes constituting the cage (also dR of the mother)
      Phi1    = 0.0         ! Starting angle of the mother
      Phi2    = 70.0        ! Finishing angle of the mother
      DiamOut = 0.711       ! Outer diam of the carbon tube
      DiamIn  = 0.620       ! Inner diam of the carbon tube
   EndFill
*
*------------------------------  SHALL WE BEGIN? -------------------------------------------
      USE SVTG
      USE SWCA
      USE SELC
      USE SSUP version=SVTG_ConeVer
      USE SSUB
      USE SERG
      USE SWAM
      USE SELC
      USE SSLD version=SVTG_SupportVer
      USE SCBP
      USE SWCX
      USE SOUP

      if(SVTG_SupportVer==2) then ! shrink the inner radius so that pixel fits in
         write(*,*) 'ATTENTION: THE INNER SVT RADIUS IS 6.0 CM'
         SVTG_RsizeMin = 6.0
      endif

      write(*,*) '********* SVT Support Cone Cable layer thickness: ', SSUP_CabThk

* introduce common materials here
*
*     G10 is about 60% SiO2 and 40% epoxy (stolen from ftpcgeo.g)
        Component Si  A=28.08  Z=14   W=0.6*1*28./60.
        Component O   A=16     Z=8    W=0.6*2*16./60.
        Component C   A=12     Z=6    W=0.4*8*12./174.
        Component H   A=1      Z=1    W=0.4*14*1./174.
        Component O   A=16     Z=8    W=0.4*4*16./174.
      Mixture   G10   Dens=1.7

* use aluminized mylar mixture instead of kapton
        Component C5  A=12    Z=6  W=5
        Component H4  A=1     Z=1  W=4
        Component O2  A=16    Z=8  W=2
        Component Al  A=27    Z=13 W=3
      Mixture  ALKAP  Dens=1.65
*
*     put real water only when manifold is installed
      Component H2     A=1   Z=1   W=2
      Component O      A=16  Z=8   W=1
      if (swam_Len>0) then
         Mixture   Water  Dens=1.0
      else
*        i don't care about the composition as soon as this is as light as air
         Mixture   Water  Dens=0.0009
      endif

      if(SVTG_ifMany>0) then ! custom positioning depending on pipe
           Create and Position SVTT in Cave Konly='MANY'
           write(*,*) '************** ATTENTION: SVT Positioned with MANY'
      else
           Create and Position SVTT in Cave
      endif
*
*******************************************************************************
*
Block SVTT is the mother of all SVT volumes
*
      RsizeMax=ssup_Con4IdMx
      RsizeMax=RsizeMax+ssup_GrphThk+ssup_CabThk+2.0*ssup_HosRmx
      RsizeMax=RsizeMax+ssup_WrpMyThk+ssup_WrpAlThk
*
      Clearance=svtg_RsizeMax-RsizeMax
      If (Clearance<0) print *,' SVTT max size error, clearance=',clearance
*      
      Material  Air
      Attribute SVTT  seen=0  colo=1
      Shape     TUBE  Rmin=svtg_RsizeMin,
		      Rmax=svtg_RsizeMax,
                      dz=svtg_ZsizeMax
*  wrong, if in common run:  dz=ssup_Cone4zmx
*



*  Excluded from this version :
* End rings to support the ladders:
*
*      Create    SIRP  " inner end ring polygon piece "  
*      Position  SIRP  Z=serg_EndRngZm+serg_EndRngTh/2  AlphaZ=22.5
*      Position  SIRP  Z=-serg_EndRngZm-serg_EndRngTh/2 AlphaZ=22.5
*
*      Create    SIRT  " inner end ring tube piece "  
*      Position  SIRT  Z=serg_EndRngZm+serg_EndRngTh/2  AlphaZ=22.5
*      Position  SIRT  Z=-serg_EndRngZm-serg_EndRngTh/2 AlphaZ=22.5
*      Create    SOER  " outer end ring"  
*      Position  SOER  Z=serg_EndRngZm+serg_EndRngTh/2 
*      Position  SOER  Z=-serg_EndRngZm-serg_EndRngTh/2 
***************************************************************************

* Bracket joining the end rings 
* (shape is very approximate guess)
*
      Create    SBRG " Bracket joining the end rungs"
      Position  SBRG z=  swca_Length/2+ssup_ERJzdis+ssup_ERJthk/2
      Position  SBRG z= -swca_Length/2-ssup_ERJzdis-ssup_ERJthk/2
*
* Mother volumes for the screws joining the bracket to the end ring
*
      endrng_z=serg_EndRngZm+serg_EndRngTh
      brack_z=swca_Length/2+ssup_ERJzdis
      screw_z=endrng_z+0.5*(brack_z-endrng_z)
      Create    SOES " Volume to hold outer endring screws"
      Position  SOES z= screw_z konly='MANY'
      Position  SOES z=-screw_z konly='MANY'
      Create    SIES " Volume to hold inner endring screws"
      Position  SIES z= screw_z konly='MANY'
      Position  SIES z=-screw_z konly='MANY'
* 
* SVT support cones
*
      Create    SCON  "Support cone mother"
      Position  SCON
      Position  SCON              ThetaZ=180 
*
* The beampipe support
      Create    SBSP  "Beampipe support mother "
      Position  SBSP z = (ssup_RodLen/2- ssub_KMntThk/2)
      Position  SBSP z= -(ssup_RodLen/2- ssub_KMntThk/2), ThetaZ=180

* 
* SVT support rods -- previously incorrectly described as Be,
* carbon compound in reality -- we leave it out altogether is the future construction
* isn't clear.
*      Create    SROD  "Support rod"
*      Position  SROD  y = ssup_rodDist+ssup_rodOD/2
*      Position  SROD  y =-ssup_rodDist-ssup_rodOD/2
*

*
EndBlock
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

*      Create    SCAL  " aluminization on support cone mylar wrap (top layer)"
*      Position  SCAL
endblock
*------------------------------------------------------------------------------
*
Block SGRA is the graphite/epoxy support cone
      Material   Carbon
      Attribute SGRA   Seen=1   Colo=6
      SHAPE     PCON   Phi1=0   Dphi=360   Nz=7,
      zi ={ssup_Rodlen/2, 
           ssup_Rodlen/2, 
           ssup_Rodlen/2+ssup_GrphThk, 
           ssup_Rodlen/2+ssup_GrphThk, 
	   ssup_Cone3zmx, 
	   ssup_Cone4zmx},
      Rmx={ssup_Con1IdMn+ssup_GrphThk, 
           ssup_Con3IdMn+ssup_GrphThk,
           ssup_Con3IdMn+ssup_GrphThk,
           ssup_Con3IdMn+ssup_GrphThk,
	   ssup_Con4IdMn+ssup_GrphThk,
	   ssup_Con4IdMx+ssup_GrphThk},
      Rmn={ssup_Con1IdMn, 
           ssup_Con1IdMn,
           ssup_Con1IdMn,
           ssup_Con3IdMn, 
	   ssup_Con4IdMn,
	   ssup_Con4IdMx}
endblock
*
*------------------------------------------------------------------------------
*
Block SBRG is the bracket joining the end rings
      rin=serg_IrngPrMn
      rou=serg_OrngRmax
      Material  Air
      Attribute SBRG  seen=0  colo=1
      Shape     TUBE  Rmin=rin,
		      Rmax=rou,
                      dz=ssup_ERJthk/2
*     
Endblock
*
*
*
Block SOES is the volume to hold outer endring screws
      Material  Air
      Attribute SOES seen=0 colo=1
      Shape     TUBE rmin=serg_OrngRmin,  
                     rmax=serg_OrngRmax,    
                     dz=0.5*(brack_z-endrng_z)
*
      Create    SOSM " Mother volume for outer end ring screws"
Endblock
*******************************************************************************
*
Block SIES is the volume to hold inner endring screws
      Material  Air
      Attribute SIES seen=0 colo=1
      Shape     TUBE  rmin=ir_rmin,
                      rmax=serg_IrngTrMx,
                      dz=0.5*(brack_z-endrng_z)
*
      Create    SISM " Mother volume for inner end ring screws"
Endblock
*
*******************************************************************************
*
Block SISM is the mother volume division for the inner end ring screws
*     The rotation c0 is a guess
      Attribute SISM   seen=0   colo=1
      Shape Division   Iaxis=2  Ndiv=4   c0=45
*
      Create    SCRW " Screw for attaching end rings to bracket"
      Position  SCRW x=ssup_ERJrad-ssup_ERJlen/2+ssup_ERJ1x
Endblock
*
*------------------------------------------------------------------------------
*
Block SOSM is the mother volume division for the outer end ring screws
*     The rotation c0 is a guess
      Attribute SOSM   seen=0   colo=1
      Shape Division   Iaxis=2  Ndiv=4   c0=45
*
      Create    SCRW " Screw for attaching end rings to bracket"
      Position  SCRW x=ssup_ERJrad+ssup_ERJlen/2-ssup_ERJ1x
      Position  SCRW x=ssup_ERJrad+ssup_ERJlen/2-ssup_ERJ2x
      Position  SCRW x=ssup_ERJrad+ssup_ERJlen/2-ssup_ERJ2x,
                     y=ssup_ERJ2y
      Position  SCRW x=ssup_ERJrad+ssup_ERJlen/2-ssup_ERJ2x,
                     y=-ssup_ERJ2y
Endblock

* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Block SCRW is the screw which attaches the end ring to the end ring bracket
*     The material is a guess, checking with Chong-Jer
      Material  Berillium
      Attribute SCRW   Seen=1  Colo=2
      Shape     TUBE   rmin=0,
                       rmax=ssup_ERJdia/2,  
                       dz=0.5*(brack_z-endrng_z)
Endblock
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
Endblock
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
Endblock
*------------------------------------------------------------------------------
*
Block SMRD is the aluminum rod carrying the beampipe support
      Material  Aluminium
      Attribute SMRD Seen=1 Colo=2
      Shape TUBE rmin=0.0 rmax=ssub_MRodDia/2,
                  dz=(ssub_MBlkORad-ssub_MBlkIRad+ssub_MBlkOLen)/2
Endblock
*
*------------------------------------------------------------------------------
*
Block SBMO is the outer beampipe support mounting block
      Material G10 
      Attribute SBMO Seen=1 Colo=3
      Shape BOX dx=ssub_MBlkOWid/2,
                dy=ssub_MBlkOLen/2,
                dz=ssub_MBlkHgh/2
Endblock
*------------------------------------------------------------------------------
*
Block SBMI is the inner beampipe support mounting block
      Material G10 
      Attribute SBMO Seen=1 Colo=3
      Shape BOX dx=ssub_MBlkIWid/2,
                dy=ssub_MBlkILen/2,
                dz=ssub_MBlkHgh/2
Endblock
*
* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
* 
Block SCKM is the cutout in the aluminum kinematic mount
      Material Air
      Attribute SCKM Seen=1 Colo=1
      Shape TUBS rmin=ssub_KMCutId/2 rmax=ssub_KMCutOd/2,
		 dz=ssub_KMntThk/2,
                 phi1=270-ssub_KMCutOA,
                 phi2=270+ssub_KMCutOA
Endblock
*------------------------------------------------------------------------------
*
Block SBRL is the ceramic roller supporting the beampipe
* approximate MACOR with PYREX, the ceramic used in the ftpc
      Material  PYREX A=20.719  Z=10.307  Dens=2.23  RadL=12.6  AbsL=50.7
      Attribute SBRL  Seen=1 Colo=6
      Shape     TUBE  rmin=ssub_SRollId/2  rmax=ssub_SRollOd/2,
		       dy=ssub_SRollLen/2
Endblock
*
*------------------------------------------------------------------------------
*
Block SBRX is the stainless steel roller axis
      Material  Iron
      Attribute SBRX Seen=1 Colo=2
      Shape TUBE rmin=0.0 rmax=ssub_SRollId/2,
                  dz=ssub_SWireLen/2
Endblock
*
* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
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

Endblock
*
* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
* 
Block SBCR is the cutout in the beampipe support G10 ring
      Material Air
      Attribute SBCR Seen=1 Colo=1
      Shape BOX dx=ssub_SRCutWid/2,
		dy=(ssub_SRCutOut-ssub_SRCutIn)/2,
		dz=ssub_SRingThk/2
Endblock
*
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
endblock
*
*------------------------------------------------------------------------------
*
      End

