* $Id: svttgeo7.g,v 1.5 2008/11/19 04:08:35 perev Exp $
*
* $Log: svttgeo7.g,v $
* Revision 1.5  2008/11/19 04:08:35  perev
* updates to the corrected(vp) starsim
*
* Revision 1.4  2007/11/13 21:33:46  perev
* ALKAP fixed and innermost radius increased
*
* Revision 1.3  2006/06/26 17:29:52  potekhin
* Capturing the last version of the "Distorted" SVT
* geometry used in our alignment studies
*
* Revision 1.2  2006/03/28 20:25:33  potekhin
* Added a printout of ladder numbering as per volume_id,
* and zeroed out the theta in the second shell
*
* Revision 1.1  2006/03/28 16:58:13  potekhin
* We will experiment with SVT alignment going forward,
* and to properly steer it from geometry.g without too
* much private code, we need to have this in place for smooth
* compilation. This is the source file for the "distorted
* geometry of the SVT"
*
*
*******************************************************************************
*
************** PLEASE SEE SVTTGEO4 FOR PRIOR CVS LOG MESSAGES *****************
*
*******************************************************************************
Module  SVTTGEO7  is extension of the SVTTGEO6: clamshells and distortions
*-->Based on the SVTTGEO4 code

   Author  Maxim Potekhin
   Created 24 Mar 2006

*------------------------------------------------------------------------------
*  Division lines:                                                            *
*                 *********** separates major divisions                       *
*                 ----------- separates sub-divisions                         *
*                 - - - - - - separates sub-sub divisions                     *
+cde,AGECOM,GCONST,GCUNIT.
*     
      Content          SVTT,SLYD,SLSD,SLDI,STLI,STSI,SVTD,SBER,STAB,
                       STRU,SPCB,SRHC,SBWC,SWCM,SXAI,SXBI,
                       STRA,SSID,SSIR,
                       SELE,SWCH,SWCW,SWCS,SBOI,SAGP,SDYE,SECA,SIRP,
                       SIRT,SOER,SCON,SROD,SGRA,
                       STAP,STAC,SHLA,SHLB,SHMA,SHMB,SWHO,SHWA,
                       SCMY,SCAL,SWMM,SWMB,SWRP,SXRL,SYRU,
                       SWMT,SWMS,SWMW,SOTB,SITB,
                       SBRG,SBRM,SBRI,SOES,SIES,SOSM,SISM,SCRW,
                       SGLA,SFCW,SBSP,SAKM,SCKM,SBSR,SBCR,SBRX,SBRL,
                       SBMM,SBMO,SBMI,SMRD,SALM,SISH,SSSH,SOSH,
                       SCBM,SCBL,SFED,SPLS,SOUM,SOUR
*
      structure SVTG { Version,   Nlayer,    RsizeMin,  RsizeMax,
		       ZsizeMax,  Angoff, SupportVer,   ConeVer,
                       ifMany, Nmin}
*     
      structure SWCA { Version,   Length,
                       WaferWid,  WaferLen,  WaferThk,  RohaThk,
                       WafCarWd,  WafCarTh,  WaferGap,
                       Drift,     strutlen,
                       SensWid,   SensLen,   SensGir}
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

      Integer        iLayer,s,side,ilad,iwaf,i,j, iPos
      Real           ladthk,cone_thk1,cone_thk2,roffset,RsizeMax,deg,rad,c0
      Real	     cone_len,cone_sin,cone_cos,rmin,rmax,zmin,zmax,angle
      Real           xpos,ypos,zpos,clearance,rin,rou,elethk,tabLen,radmax
      Real           endrng_z,brack_z,screw_z,ir_rmin,ang,wafpckLen,dthk,radtilt
      Real           xbuf, phi, xbuf1, xbuf2
      Real           yPCB, A, CuThk, sq, tube_angle
      Real           radii(6), rad_cones_in(5),rad_cones_out(5), rad_offset, shield_phi(4)

      Real           trapY,ssidX,ssirY

      Real           xDisp, yDisp, aDisp, cS

      Integer        i_phi, checkRez, foo, lsub

      Real           xD(6,16), yD(6,16), aD(6,16)

      data xD / 0.020,-0.010, 0.010, 0.010, 0.020, 0.000,
                0.000, 0.000,-0.010, 0.000,-0.010, 0.000,
               -0.013, 0.000, 0.000, 0.000, 0.000, 0.000,
                0.000, 0.000, 0.000, 0.000, 0.000, 0.000,
                0.010, 0.000, 0.000, 0.000, 0.000, 0.000,
                0.000, 0.000, 0.000, 0.000,-0.010, 0.000,
                0.000, 0.000,-0.010, 0.000, 0.000,-0.025,
                0.000, 0.000, 0.000, 0.000,-0.010, 0.000,
                0.010, 0.000, 0.000, 0.000, 0.000, 0.000,
               -0.010, 0.000, 0.000, 0.000, 0.000, 0.000,
               -0.020, 0.000, 0.000, 0.000, 0.000, 0.000,
                0.000,-0.020, 0.000, 0.000, 0.000, 0.000,
                0.000, 0.000, 0.000, 0.000,-0.010, 0.000,
                0.000, 0.000,-0.010, 0.000, 0.000, 0.000,
                0.000, 0.000, 0.000, 0.000, 0.000, 0.000,
                0.000, 0.000, 0.000,-0.010, 0.000, 0.000/

      data yD / 0.000, 0.010, 0.005, 0.005, 0.000, 0.000,
                0.020, 0.000, 0.000, 0.000, 0.005, 0.000,
               -0.010, 0.000, 0.000, 0.000, 0.000, 0.000,
                0.020, 0.010,-0.020, 0.020, 0.000, 0.000,
                0.000, 0.000,-0.010, 0.000, 0.000, 0.000,
                0.000, 0.000, 0.000, 0.000, 0.000, 0.000,
                0.000, 0.000, 0.000,-0.010, 0.000,-0.011,
                0.000, 0.000,-0.010, 0.000, 0.000, 0.000,
                0.000, 0.000, 0.000, 0.000, 0.000, 0.000,
               -0.020, 0.000, 0.000, 0.020, 0.000, 0.000,
               -0.010, 0.000,-0.010, 0.000, 0.000, 0.000,
               -0.010, 0.000, 0.000, 0.000, 0.000, 0.000,
                0.000, 0.000,-0.010, 0.000, 0.000, 0.000,
               -0.005, 0.000,-0.010, 0.000, 0.000,-0.010,
                0.000, 0.000,-0.010, 0.000, 0.000, 0.000,
                0.000, 0.000, 0.000, 0.010, 0.010, 0.030/

      data aD / 0.200, 0.000, 0.100, 0.200,-0.200, 0.300,
                0.000, 0.300, 0.000, 0.300, 0.000, 0.200,
                0.200, 0.000, 0.000,-0.100, 0.000, 0.100,
                0.000, 0.300,-0.200, 0.000, 0.000, 0.000,
                0.000, 0.000,-0.200, 0.000, 0.000, 0.000,
                0.000, 0.000, 0.000, 0.300, 0.000, 0.000,
                0.000, 0.000, 0.000,-0.200, 0.000,-0.200,
                0.000, 0.000, 0.000, 0.000, 0.000, 0.000,
                0.000, 0.000, 0.000, 0.000, 0.000, 0.000,
                0.000, 0.000, 0.000, 0.000, 0.000, 0.000,
                0.000, 0.000, 0.000, 0.000, 0.000, 0.000,
                0.000, 0.000, 0.000, 0.000, 0.000, 0.000,
                0.000, 0.000, 0.000, 0.000, 0.000, 0.000,
                0.000, 0.000, 0.000, 0.000, 0.000, 0.000,
                0.000, 0.000, 0.000, 0.000, 0.000, 0.000,
                0.000, 0.000, 0.000, 0.000, 0.000, 0.000/

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
      RsizeMin  = 4.100      ! STV innermost radius
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
      WaferGap  = 0.0        ! inter-wafer gap - there is a 50 micron uncertainty
      Drift     = 1          ! drift direction
      strutlen  = 1.0        ! len (z) of strut between waf car. rails (approx)
      SensWid   = 6.000      ! Define the sensitive square width  (new)
      SensLen   = 6.000      ! Define the sensitive square length (new)
      SensGir   = 5.775      ! Girth (for lack of a better word)
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
        Component Al  A=27    Z=13 W=0.2302
      Mixture  ALKAP  Dens=1.432
* use aluminized mylar mixture for ssd shield
        Component C5  A=12    Z=6  W=5
        Component H4  A=1     Z=1  W=4
        Component O2  A=16    Z=8  W=2
        Component Al  A=27    Z=13 W=0.0986
      Mixture  SSDALMY Dens=1.40845
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
* End rings to support the ladders:
*
      Create    SIRP  " inner end ring polygon piece "  
      Position  SIRP  Z=serg_EndRngZm+serg_EndRngTh/2  AlphaZ=22.5
      Position  SIRP  Z=-serg_EndRngZm-serg_EndRngTh/2 AlphaZ=22.5
      Create    SIRT  " inner end ring tube piece "  
      Position  SIRT  Z=serg_EndRngZm+serg_EndRngTh/2  AlphaZ=22.5
      Position  SIRT  Z=-serg_EndRngZm-serg_EndRngTh/2 AlphaZ=22.5
      Create    SOER  " outer end ring"  
      Position  SOER  Z=serg_EndRngZm+serg_EndRngTh/2 
      Position  SOER  Z=-serg_EndRngZm-serg_EndRngTh/2 
*
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
* Water manifold
*
      if (swam_Len > 0) then
      Create    SWMM  " water manifold mother"
      Position  SWMM  z= swam_Zmin+swam_Len/2
      Position  SWMM  z=-swam_Zmin-swam_Len/2 
      endif
*
* Bracket connecting water minifold to support cone 
* (guess, this is not designed yet)
*
      if (swam_Len > 0) then
      Create    SBWC " water manifold to support cone bracket mother"
      Position  SBWC z= (swam_Zmin+swam_Len+ _
                        (ssup_Cone1zmn-(swam_Zmin+swam_Len))/2)
      Position  SBWC z=-(swam_Zmin+swam_Len+ _
                        (ssup_Cone1zmn-(swam_Zmin+swam_Len))/2),
                     ThetaZ=180
      endif
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
* carbon compound in reality
      Create    SROD  "Support rod"
      Position  SROD  y = ssup_rodDist+ssup_rodOD/2
      Position  SROD  y =-ssup_rodDist-ssup_rodOD/2
*
* The SVT layers 
      radmax=svtg_rSizeMax
      Do ilayer = svtg_Nmin, min(6,nint(svtg_Nlayer))
         if (ilayer<6) then
            USE SVTL layer=ilayer+1
            radmax=svtl_radius
         else
            radmax=svtg_rSizeMax
         endif
         USE SVTL layer=ilayer
         Create   SLYD  " layer mother " 
         if(ilayer.eq.2.or.ilayer.eq.4.or.ilayer.eq.6) then
	   Position SLYD konly='MANY'
         else
	   Position SLYD
         endif
      EndDo
* 
* The first (test) layer
*
      if (svtg_Nlayer<0) then
         USE SVTL layer=-svtg_Nlayer
         Create   SLYD  " layer mother " 
	 Position SLYD
      endif         
*
* The shield cylinders
*
      Create and position SISH " SVT inner shield "
      Create and position SOSH " SVT outer shield "
      if (svtg_Nlayer>6) then
         Create and position SSSH " SVT/SSD separation shield "
      endif
      Create SALM  " aluminum shield mesh "
      Position SALM z=ssld_AlMshPos-ssld_AlMshThk/2
      Position SALM z=-(ssld_AlMshPos-ssld_AlMshThk/2)

* The bundles of cables connecting PCBs with the transition boards:
* modeled as conical layers of equivalent mass, withing their own
* mother volume
      Create SCBM "Mother of All Cables"
      Position SCBM x=0.0 y=0.0 z=  SWAM_Zmin-SCBP_Len
      Position SCBM x=0.0 y=0.0 z=-(SWAM_Zmin-SCBP_Len) ThetaZ=180.0

* Circular water feeds
      use svtl layer=6
      Create SXRL
      Position SXRL x=0.0 y =0.0 z= SWCA_Length/2.0-SWCX_Length/2.0-SWCX_offset konly='MANY'
      Position SXRL x=0.0 y =0.0 z=-SWCA_Length/2.0+SWCX_Length/2.0+SWCX_offset konly='MANY'

      use svtl layer=4
      Create SXRL
      Position SXRL x=0.0 y =0.0 z= SWCA_Length/2.0-SWCX_Length/2.0-SWCX_offset konly='MANY'
      Position SXRL x=0.0 y =0.0 z=-SWCA_Length/2.0+SWCX_Length/2.0+SWCX_offset konly='MANY'

      use svtl layer=2
      Create SXRL
      Position SXRL x=0.0 y =0.0 z= SWCA_Length/2.0-SWCX_Length/2.0-SWCX_offset konly='MANY'
      Position SXRL x=0.0 y =0.0 z=-SWCA_Length/2.0+SWCX_Length/2.0+SWCX_offset konly='MANY'

* Outer shileding structure
      Create SOUM
      Position SOUM x=0.0 y=0.0 z=0.0
      Position SOUM x=0.0 y=0.0 z=0.0 AlphaY=180.0
      Position SOUM x=0.0 y=0.0 z=0.0 AlphaY=180.0 AlphaZ=180
      Position SOUM x=0.0 y=0.0 z=0.0 AlphaZ=180

*
EndBlock
*
*******************************************************************************
*
Block SXRL is the mother of the circular water pipes
      Material Air
      Attribute SXRL seen=0 colo=1

      Shape TUBE _
      rmin=SWCX_rOffset+SVTL_Radius _
      rmax=SWCX_rOffset+SVTL_Radius+SWCX_dR _
      dz=SWCX_Length/2.0

      Create SWRP
      Create SYRU

      do i_phi=1,4*SVTL_Nladder
         tube_angle=(pi/(2.0*SVTL_Nladder))*(i_phi-0.5)

         Position SWRP _
         x=cos(tube_angle)*(SVTL_radius+SWCX_rOffset+SWCX_dR/2.0) _
         y=sin(tube_angle)*(SVTL_radius+SWCX_rOffset+SWCX_dR/2.0) _
         z=0.0

         Position SYRU _
         x=cos(tube_angle)*(SVTL_radius+SWCX_rOffset+SWCX_dR/2.0) _
         y=sin(tube_angle)*(SVTL_radius+SWCX_rOffset+SWCX_dR/2.0) _
         z=0.0

      enddo

EndBlock
*******************************************************************************
*
Block SWRP is an approximation of water in the circular pipe, a rectangular one
      Material Water
      Attribute SWRP    seen=1    colo=4

      Shape TUBE rmin=0.0 rmax=SWCX_rad dz=SWCX_Length/2.0

EndBlock
*
*******************************************************************************
*
Block SYRU is the  wall of the water pipe
      Component C      A=12  Z=6  W=1
      Component H2     A=1   Z=1  W=2
      Mixture   CH2    Dens=0.935

      Attribute SYRU    seen=1    colo=4
      Shape TUBE rmin=SWCX_rad rmax=SWCX_rad+SWCX_wall dz=SWCX_Length/2.0


EndBlock
*
*******************************************************************************
*
*Block SYTD is an approximation of the lower wall of the water pipe
*      Component C      A=12  Z=6  W=1
*      Component H2     A=1   Z=1  W=2
*      Mixture   CH2    Dens=0.935

*      Attribute SYTD    seen=1    colo=4
*
*      Shape TUBE _
*      rmin=SWCX_rOffset+SVTL_Radius-SWCX_wall _
*      rmax=SWCX_rOffset+SVTL_Radius _
*      dz=SWCX_Length/2.0+2.0*SWCX_wall
*
*EndBlock
*
*******************************************************************************
*
*Block SWCF is an approximation of the side wall of the water pipe
*      Component C      A=12  Z=6  W=1
*      Component H2     A=1   Z=1  W=2
*      Mixture   CH2    Dens=0.935
*
*      Attribute SWCF    seen=1    colo=4
*
*      Shape TUBE _
*      rmin=SWCX_rOffset+SVTL_Radius  rmax=SWCX_rOffset+SVTL_Radius+SWCX_dR dz=SWCX_wall/2.0

*EndBlock
*******************************************************************************
*
Block SOUM is the mother of the array of the outer shileding support tubes
      Material Air
      Attribute SOUM    seen=0   colo=1
      Shape TUBS Rmin=SOUP_Rout-SOUP_dR Rmax=SOUP_Rout _
      DZ=SOUP_Length/2.0 Phi1=SOUP_Phi1 Phi2=SOUP_Phi2

      Create SOUR
      do i_phi=1,4
      Position SOUR x=cos(shield_phi(i_phi)*pi/180.0)*(SOUP_Rout-SOUP_dR/2.0) _
                    y=sin(shield_phi(i_phi)*pi/180.0)*(SOUP_Rout-SOUP_dR/2.0) _
                    z=0.0
      enddo


EndBlock
*
*******************************************************************************
*
Block SOUR is the outer shileding support tubes (rods)
      Material Carbon
      Attribute SOUM seen=1 colo=4
      Shape TUBE Rmin=SOUP_DiamIn/2.0 _
		 Rmax=SOUP_DiamOut/2.0 _
                 dz=SOUP_Length/2.0
EndBlock
*
*******************************************************************************
*
Block SLYD is a single SVT layer  
*     Note: clearance between layers is very small. If you have to change
*     any of the thicknesses in ladthk or elethk, you should make SLYD
*     visible and check clearances. (wkw)

*     ladthk is the ladder HALF thickness, wafer is at mid-plane
      ladthk = swca_WaferThk/2+swca_RoHaThk+swca_WafCarTh

*     elethk is the electronics+electronic carrier full thickness
      elethk=2.0*selc_BeThk+selc_WatThk+selc_BeOThk+selc_AgPdThk
      elethk=elethk+selc_GlassThk+selc_DyeThk
      deg=180.0/svtl_Nladder
      rad=Pi/svtl_Nladder

      Material Air
      Attribute SLYD    seen=0    colo=1

* Note: in case of layer conflict (outer layer starts too early),
*       we can neglect some minor material loss in the corner of electronics,
*       but not in the center of sensitive wafers (this would change dE/dx). 
*       Make sure here that a layer ends up before the next layer starts

        rmin=svtl_radius-ladthk
        rmax=sqrt((svtl_radius)**2+(swca_WaferWid/2)**2)+elethk
        if (rmax>radmax-ladthk) print *,' SVT warning: layer overlap = ',
                                ilayer,rmax,radmax-ladthk
        rmax=min(rmax,radmax-ladthk)
        if (Rmax<Rmin)   print *,' SVT error: bad geometry parameters =',
                                ilayer,rmin,rmax
      Shape    TUBE rmin=rmin  rmax=rmax  dz=swca_Length/2

      c0=90-deg*mod(ilayer,2)
      write(*,*) 'nladder ',svtl_Nladder,' c0:',c0

      do iPos=1,svtl_Nladder
          xDisp=0.0 ! xD(iLayer,iPos)
          yDisp=0.0 ! yD(iLayer,iPos)
          aDisp=aD(iLayer,iPos)

          lsub    = mod(iLayer-1,2)

          checkRez =  svtl_Nladder*2.0-(iPos-1)*2-lsub

          foo =  svtl_Nladder

          Create and Position SLSD X=xDisp, Y=yDisp, _
                        AlphaZ=c0+(360/svtl_Nladder)*(iPos-1)+(180/svtl_Nladder) kOnly='MANY'
      enddo

EndBlock
*
* ----------------------------------------------------------------------------
*
Block SLSD is a single ladder mother (sector of tube) 
      Attribute SLSD    seen=1    colo=1  
      Attribute SLSD    Serial=iLayer*10+iPos

*>>>
      if (svtg_Nlayer<0) then
        Shape  Division   Iaxis=2   Ndiv=1    c0=-90
      else
        if (svtg_version==1) then
           c0=-deg*mod(ilayer,2)+svtg_Angoff*int((ilayer-1)/2) 
        else
           c0=90-deg*mod(ilayer,2)
        endif
*        Shape  Division   Iaxis=2   Ndiv=svtl_Nladder c0=c0
*	write(*,*) 'nladder ',svtl_Nladder,' c0:',c0

        Shape TUBS Rmin=rmin, Rmax=rmax, DZ=swca_Length/2, Phi1=-180/svtl_Nladder, Phi2=180/svtl_Nladder

      endif

      Attribute SLSD  Serial=0    " do not inherit "
*>>>
*
* LADDER LADDER LADDER LADDER LADDER LADDER LADDER LADDER LADDER

      write(*,*) ' Ladder Positioning: iPos',iPos,'  lsub: ', _
      lsub,' svtl_Nladder: ',foo,' result: ',  checkRez,' ',xDisp,' ',yDisp,' ',aDisp

      Create SLDI
      Position SLDI x=svtl_radius  AlphaZ=aDisp      ORT=YZX
*     Electronics go on both sides of the ladder
      Create   SELE

*     Position electronics carrier on left side of ladder (y pos)
      deg=180.0/svtl_Nladder
      rad=(TwoPi/2)/svtl_Nladder

      xpos=sin(rad)*selc_ElcaWid/2-cos(rad)*elethk/2
      ypos=cos(rad)*selc_ElcaWid/2+sin(rad)*elethk/2

      do s=-1,1,2
          cS=(s*deg+aDisp)
	  Position SELE ORT=YZX , x=svtl_radius-ladthk-xpos, y=s*(swca_WaferWid/2+ypos), AlphaX=90*(1-s), AlphaZ=cS 
      EndDo
EndBlock
*
* -----------------------------------------------------------------------------
*
Block SLDI is a ladder volume

* This contains the active wafer, Roha cell support, and the Be waffer carrier
* The center of the active wafer is at the center of this cell, the cell is
* centered on svtl_radius. 
      tabLen=swca_Length/2-7*(swca_WaferWid/2+swca_WaferGap)

      Material  Air
      Attribute SLDI   seen=0    colo=1

* - Volume created here
      Shape     BOX    dx=swca_WaferWid/2 dy=swca_Length/2  dz=ladthk

      Create and Position STLI
*         
*     SBER are the Berillium wafer carrier rails                     
      Create   SBER " wafer carrier rails"
      Position SBER x=+swca_WaferWid/2-swca_WafCarWd/2,
                    z=-ladthk+swca_WafCarTh/2 
      Position SBER x=-swca_WaferWid/2+swca_WafCarWd/2,
                    z=-ladthk+swca_WafCarTh/2 
*
*     STAB are the Berrillium tabs and the ends of the wafer carriers
      Create   STAB " wafter carrier end tabs"
      Position STAB y=swca_Length/2-tabLen/2,
                    z=-ladthk+swca_WafCarTh/2 
      Position STAB y=-swca_Length/2+tabLen/2,
                    z=-ladthk+swca_WafCarTh/2 

* ------------------------------------------------------------------------
*     STRU are the Berrillium struts between the wafer carriers rails
*     note: the positioning of these struts is approximate

      Create   STRU " struts between wafer carrier rails"
      Position STRU y=(svtl_Nwafer*(swca_WaferLen/2+swca_WaferGap)+ _
                       swca_WaferGap+swca_strutlen/2),
                    z=-ladthk+swca_WafCarTh/2 
      Position STRU y=-(svtl_Nwafer*(swca_WaferLen/2+swca_WaferGap)+ _
                       swca_WaferGap+swca_strutlen/2),
                    z=-ladthk+swca_WafCarTh/2 
* ------------------------------------------------------------------------

*     Roha cell spacers support the chips
      Create   SRHC " roha cell wafer supports"
      Position SRHC x=+swca_WaferWid/2-swca_WafCarWd/2,
                    z=-ladthk+2.0*swca_WafCarTh/2+swca_RoHaThk/2
      Position SRHC x=-swca_WaferWid/2+swca_WafCarWd/2,
                    z=-ladthk+2.0*swca_WafCarTh/2+swca_RoHaThk/2

      Create SPCB "G10"

      yPCB = SVTL_Nwafer*(swca_WaferLen+swca_WaferGap)/2.0 + SVTL_PcbGap + SVTL_PcbLen/2.0

      Position SPCB x=0.0 y= yPCB z=SVTL_PcbThk/2.0
      Position SPCB x=0.0 y=-yPCB z=SVTL_PcbThk/2.0 AlphaZ=180.0

*      Position SPCB y=SVTL_Nwafer*(swca_WaferLen+swca_WaferGap)/2.0 + _
*                      SVTL_PcbGap + _
*                      SVTL_PcbLen/2.0

*                         (swca_Length-svtl_Nwafer* _
*                         (swca_WaferLen+swca_WaferGap))/4.0
EndBlock
* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Block SRHC is the roha cell wafer support
* The material here is a guess, but the density is correct
      Component C      A=12  Z=6  W=1
      Component H2     A=1   Z=1  W=2
      Mixture   ROHA   Dens=0.0304
      Attribute SRHC   Seen=1   Colo=3
      Shape     BOX    dx=swca_WafCarWd/2,
                       dy=svtl_Nwafer*(swca_WaferLen/2+swca_WaferGap),
                       dz=swca_RoHaThk/2
EndBlock
* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
*
* Contains all the wafers in a single ladder, all conjoined:
Block STLI is the wafer pack container
      Attribute STLI   serial=0   seen=0    colo=1

      Shape     BOX    dy=svtl_Nwafer*(swca_WaferLen/2+swca_WaferGap), dz=swca_WaferThk/2

      Create    STSI
EndBlock
* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
*
* Note how the shape is division
*
Block STSI is a single waver container
      Shape   division     Iaxis=2  Ndiv=svtl_Nwafer
      Create and Position  SVTD 
EndBlock
* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Block SVTD is an active wafer volume
      Material  Silicon
      Material  SensSi  Isvol=1       

      Attribute SVTD       seen=1  Colo=4

      Shape     BOX        dy=swca_WaferLen/2 
*
      trapY=swca_SensWid/2.0-(swca_SensWid-swca_SensGir)/2.0

      Create STRA; Position STRA y=+trapY Ort=YZX AlphaZ=90; Position STRA  y=-trapY Ort=YZX AlphaZ=-90

*
      ssidX=swca_WaferLen/2.0-(swca_WaferLen-swca_SensLen)/4.0
      ssirY=swca_WaferWid/2.0-(swca_WaferWid-swca_SensWid)/4.0

      Create SSID; Position SSID x=+ssidX; Position SSID x=-ssidX
      Create SSIR; Position SSIR y=+ssirY; Position SSIR y=-ssirY

      call      GSTPAR (%Imed,'STRA',1.)

*     The following is the corrected hits definition: 25-dec-98 (PN)
      HITS    SVTD   Z:.001:S  Y:.001:   X:.001:     Ptot:18:(0,100),
                     cx:10:    cy:10:    cz:10:      Sleng:16:(0,500),
                     ToF:16:(0,1.e-6)    Step:.01:   Eloss:22:(0,0.01) 

EndBlock
* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Block SSID is a non-sensitive left-right border of the wafer
      Material  Silicon
      Material  NonsensSi  Isvol=0

      Attribute SSID       seen=1  Colo=1
      Shape BOX dx=(swca_WaferLen-swca_SensLen)/4.0 dy=swca_WaferWid/2.0
EndBlock

* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Block SSIR is a non-sensitive up-down border of the wafer
      Material  Silicon
      Material  NonsensSil  Isvol=0

      Attribute SSIR       seen=1  Colo=1
      Shape BOX dx=swca_SensLen/2.0 dy=(swca_WaferWid-swca_SensWid)/4.0
EndBlock

* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Block STRA is a trapezoid of triangular shape
      Material  Silicon
      Material  NonsensSili  Isvol=0

      Attribute STRA    seen=1  Colo=2

      SHAPE     TRD1 dx1=0.0,
                     dx2=swca_SensWid/2.0,
                     dy =swca_WaferThk/2.0,
                     dz =(swca_SensLen-swca_SensGir)/2.0

EndBlock
* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Block SBER are the Berillium wafer carrier rails
      Material  Berillium 
      Attribute SBER     Seen=1   Colo=2
      Shape     BOX      dx=swca_WafCarWd/2,
                         dy=swca_Length/2,  
                         dz=swca_WafCarTh/2
EndBlock
* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Block STAB are the Berillium wafer carrier end tabs
      Material  Berillium 
      Attribute STAB     Seen=1   Colo=2
      Shape     BOX      dx=swca_WaferWid/2-swca_WafCarWd,
                         dy=tabLen/2,
                         dz=swca_WafCarTh/2
EndBlock
* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Block STRU are the Berillium struts between the wafer carrier rails
      Material  Berillium 
      Attribute STRU     Seen=1   Colo=2
      Shape     BOX      dx=swca_WaferWid/2-swca_WafCarWd,
                         dy=swca_strutlen/2,
                         dz=swca_WafCarTh/2
EndBlock
*
* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Block SPCB is the G10 PCB
      Material  G10
      Attribute SPCB     Seen=1   Colo=5
      Shape     BOX      dx=SVTL_PcbWidth/2.0,
                         dy=SVTL_PcbLen/2.0,
                         dz=SVTL_PcbThk/2.0
EndBlock
*
* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Block SCBM is the mother for the bundle of cables going from PCBs
      Material  Air
      Attribute SCBM   Seen=0   Colo=1
      Shape TUBE Rmin=radii(1), Rmax=SWAM_Rmax, Dz=SCBP_Len

* The bundles of cables connecting PCBs with the transition boards:

      Do ilayer=1,4
         Use SCBP Layer=ilayer
         create SCBL "bundles of cables"
         Position SCBL x=0.0 y=0.0 z=0.0
      enddo

      Do ilayer=1,3
         Use SFEP Layer=ilayer
         create SFED "bundles of water pipes"
         Position SFED x=0.0 y=0.0 z=0.0

         create SPLS "plastic of the water pipes"
         Position SPLS x=0.0 y=0.0 z=0.0
      enddo


EndBlock
*------------------------------------------------------------------------------
*
Block SCBL is the bundle of cables going from PCBs to manifolds
      Material  Copper
      Attribute SCBL   Seen=1   Colo=3

      sq=SCBP_Len**2/(SCBP_Rmin2-SCBP_Rmin1)**2
      A=pi*(SCBP_Rmin1**2+SCBP_Rmin2**2)*sqrt(1+sq)

      CuThk=(SCBP_Vol/A)*sqrt(1.0+1.0/sq)
*      write(*,*) 'A, Th=',A,CuThk

      SHAPE     CONE   Dz  =SCBP_Len,
                       Rmn1=SCBP_Rmin1,
                       Rmx1=SCBP_Rmin1+CuThk,
                       Rmn2=SCBP_Rmin2,
                       Rmx2=SCBP_Rmin2+CuThk
*
endblock
*------------------------------------------------------------------------------
*
Block SFED is the watrer in the bundle of pipes going to manifolds
      Material  Water
      Attribute SFED   Seen=1   Colo=1

      sq=SFEP_Len**2/(SFEP_Rmin2-SFEP_Rmin1)**2
      A=pi*(SFEP_Rmin1**2+SFEP_Rmin2**2)*sqrt(1+sq)

      CuThk=(SFEP_Vol/A)*sqrt(1.0+1.0/sq)

      SHAPE     CONE   Dz  =SFEP_Len,
                       Rmn1=SFEP_Rmin1,
                       Rmx1=SFEP_Rmin1+CuThk,
                       Rmn2=SFEP_Rmin2,
                       Rmx2=SFEP_Rmin2+CuThk
*
endblock
*------------------------------------------------------------------------------
*
Block SPLS is the plastic walls of the bundle of pipes going to manifolds
      Component C      A=12  Z=6  W=1
      Component H2     A=1   Z=1  W=2
      Mixture   CH2    Dens=0.935
      Attribute SPLS   Seen=1   Colo=6

      sq=SFEP_Len**2/(SFEP_Rmin2-SFEP_Rmin1)**2
      A=pi*(SFEP_Rmin1**2+SFEP_Rmin2**2)*sqrt(1+sq)

      CuThk=(SFEP_Vol/A)*sqrt(1.0+1.0/sq)

      SHAPE     CONE   Dz  =SFEP_Len,
                       Rmn1=SFEP_Rmin1+0.2,
                       Rmx1=SFEP_Rmin1+0.2+CuThk,
                       Rmn2=SFEP_Rmin2+0.2,
                       Rmx2=SFEP_Rmin2+0.2+CuThk
*
endblock
*------------------------------------------------------------------------------
*
Block SELE is the electronics mother volume
      Material  Air  
*     --maxim--
*     Note: we need to make sure the SELE block is
*     replicated in each layer, not just poistioned
*     Therefore, we need to break the symmetry by adding the Serial attribute
      Attribute SELE  Seen=0  Colo=1  Serial=iLayer

      Shape     BOX   dx=selc_ElcaWid/2  dy=swca_Length/2  dz=elethk/2
*
*     build layers from bottom to top 
*     --maxim--
*     Note: now, we need to make sure we don't duplicate the
*     rest of the tree under SELE, hence we do not inherit:
      Attribute SELE  Serial=0    " do not inherit "

      Create    SWCH " water channel top/bottom"
      Position  SWCH z=-elethk/2+selc_BeThk/2
      Position  SWCH z=-elethk/2+selc_BeThk+selc_WatThk+selc_BeThk/2
      Create    SWCS " water channel side"
      Position  SWCS z=-elethk/2+selc_BeThk+selc_WatThk/2,
                     x=-selc_ElcaWid/2+selc_BeThk/2
      Position  SWCS z=-elethk/2+selc_BeThk+selc_WatThk/2,
                     x=+selc_ElcaWid/2-selc_BeThk/2
      Create    SWCW " water channel water"
      Position  SWCW z=-elethk/2+selc_BeThk+selc_WatThk/2
      Create    SBOI " BeO substrate for hybrid"
      Position  SBOI z=(elethk/2-selc_DyeThk-selc_AgPdThk _
                        -selc_GlassThk-selc_BeOThk/2)
      Create    SGLA " Glass insulating plane"
      Position  SGLA z=elethk/2-selc_DyeThk-selc_AgPdThk-selc_GlassThk/2
      Create    SAGP " Silver-Palladium Grounding plane"
      Position  SAGP z=elethk/2-selc_DyeThk-selc_AgPdThk/2
      Create    SDYE " ic chips"
      Position  SDYE z=elethk/2-selc_DyeThk/2,
                     x=selc_ElcaWid/2-selc_Dyespc-selc_Dyewid/2
      Position  SDYE z=elethk/2-selc_DyeThk/2,
                     x=selc_ElcaWid/2-2.0*selc_Dyespc-3.0*selc_Dyewid/2
      Create    SECA " cables on electronics carriers"
      Position  SECA z=elethk/2-selc_DyeThk+selc_CabThk/2,
                     x=-selc_ElcaWid/2+selc_CabWid/2
*
EndBlock
* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Block SDYE is the ic chip on the hybrid
      Material  Silicon
      Attribute SDYE   seen=1  colo=6
      Shape     BOX    dx=selc_DyeWid/2,
                       dy=svtl_Nwafer*(swca_WaferLen/2+swca_WaferGap),
                       dz=selc_DyeThk/2
EndBlock
* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Block SECA is the cable on the electronics carrier
      Material  Copper
      Attribute SECA   seen=1  colo=2
      Shape     BOX    dx=selc_CabWid/2,
                       dy=swca_Length/2,
                       dz=selc_CabThk/2
EndBlock
* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Block SBOI is the Berillia layer
      Component Be     A=9    Z=4   W=1
      Component O      A=16   Z=8   W=1
      Mixture   BeO    Dens=2.85
      Attribute SBOI   seen=1  colo=6
      Shape     BOX    dx=selc_ElcaWid/2,
	               dy=svtl_Nwafer*(swca_WaferLen/2+swca_WaferGap),
  		       dz=selc_BeOThk/2
EndBlock
* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Block SAGP is the Silver-Palladium layer
      Component Ag     A=108  Z=47  W=1
      Component Pd     A=106  Z=46   W=1
      Mixture   AgPd   Dens=11.25
      Attribute SAGP   seen=1  colo=2
      Shape     BOX    dx=selc_ElcaWid/2,
	               dy=svtl_Nwafer*(swca_WaferLen/2+swca_WaferGap),
  		       dz=selc_AgPdThk/2
EndBlock
* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Block SGLA is the insulating glass layer
      Component Si     A=28  Z=14  W=1
      Component O      A=16   Z=8  W=2
      Mixture   glass  Dens=2.2
      Attribute SGLA   seen=1  colo=6
      Shape     BOX    dx=selc_ElcaWid/2,
	               dy=svtl_Nwafer*(swca_WaferLen/2+swca_WaferGap),
  		       dz=selc_GlassThk/2
EndBlock
* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Block SWCH is the Be top and bottom of the water channel
      Material  Berillium
      Attribute SWCH   seen=1  colo=2
      Shape     BOX    dx=selc_ElcaWid/2,
                       dy=swca_Length/2,
                       dz=selc_BeThk/2
EndBlock
* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Block SWCS is the Be side of the water channel
      Material  Berillium
      Attribute SWCS   seen=1  colo=2
      Shape     BOX    dx=selc_BeThk/2,
                       dy=swca_Length/2,
                       dz=selc_WatThk/2
EndBlock
* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Block SWCW is the water channel water (probably Evian?) 
*
      Material  Water
      Attribute SWCW   seen=1  colo=6
      Shape     BOX    dx=(selc_ElcaWid-2.0*selc_BeThk)/2,
                       dy=swca_Length/2,
                       dz=(selc_WatThk/2)
EndBlock
*
* ****************************************************************************
*
Block SIRT is the SVT inner end ring tube
*
      Material  Berillium
      Attribute SIRT  Seen=1  Colo=2
      ir_rmin=serg_IrngPrMn*(cos(pi/8.)+sqrt(tan(pi/8.)**2.-sin(pi/8.)**2.))
      Shape     TUBE  rmin=ir_rmin,
                      rmax=serg_IrngTrMx,
                      dz=serg_EndRngTh/2
Endblock
*
* ****************************************************************************
*
Block SIRP is the SVT inner end ring polycone (overlaps tube)
* The inner surface of the polycone is inscribed within the inner tube radius
* The outer surface of the polycone is inscribed within the outer tube radius
*
      Material  Berillium
      Attribute SIRP  Seen=1  Colo=2
      rou=serg_IrngTrMx/(cos(pi/8.)+sqrt(tan(pi/8.)**2.-sin(pi/8.)**2.))
      Shape     PGON  Phi1=0   Dphi=360  Nz=2,
                      NpDiv=8,
                      zi ={-serg_EndRngTh/2, +serg_EndRngTh/2},
                      rmn={ serg_IrngPrMn, serg_IrngPrMn},
                      rmx={ rou, rou}
Endblock
*
* ****************************************************************************
*
Block SOER is the SVT outer end ring
      Material  Berillium
      Attribute SOER  Seen=1  Colo=2
      Shape     TUBE   rmin=serg_OrngRmin,  
                       rmax=serg_OrngRmax,  
                       dz=serg_EndRngTh/2
Endblock
*
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
*------------------------------------------------------------------------------
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
*******************************************************************************
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
*
*******************************************************************************
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
      Create    SBRM " Mother volume for a bracket joining the end rings"     
Endblock
*
* ----------------------------------------------------------------------------
*
Block SBRM is a the mother of a single bracket joining the end rings
*     The rotation c0 is a guess
      Attribute SBRM    seen=0    colo=1
      Shape  Division   Iaxis=2   Ndiv=4  c0=45
*
      Create    SBRI
      Position  SBRI x=ssup_ERJrad
*
EndBlock
*
* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
*
Block SBRI is the bracket which joins the rings
*     This is a major simplification of a complex shape
      Attribute SBRI   Seen=1    Colo=2
      Material Berillium
      Shape    BOX  dx=ssup_ERJlen/2,
                    dy=ssup_ERJwid/2,
                    dz=ssup_ERJThk/2
EndBlock
*
* ****************************************************************************
*
Block SROD is the SVT Carbon composite support rod
      Material  Carbon
      Attribute SROD  Seen=1  Colo=2
      Shape     TUBE   rmin=ssup_RodID/2,
                       rmax=ssup_RodOD/2,
                       dz=ssup_RodLen/2
endblock
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
      Create    STAP  " twinax cable approximation, plastic"
      Position  STAP
      Create    STAC  " twinax cable approximation, copper"
      Position  STAC
      Create    SHLA  " water hose cone 3 layer"
      Position  SHLA  z=ssup_Rodlen/2+ssup_GrphThk+_
                      0.5*(ssup_Cone3zmx-ssup_Rodlen/2-ssup_GrphThk)
      Create    SHLB  " water hose cone 4 layer"
      Position  SHLB  z=ssup_Cone3zmx+0.5*(ssup_Cone4zmx-ssup_Cone3zmx) 
      Create    SCMY  " support cone mylar wrap"
      Position  SCMY
      Create    SCAL  " aluminization on support cone mylar wrap (top layer)"
      Position  SCAL
endblock
*
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
Block STAP is the plastic part of the twin-ax cable layer (guess polyethylene)
      roffset=ssup_GrphThk
      Component C      A=12  Z=6  W=1
      Component H2     A=1   Z=1  W=2
      Mixture   CH2    Dens=0.935
      Attribute STAP   Seen=1   Colo=3
      SHAPE     PCON   Phi1=0   Dphi=360  Nz=3,
      zi ={ssup_Rodlen/2+ssup_GrphThk, 
	   ssup_Cone3zmx, 
	   ssup_Cone4zmx},
      Rmx={ssup_Con3IdMn+roffset+ssup_CabThk/2,
	   ssup_Con4IdMn+roffset+ssup_CabThk/2,
	   ssup_Con4IdMx+roffset+ssup_CabThk/2},
      Rmn={ssup_Con3IdMn+roffset, 
	   ssup_Con4IdMn+roffset,
	   ssup_Con4IdMx+roffset}
endblock
*
*------------------------------------------------------------------------------
*
Block STAC is the copper part of the twin-ax cable layer
      roffset=ssup_GrphThk+ssup_CabThk/2
      Material  Copper
      Attribute STAC   Seen=1   Colo=2
      SHAPE     PCON   Phi1=0   Dphi=360  Nz=3,
      zi ={ssup_Rodlen/2+ssup_GrphThk, 
	   ssup_Cone3zmx, 
	   ssup_Cone4zmx},
      Rmx={ssup_Con3IdMn+roffset+ssup_CabThk/2,
	   ssup_Con4IdMn+roffset+ssup_CabThk/2,
	   ssup_Con4IdMx+roffset+ssup_CabThk/2},
      Rmn={ssup_Con3IdMn+roffset, 
	   ssup_Con4IdMn+roffset,
	   ssup_Con4IdMx+roffset}
endblock
*
*------------------------------------------------------------------------------
*
Block SHLA is the water hose layer for cone 3 (closer to vertex)
      roffset=ssup_GrphThk+ssup_CabThk
      Material  Air
      Attribute SHLA   Seen=0   Colo=1
      SHAPE     CONE   Dz=.5*(ssup_Cone3zmx-ssup_Rodlen/2-ssup_GrphThk),
                       Rmn1=ssup_Con3IdMn+roffset,
                       Rmx1=ssup_Con3IdMn+roffset+2.0*ssup_HosRmx,
                       Rmn2=ssup_Con4IdMn+roffset,
                       Rmx2=ssup_Con4IdMn+roffset+2.0*ssup_HosRmx
*
      Create   SHMA  " mother volumes for the water hoses on cone 3"
endblock
* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Block SHMA is a single mother volume for a water hose on the cone 3
*     These are the divisions for the water hoses, rotation c0=guess
      roffset=ssup_GrphThk+ssup_CabThk
      rmin=ssup_Con3IdMn+roffset
      rmax=ssup_Con4IdMn+roffset
      zmin=ssup_Rodlen/2+ssup_GrphThk
      zmax=ssup_Cone3zmx
      cone_len=sqrt((zmax-zmin)**2.+(rmax-rmin)**2.)
      cone_sin=(rmax-rmin)/cone_len
      cone_cos=(zmax-zmin)/cone_len
      angle=asin(cone_sin)*180/pi
      xpos=rmin+0.5*(rmax-rmin)+ssup_HosRmx*cone_cos
      zpos=-ssup_HosRmx*cone_sin
*
      Attribute  SHMA   Seen=0    Colo=1
      Shape  Division   Iaxis=2   Ndiv=ssup_Nhoses c0=0.0
*
      Create   SWHO  " water hose on cone 3"
      Position SWHO  x=xpos y=0 z=zpos AlphaY=angle

endblock
*
*------------------------------------------------------------------------------
*
Block SHLB is the water hose layer cone 4 (further from vertex)
      roffset=ssup_GrphThk+ssup_CabThk
      Material  Air
      Attribute SHLB   Seen=0   Colo=1
      SHAPE     CONE   Dz=.5*(ssup_Cone4zmx-ssup_Cone3zmx),
                       Rmn1=ssup_Con4IdMn+roffset,
                       Rmx1=ssup_Con4IdMn+roffset+2.0*ssup_HosRmx,
                       Rmn2=ssup_Con4IdMx+roffset,
                       Rmx2=ssup_Con4IdMx+roffset+2.0*ssup_HosRmx
*
      Create   SHMB  " mother volumes for the water hoses on cone 4"
endblock
* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Block SHMB is a single mother volume for a water hose on the cone 4
*     These are the divisions for the water hoses, rotation c0=guess

      roffset=ssup_GrphThk+ssup_CabThk
      rmin=ssup_Con4IdMn+roffset
      rmax=ssup_Con4IdMx+roffset

      zmin=ssup_Cone3zmx
      zmax=ssup_Cone4zmx

      cone_len=sqrt((zmax-zmin)**2.+(rmax-rmin)**2.)
      cone_sin=(rmax-rmin)/cone_len
      cone_cos=(zmax-zmin)/cone_len

      angle=asin(cone_sin)*180/pi

      xpos=rmin+0.5*(rmax-rmin)+ssup_HosRmx*cone_cos
      zpos=-ssup_HosRmx*cone_sin
*
      Attribute  SHMB   Seen=0    Colo=1
      Shape  Division   Iaxis=2   Ndiv=ssup_Nhoses c0=0.0
*
      Create   SWHO  " water hose on cone 4"
      Position SWHO  x=xpos y=0 z=zpos AlphaY=angle
 
endblock
*
*------------------------------------------------------------------------------
*
Block SWHO is a water hose 
* note: fix hose length a la Pavel's example
      Attribute  SWHO   Seen=1    Colo=3
*     the material is a guess
      Component C      A=12  Z=6  W=1
      Component H2     A=1   Z=1  W=2
      Mixture   CH2    Dens=0.935
      Attribute SWHO   Seen=1   Colo=3
*
      Shape    TUBE rmin=0  rmax=ssup_HosRmx  dz=0.5*cone_len
      Create   SHWA  " hoses water on cone 4"
      Position SHWA  
EndBlock
* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Block SHWA is the water in the hose 
* note: fix hose length a la Pavel's example
      Attribute  SHWA   Seen=1    Colo=6
      Material   Water
      Shape      TUBE   rmax=ssup_HosRmn

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
endblock
*
*------------------------------------------------------------------------------
*
Block SCAL is the aluminization on the mylar wrap around the support cone
*     note combine with mylar a la Pavel's example
      roffset=ssup_GrphThk+ssup_CabThk+2.0*ssup_HosRmx+ssup_WrpMyThk
      Material  Aluminium
      Attribute SCAL   Seen=1   Colo=2
      SHAPE     PCON   Phi1=0   Dphi=360  Nz=3,
      zi ={ssup_Rodlen/2+ssup_GrphThk, 
	   ssup_Cone3zmx, 
	   ssup_Cone4zmx},
      Rmx={ssup_Con3IdMn+roffset+ssup_WrpAlThk,
	   ssup_Con4IdMn+roffset+ssup_WrpAlThk,
	   ssup_Con4IdMx+roffset+ssup_WrpAlThk},
      Rmn={ssup_Con3IdMn+roffset, 
	   ssup_Con4IdMn+roffset,
	   ssup_Con4IdMx+roffset}
endblock
*
*******************************************************************************
*
Block SWMM is the water manifold mother
      rin=swam_Rmin-swam_TbrdThk
      rou=swam_Rmax+swam_TbrdThk
      Material  Air
      Attribute SWMM  seen=0  colo=1

      Shape     PGON  Phi1=0   Dphi=360  Nz=2,
                      NpDiv=18,
                      zi ={-swam_Len/2,+swam_Len/2},
                      rmn={rin,rin},
                      rmx={rou,rou}
*
      Create   SWMB  " water manifold bottom"
      Position SWMB
      Create   SWMT  " water manifold top"
      Position SWMT
      Create   SWMS  " water manifold side"
      Position SWMS  z=-swam_Len/2+swam_WallThk/2
      Position SWMS  z= swam_Len/2-swam_WallThk/2
*
      Create   SWMW  " water manifold water"
      Position SWMW  
*
      Create   SOTB  " outer transition board"
      Position SOTB
      Create   SITB  " inner transition board"
      Position SITB
Endblock
*
*------------------------------------------------------------------------------
*
Block SWMB is the water manifold bottom piece (small r)
      Material  Aluminium
      Attribute SWMB   Seen=1   Colo=2
      Shape     PGON  Phi1=0   Dphi=360  Nz=2,
                      NpDiv=18,
                      zi ={-swam_Len/2+swam_WallThk,
                            swam_Len/2-swam_WallThk},
                      rmn={swam_Rmin,swam_Rmin},
                      rmx={swam_Rmin+swam_WallThk,swam_Rmin+swam_WallThk}
Endblock
*
*------------------------------------------------------------------------------
*
Block SWMT is the water manifold top piece (big r)
      Material  Aluminium
      Attribute SWMT   Seen=1   Colo=2
      Shape     PGON  Phi1=0   Dphi=360  Nz=2,
                      NpDiv=18,
                      zi ={-swam_Len/2+swam_WallThk,
                            swam_Len/2-swam_WallThk},
                      rmn={swam_Rmax-swam_WallThk,swam_Rmax-swam_WallThk},
                      rmx={swam_Rmax,swam_Rmax}
Endblock
*
*------------------------------------------------------------------------------
*
Block SWMS is the water manifold side pieces
      Material  Aluminium
      Attribute SWMS   Seen=1   Colo=2
      Shape     PGON  Phi1=0   Dphi=360  Nz=2,
                      NpDiv=18,
                      zi ={-swam_WallThk/2,swam_WallThk/2},
                      rmn={swam_Rmin,swam_Rmin},
                      rmx={swam_Rmax,swam_Rmax}
Endblock
*
*------------------------------------------------------------------------------
*
Block SWMW is the water in the water manifold
      Attribute SWMW   seen=1  colo=6
      Material  Water 
      Shape     PGON  Phi1=0   Dphi=360  Nz=2,
                      NpDiv=18,
                      zi ={-swam_Len/2+swam_WallThk,
                            swam_Len/2-swam_WallThk},
                      rmn={swam_Rmin+swam_WallThk,swam_Rmin+swam_WallThk},
                      rmx={swam_Rmax-swam_WallThk,swam_Rmax-swam_WallThk}
Endblock
*
*------------------------------------------------------------------------------
*
Block SOTB is the outer transition board (large r)
      material  G10
      Attribute SOTB   Seen=1   Colo=3
      Shape     PGON  Phi1=0   Dphi=360  Nz=2,
                      NpDiv=18,
                      zi ={-swam_Len/2,+swam_Len/2},
                      rmn={swam_Rmax,swam_Rmax},
                      rmx={swam_Rmax+swam_TbrdThk,swam_Rmax+swam_TbrdThk}
Endblock
*
*------------------------------------------------------------------------------
*
Block SITB is the inner transition board (small r)
*     The material is CH2
       Component C      A=12  Z=6  W=1
       Component H2     A=1   Z=1  W=2
      Mixture   CH2    Dens=0.935
      Attribute SITB   Seen=1   Colo=3
      Shape     PGON  Phi1=0   Dphi=360  Nz=2,
                      NpDiv=18,
                      zi ={-swam_Len/2,+swam_Len/2},
                      rmn={swam_Rmin-swam_TbrdThk,swam_Rmin-swam_TbrdThk},
                      rmx={swam_Rmin,swam_Rmin}
Endblock
*
*******************************************************************************
*
Block SBWC is the bracket connecting the water manifold to the cone
      rin=swam_Rmin
      rou=ssup_Con1IdMn
      Material  Air
      Attribute SBWC  seen=0  colo=1
      Shape     TUBE  Rmin=rin,
		      Rmax=rou,
                      dz=(ssup_Cone1zmn-(swam_Zmin+swam_Len))/2
*     
      Create    SWCM " Mother volume for a bracket between mani and cone"     
Endblock
*
* ----------------------------------------------------------------------------
*
Block SWCM is a single bracket mother between mani and cone
      Attribute SWCM    seen=0    colo=1
      Shape  Division   Iaxis=2   Ndiv=3 c0=0
*
      Create    SXAI
      Position  SXAI  Z=(-(ssup_Cone1zmn-(swam_Zmin+swam_Len))/2 _
                        +ssup_BraThk/2) 
      Create    SXBI
      Position  SXBI  Z=(ssup_BraThk/2) 
*
EndBlock
*
* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
*
Block SXAI is a first piece (A) of the bracket between mani and cone (X)
      Attribute SXAI   Seen=1    Colo=2
      Material   Aluminium
      Shape    TUBS rmin=swam_Rmin,
                    rmax=ssup_Con1IdMn,
                    dz=ssup_BraThk/2,
                    phi1=-5,
		    phi2=5
EndBlock
*
* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
*
Block SXBI is a second piece (B) of the bracket between mani and cone (X)
      Attribute SXBI   Seen=1    Colo=2
      Material   Aluminium
      Shape    TUBS rmin=ssup_Con1IdMn-ssup_BraThk,
                    rmax=ssup_Con1IdMn,
                    dz=((ssup_Cone1zmn-(swam_Zmin+swam_Len)- _
                         ssup_BraThk)/2),
                    phi1=-5,
		    phi2=5
EndBlock
* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Block SCRW is the screw which attaches the end ring to the end ring bracket
*     The material is a guess, checking with Chong-Jer
      Material  Berillium
      Attribute SCRW   Seen=1  Colo=2
      Shape     TUBE   rmin=0,
                       rmax=ssup_ERJdia/2,  
                       dz=0.5*(brack_z-endrng_z)
Endblock
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
*
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
*
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
*------------------------------------------------------------------------------
*
Block SALM is the aluminum shield mesh
      Material  Aluminium
      Attribute SALM Seen=1 Colo=2
      Shape TUBE rmin=ssld_AlMeshId/2 rmax=ssld_AlMeshOd/2,
                  dz=ssld_AlMshThk/2
Endblock
*
*------------------------------------------------------------------------------
*
Block SISH is the inner shield cylinder
* use aluminised mylar mixture instead of kapton
      material  ALKAP
      Attribute SISH Seen=1 Colo=3
      Shape TUBE rmin=ssld_SInRInn rmax=ssld_SInROut,
                  dz=ssld_SInLen
Endblock
*
*------------------------------------------------------------------------------
*
Block SSSH is the separation shield cylinder
* use aluminised mylar mixture instead of kapton
      material  SSDALMY
      Attribute SSSH Seen=1 Colo=3
      Shape TUBE rmin=ssld_SSepRInn rmax=ssld_SSepROut,
                  dz=ssld_SSepLen
Endblock
*
*------------------------------------------------------------------------------
*
Block SOSH is the separation shield cylinder
* use aluminised mylar mixture instead of kapton
      material  SSDALMY
      Attribute SOSH Seen=1 Colo=3
      Shape TUBE rmin=ssld_SOutRInn rmax=ssld_SOutROut,
                  dz=ssld_SOutLen
Endblock
*
*------------------------------------------------------------------------------
*
      End

