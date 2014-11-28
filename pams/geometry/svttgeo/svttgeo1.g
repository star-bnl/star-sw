* $Id: svttgeo1.g,v 1.6 2009/03/31 02:25:57 perev Exp $
*
* $Log: svttgeo1.g,v $
* Revision 1.6  2009/03/31 02:25:57  perev
* Bug in SVTT.SRDA fixed
*
* Revision 1.5  2008/11/19 04:08:30  perev
*  updates to the corrected(vp) starsim
*
* Revision 1.4  2007/11/13 21:33:15  perev
* ALKAP fixed and innermost radius increased
*
* Revision 1.3  2003/08/05 23:38:23  potekhin
* Correct the comment typo
*
* Revision 1.2  2003/08/05 22:37:07  potekhin
* Added Id and Log tags
*
*
*
*******************************************************************************
Module  SVTTGEO1  is the SVT geometry for STAR
   Author  Maxim Potekhin
*   based on the SVTTGEO code, bug corrections
   created 05 Aug 03
*                                                                             *
*------------------------------------------------------------------------------
*  Division lines:                                                            *
*                 *********** separates major divisions                       *
*                 ----------- separates sub-divisions                         *
*                 - - - - - - separates sub-sub divisions                     *
+cde,AGECOM,GCONST,GCUNIT.
*     
      Content          SVTT,SLYD,SLSD,SLDI,STLI,STSI,SVTD,SBER,STAB,
                       STRU,SRHC,SBWC,SWCM,SXAI,SXBI,
                       SELE,SWCH,SWCW,SWCS,SBOI,SAGP,SDYE,SECA,SIRP,
                       SIRT,SOER,SCON,SROD,SGRA,
                       STAP,STAC,SHLA,SHLB,SHMA,SHMB,SWHO,SHWA,
                       SCMY,SCAL,SWMM,SWMB,SWMT,SWMS,SWMW,SOTB,SITB,
                       SBRG,SBRM,SBRI,SOES,SIES,SOSM,SISM,SCRW,
                       SGLA,SFMO,SFLM,SFDM,SFSW,SFSD,SFSM,
                       SFSS,SFCP,SFCF,SFCT,SFCX,SFCW,
                       SBSP,SAKM,SCKM,SBSR,SBCR,SBRX,SBRL,
                       SBMM,SBMO,SBMI,SMRD,SALM,SISH,SSSH,SOSH
*
      structure SVTG { Version,   Nlayer,    RsizeMin,  RsizeMax,
		       ZsizeMax,  Angoff}
*     
      structure SWCA { Version,   Length,
                       WaferWid,  WaferLen,  WaferThk,  RohaThk, 
                       WafCarWd,  WafCarTh,  WaferGap,  Drift,    
                       strutlen}
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
      Structure SVTL { Layer,    Nladder,  Nwafer,   Radius    }
*
      Structure SFPA { Version,  rmin,     rmax,     Len,
                       rad,      nssd,     dmWid,    dmThk,
		       dmLen,    smWid,    smThk,    smLen,
                       ssLen,    wpLen,    sdlen,    tilt,     
                       cprad,    cpral,    cfrad,    gpThk}
*
      structure SSLD { Version,  SInRInn,  SInROut,  SInLen,
                       SSepRInn, SSepROut, SSepLen,
                       SOutRInn, SOutROut, SOutLen,
                       AlMeshId, AlMeshOd, AlMshThk, AlMshPos} 
*     
      Integer        iLayer,s,side,ilad,iwaf,i,j
      Real           ladthk,cone_thk1,cone_thk2,roffset,RsizeMax,deg,rad,c0
      Real	     cone_len,cone_sin,cone_cos,rmin,rmax,zmin,zmax,angle
      Real           xpos,ypos,zpos,clearence,rin,rou,elethk,tabLen,radmax
      Real           endrng_z,brack_z,screw_z,ir_rmin,ang,wafpckLen,dthk,radtilt
      Real           xbuf, phi, xbuf1, xbuf2
*
*******************************************************************************
*
   Fill SVTG ! Basic SVT dimensions 
      Version   = 2          ! geometry version
      Nlayer    = 7          ! number of svt layers (was 7)
      RsizeMin  = 4.100      ! STV innermost radius
      RsizeMax  = 46.107     ! STV outermost radius
      ZsizeMax  = 270        ! SVT+FTPC length
      Angoff    = 0          ! angular offset x1 for slayer 2 x2 for slayer 3
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
      WaferGap  = 0.0        ! width of the inter-wafer gap      
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
      RodID     = 3.5        ! ID of Be support rods
      RodOD     = 3.81       ! OD of Be support rods
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
*
   Fill SSUB ! beampipe support
      Version   = 1          ! geometry version
      KMountId  = 31.34      ! id of beampipe support kinematic mount 
      KMountOd  = 38.96      ! od of beampipe support kinematic mount 
      KMntThk   = 0.64       ! thickness of support kinematic mount
      KMCutOd   = 18.31      ! od of cutout in kinematic mount
      KMCutId   = 14         ! id of cutout in kinematic mount
      KMCutOA   = 38         ! opening angle of cutout
      KMCutOff  = 26.58      ! offset of cutout center from axis
      SRingId   = 8.47       ! id of beampipe support ring 
      SRingOd   = 12.78      ! od of beampipe support ring
      SRingThk  = 0.64       ! thichkness of beampipe support ring
      SRCutPhi  = 38         ! support ring cutout angle to z-axis
      SRCutWid  = 3.63       ! support ring cutout width
      SRCutOut  = 5.08       ! support ring cutout depth
      SRCutIn   = 3.5        ! support ring cutout start
      SRollId   = 0.2        ! support roller Id
      SRollOd   = 0.62       ! support roller Od
      SRollLen  = 2.54       ! support roller length
      SWireLen  = 5.08       ! support roller axis length
      MBlkHgh   = 0.97       ! mounting block height
      MBlkOWid  = 2.54       ! outer mounting block width
      MBlkOLen  = 1.27       ! outer mounting block length
      MBlkIWid  = 3.175      ! inner mounting block width
      MBlkILen  = 1.27       ! inner mounting block length
      MBlkORad  = 17.4       ! outer mounting block at radius
      MBlkIRad  = 5.42       ! inner mounting block at radius
      MRodDia   = 0.32       ! mounting support rod diameter
*
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
* important -- in the following, the 150 um correction
* corresponds to half thickness of the Silicon wafer,
* such that the surveyed data is translated into the middle
* of the volume. In the previous version, this number was 250 um
   Fill SVTL ! single layer data
      layer    = 1           ! layer number
      Radius   = 6.37+0.015  ! layer radius (center of wafer position)
      Nladder  = 4           ! number of ladder
      Nwafer   = 4           ! number of wafers
*      
   Fill SVTL ! single layer parameters
      layer    = 2           ! layer number
      Radius   = 7.38+0.015  ! layer radius
*      
   Fill SVTL ! single layer parameters
      layer    = 3           ! layer number
      Radius   = 10.38+0.015 ! layer radius
      Nladder  = 6           ! number of ladder
      Nwafer   = 6           ! number of wafers
*      
   Fill SVTL ! single layer parameters
      layer    = 4           ! layer number
      Radius   = 11.27+0.015 ! layer radius
*      
   Fill SVTL ! single layer parameters
      layer    = 5           ! layer number
      Radius   = 14.19+0.015 ! layer radius
      Nladder  = 8           ! number of ladder
      Nwafer   = 7           ! number of wafers
*      
   Fill SVTL ! single layer parameters
      layer    = 6           ! layer number
      Radius   = 15.13+0.015 ! layer radius
*
   Fill SFPA ! fourth layer parameters
      version  = 1          ! geometry version
      rmin     = 22.0       ! mother rmin
      rmax     = 28.0       ! mother rmax
      Len      = 100.       ! mother Len
      rad      = 23.        ! distance from beam axis to detector center
      nssd     = 16         ! number of silicon strip detectors 
      dmWid    = 7.5        ! detector mother width 
      dmThk    = 0.03       ! detector mother thickness
      dmLen    = 90.        ! detector mother length (detectors + adc board) 
      smWid    = 7.5        ! structure mother width
      smThk    = 3.5        ! structure mother thickness
      smLen    = 95.        ! structure mother length (cool. pipe+carbon fiber)
      ssLen    = 95./20.    ! length of a subvolume of the structure
      wpLen    = 68.8       ! length of wafer pack
      sdlen    = 4.2        ! lenght of one strip detector (along beam axis)
      tilt     = 5.0        ! tiling angle (degrees)
      cprad    = 0.1        ! cooling pipe outer radius
      cpral    = 0.09       ! cooling pipe inner radius
      cfrad    = 0.1        ! carbon fiber tube radius (support structure)
      gpThk    = 0.5        ! gap between structure mother and detector
*
   Fill SSLD ! shielding parameters
      version  = 1          ! geometry version
      SInRInn  = 5          ! inner shield cylinder, inner radius
      SInROut  = 5.008      ! inner shield cylinder, outer radius
      SInLen   = 53.5       ! inner shield cylinder, half length
      SSepRInn = 22         ! separation shield cylinder, inner radius
      SSepROut = 22.018     ! separation shield cylinder, outer radius
      SSepLen  = 55.4       ! separation shield cylinder, half length
      SOutRInn = 28         ! outer shield cylinder, inner radius
      SOutROut = 28.02      ! outer shield cylinder, outer radius
      SOutLen  = 65.4       ! outer shield cylinder, half length
      AlMeshId  = 9.7       ! Aluminum shield mesh inner diameter
      AlMeshOd  = 44        ! Aluminum shield mesh outer diameter
      AlMshThk  = 0.03      ! Aluminum shield mesh effective thickness
      AlMshPos  = 53.5      ! Aluminum shield mesh z position
   EndFill
*
      USE SVTG
      USE SWCA
      USE SELC
      USE SSUP
      USE SSUB
      USE SERG
      USE SWAM
      USE SELC
      USE SFPA
      USE SSLD

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
*
      Create and Position SVTT in Cave
*
*******************************************************************************
*
Block SVTT is the mother of all SVT volumes
*
      RsizeMax=ssup_Con4IdMx
      RsizeMax=RsizeMax+ssup_GrphThk+ssup_CabThk+2.0*ssup_HosRmx
      RsizeMax=RsizeMax+ssup_WrpMyThk+ssup_WrpAlThk
*
      Clearence=svtg_RsizeMax-RsizeMax
      If (Clearence<0) print *,' SVTT max size error, clearence=',clearence
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
      Position  SOES z= screw_z
      Position  SOES z=-screw_z
      Create    SIES " Volume to hold inner endring screws"
      Position  SIES z= screw_z
      Position  SIES z=-screw_z
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
      Create    SCON  " support cone mother"
      Position  SCON
      Position  SCON              ThetaZ=180 
*
* The beampipe support
*
      Create    SBSP  " Beampipe support mother "
      Position  SBSP z=(ssup_RodLen/2-
                        ssub_KMntThk/2)
      Position  SBSP z=-(ssup_RodLen/2-
                         ssub_KMntThk/2),
                     ThetaZ=180
* 
* SVT support rods
*
      Create    SROD  " Be support rod"
      Position  SROD  y=ssup_rodDist+ssup_rodOD/2
      Position  SROD  y=-ssup_rodDist-ssup_rodOD/2
*
* The SVT layers 
*
      radmax=svtg_rSizeMax
      Do ilayer = 1, min(6,nint(svtg_Nlayer))
         if (ilayer<6) then
            USE SVTL layer=ilayer+1
            radmax=svtl_radius
         else
            radmax=svtg_rSizeMax
         endif
         USE SVTL layer=ilayer
         Create   SLYD  " layer mother " 
	 Position SLYD
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
* The fourth (super) layer made of strip detectors
*
      if (svtg_Nlayer>6) then
         Create and position SFMO " mother of 4th layer (strip detectors)"
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

*
EndBlock
*
*******************************************************************************
*
Block SLYD is a single SVT layer  
*     Note: clearence between layers is very small. If you have to change
*     any of the thicknesses in ladthk or elethk, you should make SLYD
*     visible and check clearences. (wkw)
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

      Create   SLSD
EndBlock
*
* ----------------------------------------------------------------------------
*
Block SLSD is a single ladder mother (sector of tube) 
      Attribute SLSD    seen=0    colo=1
*>>>
      if (svtg_Nlayer<0) then
        Shape  Division   Iaxis=2   Ndiv=1    c0=-90
      else
        if (svtg_version==1) then
           c0=-deg*mod(ilayer,2)+svtg_Angoff*int((ilayer-1)/2) 
        else
           c0=90-deg*mod(ilayer,2)
        endif
        Shape  Division   Iaxis=2   Ndiv=svtl_Nladder c0=c0
      endif
*>>>
*
      Create and Position SLDI x=svtl_radius                ORT=YZX
*
*     Electronics go on both sides of the ladder
      Create   SELE

*     Position electronics carrier on left side of ladder (y pos)
      deg=180.0/svtl_Nladder
      rad=(TwoPi/2)/svtl_Nladder
      xpos=sin(rad)*selc_ElcaWid/2-cos(rad)*elethk/2
      ypos=cos(rad)*selc_ElcaWid/2+sin(rad)*elethk/2
      do s=-1,1,2
              side=s
	      Position SELE ORT=YZX,
	      x=svtl_radius-ladthk-xpos,
	      y=s*(swca_WaferWid/2+ypos),
              AlphaX=90*(1-s), AlphaZ=s*deg
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
      Attribute SLDI   seen=0    colo=1     serial=svtl_Nwafer
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
*
*     STRU are the Berrillium struts between the wafer carriers rails
*     note: the positioning of these struts is approximate
      Create   STRU " struts between wafer carrier rails"
      Position STRU y=(svtl_Nwafer*(swca_WaferLen/2+swca_WaferGap)+ _
                       swca_WaferGap+swca_strutlen/2),
                    z=-ladthk+swca_WafCarTh/2 
      Position STRU y=-(svtl_Nwafer*(swca_WaferLen/2+swca_WaferGap)+ _
                       swca_WaferGap+swca_strutlen/2),
                    z=-ladthk+swca_WafCarTh/2 
*
*     Roha cell spacers support the chips
      Create   SRHC " roha cell wafer supports"
      Position SRHC x=+swca_WaferWid/2-swca_WafCarWd/2,
                    z=-ladthk+2.0*swca_WafCarTh/2+swca_RoHaThk/2
      Position SRHC x=-swca_WaferWid/2+swca_WafCarWd/2,
                    z=-ladthk+2.0*swca_WafCarTh/2+swca_RoHaThk/2


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
Block STLI is the waver pack container
      Attribute STLI   serial=0   seen=0    colo=1
      Shape     BOX    dy=svtl_Nwafer*(swca_WaferLen/2+swca_WaferGap),
                       dz=swca_WaferThk/2
      Create    STSI
EndBlock
* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Block STSI is a single waver container
      Shape   division     Iaxis=2  Ndiv=svtl_Nwafer
      Create and Position  SVTD 
EndBlock
* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Block SVTD is an active wafer volume
      Material  Silicon  
      Material  Sensitive  Isvol=1       
      Attribute SVTD       seen=1  Colo=4
      Shape     BOX        dy=swca_WaferLen/2 
      call      GSTPAR (%Imed,'STRA',1.)

*     The following is the corrected hits definition: 25-dec-98 (PN)
      HITS    SVTD   Z:.001:S  Y:.001:   X:.001:     Ptot:18:(0,100),
                     cx:10:    cy:10:    cz:10:      Sleng:16:(0,500),
                     ToF:16:(0,1.e-6)    Step:.01:   Eloss:22:(0,0.01) 

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
*------------------------------------------------------------------------------
*
Block SELE is the electronics mother volume
      Material  Air  
      Attribute SELE  Seen=0  Colo=1
      Shape     BOX   dx=selc_ElcaWid/2  dy=swca_Length/2  dz=elethk/2
*
*     build layers from bottom to top 
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
Block SROD is the SVT Be support rod
      Material  Berillium
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
      angle=asin(cone_sin)*180/3.1416
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
      angle=asin(cone_sin)*180/3.1416
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
Block SFMO is the mother of the fourth layer (strip detectors)
      Material   Air
      Attribute  SFMO   Seen=0 Colo=1
      Shape TUBE Rmin=sfpa_rmin,
		 Rmax=sfpa_rmax,
		 dz=sfpa_Len/2

	dthk=sfpa_smThk+sfpa_gpThk
	radtilt=(sfpa_tilt*pi)/180.
      Do ilad = 1,20
	Create SFLM " ladder mother"
	ang=(float(ilad)-1)*2.0*pi/20.0
      	Position SFLM x=-(sfpa_rad*sin(ang)+ _
                         (dthk*sin(ang+radtilt))/2),
		      y=(sfpa_rad*cos(ang)+ _
                        (dthk*cos(ang+radtilt))/2),
                      z=0,
                      AlphaZ=(180.*ang/pi)+sfpa_tilt
      EndDo
Endblock 
*
*------------------------------------------------------------------------------
* 
Block SFLM is the mother of the 4th layer ladder
* (dets,adc's and struct.)
      Material Air
      Attribute SFLM Seen=0 Colo=1
      Shape BOX dx=sfpa_dmWid/2,
		dy=(sfpa_dmThk+sfpa_gpThk+sfpa_smThk)/2,
		dz=sfpa_smLen/2 
      Create   SFDM " the detectors and adcs mother volume "
      Position SFDM y=-(sfpa_smThk+sfpa_gpThk)/2
		    

      Create   SFSM " the structure mother volume"
      Position SFSM y=(sfpa_dmThk+sfpa_gpThk)/2
                    
Endblock 
*
*------------------------------------------------------------------------------
* 
Block SFDM is the mother of the detectors 
      Material Air
      Attribute SFDM Seen=0 Colo=1
      Shape BOX         dx=sfpa_dmWid/2,
			dy=sfpa_dmThk/2,
			dz=sfpa_wpLen/2 
      
      wafpckLen=sfpa_wpLen/(sfpa_nssd*1.)
      Do iwaf=1,sfpa_nssd
        Create    SFSW " single wafer container"
        Position  SFSW z=-(sfpa_wpLen+wafpckLen)/2+iwaf*wafpckLen
     EndDo 
Endblock 
*
* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
*
Block SFSW is a single wafer container
      Attribute SFSW Seen=0 Colo=1
      Shape BOX dx=sfpa_dmWid/2,
	        dy=sfpa_dmThk/2,
	        dz=wafpckLen/2
 
     Create and position  SFSD " strip detector"

Endblock
*
* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
*
Block SFSD is the strip detector
      Material  Silicon  
      Material  Sensitive  Isvol=1       
      Attribute SFSD       seen=2  Colo=4
      Shape   BOX dx=sfpa_dmWid/2,
 		  dy=sfpa_dmThk/2,
		  dz=sfpa_sdlen/2
      call      GSTPAR (%Imed,'STRA',1.)

*     The following is the corrected hits definition: 25-apr-99 (PN)
      HITS    SFSD   X:.001:S   Y:.001:   Z:.001:   cx:10:   cy:10:   cz:10:,
                     Step:.01:   Sleng:16:(0,500)   ToF:16:(0,1.e-6),  
                     Ptot:16:(0,100)      Eloss:16:(0,0.001) 

Endblock
*
*------------------------------------------------------------------------------
* 
Block SFSM is the mother of the ladder struct. 
* (cool. pipe and carbon fiber)
      Material Air
      Attribute SFSM  Seen=0 Colo=1
      Shape BOX dx=sfpa_smWid/2,
		dy=sfpa_smThk/2,
		dz=sfpa_smLen/2 
      Create   SFSS " subvolume structure"
Endblock 
*
*------------------------------------------------------------------------------
* 
Block SFSS is the subvolume of the mother struct. 
* (cool. pipe and carbon fiber)
      Material Air
      Attribute SFSS Seen=0 Colo=1
      Shape   division     Iaxis=3  Ndiv=20

      Create   SFCP " cooling pipes"
      Position SFCP x=sfpa_smWid/2-5.*sfpa_cprad,
                    y=-sfpa_smThk/2+sfpa_cprad
      Position SFCP x=-sfpa_smWid/2+5.*sfpa_cprad,
                    y=-sfpa_smThk/2+sfpa_cprad

      Create   SFCF " carbon fiber"
      Position SFCF
Endblock 
*
* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
* 
Block SFCP is the cooling pipe 
      Material Carbon
      Attribute SFCP Seen=1 Colo=6
      Shape TUBE  rmin=0 rmax=sfpa_cprad dz=sfpa_ssLen/2
		 
      Create and Position SFCW " water cylinder"

Endblock
*
* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
* 
Block SFCW is the water cylinder in the cooling pipe
      Attribute SFCW Seen=1 Colo=6
      Material  Water
      Shape     TUBE rmax=sfpa_cpral
                
Endblock
*
* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
* 
Block SFCF is the carbon fiber structure container
      Material Air
      Attribute SFCF Seen=0  Colo=3
      Shape BOX  dx=sfpa_smThk*tan(pi/6.),
		 dy=sfpa_smThk/2.,
		 dz=sfpa_ssLen/2

      Create   SFCT " carbon tube"
      Position SFCT y= sfpa_smThk/2.-sfpa_cfrad
       
      Position SFCT x= sfpa_smThk*tan(pi/6.)-sfpa_cfrad,
                    y=-sfpa_smThk/2.+sfpa_cfrad

      Position SFCT x=-sfpa_smThk*tan(pi/6.)+sfpa_cfrad,
                    y=-sfpa_smThk/2.+sfpa_cfrad


      Create   SFCX " carbon tube (crossing)"
      Position SFCX y=-sfpa_smThk/2.+sfpa_cfrad,
                    ort=yzx

      Position SFCX x=sfpa_smThk*tan(pi/6.)/2.-sfpa_cfrad/3.,
                    ort=yzx AlphaZ=-60

      Position SFCX x=-sfpa_smThk*tan(pi/6.)/2.+sfpa_cfrad/3.,
                    ort=yzx AlphaZ=+60

Endblock
*
* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
* 
Block SFCT is the carbon fiber tube
      Material Carbon
      Attribute SFCT Seen=1 Colo=6
      Shape TUBE rmin=0  rmax=sfpa_cfrad,
		 dz=sfpa_ssLen/2
Endblock
*
* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
* 
Block SFCX is the carbon fiber tube
      Material Carbon
      Attribute SFCX Seen=1 Colo=7
      Shape TUBE rmin=0  rmax=sfpa_cfrad,
		 dz=sfpa_smThk*tan(pi/6.)-sfpa_cfrad
Endblock
*
* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
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

