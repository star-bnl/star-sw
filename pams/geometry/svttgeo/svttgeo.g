*******************************************************************************
Module  SVTTGEO  is the SVT geometry for STAR
   Author  Pavel Nevski, Ken Wilson
   created 05 apr 96
*                                                                             *
*  Original version: Claude Pruneau, Wayne State University, 25-jul-1993      *
*------------------------------------------------------------------------------
*  Division lines:                                                            *
*                 *********** separates major divisions                       *
*                 ----------- separates sub-divisions                         *
*                 - - - - - - separates sub-sub divisions                     *
*------------------------------------------------------------------------------
*  04-nov-93 cap integrated in STAR/GEANT                                     *
*  18-Nov-93 Updated by Stathes Paganis, Spiros Margetis                      *
*  05-Apr-96 Put into AGI format by Pavel Nevski, material checked,           *
*            some overlaps fixed, supports added                              *
*  Late Apr   Modified by Ken Wilson: redid cone, end rings, etc.             *
*             Set colors to STAR convention                                   *
*  May 21 WKW changed water channel thickness from 500 to 750 microns         *
*             changed AgPd thickness from 30 to 45 microns                    *
*             added glass insulating layer                                    *
*             all based on info from David Lynn                               *
*  May 22 WKW separated waf carrier and support struct. parameters from SVTG  *
*  May 29 WKW added brackets between cone and water manifold                  *
*             added brackets between end rings                                *
*             checked and changed many numbers using new info from Chong-Jer  *
*  Jun 14 WKW added screws between end rings and end ring brackets            *
*  Jul 15 WKW added SGLA to the content list (should have been there!)        *
*  Oct 18 WKW corrected thickness of copper for cables on the support cone    *
*  Oct 24 WKW set the id of the sup. cone section 3 at 37.4 cm for ftpc group *
*             set end ring thickness to .2                                    *
*	      removed the tpc support ring (which was a guess by Pavel)       *
*  Dec 15 WKW set space between wafers on ladder in z direction to 0          *
*  Feb 12/97  WKW changed the hits definition to agree with g2t, this should  *
*             fix eloss problems                                              *
*  Apr 29 WKW fixed mistake in glass, O was A=8, Z=16                         *
*             changed glass density from 2.0 to 2.2 two reflect PDG value     *
*             dye thickness changed to give .11% per hybrid                   *
*             glass thickness changed from 350 to 150 microns to agree with   *
*              D.L. note                                                      *
*  Aug 17 PN  C0,Iring is not used anymore, delete their typedef              *
*******************************************************************************

+cde,AGECOM,GCONST,GCUNIT.
*     
      Content          SVTT,SLYD,SLSD,SLDI,STLI,STSI,SVTD,SBER,STAB,
                       STRU,SRHC,SBWC,SWCM,SXAI,SXBI,
                       SELE,SWCH,SWCW,SWCS,SBOI,SAGP,SDYE,SECA,SIRP,
                       SIRT,SOER,SCON,SROD,SGRA,
                       STAP,STAC,SHLA,SHLB,SHMA,SHMB,SWHO,SHWA,
                       SCMY,SCAL,SWMM,SWMB,SWMT,SWMS,SWMW,SOTB,SITB,
                       SBRG,SBRM,SBRI,SOES,SIES,SOSM,SISM,SCRW,
                       SGLA
*
      structure SVTG { Version,   Nlayer,    RsizeMin,  RsizeMax,
		       ZsizeMax,  Angoff}
*     
      structure SWCA { Version,   Length,
                       WaferWid,  WaferLen,  WaferThk,  RohaThk, 
                       WafCarWid, WafCarThk, WaferGap,  Drift,    
                       strutlen}
*
      structure SSUP { Version,   CabThk,    HosRmn,
		       HosRmx,    Nhoses,    WrpMyThk,  WrpAlThk,    
                       GrphThk,   Cone1Zmn,  RodLen,    RodDist,
                       RodID,     RodOD,     Cone1idmn, Cone3idmn,  
                       Cone4idmn, Cone4idmx, Cone3zmx,  Cone4zmx,  
                       BraThk,    ERJThk,    ERJWid,
                       ERJLen,    ERJzdis,   ERJ1x,     ERJ2x,
                       ERJ2y,     ERJrad,    ERJdia}
*
      structure SWAM { Version,   Zmin,      Len,       Rmin, 
                       Rmax,      TbrdThk,   WallThk} 
*
      structure SERG { Version,   IrngTRmax, IrngPRmin, OrngRmin, 
                       OrngRmax,  EndRngThk, EndRngZmn}
*
      structure SELC { Version,   BeThk,     WatThk,    BeOThk,  
                       DyeThk,    DyeWid,    DyeSpc,    ElcaWid, 
                       AgPdThk,   GlassThk,  CabThk,    CabWid}
*
      Structure SVTL { Layer,    Nladder,  Nwafer,   Radius    }
*     
      Integer        iLayer,s,side
      Real           ladthk,cone_thk1,cone_thk2,roffset,RsizeMax,deg,rad
      Real	     cone_len,cone_sin,cone_cos,rmin,rmax,zmin,zmax,angle
      Real           xpos,ypos,zpos,clearence,rin,rou,elethk,tabLen
      Real           endrng_z,brack_z,screw_z,ir_rmin               
*
*******************************************************************************
*
   Fill SVTG ! Basic SVT dimensions 
      Version   = 1          ! geometry version
      Nlayer    = 6          ! number of svt layers
      RsizeMin  = 5          ! STV innermost radius
      RsizeMax  = 46.107     ! STV outermost radius
      ZsizeMax  = 270        ! SVT+FTPC length
      Angoff    = 0          ! angular offset x1 for slayer 2 x2 for slayer 3
*
   Fill SWCA ! Wafer Carrier
      Version   = 1          ! geometry version
      Length    = 56.04      ! ladder length
      WaferWid  = 6          ! wafer width
      WaferLen  = 6          ! wafer length
      WaferThk  = 0.0300     ! wafer thickness
      RoHaThk   = 0.0381     ! Roha cell plus glue thickness 
      WafCarWid = 1.5        ! wafer carrier rails width
      WafCarThk = 0.0300     ! wafer carrier thickness 
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
      Cone1idmn = 15.67      ! Minimum id of cone 1 
      Cone3idmn = 21.67      ! Minimum id of cone 3 (TBD)
      Cone4idmn = 37.4       ! Minimum id of cone 4 (TBD)
      Cone4idmx = 37.4       ! Maximum id of cone 4 (TBD)
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
   Fill SERG ! end rings
      Version   = 1          ! geometry version
      IrngTRmax = 9.703      ! Inner end ring tube maximum radius 
      IrngPRmin = 7.671      ! Inner end ring polygon minimum radius
      OrngRmin  = 11.900     ! Outer end ring minimum radius
      OrngRmax  = 13.805     ! Outer end ring maximum radius
      EndRngThk = 0.2        ! End ring thickness
      EndRngZmn = 23.01      ! minimum z for end rings
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
   Fill SVTL ! single layer data
      layer    = 1          ! layer number
      Radius   = 6.1+.025   ! layer radius (.025 puts it in center of wafer)
      Nladder  = 4          ! number of ladder
      Nwafer   = 4          ! number of wafers
*      
   Fill SVTL ! single layer parameters
      layer    = 2          ! layer number
      Radius   = 7.16+.025  ! layer radius
*      
   Fill SVTL ! single layer parameters
      layer    = 3          ! layer number
      Radius   = 10.16+.025 ! layer radius
      Nladder  = 6          ! number of ladder
      Nwafer   = 6          ! number of wafers
*      
   Fill SVTL ! single layer parameters
      layer    = 4          ! layer number
      Radius   = 11.05+.025 ! layer radius
*      
   Fill SVTL ! single layer parameters
      layer    = 5          ! layer number
      Radius   = 13.97+.025 ! layer radius
      Nladder  = 8          ! number of ladder
      Nwafer   = 7          ! number of wafers
*      
   Fill SVTL ! single layer parameters
      layer    = 6          ! layer number
      Radius   = 14.91+.025 ! layer radius
   EndFill
*
      USE SVTG  version=1
      USE SWCA  version=1
      USE SELC  version=1
      USE SSUP  version=1
      USE SERG  version=1
      USE SWAM  version=1
      USE SELC  version=1

      Create and Position SVTT in Cave
*
*******************************************************************************
*
Block SVTT is the mother of all SVT volumes
*
      RsizeMax=ssup_Cone4idmx
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
      Position  SIRP  Z=serg_EndRngZmn+serg_EndRngThk/2  AlphaZ=22.5
      Position  SIRP  Z=-serg_EndRngZmn-serg_EndRngThk/2 AlphaZ=22.5
      Create    SIRT  " inner end ring tube piece "  
      Position  SIRT  Z=serg_EndRngZmn+serg_EndRngThk/2  AlphaZ=22.5
      Position  SIRT  Z=-serg_EndRngZmn-serg_EndRngThk/2 AlphaZ=22.5
      Create    SOER  " outer end ring"  
      Position  SOER  Z=serg_EndRngZmn+serg_EndRngThk/2 
      Position  SOER  Z=-serg_EndRngZmn-serg_EndRngThk/2 
*
* Bracket joining the end rings 
* (shape is very approximate guess)
*
      Create    SBRG " Bracket joining the end rungs"
      Position  SBRG z=  swca_Length/2.0+ssup_ERJzdis+ssup_ERJthk/2.0
      Position  SBRG z= -swca_Length/2.0-ssup_ERJzdis-ssup_ERJthk/2.0
*
* Mother volumes for the screws joining the bracket to the end ring
*
      endrng_z=serg_EndRngZmn+serg_EndRngThk
      brack_z=swca_Length/2.0+ssup_ERJzdis
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
      Create    SWMM  " water manifold mother"
      Position  SWMM  z= swam_Zmin+swam_Len/2.0
      Position  SWMM  z=-swam_Zmin-swam_Len/2.0 
*
* Bracket connecting water minifold to support cone 
* (guess, this is not designed yet)
*
      Create    SBWC " water manifold to support cone bracket mother"
      Position  SBWC z= (swam_Zmin+swam_Len+ _
                        (ssup_Cone1zmn-(swam_Zmin+swam_Len))/2.0)
      Position  SBWC z=-(swam_Zmin+swam_Len+ _
                        (ssup_Cone1zmn-(swam_Zmin+swam_Len))/2.0),
                     ThetaZ=180
* 
* SVT support cones
*
      Create    SCON  " support cone mother"
      Position  SCON
      Position  SCON              ThetaZ=180 
* 
* SVT support rods
*
      Create    SROD  " Be support rod"
      Position  SROD  y=ssup_rodDist+ssup_rodOD/2.0
      Position  SROD  y=-ssup_rodDist-ssup_rodOD/2.0
*
* The SVT layers 
*
      Do ilayer = 1, nint(svtg_Nlayer)
         USE SVTL layer=ilayer
         Create   SLYD  " layer mother " 
	 Position SLYD
      EndDo
EndBlock
*
*******************************************************************************
*
Block SLYD is a single SVT layer  
*     Note: clearence between layers is very small. If you have to change
*     any of the thicknesses in ladthk or elethk, you should make SLYD
*     visible and check clearences. (wkw)
*     ladthk is the ladder HALF thickness, wafer is at mid-plane
      ladthk = swca_WaferThk/2.0+swca_RoHaThk+swca_WafCarThk
*     elethk is the electronics+electronic carrier full thickness
      elethk=2.0*selc_BeThk+selc_WatThk+selc_BeOThk+selc_AgPdThk
      elethk=elethk+selc_GlassThk+selc_DyeThk
      Material Air
      Attribute SLYD    seen=0    colo=1
      Shape    TUBE rmin=svtl_radius-ladthk,
                    rmax=(sqrt((svtl_radius)**2+ _
                         (swca_WaferWid/2.0)**2)+elethk),
                    dz=swca_Length/2.0
      Create   SLSD
EndBlock
*
* ----------------------------------------------------------------------------
*
Block SLSD is a single ladder mother (sector of tube) 
      Attribute SLSD    seen=0    colo=1
      Shape  Division   Iaxis=2   Ndiv=svtl_Nladder,
      c0=-180.0/svtl_Nladder*mod(ilayer,2)+svtg_Angoff*int((ilayer-1)/2) 
*      c0=180.0/svtl_Nladder*mod(ilayer-1,2)+svtg_Angoff*int((ilayer-1)/2) 

*
      Create and Position SLDI x=svtl_radius                ORT=YZX
*
*     Electronics go on both sides of the ladder
      Create   SELE
      deg=180/svtl_Nladder
      rad=(TwoPi/2)/svtl_Nladder
*     Position electronics carrier on left side of ladder (y pos)
      xpos=sin(rad)*selc_ElcaWid/2.0-cos(rad)*elethk/2.0
      ypos=cos(rad)*selc_ElcaWid/2.0+sin(rad)*elethk/2.0
      do s=-1,1,2
              side=s
	      Position SELE ORT=YZX AlphaZ=s*deg,
	      x=svtl_radius-ladthk-xpos,
	      y=s*(swca_WaferWid/2.0+ypos),
              AlphaX=-90*(1-s)
      EndDo
EndBlock
*
* -----------------------------------------------------------------------------
*
Block SLDI is a ladder volume
* This contains the active wafer, Roha cell support, and the Be waffer carrier
* The center of the active wafer is at the center of this cell, the cell is
* centered on svtl_radius. 
      tabLen=swca_Length/2.0-7*(swca_WaferWid/2.0+swca_WaferGap)
      Material  Air
      Attribute SLDI   seen=0    colo=1     serial=svtl_Nwafer
      Shape     BOX    dx=swca_WaferWid/2.0 dy=swca_Length/2.0  dz=ladthk
      Create and Position STLI          z=0
*         
*     SBER are the Berillium wafer carrier rails                     
      Create   SBER " wafer carrier rails"
      Position SBER x=+swca_WaferWid/2.0-swca_WafCarWid/2.0,
                    z=-ladthk+swca_WafCarThk/2.0 
      Position SBER x=-swca_WaferWid/2.0+swca_WafCarWid/2.0,
                    z=-ladthk+swca_WafCarThk/2.0 
*
*     STAB are the Berrillium tabs and the ends of the wafer carriers
      Create   STAB " wafter carrier end tabs"
      Position STAB x=0,
                    y=swca_Length/2.0-tabLen/2.0,
                    z=-ladthk+swca_WafCarThk/2.0 
      Position STAB x=0,
                    y=-swca_Length/2.0+tabLen/2.0,
                    z=-ladthk+swca_WafCarThk/2.0 
*
*     STRU are the Berrillium struts between the wafer carriers rails
*     note: the positioning of these struts is approximate
      Create   STRU " struts between wafer carrier rails"
      Position STRU x=0,
                    y=(svtl_Nwafer*(swca_WaferLen/2.0+swca_WaferGap)+ _
                       swca_WaferGap+swca_strutlen/2.0),
                    z=-ladthk+swca_WafCarThk/2.0 
      Position STRU x=0,
                    y=-(svtl_Nwafer*(swca_WaferLen/2.0+swca_WaferGap)+ _
                       swca_WaferGap+swca_strutlen/2.0),
                    z=-ladthk+swca_WafCarThk/2.0 
*
*     Roha cell spacers support the chips
      Create   SRHC " roha cell wafer supports"
      Position SRHC x=+swca_WaferWid/2.0-swca_WafCarWid/2.0,
                    z=-ladthk+2.0*swca_WafCarThk/2.0+swca_RoHaThk/2.0
      Position SRHC x=-swca_WaferWid/2.0+swca_WafCarWid/2.0,
                    z=-ladthk+2.0*swca_WafCarThk/2.0+swca_RoHaThk/2.0


EndBlock
* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Block SRHC is the roha cell wafer support
* The material here is a guess, but the density is correct
      Component C      A=12  Z=6  W=1
      Component H2     A=1   Z=1  W=2
      Mixture   ROHA   Dens=0.0304
      Attribute SRHC   Seen=1   Colo=3
      Shape     BOX    dx=swca_WafCarWid/2.0,
                       dy=svtl_Nwafer*(swca_WaferLen/2.0+swca_WaferGap),
                       dz=swca_RoHaThk/2.0
EndBlock
* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Block STLI is the waver pack container
      Attribute STLI   serial=0   seen=0    colo=1
      Shape     BOX    dy=svtl_Nwafer*(swca_WaferLen/2.0+swca_WaferGap),
                       dz=swca_WaferThk/2.0
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
      Shape     BOX        dy=swca_WaferLen/2.0 
      call      GSTPAR (%Imed,'STRA',1.)
*      The following is the corrected hits definition: 2-12-97 (wkw)
       HITS      SVTD   xx:16:SH(-20,20)   yy:16:(-20,20)     zz:16:(-30,30),
	                px:16:(-100,100)   py:16:(-100,100)   pz:16:(-100,100),
                        Slen:16:(0,1.e4)   Tof:16:(0,1.e-6)   Step:16:(0,10),
                        SHTN:16:           Elos:32:(0,1)
EndBlock
* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Block SBER are the Berillium wafer carrier rails
      Material  Berillium 
      Attribute SBER     Seen=1   Colo=2
      Shape     BOX      dx=swca_WafCarWid/2.0,
                         dy=swca_Length/2.0,  
                         dz=swca_WafCarThk/2.0
EndBlock
* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Block STAB are the Berillium wafer carrier end tabs
      Material  Berillium 
      Attribute STAB     Seen=1   Colo=2
      Shape     BOX      dx=swca_WaferWid/2.0-swca_WafCarWid,
                         dy=tabLen/2.0,
                         dz=swca_WafCarThk/2.0
EndBlock
* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Block STRU are the Berillium struts between the wafer carrier rails
      Material  Berillium 
      Attribute STRU     Seen=1   Colo=2
      Shape     BOX      dx=swca_WaferWid/2.0-swca_WafCarWid,
                         dy=swca_strutlen/2.0,
                         dz=swca_WafCarThk/2.0
EndBlock
*
*------------------------------------------------------------------------------
*
Block SELE is the electronics mother volume
      Material  Air  
      Attribute SELE  Seen=0  Colo=1
      Shape     BOX   dx=selc_ElcaWid/2.0  dy=swca_Length/2.0  dz=elethk/2.0
*
*     build layers from bottom to top 
      Create    SWCH " water channel top/bottom"
      Position  SWCH z=-elethk/2.0+selc_BeThk/2.0
      Position  SWCH z=-elethk/2.0+selc_BeThk+selc_WatThk+selc_BeThk/2.0
      Create    SWCS " water channel side"
      Position  SWCS z=-elethk/2.0+selc_BeThk+selc_WatThk/2.0,
                     x=-selc_ElcaWid/2.0+selc_BeThk/2.0
      Position  SWCS z=-elethk/2.0+selc_BeThk+selc_WatThk/2.0,
                     x=+selc_ElcaWid/2.0-selc_BeThk/2.0
      Create    SWCW " water channel water"
      Position  SWCW z=-elethk/2.0+selc_BeThk+selc_WatThk/2.0
      Create    SBOI " BeO substrate for hybrid"
      Position  SBOI z=(elethk/2.0-selc_DyeThk-selc_AgPdThk _
                        -selc_GlassThk-selc_BeOThk/2.0)
      Create    SGLA " Glass insulating plane"
      Position  SGLA z=elethk/2.0-selc_DyeThk-selc_AgPdThk-selc_GlassThk/2.0
      Create    SAGP " Silver-Palladium Grounding plane"
      Position  SAGP z=elethk/2.0-selc_DyeThk-selc_AgPdThk/2.0
      Create    SDYE " ic chips"
      Position  SDYE z=elethk/2.0-selc_DyeThk/2.0,
                     x=selc_ElcaWid/2.0-selc_Dyespc-selc_Dyewid/2.0
      Position  SDYE z=elethk/2.0-selc_DyeThk/2.0,
                     x=selc_ElcaWid/2.0-2.0*selc_Dyespc-3.0*selc_Dyewid/2.0
      Create    SECA " cables on electronics carriers"
      Position  SECA z=elethk/2.0-selc_DyeThk+selc_CabThk/2.0,
                     x=-selc_ElcaWid/2.0+selc_CabWid/2.0
*
EndBlock
* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Block SDYE is the ic chip on the hybrid
      Material  Silicon
      Attribute SDYE   seen=1  colo=6
      Shape     BOX    dx=selc_DyeWid/2.0,
                       dy=svtl_Nwafer*(swca_WaferLen/2.0+swca_WaferGap),
                       dz=selc_DyeThk/2.0
EndBlock
* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Block SECA is the cable on the electronics carrier
      Material  Copper
      Attribute SECA   seen=1  colo=2
      Shape     BOX    dx=selc_CabWid/2.0,
                       dy=swca_Length/2.0,
                       dz=selc_CabThk/2.0
EndBlock
* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Block SBOI is the Berillia layer
      Component Be     A=9    Z=4   W=1
      Component O      A=16   Z=8   W=1
      Mixture   BeO    Dens=2.85
      Attribute SBOI   seen=1  colo=6
      Shape     BOX    dx=selc_ElcaWid/2.0,
	               dy=svtl_Nwafer*(swca_WaferLen/2.0+swca_WaferGap),
  		       dz=selc_BeOThk/2.0
EndBlock
* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Block SAGP is the Silver-Palladium layer
      Component Ag     A=108  Z=47  W=1
      Component Pd     A=106  Z=46   W=1
      Mixture   AgPd   Dens=11.25
      Attribute SAGP   seen=1  colo=2
      Shape     BOX    dx=selc_ElcaWid/2.0,
	               dy=svtl_Nwafer*(swca_WaferLen/2.0+swca_WaferGap),
  		       dz=selc_AgPdThk/2.0
EndBlock
* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Block SGLA is the insulating glass layer
      Component Si     A=28  Z=14  W=1
      Component O      A=16   Z=8  W=2
      Mixture   glass  Dens=2.2
      Attribute SGLA   seen=1  colo=6
      Shape     BOX    dx=selc_ElcaWid/2.0,
	               dy=svtl_Nwafer*(swca_WaferLen/2.0+swca_WaferGap),
  		       dz=selc_GlassThk/2.0
EndBlock
* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Block SWCH is the Be top and bottom of the water channel
      Material  Berillium
      Attribute SWCH   seen=1  colo=2
      Shape     BOX    dx=selc_ElcaWid/2.0,
                       dy=swca_Length/2.0,
                       dz=selc_BeThk/2.0
EndBlock
* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Block SWCS is the Be side of the water channel
      Material  Berillium
      Attribute SWCS   seen=1  colo=2
      Shape     BOX    dx=selc_BeThk/2.0,
                       dy=swca_Length/2.0,
                       dz=selc_WatThk/2.0
EndBlock
* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Block SWCW is the water channel water (probably Evian?) 
*
      Component H2     A=1   Z=1   W=2
      Component O      A=16  Z=8   W=1
      Mixture   Water  Dens=1.0
      Attribute SWCW   seen=1  colo=6
      Shape     BOX    dx=(selc_ElcaWid-2.0*selc_BeThk)/2.0,
                       dy=swca_Length/2.0,
                       dz=(selc_WatThk/2.0)
EndBlock
*
* ****************************************************************************
*
Block SIRT is the SVT inner end ring tube
*
      Material  Berillium
      Attribute SIRT  Seen=1  Colo=2
      ir_rmin=serg_IrngPRmin*(cos(pi/8.)+sqrt(tan(pi/8.)**2.-sin(pi/8.)**2.))
      Shape     TUBE  rmin=ir_rmin,
                      rmax=serg_IrngTRmax,
                      dz=serg_EndRngThk/2.0
Endblock
*
* ****************************************************************************
*
Block SIRP is the SVT inner end ring polycone (overlaps tube)
* The inner surface of the ploycone is inscribed within the inner tube radius
* The outer surface of the ploycone is inscribed within the outer tube radius
*
      Material  Berillium
      Attribute SIRP  Seen=1  Colo=2
      rou=serg_IrngTRmax/(cos(pi/8.)+sqrt(tan(pi/8.)**2.-sin(pi/8.)**2.))
      Shape     PGON  Phi1=0   Dphi=360  Nz=2,
                      NpDiv=8,
                      zi ={-serg_EndRngThk/2, +serg_EndRngThk/2},
                      rmn={ serg_IrngPRmin, serg_IrngPRmin},
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
                       dz=serg_EndRngThk/2.0
Endblock
*
*******************************************************************************
*
Block SIES is the volume to hold inner endring screws
      Material  Air
      Attribute SIES seen=0 colo=1
      Shape     TUBE  rmin=ir_rmin,
                      rmax=serg_IrngTRmax,
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
      Position  SCRW x=ssup_ERJrad-ssup_ERJlen/2.0+ssup_ERJ1x
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
      Position  SCRW x=ssup_ERJrad+ssup_ERJlen/2.0-ssup_ERJ1x
      Position  SCRW x=ssup_ERJrad+ssup_ERJlen/2.0-ssup_ERJ2x
      Position  SCRW x=ssup_ERJrad+ssup_ERJlen/2.0-ssup_ERJ2x,
                     y=ssup_ERJ2y
      Position  SCRW x=ssup_ERJrad+ssup_ERJlen/2.0-ssup_ERJ2x,
                     y=-ssup_ERJ2y
Endblock
*
*******************************************************************************
*
Block SBRG is the bracket joining the end rings
      rin=serg_IrngPRmin
      rou=serg_OrngRmax
      Material  Air
      Attribute SBRG  seen=0  colo=1
      Shape     TUBE  Rmin=rin,
		      Rmax=rou,
                      dz=ssup_ERJthk/2.0
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
      Shape    BOX  dx=ssup_ERJlen/2.0,
                    dy=ssup_ERJwid/2.0,
                    dz=ssup_ERJThk/2.0
EndBlock
*
* ****************************************************************************
*
Block SROD is the SVT Be support rod
      Material  Berillium
      Attribute SROD  Seen=1  Colo=2
      Shape     TUBE   rmin=ssup_RodID/2.0,
                       rmax=ssup_RodOD/2.0,
                       dz=ssup_RodLen/2.0
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
           ssup_Rodlen/2.0, 
           ssup_Rodlen/2.0, 
           ssup_Rodlen/2.0+ssup_GrphThk, 
           ssup_Rodlen/2.0+ssup_GrphThk, 
	   ssup_Cone3zmx, 
	   ssup_Cone4zmx},
      Rmx={ssup_Cone1idmn+cone_thk1,
           ssup_Cone1idmn+cone_thk1, 
           ssup_Cone3idmn+cone_thk1,
           ssup_Cone3idmn+cone_thk1,
           ssup_Cone3idmn+cone_thk2,
	   ssup_Cone4idmn+cone_thk2,
	   ssup_Cone4idmx+cone_thk2},
      Rmn={ssup_Cone1idmn,
           ssup_Cone1idmn, 
           ssup_Cone1idmn,
           ssup_Cone1idmn,
           ssup_Cone3idmn, 
	   ssup_Cone4idmn,
	   ssup_Cone4idmx}
*
      Create    SGRA  " graphite/epoxy support cone (lowest layer)"
      Position  SGRA
      Create    STAP  " twinax cable approximation, plastic"
      Position  STAP
      Create    STAC  " twinax cable approximation, copper"
      Position  STAC
      Create    SHLA  " water hose cone 3 layer"
      Position  SHLA  z=ssup_Rodlen/2.0+ssup_GrphThk+_
                      0.5*(ssup_Cone3zmx-ssup_Rodlen/2.0-ssup_GrphThk)
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
      zi ={ssup_Cone1zmn,
           ssup_Rodlen/2.0, 
           ssup_Rodlen/2.0, 
           ssup_Rodlen/2.0+ssup_GrphThk, 
           ssup_Rodlen/2.0+ssup_GrphThk, 
	   ssup_Cone3zmx, 
	   ssup_Cone4zmx},
      Rmx={ssup_Cone1idmn+ssup_GrphThk,
           ssup_Cone1idmn+ssup_GrphThk, 
           ssup_Cone3idmn+ssup_GrphThk,
           ssup_Cone3idmn+ssup_GrphThk,
           ssup_Cone3idmn+ssup_GrphThk,
	   ssup_Cone4idmn+ssup_GrphThk,
	   ssup_Cone4idmx+ssup_GrphThk},
      Rmn={ssup_Cone1idmn,
           ssup_Cone1idmn, 
           ssup_Cone1idmn,
           ssup_Cone1idmn,
           ssup_Cone3idmn, 
	   ssup_Cone4idmn,
	   ssup_Cone4idmx}
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
      zi ={ssup_Rodlen/2.0+ssup_GrphThk, 
	   ssup_Cone3zmx, 
	   ssup_Cone4zmx},
      Rmx={ssup_Cone3idmn+roffset+ssup_CabThk/2.0,
	   ssup_Cone4idmn+roffset+ssup_CabThk/2.0,
	   ssup_Cone4idmx+roffset+ssup_CabThk/2.0},
      Rmn={ssup_Cone3idmn+roffset, 
	   ssup_Cone4idmn+roffset,
	   ssup_Cone4idmx+roffset}
endblock
*
*------------------------------------------------------------------------------
*
Block STAC is the copper part of the twin-ax cable layer
      roffset=ssup_GrphThk+ssup_CabThk/2.0
      Material  Copper
      Attribute STAC   Seen=1   Colo=2
      SHAPE     PCON   Phi1=0   Dphi=360  Nz=3,
      zi ={ssup_Rodlen/2.0+ssup_GrphThk, 
	   ssup_Cone3zmx, 
	   ssup_Cone4zmx},
      Rmx={ssup_Cone3idmn+roffset+ssup_CabThk/2.0,
	   ssup_Cone4idmn+roffset+ssup_CabThk/2.0,
	   ssup_Cone4idmx+roffset+ssup_CabThk/2.0},
      Rmn={ssup_Cone3idmn+roffset, 
	   ssup_Cone4idmn+roffset,
	   ssup_Cone4idmx+roffset}
endblock
*
*------------------------------------------------------------------------------
*
Block SHLA is the water hose layer for cone 3 (closer to vertex)
      roffset=ssup_GrphThk+ssup_CabThk
      Material  Air
      Attribute SHLA   Seen=0   Colo=1
      SHAPE     CONE   Dz=.5*(ssup_Cone3zmx-ssup_Rodlen/2.0-ssup_GrphThk),
                       Rmn1=ssup_Cone3idmn+roffset,
                       Rmx1=ssup_Cone3idmn+roffset+2.0*ssup_HosRmx,
                       Rmn2=ssup_Cone4idmn+roffset,
                       Rmx2=ssup_Cone4idmn+roffset+2.0*ssup_HosRmx
*
      Create   SHMA  " mother volumes for the water hoses on cone 3"
endblock
* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Block SHMA is a single mother volume for a water hose on the cone 3
*     These are the divisions for the water hoses, rotation c0=guess
      roffset=ssup_GrphThk+ssup_CabThk
      rmin=ssup_Cone3idmn+roffset
      rmax=ssup_Cone4idmn+roffset
      zmin=ssup_Rodlen/2.0+ssup_GrphThk
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
      Create   SHWA  " hoses water on cone 3"
      Position SHWA  x=xpos y=0 z=zpos AlphaY=angle
endblock
*
*------------------------------------------------------------------------------
*
Block SHLB is the water hose layer cone 4 (further from vertex)
      roffset=ssup_GrphThk+ssup_CabThk
      Material  Air
      Attribute SHLB   Seen=0   Colo=1
      SHAPE     CONE   Dz=.5*(ssup_Cone4zmx-ssup_Cone3zmx),
                       Rmn1=ssup_Cone4idmn+roffset,
                       Rmx1=ssup_Cone4idmn+roffset+2.0*ssup_HosRmx,
                       Rmn2=ssup_Cone4idmx+roffset,
                       Rmx2=ssup_Cone4idmx+roffset+2.0*ssup_HosRmx
*
      Create   SHMB  " mother volumes for the water hoses on cone 4"
endblock
* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Block SHMB is a single mother volume for a water hose on the cone 4
*     These are the divisions for the water hoses, rotation c0=guess
      roffset=ssup_GrphThk+ssup_CabThk
      rmin=ssup_Cone4idmn+roffset
      rmax=ssup_Cone4idmx+roffset
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
      Create   SHWA  " hoses water on cone 4"
      Position SHWA  x=xpos y=0 z=zpos AlphaY=angle
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
      Shape    TUBE rmin=ssup_HosRmn,
                    rmax=ssup_HosRmx,
                    dz=0.5*(cone_len-2.0*ssup_HosRmx)
EndBlock
* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Block SHWA is the water in the hose 
* note: fix hose length a la Pavel's example
      Attribute  SHWA   Seen=1    Colo=6
      Component H2     A=1   Z=1   W=2
      Component O      A=16  Z=8   W=1
      Mixture   Water  Dens=1.0
      Shape    TUBE rmin=0,
                    rmax=ssup_HosRmn,
                    dz=0.5*(cone_len-2.0*ssup_HosRmx)
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
      zi ={ssup_Rodlen/2.0+ssup_GrphThk, 
	   ssup_Cone3zmx, 
	   ssup_Cone4zmx},
      Rmx={ssup_Cone3idmn+roffset+ssup_WrpMyThk,
	   ssup_Cone4idmn+roffset+ssup_WrpMyThk,
	   ssup_Cone4idmx+roffset+ssup_WrpMyThk},
      Rmn={ssup_Cone3idmn+roffset, 
	   ssup_Cone4idmn+roffset,
	   ssup_Cone4idmx+roffset}
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
      zi ={ssup_Rodlen/2.0+ssup_GrphThk, 
	   ssup_Cone3zmx, 
	   ssup_Cone4zmx},
      Rmx={ssup_Cone3idmn+roffset+ssup_WrpAlThk,
	   ssup_Cone4idmn+roffset+ssup_WrpAlThk,
	   ssup_Cone4idmx+roffset+ssup_WrpAlThk},
      Rmn={ssup_Cone3idmn+roffset, 
	   ssup_Cone4idmn+roffset,
	   ssup_Cone4idmx+roffset}
endblock
*
*******************************************************************************
*
Block SWMM is the water manifold mother
      rin=swam_Rmin-swam_TbrdThk
      rou=swam_Rmax+swam_TbrdThk
      Material  Air
      Attribute SWMM  seen=0  colo=1
*      Shape     TUBE  Rmin=rin,
*		      Rmax=rou,
*                      dz=swam_Len/2.0
      Shape     PGON  Phi1=0   Dphi=360  Nz=2,
                      NpDiv=18,
                      zi ={-swam_Len/2.0,+swam_Len/2.0},
                      rmn={rin,rin},
                      rmx={rou,rou}
*
      Create   SWMB  " water manifold bottom"
      Position SWMB
      Create   SWMT  " water manifold top"
      Position SWMT
      Create   SWMS  " water manifold side"
      Position SWMS  z=-swam_Len/2.0+swam_WallThk/2.0
      Position SWMS  z= swam_Len/2.0-swam_WallThk/2.0
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
      Material   Aluminium
      Attribute  SWMB   Seen=1   Colo=2
*      Shape      TUBE   Rmin=swam_Rmin,
*                        Rmax=swam_Rmin+swam_WallThk,
*                        dz=swam_Len/2.0-swam_WallThk
      Shape     PGON  Phi1=0   Dphi=360  Nz=2,
                      NpDiv=18,
                      zi ={-swam_Len/2.0+swam_WallThk,
                            swam_Len/2.0-swam_WallThk},
                      rmn={swam_Rmin,swam_Rmin},
                      rmx={swam_Rmin+swam_WallThk,swam_Rmin+swam_WallThk}
Endblock
*
*------------------------------------------------------------------------------
*
Block SWMT is the water manifold top piece (big r)
      Material   Aluminium
      Attribute  SWMT   Seen=1   Colo=2
*      Shape      TUBE   Rmin=swam_Rmax-swam_WallThk,
*                        Rmax=swam_Rmax,
*                        dz=swam_Len/2.0-swam_WallThk
      Shape     PGON  Phi1=0   Dphi=360  Nz=2,
                      NpDiv=18,
                      zi ={-swam_Len/2.0+swam_WallThk,
                            swam_Len/2.0-swam_WallThk},
                      rmn={swam_Rmax-swam_WallThk,swam_Rmax-swam_WallThk},
                      rmx={swam_Rmax,swam_Rmax}
Endblock
*
*------------------------------------------------------------------------------
*
Block SWMS is the water manifold side pieces
      Material   Aluminium
      Attribute  SWMS   Seen=1   Colo=2
*      Shape      TUBE   Rmin=swam_Rmin,
*                        Rmax=swam_Rmax,
*                        dz=swam_WallThk/2.0
      Shape     PGON  Phi1=0   Dphi=360  Nz=2,
                      NpDiv=18,
                      zi ={-swam_WallThk/2.0,swam_WallThk/2.0},
                      rmn={swam_Rmin,swam_Rmin},
                      rmx={swam_Rmax,swam_Rmax}
Endblock
*
*------------------------------------------------------------------------------
*
Block SWMW is the water in the water manifold
      Component H2     A=1   Z=1   W=2
      Component O      A=16  Z=8   W=1
      Mixture   Water  Dens=1.0
      Attribute SWMW   seen=1  colo=6
*      Shape     TUBE   Rmin=swam_Rmin+swam_WallThk,
*                       Rmax=swam_Rmax-swam_WallThk,
*                       dz=swam_Len/2.0-swam_WallThk
      Shape     PGON  Phi1=0   Dphi=360  Nz=2,
                      NpDiv=18,
                      zi ={-swam_Len/2.0+swam_WallThk,
                            swam_Len/2.0-swam_WallThk},
                      rmn={swam_Rmin+swam_WallThk,swam_Rmin+swam_WallThk},
                      rmx={swam_Rmax-swam_WallThk,swam_Rmax-swam_WallThk}
Endblock
*
*------------------------------------------------------------------------------
*
Block SOTB is the outer transition board (large r)
*     G10 is about 60% SiO2 and 40% epoxy (stolen from ftpcgeo.g)
         Component Si  A=28.08  Z=14   W=0.6*1*28./60.
         Component O   A=16     Z=8    W=0.6*2*16./60.
         Component C   A=12     Z=6    W=0.4*8*12./174.
         Component H   A=1      Z=1    W=0.4*14*1./174.
         Component O   A=16     Z=8    W=0.4*4*16./174.
      Mixture   G10    Dens=1.7
      Attribute SOTB   Seen=1   Colo=3
*      Shape     TUBE   Rmin=swam_Rmax,
*                       Rmax=swam_Rmax+swam_TbrdThk,
*                       dz=swam_Len/2.0
      Shape     PGON  Phi1=0   Dphi=360  Nz=2,
                      NpDiv=18,
                      zi ={-swam_Len/2.0,+swam_Len/2.0},
                      rmn={swam_Rmax,swam_Rmax},
                      rmx={swam_Rmax+swam_TbrdThk,swam_Rmax+swam_TbrdThk}
Endblock
*
*------------------------------------------------------------------------------
*
Block SITB is the inner transition board (small r)
*     The material is G-10
      Component C      A=12  Z=6  W=1
      Component H2     A=1   Z=1  W=2
      Mixture   CH2    Dens=0.935
      Attribute SITB   Seen=1   Colo=3
*      Shape     TUBE   Rmin=swam_Rmin-swam_TbrdThk,
*                       Rmax=swam_Rmin,
*                       dz=swam_Len/2.0
      Shape     PGON  Phi1=0   Dphi=360  Nz=2,
                      NpDiv=18,
                      zi ={-swam_Len/2.0,+swam_Len/2.0},
                      rmn={swam_Rmin-swam_TbrdThk,swam_Rmin-swam_TbrdThk},
                      rmx={swam_Rmin,swam_Rmin}
Endblock
*
*******************************************************************************
*
Block SBWC is the bracket connecting the water manifold to the cone
      rin=swam_Rmin
      rou=ssup_Cone1idmn
      Material  Air
      Attribute SBWC  seen=0  colo=1
      Shape     TUBE  Rmin=rin,
		      Rmax=rou,
                      dz=(ssup_Cone1zmn-(swam_Zmin+swam_Len))/2.0
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
      Position  SXAI  Z=(-(ssup_Cone1zmn-(swam_Zmin+swam_Len))/2.0 _
                        +ssup_BraThk/2.0) 
      Create    SXBI
      Position  SXBI  Z=(ssup_BraThk/2.0) 
*
EndBlock
*
* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
*
Block SXAI is a first piece (A) of the bracket between mani and cone (X)
      Attribute SXAI   Seen=1    Colo=2
      Material   Aluminium
      Shape    TUBS rmin=swam_Rmin,
                    rmax=ssup_Cone1idmn,
                    dz=ssup_BraThk/2.0,
                    phi1=-5,
		    phi2=5
EndBlock
*
* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
*
Block SXBI is a second piece (B) of the bracket between mani and cone (X)
      Attribute SXBI   Seen=1    Colo=2
      Material   Aluminium
      Shape    TUBS rmin=ssup_Cone1idmn-ssup_BraThk,
                    rmax=ssup_Cone1idmn,
                    dz=((ssup_Cone1zmn-(swam_Zmin+swam_Len)- _
                         ssup_BraThk)/2.0),
                    phi1=-5,
		    phi2=5
EndBlock
* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Block SCRW is the screw which attaches the end ring to the end ring bracket
*     The material is a guess, checking with Chong-Jer
      Material  Berillium
      Attribute SCRW   Seen=1  Colo=2
      Shape     TUBE   rmin=0,
                       rmax=ssup_ERJdia/2.0,  
                       dz=0.5*(brack_z-endrng_z)
Endblock
*
*******************************************************************************
*
      End

