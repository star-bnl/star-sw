* $Id: ecalgeo.g,v 1.7 2001/03/15 01:14:19 nevski Exp $
* $Name:  $
* $Log: ecalgeo.g,v $
* Revision 1.7  2001/03/15 01:14:19  nevski
* first approach to forward pion detector
*
*
******************************************************************************
Module ECALGEO is the EM EndCap Calorimeter GEOmetry
Created   26 jan 1996
Author    Rashid Mehdiyev
*
* Version 1.1, W.J. Llope
*		- changed sensitive medium names...
*
* Version 2.0, R.R. Mehdiyev                                  16.04.97
*               - Support walls included
*               - intercell and intermodule gaps width updated
*               - G10 layers inserted
* Version 2.1, R.R. Mehdiyev                                  23.04.97
*               - Shower Max Detector geometry added          
*               - Variable eta grid step size introduced 
* Version 2.2, R.R. Mehdiyev                                  03.12.97
*               - Eta grid corrected 
*               - Several changes in volume's dimensions
*               - Material changes in SMD
*       
* Version 3.0, O. Rogachevski                                 28.11.99
*               - New proposal for calorimeter SN 0401
*
* Version 4.0, O. Akio                                        23 Jan 01
*               - Include forward pion detectors
* 		      
******************************************************************************
+CDE,AGECOM,GCONST,GCUNIT.
*
      Content    ECAL,ECVO,EMDI,EMSS,EAGA,ETOW,ESEC,EPER,EFLP,EXFP,ESHM,
                 ESSP,ETAR,EGTN,ESCI,ELED,EMOD,EXGT,EXSG,EALP,EHMS,
		 ELGD,ELGT,EWAL,ELGR,ESRB,EPCT,EUMT
*
      Structure  EMCG { Version,ZOrig(2),ZEnd(2),EtaMin(2),EtaMax(2),
			PhiMin(2),PhiMax(2),Offset(2),
			Nsupsec(2),Nsector(2),Nsection(2),Nslices(2),
			Front,Gten,Plate(2),PlateS,PlateB,
                        Hub,Rmshift,SMShift,GapPlt,GapCel,GapSMD}
*
      Structure  EETR { Etagr,Phigr,Neta(2),EtaBin(13),EtaBin2(5)}
*
      Structure  ESEC { Isect, FPlmat, Cell, Scint(2), Nlayer }
*
      Structure  EMXG {Version,Sapex,Sbase}
*
      Structure  EXSE {Jsect,Swidth,Aplate}
*
      Structure  ELGG {Width,Depth,ZPos,DGap,NPhi,NEta,RDis,
	               AlThick,SiRubDz,PhCathDz,PhCathR,MuMetDz,MuMetR}
*
      Structure  ELGM {Density,RadLen,Index,PbContent,CritEne,MoliereR}
*
      Integer    I_section,J_section,Ie,is,isec,i_str,Nstr,Type,ii,jj
*			
      Real       Secwid,Section,center,current,Plate,Gap,Cell,G10,
                 tan_low,tan_upp,Tanf,RBot,Rtop,Deta,etax,
                 dup,dd,d2,d3,rshift,dphi,
                 maxcnt,msecwd,mxgten,curr,
		 curcl,EtaTop,EtaBot,
		 xleft,xright,yleft,yright,
		 sq2,sq3,rth,tng,len,p,xc,yc,diff,
		 xx,yy,zz


    Integer    N
    Parameter (N=12)
* --- Enegry bins
    real E(N)  /3.2e-9, 3.5e-9, 3.75e-9, 4.0e-9, 4.2e-9, 4.4e-9,
                4.6e-9, 5.0e-9, 5.5e-9,  6.0e-9, 6.5e-9, 7.0e-9/
* --- Refraction indexes
    real rindex_PbG(N)    /N*1.67/
    real rindex_SiRub(N)  /N*1.67/
    real rindex_PhCath(N) /N*1.67/
    real rindex_Alm(N)    /N*1.0/
    real rindex_MuMet(N)  /N*1.0/
* ---- Absorbtion lenghts (in cm)
    real absco_PbG(N)     /N*38.0/
    real absco_SiRub(N)   /N*38.0/ 
    real absco_PhCath(N)  /N*0.0001/
    real absco_Alm(N)     /N*0.0001/
    real absco_MuMet(N)   /N*0.0001/
* ---- Detection efficiencies (quantum efficiency for Photo Cathode)
    real effic_PhCath(N)  / 0.09,   0.13,   0.175,   0.195,  0.2,    0.18,
                            0.165,  0.155,  0.14,    0.1,    0.08,   0.06/
    real effic_all(N)    /N*1./
* 
    Tanf(etax) = tan(2*atan(exp(-etax)))
*
* ----------------------------------------------------------------------------
*
Fill  EMCG                          ! EM EndCAp Calorimeter basic data 
      Version  = 4                  ! Geometry version 
      ZOrig    = {273.5,   800}     ! calorimeter origin in z
      ZEnd     = {310.007, 836.507} ! Calorimeter end in z
      EtaMin   = {1.078,   1.6317}  ! upper feducial eta cut 
      EtaMax   = {2.0,     2.0}     ! lower feducial eta cut
      PhiMin   = {-180,    -9}      ! Min phi 
      PhiMax   = {180,     9}       ! Max phi
      Offset   = {0.0,  50.0}	    ! offset in x
      Nsupsec  = {12,      1}       ! Number of azimuthal supersectors        
      Nsector  = {60,      3}       ! Number of azimutal sectors (Phi granularity)
      Nslices  = {5,       3}       ! number of phi slices in supersector
      Nsection = {3,       3}       ! Number of readout sections
      Front    = 0.953              ! thickness of the front AL plates
      Gten     = 0.16               ! Fiber routing guides
      Plate    = {0.468, 0.5}       ! Lead radiator thickness
      PlateS   = 0.05               ! Laminated SS plate thickness
      PlateB   = 3.175              ! Back plate thickness SS
      Hub      = 2.5                ! thickness of EndCap hub
      Rmshift  = 1.228              ! radial shift of module
      smshift  = 0.12               ! radial shift of steel support walls
      GapPlt   = 0.3/2              ! HALF of the inter-plate gap in phi
      GapCel   = 0.03/2             ! HALF of the radial inter-cell gap
      GapSMD   = 3.2                ! space for SMD detector
* --------------------------------------------------------------------------
Fill EETR                      ! Eta and Phi grid values
      EtaGr    = 1.0536        ! eta_top/eta_bot tower granularity
      PhiGr    = 0.0981747     ! Phi granularity (radians)
      NEta     = {12,4}        ! Eta granularity
      EtaBin   = {2.0,1.9008,1.8065,1.7168,1.6317,1.5507,1.4738,1.4007,1.3312,1.2651,1.2023,1.1427,1.086}! Eta rapidities
      EtaBin2  = {2.0,1.9,1.8,1.7,1.6} ! Eta rapidities for FPD
*---------------------------------------------------------------------------
Fill ESEC           ! First EM section
      ISect    = 1                           ! Section number	
      Nlayer   = 2                           ! Number of Sci layers along z
      Cell     = 1.328                       ! Cell full width in z
      Scint    = {0.5,0.5}    		     ! Sci layer thickness
*
Fill ESEC           ! Second EM section
      ISect    = 2                           ! Section number
      Nlayer   = 3                           ! Number of Sci layers along z
      Cell     = 1.228                       ! Cell full width in z
      Scint    = {0.4,0.5}     		     ! Sci layer thickness
*
Fill ESEC           ! Third EM section
      ISect    = 3                           ! Section
      Nlayer   = 19                          ! Number of Sci layers along z
      Cell     = 1.228                       ! Cell full width in z
      Scint    = {0.4,0.5}      	     ! Sci layer thickness
*----------------------------------------------------------------------------
Fill EMXG           ! EM Endcap SMD basic data
     Version   = 1         		     ! Geometry version
     Sapex     = 0.5       		     ! Scintillator strip apex
     Sbase     = 1.0       		     ! Scintillator strip base
*----------------------------------------------------------------------------
Fill EXSE           ! First SMD section
      JSect    = 1                           ! Section number
      Swidth   = 1.6                         ! Section width
      Aplate   = 0.15                        ! Width of Aluminium plate
*
Fill EXSE           ! Second SMD section
      JSect    = 2                           ! Section number	
      Swidth   = 1.6                         ! Section width
      Aplate   = 0.15                        ! Width of Aluminium plate
*----------------------------------------------------------------------------
Fill ELGG                                    ! PbG detector geometry
      Width    = 3.8			     ! PbG width	
      Depth    = 45.0			     ! PbG depth
      DGap     = 0.1			     ! Gap between PbG
      Zpos     = 800.0                       ! Z position
      NPhi     = 4			     ! # of tower in phi
      NEta     = 4                           ! # of tower in eta
      RDis     = 30.0			     ! distance from beam
      AlThick  = 0.002			     ! almunim wrap thinkness
      SiRubDz  = 2.0			     ! silicon lubber thinkness
      PhCathDz = 2.0 			     ! Photo Cathode thinkness
      PhCathR  = 1.7 			     ! Photo Cathode radius
      MuMetDz  = 11.0 			     ! Mu Metal Length
      MuMetR   = 1.8 			     ! Mu metal outer Radius
*
Fill ELGM				     ! PbG detector materials
      Density  = 3.86			     ! gdensity [/cm^3]
      RadLen   = 2.5			     ! radiation length [cm]
      PbContent= 65.4			     ! PbO content [%]
      CritEne  = 0.0158   		     ! critical energy [GeV]
      MoliereR = 3.32			     ! Moliere radius [cm]
*
*----------------------------------------------------------------------------
*
      Use    EMCG
      Use    ELGG
*
      prin0 emcg_version 
	('ECALGEO version ', F4.2)
*
      Type=1 			! Endcap 
      if(emcg_Nsupsec(Type)>0) then
        diff = 0.0
        center  = (emcg_ZOrig(1)+emcg_ZEnd(1))/2
        Tan_Upp = tanf(emcg_EtaMin(1))
        Tan_Low = tanf(emcg_EtaMax(1))
        rshift  = emcg_Hub * sqrt(1. + Tan_Low*Tan_Low)
        dup=emcg_Rmshift*Tan_Upp
        dd=emcg_Rmshift*Tan_Low
        d2=rshift + dd
*       d3=emcg_Rmshift-2*emcg_smshift
        dphi = (emcg_PhiMax(Type)-emcg_PhiMin(Type))/emcg_Nsector(Type)
        Create    ECAL
        Position  ECAL in CAVE    z=+center 
*       Position  ECAL in CAVE    z=-center ThetaZ=180  

        If(section > emcg_Zend(1)) then
           prin0 section,emcg_Zend(1)
           (' ECALGEO error: sum of sections exceeds maximum ',2F12.4)
        endif
*       prin0 section
*       (' EndCap calorimeter total depth ',F12.4)
      endif
 
      Type=2			! Foward Pion detector
      if(emcg_Nsupsec(Type)>0) then
        diff = emcg_ZOrig(2) - emcg_ZOrig(1)
        center = (emcg_ZOrig(2)+emcg_ZEnd(2))/2
        Tan_Upp = tanf(emcg_EtaMin(2))  
        Tan_Low = tanf(emcg_EtaMax(2))
        rshift  = emcg_Hub * sqrt(1. + Tan_Low*Tan_Low)
        dup=emcg_Rmshift*Tan_Upp
        dd=emcg_Rmshift*Tan_Low
        d2=rshift + dd
        dphi = (emcg_PhiMax(Type)-emcg_PhiMin(Type))/emcg_Nsector(Type)
        Create    ECAL
        Position  ECAL in CAVE  z=-center x=emcg_Offset(Type) ThetaZ=180

        If(section > emcg_Zend(2)) then
          prin0 section,emcg_Zend(2)
          (' ECALGEO error: sum of sections exceeds maximum ',2F12.4)
        endif
      endif

      Type=3                     ! PbG detectors
      if(ELGG_NEta*ELGG_NPhi>0) then
	zz = (ELGG_Depth+ELGG_AlThick+ELGG_MuMetDz)/2.0
        yy = (ELGG_NEta*ELGG_Width + (ELGG_NEta+1)*ELGG_DGap)/2.0

	Create and Position ELGD in CAVE z=-(ELGG_ZPos+zz) y=ELGG_Rdis+yy x=0,
 			       phix=180  phiy=90   phiz=0,
			       thetax=90 thetay=90 thetaz=180 
	Create and Position ELGD in CAVE z=-(ELGG_ZPos+zz) x=ELGG_Rdis+yy y=0,
 			       phix=90   phiy=0    phiz=0,
			       thetax=90 thetay=90 thetaz=180 
	Create and Position ELGD in CAVE z=-(ELGG_ZPos+zz) y=-(ELGG_Rdis+yy) x=0,
 			       phix=0    phiy=-90  phiz=0,
			       thetax=90 thetay=90 thetaz=180 
      endif
      prin1
	('ECALGEO finished')
*
* ----------------------------------------------------------------------------
Block ECAL is one EMC EndCap wheel
      Material  Air
      Medium    standard
      Attribute ECAL   seen=1 colo=7				!  lightblue
      shape     CONE   dz=(emcg_Zend(Type)-emcg_ZOrig(Type))/2,
                Rmn1=emcg_ZOrig(1)*Tan_Low-d2 Rmn2=emcg_ZEnd(1)*Tan_Low-d2,
                Rmx1=emcg_ZOrig(1)*Tan_Upp+dup Rmx2=emcg_ZEnd(1)*Tan_Upp+dup,
	        phi1=emcg_PhiMin(Type) phi2=emcg_PhiMax(Type)

      if (Type=1) then
      Create  and Position ECVO thetax=90 thetay=90 thetaz=0,
                                phix = 75 phiy =-15 phiz=0
      else
      Create  and Position ECVO
      endif

EndBlock
* ----------------------------------------------------------------------------
Block ECVO is one EMC EndCap wheel volume
      Material  Air
      Attribute ECVO   seen=1 colo=3				! green
      shape     CONE   dz=(emcg_Zend(Type)-emcg_ZOrig(Type))/2,
                Rmn1=emcg_ZOrig(1)*Tan_Low-d2 Rmn2=emcg_ZEnd(1)*Tan_Low-d2,
                Rmx1=emcg_ZOrig(1)*Tan_Upp+dup Rmx2=emcg_ZEnd(1)*Tan_Upp+dup,
	        phi1=emcg_PhiMin(Type) phi2=emcg_PhiMax(Type)
*
      Create    EMDI 
EndBlock
* ----------------------------------------------------------------------------
Block EMDI is one 1/12 phi-division of the EMC EndCap
      Attribute EMDI      seen=1    colo=2     !  red
*     phi1, phi2 are not really used here but will be inherited by daughters
      Shape     Division  Iaxis=2   Ndiv=nint(emcg_Nsupsec(Type)),
                          phi1=emcg_PhiMin(Type)/emcg_Nsupsec(Type),
			  phi2=emcg_PhiMax(Type)/emcg_Nsupsec(Type)
 
*     Create and Position EMSS x=+emcg_smshift   
      Create and Position EMSS

Endblock
* ----------------------------------------------------------------------------
Block EMSS is steel support of the EndCap module
      Attribute EMSS      seen=1    colo=1		! black
			
      Material  Iron
      Shape     CONS   dz=(emcg_Zend(Type)-emcg_ZOrig(Type))/2,
                Rmn1=emcg_ZOrig(1)*Tan_Low-d2  Rmn2=emcg_ZEnd(1)*Tan_Low-d2,
                Rmx1=emcg_ZOrig(1)*Tan_Upp+dup Rmx2=emcg_ZEnd(1)*Tan_Upp+dup,
                phi1=emcg_PhiMin(Type)/emcg_Nsupsec(Type),
		phi2=+emcg_PhiMax(Type)/emcg_Nsupsec(Type)
*
      Create and Position EAGA

EndBlock
* ----------------------------------------------------------------------------
Block EAGA is air gap in sector of the EM EndCap
      Attribute EAGA      seen=1  colo=7			!  lightblue
      Material  Air
      Shape     CONS   dz=(emcg_Zend(Type)-emcg_ZOrig(Type))/2,
                Rmn1=emcg_ZOrig(1)*Tan_Low-dd Rmn2=emcg_ZEnd(1)*Tan_Low-dd,
                Rmx1=emcg_ZOrig(1)*Tan_Upp+dup Rmx2=emcg_ZEnd(1)*Tan_Upp+dup

       Create and Position EMOD  

EndBlock
* ----------------------------------------------------------------------------
Block EMOD is one module  of the EM EndCap
      Attribute EMOD      seen=1    colo=3			! green
      Material  Air
      Shape     CONS   dz=(emcg_Zend(Type)-emcg_ZOrig(Type))/2,
                Rmn1=emcg_ZOrig(1)*Tan_Low-dd  Rmn2=emcg_ZEnd(1)*Tan_Low-dd,
                Rmx1=emcg_ZOrig(1)*Tan_Upp+dup Rmx2=emcg_ZEnd(1)*Tan_Upp+dup
*
*    Running parameter 'section' contains the position of the current section
*     It should not be modified in daughters, use 'current' variable instead.
*     SecWid is used in all 'CONS' daughters to define dimensions.
*
      section = emcg_ZOrig(Type)
      secwid  = emcg_Front
      Create and Position EFLP     z=section-center+secwid/2
      section = section + secwid
*
        Do I_section =1,nint(Emcg_Nsection(Type))

         USE ESEC Isect=I_section  
*
         Secwid  = esec_cell*esec_Nlayer
         if (I_section == 3) then      ! Last section
          Secwid  = Secwid - emcg_plate(TYPE) - 2*emcg_plateS
				 endif
         Create and position ESEC      z=section-center+secwid/2
         section = section + Secwid
* 
         if (I_section == 2) then      ! Shower Max section
*
            secwid  = emcg_GapSMD
            Create and Position ESHM   z=section-center+secwid/2
            section = section + secwid
*
         endif
*
      enddo
         secwid  = emcg_PlateB
         Create and Position ESSP      z=section-center+secwid/2
         section = section + secwid
endblock
* ----------------------------------------------------------------------------
Block ESEC is a sinle EM section
      Attribute ESEC   seen=1  colo=1 
      Material Air
      Material CAir Isvol=0
      Medium standard
*
      Shape     CONS  dz=secwid/2,  
                rmn1=(section-diff)*Tan_Low-dd rmn2=(section+secwid-diff)*Tan_Low-dd,
                rmx1=(section-diff)*Tan_Upp+dup rmx2=(section+secwid-diff)*Tan_Upp+dup
      if (I_section==1) then
       Call GSTPAR (ag_imed,'CUTGAM',0.00001)
       Call GSTPAR (ag_imed,'CUTELE',0.00001)
       else
       Call GSTPAR (ag_imed,'CUTGAM',0.00008)
       Call GSTPAR (ag_imed,'CUTELE',0.001)
       Call GSTPAR (ag_imed,'BCUTE',0.0001)
      end if
*
      Do isec=1,nint(emcg_Nslices(type))
        Create and Position ETOW AlphaZ=(isec-emcg_Nslices(TYPE)/2.0-0.5)*dphi
      End Do 
Endblock
*---------------------------------------------------------------------------
Block ETOW is an individual 1/60 phi EM tower (section in fact)
      Attribute ETOW   seen=1  colo=1
      Shape     CONS  dz=secwid/2, 
                phi1=emcg_PhiMin(Type)/emcg_Nsector(Type),
		phi2=+emcg_PhiMax(Type)/emcg_Nsector(Type),
                rmn1=(section-diff)*Tan_Low-dd rmn2=(section+secwid-diff)*Tan_Low-dd,
                rmx1=(section-diff)*Tan_Upp+dup rmx2=(section+secwid-diff)*Tan_Upp+dup
      current = section
      Do is = 1,esec_Nlayer

*        define actual Plate and cell thickness:         
         Plate  = emcg_Plate(Type) + 2*emcg_PlateS
         Gap = esec_cell - Plate - esec_scint(Type)
         Cell = esec_cell
*
         if (is==nint(esec_Nlayer) & I_section == 3) then
          Cell = esec_cell - Plate  
				  Plate=0
				 endif
*		 
         Create    EPER 
         Position  EPER  z=-secwid/2+(is-1)*esec_cell+Cell/2 
         current = current + cell
      End Do
*
endblock
* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Block EPER  is a EM sesection period (super layer)
*
      Material  Air 
      Attribute EPER   seen=1  colo=1
      Shape     CONS   dz=Cell/2, 
                phi1=emcg_PhiMin(Type)/emcg_Nsector(Type),
		phi2=+emcg_PhiMax(Type)/emcg_Nsector(Type), 
                rmn1=(current-diff)*Tan_Low-dd  rmn2=(current+Cell-diff)*Tan_Low-dd,
                rmx1=(current-diff)*Tan_Upp+dup  rmx2=(current+Cell-diff)*Tan_Upp+dup
*
* --- Divide module (section) into radial blocks 
* 
      curcl = current+Cell/2
      Do ie = 1,nint(eetr_NEta(Type))
	if(Type=1)then
          EtaBot  = eetr_EtaBin(ie)
          EtaTop  = eetr_EtaBin(ie+1)
	else
          EtaBot  = eetr_EtaBin2(ie)
          EtaTop  = eetr_EtaBin2(ie+1)
        endif

        if(ie == 1) then         ! Lower slice
          RBot=(current-diff)*Tan_Low
        else                     ! 
          RBot=(curcl-diff)*Tanf(EtaBot)
        endif
*
        if(Plate > 0) then         ! Ordinary Sci layer
         RTop=min((curcl-diff)*Tanf(EtaTop), _
				         ((current-diff)*Tan_Upp+dup))
        else                     ! last Sci layer in section
         RTop=min((curcl-diff)*Tanf(EtaTop), _
				         ((current-diff)*Tan_Upp+dup))
        endif
        check RBot<RTop
*
	xx=tan(pi*emcg_PhiMax(Type)/180.0/emcg_Nsector(Type))
        yy=cos(pi*emcg_PhiMax(Type)/180.0/emcg_Nsector(Type))
	Create and Position  ETAR    x=(RBot+RTop)/2  ORT=YZX 
*         prin0 ie,EtaTop,EtaBot,rbot,rtop
*         (' EPER : ie,EtaTop,EtaBot,rbot,rtop ',i3,4F12.4)
      enddo
*
EndBlock
*  - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Block ETAR is one CELL of scintillator, fiber and laminated lead
*
      Attribute ETAR   seen=1  colo=2				! violet
*     local z goes along the radius, y is the thickness
      Shape     TRD1   dy=Cell/2   dz=(RTop-RBot)/2,
           dx1=RBot*xx-emcg_GapCel/yy,
           dx2=RTop*xx-emcg_GapCel/yy
*
      G10 = emcg_Gten
      Create and Position    ESCI        y=(-cell + esec_scint(Type))/2
      Create and Position    EGTN        y=(-cell+G10)/2+esec_scint(Type)
      if (Plate>0) then
      Create and Position    EXFP        y=(cell + emcg_plateS)/2 - plate
      Create and Position    ELED        y=(cell - plate)/2
      Create and Position    EXFP        y=(cell - emcg_plateS)/2
      end if
EndBlock
* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Block EGTN  is the G10 layer  
*
*     G10 is about 60% SiO2 and 40% epoxy
      Component Si    A=28.08  Z=14   W=0.6*1*28./60.
      Component O     A=16     Z=8    W=0.6*2*16./60.
      Component C     A=12     Z=6    W=0.4*8*12./174.
      Component H     A=1      Z=1    W=0.4*14*1./174.
      Component O     A=16     Z=8    W=0.4*4*16./174.
      Mixture   g10   Dens=1.7
      Attribute EGTN   seen=1   colo=4			! blue       
*     local z goes along the radius, y is the thickness
      Shape     TRD1   dy=Emcg_GTen/2  dz=(RTop-RBot)/2
EndBlock
* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Block ESCI  is the active scintillator (polystyren) layer  
*
  Material  POLYSTYREN
      Material  Cpolystyren   Isvol=1
      Attribute ESCI   seen=1   colo=7   fill=0   	! lightblue
*     local z goes along the radius, y is the thickness
      Shape     TRD1   dy=esec_scint(Type)/2  dz=(RTop-RBot)/2-emcg_GapCel
      Call GSTPAR (ag_imed,'CUTGAM',0.00008)
      Call GSTPAR (ag_imed,'CUTELE',0.001)
      Call GSTPAR (ag_imed,'BCUTE',0.0001)
      Call GSTPAR (ag_imed,'CUTNEU',0.001)
      Call GSTPAR (ag_imed,'CUTHAD',0.001)
      Call GSTPAR (ag_imed,'CUTMUO',0.001)
* define Birks law parameters
      Call GSTPAR (ag_imed,'BIRK1',1.)
      Call GSTPAR (ag_imed,'BIRK2',0.013)
      Call GSTPAR (ag_imed,'BIRK3',9.6E-6)
*     
       HITS ESCI   Birk:0:(0,10)
*                  xx:16:H(-250,250)   yy:16:(-250,250)   zz:16:(-350,350),
*                  px:16:(-100,100)    py:16:(-100,100)   pz:16:(-100,100),
*                  Slen:16:(0,1.e4)    Tof:16:(0,1.e-6)   Step:16:(0,100),
*                  none:16:         
endblock
* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Block ELED  is lead absorber Plate 
*
      Material  Lead
      Material  CLead Isvol=0
      Attribute ELED   seen=1   colo=3  fill=1			! green
      Shape     TRD1   dy=emcg_Plate(Type)/2  dz=(RTop-RBot)/2
      Call GSTPAR (ag_imed,'CUTGAM',0.00008)
      Call GSTPAR (ag_imed,'CUTELE',0.001)
      Call GSTPAR (ag_imed,'BCUTE',0.0001)
      Call GSTPAR (ag_imed,'CUTNEU',0.001)
      Call GSTPAR (ag_imed,'CUTHAD',0.001)
      Call GSTPAR (ag_imed,'CUTMUO',0.001)

endblock
* ----------------------------------------------------------------------------
Block EFLP  is First Aluminium plate 
*
      Material  Aluminium
      Attribute EFLP   seen=1  colo=3 fill=1			! green
      Shape     TUBS   dz=SecWid/2,
                       rmin=(section-diff)*Tan_Low,
                       rmax=(section-diff)*Tan_Upp+dup,
                       phi1=emcg_PhiMin(Type)/emcg_Nsupsec(Type),
		       phi2=emcg_PhiMax(Type)/emcg_Nsupsec(Type)
*                rmn1=(section-diff)*Tan_Low-dd rmn2=(section+secwid-diff)*Tan_Low-dd,
*                rmx1=(section-diff)*Tan_Upp-dd rmx2=(section+secwid-diff)*Tan_Upp-dd
endblock
* ----------------------------------------------------------------------------
Block EXFP  is SS laminated plate
*
      Material  Iron
      Attribute EXFP   seen=1  colo=6 fill=1		! violet
      Shape     TRD1   dy=emcg_PlateS/2  dz=(RTop-RBot)/2
endblock
* ----------------------------------------------------------------------------
Block ESHM  is the SHower Max  section
*
      Material  Air Isvol=0
      Attribute ESHM   seen=1   colo=4			!  blue
      Shape     CONS   dz=SecWid/2,
                phi1=emcg_PhiMin(Type)/emcg_Nsupsec(Type),
		phi2=+emcg_PhiMax(Type)/emcg_Nsupsec(Type),
                rmn1=(section-diff)*Tan_Low-dd rmn2=(section+secwid-diff)*Tan_Low-dd,
                rmx1=(section-diff)*Tan_Upp+dup rmx2=(section+secwid-diff)*Tan_Upp+dup
*      Call GSTPAR (ag_imed,'CUTGAM',0.00001)
*      Call GSTPAR (ag_imed,'CUTELE',0.00001)
*      Call GSTPAR (ag_imed,'LOSS',1.)
*      Call GSTPAR (ag_imed,'STRA',1.)
*
      USE EMXG Version=1
      curr =  section
      maxcnt = curr+emcg_GapSMD/2 
*
        Do J_section = 1,2
*       
         USE EXSE Jsect=J_section
*
	 msecwd=exse_Swidth - exse_Aplate - emxg_Sapex
         Create and Position EXGT      z=curr-maxcnt+msecwd/2
         curr = curr + msecwd
*
         msecwd  = emxg_Sapex
	 if (J_section =1) then
  	   rtop = curr+msecwd/2
*          prin0 rtop
*          (' Z for u:  ',F12.4)
           Create and position EXSG   z=curr-maxcnt+msecwd/2
	 else
	   rtop = curr+msecwd/2
*          prin0 rtop
*          (' Z for v:  ',F12.4)
           Create and position EXSG   z=curr-maxcnt+msecwd/2 AlphaX=180
				 
         endif
           curr = curr + msecwd
*
           msecwd  = exse_Aplate
           Create and Position EALP   z=curr-maxcnt+msecwd/2
           curr = curr + msecwd
        End do

Endblock
* ----------------------------------------------------------------------------
Block EXGT  is the G10 layer in the SMax  
*
*     G10 is about 60% SiO2 and 40% epoxy
      Component Si    A=28.08  Z=14   W=0.6*1*28./60.
      Component O     A=16     Z=8    W=0.6*2*16./60.
      Component C     A=12     Z=6    W=0.4*8*12./174.
      Component H     A=1      Z=1    W=0.4*14*1./174.
      Component O     A=16     Z=8    W=0.4*4*16./174.
      Mixture   g10   Dens=1.7
      Attribute EXGT   seen=1   colo=7
      Shape     CONS   dz=msecwd/2,
                phi1=emcg_PhiMin(Type)/emcg_Nsupsec(Type),
		phi2=emcg_PhiMax(Type)/emcg_Nsupsec(Type),
                rmn1=(curr-diff)*Tan_Low-dd rmn2=(curr+msecwd-diff)*Tan_Low-dd,
                rmx1=(curr-diff)*Tan_Upp-dd rmx2=(curr+msecwd-diff)*Tan_Upp-dd    
      Call GSTPAR (ag_imed,'CUTGAM',0.00001)
      Call GSTPAR (ag_imed,'CUTELE',0.00001)
EndBlock
* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Block EXSG  is the Shower max  Gap for scintillator strips
*
      Attribute EXSG   seen=1   colo=7			! black
      Material  Air   Isvol=0
      Shape     CONS   dz=msecwd/2,
                phi1=emcg_PhiMin(Type)/emcg_Nsupsec(Type),
		phi2=emcg_PhiMax(Type)/emcg_Nsupsec(Type),
                rmn1=(curr-diff)*Tan_Low-dd  rmn2=(curr+msecwd-diff)*Tan_Low-dd,
                rmx1=(curr-diff)*Tan_Upp+dup rmx2=(curr+msecwd-diff)*Tan_Upp+dup
*
      sq3 = sqrt(3.)
      sq2 = sqrt(2.)
      Rbot = (curr-diff+msecwd/2.)*Tan_Low-dd
      Rtop = (curr-diff+msecwd/2.)*Tan_Upp+dup
*
      if (Type=1) then
        rth = Pi/emcg_Nsupsec(Type)
        tng = tan(rth)
	Nstr = nint((rtop*sq3 - rbot)/emxg_Sbase)
	if(Nstr > 300) Nstr = 300
        Do i_str = 1,nstr
        	p = .5*(i_str*emxg_Sbase + rbot)
*
		if (p <= .5*rbot*sq3) then
        		yleft = .5*(sq2*p - sqrt(2.*rbot*rbot - 2.*p*p))
			xleft = sq2*p - yleft
   			xright = sq2*p/(1. - tng )
   			yright = -tng*xright
		else if (.5*rbot < p <= .5*Rtop) then 
   			xleft = sq2*p/(1. + tng )
   			yleft = tng*xleft
   			xright = sq2*p/(1. - tng )
   			yright = -tng*xright
		else if (.5*rtop < p <= .5*rtop*sq3) then
   			xleft = sq2*p/(1. + tng )
   			yleft = tng*xleft
        	 	yright = .5*(sq2*p - sqrt(2.*rtop*rtop - 2.*p*p))
			xright = sq2*p - yright
		endif

		len = sqrt((xleft-xright)**2 + (yleft-yright)**2 )
		xc = .5*(xright+xleft)
		yc = .5*(yright+yleft)
*         	prin0 i_str,xleft,yleft,xright,yright,len
*		      (' EXSG: i,xleft,yleft,xright,yright,len',i3,5F12.4)
			 
      		if (mod(i_str,2) != 0 ) then			 
       			Create and Position EHMS  x=xc y=yc AlphaZ=45
		else
       			Create and Position EHMS  x=xc y=yc AlphaZ=45 AlphaX=180
		endif
	End do
      else
	if (J_section=1) then
	  Nstr = 60
	  len  = 36
          Do i_str = 1,nstr
		xc = (Rbot+Rtop)/2 + (i_str-Nstr/2.0)*emxg_Sbase/2.0
	        yc = 0.0
      		if (mod(i_str,2) != 0 ) then			 
       			Create and Position EHMS  x=xc y=yc
		else
       			Create and Position EHMS  x=xc y=yc AlphaX=180
		endif
	  End do
	else
	  Nstr = 100
	  len  = 36
          Do i_str = 1,nstr
		xc = (Rbot+Rtop)/2.0
		yc = (i_str-Nstr/2.0)*emxg_Sbase/2.0
      		if (mod(i_str,2) != 0 ) then			 
       			Create and Position EHMS  x=xc y=yc AlphaZ=90
		else
       			Create and Position EHMS  x=xc y=yc AlphaZ=90 AlphaX=180
		endif
	  End do
	endif
      endif

*     dcut exsg z 0 0 10 0.1 0.1
*     dcut exsg y 0 10 -50 0.7 0.7

endblock
* ----------------------------------------------------------------------------
Block EHMS is  sHower Max Strip
*
      Material  POLYSTYREN
      Material  Cpolystyren   Isvol=1
      Attribute EHMS      seen=1    colo=2		! red
      Shape     TRD1 dx1=0 dx2=emxg_Sbase/2 dy=len/2 dz=emxg_Sapex/2
      Call GSTPAR (ag_imed,'CUTGAM',0.00008)
      Call GSTPAR (ag_imed,'CUTELE',0.001)
      Call GSTPAR (ag_imed,'BCUTE',0.0001)
* define Birks law parameters
      Call GSTPAR (ag_imed,'BIRK1',1.)
      Call GSTPAR (ag_imed,'BIRK2',0.0130)
      Call GSTPAR (ag_imed,'BIRK3',9.6E-3)
*
       HITS EHMS     Birk:0:(0,10)
*                     xx:16:SH(-250,250)  yy:16:(-250,250)  zz:16:(-350,350),
*                     px:16:(-100,100)    py:16:(-100,100)  pz:16:(-100,100),
*                     Slen:16:(0,1.e4)    Tof:16:(0,1.e-6)  Step:16:(0,100),
*                     none:16:            Eloss:0:(0,10)
* 
Endblock
* ----------------------------------------------------------------------------
Block EALP  is ALuminium  Plate in shower max 
*
      Material  Aluminium
      Material  CAluminium   Isvol=0
      Attribute EALP   seen=1  colo=1
      Shape     CONS   dz=msecwd/2,
                phi1=emcg_PhiMin(Type)/emcg_Nsupsec(Type),
		phi2=+emcg_PhiMax(Type)/emcg_Nsupsec(Type),
                rmn1=(curr-diff)*Tan_Low-dd rmn2=(curr+msecwd/2-diff)*Tan_Low-dd,
                rmx1=(curr-diff)*Tan_Upp-dd rmx2=(curr+msecwd/2-diff)*Tan_Upp-dd
      Call GSTPAR (ag_imed,'CUTGAM',0.00001)
      Call GSTPAR (ag_imed,'CUTELE',0.00001)
      Call GSTPAR (ag_imed,'LOSS',1.)
      Call GSTPAR (ag_imed,'STRA',1.)
endblock
* ----------------------------------------------------------------------------
Block ESSP  is stainless steel  Plate 
*
      Material  Iron      
      Attribute ESSP   seen=1  colo=6 fill=1	
      Shape     CONS   dz=Emcg_PlateB/2,
                phi1=emcg_PhiMin(Type)/emcg_Nsupsec(Type),
		phi2=+emcg_PhiMax(Type)/emcg_Nsupsec(Type),
                rmn1=(section-diff)*Tan_Low-dd rmn2=(section+secwid-diff)*Tan_Low-dd,
                rmx1=(section-diff)*Tan_Upp+dup rmx2=(section+secwid-diff)*Tan_Upp+dup
endblock
* ----------------------------------------------------------------------------
* ECAL nice views: dcut ecvo x 1       10 -5  .5 .1
*                  draw emdi 105 0 160  2 13  .2 .1
*                  draw emdi 120 180 150  1 14  .12 .12
* ---------------------------------------------------------------------------
Block ELGD is one Pb-Grass fpd detector
      Material  Air
      Medium    standard
      Attribute ELGD seen=1 colo=1
      shape box dz= zz,
                dy=(ELGG_NEta*ELGG_Width + (ELGG_NEta+1)*ELGG_DGap)/2.0,
                dx=(ELGG_NPhi*ELGG_Width + (ELGG_NPhi+1)*ELGG_DGap)/2.0

      do ii=1, ELGG_NEta
	yy = (ii-ELGG_NEta/2.0-0.5)*(ELGG_Width+ELGG_DGap)
        do jj=1, ELGG_NPhi
	  xx = -(jj-ELGG_NPhi/2.0-0.5)*(ELGG_Width+ELGG_DGap)
            Create and Position ELGT x=xx y=yy 
	enddo
      enddo
EndBlock
* ----------------------------------------------------------------------------
Block ELGT is one PbG Tower
      material Air
      Attribute ELGT seen=1 colo=2
      Shape box	dz=zz,
		dx=ELGG_Width/2.0+ELGG_AlThick,
		dy=ELGG_Width/2.0+ELGG_AlThick 

      Create and Position EWAL z=-zz+ELGG_AlThick+ELGG_depth/2.0
      Create and Position EUMT z=-zz+ELGG_AlThick+ELGG_depth+ELGG_MuMetDz/2.0
      Create and Position ESRB z=-zz+ELGG_AlThick+ELGG_depth+ELGG_SiRubDz/2.0
      Create and Position EPCT z=-zz+ELGG_AlThick+ELGG_depth+ELGG_SiRubDz+ELGG_PhCathDz/2.0
Endblock
* ----------------------------------------------------------------------------
Block EWAL is almunum wrapper
      material Aluminium
      Attribute EWAL seen=1 colo=3
      Shape box	dz=ELGG_Depth/2.0+ELGG_AlThick,
		dx=ELGG_Width/2.0+ELGG_AlThick,
		dy=ELGG_Width/2.0+ELGG_AlThick 
      CALL GSCKOV(%Imed,N,E,ABSCO_Alm,EFFIC_all,RINDEX_Alm)
      Create and Position ELGR
Endblock
* ----------------------------------------------------------------------------
Block ELGR is Lead Grass detector
*     PbG is about 65% Pb 
      Component Pb    A=207.19 Z=82   W=ELGM_PbContent*207.19/(207.19+16)
      Component O     A=16     Z=8    W=ELGM_PbContent*16./(207.19+16)
      Component Si    A=28.08  Z=14   W=(1.0-ELGM_PbContent)*1*28./202.
      Component C     A=12     Z=6    W=(1.0-ELGM_PbContent)*8*12./202.
      Component H     A=1      Z=1    W=(1.0-ELGM_PbContent)*14*1./202.
      Component O     A=16     Z=8    W=(1.0-ELGM_PbContent)*4*16./202.
      Mixture   PbG   Dens=ELGM_Density Radl=ELGM_RadLen
      Attribute ELGR  seen=1    colo=4		! red
      Shape     box dz=ELGG_depth/2 dx=ELGG_Width/2 dy=ELGG_Width/2
      CALL GSCKOV(%Imed,N,E,ABSCO_PbG,EFFIC_All,RINDEX_PbG)
Endblock
* ----------------------------------------------------------------------------
Block ESRB is silicon rubber
      material silicon
      Attribute ESRB seen=1 colo=5
      Shape tube dz=ELGG_SiRubDz/2.0 rmin=0 rmax=ELGG_PhCathR
      CALL GSCKOV(%Imed,N,E,ABSCO_SiRub,EFFIC_All,RINDEX_SiRub)
Endblock
* ----------------------------------------------------------------------------
Block EPCT is Photo Cathode
      material Air
      Attribute LPCT seen=1 colo=6
      Shape tube dz=ELGG_SiRubDz/2.0 rmin=0 rmax=ELGG_PhCathR
      CALL GSCKOV(%Imed,N,E,ABSCO_PhCath,EFFIC_PhCath,RINDEX_PhCath)

      HITS EPCT  x:.005:   y:.01:   z:.005:   cx:10:   cy:10:   cz:10:,
                 Slen:.1:(0,500)    Lptot:18:(-3,2),
                 Tof:16:(0,1.e-6)   LGAM:16:(-2,2),
*                 Step:16:(0,10)     USER:32:(-0.01,0.01)
Endblock
* ----------------------------------------------------------------------------
Block EUMT is mu metal
      material iron
      Attribute LUMT  seen=1    colo=1
      Shape tube dz=ELGG_MuMetDz/2.0 rmin=ELGG_PhCathR rmax=ELGG_MuMetR
      CALL GSCKOV(%Imed,N,E,ABSCO_MuMet,EFFIC_All,RINDEX_MuMet)
Endblock
* ----------------------------------------------------------------------------

End
