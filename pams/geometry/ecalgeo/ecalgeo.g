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
******************************************************************************
+CDE,AGECOM,GCONST,GCUNIT.
*
      Content    ECAL,ECVO,EMDI,EMSS,EAGA,ETOW,ESEC,EPER,EFLP,EXFP,ESHM,
                 ESSP,ETAR,EGTN,ESCI,ELED,EMOD,EXGT,EXSG,EALP,EHMS
*
      Structure  EMCG { Version,ZOrig,ZEnd,EtaMin,EtaMax,Nsupsec,Nsector,
                        Nsection,Nslices,Front,Gten,Plate,PlateS,PlateB,
                        Hub,Rmshift,SMShift,GapPlt,GapCel,GapSMD}
*
      Structure  EETR { Etagr,Phigr,Neta,Nphi,EtaRmn,EtaRmx}
*
      Structure  ESEC { Isect, FPlmat, Cell, Scint, Nlayer }
*
      Structure  EMXG {Version,Zsmax,Sapex,Sbase}
*
      Structure  EXSE {Jsect,Swidth,Aplate}
*
      Integer    I_section,J_section,Ie,is,isec,i_str,Nstr
*			
      Real       Secwid,Section,center,current,Plate,Gap,Cell,G10,
                 tan_low,tan_upp,Tanf,RBot,Rtop,Deta,etax,
                 dup,dd,d2,d3,rshift,
                 maxcnt,msecwd,mxgten,curr,
								 curcl,EtaTop,EtaBot,
								 xleft,xright,yleft,yright,
								 sq2,sq3,rth,tng,len,p,xc,yc

      Tanf(etax) = tan(2*atan(exp(-etax)))
*
* ----------------------------------------------------------------------------
*
Fill  EMCG          ! EM EndCAp Calorimeter basic data 
      Version  = 3         ! Geometry version 
      ZOrig    = 273.5     ! calorimeter origin in z
      ZEnd     = 310.007   ! Calorimeter end in z
      EtaMin   = 1.078     ! upper feducial eta cut 
      EtaMax   = 2.0       ! lower feducial eta cut
      Nsupsec  = 12        ! Number of azimuthal supersectors        
      Nsector  = 60        ! Number of azimutal sectors (Phi granularity)
      Nslices  =  5        ! number of phi slices in supersector
      Nsection =  3        ! Number of readout sections
      Front    = 0.953     ! thickness of the front AL plates
      Gten     = 0.16      ! Fiber routing guides
      Plate    = 0.468     ! Lead radiator thickness
      PlateS   = 0.05      ! Laminated SS plate thickness
      PlateB   = 3.175     ! Back plate thickness SS
      Hub      = 2.5       ! thickness of EndCap hub
      Rmshift  = 1.228     ! radial shift of module
      smshift  = 0.12      ! radial shift of steel support walls
      GapPlt   = 0.3/2     ! HALF of the inter-plate gap in phi
      GapCel   = 0.03/2    ! HALF of the radial inter-cell gap
      GapSMD   = 3.2       ! space for SMD detector
* --------------------------------------------------------------------------
Fill EETR           ! Eta and Phi grid values
      EtaGr    = 1.0536     ! eta_top/eta_bot tower granularity
      PhiGr    = 0.0981747  ! Phi granularity (radians)
      NEta     = 12         ! Eta granularity
      Nphi     = 60         ! Phi granularity
      EtaRmn   = 1.078      ! Min readout rapidity
      EtaRmx   = 2.0        ! Max readout rapidity
*---------------------------------------------------------------------------
Fill ESEC           ! First EM section
      ISect    = 1                           ! Section number	
      Nlayer   = 2                           ! Number of Sci layers along z
      Cell     = 1.328                       ! Cell full width in z
      Scint    = 0.5     							       ! Sci layer thickness
*
Fill ESEC           ! Second EM section
      ISect    = 2                           ! Section number
      Nlayer   = 3                           ! Number of Sci layers along z
      Cell     = 1.228                       ! Cell full width in z
      Scint    = 0.4      									 ! Sci layer thickness
*
Fill ESEC           ! Third EM section
      ISect    = 3                           ! Section
      Nlayer   = 19                          ! Number of Sci layers along z
      Cell     = 1.228                       ! Cell full width in z
      Scint    = 0.4      									 ! Sci layer thickness
*----------------------------------------------------------------------------
Fill EMXG           ! EM Endcap SMD basic data
    	Version   = 1         ! Geometry version
    	Zsmax     = 279.04    ! Shower Max start
    	Sapex     = 0.7       ! Scintillator strip apex
    	Sbase     = 1.0       ! Scintillator strip base
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
Endfill
*
* ----------------------------------------------------------------------------
*
      Use    EMCG
      center  = (emcg_ZOrig+emcg_ZEnd)/2
      Tan_Upp = tanf(emcg_EtaMin)  
      Tan_Low = tanf(emcg_EtaMax)
*
      rshift  = emcg_Hub * sqrt(1. + Tan_Low*Tan_Low)
      dup=emcg_Rmshift*Tan_Upp
			dd=emcg_Rmshift*Tan_Low
      d2=rshift + dd
*			d3=emcg_Rmshift-2*emcg_smshift
*
      Create    ECAL
      Position  ECAL in CAVE    z=+center 
      Position  ECAL in CAVE    z=-center ThetaZ=180  
*      prin0 section
*      (' EndCap calorimeter total depth ',F12.4)
      If (section > emcg_Zend) then
         prin0 section,emcg_Zend
         (' ECALGEO error: sum of sections exceeds maximum ',2F12.4)
      endif
*
* ----------------------------------------------------------------------------
Block ECAL is one EMC EndCap wheel
      Material  Air
      Medium    standard
      Attribute ECAL   seen=1 colo=7				!  lightblue
      shape     CONE   dz=(emcg_Zend-emcg_ZOrig)/2,
                Rmn1=emcg_ZOrig*Tan_Low-d2 Rmn2=emcg_ZEnd*Tan_Low-d2,
                Rmx1=emcg_ZOrig*Tan_Upp+dup Rmx2=emcg_ZEnd*Tan_Upp+dup
*
      Create  and Position ECVO thetax=90 thetay=90 thetaz=0,
                                phix = 75 phiy =-15 phiz=0

EndBlock
* ----------------------------------------------------------------------------
Block ECVO is one EMC EndCap wheel volume
      Material  Air
      Attribute ECVO   seen=0 colo=3				! green
      shape     CONE   dz=(emcg_Zend-emcg_ZOrig)/2,
                Rmn1=emcg_ZOrig*Tan_Low-d2 Rmn2=emcg_ZEnd*Tan_Low-d2,
                Rmx1=emcg_ZOrig*Tan_Upp+dup Rmx2=emcg_ZEnd*Tan_Upp+dup
*
      Create    EMDI 
EndBlock
* ----------------------------------------------------------------------------
Block EMDI is one 1/12 phi-division of the EMC EndCap
      Attribute EMDI      seen=1    colo=2     !  red
*     phi1, phi2 are not really used here but will be inherited by daughters
      Shape     Division  Iaxis=2   Ndiv=nint(emcg_Nsupsec),
                          phi1=-180/emcg_Nsupsec phi2=+180/emcg_Nsupsec
* 
*      Create and Position EMSS x=+emcg_smshift   
      Create and Position EMSS

Endblock
* ----------------------------------------------------------------------------
Block EMSS is steel support of the EndCap module
      Attribute EMSS      seen=1    colo=1			! black
			
      Material  Iron
      Shape     CONS   dz=(emcg_Zend-emcg_ZOrig)/2,
                Rmn1=emcg_ZOrig*Tan_Low-d2   Rmn2=emcg_ZEnd*Tan_Low-d2,
                Rmx1=emcg_ZOrig*Tan_Upp+dup Rmx2=emcg_ZEnd*Tan_Upp+dup,
                phi1=-180/emcg_Nsupsec phi2=+180/emcg_Nsupsec
*
      Create and Position EAGA

EndBlock
* ----------------------------------------------------------------------------
Block EAGA is air gap in sector of the EM EndCap
      Attribute EAGA      seen=1  colo=7				!  lightblue
      Material  Air
      Shape     CONS   dz=(emcg_Zend-emcg_ZOrig)/2,
                Rmn1=emcg_ZOrig*Tan_Low-dd Rmn2=emcg_ZEnd*Tan_Low-dd,
                Rmx1=emcg_ZOrig*Tan_Upp+dup Rmx2=emcg_ZEnd*Tan_Upp+dup

       Create and Position EMOD  

EndBlock
* ----------------------------------------------------------------------------
Block EMOD is one module  of the EM EndCap
      Attribute EMOD      seen=1    colo=3			! green
      Material  Air
      Shape     CONS   dz=(emcg_Zend-emcg_ZOrig)/2,
                Rmn1=emcg_ZOrig*Tan_Low-dd Rmn2=emcg_ZEnd*Tan_Low-dd,
                Rmx1=emcg_ZOrig*Tan_Upp+dup  Rmx2=emcg_ZEnd*Tan_Upp+dup
*
*    Running parameter 'section' contains the position of the current section
*     It should not be modified in daughters, use 'current' variable instead.
*     SecWid is used in all 'CONS' daughters to define dimensions.
*
      section = emcg_ZOrig
      secwid  = emcg_Front
      Create and Position EFLP         z=section-center+secwid/2
      section = section + secwid
*
        Do I_section =1,nint(Emcg_Nsection)

         USE ESEC Isect=I_section  
*
         Secwid  = esec_cell*esec_Nlayer
         if (I_section == 3) then      ! Last section
          Secwid  = Secwid - emcg_plate - 2*emcg_plateS
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
      Attribute ESEC   seen=0  colo=1 
      Material Air
      Material CAir Isvol=0
      Medium standard
*
      Shape     CONS  dz=secwid/2,  
                rmn1=section*Tan_Low-dd rmn2=(section+secwid)*Tan_Low-dd,
                rmx1=section*Tan_Upp+dup rmx2=(section+secwid)*Tan_Upp+dup
      if (I_section==1) then
       Call GSTPAR (ag_imed,'CUTGAM',0.00001)
       Call GSTPAR (ag_imed,'CUTELE',0.00001)
       else
       Call GSTPAR (ag_imed,'CUTGAM',0.00008)
       Call GSTPAR (ag_imed,'CUTELE',0.001)
       Call GSTPAR (ag_imed,'BCUTE',0.0001)
      end if
*
      Do isec=1,nint(emcg_Nslices)
       Create and Position ETOW AlphaZ=-12+(isec-1)*6
      End Do 
Endblock
*---------------------------------------------------------------------------
Block ETOW is an individual 1/60 phi EM tower (section in fact)
      Attribute ETOW   seen=0  colo=1
      Shape     CONS  dz=secwid/2, 
                phi1=-180/emcg_Nsector phi2=+180/emcg_Nsector,
                rmn1=section*Tan_Low-dd rmn2=(section+secwid)*Tan_Low-dd,
                rmx1=section*Tan_Upp+dup rmx2=(section+secwid)*Tan_Upp+dup
      current = section
      Do is = 1,esec_Nlayer

*        define actual Plate and cell thickness:         
         Plate  = emcg_Plate + 2*emcg_PlateS
         Gap = esec_cell - Plate - esec_scint
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
Block EPER  is a EM section period (super layer)
*
      Material  Air 
      Attribute EPER   seen=0  colo=1
      Shape     CONS   dz=Cell/2, 
                phi1=-180/emcg_Nsector phi2=+180/emcg_Nsector, 
                rmn1=current*Tan_Low-dd  rmn2=(current+Cell)*Tan_Low-dd,
                rmx1=current*Tan_Upp+dup  rmx2=(current+Cell)*Tan_Upp+dup
*
* --- Divide module (section) into radial blocks 
* 
      curcl = current+Cell/2
      Deta  = eetr_etagr
      EtaTop  = eetr_EtaRmx
      Do ie = 1,nint(eetr_NEta)
        EtaBot  = EtaTop
        EtaTop  = EtaBot/Deta

        if(ie == 1) then         ! Lower slice
          RBot=current*Tan_Low
        else                     ! 
          RBot=curcl*Tanf(EtaBot)
        endif
*
        if(Plate > 0) then         ! Ordinary Sci layer
         RTop=min(curcl*Tanf(EtaTop), _
				         (current*Tan_Upp+dup))
        else                     ! last Sci layer in section
         RTop=min(curcl*Tanf(EtaTop), _
				         (current*Tan_Upp+dup))
        endif
        check RBot<RTop
*
				Create and Position  ETAR    x=(RBot+RTop)/2  ORT=YZX 
*         prin0 ie,EtaTop,EtaBot,rbot,rtop
*         (' EPER : ie,EtaTop,EtaBot,rbot,rtop ',i3,4F12.4)
      enddo
*
EndBlock
*  - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Block ETAR is one CELL of scintillator, fiber and laminated lead
*
      Attribute ETAR   seen=1  colo=6				! violet
*     local z goes along the radius, y is the thickness
      Shape     TRD1   dy=Cell/2   dz=(RTop-RBot)/2,
           dx1=RBot*tan(pi/emcg_Nsector)-emcg_GapCel/cos(pi/emcg_Nsector),
           dx2=RTop*tan(pi/emcg_Nsector)-emcg_GapCel/cos(pi/emcg_Nsector)
*
      G10 = emcg_Gten
      Create and Position    ESCI        y=(-cell + esec_scint)/2
      Create and Position    EGTN        y=(-cell+G10)/2+esec_scint
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
      Attribute ESCI   seen=1   colo=7   fill=1   	! lightblue
*     local z goes along the radius, y is the thickness
      Shape     TRD1   dy=esec_scint/2  dz=(RTop-RBot)/2-emcg_GapCel
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
      Shape     TRD1   dy=emcg_Plate/2  dz=(RTop-RBot)/2
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
                       rmin= section*Tan_Low,
                       rmax=(section)*Tan_Upp+dup,
                       phi1=-180/emcg_Nsupsec phi2=+180/emcg_Nsupsec
*                rmn1=section*Tan_Low-dd rmn2=(section+secwid)*Tan_Low-dd,
*                rmx1=section*Tan_Upp-dd rmx2=(section+secwid)*Tan_Upp-dd
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
      Attribute ESHM   seen=0   colo=4			!  blue
      Shape     CONS   dz=SecWid/2,
                phi1=-180/emcg_Nsupsec phi2=+180/emcg_Nsupsec,
                rmn1=section*Tan_Low-dd rmn2=(section+secwid)*Tan_Low-dd,
                rmx1=section*Tan_Upp+dup rmx2=(section+secwid)*Tan_Upp+dup
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
*         prin0 rtop
*         (' Z for u:  ',F12.4)
          Create and position EXSG   z=curr-maxcnt+msecwd/2
				 else
				 rtop = curr+msecwd/2
*         prin0 rtop
*         (' Z for v:  ',F12.4)
          Create and position EXSG   z=curr-maxcnt+msecwd/2 AlphaX=180
				 endif
         curr = curr + msecwd
*
         msecwd  = exse_Aplate
         Create and Position EALP      z=curr-maxcnt+msecwd/2
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
      Attribute EXGT   seen=0   colo=7
      Shape     CONS   dz=msecwd/2,
                phi1=-180/emcg_Nsupsec phi2=+180/emcg_Nsupsec,
                rmn1=curr*Tan_Low-dd rmn2=(curr+msecwd)*Tan_Low-dd,
                rmx1=curr*Tan_Upp-dd rmx2=(curr+msecwd)*Tan_Upp-dd    
      Call GSTPAR (ag_imed,'CUTGAM',0.00001)
      Call GSTPAR (ag_imed,'CUTELE',0.00001)
EndBlock
* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Block EXSG  is the Shower max  Gap for scintillator strips
*
      Attribute EXSG   seen=0   colo=7			! black
      Material  Air   Isvol=0
      Shape     CONS   dz=msecwd/2,
                phi1=-180/emcg_Nsupsec phi2=+180/emcg_Nsupsec,
                rmn1=curr*Tan_Low-dd  rmn2=(curr+msecwd)*Tan_Low-dd,
                rmx1=curr*Tan_Upp+dup  rmx2=(curr+msecwd)*Tan_Upp+dup
*
			rth = Pi/emcg_Nsupsec
			tng = tan(rth)
			sq3 = sqrt(3.)
			sq2 = sqrt(2.)
*
			Rbot = (curr+msecwd/2.)*Tan_Low-dd
			Rtop = (curr+msecwd/2.)*Tan_Upp+dup
*
			Nstr = nint((rtop*sq3 - rbot)/emxg_Sbase)
*         prin0 nstr
*				  (' EXSG: nstr',I5)
			if(Nstr > 300) Nstr = 300
*
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
*         prin0 i_str,xleft,yleft,xright,yright,len
*				  (' EXSG: i,xleft,yleft,xright,yright,len',i3,5F12.4)
			 xc = .5*(xright+xleft)
			 yc = .5*(yright+yleft)
*
      if (mod(i_str,2) != 0 ) then			 
       Create and Position EHMS	x=xc y=yc  AlphaZ=45
			else
       Create and Position EHMS	x=xc y=yc AlphaZ=45 AlphaX=180
			endif
        End do
*
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
      Attribute EALP   seen=0  colo=1
      Shape     CONS   dz=msecwd/2,
                phi1=-180/emcg_Nsupsec phi2=+180/emcg_Nsupsec,
                rmn1=curr*Tan_Low-dd rmn2=(curr+msecwd/2)*Tan_Low-dd,
                rmx1=curr*Tan_Upp-dd rmx2=(curr+msecwd/2)*Tan_Upp-dd
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
                phi1=-180/emcg_Nsupsec phi2=+180/emcg_Nsupsec,
                rmn1=section*Tan_Low-dd rmn2=(section+secwid)*Tan_Low-dd,
                rmx1=section*Tan_Upp+dup rmx2=(section+secwid)*Tan_Upp+dup
endblock
* ----------------------------------------------------------------------------
* ECAL nice views: dcut ecvo x 1       10 -5  .5 .1
*                  draw emdi 105 0 160  2 13  .2 .1
*                  draw emdi 120 180 150  1 14  .12 .12
* ---------------------------------------------------------------------------
      End
