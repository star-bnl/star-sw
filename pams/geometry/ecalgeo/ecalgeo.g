******************************************************************************
Module ECALGEO is the EM End-Cap Calorimeter GEOmetry
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
******************************************************************************
+CDE,AGECOM,GCONST,GCUNIT.
*
      Content    ECAL,ECVO,EMDI,EMSS,EAGA,ETOW,ESEC,EPER,EFLP,EXFP,ESHM,
                 ESSP,ETAR,EGTN,ESCI,ELED,EMOD,EXGT,EXSE,EALP,ERIB
*
      Structure  EMCG { Version,ZOrig,ZEnd,EtaMin,EtaMax,Nsupsec,Nsector,
                        Nsection,Nslices,Front,Cell,Gten,Scint,Plate,PlateB,
                        Wall,Rmshift,SMShift,GapPlt,GapCel,GapMax}
*
      Structure  EETR { Etagr,Phigr,Neta,Nphi,EtaRmn,EtaRmx}
*
      Structure  ESEC { Isect, FPlmat, Plateb, Nlayer }
*
      Structure  EMXG {Version,Zsmax,Gtenm,Salup,Riblg,Ribwd,Rstep}
*
      Structure  EXSE { Jsect, Swidth, Glayer, Mat, Med }
*
      Integer    I_section,J_section,Ie,is,isec,ir,nleft,nright
      Real       Secwid,Section,center,current,Plate,Gap,Cell,G10,
                 tan_low,tan_upp,Tanf,RBot,Rtop,Deta,etax,
                 d,dd,d2,d3,rshift,xleft,xright,yleft,yright,
                 maxcnt,msecwd,mxgten,curr,Rleft,Right
      Tanf(etax) = tan(2*atan(exp(-etax)))
*
* ----------------------------------------------------------------------------
*
Fill  EMCG          ! EM EndCAp Calorimeter basic data 
      Version  = 2         ! Geometry version 
      ZOrig    = 271.44    ! calorimeter origin in z
      ZEnd     = 307.0     ! Calorimeter end in z
      EtaMin   = 1.05      ! upper feducial eta cut 
      EtaMax   = 2.0       ! lower feducial eta cut
      Nsupsec  = 12        ! Number of azimuthal supersectors        
      Nsector  = 60        ! Number of azimutal sectors (Phi granularity)
      Nslices  =  5        ! number of phi slices in supersector
      Nsection =  3        ! Number of readout sections
      Front    = 0.8       ! thickness of the front AL plates (CALO and SMD)
      Cell     = 1.08      ! Cell full width in z
      Gten     = 0.16      ! G10 plate width
      Scint    = 0.4       ! Sci layer thickness
      Plate    = 0.48      ! Lead plate thickness
      Wall     = 0.12/2    ! Half thickness of aluminium walls   
      Rmshift  = 0.58      ! radial shift of module
      smshift  = 0.12      ! radial shift of steel support walls
      GapPlt   = 0.3/2     ! HALF of the inter-plate gap in phi
      GapCel   = 0.03/2    ! HALF of the radial inter-cell gap
      GapMax   = 2.8       ! space reserved for SMD detector
* --------------------------------------------------------------------------
Fill EETR           ! Eta and Phi grid values
      EtaGr    = 0.0441     ! Min eta granularity
      PhiGr    = 0.0981747  ! Phi granularity (radians)
      NEta     = 12         ! Eta granularity
      Nphi     = 60         ! Phi granularity
      EtaRmn   = 1.0663     ! Min readout rapidity
      EtaRmx   = 2.0        ! Max readout rapidity
*---------------------------------------------------------------------------
Fill ESEC           ! First EM section
      ISect    = 1                           ! Section number	
      Nlayer   = 6.                          ! Number of Sci layers along z
      PlateB   = 0.8                         ! Back Plate, SS    
*
Fill ESEC           ! Second EM section
      ISect    = 2                           ! Section number
      Nlayer   = 10.                         ! Number of Sci layers along z
      PlateB   = 0.8                         ! Back Plate, SS
*
Fill ESEC           ! Third EM section
      ISect    = 3                           ! Section
      Nlayer   = 9.                          ! Number of Sci layers along z
      PlateB   = 4.0                         ! Back  Plate, SS
*----------------------------------------------------------------------------
Fill EMXG           ! EM Endcap SMD basic data
     Version   = 1         ! Geometry version
     Zsmax     = 279.04    ! Shower Max start
     Gtenm     = 0.2       ! G10 layer width
     Salup     = 0.2       ! Aluminium plate width
     Ribwd     = 0.1       ! Aluminium rib width
     Riblg     = 0.6       ! Aluminium rib length
     Rstep     = 0.7       ! Rib step
*----------------------------------------------------------------------------
Fill EXSE           ! First SMD section
      JSect    = 1                           ! Section number	
      Swidth   = 1.3                         ! Width of SMD section along z
      Glayer   = 0.2                         ! Front Plate, G10  
      Mat      = 1                           ! Section Material  
      Med      = 1                           ! Section Medium
*
Fill EXSE           ! Second SMD section
      JSect    = 2                           ! Section number
      Swidth   = 0.6                         ! Width of SMD section along z
      Glayer   = 0.2                         ! Front Plate, G10
      Mat      = 2                           ! section Material 
      Med      = 2                           ! section Medium
Endfill
*
* ----------------------------------------------------------------------------
*
      Use    EMCG
      center  = (emcg_ZOrig+emcg_ZEnd)/2
      Tan_Upp = tanf(emcg_EtaMin)  
      Tan_Low = tanf(emcg_EtaMax)
      rshift  = emcg_Rmshift
      d=emcg_RmShift               ;  dd=emcg_smshift 
      d2=emcg_RMShift-emcg_smshift ;  d3=emcg_Rmshift-2*emcg_smshift
*
      Create    ECAL
      Position  ECAL in CAVE    z=+center 
      Position  ECAL in CAVE    z=-center ThetaZ=180  
      If (section > emcg_Zend) then
         prin0 section,emcg_Zend
         (' ECALGEO error: sum of sections exceeds maximum ',2F12.4)
      endif
*
* ----------------------------------------------------------------------------
Block ECAL is one EM END-cap wheel
      Material  Vacuum
      Medium    standard
      Attribute ECAL   seen=0 colo=7
      shape     CONE   dz=(emcg_Zend-emcg_ZOrig)/2,
                       Rmn1=emcg_ZOrig*Tan_Low Rmn2=emcg_ZEnd*Tan_Low,
                       Rmx1=emcg_ZOrig*Tan_Upp+d2 Rmx2=emcg_ZEnd*Tan_Upp+d2
*
      Create  and Position ECVO thetax=90 thetay=90 thetaz=0,
                                phix = 75 phiy =-15 phiz=0

EndBlock
* ----------------------------------------------------------------------------
Block ECVO is one EM END-cap wheel volume
      Material  Air
      Attribute ECVO   seen=1 colo=3
      shape     CONE   dz=(emcg_Zend-emcg_ZOrig)/2,
                       Rmn1=emcg_ZOrig*Tan_Low Rmn2=emcg_ZEnd*Tan_Low,
                       Rmx1=emcg_ZOrig*Tan_Upp+d2 Rmx2=emcg_ZEnd*Tan_Upp+d2
*
      Create    EMDI 
EndBlock
* ----------------------------------------------------------------------------
Block EMDI is one 1/12 phi-division of the EM END-CAP
      Attribute EMDI      seen=1    colo=1
*     phi1, phi2 are not really used here but will be inherited by daughters
      Shape     Division  Iaxis=2   Ndiv=nint(emcg_Nsupsec),
                          phi1=-180/emcg_Nsupsec phi2=+180/emcg_Nsupsec
* 
      Create and Position EMSS x=+emcg_smshift   

Endblock
* ----------------------------------------------------------------------------
Block EMSS is steel support of the End-Cap module
      Attribute EMSS      seen=1    colo=2
      Material  Iron
      Shape     CONS   dz=(emcg_Zend-emcg_ZOrig)/2,
                Rmn1=emcg_ZOrig*Tan_Low+d3   Rmn2=emcg_ZEnd*Tan_Low+d3,
                Rmx1=(emcg_ZOrig*Tan_Upp)+d3 Rmx2=emcg_ZEnd*Tan_Upp+d3,
                phi1=-180/emcg_Nsupsec phi2=+180/emcg_Nsupsec
      Create and Position EAGA x=+d2

EndBlock
* ----------------------------------------------------------------------------
Block EAGA is air gap in sector of the EM END-CAP
      Attribute EAGA      seen=1  colo=7
      Material  Air
      Shape     CONS   dz=(emcg_Zend-emcg_ZOrig)/2,
                Rmn1=emcg_ZOrig*Tan_Low-dd Rmn2=emcg_ZEnd*Tan_Low-dd,
                Rmx1=emcg_ZOrig*Tan_Upp-dd Rmx2=emcg_ZEnd*Tan_Upp-dd

       Create and Position EMOD  

EndBlock
* ----------------------------------------------------------------------------
Block EMOD is one module  of the EM END-CAP
      Attribute EMOD      seen=1    colo=3  
      Material  Air
      Shape     CONS   dz=(emcg_Zend-emcg_ZOrig)/2,
                Rmn1=emcg_ZOrig*Tan_Low-dd Rmn2=emcg_ZEnd*Tan_Low-dd,
                Rmx1=emcg_ZOrig*Tan_Upp-dd*2  Rmx2=emcg_ZEnd*Tan_Upp-dd*2
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
       Do I_section =1, nint(Emcg_Nsection)

         USE ESEC Isect=I_section  
*
         Secwid  = esec_Nlayer*emcg_Cell-emcg_Plate
         Create and position ESEC      z=section-center+secwid/2
         section = section + Secwid
* 
         if (I_section == 1) then      ! Shower Max section
*
            secwid  = emcg_Front
            Create and Position EXFP   z=section-center+secwid/2
            section = section + secwid
*
            secwid  = emcg_GapMax
            Create and Position ESHM   z=section-center+secwid/2
            section = section + secwid
*
         endif
*
         secwid  = esec_PlateB
         Create and Position ESSP      z=section-center+secwid/2
         section = section + secwid
      End Do
endblock
* ----------------------------------------------------------------------------
Block ESEC is a sinle EM section
      Attribute ESEC   seen=1  colo=1 
      Material Air
      Material CAir Isvol=0
*      Medium standard
*
      Shape     CONS  dz=secwid/2,  
                rmn1=section*Tan_Low-dd rmn2=(section+secwid)*Tan_Low-dd,
                rmx1=section*Tan_Upp-dd rmx2=(section+secwid)*Tan_Upp-dd
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
                rmx1=section*Tan_Upp-dd rmx2=(section+secwid)*Tan_Upp-dd
      current = section
      Do is = 1,esec_Nlayer

*        define actual Plate and cell thickness:         
         Plate  = emcg_Plate 
         Gap = emcg_Cell-emcg_Plate-emcg_Scint
         if (is==nint(esec_Nlayer))  Plate=0
         Cell   = emcg_Cell-emcg_Plate+Plate  
         Create    EPER 
         Position  EPER  z=-secwid/2+(is-1)*emcg_Cell+Cell/2 
         current = current + emcg_Cell
      End Do
*
endblock
* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Block EPER  is a EM section period (super layer)
*
      Material  Air 
      Attribute EPER   seen=1  colo=1
      Shape     CONS   dz=Cell/2, 
                phi1=-180/emcg_Nsector phi2=+180/emcg_Nsector, 
                rmn1=current*Tan_Low-dd  rmn2=(current+Cell)*Tan_Low-dd,
                rmx1=current*Tan_Upp-dd  rmx2=(current+Cell)*Tan_Upp-dd
*
* --- Divide module (section) into radial blocks 
* 

      Deta  = ((current+Cell/2)*Tan_Upp-(current+Cell/2)*Tan_Low)/eetr_Neta
      Do ie = 1,nint(eetr_NEta)
*                                         
        RBot=max(current*Tanf(eetr_EtaRmx)+(ie-1)*Deta, _
                (current+Gap+emcg_scint/2)*Tan_Low)
        if(is != nint(esec_NLayer)) then         ! Ordinary Sci layer
        RTop=min(current*Tanf(eetr_EtaRmx)+ ie*Deta,(current-Cell/2)*Tan_Upp)
        else                                     ! last Sci layer in section
        RTop=min(current*Tanf(eetr_EtaRmx)+ ie*Deta,(current-Cell) *Tan_Upp)
        end if
        check RBot<RTop
*
        Create and Position  ETAR    x=(RBot+RTop)/2  ORT=YZX 
      End Do
*
EndBlock
*  - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Block ETAR is one CELL of gap, scintillator and lead
*
      Attribute ETAR   seen=1  colo=6 
*     local z goes along the radius, y is the thickness
      Shape     TRD1   dy=Cell/2   dz=(RTop-RBot)/2,
           dx1=RBot*tan(pi/emcg_Nsector)-emcg_GapCel/cos(pi/emcg_Nsector),
           dx2=RTop*tan(pi/emcg_Nsector)-emcg_GapCel/cos(pi/emcg_Nsector)
*
      Gap = emcg_Cell-emcg_Plate-emcg_Scint
      G10 = emcg_Gten
        if(is != nint(esec_NLayer)) then          ! Ordinary Sci layer
      Create and Position    EGTN        y=(-emcg_Cell+0.04+G10)/2
        else
      Create and Position    EGTN        y=(-emcg_Scint)/2
        end if
      Create and Position    ESCI        y=(Gap-Plate)/2
      if (Plate>0) then
      Create and Position    ELED        y=(Gap+emcg_Scint)/2
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
      Attribute EGTN   seen=1   colo=4       
*     local z goes along the radius, y is the thickness
      Shape     TRD1   dy=Emcg_GTen/2  dz=(RTop-RBot)/2
EndBlock
* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Block ESCI  is the active scintillator (polystyren) layer  
*
      Material  POLYSTYREN
      Material  Cpolystyren   Isvol=1
      Attribute ESCI   seen=1   colo=7  fill=1   
*     local z goes along the radius, y is the thickness
      Shape     TRD1   dy=Emcg_Scint/2  dz=(RTop-RBot)/2-emcg_GapCel
      Call GSTPAR (ag_imed,'CUTGAM',0.00008)
      Call GSTPAR (ag_imed,'CUTELE',0.001)
      Call GSTPAR (ag_imed,'BCUTE',0.0001)
* define Birks law parameters
      Call GSTPAR (ag_imed,'BIRK1',1.)
      Call GSTPAR (ag_imed,'BIRK2',0.0130)
      Call GSTPAR (ag_imed,'BIRK3',9.6E-3)
*     
       HITS ESCI   xx:16:H(-250,250)   yy:16:(-250,250)   zz:16:(-350,350),
                   px:16:(-100,100)    py:16:(-100,100)   pz:16:(-100,100),
                   Slen:16:(0,1.e4)    Tof:16:(0,1.e-6)   Step:16:(0,100),
                   none:16:            Birk:0:(0,10)
endblock
* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Block ELED  is lead absorber Plate 
*
      Material  Lead
      Material  CLead Isvol=0
      Attribute ELED   seen=1   colo=6  fill=1
      Shape     TRD1   dy=emcg_Plate/2  dz=(RTop-RBot)/2
      Call GSTPAR (ag_imed,'CUTGAM',0.00008)
      Call GSTPAR (ag_imed,'CUTELE',0.001)
      Call GSTPAR (ag_imed,'BCUTE',0.0001)

endblock
* ----------------------------------------------------------------------------
Block EFLP  is First Aluminium plate 
*
      Material  Aluminium
      Attribute EFLP   seen=1  colo=3 fill=1
      Shape     TUBS   dz=SecWid/2,
                       rmin=(section+secwid)*Tan_Low,
                       rmax=(section)*Tan_Upp-dd,
                       phi1=-180/emcg_Nsupsec phi2=+180/emcg_Nsupsec
*                rmn1=section*Tan_Low-dd rmn2=(section+secwid)*Tan_Low-dd,
*                rmx1=section*Tan_Upp-dd rmx2=(section+secwid)*Tan_Upp-dd
endblock
* ----------------------------------------------------------------------------
Block EXFP  is First  plate in SMAX
*
      Material  Iron
      Attribute EXFP   seen=1  colo=6 fill=1
      Shape     TUBS   dz=SecWid/2,
                       rmin=(section+secwid)*Tan_Low,
                       rmax=(section)*Tan_Upp-dd,
                       phi1=-180/emcg_Nsupsec phi2=+180/emcg_Nsupsec
*                rmn1=section*Tan_Low-dd rmn2=(section+secwid)*Tan_Low-dd,
*                rmx1=section*Tan_Upp-dd rmx2=(section+secwid)*Tan_Upp-dd
endblock
* ----------------------------------------------------------------------------
Block ESHM  is the Shower Max  section
*
      Material  Air 
      Material  SmAir Isvol=0
      Attribute ESHM   seen=1   colo=4
      Shape     CONS   dz=SecWid/2,
                phi1=-180/emcg_Nsupsec phi2=+180/emcg_Nsupsec,
                rmn1=section*Tan_Low-dd rmn2=(section+secwid)*Tan_Low,
                rmx1=section*Tan_Upp-dd rmx2=(section+secwid)*Tan_Upp-dd
      Call GSTPAR (ag_imed,'CUTGAM',0.00001)
      Call GSTPAR (ag_imed,'CUTELE',0.00001)
      Call GSTPAR (ag_imed,'LOSS',1.)
      Call GSTPAR (ag_imed,'STRA',1.)
*
      USE EMXG Version=1
      curr =  section
      maxcnt = emxg_Zsmax+emcg_Gapmax/2 
      msecwd = emxg_Gtenm ; mxgten = emxg_Gtenm
      Create and Position EXGT         z=curr-maxcnt+msecwd/2
      curr = curr + msecwd+0.1
*
        Do J_section = 1,2
*       
         USE EXSE Jsect=J_section
        
         msecwd  = exse_Swidth
**         if(J_section==2) curr=curr+0.1
         Create and position EXSE      z=curr-maxcnt+msecwd/2
         curr = curr + msecwd ; msecwd=exse_glayer
         if (J_section==2) then
         msecwd=emxg_salup
         Create and Position EALP      z=curr-maxcnt+emxg_salup/2
         curr = curr + exse_Glayer
         end if 
         Create and Position EXGT      z=curr-maxcnt+msecwd/2
         curr = curr + exse_Glayer
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
      Shape     CONS   dz=mxgten/2,
                phi1=-180/emcg_Nsupsec phi2=+180/emcg_Nsupsec,
                rmn1=curr*Tan_Low-dd rmn2=(curr+mxgten)*Tan_Low-dd,
                rmx1=curr*Tan_Upp-dd rmx2=(curr+mxgten)*Tan_Upp-dd    
      Call GSTPAR (ag_imed,'CUTGAM',0.00001)
      Call GSTPAR (ag_imed,'CUTELE',0.00001)
EndBlock
* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Block EXSE  is the Shower Max  section
*     SMD default gas P10: Ar/methane 9:1 by weight
      Component Ar    A=40  Z=18 W=.9
      Component C     A=12  Z=6  W=.1*12./16.
      Component H     A=1   Z=1  W=.1* 4./16.
      Mixture   P10   Dens=1.78e-3    
      Material  EXSE_mat {Air, P10}
      Material  EXSE_med {SmAir isvol=0, sensitive_gas isvol=1 stemax=5}
      Attribute EXSE   seen=1   colo=7
      Shape     CONS   dz=msecwd/2,
                phi1=-180/emcg_Nsupsec phi2=+180/emcg_Nsupsec,
                rmn1=curr*Tan_Low-dd  rmn2=(curr+msecwd)*Tan_Low-dd,
                rmx1=curr*Tan_Upp-dd  rmx2=(curr+msecwd)*Tan_Upp-dd
      Call GSTPAR (ag_imed,'CUTGAM',0.00001)
      Call GSTPAR (ag_imed,'CUTELE',0.00001)
      Call GSTPAR (ag_imed,'LOSS',1.)
      Call GSTPAR (ag_imed,'STRA',1.)

      if (J_section==2) then
*
      xleft=curr*Tan_Low ; xright=curr*Tan_upp
      yleft=xleft*sin(pi/emcg_nsupsec) 
      yright=xright*sin(pi/emcg_nsupsec)
      nleft=(yleft/emxg_Rstep) ; nright=(yright/emxg_Rstep)
*
       do ir=0,nright
        Right=sqrt(xright**2-((ir)*emxg_Rstep)**2)
         if(  (ir-1) < nleft ) then
           Rleft=sqrt(xleft**2-((ir)*emxg_Rstep)**2)
         else
           Rleft=ir*emxg_Rstep/tan(pi/emcg_nsupsec)
         end if

       Create   ERIB
       if(Ir==1) then
       Position ERIB  x=+(Right+Rleft)/2  
       end if 
       Position ERIB  x=+(Right+Rleft)/2  y=+(emxg_Rstep)*(ir) 
       Position ERIB  x=+(Right+Rleft)/2  y=-(emxg_Rstep)*(ir) 
       end do
       HITS     EXSE xx:16:SH(-250,250)  yy:16:(-250,250)  zz:16:(-350,350),
                     px:16:(-100,100)    py:16:(-100,100)  pz:16:(-100,100),
                     Slen:16:(0,1.e4)    Tof:16:(0,1.e-6)  Step:16:(0,100),
                     none:16:            Eloss:0:(0,10)
*
      end if
      
endblock
* ----------------------------------------------------------------------------
Block EALP  is aluminium  Plate in Shower Max 
*
      Material  Aluminium
      Material  CAluminium   Isvol=0
      Attribute EALP   seen=1  colo=1
      Shape     CONS   dz=emxg_salup/2,
                phi1=-180/emcg_Nsupsec phi2=+180/emcg_Nsupsec,
                rmn1=curr*Tan_Low-dd rmn2=(curr+emxg_salup/2)*Tan_Low-dd,
                rmx1=curr*Tan_Upp-dd rmx2=(curr+emxg_salup/2)*Tan_Upp-dd
      Call GSTPAR (ag_imed,'CUTGAM',0.00001)
      Call GSTPAR (ag_imed,'CUTELE',0.00001)
      Call GSTPAR (ag_imed,'LOSS',1.)
      Call GSTPAR (ag_imed,'STRA',1.)
endblock
* ----------------------------------------------------------------------------
Block ERIB  is aluminium  rib in Shower Max 
*
      Material  Aluminium
      Material  CAluminium   Isvol=0
      Attribute ERIB   seen=1  colo=1
      Shape     BOX   dz=emxg_riblg/2   dx=(Right-Rleft)/2,
                      dy=emxg_ribwd/2
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
      Shape     CONS   dz=Esec_PlateB/2,
                phi1=-180/emcg_Nsupsec phi2=+180/emcg_Nsupsec,
                rmn1=section*Tan_Low-dd rmn2=(section+secwid)*Tan_Low-dd,
                rmx1=section*Tan_Upp-dd rmx2=(section+secwid)*Tan_Upp-dd
endblock
* ----------------------------------------------------------------------------
* ECAL nice views: dcut ecvo x 1       10 -5  .5 .1
*                  draw emdi 105 0 160  2 13  .2 .1
*                  draw emdi 120 180 150  1 14  .12 .12
* ---------------------------------------------------------------------------
      End
