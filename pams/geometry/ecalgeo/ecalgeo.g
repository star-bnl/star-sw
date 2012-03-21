******************************************************************************
Module ECALGEO is the EM EndCap Calorimeter GEOmetry
Created   26 jan 1996
Author    Rashid Mehdiyev
*
* Version 1.1, W.J. Llope
*               - changed sensitive medium names...
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
* Version 3.0, O. Rogachevsky                                 28.11.99
*               - New proposal for calorimeter SN 0401
*
* Version 4.1, O.Akio                                          3 Jan 01
*               - Include forward pion detectors

* Version 5.0, O. Rogachevsky                                 20.11.01
*               - FPD is eliminated in this version
*               - More closed to proposal description
*                 of calorimeter and SMD structure
*
******************************************************************************
+CDE,AGECOM,GCONST,GCUNIT.
*
      Content    EAGA,EALP,ECAL,ECHC,ECVO,ECGH,EFLP,EHMS,
                 ELED,EMGT,EMOD,EPER,EPSB,ERAD,ERCM,ERSM,
		 ESHM,ESEC,ESCI,ESGH,ESPL,ESSP,EMSS,
		 ETAR,EXGT,EXSG
*
      Structure  EMCG { Version, int Onoff, int fillMode}

      Structure  EMCS { Type,ZOrig,ZEnd,EtaMin,EtaMax,
                        PhiMin,PhiMax,Offset,
                        Nsupsec,Nsector,Nsection,Nslices,
                        Front,AlinCell,Frplast,Bkplast,PbPlate,LamPlate,
												BckPlate,Hub,Rmshift,SMShift,GapPlt,GapCel,
                        GapSMD,SMDcentr,TieRod(2),Bckfrnt,GapHalf,Cover}
*
      Structure  EETR { Type,Etagr,Phigr,Neta,EtaBin(13)}
*
      Structure  ESEC { Isect, FPlmat, Cell, Scint, Nlayer }
*
      Structure  EMXG {Version,Sapex,Sbase,Rin,Rout,F4}
*
      Structure  EXSE {Jsect,Zshift,Sectype(6)}
*
      Integer    I_section,J_section,Ie,is,isec,i_str,Nstr,Type,ii,jj,
                 cut,fsect,lsect,ihalf,filled,myKase
*                       
      Real       center,Plate,Cell,G10,diff,halfi,
                 tan_low,tan_upp,Tanf,RBot,Rtop,Deta,etax,sq2,sq3,
                 dup,dd,d2,d3,rshift,dphi,radiator,orgkeep,endkeep
								 
*
      Real       maxcnt,msecwd,mxgten,curr,Secwid,Section,
                 curcl,EtaTop,EtaBot,slcwid,zslice,Gap,mgt,
                 xleft,xright,yleft,yright,current,
                 rth,len,p,xc,yc,xx,yy,rbotrad,
                 Rdel,dxy,ddn,ddup
                 
    Integer    N
    Parameter (N=12)
* 
    Tanf(etax) = tan(2*atan(exp(-etax)))
* 
* ----------------------------------------------------------------------------
*
* FillMode =1 only 2-5 sectors (in the first half) filled with scintillators 
* FillMode =2 all sectors filled (still only one half of one side)
* FillMode =3 both halves (ie all 12 sectors are filled)

Fill  EMCG                          ! EM EndCAp Calorimeter basic data 
      Version  = 5.0                ! Geometry version 
      OnOff    = 3                  ! Configurations 0-no, 1-west 2-east 3-both
      FillMode = 3                  ! sectors fill mode 

Fill  EMCS                          ! EM Endcap Calorimeter geometry
      Type     = 1                  ! =1 endcap, =2 fpd edcap prototype
      ZOrig    = 268.763            ! calorimeter origin in z
      ZEnd     = 310.007            ! Calorimeter end in z
      EtaMin   = 1.086              ! upper feducial eta cut 
      EtaMax   = 2.0,               ! lower feducial eta cut
      PhiMin   = -90                ! Min phi 
      PhiMax   = 90                 ! Max phi
      Offset   = 0.0                ! offset in x
      Nsupsec  = 6                  ! Number of azimuthal supersectors        
      Nsector  = 30                 ! Number of azimutal sectors (Phi granularity)
      Nslices  = 5                  ! number of phi slices in supersector
      Nsection = 4                  ! Number of readout sections
      Front    = 0.953              ! thickness of the front AL plates
      AlinCell   = 0.02             ! Aluminim plate in cell
      Frplast  = 0.015              ! Front plastic in megatile
      Bkplast  = 0.155              ! Fiber routing guides and back plastic
      Pbplate  = 0.457              ! Lead radiator thickness
      LamPlate  = 0.05              ! Laminated SS plate thickness
      BckPlate = 3.175              ! Back SS plate thickness
      Hub      = 3.81               ! thickness of EndCap hub
      Rmshift  = 2.121              ! radial shift of module
      smshift  = 0.12               ! radial shift of steel support walls
      GapPlt   = 0.3/2              ! HALF of the inter-plate gap in phi
      GapCel   = 0.03/2             ! HALF of the radial inter-cell gap
      GapSMD   = 3.400              ! space for SMD detector
      SMDcentr = 279.542            ! SMD position
      TieRod   = {160.,195}         ! Radial position of tie rods
      Bckfrnt  = 306.832            ! Backplate front Z
      GapHalf  = 0.4                ! 1/2 Gap between halves of endcap wheel
      Cover    = 0.075              ! Cover of wheel half
*      Rmshift  = 2.121              ! radial shift of module
* --------------------------------------------------------------------------
Fill EETR                      ! Eta and Phi grid values
      Type     = 1             ! =1 endcap, =2 fpd
      EtaGr    = 1.0536        ! eta_top/eta_bot tower granularity
      PhiGr    = 0.0981747     ! Phi granularity (radians)
      NEta     = 12            ! Eta granularity
      EtaBin   = {2.0,1.9008,1.8065,1.7168,1.6317,1.5507,1.4738,
                  1.4007,1.3312,1.2651,1.2023,1.1427,1.086}! Eta rapidities
*---------------------------------------------------------------------------
Fill ESEC           ! First EM section
      ISect    = 1                           ! Section number   
      Nlayer   = 1                           ! Number of Sci layers along z
      Cell     = 1.505                       ! Cell full width in z
      Scint    = 0.5                         ! Sci layer thickness
*
Fill ESEC           ! First EM section
      ISect    = 2                           ! Section number   
      Nlayer   = 1                           ! Number of Sci layers along z
      Cell     = 1.505                       ! Cell full width in z
      Scint    = 0.5                         ! Sci layer thickness
*
Fill ESEC           ! Second EM section
      ISect    = 3                           ! Section number
      Nlayer   = 4                           ! Number of Sci layers along z
      Cell     = 1.405                       ! Cell full width in z
      Scint    = 0.4                         ! Sci layer thickness
*
Fill ESEC           ! Third EM section
      ISect    = 4                           ! Section
      Nlayer   = 18                          ! Number of layers along z
      Cell     = 1.405                       ! Cell full width in z
      Scint    = 0.4                         ! Sci layer thickness
*
Fill ESEC           ! 4th EM section
      ISect    = 5                           ! Section
      Nlayer   = 1                           ! Number of  layers along z
      Cell     = 1.505                       ! Cell full width in z
      Scint    = 0.5                         ! Sci layer thickness
*----------------------------------------------------------------------------
Fill EMXG           ! EM Endcap SMD basic data
      Version   = 1                         ! Geometry version
      Sapex     = 0.7                       ! Scintillator strip apex
      Sbase     = 1.0                       ! Scintillator strip base
      Rin = 77.41                           ! inner radius of SMD plane  
      Rout = 213.922                        ! outer radius of SMD plane
      F4 = .15                              ! F4 thickness
*----------------------------------------------------------------------------
Fill EXSE           ! First SMD section
      JSect    = 1                           ! Section number
      Zshift   = -1.215                      ! Section width
      sectype  = {4,1,0,2,1,0}               ! 1-V,2-U,3-cutV,4-cutU    
*
Fill EXSE           ! Second SMD section
      JSect    = 2                           ! Section number   
      Zshift   = 0.                          ! Section width
      sectype  = {0,2,1,0,2,3}               ! 1-V,2-U,3-cutV,4-cutU    
*
Fill EXSE           ! Third SMD section
      JSect    = 3                           ! Section number   
      Zshift   = 1.215                       ! Section width
      sectype  = {1,0,2,1,0,2}               ! 1-V,2-U,3-cutV,4-cutU    
EndFill

*----------------------------------------------------------------------------
*
      Use    EMCG
*
      sq3 = sqrt(3.)
      sq2 = sqrt(2.)

      prin1 emcg_version 
        ('ECALGEO version ', F4.2)

* Endcap
      USE EMCS type=1
      USE EETR type=1
      orgkeep =  emcs_ZOrig
      endkeep =  emcs_ZEnd
      if(emcg_OnOff>0) then
        diff = 0.0
        center  = (emcs_ZOrig+emcs_ZEnd)/2
        Tan_Upp = tanf(emcs_EtaMin)
        Tan_Low = tanf(emcs_EtaMax)
        rth  = sqrt(1. + Tan_Low*Tan_Low)
        rshift  = emcs_Hub * rth
        dup=emcs_Rmshift*Tan_Upp
        dd=emcs_Rmshift*rth
        d2=rshift + dd
        radiator  = emcs_Pbplate + 2*emcs_LamPlate
*       d3=emcs_Rmshift-2*emcs_smshift
        dphi = (emcs_PhiMax-emcs_PhiMin)/emcs_Nsector
        Create ECAL
        if (emcg_OnOff==1 | emcg_OnOff==3) then
             Position ECAL in CAVE z=+center
        endif
        if (emcg_OnOff==2 | emcg_OnOff==3) then
             Position ECAL in CAVE z=-center ThetaZ=180
        endif

        if(section > emcs_Zend) then
          prin0 section,emcs_Zend
          (' ECALGEO error: sum of sections exceeds maximum ',2F12.4)
        endif
        prin1 section
        (' EndCap calorimeter total depth ',F12.4)
      endif
 
      prin1
        ('ECALGEO finished')
*
* ----------------------------------------------------------------------------
Block ECAL is one EMC EndCap wheel
      Material  Air
      Medium    standard
      Attribute ECAL   seen=1 colo=7                            !  lightblue
      shape     CONE   dz=(emcs_Zend-emcs_ZOrig)/2,
                Rmn1=orgkeep*Tan_Low-d2 Rmn2=endkeep*Tan_Low-d2,
                Rmx1=orgkeep*Tan_Upp+dup Rmx2=endkeep*Tan_Upp+dup


      do ihalf=1,2
	 filled=1
	 halfi = -105 + (ihalf-1)*180
         if (ihalf=2 & emcg_FillMode<3) filled = 0	

         Create and Position EAGA  AlphaZ=halfi

      enddo
*
			
EndBlock
* ----------------------------------------------------------------------------
Block EAGA is half of wheel air volume for  the EndCap module
      Attribute EAGA      seen=1    colo=1   serial=filled           ! black
                        
      Material  Air
      shape     CONS   dz=(emcs_Zend-emcs_ZOrig)/2,
                Rmn1=orgkeep*Tan_Low-d2 Rmn2=endkeep*Tan_Low-d2,
                Rmx1=orgkeep*Tan_Upp+dup Rmx2=endkeep*Tan_Upp+dup,
                phi1=emcs_PhiMin phi2=emcs_PhiMax

        if (filled=1) then
          Create and Position EMSS  konly='MANY'
      		curr = orgkeep ; curcl = endkeep
      		Create and position ECGH  AlphaZ=90 konly='ONLY'
				endif


EndBlock

* ----------------------------------------------------------------------------
Block EMSS is steel support of the EndCap module
      Attribute EMSS      seen=1    colo=1              ! black
                        
      Material  Iron
      shape     CONS   dz=(emcs_Zend-emcs_ZOrig)/2,
                Rmn1=orgkeep*Tan_Low-d2 Rmn2=endkeep*Tan_Low-d2,
                Rmx1=orgkeep*Tan_Upp+dup Rmx2=endkeep*Tan_Upp+dup,
                phi1=emcs_PhiMin phi2=emcs_PhiMax

      zslice = emcs_ZOrig
      prin1 zslice
      (' Front Al plane starts at:  ',F12.4)
      slcwid  = emcs_Front
      Create and Position EFLP  z=zslice-center+slcwid/2
      zslice = zslice + slcwid
                        
      prin1 zslice
      (' First calorimeter starts at:  ',F12.4)

      fsect = 1; lsect = 3

			slcwid = emcs_SMDcentr - emcs_GapSMD/2 - zslice
*
       Create and Position ECVO  z=zslice-center+slcwid/2

      slcwid  = emcs_GapSMD
      zslice = emcs_SMDcentr - emcs_GapSMD/2

			prin1 section,zslice
      (' 1st calorimeter ends, SMD starts at:  ',2F10.5)

      Create and Position ESHM  z=zslice-center+slcwid/2
      zslice = zslice + slcwid

      prin1 zslice
      ('  SMD ends at:  ',F10.5)
*
      slcwid = 0
      fsect = 4; lsect = 5
      do I_section =fsect,lsect
        USE ESEC Isect=I_section  
        Slcwid  = slcwid + esec_cell*esec_Nlayer
      enddo

			slcwid = emcs_bckfrnt - zslice

*
      Create and Position ECVO  z = zslice-center+slcwid/2

      zslice = emcs_bckfrnt

			prin1 section,zslice
      (' 2nd calorimeter ends, Back plate starts at:  ',2F10.5)

      slcwid  = emcs_BckPlate
*
         Create and Position ESSP    z=zslice-center+slcwid/2
         zslice = zslice + slcwid
      prin1 zslice
      (' BackPlate ends at:  ',F10.5)

        slcwid = emcs_Zend-emcs_ZOrig
        Create ERCM

				do i_str = 1,2
					do is = 1,5
				  	xx = emcs_phimin + is*30
						yy = xx*degrad
						xc = cos(yy)*emcs_TieRod(i_str)
						yc = sin(yy)*emcs_TieRod(i_str)
        		Position ERCM z=0 x=xc y=yc  
					enddo
				enddo

        rth = orgkeep*Tan_Upp+dup + 2.5/2
				xc = (endkeep - orgkeep)*Tan_Upp
				len = .5*(endkeep + orgkeep)*Tan_Upp + dup + 2.5/2
				yc = emcs_Zend-emcs_ZOrig
				p = atan(xc/yc)/degrad

				Create EPSB
				do is = 1,6
				  xx = -75 + (is-1)*30
					yy = xx*degrad
					xc = cos(yy)*len
					yc = sin(yy)*len
        	Position EPSB x=xc y=yc  AlphaZ=xx
				enddo

EndBlock
* ----------------------------------------------------------------------------
Block ECVO is one of EndCap Volume with megatiles and radiators
      Material  Air
      Attribute ECVO   seen=1 colo=3                            ! green
      shape     CONS   dz=slcwid/2,
                Rmn1=zslice*Tan_Low-dd Rmn2=(zslice+slcwid)*Tan_Low-dd,
                Rmx1=zslice*Tan_Upp+dup Rmx2=(zslice+slcwid)*Tan_Upp+dup

      do J_section = 1,6
			if (1 < J_section < 6 | emcg_FillMode > 1)then
			 filled = 1
			else
			 filled = 0
			endif
			d3 = 75 - (J_section-1)*30
      Create and Position EMOD AlphaZ=d3   Ncopy=J_section
			enddo

*

EndBlock
* ----------------------------------------------------------------------------
Block ESHM  is the SHower Max  section
*
      Material  Air 
      Attribute ESHM   seen=1   colo=4                  !  blue
      Shape     CONS   dz=SlcWid/2,
          rmn1=zslice*Tan_Low-dd,
          rmn2=(zslice+slcwid)*Tan_Low-dd,
          rmx1=(zslice)*Tan_Upp+dup,
          rmx2=(zslice+slcwid)*Tan_Upp+dup,
          phi1=emcs_PhiMin phi2=emcs_PhiMax

      USE EMXG Version=1
      maxcnt = emcs_SMDcentr
          prin1 zslice,section,center
          (' Z start for SMD,section:  ',3F12.4)
*
        do J_section = 1,3
         USE EXSE Jsect=J_section
*
          current = exse_Zshift
          secwid  = emxg_Sapex + 2.*emxg_F4
          section = maxcnt + exse_zshift
          prin1 j_section,current,section,secwid
          (' layer, Z, width :  ',i3,3F12.4)
          rbot=section*Tan_Low
          rtop=section*Tan_Upp
          prin1 j_section,rbot,rtop
          (' layer, rbot,rtop :  ',i3,2F12.4)
          Create and position ESPL z=current
*
        end do

        Create ERSM
				do i_str = 1,2
					do is = 1,5
				  	xx = emcs_phimin + (is)*30
						yy = xx*degrad
						xc = cos(yy)*emcs_TieRod(i_str)
						yc = sin(yy)*emcs_TieRod(i_str)
        		Position ERSM z=0 x=xc y=yc  
					enddo
				enddo

Endblock
* ----------------------------------------------------------------------------
Block ECGH is air Gap between endcap Half wheels
      Material  Air
      Medium    standard
      Attribute ECGH   seen=0 colo=7                            !  lightblue
      shape     TRD1   dz=(emcs_Zend-emcs_ZOrig)/2,
                dy =(emcs_gaphalf+emcs_cover)/2,
                dx1=orgkeep*Tan_Upp+dup,
                dx2=endkeep*Tan_Upp+dup
                

      rth = emcs_GapHalf + emcs_cover
			xx=curr*Tan_Low-d2
			xleft = sqrt(xx*xx - rth*rth)
			yy=curr*Tan_Upp+dup
			xright = sqrt(yy*yy - rth*rth)
			secwid = yy - xx
			xx=curcl*Tan_Low-d2
			yleft = sqrt(xx*xx - rth*rth)
			yy=curcl*Tan_Upp+dup
			yright = sqrt(yy*yy - rth*rth)
			slcwid = yy - xx
      xx=(xleft+xright)/2
      yy=(yleft + yright)/2
			xc = yy - xx
			len = (xx+yy)/2
			yc = curcl - curr
			p = atan(xc/yc)/degrad
      rth = -(emcs_GapHalf + emcs_cover)/2
      Create  ECHC
      Position ECHC  x=len y=rth
      Position ECHC  x=-len y=rth AlphaZ=180

EndBlock
* ----------------------------------------------------------------------------
Block ECHC is steel EndCap Half Cover
      Attribute ECHC      seen=1    colo=1              ! black
                        
      Material  Iron
      shape     TRAP   dz=(curcl-curr)/2,
			          thet=p,
                bl1=secwid/2,
                tl1=secwid/2,
                bl2=slcwid/2,
                tl2=slcwid/2,
                h1=emcs_cover/2 h2=emcs_cover/2,
                phi=0  alp1=0 alp2=0
EndBlock
* ----------------------------------------------------------------------------
Block ESSP  is Stainless Steel  back Plate 
*
      Material  Iron      
      Attribute ESSP   seen=1  colo=6 fill=1    
      shape     CONS   dz=emcs_BckPlate/2,
                Rmn1=zslice*Tan_Low-dd Rmn2=(zslice+slcwid)*Tan_Low-dd,
                Rmx1=zslice*Tan_Upp+dup Rmx2=(zslice+slcwid)*Tan_Upp+dup,
                phi1=emcs_PhiMin phi2=emcs_PhiMax
endblock
* ----------------------------------------------------------------------------
Block EPSB  is Projectile Stainless steel Bar
*
      Material  Iron      
      Attribute EPSB   seen=1  colo=6 fill=1    
      shape     TRAP   dz=(emcs_Zend-emcs_ZOrig)/2,
			          thet=p,
                bl1=2.5/2,
                tl1=2.5/2,
                bl2=2.5/2,
                tl2=2.5/2,
                h1=2.0/2  h2=2.0/2,
                phi=0  alp1=0 alp2=0
endblock
* ----------------------------------------------------------------------------
Block ERCM  is stainless steel tie Rod in CaloriMeter sections
*
      Material  Iron      
      Attribute ERSM   seen=1  colo=6 fill=1    
      shape     TUBE   dz=slcwid/2,
                rmin=0,
                rmax=1.0425  !    nobody knows exactly
endblock
* ----------------------------------------------------------------------------
Block ERSM  is stainless steel tie Rod in Shower Max
*
      Material  Iron      
      Attribute ERSM   seen=1  colo=6 fill=1    
      shape     TUBE   dz=slcwid/2,
                rmin=0,
                rmax=1.0425
endblock
* ----------------------------------------------------------------------------
Block EMOD is one module  of the EM EndCap
      Attribute EMOD      seen=1    colo=3  serial=filled         ! green
      Material  Air
      Shape     CONS   dz=slcwid/2,
           phi1=emcs_PhiMin/emcs_Nsupsec,
           phi2=emcs_PhiMax/emcs_Nsupsec,
           Rmn1=zslice*Tan_Low-dd  Rmn2=(zslice+slcwid)*Tan_Low-dd,
           Rmx1=zslice*Tan_Upp+dup Rmx2=(zslice+slcwid)*Tan_Upp+dup
*
*    Running parameter 'section' contains the position of the current section
*     It should not be modified in daughters, use 'current' variable instead.
*     SecWid is used in all 'CONS' daughters to define dimensions.
*
*
        section = zslice
        curr = zslice + slcwid/2

        Do I_section =fsect,lsect

         USE ESEC Isect=I_section  
*
         Secwid  = esec_cell*esec_Nlayer
         if (I_section = 3 | I_section = 5) then   ! no last radiator 
           Secwid  = Secwid - radiator
         else if (I_section = 4) then         ! add one more radiator 
           Secwid  = Secwid - esec_cell + radiator
         endif  
         Create and position ESEC      z=section-curr+secwid/2
         section = section + secwid
* 
      enddo
endblock
* ----------------------------------------------------------------------------
Block ESEC is a single EM section
      Attribute ESEC   seen=1    colo=1 serial=filled
      Material Air
      Medium standard
*
      Shape     CONS  dz=secwid/2,  
                rmn1=(section-diff)*Tan_Low-dd,
								rmn2=(section+secwid-diff)*Tan_Low-dd,
                rmx1=(section-diff)*Tan_Upp+dup,
								rmx2=(section+secwid-diff)*Tan_Upp+dup
*
			len = -secwid/2
      current = section
			mgt = esec_scint + emcs_AlinCell _
			       + emcs_FrPlast + emcs_BkPlast
      gap = esec_cell - radiator - mgt
      prin2 I_section,section
      (' ESEC:I_section,section',i3,F12.4)

      Do is = 1,esec_Nlayer
			
* define actual  cell thickness:         
        Cell = esec_cell
				plate = radiator
*
        if (is=nint(esec_Nlayer) & (I_section = 3 | I_section = 5)) then  
         Cell = mgt + gap
         Plate=0
        else if (I_section = 4 & is = 1) then    ! radiator only
         Cell = radiator  
        endif
*                
        prin2 I_section,is,len,cell,current
        (' ESEC:I_section,is,len,cell,current  ',2i3,3F12.4)

      	if (I_section = 4 & is = 1) then       ! radiator only
			  	cell = radiator + .14
     			Create and Position    ERAD     z=len + (cell)/2
        	len = len + cell
        	current = current + cell
      	else
          cell = mgt
					if(filled = 1) then
          	Create and Position EMGT	z=len +(gap+cell)/2
            xx = current + (gap+cell)/2
            prin2 I_section,is,xx
            (' MEGA  I_section,is ',2i3,F10.4)						
					endif
        	len = len + cell + gap
        	current = current + cell + gap

      		if (Plate>0) then
				  	cell = radiator
      			Create and Position    ERAD     z=len + cell/2
          	len = len + cell
          	current = current + cell
      		end if
        end if
      end do 
Endblock
* ----------------------------------------------------------------------------
Block EMGT is a megatile EM section
      Attribute EMGT   seen=1  colo=1 
      Material Air
      myKase=2
      if (I_section=1 | I_section=2 | I_section=5) myKase=1
      if (myKase==1) then
        Material Air_EMGT1 isVol=0
      else 
        Material Air_EMGT2 isVol=0
      endif
*
      Shape     CONS  dz=mgt/2,
      rmn1=(current-diff)*Tan_Low-dd,  rmn2=(current+mgt-diff)*Tan_Low-dd,
      rmx1=(current-diff)*Tan_Upp+dup, rmx2=(current+mgt-diff)*Tan_Upp+dup

      if (myKase==1) then
         Call GSTPAR (ag_imed,'CUTGAM',0.00001)
         Call GSTPAR (ag_imed,'CUTELE',0.00001)
      else
         Call GSTPAR (ag_imed,'CUTGAM',0.00008)
         Call GSTPAR (ag_imed,'CUTELE',0.001)
         Call GSTPAR (ag_imed,'BCUTE' ,0.0001)
      end if
*
      Do isec=1,nint(emcs_Nslices)
         Create and Position EPER AlphaZ=(emcs_Nslices/2-isec+0.5)*dphi
      End Do 
Endblock
*---------------------------------------------------------------------------
Block EPER  is a EM subsection period (super layer)
*
      Material  POLYSTYREN
      Attribute EPER   seen=1  colo=1
      Shape     CONS  dz=mgt/2, 
                phi1=emcs_PhiMin/emcs_Nsector,
                phi2=+emcs_PhiMax/emcs_Nsector,
                rmn1=(current-diff)*Tan_Low-dd,
								rmn2=(current+mgt-diff)*Tan_Low-dd,
                rmx1=(current-diff)*Tan_Upp+dup,
								rmx2=(current+mgt-diff)*Tan_Upp+dup
* 
      curcl = current+mgt/2 
      Do ie = 1,nint(eetr_NEta)
        EtaBot  = eetr_EtaBin(ie)
        EtaTop  = eetr_EtaBin(ie+1)

          RBot=(curcl-diff)*Tanf(EtaBot)
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
        xx=tan(pi*emcs_PhiMax/180.0/emcs_Nsector)
        yy=cos(pi*emcs_PhiMax/180.0/emcs_Nsector)

        Create and Position  ETAR    x=(RBot+RTop)/2  ORT=YZX
        prin2 ie,EtaTop,EtaBot,rbot,rtop
        (' EPER : ie,EtaTop,EtaBot,rbot,rtop ',i3,4F12.4)
      enddo
*
EndBlock
*  - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Block ETAR is one CELL of scintillator, fiber and plastic
*
      Attribute ETAR   seen=1  colo=4                           ! blue
*     local z goes along the radius, y is the thickness
      Shape     TRD1   dy=mgt/2   dz=(RTop-RBot)/2,
           dx1=RBot*xx-emcs_GapCel/yy,
           dx2=RTop*xx-emcs_GapCel/yy
*
        Create and Position EALP          y=(-mgt+emcs_AlinCell)/2
      	G10 = esec_scint
      	Create and Position    ESCI       y=(-mgt+G10)/2+emcs_AlinCell _
				                                            +emcs_FrPlast
EndBlock
* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Block ESCI  is the active scintillator (polystyren) layer  
*
  Material  POLYSTYREN
      Material  Cpolystyren   Isvol=1
      Attribute ESCI   seen=1   colo=7   fill=0         ! lightblue
*     local z goes along the radius, y is the thickness
      Shape     TRD1   dy=esec_scint/2,
			                 dz=(RTop-RBot)/2-emcs_GapCel
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
* ----------------------------------------------------------------------------
Block ERAD  is radiator 
*
      Material  Iron
      Attribute ERAD   seen=1  colo=6 fill=1            ! violet
      Shape     CONS  dz=radiator/2, 
                rmn1=(current)*Tan_Low-dd,
								rmn2=(current+cell)*Tan_Low-dd,
                rmx1=(current)*Tan_Upp+dup,
		rmx2=(current+radiator)*Tan_Upp+dup

      		Create and Position    ELED     

endblock
* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Block ELED  is lead absorber Plate 
*
      Material  Lead
      Material  Lead_ELED isVol=0
      Attribute ELED   seen=1   colo=4  fill=1
      Shape     TUBS  dz=emcs_Pbplate/2,  
                rmin=(current)*Tan_Low,
	        rmax=(current+emcs_Pbplate)*Tan_Upp

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
      Attribute EFLP   seen=1  colo=3 fill=1                    ! green
      shape     CONS   dz=emcs_Front/2,
                Rmn1=68.813 Rmn2=68.813,
                Rmx1=(zslice-diff)*Tan_Upp+dup,
								Rmx2=(zslice + slcwid-diff)*Tan_Upp+dup,
                phi1=emcs_PhiMin phi2=emcs_PhiMax


endblock
* ----------------------------------------------------------------------------
Block EALP  is ALuminium  Plate in calorimeter cell
*
      Material  Aluminium
      Material  StrAluminium isvol=0
      Attribute EALP   seen=1  colo=1
      Shape     TRD1   dy=emcs_AlinCell/2  dz=(RTop-RBot)/2
      Call GSTPAR (ag_imed,'CUTGAM',0.00001)
      Call GSTPAR (ag_imed,'CUTELE',0.00001)
      Call GSTPAR (ag_imed,'LOSS',1.)
      Call GSTPAR (ag_imed,'STRA',1.)
endblock
* ----------------------------------------------------------------------------
Block ESPL  is one of the Shower max  PLanes
*
      Material  Air 
      Attribute ESPL   seen=1   colo=3                  !  blue
      Shape     TUBS   dz=SecWid/2,
                rmin=section*Tan_Low-1.526,
                rmax=(section-secwid/2)*Tan_Upp+dup,
                phi1=emcs_PhiMin phi2=emcs_PhiMax

      USE EMXG Version=1
      msecwd = (emxg_Sapex+emxg_F4)/2
			
      do isec=1,6
	 cut=1
  	 d3 = 75 - (isec-1)*30
	 if (exse_sectype(isec) = 0 | (emcg_FillMode=1 & (isec=6 | isec=1))) then
 	    cut = 0
            Create and position EXSG AlphaZ=d3              Ncopy=isec
	 else if(exse_sectype(isec) = 1) then               !   V
            Create and position EXSG AlphaZ=d3              Ncopy=isec
            Create and position EXGT z=msecwd AlphaZ=d3
	 else if(exse_sectype(isec) = 2) then               !   U
            Create and position EXSG AlphaZ=d3 ORT=X-Y-Z   Ncopy=isec
            Create and position EXGT z=-msecwd AlphaZ=d3
	 else if(exse_sectype(isec) = 3) then               !  cut V
	    cut=2
            Create and position EXSG AlphaZ=d3              Ncopy=isec
            Create and position EXGT z=msecwd AlphaZ=d3
	 else if(exse_sectype(isec) = 4) then               !  cut U 
	    cut=2
            Create and position EXSG AlphaZ=d3 ORT=X-Y-Z   Ncopy=isec
            Create and position EXGT z=-msecwd AlphaZ=d3
	 endif
      enddo

Endblock
* ----------------------------------------------------------------------------
Block EXSG  is the Shower max  Gap for scintillator strips
*
      Attribute EXSG   seen=1   colo=7   serial=cut     ! black
      Material  Air   
      Shape     TUBS   dz=SecWid/2,
                rmin=section*Tan_Low-1.526,
                rmax=(section-secwid/2)*Tan_Upp+dup,
                phi1=emcs_PhiMin/emcs_Nsupsec,
                phi2=emcs_PhiMax/emcs_Nsupsec
*
      Rbot = emxg_Rin
      Rtop = emxg_Rout

      if(cut > 0) then
      if(cut = 1) then
      	Rdel = 3.938
       	Nstr = 288
			else
      	Rdel = -.475
       	Nstr = 285
			endif
			rth = .53*rdel        ! .53 --- tentatavily
    	ddn = sq3*1.713 + Rdel  
    	ddup = .5*1.846 + 1.713 
       prin2 Rbot,Rtop,Nstr
       (' EXSG: Rbot,Rtop,Nstr',2F12.4,I5)
			 mgt = emxg_Sbase + .01
    	do i_str = 1,nstr
        p = .5*(i_str-1)*mgt + 41.3655
*
        if (p <= (.5*rbot*sq3 + rth)) then
           dxy = 1.9375*sq2
           xleft = .5*sq2*p*(sq3 + 1.) - dxy
           yleft = .5*sq2*p*(sq3 - 1.) - dxy 
           yright = .5*sq2*(sqrt( rbot*rbot - p*p) - p)
           xright = sq2*p + yright
        else if ((.5*rbot*sq3  + rth) < p <= (.5*Rtop + 1.5)) then 
           prin2 i_str,p
           (' EXSG: 2 - -i_str,p:',i3,F12.4)
           dxy = 1.9375*sq2
           xleft = .5*sq2*p*(sq3 + 1.) - dxy
           yleft = .5*sq2*p*(sq3 - 1.) - dxy 
					 dxy = rdel*sq2/sq3
           yright = .5*sq2*p*(1.- 1./sq3)
           xright = sq2*p - yright - dxy
           yright = -yright - dxy
        else if (p > (.5*rtop +1.5)) then
           prin2 i_str,p
           (' EXSG: 3 - - i_str,p:',i3,F12.4)
           yleft = (sqrt(rtop*rtop - p*p) - p)/sq2
           xleft = sq2*p + yleft
					 dxy = rdel*sq2/sq3
           yright = .5*sq2*p*(1.- 1./sq3)
           xright = sq2*p - yright - dxy
           yright = -yright - dxy
           dxy = 0. 
           if ((.5*sq3*160.- ddn) < p <= (.5*sq3*160.+ ddup) ) then
             prin2 i_str,p
             (' EXSG: 4 - - i_str,p:',i3,F12.4)
						 xc = .5*(sq3*160.+1.846)
						 yc = xc - .5*sq3*1.713
           if (p > yc) then
             dxy = .5*sq2*(2/sq3*rdel + .5*sq3*1.846 +_
								   sqrt(1.713*1.713 - (p-xc)*(p-xc)))
					 else
             dxy = sq2/sq3*(p - .5*sq3* 160. + ddn)
					 endif
           else if ((.5*sq3*195.- ddn) < p <= (.5*sq3*195. + ddup) ) then
             prin2 i_str,p
             (' EXSG: 5 - - i_str,p:',i3,F12.4)
						 xc = .5*(sq3*195.+1.846)
						 yc = xc - .5*sq3*1.713
           if (p > yc) then
             dxy = .5*sq2*(2/sq3*rdel + .5*sq3*1.846 +_
								   sqrt(1.713*1.713 - (p-xc)*(p-xc)))
					 else
             dxy = sq2/sq3*(p - .5*sq3*195. + ddn)
					 endif
           endif
             xright = xright + dxy
             yright = yright + dxy
          endif

          dxy = section*Tan_Upp - Rtop
          xc = .5*(xright+xleft) + dxy
          yc = .5*(yright+yleft)
          xx = .5*sq2*(xleft+yleft)
          yy = .5*sq2*(xright+yright)
          len = xx-yy
           prin2 i_str,p,yy,xx,len,xc,yc
           (' EXSG: i_str,x,y1,y2,len,xc,yc:',i3,6F12.4)
*
       	 Create  EHMS
      	 if (mod(i_str,2) != 0 ) then                     
          	 Position EHMS  x=xc y=yc AlphaZ=-45
      	 else
          	 Position EHMS  x=xc y=yc AlphaZ=-45 ORT=X-Y-Z
      	 endif
        end do
     	 endif


*     dcut exsg z 0 0 10 0.1 0.1
*     dcut exsg y 0 10 -50 0.7 0.7

endblock
* ----------------------------------------------------------------------------
Block EHMS is  sHower Max Strip
*
      Material  POLYSTYREN
      Material  Cpolystyren   Isvol=1
      Attribute EHMS      seen=1    colo=2  serial=cut          ! red
      Shape     TRD1 dx1=0 dx2=emxg_Sbase/2 dy=len/2 dz=emxg_Sapex/2
      Call GSTPAR (ag_imed,'CUTGAM',0.00008)
      Call GSTPAR (ag_imed,'CUTELE',0.001)
      Call GSTPAR (ag_imed,'BCUTE',0.0001)
* define Birks law parameters
      Call GSTPAR (ag_imed,'BIRK1',1.)
      Call GSTPAR (ag_imed,'BIRK2',0.0130)
      Call GSTPAR (ag_imed,'BIRK3',9.6E-6)
*
       HITS EHMS     Birk:0:(0,10)  
*                     xx:16:SH(-250,250)  yy:16:(-250,250)  zz:16:(-350,350),
*                     px:16:(-100,100)    py:16:(-100,100)  pz:16:(-100,100),
*                     Slen:16:(0,1.e4)    Tof:16:(0,1.e-6)  Step:16:(0,100),
*                     none:16:            Eloss:0:(0,10)
* 
Endblock
* ----------------------------------------------------------------------------
Block EXGT  is the G10 layer in the Shower Max  
*
*     G10 is about 60% SiO2 and 40% epoxy
      Component Si    A=28.08  Z=14   W=0.6*1*28./60.
      Component O     A=16     Z=8    W=0.6*2*16./60.
      Component C     A=12     Z=6    W=0.4*8*12./174.
      Component H     A=1      Z=1    W=0.4*14*1./174.
      Component O     A=16     Z=8    W=0.4*4*16./174.
      Mixture   g10   Dens=1.7
      Attribute EXGT   seen=1   colo=7
      Shape     TUBS   dz=emxg_F4/2,
                rmin=(section-diff)*Tan_Low-1.526,
                rmax=(section+msecwd-diff)*Tan_Upp,
                phi1=emcs_PhiMin/emcs_Nsupsec,
                phi2=emcs_PhiMax/emcs_Nsupsec
      Call GSTPAR (ag_imed,'CUTGAM',0.00001)
      Call GSTPAR (ag_imed,'CUTELE',0.00001)
EndBlock
* ----------------------------------------------------------------------------
* ECAL nice views: dcut ecvo x 1       10 -5  .5 .1
*                  draw emdi 105 0 160  2 13  .2 .1
*                  draw emdi 120 180 150  1 14  .12 .12
* ---------------------------------------------------------------------------
end
