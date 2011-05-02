* $Id: mutdgeo4.g,v 1.3 2011/05/02 20:21:35 jwebb Exp $
* $Log: mutdgeo4.g,v $
* Revision 1.3  2011/05/02 20:21:35  jwebb
* Major revision to the geometry hierarchy, bug fixes, new sensitve volumes.
*
* Revision 1.2  2010/10/31 16:28:21  jwebb
* Latest version of MUTD geometry, was 27 backlegs and 117 trays, now 28
* backlegs and 118 tray.
*
* Revision 1.1  2010/07/30 18:32:29  jwebb
* Added 4th version of the muon telescope detector provided by Bill Llope.
*
*
*
********************************************************************************
Module  MUTDGEO4 is the geometry of the STAR MTD, WMRPC Version
********************************************************************************
+CDE,AGECOM,GCUNIT.
   Author    W.J. Llope
   Created   21 July 2010

*---- List of Volumes....
   Content   MUTD,MTMF,MTLB,MTSB,MMBL,MTRF,MTBP,MTTP,
             MLCH,MSCH,MLAI,MSAI,MIGF,MGAP,MOGL,MPCB,
             MNOM

*---- Basic Configuration...
	Structure MTDG { Version, Config, blconfig(30) }
*---- Configuration Parameters...
	Structure MTDD { Rmin, Rmax, dZmother,
	                 BackLegR, 
	                 BemcElectBoxdX,  BemcElectBoxdY, 
	                 BemcElectBoxdZ3, BemcElectBoxdZ5,
	                 Rgap,
	                 MtdMotherdX,  MtdMotherdY, 
	                 MtdMotherdZ3, MtdMotherdZ5, 
	                 MtdTrayZ1, MtdTrayZ2,
	                 MtdTraydX, MtdTraydY, MtdTraydZ,
	                 MtdChannel,
	                 MtdBPlatedX, MtdTPlatedX,
	                 MtdIGstackdX, MtdIGstackdY, MtdIGstackdZ,
	                 MtdIGngap,MtdIGglassdx,MtdIGgasgapdx,
	                 MtdOGglassdx,MtdOGglassdy,MtdOGglassdz,
	                 MtdPCBdx,MtdPCBdy,MtdPCBdz,
	                 MtdNomexdx,MtdNomexdy,MtdNomexdz}

*---- local variables...
	Integer kTrayConfig,iphi,Ntrayplaced
	Real	thisphi,thisx,thisxb
	Real	innerglassstackhalfthickness
	Integer kNgaps,kphi

*---- Define default values...
*---- Basic Configuration...
	Fill MTDG               ! Muon system configuration parameters
		Version  = 1        ! version number
		Config   = 1        ! =4 single backleg (Run11), =5 full system (Run15)
		blconfig = {5,0,5,5,5,5,5,5,5,5,
		            5,5,5,5,5,5,5,0,5,3,
		            3,3,3,3,3,3,3,3,3,3}   ! Ntrays per backleg configuration...
	Endfill
   
	Fill MTDD                        ! Muon system geometry parameters
		Rmin            = 364.25     ! integration radius lower
		Rmax            = 415.0      ! integration radius upper
		dZmother        = 300.0      ! integration half length in Z
		BackLegR        = 365.0      ! radius of a backleg at phi-center
		BemcElectBoxdX  =   11.035   ! BEMC electronics box half-length X (height)
		BemcElectBoxdY  =   30.65    ! BEMC electronics box half-length Y (width)
		BemcElectBoxdZ5 =  290.0     ! BEMC electronics box half-length Z (length long)
		BemcElectBoxdZ3 =  200.0     ! BEMC electronics box half-length Z (length short)
		Rgap            =    3.81    ! Distance top of Bemc box to underside of lowest tray
	    MtdMotherdX     =    5.4     ! Tray Mother half-length X (height)
	    MtdMotherdY     =   35.0     ! Tray Mother half-length Y (width)
	    MtdMotherdZ5    =  285.0     ! Tray Mother half-length Z (length long)
	    MtdMotherdZ3    =  195.0     ! Tray Mother half-length Z (length short)
	    MtdTrayZ1       =   87.0     ! Tray offset in Z (to center), same as strip length...
	    MtdTrayZ2       =  174.0     ! Tray offset in Z (to center), twice the strip length...
	    MtdTraydX       =    2.49555 ! Tray half-height (thickness=1.75in+0.125in+0.090in)
	    MtdTraydY       =   33.815   ! Tray half-width (width=26.625in) 33.815
	    MtdTraydZ       =   54.610   ! Tray half-length (width=43.00in) 54.610
	    MtdChannel      =    2.2225  ! Architectural Channel (dim=1.75in) 
	    MtdBPlatedX     =    0.15875 ! Tray bottom plate half-thickness (thickness=0.125in)
	    MtdTPlatedX     =    0.1143  ! Tray top plate half-thickness (thickness=0.090in)
	    MtdIGstackdY	=    26.1    ! WMRPC inner glass stack volume half-length Y (<-Phi)
	    MtdIGstackdZ	=    43.5    ! WMRPC inner glass stack volume half-length Z (<-Z)
	    MtdIGngap		=     5.0    ! WMRPC number of gas gaps...
	    MtdIGglassdx	=     0.035  ! WMRPC inner glass half-thickness (thickness=0.700mm)
	    MtdIGgasgapdx	=     0.0125 ! WMRPC gas gap half-thickness (thickness=0.250mm)
	    MtdOGglassdx    =     0.055  ! WMRPC outer glass half-thickness (thickness=1.1mm)
	    MtdOGglassdy    =    27.1    ! WMRPC outer glass half-thickness
	    MtdOGglassdz    =    44.5    ! WMRPC outer glass half-thickness
	    MtdPCBdx        =     0.045  ! WMRPC PCB half-thickness (thickness=0.9mm)
	    MtdPCBdy        =    29.0    ! WMRPC PCB half-thickness
	    MtdPCBdz        =    45.75   ! WMRPC PCB half-thickness
	    MtdNomexdx      =     0.5    ! WMRPC Nomex half-thickness (thickness=1cm)
	    MtdNomexdy      =    28.0    ! WMRPC Nomex half-thickness (PCBdy-1cm)
	    MtdNomexdz      =    44.75   ! WMRPC Nomex half-thickness (PCBdz-1cm)
	Endfill
   
	Use    MTDG
	Use    MTDD

	Prin1 NINT(MTDG_Version); (' MuTD: MTDG_Version       = ',i2)
	Prin1 NINT(MTDG_Config);  (' MuTD: MTDG_Config        = ',i2)
	kTrayConfig = NINT(MTDG_Config)

	if (kTrayConfig==4) then
              Prin0 'MuTD: You have requested the Run11 Geometry - Single Backleg, 3 trays'; (A70);
	else if (kTrayConfig==5) then
	      Prin0 'MuTD: You have requested the Run14 Geometry - 28 Backlegs, 118 trays'; (A70);
	else
              Prin0 ' MuTD: Unknown Tray Configuration  .....WATCH OUT.....'; 
                       (/,70('='),/,70('='),/,A70,/,70('='),/,70('='),/);
	end if

	kNgaps = NINT(MTDD_MtdIGngap)
	Prin1 kNgaps; (' MuTD: Number of gas gaps = ',i2)
	if (kNgaps.lt.5.or.kNgaps.gt.6)then
		prin0 ' MuTD: Unknown Number of Gaps      .....WATCH OUT.....'
                       (/,70('='),/,70('='),/,A70,/,70('='),/,70('='),/);
	end if
	innerglassstackhalfthickness = MTDD_MtdIGngap*MTDD_MtdIGgasgapdx + (MTDD_MtdIGngap-1.)*MTDD_MtdIGglassdx
	Prin1 MTDD_MtdTrayZ1;               (' MuTD: Strip Length       = ',f4.0)
	Prin1 innerglassstackhalfthickness; (' MuTD: IGStack half-thick = ',f7.4)

*---- Define gas...
	Prin1 ; (' MuTD: Defining RPCgas...')
	Component H         A=1     Z=1   W=0.90*2*1./102.  + 0. + 0.05*10*1./58.
	Component C         A=12    Z=6   W=0.90*2*12./102. + 0. + 0.05*4*12./58.
	Component F         A=19    Z=9   W=0.90*4*19./102. + 0.05*6*19./146. + 0.
	Component S         A=32    Z=16  W=0.              + 0.05*1*32./146. + 0.
	Mixture   RPCgas    Dens=4.55E-3
*
*---- Define glass...
	Prin1 ; (' MuTD: Defining Glass...')
	Component Si        A=28    Z=14  W=1.
	Component O         A=16    Z=8   W=2.
	Mixture   Glass     Dens=2.5
*
*---- Define printed circuit boards...
	Prin1 ; (' MuTD: Defining PCB...')
	Component Si        A=28.08 Z=14  W=0.6*1*28./60.   
	Component O         A=16    Z=8   W=0.6*2*16./60.   
	Component C         A=12    Z=6   W=0.4*8*12./174.  
	Component H         A=1     Z=1   W=0.4*14*1./174.
	Component O         A=16    Z=8   W=0.4*4*16./174.
	Mixture   G10       Dens=1.7
*
*---- Define nomex
	Prin1 ; (' MuTD: Defining Nomex...')
	Component Al        A=27    Z=13  W=0.0105
	Component N         A=14    Z=7   W=0.7395
	Component Adhesive  A=9     Z=4.5 W=0.2500
	Mixture   Nomex     Dens=0.73

	Medium    Cave_standard
	
	create and position MUTD in Cave

* --------------------------------------------------------------------------
Block MUTD is the muon detector mother
	Attribute MUTD seen=0 colo=1 serial=kTrayConfig
	material  Air
	Shape     TUBE  rmin=MTDD_Rmin rmax=MTDD_Rmax dz=MTDD_dZmother
	!	
	Ntrayplaced	= 0
	!
	if (kTrayConfig==4) then					! single backleg, 3 trays
		!
		thisphi = 150
		kphi    =   1
		Create and Position  MTMF AlphaZ=thisphi z=0 
		Ntrayplaced = Ntrayplaced + 3
		!
	else if (kTrayConfig==5) then				! full system
		!
		do iphi = 1,30							! 30 backlegs
			kphi = iphi                 		! this is the serialized parameter...
			thisphi	= -18.0 + (iphi-1)*12.0
			Create and Position MTMF AlphaZ=thisphi Z=0 
			Ntrayplaced = Ntrayplaced + NINT(MTDG_blconfig(iphi))
		enddo
		!		
	endif
	Prin0 Ntrayplaced; (' MuTD: You have placed ',i3,' MTD trays......')
	!	
EndBlock
*
* ---------------------------------------------------------------------------
Block MTMF is the backleg mother that encloses five trays
	Attribute MTMF seen=1 colo=6 serial=kphi
	Shape     TUBS phi1=-6.0 phi2=6.0 
	!
	if (NINT(MTDG_blconfig(iphi))>0) then
		!
		! BEMC PMT box................
		if (NINT(MTDG_blconfig(iphi))==5) then
			Create and Position MTLB _
			             x=MTDD_BackLegR+MTDD_BemcElectBoxdX _
			             y=0 z=0
		elseif (NINT(MTDG_blconfig(iphi))==3) then
			Create and Position MTSB _
                         x=MTDD_BackLegR+MTDD_BemcElectBoxdX _
			             y=0 z=0
		endif
		!
	endif
	!
	! MuTD phi mother................
	thisx      = MTDD_BackLegR _
			   + 2.*(MTDD_BemcElectBoxdX + MTDD_Rgap) _
			   + MTDD_MtdMotherdX
	Create and Position MMBL x=thisx dz=MTDD_MtdMotherdZ5
	!
EndBlock
*
* ---------------------------------------------------------------------------
*
Block MMBL is the MTD11 group mother
	Attribute MMBL seen=1 colo=3 serial=kphi
	Shape BOX dx=MTDD_MtdMotherdX dy=MTDD_MtdMotherdY
	!
	if (kTrayConfig.eq.4)then
		Prin1 kphi; (' MuTD: iphi = ',i2,'  -> 3-pack ... Run-11')	
		Create and Position MTRF x=-MTDD_MtdTraydX-0.1 z=-MTDD_MtdTrayZ1
		Create and Position MTRF x=MTDD_MtdTraydX+0.1 z=0.0
		Create and Position MTRF x=-MTDD_MtdTraydX-0.1 z=MTDD_MtdTrayZ1
	elseif (kTrayConfig.eq.5)then
		if (NINT(MTDG_blconfig(kphi))==5)then
			Prin1 kphi; (' MuTD: kphi = ',i2,'  -> 5-pack ')
			Create and Position MTRF x=-MTDD_MtdTraydX-0.1 z=-MTDD_MtdTrayZ2
			Create and Position MTRF x=MTDD_MtdTraydX+0.1 z=-MTDD_MtdTrayZ1
			Create and Position MTRF x=-MTDD_MtdTraydX-0.1 z=0.0
			Create and Position MTRF x=MTDD_MtdTraydX+0.1 z=MTDD_MtdTrayZ1
			Create and Position MTRF x=-MTDD_MtdTraydX-0.1 z=MTDD_MtdTrayZ2
		elseif (NINT(MTDG_blconfig(kphi))==3)then
			Prin1 kphi; (' MuTD: kphi = ',i2,'  -> 3-pack ')
			Create and Position MTRF x=-MTDD_MtdTraydX-0.1 z=-MTDD_MtdTrayZ1
			Create and Position MTRF x=MTDD_MtdTraydX+0.1 z=0.0
			Create and Position MTRF x=-MTDD_MtdTraydX-0.1 z=MTDD_MtdTrayZ1
		elseif (NINT(MTDG_blconfig(kphi))==0)then
			Prin1 kphi; (' MuTD: kphi = ',i2,'  -> No Trays ')
		endif
	endif
	!
EndBlock
*
* ---------------------------------------------------------------------------
*
Block MTRF is an MTD11-style tray 
	material  RPCgas
	Attribute MTRF seen=1 colo=6
	Shape BOX dx=MTDD_MtdTraydX dy=MTDD_MtdTraydY dz=MTDD_MtdTraydZ
	!
	!---- bottom plate
	Create MTBP dx=MTDD_MtdBPlatedX _
                dy=MTDD_MtdTraydY _
                dz=MTDD_MtdTraydZ
	Position MTBP x=-MTDD_MtdTraydX+MTDD_MtdBPlatedX
	!
	!---- top plate
	Create MTTP dx=MTDD_MtdTPlatedX _
                dy=MTDD_MtdTraydY _ 
                dz=MTDD_MtdTraydZ
	Position MTTP x=MTDD_MtdTraydX-MTDD_MtdTPlatedX
	!
	!---- long side Channels
	Create and Position MLCH _
	              x=-MTDD_MtdTraydX+2.*MTDD_MtdBPlatedX+MTDD_MtdChannel _
	              y=MTDD_MtdTraydY-MTDD_MtdChannel _
	              z=0
	Create and Position MLCH _
	              x=-MTDD_MtdTraydX+2.*MTDD_MtdBPlatedX+MTDD_MtdChannel _
	              y=-MTDD_MtdTraydY+MTDD_MtdChannel _
	              z=0 AlphaZ=180
	!
	!---- short side Channels
	Create and Position MSCH _
	              x=-MTDD_MtdTraydX+2.*MTDD_MtdBPlatedX+MTDD_MtdChannel _
	              y=0 _
	              z=MTDD_MtdTraydZ-MTDD_MtdChannel
	Create and Position MSCH _
	              x=-MTDD_MtdTraydX+2.*MTDD_MtdBPlatedX+MTDD_MtdChannel _
	              y=0 _
	              z=-MTDD_MtdTraydZ+MTDD_MtdChannel AlphaY=180
	!
	!---- outer glass
	Create MOGL dx=MTDD_MtdOGglassdx _
	            dy=MTDD_MtdOGglassdy _
	            dz=MTDD_MtdOGglassdz 
	Position MOGL x=-innerglassstackhalfthickness-MTDD_MtdOGglassdx _
	            y=0 z=0
	Position MOGL x=innerglassstackhalfthickness+MTDD_MtdOGglassdx _
	            y=0 z=0
	thisxb = innerglassstackhalfthickness+2.*MTDD_MtdOGglassdx
	!
	!---- PCB
	Create MPCB dx=MTDD_MtdPCBdx _
	            dy=MTDD_MtdPCBdy _
	            dz=MTDD_MtdPCBdz 
	Position MPCB x=-thisxb-MTDD_MtdPCBdx _
	            y=0 z=0
	Position MPCB x=thisxb+MTDD_MtdPCBdx _
	            y=0 z=0
	thisxb = thisxb+2.*MTDD_MtdPCBdx
	!
	!---- Nomex
	Create MNOM dx=MTDD_MtdNomexdx _
	            dy=MTDD_MtdNomexdy _
	            dz=MTDD_MtdNomexdz 
	Position MNOM x=-thisxb-MTDD_MtdNomexdx _
	            y=0 z=0
	Position MNOM x=thisxb+MTDD_MtdNomexdx _
	            y=0 z=0
	thisxb = thisxb+2.*MTDD_MtdNomexdx
	!
	!---- inner glass stack
	Create and Position MIGF x=0 y=0 z=0
	!
EndBlock
*
* ---------------------------------------------------------------------------
*
Block MIGF is the inner glass stack 
	Attribute MIGF seen=1 colo=6
	material  Glass
	Shape BOX dx=innerglassstackhalfthickness _
	            dy=MTDD_MtdIGstackdY _
	            dz=MTDD_MtdIGstackdZ 
	!
	Create MGAP
	if (kNgaps.eq.5) then
		Position MGAP x=4.*MTDD_MtdIGglassdx+4.*MTDD_MtdIGgasgapdx
		Position MGAP x=2.*MTDD_MtdIGglassdx+2.*MTDD_MtdIGgasgapdx
		Position MGAP x=0.0
		Position MGAP x=-2.*MTDD_MtdIGglassdx-2.*MTDD_MtdIGgasgapdx
		Position MGAP x=-4.*MTDD_MtdIGglassdx-4.*MTDD_MtdIGgasgapdx
	elseif (kNgaps.eq.6) then
		Position MGAP x=5.*MTDD_MtdIGglassdx+5.*MTDD_MtdIGgasgapdx
		Position MGAP x=3.*MTDD_MtdIGglassdx+3.*MTDD_MtdIGgasgapdx
		Position MGAP x=1.*MTDD_MtdIGglassdx+1.*MTDD_MtdIGgasgapdx
		Position MGAP x=-1.*MTDD_MtdIGglassdx-1.*MTDD_MtdIGgasgapdx
		Position MGAP x=-3.*MTDD_MtdIGglassdx-3.*MTDD_MtdIGgasgapdx
		Position MGAP x=-5.*MTDD_MtdIGglassdx-5.*MTDD_MtdIGgasgapdx
	endif
	!
EndBlock
*
* ---------------------------------------------------------------------------
*
Block MGAP is a gas gap 
	material  RPCgas
	Medium    sensitive isvol=1
	Attribute MGAP seen=1 colo=7
	Shape BOX dx=MTDD_MtdIGgasgapdx
	HITS MGAP X:.01:S   Y:.01:   Z:.01:,
	          Ptot:18:(0,100)    cx:10:   cy:10:   cz:10:,
	          Sleng:.1:(0,500)   ToF:16:(0,1.e-6) Step:.01:,      
	          Eloss:16:(0,0.01) 
EndBlock
*
* ---------------------------------------------------------------------------
* inactive tray components.......
* ---------------------------------------------------------------------------
*
Block MTLB is the longer bemc electronics box
	Attribute MTLB seen=1 colo=1 
	Shape BOX dx=MTDD_BemcElectBoxdX dy=MTDD_BemcElectBoxdY _
	          dz=MTDD_BemcElectBoxdZ5
EndBlock
*
Block MTSB is the shorter bemc electronics box
	Attribute MTSB seen=1 colo=1 
	Shape BOX dx=MTDD_BemcElectBoxdX dy=MTDD_BemcElectBoxdY _
	          dz=MTDD_BemcElectBoxdZ3

EndBlock
*
Block MTBP is the MTD11 bottom plate
	Attribute MTBP seen=1 colo=1
	material  Aluminium
	Shape BOX 
EndBlock
*
Block MTTP is the MTD11 top plate
	Attribute MTTP seen=1 colo=1
	material  Aluminium
	Shape BOX 
EndBlock
*
Block MLCH is the MTD11 architectural Channel long side
	Attribute MLCH seen=1 colo=1
	material  Aluminium
	Shape BOX dx=MTDD_MtdChannel _
              dy=MTDD_MtdChannel _
              dz=MTDD_MtdTraydZ-2.*MTDD_MtdChannel
	Create and Position MLAI x=0 y=0.3175/2. z=0
EndBlock
Block MLAI is the air in the MTD11 architectural Channel long side
	material  Air
	Attribute MLAI seen=1 colo=7
	Shape BOX dx=MTDD_MtdChannel-0.3175 _
              dy=MTDD_MtdChannel-0.3175/2. _
              dz=MTDD_MtdTraydZ-2.*MTDD_MtdChannel
EndBlock
*
Block MSCH is the MTD11 architectural Channel short side
	Attribute MSCH seen=1 colo=1
	material  Aluminium
	Shape BOX dx=MTDD_MtdChannel _
              dy=MTDD_MtdTraydY _
              dz=MTDD_MtdChannel
	Create and Position MSAI x=0 y=0 z=+0.3175/2.
EndBlock
Block MSAI is the air in the MTD11 architectural MtdChannel short side
	material  Air
	Attribute MSAI seen=1 colo=7
	Shape BOX dx=MTDD_MtdChannel-0.3175 _
              dy=MTDD_MtdTraydY _
              dz=MTDD_MtdChannel-0.3175/2.
EndBlock

Block MOGL is the outer glass layers
	material  Glass
	Attribute MOGL seen=1 colo=4
	Shape BOX 
EndBlock
*
Block MPCB is the printed circuit boards (readout pads)
	Attribute MPCB seen=1 colo=3
	material  G10
	Shape BOX 
EndBlock
*
Block MNOM is the nomex layers
	material  Nomex
	Attribute MNOM seen=1 colo=1
	Shape BOX 
EndBlock
*
* ---------------------------------------------------------------------------
*
End

