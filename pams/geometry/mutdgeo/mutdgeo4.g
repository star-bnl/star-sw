* $Id: mutdgeo4.g,v 1.1 2010/07/30 18:32:29 jwebb Exp $
* $Log: mutdgeo4.g,v $
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
   Content   MUTD,MTMT,MTMF,MTEB,MTTF,MTTT,MTRA,MTBP,MTTP,
             MCHL,MCAL,MCHS,MCAS,MIGS,MMRP

*---- Volume Information...
*---- Basic Configuration...
	Structure MTDG { Version, Config }
*---- Configuration Parameters...
	Structure MTDD { Rmin, Rmax, dZmother,
	                 BackLegR, 
	                 BemcElectBoxdX,  BemcElectBoxdY, 
	                 BemcElectBoxdZ3, BemcElectBoxdZ5,
	                 Rgap,
	                 MtdMotherdX,  MtdMotherdY, 
	                 MtdMotherdZ3, MtdMotherdZ5, 
	                 MtdTraydX, MtdTraydY, MtdTraydZ,
	                 MtdBPlate, MtdTPlate,
	                 MtdIGstackdX, MtdIGstackdY, MtdIGstackdZ }

*---- local variables...
	Integer kTrayConfig,iphi;
	Real	thisphi,thisx,chandim;

*---- Define default values...
*---- Basic Configuration...
	Fill MTDG               ! Muon system configuration parameters
		Version  = 1        ! version number
		Config   = 1        ! =1 single backleg (Run11), =2 full system (Run13)
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
	    MtdMotherdX     =    5.5     ! Tray Mother half-length X (height)
	    MtdMotherdY     =   32.0     ! Tray Mother half-length Y (width)
	    MtdMotherdZ5    =  285.0     ! Tray Mother half-length Z (length long)
	    MtdMotherdZ3    =  195.0     ! Tray Mother half-length Z (length short)
	    MtdTraydX       =    2.4956  ! Tray half-height 
	    MtdTraydY       =   33.765   ! Tray half-width
	    MtdTraydZ       =   54.005   ! Tray half-length
	    MtdBPlate       =    0.3175  ! Tray bottom plate thickness
	    MtdTPlate       =    0.2286  ! Tray top plate thickness
	    MtdIGstackdX	=     0.3    ! WMRPC inner glass stack volume half-length X
	    MtdIGstackdY	=    26.1    ! WMRPC inner glass stack volume half-length Y
	    MtdIGstackdZ	=    43.5    ! WMRPC inner glass stack volume half-length Y
	Endfill
   
	Use    MTDG
	Use    MTDD

	print *,' MuTD: MTDG_Version = ',NINT(MTDG_Version)
	print *,' MuTD: MTDG_Config  = ',NINT(MTDG_Config)
	kTrayConfig = NINT(MTDG_Config)
	if (kTrayConfig==4) then
		print *,' MuTD: You have requested the Run11 Geometry - Single Backleg, 3 trays'
	else if (kTrayConfig==5) then
		print *,' MuTD: You have requested the Run13 Geometry - 27 Backlegs, 117 trays'
	else
		print *,' MuTD: Unknown Tray Configuration. .....WATCH OUT.....'
	end if

*---- Define Gas
	Component H    A=1      Z=1    W=0.90*2*1./102.  + 0. + 0.05*10*1./58.
	Component C    A=12     Z=6    W=0.90*2*12./102. + 0. + 0.05*4*12./58.
	Component F    A=19     Z=9    W=0.90*4*19./102. + 0.05*6*19./146. + 0.
	Component S    A=32     Z=16   W=0.              + 0.05*1*32./146. + 0.
	Mixture   RPCgas  Dens=4.55E-3
	
	create and position MUTD in Cave

* --------------------------------------------------------------------------
Block MUTD is the muon detector mother
	material  Air
	medium    Standard
	Attribute MUTD     seen=0  colo=1
	
	Shape     TUBE  rmin=MTDD_Rmin rmax=MTDD_Rmax dz=MTDD_dZmother
	
	if (kTrayConfig.eq.4) then			! single backleg, 3 trays
		Create    MTMT 
		Position  MTMT AlphaZ=90 z=0 
		
	else if (kTrayConfig.eq.5) then		! full system
		Create    MTMT 
		Create    MTMF 
		do iphi = 1,30
			thisphi	= -30.0 + (iphi-1)*12.0
			if (iphi.le.21)then
				if (iphi.ne.3.and.iphi.ne.19.and.iphi.ne.20) then
					Position  MTMF AlphaZ=thisphi Z=0  
				end if
			else 
					Position  MTMT AlphaZ=thisphi Z=0  
			end if
		end do
	end if

EndBlock
*
* ---------------------------------------------------------------------------
Block MTMF is the backleg mother that encloses five trays
	material  Air
	medium    Standard
	Attribute MTMF seen=1 colo=6
	Shape     TUBS phi1=-6.0 phi2=6.0 
	!
	thisx      = MTDD_BackLegR _
	           + MTDD_BemcElectBoxdX
	Create and Position MTEB x=thisx dz=MTDD_BemcElectBoxdZ5
	!
	thisx      = MTDD_BackLegR _
	           + 2.*(MTDD_BemcElectBoxdX + MTDD_Rgap) _
	           + MTDD_MtdMotherdX
	Create and Position MTTF x=thisx dz=MTDD_MtdMotherdZ5
	!	
EndBlock
* ---------------------------------------------------------------------------
Block MTMT is the backleg mother that encloses three trays
	material  Air
	medium    Standard
	Attribute MTMT seen=1 colo=4
	Shape     Tubs phi1=-6.0 phi2=6.0
	!
	thisx      = MTDD_BackLegR _
	           + MTDD_BemcElectBoxdX
	Create and Position MTEB x=thisx dz=MTDD_BemcElectBoxdZ3
	!	                         
	thisx      = MTDD_BackLegR _
	           + 2.*(MTDD_BemcElectBoxdX + MTDD_Rgap) _
	           + MTDD_MtdMotherdX
	Create and Position MTTT x=thisx dz=MTDD_MtdMotherdZ3
	!
EndBlock
*
* ---------------------------------------------------------------------------
*
Block MTEB is the bemc electronics box
	material  Air
	medium    Standard
	Attribute MTEB seen=1 colo=1
	Shape BOX dx=MTDD_BemcElectBoxdX dy=MTDD_BemcElectBoxdY
EndBlock
*
* ---------------------------------------------------------------------------
*
Block MTTF is the MTD11 5-tray group mother
	material  Air
	medium    Standard
	Attribute MTTF seen=1 colo=3
	Shape BOX dx=MTDD_MtdMotherdX dy=MTDD_MtdMotherdY
	!	
	Create and Position MTRA x=-MTDD_MtdTraydX z=-174
	Create and Position MTRA x=MTDD_MtdTraydX z=-87
	Create and Position MTRA x=-MTDD_MtdTraydX z=0
	Create and Position MTRA x=MTDD_MtdTraydX z=87
	Create and Position MTRA x=-MTDD_MtdTraydX z=174
	!
EndBlock
* ---------------------------------------------------------------------------
Block MTTT is the MTD11 3-tray group mother
	material  Air
	medium    Standard
	Attribute MTTT seen=1 colo=3
	Shape BOX dx=MTDD_MtdMotherdX dy=MTDD_MtdMotherdY
	!
	Create and Position MTRA x=MTDD_MtdTraydX z=-87
	Create and Position MTRA x=-MTDD_MtdTraydX z=0
	Create and Position MTRA x=MTDD_MtdTraydX z=87
	!
EndBlock
*
* ---------------------------------------------------------------------------
*
Block MTRA is an MTD11-style tray
	material  RPCgas
	medium    Standard
	Attribute MTRA seen=1 colo=2
	Shape BOX dx=MTDD_MtdTraydX dy=MTDD_MtdTraydY dz=MTDD_MtdTraydZ
	!
	!---- top plate
	Create MTBP dx=MTDD_MtdBPlate/2.
	Position MTBP x=-MTDD_MtdTraydX+(MTDD_MtdBPlate/2.)
	!
	!---- bottom plate
	Create MTTP dx=MTDD_MtdTPlate/2.
	Position MTTP x=MTDD_MtdTraydX-(MTDD_MtdTPlate/2.)
	!
	!---- long side channels
	chandim = MTDD_MtdTraydX-MTDD_MtdBPlate/2.-MTDD_MtdBPlate/2.
	Create MCHL dx=chandim _
	            dy=chandim _
	            dz=MTDD_MtdTraydZ-2.*chandim
	Position MCHL x=-MTDD_MtdTraydX+MTDD_MtdBPlate+chandim _
	              y=MTDD_MtdTraydY-chandim _
	              z=0
	Position MCHL x=-MTDD_MtdTraydX+MTDD_MtdBPlate+chandim _
	              y=-MTDD_MtdTraydY+chandim _
	              z=0 AlphaZ=180
	!
	!---- short side channels
	Create MCHS dx=chandim _
	            dy=MTDD_MtdTraydY _
	            dz=chandim 
	Position MCHS x=-MTDD_MtdTraydX+MTDD_MtdBPlate+chandim _
	              y=0 _
	              z=MTDD_MtdTraydZ-chandim
	Position MCHS x=-MTDD_MtdTraydX+MTDD_MtdBPlate+chandim _
	              y=0 _
	              z=-MTDD_MtdTraydZ+chandim AlphaY=180
	!
	!---- inner glass stack
	Create MIGS dx=MTDD_MtdIGstackdX _
	            dy=MTDD_MtdIGstackdY _
	            dz=MTDD_MtdIGstackdZ 
	Position MIGS x=0 y=0 z=0
	!
EndBlock
*
* ---------------------------------------------------------------------------
* tray components.......
* ---------------------------------------------------------------------------
*
Block MTBP is the MTD11 bottom plate
	material  Aluminium
	medium    Standard
	Attribute MTBP seen=1 colo=1
	Shape BOX 
EndBlock
Block MTTP is the MTD11 top plate
	material  Aluminium
	medium    Standard
	Attribute MTTP seen=1 colo=1
	Shape BOX 
EndBlock
Block MCHL is the MTD11 architectural channel long side
	material  Aluminium
	medium    Standard
	Attribute MCHL seen=1 colo=1
	Shape BOX 
	Create MCAL dx=chandim-0.3175 _
	            dy=chandim-0.3175/2. _
	            dz=MTDD_MtdTraydZ-2.*chandim
	Position MCAL x=0 y=0.3175/2. z=0
EndBlock
Block MCAL is the air in the MTD11 architectural channel long side
	material  Air
	medium    Standard
	Attribute MCAL seen=1 colo=7
	Shape BOX 
EndBlock
Block MCHS is the MTD11 architectural channel short side
	material  Aluminium
	medium    Standard
	Attribute MCHS seen=1 colo=1
	Shape BOX 
	Create MCAS dx=chandim-0.3175 _
	            dy=MTDD_MtdTraydY _
	            dz=chandim-0.3175/2. 
	Position MCAS x=0 y=0 z=+0.3175/2.
EndBlock
Block MCAS is the air in the MTD11 architectural channel short side
	material  Air
	medium    Standard
	Attribute MCAS seen=1 colo=7
	Shape BOX 
EndBlock
Block MIGS is the inner glass stack
	material  RPCgas
	medium    Standard
	Attribute MCAS seen=1 colo=4
	Shape BOX 
	Create and Position MMRP
EndBlock
*
Block MMRP is a readout strip (just a division of the inner glass stack)
	Attribute MMRP seen=1 colo=4
	Shape Division Iaxis=2 Ndiv=12
	HITS MMRP X:.01:S   Y:.01:   Z:.01:,
	          Ptot:18:(0,100)    cx:10:   cy:10:   cz:10:,
	          Sleng:.1:(0,500)   ToF:16:(0,1.e-6) Step:.01:,      
	          Eloss:16:(0,0.01) 
EndBlock
*
* ---------------------------------------------------------------------------
*


End
