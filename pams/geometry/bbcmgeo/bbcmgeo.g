******************************************************************************
Module BBCMGEO is the Beam Beam Counter Modules GEOmetry
Created   15 march 2002
Author    Yiqun Wang
*
*****************************************************************************
*
* Beam pipe has a diameter of 4cm
*
******************************************************************************
+CDE,AGECOM,GCONST,GCUNIT.
*
	Content		BBCM,BBCA,THXM,SHXT,BPOL,CLAD
*
	Structure	BBCG {version,onoff(3),zdis(2)}
*
	Structure	HEXG {type,irad,clad,thick,zoffset,xoffset,yoffset}
*
	Real		actr,srad,lrad,ztotal,x0,y0,theta0,phi0,
			xtrip,ytrip,rtrip,thetrip,rsing,thesing
*
	Integer		I_trip,J_sing
*
* ----------------------------------------------------------------------------
*
Fill	BBCG					! BBC geometry
	Version = 1.0				! Geometry version
	Onoff	= {3,3,3}			! 0 off, 1 west on, 2 east on, 3 both on: for BBC,Small tiles,Large tiles
	zdis	= {374.24,-374.24}		! z-coord from center in STAR (715/2+6*2.54+1=373.8)
EndFILL

*
* ----------------------------------------------------------------------------
*
Fill	HEXG					! hexagon tile geometry
	Type	= 1				! 1 for small hex tile, 2 for large tile
	irad	= 4.174				! inscribing circle radius =9.64/2*sin(60)=4.174
	clad	= 0.1				! cladding thickness
	thick	= 1.0				! thickness of tile
	zoffset	= 1.5			! z-offset from center of BBCW (1), or BBCE (2)
	xoffset	= 0.0			! x-offset center from beam for BBCW (1), or BBCE (2)
	yoffset	= 0.0			! y-offset center from beam for BBCW (1), or BBCE (2)
EndFILL
*
*
Fill	HEXG					! hexagon tile geometry
	Type	= 2				! 1 for small hex tile, 2 for large tile
	irad	= 16.697			! inscribing circle radius (4x that of small one)
	clad	= 0.1				! cladding of tile
	thick	= 1.0				! thickness of tile
	zoffset	= -1.5			! z-offset from center of BBCW (1), or BBCE (2)
	xoffset	= 0.0			! x-offset center from beam for BBCW (1), or BBCE (2)
	yoffset	= 0.0			! y-offset center from beam for BBCW (1), or BBCE (2)
EndFILL
*
*----------------------------------------------------------------------------
*
	Use BBCG
*
      	prin1 bbcg_version 
		('BBCMGEO version ', F4.2)

* use aluminized mylar mixture instead of kapton
          Component C5  A=12    Z=6  W=5
          Component H4  A=1     Z=1  W=4
          Component O2  A=16    Z=8  W=2
          Component Al  A=27    Z=13 W=0.2302
        Mixture  ALKAP  Dens=1.432

	Use HEXG type=1
	srad = hexg_irad*6.0
	ztotal = hexg_thick+2*abs(hexg_zoffset)

	Use HEXG type=2
	lrad = hexg_irad*6.0
	ztotal = ztotal+hexg_thick+2*abs(hexg_zoffset)	! hexg_zoffset is negative for Large (type=2)
*
* Beam Beam Counter Module
*

Create BBCM
* West BBC Module
	if(bbcg_OnOff(1)==1 | bbcg_OnOff(1)==3) then
		Position BBCM in CAVE z=bbcg_zdis(1) x=0 y=0 

	endif
* East BBC Module
	if(bbcg_OnOff(1)==2 | bbcg_OnOff(1)==3) then
		Position BBCM in CAVE z=bbcg_zdis(2) x=0 y=0 AlphaY=180

	endif

	prin1
	 	('BBCMGEO finished')
*
* ----------------------------------------------------------------------------
Block BBCM is one BBC East or West module 
	Material  Air
	Medium    standard
	Attribute BBCM   seen=0 colo=7				!  lightblue
	shape     tube   dz=ztotal/2 rmin=0 rmax=lrad

* Small BBC hex tiles
	Use HEXG type=1
* West
	if(bbcg_OnOff(2)==1 | bbcg_OnOff(2)==3) then
Create and Position BBCA in BBCM z=hexg_zoffset x=hexg_xoffset y=hexg_yoffset
	endif
*
* Large BBC hex tiles
	Use HEXG type=2
* West
	if(bbcg_OnOff(3)==1 | bbcg_OnOff(3)==3) then
Create and Position BBCA in BBCM z=hexg_zoffset x=hexg_xoffset y=hexg_yoffset
	endif
EndBlock
*
* ----------------------------------------------------------------------------
Block BBCA is one BBC Annulus module 
	Material  Air
	Medium    standard
	Attribute BBCA   seen=0 colo=3				!  green
	shape     tube   dz=hexg_thick/2 rmin=hexg_irad rmax=hexg_irad*6.0

	x0=hexg_irad*tan(pi/6.0)
	y0=hexg_irad*3.0
	rtrip = sqrt(x0*x0+y0*y0)
	theta0 = atan(y0/x0)

	Do I_trip =0,5

		phi0 = I_trip*60
		thetrip = theta0+I_trip*pi/3.0
		xtrip = rtrip*cos(thetrip)
		ytrip = rtrip*sin(thetrip)

		Create and Position THXM in BBCA z=0 x=xtrip y=ytrip,
			phix=phi0	phiy=90+phi0	phiz=0,
			thetax=90	thetax=90	thetaz=0  konly='MANY'

	Enddo

EndBlock
*
* ----------------------------------------------------------------------------
Block THXM is on Triple HeXagonal Module
	Material  Air
	Medium    standard
	Attribute THXM  seen=0 colo=2				!  red
	shape     tube   dz=hexg_thick/2 rmin=0 rmax=hexg_irad*2.0/sin(pi/3.0)

	Do J_sing =0,2

		rsing=hexg_irad/sin(pi/3.0)
		thesing=J_sing*pi*2.0/3.0
		Create and Position SHXT in THXM z=0 x=rsing*cos(thesing) y=rsing*sin(thesing)

	enddo
*
EndBlock
*
* ----------------------------------------------------------------------------
Block SHXT is one Single HeXagonal Tile
	Material  Air
	Medium    standard
	Attribute SHXT  seen=1 colo=6				!  violet
	shape	  PGON	Phi1=0   Dphi=360  Nz=2,
                        NpDiv=6,
			zi ={-hexg_thick/2,hexg_thick/2},
			rmn={0,0},
			rmx={hexg_irad,hexg_irad}

	actr = hexg_irad-hexg_clad

	Create and Position CLAD in SHXT z=0 x=0 y=0
	Create and Position BPOL in SHXT z=0 x=0 y=0

EndBlock
*
* ----------------------------------------------------------------------------
Block CLAD is one CLADding of BPOL active region
*	MYLAR, ALKAP or something else
	Material  ALKAP
	Attribute CLAD seen=1 colo=3				!  red
	shape	  PGON	Phi1=0   Dphi=360  Nz=2,
                        NpDiv=6,
			zi ={-hexg_thick/2,hexg_thick/2},
			rmn={actr,actr},
			rmx={hexg_irad,hexg_irad}

EndBlock
*
* ----------------------------------------------------------------------------
Block BPOL is one Bbc POLystyren active scintillator layer 
	Material  POLYSTYREN
	Material  Cpolystyren   Isvol=1
	Attribute BPOL  seen=1 colo=4				!  blue
	shape	  PGON	Phi1=0   Dphi=360  Nz=2,
                        NpDiv=6,
			zi ={-hexg_thick/2,hexg_thick/2},
			rmn={0,0},
			rmx={actr,actr}
			
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
	HITS BPOL  Tof:16:C(0,1.e-6)   Birk:0:C(0,10) 

*                  xx:16:H(-250,250)   yy:16:(-250,250)   zz:16:(-350,350),
*                  px:16:(-100,100)    py:16:(-100,100)   pz:16:(-100,100),
*                  Slen:16:(0,1.e4)    Tof:16:(0,1.e-6)   Step:16:(0,100),
*                  none:16:  
Endblock
* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
*
End


*--------------------------------------------------------------------------
