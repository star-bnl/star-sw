*****************************************************************
 Module  PHMDGEO  is the geometry of photon multiplicity detector
*****************************************************************
* modified: 18th July 2003: Tapan, Dipak
* Two modifications:
* 1. CUTGAM and CUTELE values are introduced in the
*    sensitive medium
* 2. Proper mixture of Ar+CO2(70:30) has been introduced.
*
* modified: 14th June 2002: Viyogi,Tapan,Bedanga and Dipak
*
* 13th Aug. 2001 : Viyogi,Tapan,Bedanga and Dipak
*
* The New PMD has been implemented by deviding the
* volume PHMD into 3 Sector volumes (Two PHMS volumes  and 
* one PHMT volume). The 3 sectors have been placed by rotating 
* properly. Sector volume PHMS  has 5 supermodules 
* and sector volume PHMT has 7 supermodules of varying sizes. 
*
*1st July 2001 : Bedanga and Pavel
*
* In each module we place the cells stripwise (ASTR) and in
* each strip we create and position pusedo-cell (PSTR)
*
*June 2001 : Viyogi,Dipak,Tapan, Murthy
*
* The geometry for PMD, with all materials properly
* taken care of. PCB, Base plate(G10), Lead, steel support.
* The hole in PMD automatically formed by positioning of modules.
* The new geometry design has been finalised
* 
+include,AGECOM,GCUNIT,GCONST.
    Author    Subhasis, Viyogi, Bedanga,Tapan and Dipak
    Created   03-july-2001
    Content   PHMD,PHMS,PHMT,PHSR,PMDA,AIRA,PHCA,PPBA,
              PFEA,PCBA,BASA,ASTR,PSTR,PDCU,PDGS
              
              
    structure PMDG {version,m_max,m_min,zdist(2),DPMDx,DPMDy,DPMDz, 
                    PARGCz,PARSCz,PARFEz,PARPBz,
                    cell_radius,cell_depth,cell_wall,boundary,
                    th_base,th_air,th_pcb,th_lead,th_steel,
                    Int nx(5),Int ny(5),Int mx(7),Int my(7),
                    hexd2(10),hexd1(10),dpara(6)}

    Integer         J,Itype,Ncellx,Ncelly,N,Mcellx
    Real xb,yb,zb,xlen,xlen0,ylen,ylen0,phi,phideg,xpos,ypos,xsize,ysize
    Real sm_thick,zz,root32,root34,SizeN,xlen1,xlen2,xsize1,xlen3,ylen3
    Real phideg1,phideg2,phideg3,phi1,phi2,phi3,xpos1,ypos1,ylen1,ylen2
    Real zlen,zlen0,zlen1,sm_thick_a,xx,zlen2
    SizeN (N) = ((N + 1./3.)*pmdg_CELL_RADIUS)*2 + pmdg_boundary*2.*2./sqrt(3.)

*
    Fill PMDG                   ! PMD geometry
      version = 1               ! geometry version
      m_max   = 135.0           ! Mother volume max radius
      m_min   = 22.0            ! Mother volume min radius
      zdist   = {-535.0,-550.0} ! PMD placed at 5.5 metre from the interaction point
      DPMDx   = 270    !  (X-halfwidth of the PMD box,was 190 earlier)
      DPMDy   = 270    ! Y-halfwidth of the  PMD box.
      DPMDz   = 10.    ! total z half-width of the box.
      PARGCz  = 0.40   ! half thickness of gas (CPV sensitive)
      PARSCz  = 0.40   ! (8 mm thick gas)
      PARFEz  = 0.25   ! (iron frame)
      PARPBz  = 0.75   ! (3 X0 of lead converter)
      nx      = { 48,72,72,48,48 } !  x-dimensions of modules
      ny      = { 24,48,48,72,48 } !  y-dimensions of modules
      mx      = { 24,48,72,72,24,48,48 } ! x-dimensions of modules
      my      = { 24,24,24,48,48,72,48 } ! y-dimensions of modules
      hexd2   = {0.,360.,6,2,-0.4,0.,0.51,0.4,0.,0.51} ! inner hex
      hexd1   = {0.,360.,6,2,-0.4,0.,0.53,0.4,0.,0.53} ! outer hex
      dpara   = {38.29,33.16,0.4,30.,0.,0.} ! supermodule
      CELL_RADIUS = 0.5282   ! Radius of a cell
      CELL_DEPTH  = 0.8    ! Gas depth
      CELL_WALL   = 0.2    ! Cell wall thickness
      BOUNDARY    = 0.8    ! Boundary
      th_base     = 0.3    ! Thickness of the base plate
      th_air      = 0.1    ! Air gap
      th_pcb      = 0.16   ! Thickness of the PCB
      th_lead     = 1.5    ! Thickness of the Lead
      th_steel    = 0.5    ! Thickness of the steel support
    endfill
    
      Use  PMDG
      root32=sqrt(3.0)/2.0
      root34=root32/2.0

      xlen3=(SizeN(72)+SizeN(48))/4.0
      ylen3=-(SizeN(72)+SizeN(48))*root34      
      zlen2=(SizeN(72)+SizeN(48)+4.5*pmdg_th_air)/4.
      zlen1=-(SizeN(72)+SizeN(48)+(2.0*pmdg_boundary+3.75*pmdg_th_air)/root32)*root34 

      sm_thick_a = (pmdg_th_base + 4.0*pmdg_th_air + 3.0*pmdg_th_pcb  + pmdg_CELL_depth)

      sm_thick   = 2.0*sm_thick_a  + pmdg_th_lead + pmdg_th_steel

* Postion according to the current version
      Create and Position PHMD in CAVE z=pmdg_ZDIST(PMDG_version)
*
* -----------------------------------------------------------------------
* Position the sectors inside PHMD
Block PHMD the PMD box volume and fill with air 
      Material  Air
      Medium    STandard
      Attribute PHMD   seen=1    colo=2
      Shape     TUBE   Rmin=pmdg_m_min  Rmax=pmdg_m_max  Dz=sm_thick/2.
      phideg1=90.
      phideg2=210.
      phideg3=330.
      phi1=phideg1*degrad
      phi2=phideg2*degrad
      phi3=phideg3*degrad

      create and position   PHMS x=xlen3*cos(phi1)-ylen3*sin(phi1)+1.5*pmdg_th_air,
                                 y=xlen3*sin(phi1)+ylen3*cos(phi1),
                                 z=0 Alphaz=phideg1 Ncopy=1 

      create and position   PHMS x=xlen3*cos(phi2)-ylen3*sin(phi2)-1.5*pmdg_th_air,
                                 y=xlen3*sin(phi2)+ylen3*cos(phi2),
                                 z=0 Alphaz=phideg2 Ncopy=2

      create and position   PHMT x=zlen2*cos(phi3)-zlen1*sin(phi3),
                                 y=zlen2*sin(phi3)+zlen1*cos(phi3),
                                 z=0 Alphaz=phideg3 Ncopy=3

endblock    
* -----------------------------------------------------------------------
Block PHMS the PMD sector volume - 1/3rd of PHMD

      Attribute PHMS     seen=1  colo=2
       Material  Air
       Medium    STandard
       Shape     para dx = xlen3*2. dy = -ylen3 dz=sm_thick/2.,
                      Alph=30. thet=0 phi=0  
                          
* --- Place the CPV in PMD with front edge 550cm from the target ---
       phideg =0
      ypos   = (SizeN(72)+SizeN(48))*root34 
      xlen2 = -(SizeN(72)+SizeN(48))/4.
      xsize1 = (SizeN(72)+SizeN(48))/2.
* Modules numbered in an anti-clockwise manner

      do Itype = 1,5
        xsize = SizeN(pmdg_Nx(Itype))
        ysize = SizeN(pmdg_Ny(Itype))-pmdg_boundary/root32
	if(Itype>2)ysize= SizeN(pmdg_Ny(Itype))
        xlen0  = xsize/2
	ylen0  = (ysize)*root34 
        ylen   = (ysize-pmdg_boundary/root32)*root34
        xpos = xlen2 -xsize1 +xsize/2.+ SizeN(pmdg_Ny(Itype))/4.
        if(Itype>2)ylen = (ysize-pmdg_boundary/root34)*root34
        ylen1=0.
        ylen2=0.
	xlen1=1.
	if(Itype==2)xlen1=-1.
	if(Itype>2)xlen1=0.
        zlen=1.
        if(Itype==2)zlen=-1.
        if(Itype>2)zlen=0.
        zb = 0
        NcellX = pmdg_Nx(Itype)
        NcellY = pmdg_Ny(Itype)
        if (Itype==1)xpos=xpos + xsize + SizeN(pmdg_Ny(Itype))- 11.*pmdg_boundary/(4.*root32)-_
                                                   pmdg_CELL_RADIUS*2./3.
        if (Itype==2)xpos=xpos + SizeN(pmdg_Ny(Itype))/2.-pmdg_boundary/(4.*root32)
        if (Itype==3)xpos = xpos 
        if (Itype==4)xpos=xpos + 2.*xsize1-xsize/2.
        if (Itype==5)xpos = xpos +2.*xsize1 - xsize 
        ypos = ypos-ylen0
        Create PHSR 
        Position PHSR x=xpos y=ypos z=0., 
                      AlphaZ=phideg   

        ypos = ypos-ylen0
        if (Itype==3) ypos=(SizeN(72)+SizeN(48))*root34 

      enddo
endblock
* ----------------------------------------------------------------------- 
Block PHMT the PMD 3rd sector volume - 1/3rd of PHMD

      Attribute PHMT     seen=1  colo=2
       Material  Air
       Medium    STandard
       Shape     PARA dx=zlen2*2. dy=-zlen1 dz=sm_thick/2.,
                      Alph=30. thet=0 phi=0  
                          
* --- Place the CPV in PMD with front edge 550cm from the target ---
       phideg =0
      ypos1   = (SizeN(72)+SizeN(48)+2.*pmdg_boundary/root32+.375)*root34 
      xlen2 = -(SizeN(72)+SizeN(48)+3.75*pmdg_th_air)/4.
      xsize1 = (SizeN(72)+SizeN(48)+4.5*pmdg_th_air)/2.
* Modules numbered in an anti-clockwise manner

      do Itype = 1,7
        xsize = SizeN(pmdg_Mx(Itype))
	ysize = SizeN(pmdg_My(Itype))
        xlen0  = xsize/2
	ylen0  = (ysize)*root34 
        zlen0=(ysize)*root34 
        if(Itype==2)zlen0=(ysize-pmdg_boundary/root32)*root34+3.75*pmdg_th_air
        if(Itype==3)zlen0=(ysize-pmdg_boundary/root32)*root34-3.75*pmdg_th_air 
        if(Itype==4)zlen0=(ysize)*root34+3.75*pmdg_th_air 
        if(Itype==5)zlen0=(ysize+pmdg_boundary/root32)*root34 
        xpos1 = xlen2 -xsize1 + xsize/2.+ ysize/4.
        ylen = (ysize-2*pmdg_boundary)*root34

	ylen1 = 0.
	if(Itype==1)ylen1=1.
        if(Itype==2)ylen1=-1.
        if(Itype==3)ylen1=-1.
        if(Itype==5)ylen1=2.
	if(Itype==6)ylen1=1.

	ylen2 = 0.
        if(Itype==2)ylen2=1.
        if(Itype==3)ylen2=1.
        if(Itype==5)ylen2=-1.

        xlen1=0.
	if(Itype==1)xlen1=-2.
	if(Itype==2)xlen1=3.
	if(Itype==3)xlen1=1.
	if(Itype==5)xlen1=-1.
	if(Itype==6)xlen1=2.


	zlen=0.
	if(Itype==2)zlen=0.75
        if(Itype==3)zlen=-0.75
        if(Itype==5)zlen=-1.

        zb = 0
        NcellX = pmdg_Mx(Itype)
        NcellY = pmdg_My(Itype)

        if (Itype==1)xpos1=xpos1 + SizeN(pmdg_Mx(Itype+1))/2.+2.*ysize-_
                                          2* pmdg_boundary/root32-pmdg_boundary/3.

        if (Itype==2)xpos1=xpos1 + xsize/2.+ ysize/2.-pmdg_boundary*root32-pmdg_boundary-_
                                                              2.25*pmdg_th_air

        if (Itype==3)xpos1=xpos1 + SizeN(pmdg_Mx(Itype-1))/2 - pmdg_boundary-2.25*pmdg_th_air

        if (Itype==4)xpos1 = xpos1 -pmdg_boundary*root32


        if (Itype==5)xpos1 = xpos1+2.*xsize1-xsize+ SizeN(pmdg_Mx(Itype-1))/2._
                                            -ysize +3*pmdg_boundary*root32+2.25*pmdg_th_air
        if (Itype==6)xpos1=xpos1 + 2.*xsize1-xsize/2.+pmdg_boundary/root32+2.25*pmdg_th_air
                                                   
        if (Itype==7)xpos1=xpos1 + 2.*xsize1-xsize-pmdg_boundary/(2.*root32)+pmdg_boundary


                                                    

        ypos1 = ypos1-zlen0
        Create PHSR 
	 Position PHSR x=xpos1 y=ypos1 z=0.,
                      AlphaZ=phideg   
        ypos1 = ypos1-zlen0
        if (Itype==4) ypos1=(SizeN(72)+SizeN(48)+2.*pmdg_boundary/root32+.375)*root34 
        if (Itype==5) ypos1=(SizeN(72)+SizeN(48)+2.*pmdg_boundary/root32+.375)*root34 
        if (Itype==6) ypos1=ypos1-4.*pmdg_th_air
      enddo
endblock
* -------------------------------------------------------
Block PHSR is a detector box made in air
      Material  Air
      Attribute PHSR   seen=1    colo=6
      Shape     PARA   dx=xlen0-ylen1*pmdg_boundary/(2.*root32),
                       dy=ylen0-ylen2*pmdg_boundary*root34/root32,
                       dz=sm_thick/2 Alph=30 thet=0 phi=0
*--Place the various planes inside the supermodule box.
*position of PMD
        xx = -sm_thick/2.+ sm_thick_a/2.
        Create and Position PMDA Z=xx
*lead converter
        xx = -sm_thick/2. + sm_thick_a + pmdg_th_lead/2.
       Create and Position PPBA   Z=xx
*steel support
        xx = xx + pmdg_th_lead/2. + pmdg_th_steel/2.
       Create and Position PFEA   Z=xx
* position of CPV
        xx = xx + pmdg_th_steel/2. + sm_thick_a/2.
        Create and Position PMDA  Z=xx ThetaZ=180

endblock
*---------------------------------------------------------------------			  
Block PMDA is a detector box made in aluminium
      Material  Aluminium
      Attribute PMDA   seen=1    colo=6
      Shape     PARA   dx=xlen0-ylen1*pmdg_boundary/(2.*root32),
                       dy=ylen0-ylen2*pmdg_boundary*root34/root32,
                       dz=sm_thick_a/2. Alph=30 thet=0 phi=0

      Create and Position AIRA x=-pmdg_boundary/(4.*root32)*xlen1 y=-pmdg_boundary/2.*zlen
			         
endblock
*----------------------------------------------------------
Block AIRA is a detector made in air  

      Material  Air
      Attribute AIRA   seen=1    colo=4
      Shape     PARA    dx=xlen0-pmdg_boundary/root32 dy=ylen dz=sm_thick_a/2. 

* --- Place the various planes inside the detector

* first the chamber PCB   
        zz = -sm_thick_a/2. + pmdg_th_pcb/2.
      Create and Position PCBA   Z=zz
*second the chamber PCB
        zz = zz + pmdg_th_pcb/2 +3.* pmdg_th_air + pmdg_th_pcb/2.
      Create and Position PCBA   Z=zz
* sensetive layer
        zz = zz + pmdg_th_pcb/2. + pmdg_CELL_depth/2.
      Create and Position PHCA   Z=zz
* pcb layer:
        zz = zz + pmdg_CELL_depth/2. + pmdg_th_pcb/2.
      Create and Position PCBA   Z=zz
* G10 base plate
        zz = zz + pmdg_th_pcb/2. + pmdg_th_air + pmdg_th_base/2.
      Create and Position BASA   Z=zz

endblock
*--------------------------------------------------------------
Block PHCA  is the detector made in air
      Material  Air
      Attribute PHCA   seen=1    colo=4

      Shape     PARA    dx=xlen0-pmdg_boundary/root32 dy=ylen dz=pmdg_CELL_depth/2.

*  Place outer hex inside PGCO

      Create ASTR

      DO J = 1,NcellY
        xb=-ylen*(1/(2*root32))+pmdg_hexd1(7)*2./3.  +(J-1)*pmdg_hexd1(7)
        yb=-ylen+(2./sqrt(3.))*pmdg_hexd1(7)+(J-1)*pmdg_hexd1(7)*sqrt(3.)
        Position ASTR X=xb y=yb Z=zb Konly='MANY'
      ENDDO   

endblock
*------------------------------------------------------------------
Block ASTR  is the  strip 
      Material  Air
      Attribute ASTR   seen=0    colo=6
      
      xlen=NCellx*pmdg_CELL_radius
      Shape  PARA  dx=xlen dy=pmdg_CELL_radius/root32 dz=pmdg_CELL_depth/2  _
                   ALPH=0
      create PSTR

endblock
*---------------------------------------------------------
Block PSTR is one pseudo-cell
**      Material  Argon_gas
**      Medium    sensitive  Isvol=1
      shape division Ndiv=NCellx Iaxis=1
**        call GSTPAR (ag_imed, 'CUTGAM', .0001)
**        call GSTPAR (ag_imed, 'CUTELE', .00001)
*  Place outer hex inside PGCO
      create and position PDCU AlphaZ=90

endblock
*---------------------------------------------------------
Block PPBA is The lead plates for different modules
      Material  Lead 
      Attribute PPBA   seen=1    colo=7
      Shape     PARA   dx=xlen0-ylen1*pmdg_boundary/(2.*root32),
                       dy=ylen0-ylen2*pmdg_boundary*root34/root32,
                        dz=pmdg_th_lead/2.

        call GSTPAR (ag_imed, 'CUTGAM', .0001)
        call GSTPAR (ag_imed, 'CUTELE', .0001)
endblock
*-------------------------------------------------------------
Block PFEA is  The iron plates for different modules 
      Material  Iron 
      Attribute PFEA   seen=1    colo=2
      Shape     PARA    dx=xlen0-ylen1*pmdg_boundary/(2.*root32),
                        dy=ylen0-ylen2*pmdg_boundary*root34/root32,
                        dz=pmdg_th_steel/2.

        call GSTPAR (ag_imed, 'CUTGAM', .0001)
        call GSTPAR (ag_imed, 'CUTELE', .0001)
endblock
*------------------------------------------------------
Block BASA is the G10 base plate
*      G10 is about 60% Sio2 and 40% epoxy
        Component Si  A=28.08  Z=14  W=0.6*1*28./60.
        Component O   A=16     Z=8   W=0.6*2*16./60.
        Component C   A=12     Z=6   W=0.4*8*12./174.
        Component H   A=1      Z=1   W=0.4*14*1./174.
        Component O   A=16     Z=8   W=0.4*4*16./174.
        Mixture   G10    Dens=1.7
        Material G10
        
        Attribute BASA    seen=1     colo=6
        Shape     PARA    dx=xlen0-pmdg_boundary/root32 dy=ylen dz=pmdg_th_base/2.

        call GSTPAR (ag_imed, 'CUTGAM', .0001)
        call GSTPAR (ag_imed, 'CUTELE', .0001)
endblock
*-------------------------------------------
Block PCBA is the chamber PCB
*      G10 is about 60% Sio2 and 40% epoxy
         Component Si  A=28.08  Z=14  W=0.6*1*28./60.
         Component O   A=16     Z=8   W=0.6*2*16./60.
         Component C   A=12     Z=6   W=0.4*8*12./174.
         Component H   A=1      Z=1   W=0.4*14*1./174.
         Component O   A=16     Z=8   W=0.4*4*16./174.
         Mixture   G10    Dens=1.7
         Material G10
        
         Attribute PCBA    seen=1     colo=4
         Shape     PARA    dx=xlen0-pmdg_boundary/root32 dy=ylen dz=pmdg_th_pcb/2.

         call GSTPAR (ag_imed, 'CUTGAM', .0001)
         call GSTPAR (ag_imed, 'CUTELE', .0001)
endblock
*-------------------------------------------------------------
Block PDCU is The outer cell in the PMD module
      Material Copper 
      Attribute PDCU   seen=0      colo=3
      Shape  PGON     Phi1=pmdg_hexd1(1) DPhi=pmdg_hexd1(2) Nz=pmdg_hexd1(4),
                      NpDiv=pmdg_hexd1(3),
                      Zi={pmdg_hexd1(5),pmdg_hexd1(8)},
                      rmn={pmdg_hexd1(6),pmdg_hexd1(9)},
                      rmx={pmdg_hexd1(7),pmdg_hexd1(10)}
      Create and Position PDGS  X=0.0 Y=0.0 Z=0.0
endblock
* -----------------------------------------------------------------------
Block PDGS is The inner cell in the PMD module
**      Material  Argon_CO2
      Medium    sensitive  Isvol=1
      Component Ar    A=40  Z=18 W=.7
      Component C     A=12  Z=6  W=.3*12/44.
      Component O     A=16   Z=8  W=.3*32/44.
**PMD uses gas mixture Ar+CO2  7:3 by weight
      Mixture   Ar_CO2 Dens=0.0018405        "g/cm**3"
      Attribute PDGS   seen=0      colo=3
      Shape     PGON  Phi1=pmdg_hexd2(1) DPhi=pmdg_hexd2(2),
                      Nz=pmdg_hexd2(4)   NpDiv=pmdg_hexd2(3),
                      Zi={pmdg_hexd2(5),pmdg_hexd2(8)},
                      rmn={pmdg_hexd2(6),pmdg_hexd2(9)},
                      rmx={pmdg_hexd2(7),pmdg_hexd2(10)}

        call GSTPAR (ag_imed, 'CUTGAM', .0001)
        call GSTPAR (ag_imed, 'CUTELE', .00001)

      HITS      PDGS  Eloss:0:(0,1)
*     HITS      PDGS  Eloss:0:Calo(0,1) - do not keep track id

endblock
* -----------------------------------------------------------------------
*
   end






