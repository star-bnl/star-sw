**********************************************************************
 Module  PHMDGEO  is the geometry of photon multiplicity detector
**********************************************************************
*
*1st July 2001 : Bedanga and Pavel
*
* The New PMD geometry has been implemented 
* by defining a 1/3rd sector volume (PHMS)
* There are 5 supermodules of varying sizes in a sector (Itype,Ncellx,Ncelly)
* In each module we place the cells stripwise (STRA) and in
* each strip we create and position pusedo-cell (PSTR)
*
*Viyogi,Dipak,Tapan, Murthy
*
* The geometry for PMD, with all materials properly
* taken care of. PCB, Base plate(G10), Lead, steel support.
* The hole in PMD automatically formed
* by positioning of modules. The New geometry design was
* finalised
*
+include,AGECOM,GCUNIT,GCONST.
    Author    Y.P. Viyogi,Subhasis,Tapan,Bedanga,Anand and Dipak
    Created   03-july-2001
    Content   PHMD,PMDA,PHCA,PCBA,BASA,AIRA,PPBA,PFEA,
	      STRA,PDCU,PDGS,PSTR,PHMS
 	      
    structure PMDG {version,m_max,m_min,zdist,DPMDx,DPMDy,DPMDz, 
                    PARGCz,PARSCz,PARFEz,PARPBz,
                    cell_radius,cell_depth,cell_wall,boundary,
                    th_base,th_air,th_pcb,th_lead,th_steel,
                    Int nx(5),Int ny(5),hexd2(10),hexd1(10),dpara(6)}

    Integer         J,Itype,Ncellx,Ncelly,N
    Real xb,yb,zb,xlen,xlen0,ylen,ylen0,phi,phideg,xpos,ypos,xsize,ysize
    Real sm_thick,zz,root32,root34,SizeN
    SizeN (N) = ((N + 1./3.)*pmdg_CELL_RADIUS)*2 + pmdg_boundary*2
*
    Fill PMDG   ! PMD geometry
      version = 1      ! geometry version
      m_max   = 150.   ! Mother volume max radius
      m_min   = 15.    ! Mother volume min radius
      ZDIST   = 550.   ! PMD placed at 5.5 metre from the interaction point
      DPMDx   = 270    !  (X-halfwidth of the PMD box,was 190 earlier)
      DPMDy   = 270    ! Y-halfwidth of the  PMD box.
      DPMDz   = 10.    ! total z half-width of the box.
      PARGCz  = 0.40   ! half thickness of gas (CPV sensitive)
      PARSCz  = 0.40   ! (8 mm thick gas)
      PARFEz  = 0.25   ! (iron frame)
      PARPBz  = 0.75   ! (3 X0 of lead converter)
      nx      = { 56,72,72,48,48 } !  x-dimensions of modules
      ny      = { 16,56,48,72,48 } !  y-dimensions of modules
      hexd2   = {0.,360.,6,2,-0.4,0.,0.51,0.4,0.,0.51} ! inner hex
      hexd1   = {0.,360.,6,2,-0.4,0.,0.53,0.4,0.,0.53} ! outer hex
      dpara   = {38.29,33.16,0.4,30.,0.,0.} ! supermodule
      CELL_RADIUS = 0.53   ! Radius of a cell
      CELL_DEPTH  = 0.8    ! Gas depth
      CELL_WALL   = 0.2    ! Cell wall thickness
      BOUNDARY    = 0.7    ! Boundary
      th_base     = 0.3    ! Thickness of the base plate
      th_air      = 0.1    ! Air gap
      th_pcb      = 0.16   ! Thickness of the PCB
      th_lead     = 1.5    ! Thickness of the Lead
      th_steel    = 0.5    ! Thickness of the steel support
    endfill

      Use  PMDG  version=1
      root32=sqrt(3.)/2.
      root34=root32/2.
      sm_thick=2.*(pmdg_th_base + 2.*pmdg_th_air + 3.*pmdg_th_pcb _
                   + pmdg_CELL_depth) +pmdg_th_lead + pmdg_th_steel

      Create and Position PHMD in CAVE z=pmdg_ZDIST
*
* -----------------------------------------------------------------------
Block PHMD the PMD box volume and fill with air 
      Material  Air
      Medium    STandard
      Attribute PHMD   seen=0    colo=2
      Shape     TUBE   Rmin=pmdg_m_min  Rmax=pmdg_m_max  Dz=sm_thick/2.
      create    PHMS
endblock    
* -----------------------------------------------------------------------
Block PHMS the PMD sector volume - 1/3rd of PHMD

      Attribute PHMS     seen=1  colo=2
      Shape     Division Ndiv=3  Iaxis=2
                          
* --- Place the CPV in PMD with front edge 550cm from the target ---

      phideg = -120
      phi    = phideg*degrad
      ypos   = 0
* Modules numbered in an anti-clockwise manner
      do Itype = 1,5
        xsize = SizeN(pmdg_Nx(Itype))
        ysize = SizeN(pmdg_Ny(Itype))
        xlen0  = xsize/2
        ylen0  = (ysize-pmdg_boundary)*root34  
        ylen   = (ysize-2*pmdg_boundary)*root34            
        zb     = 0
        NcellX = pmdg_Nx(Itype)
        NcellY = pmdg_Ny(Itype)
        xpos   = SizeN(72)-xsize/2
        if (Itype>3) xpos = SizeN(72)+xsize/2

        ypos = ypos+ysize/2
        Create PMDA 
        Position PMDA x=(-xpos+ypos/2)*cos(phi)-ypos*sin(phi)*Root32,
                      y=(-xpos+ypos/2)*sin(phi)+ypos*cos(phi)*Root32,
	              AlphaZ=phideg
  
        ypos = ypos+ysize/2
        if (Itype==3) ypos=0
      enddo

endblock
* ----------------------------------------------------------------------- 
Block PMDA is a detector box
      Material  Aluminium
      Attribute PMDA   seen=1    colo=6
      Shape     PARA   dx=xlen0 dy=ylen0 dz=sm_thick/2 Alph=30 thet=0 phi=0

      Create and Position AIRA x=-pmdg_boundary/4. y=-pmdg_boundary*root34

endblock
*----------------------------------------------------------
Block AIRA is a detector made in air  

      Material  Air
      Attribute PMDA   seen=1    colo=4
      Shape     PARA    dx=xlen0-pmdg_boundary dy=ylen dz=sm_thick/2. 

* --- Place the various planes inside the supermodules

* first the chamber PCB   
        zz = -sm_thick/2. + pmdg_th_pcb/2.
      Create and Position PCBA   Z=zz
	zz = zz + pmdg_th_pcb/2 + pmdg_th_air + pmdg_th_pcb/2.
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
* lead convertor
	zz = zz + pmdg_th_base/2. + pmdg_th_lead/2.
      Create and Position PPBA   Z=zz
* still support
	zz = zz + pmdg_th_lead/2. + pmdg_th_steel/2.
      Create and Position PFEA   Z=zz
* second G10 base plate 
	zz = zz + pmdg_th_steel/2. + pmdg_th_base/2.
      Create and Position BASA   Z=zz
* PCB second layer
	zz = zz + pmdg_th_base/2. + pmdg_th_air + pmdg_th_pcb/2.
      Create and Position PCBA   Z=zz
* sensetive layer
	zz = zz + pmdg_th_pcb/2. + pmdg_CELL_depth/2.
      Create and Position PHCA   Z=zz
* last two PCB
	zz = zz + pmdg_CELL_depth/2. + pmdg_th_pcb/2.
      Create and Position PCBA   Z=zz
	zz = zz + pmdg_th_pcb/2. + pmdg_th_air + pmdg_th_pcb/2.
      Create and Position PCBA   Z=zz

endblock
*------------------------------------------------------------------
Block PHCA  is the supermodule
      Material  Air
      Attribute PHCA   seen=1    colo=7

      Shape     PARA    dx=xlen0-pmdg_boundary dy=ylen dz=pmdg_CELL_depth/2.

*  Place outer hex inside PGCO

      Create STRA

      DO J = 1,pmdg_Ny(Itype)
        xb=-ylen*(1/(2*root32))+pmdg_hexd1(7)*2./3.  +(J-1)*pmdg_hexd1(7)
        yb=-ylen+(2./sqrt(3.))*pmdg_hexd1(7)+(J-1)*pmdg_hexd1(7)*sqrt(3.)
        Position STRA X=xb y=yb Z=zb Konly='MANY'
      ENDDO   

endblock
*------------------------------------------------------------------
Block STRA  is the  supermodule
      Material  Air
      Attribute STRA   seen=1    colo=6
      
      xlen=NCellx*pmdg_CELL_radius
      Shape  PARA  dx=xlen dy=pmdg_CELL_radius/root32 dz=pmdg_CELL_depth/2  _
                   ALPH=0
      create PSTR

endblock
*---------------------------------------------------------
Block PSTR is one pseudo-cell

      shape division Ndiv=NCellx Iaxis=1
*  Place outer hex inside PGCO
      create and position PDCU AlphaZ=90

endblock
*---------------------------------------------------------
Block PPBA is The lead plates for different modules
      Material  Lead 
      Attribute PPBA   seen=0    colo=4
      Shape     PARA    dx=xlen0-pmdg_boundary dy=ylen dz=pmdg_th_lead/2.

	call GSTPAR (ag_imed, 'CUTGAM', .0001)
	call GSTPAR (ag_imed, 'CUTELE', .0001)
endblock
*-------------------------------------------------------------
Block PFEA is  The iron plates for different modules 
      Material  Iron 
      Attribute PFEA   seen=0    colo=2
      Shape     PARA    dx=xlen0-pmdg_boundary dy=ylen dz=pmdg_th_steel/2.

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
	
        Attribute BASA    seen=0     colo=4
        Shape     PARA    dx=xlen0-pmdg_boundary dy=ylen dz=pmdg_th_base/2.

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
	
         Attribute PCBA    seen=0     colo=4
         Shape     PARA    dx=xlen0-pmdg_boundary dy=ylen dz=pmdg_th_base/2.

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
      Material  Argon_gas
      Medium    sensitive  Isvol=1
      Attribute PDGS   seen=0      colo=3
      Shape     PGON  Phi1=pmdg_hexd2(1) DPhi=pmdg_hexd2(2),
                      Nz=pmdg_hexd2(4)   NpDiv=pmdg_hexd2(3),
                      Zi={pmdg_hexd2(5),pmdg_hexd2(8)},
                      rmn={pmdg_hexd2(6),pmdg_hexd2(9)},
                      rmx={pmdg_hexd2(7),pmdg_hexd2(10)}

      HITS      PDGS  Eloss:0:(0,1)
*     HITS      PDGS  Eloss:0:Calo(0,1) - do not keep track id

endblock
* -----------------------------------------------------------------------
*
   end

















