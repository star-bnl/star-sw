* $Id: phmdgeo.g,v 1.1 2000/11/29 17:02:41 nevski Exp $
* $Log: phmdgeo.g,v $
* Revision 1.1  2000/11/29 17:02:41  nevski
* first official version
*
**********************************************************************
 Module  PHMDGEO  is the geometry of photon multiplicity detector
**********************************************************************
* YY      = {  98.5,   19.7,   -59.1,   -137.9,  -98.5, -19.7,  59.1} !a
* 7/9/97: Sub:: gas half thickness changed to .5 from 0.15 after discussion
* with GSNM.
* 8/12/97: Sub::  Both cpv and preshower parts have been made 
* with gas (8mm thick)
* 29/4/2000 : Anand : hole placed properly
* 29/11/2000 : Bedanga : final checks to be put in CVS
*
*************************************************************************
+include,AGECOM,GCUNIT.
    Author    Subhasis Chattopadhyay
    Created   November 2000
    Content   PHMD,CPVV,PHOL,PGCO,PFEO,PSCO,PPBO,PDCU,PDGS
    structure PMDG {version, zdist,DPMDx,DPMDy,DPMDz, 
                    DCPVx(2),DCPVy(2),DCPVz,DPHOLx,DPHOLy,DPHOLz,
                    PARGCz,PARSCz,PARFEz,PARPBz,xx(7),yy(7),irot(24),
                    hexd2(10),hexd1(10),dpara(6)}
    Integer         I,J,K
    Real xb,yb,zb,xrow
    Integer number
*
    Fill PMDG   ! PMD geometry
      version = 1      ! geometry version
      ZDIST   = 550.   ! PMD placed at 5.5m from the interaction point
      DPMDx   = 270    !  (X-halfwidth of the PMD box,was 190 earlier)
      DPMDy   = 270    ! Y-halfwidth of the  PMD box.
      DPMDz   = 2.1    ! total z half-width of the box.
      DCPVx   = { 30.7, 38.3 } ! x-half width,y-half width of smaller boxes
      DCPVy   = { 23.4, 29.2 } ! x-halfwidth,  y-half width of the bigger box.
      DCPVz   = 2.0    ! half thickness of the box for pmd+cpv materials. 
      DPHOLx  = 13.2   !  inner module hole x
      DPHOLy  = 11.4   !  inner module hole y
      PARGCz  = 0.40   ! half thickness of gas (CPV sensitive)
      PARSCz  = 0.40   ! (8 mm thick gas)
      PARFEz  = 0.25   ! (iron frame)
      PARPBz  = 0.75   ! (3 X0 of lead converter)
      IROT    = {1,1,1,1,1,1,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0}    !rotation  
      XX      = {-34.19,-34.19,-34.19,-34.19,-102.59,-102.59,-102.59} !x-pos
      YY      = {138.22, 59.24,-19.74,-98.72, 98.72,  19.74, -59.24}  !y-pos
      hexd2   ={0.,360.,6,2,-0.4,0.,0.51,0.4,0.,0.51} ! inner hex
      hexd1   ={0.,360.,6,2,-0.4,0.,0.53,0.4,0.,0.53} ! outer hex
      dpara   = {38.29,33.16,0.4,30.,0.,0.} ! supermodule
    endfill
*
    Use  PMDG  version=1
    Create and Position PHMD in CAVE z=pmdg_ZDIST Konly=Many
*
* -----------------------------------------------------------------------
Block PHMD the PMD box volume and fill with air 
      Material  Air
      Medium    Standard
      Attribute PMDD   seen=0    colo=2
      Shape     BOX    dx=pmdg_DPMDx  dy=pmdg_DPMDy  dz=pmdg_DPMDz+0.1

* --- Place the CPV in PMD with front edge 600cm from the target ---
         Do K=1,7
            Create and Position CPVV  x=pmdg_XX(k)  y=pmdg_YY(k),
                                      Z=-pmdg_DPMDz+pmdg_DCPVz,
                                      AlphaZ=90    
         enddo
         Do K=8,14
            Create and Position CPVV  x=abs(pmdg_XX(k-7))  y=pmdg_YY(k-7),
                                      Z=-pmdg_DPMDz+pmdg_DCPVz,
                                      AlphaZ=30*pmdg_IROT(k-7)

         enddo
* --- Make the central hole in the PMD and fill with vacuum ---

        Create and position PHOL   x=-11.4 y=19.8,
                                  Z=-pmdg_DPMDz+pmdg_DCPVz,
   					AlphaZ=90    Konly='MANY'
        Create and position PHOL   x=-11.4 y=-6.6,
                                  Z=-pmdg_DPMDz+pmdg_DCPVz,
   					AlphaZ=90    Konly='MANY'
        Create and position PHOL   x=11.4 y=19.8,
                                  Z=-pmdg_DPMDz+pmdg_DCPVz,
   					AlphaZ=30    Konly='MANY'
        Create and position PHOL   x=11.4 y=-6.6,
                                  Z=-pmdg_DPMDz+pmdg_DCPVz,
   					AlphaZ=30    Konly='MANY'
endblock
* ----------------------------------------------------------------------- 
Block CPVV is a detector box
       Attribute CPVV   seen=1       colo=7
*      Shape     BOX    dx=pmdg_DCPVx(j)  dy=pmdg_DCPVy(j)  dz=pmdg_DCPVz
      Shape     PARA    dx=39.49  dy=34.19  dz=pmdg_DCPVz Alph=30. thet=0 phi=0

*     Now place the gas planes
*      Create and Position PGCO  Z=pmdg_DCPVz-2*pmdg_PARFEz-2*pmdg_PARPBz _
*                                            -pmdg_PARGCz-2*pmdg_PARSCz
* --- Place the various planes inside the cameras
* --- Start with the iron at the front edge of the PMD ---
*      Create and Position PFEO  Z=pmdg_DCPVz-pmdg_PARFEz-2*pmdg_PARPBz _
*                                                        -2*pmdg_PARSCz
*      Create and Position PPBO  Z=pmdg_DCPVz-pmdg_PARPBz-2*pmdg_PARSCz
*      Create and Position PGCO  Z=pmdg_DCPVz-pmdg_PARSCz

      Create and Position PGCO  Z=-pmdg_DCPVz+pmdg_PARGCz
      Create and Position PFEO  Z=-pmdg_DCPVz+2*pmdg_PARGCz+pmdg_PARFEz
      Create and Position PPBO  Z=-pmdg_DCPVz+2*pmdg_PARGCz+2*pmdg_PARFEz _
                       +pmdg_PARPBz
      Create and Position PGCO  Z=-pmdg_DCPVz+2*pmdg_PARGCz+2*pmdg_PARFEz _
                       +2*pmdg_PARPBz+pmdg_PARGCz
endblock
* -----------------------------------------------------------------------  
Block PGCO  is the detector
      Material  Argon_gas
      Medium    sensitive  Isvol=1
      Attribute PGCO   seen=1    colo=6
*      Shape     BOX    dz=pmdg_PARGCz
      Shape     PARA    dx=38.29 dz=0.4
	call GSTPAR (ag_imed, 'CUTGAM', .0001)
	call GSTPAR (ag_imed, 'CUTELE', .00001)
*  Place outer hex inside PGCO
      xrow=1
      yb = -pmdg_dpara(2) + (2./sqrt(3.))*pmdg_hexd1(7)
      zb = 0.
    DO J = 1,72
       xb=-(pmdg_dpara(1)+
       pmdg_dpara(2)*0.577)+2*pmdg_hexd1(7)
       IF(XROW .GE. 2)THEN
        xb = xb+(xrow-1)*pmdg_hexd1(7)
       ENDIF  
      DO I = 1,72
       number = i+(j-1)*72;
       Create and Position PDCU  X=xb Y=yb Z=zb,
                        AlphaZ=90.
*                        AlphaX=180.                                
       xb = xb+pmdg_hexd1(7)*2.
      ENDDO 
       xrow = xrow+1
       yb = yb+pmdg_hexd1(7)*sqrt(3.)
    ENDDO   
*
*      HITS      PGCO  xx:32:H(-200,200)  yy:32:(-200,200)  zz:32:(-1000,1000),
*                      px:16:(-100,100)   py:16:(-100,100)  pz:16:(-100,100),
*                      Slen:16:(0,1.e4)   Tof:16:(0,1.e-6)  Step:16:(0,100),
*                      none:16:           Eloss:32:(0,1)
endblock
* -----------------------------------------------------------------------
Block PPBO is The lead plates for different cameras
      Material  Lead 
      Attribute PPBO   seen=1    colo=4
*      Shape     BOX    dz=pmdg_PARPBz
      Shape     PARA    dz=pmdg_PARPBz
	call GSTPAR (ag_imed, 'CUTGAM', .0001)
	call GSTPAR (ag_imed, 'CUTELE', .0001)
endblock
* -----------------------------------------------------------------------
Block PFEO is  The iron plates for different cameras 
      Material  Iron 
      Attribute PFEO   seen=1    colo=2
*      Shape     BOX    dz=pmdg_PARFEz
      Shape     PARA    dz=pmdg_PARFEz
	call GSTPAR (ag_imed, 'CUTGAM', .0001)
	call GSTPAR (ag_imed, 'CUTELE', .0001)
endblock
* -----------------------------------------------------------------------
*block PSCO is The sc pad plane 
*      Material  Argon_gas 
*      Medium    Sensitive   Isvol=1
*      Attribute PSCO  seen=1    colo=4
**      Shape     BOX   dz=pmdg_PARSCz
*      Shape     PARA    dx=38.29 dz=0.4
*	call GSTPAR (ag_imed, 'CUTGAM', .0001)
*	call GSTPAR (ag_imed, 'CUTELE', .0001)
*      HITS    PSCO  xx:16:H(-200,200)  yy:16:(-200,200)  zz:32:(-1000,1000),
*                      px:16:(-100,100)   py:16:(-100,100)  pz:16:(-100,100),
*                      Slen:16:(0,1.e4)   Tof:16:(0,1.e-6)  Step:16:(0,100),
*                      none:16:           Eloss:32:(0,1)
*endblock
*
* -----------------------------------------------------------------------
block PDCU is The outer cell in the PMD box
      Material Copper 
      Attribute PDCU   seen=1      colo=3
      Shape  PGON     Phi1=pmdg_hexd1(1) DPhi=pmdg_hexd1(2) Nz=pmdg_hexd1(4),
                      NpDiv=pmdg_hexd1(3),
                      Zi={pmdg_hexd1(5),pmdg_hexd1(8)},
                      rmn={pmdg_hexd1(6),pmdg_hexd1(9)},
                      rmx={pmdg_hexd1(7),pmdg_hexd1(10)}
      Create and Position PDGS  X=0.0 Y=0.0 Z=0.0
endblock
* -----------------------------------------------------------------------
Block PDGS is The inner cell in the PMD box
      Material  Argon_gas
      Medium    sensitive  Isvol=1
      Attribute PDGS   seen=1      colo=4
      Shape  PGON     Phi1=pmdg_hexd2(1) DPhi=pmdg_hexd2(2) Nz=pmdg_hexd2(4),
                      NpDiv=pmdg_hexd2(3),
                      Zi={pmdg_hexd2(5),pmdg_hexd2(8)},
                      rmn={pmdg_hexd2(6),pmdg_hexd2(9)},
                      rmx={pmdg_hexd2(7),pmdg_hexd2(10)}

      HITS      PDGS  x:0.001:   y:0.001:  z:0.001:,
                      cx:0.001:  cy:0.001: cz:0.001:,
                      Slen:16:(0,1.e4)   Tof:16:(0,1.e-6),  
                      Step:16:(0,100)    Eloss:32:(0,1)
endblock
* -----------------------------------------------------------------------
*
Block PHOL is the central hole in the PMD 
      Material  Vacuum
      Attribute PHOL   seen=1      colo=3
      Shape     PARA    dx=13.2  dy=11.4  dz=pmdg_DPMDz Alph=30. thet=0 phi=0
endblock
*
* -----------------------------------------------------------------------
   end
