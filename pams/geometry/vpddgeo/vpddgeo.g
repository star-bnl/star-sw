******************************************************************************
module  VPDDGEO  is the Vertex Position Detector of STAR
Author  Pavel Nevski
Created 27 March 1996
* tentative dimension set according to the:
*  - 'VPD geometry' note by Z.Milosevich et al.
*  - L.Geiner drawings and numbers 
*  7-18-96 Modified by Z. Milosevich
*          Detector positions corrected and set according to STAR Note
* 10-10-96 Modified by Z. Milosevich
*          Silicon Material in radiator replaced by quartz (SiO2)
* 10-30-96 Modified by Z. Milosevich
*          Detector position put at 290 cm to accommodate FTPC
* 11-10-97 Modified by Z. Milosevich
*          Added energy to hit.
******************************************************************************
+CDE,AGECOM,GCUNIT,GCONST.
*
     CONTENT   VPDD,VRNG,VSEC,VDET,VCNV,VRAD,VALL,VSUP,VPMT,VXST
     Structure VPDG { Version,Rmin,Rmax,Length,Position,DrLayer(2),NumPMT(2),
                      PMTradi,PMTwall,PMTleng,EleLeng,ConvThk,RadiThk,TubeThk,
                      SuppThk,SuppDist }
* Local variables
     Real      Rcurrent,Detector_Length
     Integer   Layer
* ----------------------------------------------------------------------------
*
     FILL VPDG           ! VPD basic dimensions
        Version  = 1         ! geometry version
        Rmin     = 4         ! VPD inner radius
        Rmax     = 16        ! VPD outer radius
        Length   = 30        ! full VPD assembly length along the beam 
        Position = 290       ! Z position of VPD along beam axis
        DrLayer  = {6,6 }    ! layer radial width
        NumPMT   = {8,16}    ! number of PMT in layer
        PMTwall  = 0.1       ! PMT wall thickness
        PMTradi  = 2.54      ! PMT and detector radius
        PMTLeng  = 8.0       ! PMT tube length 
        EleLeng  = 15.0      ! electronics mount length
        ConvThk  = 0.635     ! Converter layer thickness
        RadiThk  = 0.635     ! Radiator layer thickness
        TubeThk  = 0.00      ! piece of beam pipe thickness (if needed)
        SuppThk  = 0.64      ! Support rings thickness
        SuppDist = 16        ! distance between supporting rings
     Endfill
*
     USE  VPDG  Version=1
     Create and Position VPDD in Cave  z=+vpdg_Position
                Position VPDD in Cave  z=-vpdg_Position   ThetaZ=180
* ----------------------------------------------------------------------------
Block VPDD  is the whole VPD assembly
     Material  Air
     Medium    Standard
     Attribute VPDD   Seen=0  colo=5
     Shape     TUBE   Rmin=vpdg_Rmin-vpdg_TubeThk  Rmax=vpdg_Rmax,
                      Dz=vpdg_Length/2
*    
     If (vpdg_TubeThk>0)  then
        create and Position VALL 
     Endif     
     Rcurrent = vpdg_Rmin
     Do layer=1,2     
        Create and Position VRNG 
        Rcurrent = Rcurrent + vpdg_DrLayer(Layer)
     enddo
Endblock
* -----------------------------------------------------------------------------
Block VRNG  is a single (inner or outer) VPD Ring
     detector_length = vpdg_ConvThk+vpdg_RadiThk+vpdg_PMTleng+vpdg_EleLeng
     Shape     TUBE    Rmin=Rcurrent  Rmax=Rcurrent+vpdg_DrLayer(Layer),
                       Dz=Detector_length/2
     Create    VSEC
Endblock
* 
Block VSEC  is one VPD sector with all stuff inside
     Shape  Division           Iaxis=2  Ndiv=vpdg_NumPMT(layer),
                                        C0=90
*
     create and position VDET  X=Rcurrent+vpdg_DrLayer(Layer)/2 
     create and position VSUP  z=-vpdg_SuppDist/2   Konly='MANY'  
     create and position VSUP  z=+vpdg_SuppDist/2   Konly='MANY' 
*
*                     these cables are for fan only
     create and position VXST  x=Rcurrent+vpdg_DrLayer(Layer)/2+0.4  y=+1.5,
                               z=Detector_length/2+1.0
     create and position VXST  x=Rcurrent+vpdg_DrLayer(Layer)/2-0.4  y=-1.5,
                               z=Detector_length/2+1.0
EndBlock
* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Block VDET  is a single detector (Radiator+converter and PMT+electroncs)
* not going into too much details, approximate PMT walls and electronic by this
     Material  Polystyren
     Attribute VDET   Seen=1  Colo=7
     SHAPE     TUBE   Rmin=0  Rmax=vpdg_PMTradi
     Create and position VCNV z=-(vpdg_PMTleng+vpdg_EleLeng)/2
     Create and position VPMT z=+(vpdg_ConvThk+vpdg_RadiThk-vpdg_EleLeng)/2
endblock
*
Block VCNV  is converter layer (radiator included)
     Material  Lead 
     Attribute VCNV   seen=1   Colo=2
     SHAPE     TUBE   dz=vpdg_ConvThk/2+vpdg_RadiThk/2
     create and position VRAD  z=vpdg_ConvThk/2
EndBlock
*
Block VRAD  is Cerenkov Radiator layer
     Component Si    A=28.09   Z=14  W=1
     Component O2    A=16      Z=8   W=2
     Mixture   SiO2  Dens=2.65
     Medium    sensitive   IsVol=1
     Attribute VRAD   seen=1   Colo=3
     Shape     TUBE   dz=vpdg_RadiThk/2
     HITS      VRAD   xx:16:H(-25,25)    yy:16:(-25,25)    zz:32:(-1000,1000),
                      px:16:(-100,100)   py:16:(-100,100)  pz:16:(-100,100),
                      Slen:16:(0,1.e4)   Tof:16:(0,1.e-6)  Step:16:(0,100),
                      ETOT:16:(0,100)    Eloss:32:(0,1)
EndBlock
*
Block VPMT is the PMT inner volume
     Material  Vacuum
     Attribute VPMT   Seen=1   Colo=7
     Shape     TUBE   Rmax=vpdg_PMTradi-vpdg_PMTwall,
                      Dz=vpdg_PMTleng/2-vpdg_PMTwall 
EndBlock
* ----------------------------------------------------------------------------
Block VALL is a mainframe supporting tube
     Material  Aluminium 
     Attribute VALL   seen=1   Colo=4
     Shape     TUBE   Rmin=vpdg_Rmin     Rmax=vpdg_Rmin+vpdg_TubeThk
EndBlock
*
Block VSUP are PMT assembly supports
     Material  Aluminium 
     Attribute VSUP   seen=1   Colo=6
     Shape     TUBS   Dz=vpdg_SuppThk/2  Phi1=-180/vpdg_NumPMT(layer),
                                         Phi2=+180/vpdg_NumPMT(layer)
EndBlock
*
Block VXST are PMT output cables (just to look nicer)
     attribute VXST   seen=1   Colo=7
     shape     TUBE   Rmin=0   Rmax=0.3   dz=1.0
Endblock
     END









