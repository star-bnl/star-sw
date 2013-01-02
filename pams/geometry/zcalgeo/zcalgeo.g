******************************************************************
Module ZCALGEO is the geometry of the Zero deg. Quartz Calorimeter
Created 10-Feb-1997
Author  W.B.Christie
****************************************************************
+CDE,AGECOM,GCUNIT.
*
        Content ZCAL, QCAL, QDIV, QSCI, PIPH, PVAH, PLAT, PLVA,
                PIPJ, PVAJ                                                 
*
        Structure CALP {Version, DROutR, DRdz,
                        PHLeng, PHInnR, PHOutR,
                        Pltdz, PltOutR,  HOutR,
                        PJInnR, PJOutR, PJLeng,
                        QCdx, QCdy, QCdz, SCdz, Sdiv }
*
*  Local variables
        Real z1, z2, z3, z4

*
*--------------------------------------------------------------
*
   FILL CALP          ! Quartz Calorimeter Data
     version = 1     ! Geometry version number
     DROutR  = 40.0  ! Outer radius of mother volume tube
     DRdz    = 250.0 ! Half length of mother volume
     PHLeng  = 133.35! Half Length of large diameter Pipe.
     PHInnR  = 20.0  ! Inner radius of Pipe H
     PHOutR  = 20.96 ! Outer radius of Pipe H
     Pltdz   = 0.47  ! Half thickness of plate at end of large dia pipe.
     PltOutR = 20.96 ! Radius of plate.
     HOutR   = 6.35  ! Radius of holes in the End plate
     PJInnR  = 6.07  ! Inner radius of final beam pipe
     PJOutR  = 6.35  ! Outer radius of final beam pipe
     PJLeng  = 91.5  ! Half length of final beam pipe
     QCdx    = 5.0   ! Half width of Qcal (cm)
     QCdy    = 5.0   ! Half width of Qcal (cm)
     QCdz    = 65.0  ! Half length of Qcal (cm)
     SCdz    = 0.05  ! Half length of Fiber layer (cm)
     Sdiv    = 260   ! Number of Fiber layers in Qcal
   endfill
*
   USE CALP Version = 1
*
        z1 = -calp_drdz + calp_phleng  ! Center of Large Dia. Pipe (-116.65)
        z2 = z1+calp_phleng+calp_pltdz ! Center of lrg pipe end plate (17.17)
        z3 = z2+calp_pltdz+0.24+calp_pjleng    ! Center of beam pipes 
        z4 = z2+calp_pltdz+40.0+calp_qcdz ! Center of Qcal
*
*       The start of the ZCAL volume should be 2 mm past the
*       end of the mother volume that contains the DX magnet.
   Create   ZCAL
   Position ZCAL in CAVE z=1767.66
   Position ZCAL in CAVE z=-1767.66 thetaZ=180
*
*----------------------------------------------------------------
Block ZCAL is the region between the DX and the D0 magnets
       Material Air
       Medium standard
       Attribute ZCAL seen = 0 colo = 1
       SHAPE     TUBE rmin=0.0 rmax=calp_droutr dz=calp_drdz
       Create and position PIPH z=z1
       Create and Position PLAT z=z2
       Create and Position QCAL z=z4
       Create    PIPJ
       Position  PIPJ x=12.82  z=z3 alphay=1.074
       Position  PIPJ x=-12.82 z=z3 alphay=-1.074
endblock
*---------------------------------------------------------------
Block PIPH is the Large diameter Pipe before the beam pipes split
       Material  Iron
       Attribute Piph      Seen=1  colo=2
       Shape     TUBE      Rmin=0  Rmax=calp_phOutR,
                           Dz=calp_phLeng
       Create and Position PVAH
EndBlock
*
Block PVAH is the Vacuum Volume of the large diameter pipe
       Material  Vacuum
       Shape     TUBE      Rmax=calp_phInnR  
EndBlock
*---------------------------------------------------------------
Block PLAT is the End Plate of the large dia. Pipe
       Material  Iron
       Attribute Plat      Seen=1  colo=2
       Shape     TUBE      Rmin=0  Rmax=calp_pltOutR,
                           Dz=calp_pltdz
       Create PLVA
       Position PLVA x=11.10  
       Position PLVA x=-11.10 
EndBlock
*
Block PLVA is the Vacuum Volume of the beam pipe holes in the end plate
       Material  Vacuum
       Shape     TUBE      Rmax=calp_houtR  
EndBlock
*---------------------------------------------------------------
Block PIPJ are the final beam Pipes
       Material  Iron
       Attribute PIPJ      Seen=1  colo=7
       Shape     TUBE      Rmin=0  Rmax=calp_pjOutR,
                           Dz=calp_pjleng
       Create and Position PVAJ
EndBlock
*
Block PVAJ is the Vacuum Volume of the final beam pipes
       Material  Vacuum
       Shape     TUBE      Rmax=calp_pjinnr  
EndBlock
*---------------------------------------------------------------

Block QCAL is the Zero degree calorimeter
        Material Lead
        Material dirty_lead   Isvol=0
        Attribute Qcal seen=1 colo = 4
        SHAPE BOX dx=calp_qcdx dy=calp_qcdy dz=calp_qcdz
        Call GSTPAR (ag_imed,'CUTGAM',0.0005)
        Call GSTPAR (ag_imed,'CUTELE',0.00015)
        Create QDIV
endblock
*---------------------------------------------------------------
*
Block QDIV is one section/layer of the Quartz Calorimeter
        SHAPE division Iaxis=3 Ndiv=calp_sdiv
        Create and Position QSCI z=-calp_qcdz/(calp_sdiv)+calp_scdz
endblock
*----------------------------------------------------------------
*
Block QSCI is a sensitive Fiber layer
        Material Polystyren
        Material scintillator isvol=1
        Attribute QSCI seen=1 colo=3
        Shape BOX dz=calp_scdz
        Call GSTPAR (ag_imed,'CUTGAM',0.0005)
        Call GSTPAR (ag_imed,'CUTELE',0.00015)
*       HITS QSCI x:0.01: y:0.01: z:0.01: cx:10: cy:10: cz:10:  _
*                 lgam:16:(0,5) Etot:16:(0,1000) Eloss:0:(0,1) 
        HITS QSCI Eloss:0:(0,10) 
endblock
*-------------------------------------------------------------------
END



