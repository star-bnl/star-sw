********************************************************************************
Module  MAGPGEO is the geometry of the STAR magnet
********************************************************************************
+CDE,AGECOM,GCUNIT.
   Author    Pavel Nevski
   Created   19 March 1996
*  Original  version:  W.B. Christie 15-NOV-1993
   Content   MAGP,COIL,MTCL,MPTV,MPCV,MRET,MRGV,MSEC,MBAR,MCSE
   structure MAGG { version,Rmax,Length,test}
   structure MBAR { CoilRmn,CoilRmx,CoilLen,RetYRmn,RetYLen,
                    BarWidin,BarWidou,BarHeigh,RingRmn,
                    Ncoil,zcoil(6),dzcoil(6) }
   structure MEND { PoleRmn,poleZ,PoleRmx,tcoilRmn,tcoilRmx,PoleCavR,
                    PoleCavD,tcoilDZ,etacut  }
   real      TanTheta,Rcorner,Zcut,d
   Integer   I_coil
********************************************************************************
*
   Fill MAGG             ! Magnet basic dimensions
      version  = 1          ! version number
      Rmax     = 364.29     ! outer radius of the magnet system
      Length   = 715.00     ! magnet system full length
      test     = 0          ! geometry type: 0-standard, 1-test
   Fill MBAR             ! Barrel Part 
      CoilRmn  = 264.90     ! barrel coil inner radius (barrel minimum)
      CoilRmx  = 299.30     ! barrel coil outer radius
      CoilLen  = 627.40     ! barrel coil full length
      RetYRmn  = 303.29     ! Return Yoke minimum radius
      RetYLen  = 684.40     ! Return Yoke full length
      BarWidin = 44.34      ! return yoke bare inner width
      BarWidou = 57.15      ! return yoke bare outer width
      BarHeigh = 60.00      ! return yoke bare height
      RingRmn  = 263.68     ! Return Ring minimum radius
      Ncoil    = 12         ! total number of barrel coils
"=>"  Zcoil    = {30.95, 89.05, 147.17, 205.25, 249.0,  288.35} ! coil position
      dZcoil   = {45.24, 45.24,  45.24,  45.24,  22.71,  45.24} ! coil width  
   Fill MEND             ! EndCap Part 
      PoleZ    = 310.007    ! pole tip nominal position
      PoleRmn  = 90.028     ! Pole tip inner radius (encap minimum)
      PoleRmx  = 252.882    ! Pole tip outer radius
      PoleCavR = 152.4      ! Pole tip cavity outer radius
"=>"  PoleCavD = 18         ! Pole tip cavity depth
      tcoilRmn = 91.34      ! Pole tip trim coil inner radius
      tcoilRmx = 141.28     ! Pole tip trim coil outer radius
      tcoilDZ  = 16.5       ! full width of Pole Tip trim Coil
      etacut   = 2          ! eta limits for the Pole 
   Endfill
*
       Use    MAGG  version=1 
       Use    MBAR
       Use    MEND
       TanTheta = (exp(+mend_etacut)-exp(-mend_etacut))/2

       create and position MAGP in Cave

* --------------------------------------------------------------------------
Block MAGP is the magnet mother
       material  Air
       medium    Standard
       Attribute MAGP     seen=0  colo=1
       If (magg_test == 1) then
          Shape     TUBE  Rmin=mend_PoleRmn  Rmax=magg_Rmax  dz=magg_Length/2
       else
          Shape     PCON  Phi1=0  dPhi=360  nz=6,
                    zi  = {-magg_Length/2, -mend_PoleZ,  -mend_PoleZ,
                            mend_PoleZ,     mend_PoleZ,   magg_Length/2 }, 
                    rmn = { mend_PoleRmn,   mend_PoleRmn, mbar_CoilRmn,
                            mbar_CoilRmn,   mend_PoleRmn, mend_PoleRmn  },
                    rmx = { magg_Rmax,      magg_Rmax,    magg_Rmax,
                            magg_Rmax,      magg_Rmax,    magg_Rmax     }; 
       endif
*
       Create and Position COIL 
       Create and Position MRET 
       Create and Position MPTV  Z=+mend_PoleZ
                  Position MPTV  Z=-mend_PoleZ       thetaZ=180
       Create and Position MRGV  Z=+mbar_coilLen/2 
                  Position MRGV  Z=-mbar_coilLen/2  thetaZ=180
EndBlock
* --------------------------------------------------------------------------
*                             barrel structure
* --------------------------------------------------------------------------
Block COIL is the main coil mother
      Attribute COIL    seen=0  colo=2
      SHAPE     TUBE    Rmin=mbar_coilRmn Rmax=mbar_coilRmx dz=mbar_coilLen/2
      DO I_coil=1,nint(mbar_Ncoil/2)
         Create and position MCSE z=+mbar_Zcoil( I_coil )
                    position MCSE z=-mbar_Zcoil( I_coil )
      enddo
EndBlock
*
Block MCSE is a single barrel coil
      Material  Aluminium  
      Attribute MCSE    seen=1  colo=3
      Shape     TUBE    dz=mbar_DzCoil( I_coil )/2
EndBlock
* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Block MRET is Magnet RETurn Yoke 
      Attribute MRET    seen=0  colo=3
      SHAPE     TUBE    Rmin=mbar_RetYRmn  Rmax=magg_Rmax  dz=mbar_RetYLen/2
      Create    MSEC
EndBlock
*
Block MSEC is a sector containing a single retun bar
      Shape     Division           Iaxis=2  Ndiv=30
      create and position MBAR     Ort=YZX  x=mbar_RetYRmn+60.0/2
EndBlock
*
Block MBAR is a single return yoke bar
      material  Iron    
      Attribute MBAR    seen=1  colo=3
      Shape     TRD1    dx1=mbar_BarWidin/2  dx2=mbar_BarWidou/2,
                        dz=mbar_BarHeigh/2   dy=mbar_RetYLen/2
EndBlock
*
Block MRGV is the Magnet Return rinG
      D = (mbar_RetYLen-mbar_coilLen)/2
      material  Iron    
      Attribute MRGV    seen=1  colo=6
      SHAPE     PCON    Phi1=0  dPhi=360 nz=4,
          zi ={ 0,        D,         D,   (magg_Length-mbar_coilLen)/2 },
          rmn={ mbar_RingRmn, mbar_RingRmn, mbar_RingRmn, mbar_RingRmn },
          rmx={ mbar_RetYRmn, mbar_RetYRmn, magg_Rmax,    magg_Rmax    };
EndBlock
* --------------------------------------------------------------------------
*                            endcap structure
* --------------------------------------------------------------------------
Block MPTV is the magnet pole-tip volume 
      Zcut     = mend_tcoilRmn*TanTheta-mend_PoleZ
      Rcorner  = magg_Length/TanTheta/2
      material  Iron
      Attribute MPTV    seen=1  colo=6
      SHAPE     PCON    Phi1=0  dPhi=360 nz=3,
                zi  = {  0,      Zcut,  magg_Length/2-mend_PoleZ    },
                rmn = { mend_TcoilRmn,  mend_TcoilRmn,   Rcorner    },
                rmx = { mend_PoleRmx,   mend_PoleRmx,  mend_PoleRmx };
      Create and Position MPCV   z=+mend_PoleCavD/2
EndBlock
*
Block MPCV is the coil cavity in the pole-tip (filled with cables ?)
      Material  Aluminium
      Attribute MPTV    seen=1  colo=7
      Shape     TUBE    Rmin=mend_tcoilRmn  Rmax=mend_PoleCavR,
                        dz=mend_PoleCavD/2
      Create and Position MTCL   z=+mend_tcoilDZ/2-mend_PoleCavD/2      
EndBlock
*
Block MTCL is  TRIM COIL Volume (filled with aluminum)
      Material  Aluminium
      Attribute MTCL    seen=1  colo=3
      SHAPE     TUBE    Rmin=mend_tcoilRmn Rmax=mend_tcoilRmx Dz=mend_tcoilDZ/2
EndBlock
* ----------------------------------------------------------------------------
End


