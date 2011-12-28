* $Id: quadgeo.g,v 1.3 2011/02/28 16:27:40 jwebb Exp $
*
* $Log: quadgeo.g,v $
* Revision 1.3  2011/02/28 16:27:40  jwebb
* Cosmetic change needed for AgML syntax matching.
*
* Revision 1.2  2006/01/18 23:08:52  potekhin
* Checking in additional work on the quad model, now temporarily
* frozen, which we need for our beam background studies
*
* Revision 1.1  2005/09/02 18:22:15  potekhin
* Need a new area for the description of the upstream
* beam magnets
*
******************************************************************************
Module QUADGEO is the description of all the magnets upstream inclusive of D0
  Created  02-Sep-2005 
  Author   Maxim Potekhin
*
******************************************************************************
+CDE,AGECOM,GCUNIT.
*
      Content   MGMT, DZER,QONE, QTWO, QTHR
*
      Structure SHLQ { Version,
                       Q0,    MotherR,MotherL,Xoffset,Angle,
                       DzeroL,DzeroRi,DzeroRo,
                       Q1,        ri1,    ro1, dz1,
                       Q2,        ri2,    ro2, dz2,
                       Q3,        ri3,    ro3, dz3 }

*
*    local variable for section positioning

     Real zQuad

*
* -----------------------------------------------------------------------------
   Fill SHLQ    !  Quadrupole Geometry Data
      Version   =   1     ! geometry version  
      Q0        = 2485.26 ! offset point that corresponds to 1505.92 in CAD notation, end of D0
      MotherR   = 19      ! radius of the mother containing D0,Q1,Q2,Q3
      MotherL   = 1610    ! length of the mother containing D0,Q1,Q2,Q3

      Xoffset   = 26      ! Offset ot the mother
      Angle     = 0.3     ! Angle to the symmetry axis

      DzeroL    = 385.26  ! D0 length
      DzeroRi   =  4.775  ! D0 inner radius
      DzeroRo   = 15.995  ! D0 outer radius
*
      Q1        = 88.59   ! offset 1
      ri1       =  6.355  ! inner 1
      ro1       =  18.28  ! outer 1
      dz1       = 170.92  ! full length 1
*
      Q2        = 324.90  ! offset 2
      ri2       =  6.355  ! inner 2
      ro2       =  18.28  ! outer 2
      dz2       = 444.02  ! full length 2
*
      Q3        = 823.94  ! offset 3
      ri3       =  6.355  ! inner 3
      ro3       =  18.28  ! outer 3
      dz3       = 399.55  ! full length 3
   EndFill

* = = = = = = = = = = = =

      USE      SHLQ

      zQuad = shlq_q0-shlq_DzeroL+shlq_MotherL/2.0

      Create   MGMT
      Position MGMT in CAVE x= shlq_Xoffset y=0 z=zQuad  AlphaY= shlq_Angle
      Position MGMT in CAVE x=-shlq_Xoffset y=0 z=zQUad  AlphaY=-shlq_Angle

      Position MGMT in CAVE x= shlq_Xoffset y=0 z=-zQuad AlphaY= 180-shlq_Angle
      Position MGMT in CAVE x=-shlq_Xoffset y=0 z=-zQuad AlphaY= 180+shlq_Angle


*
* -----------------------------------------------------------------------------
Block MGMT is the magnet mother
      Material  Air
      Attribute MGMT   Seen=1  colo=3

      Shape TUBE Rmin=0.0 Rmax=shlq_MotherR dZ=shlq_MotherL/2.0

      Create DZER
      Position DZER in MGMT z=-shlq_MotherL/2.0+shlq_DzeroL/2.0

      Create QONE
      Position QONE in MGMT z=-shlq_MotherL/2.0+(shlq_q1+shlq_DzeroL)+shlq_dz1/2.0

      Create QTWO
      Position QTWO in MGMT z=-shlq_MotherL/2.0+(shlq_q2+shlq_DzeroL)+shlq_dz2/2.0

      Create QTHR
      Position QTHR in MGMT z=-shlq_MotherL/2.0+(shlq_q3+shlq_DzeroL)+shlq_dz3/2.0

EndBlock
* -----------------------------------------------------------------------------
Block DZER is the D0 yoke
      Material  Iron
      Attribute DZER   Seen=1  colo=1

      Shape TUBE Rmin=shlq_DzeroRi Rmax=shlq_DzeroRo dZ=shlq_DzeroL/2.0
EndBlock
* -----------------------------------------------------------------------------
Block QONE is the Q1 yoke
      Material  Iron
      Attribute QONE   Seen=1  colo=1

      Shape TUBE Rmin=shlq_ri1 Rmax=shlq_ro1 dZ=shlq_dz1/2.0
EndBlock
* -----------------------------------------------------------------------------
Block QTWO is the Q2 yoke
      Material  Iron
      Attribute QTWO   Seen=1  colo=1

      Shape TUBE Rmin=shlq_ri2 Rmax=shlq_ro2 dZ=shlq_dz2/2.0
EndBlock
* -----------------------------------------------------------------------------
Block QTHR is the Q3 yoke
      Material  Iron
      Attribute QTHR   Seen=1  colo=1

      Shape TUBE Rmin=shlq_ri3 Rmax=shlq_ro3 dZ=shlq_dz3/2.0
EndBlock

* -----------------------------------------------------------------------------
     END
*
*

