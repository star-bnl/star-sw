******************************************************************************
Module UPSTGEO is the geometry  of the UPSTREAM AreA.
  Created  27-Dec-1996 
  Author   W.B.Christie
* Updated Feb-10-1997, Changed length of Pipe in DX and added cone.
******************************************************************************
+CDE,AGECOM,GCUNIT.
*
      Content  UPST,PIPD,PIPE,PIPF,DXMG,PVAD,PVAE,DCON,PIPG,DVAC,PVAG
*
      Structure PIPU {Version, DZ_upst, P1InnR,   P1OutR,   P1Leng,
                      P2InnR, P2OutR, P2Leng,
                      P3InnR, P3OutR, P3Leng,
                      DXInnR,   DXOutR, DXLeng,
                      CSInnR, CSOutR, CEInnR, CEOutR, CLeng,
                      PGInnR, PGOutR, PGLeng }
*
*    local variable for section positioning
      Real    Z1,Z2,Z3,Z4,z5,z6
*
* -----------------------------------------------------------------------------
*
   FILL PIPU    !  Beam Pipe data
      version   = 1      ! geometry version  
      DZ_upst   = 385.63 ! Half length of the UPSTREAM mother volume
      P1InnR    = 6.08   ! Inner radius of Pipe in Hall
      P1OutR    = 6.35   ! Outer radius of pipe in Hall
      P1Leng    = 105.5  ! Length of Pipe in hall
      P2InnR    = 6.99   ! Large OD steel section inner radius
      P2OutR    = 7.14   ! Large OD steel section outer radius
      P2Leng    = 207.92 ! Large OD steel section half length
      P3InnR    = 9.53   ! Inner radius of large DX Pipe
      P3OutR    = 10.16  ! Outer radius of Large DX pipe
      P3Leng    = 207.92 ! Length of Large DX Pipe
      DXInnR    = 15.34  ! Inner RAdius of DX Iron Yoke.
      DXOutR    = 37.0   ! Outer RAdius of DX Iron Yoke. 
      DXLeng    = 207.92 ! HALF Length of DX MAgnet.
      CSInnR    = 7.14   ! Inner radius of Start of DX Cone
      CSOutR    = 7.77   ! Outer radius of Start of DX cone
      CEInnR    = 14.60  ! Inner radius at End of DX cone
      CEOutR    = 15.24  ! Outer radius at End of DX cone
      CLeng     = 21.21  ! Half Length of Cone at end of DX.
      PGInnR    = 14.60  ! Inner radius of last Pipe
      PGOutR    = 15.24  ! Outer radius of last Pipe
      PGLeng    = 51.0   ! Half length of last pipe
   endfill
*
      USE      PIPU  Version=1
* calculate positions of the 1st, 2nd, and 3rd breaks in the expected pipe.
      Z1 = 746.2      ! End of the std STAR beAm pipe plus 2 mm
      Z2 = pipu_p1leng - pipu_dz_upst     ! Center of beam pipe before DX
      Z3 = z2 + pipu_p1leng+ pipu_p2leng  ! Center of DX magnet (33.29)
      Z4 = z3 + pipu_p2leng               ! End of DX magnet    (241.21)
      Z5 = z4 + pipu_cleng                ! Center of DX Cone   (262.42)
      Z6 = z5 + pipu_cleng                ! End of DX Cone      (283.63)
*
      Create   UPST
      Position UPST in CAVE z=1131.83    ! Leaves a 2 mm gap after pipegeo
      Position UPST in CAVE z=-1131.83 ThetaZ=180
*
* -----------------------------------------------------------------------------
Block UPST is the upstream mother volume in the STAR cave 
      Material  Air
      Medium    Standard
      Attribute Upst   Seen=0  colo=2
      SHAPE     TUBE   Rmin=0. Rmax=40.0 Dz=pipu_dz_upst
*
      Create and Position PIPD z=z2               " Center of pipe before DX"
      Create and Position PIPE z=z3               " center of DX Pipe  "
      Create and Position PIPF z=z3               " center of DX outer pipe"
      Create and Position DXMG z=z3               " Center of DX Yoke  "
      Create and Position DCON z=z4+pipu_cleng    " Center of DX Cone"
      Create and Position PIPG z=z6+pipu_pgleng   "Center of last Pipe "
endblock
* -----------------------------------------------------------------------------
Block PIPD is the Beam PIPe before the DX magnet
       Material  Iron
       Attribute Pipd      Seen=1  colo=1
       Shape     TUBE      Rmin=0  Rmax=pipu_p1OutR,
                           Dz=pipu_p1Leng
       Create and Position PVAD
EndBlock
*
Block PVAD is the Vacuum Volume of the pipe before the DX magnet
       Material  Vacuum
       Shape     TUBE      Rmax=pipu_p1InnR  
EndBlock
* -----------------------------------------------------------------------------
Block PIPE is the Beam PIPe through the DX mAgnet Volume
       Material  Iron
       Attribute Pipe      Seen=1  colo=1
       Shape     TUBE      Rmin=0  Rmax=pipu_p2OutR,
                           Dz=pipu_p2Leng
       Create and Position PVAE
EndBlock
*
Block PVAE is the Vacuum Volume of DX mAgnet pipe
       Material  Vacuum
       Shape     TUBE      Rmax=pipu_p2InnR  
EndBlock
* -----------------------------------------------------------------------------
Block PIPF is the Outer PIPe through the DX mAgnet Volume
       Material  Iron
       Attribute Pipf      Seen=1  colo=2
       Shape     TUBE      Rmin=pipu_p3InnR  Rmax=pipu_p3OutR,
                           Dz=pipu_p3Leng
EndBlock
*
* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
Block DXMG is the return yoke for the DX mAgnet
       Material  Iron
       Attribute Dxmg      Seen=1  colo=3
       Shape     TUBE      Rmin=pipu_dxinnr  Rmax=pipu_DxOutR,
                           Dz=pipu_DxLeng
EndBlock
* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
Block DCON is the beam pipe Bell section at the end of DX 
       Material  Iron
       Attribute Dcon      Seen=1  colo=4
       Shape     CONE      rmn1=0  rmx1=pipu_csoutr,
                           rmn2=0  rmx2=pipu_ceoutr,
                           dz=pipu_CLeng
       Create and Position DVAC  
EndBlock
*
Block DVAC is its cavity
       Material  Vacuum
       Shape     CONE      rmx1=pipu_csinnr,
                           rmx2=pipu_ceinnr
EndBlock
* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
Block PIPG is the Beam PIPe After the DX magnet Volume
       Material  Iron
       Attribute Pipg      Seen=1  colo=4
       Shape     TUBE      Rmin=0  Rmax=pipu_pgoutr,
                           Dz=pipu_pgLeng
       Create and Position PVAG
EndBlock
*
Block PVAG is the Vacuum Volume of the pipe after the DX magnet
       Material  Vacuum
       Shape     TUBE      Rmax=pipu_pgInnR  
EndBlock
* -----------------------------------------------------------------------------
     END










