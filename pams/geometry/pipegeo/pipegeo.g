******************************************************************************
Module PIPEGEO is the geometry  of the STAR beam pipe.
  Created  16-Sept-1996 
  Author   W.B.Christie
******************************************************************************
+CDE,AGECOM,GCUNIT.
*
      Content  PIPE,PIPC,PIPO,PIPS,PIPB,PIPT,PFLO,PFLT,PVAC,PVAO,PVAS,
               PVAB,PRIS,PRID,PRIB,PIPI,PVAI,PVAT
*
      Structure PIPG {version,  BeInnR,   BeOutR,   BeLeng,
                      S1InnR,   S1OutR,   S1Leng,   S2InnR,   S2OutR,  S2Leng,
                      S3InnR,   S3OutR,   S3Leng,
                      S4InnR,   S4OutR,   S4Leng,
                      Flange1T, Flange1R, ConeLen, 
                      RibNum,   RibSpa,   RibThk,   RibOutR,  RibCent }
*
*    local variable for section positioning
      Real    Z1,Z2,Z3,Z4,R1,R2
*
* -----------------------------------------------------------------------------
*
   FILL PIPG    !  Beam Pipe data
      version   =  1    ! geometry version     
      BeInnR    = 3.9   ! Berillium section inner radius
      BeOutR    = 4.0   ! Berillium section outer radius
      BeLeng    = 76.2  ! Berillium section half length
      S1InnR    = 3.85  ! first steel section inner radius
      S1OutR    = 4.0   ! first steel section outer radius
      S1Leng    = 153.4 ! first steel section half length
      S2InnR    = 3.85  ! second steel section inner radius
      S2OutR    = 4.00  ! second steel section outer radius
      S2Leng    = 18.0  ! second steel section half length
      S3InnR    = 3.85  ! Transition Stub steel section inner radius
      S3OutR    = 4.0   ! Transition Stub steel section outer radius
      S3Leng    = 1.0   ! Transition Stub steel section half length
      S4InnR    = 6.20  ! Large OD steel section inner radius
      S4OutR    = 6.35  ! Large OD steel section outer radius
      S4Leng    = 150.0 ! Large OD steel section half length
      ConeLen   = 12.5  ! half length of the Bell Reducer Cone
      Flange1T  = 2.0   ! flange SET half thickness
      Flange1R  = 5.85  ! flange outer radius
      RibNum    = 8     ! number of Ribs
      RibSpa    = 1.75  ! spacing between Ribs
      RibThk    = 0.05  ! Rib half thickness
      RibOutR   = 4.8   ! Rib Outer Radius
      RibCent   = 454.5 ! Rib Set center 
   endfill
*
      USE      PIPG  Version=1
* calculate positions of the 1st, 2nd, and 3rd breaks in the expected pipe.
      Z1 = pipg_BeLeng + 2*pipg_S1Leng + pipg_flange1t
      Z2 = Z1 - 2*pipg_flange1t + 2*pipg_s2leng
      Z3 = Z2 + pipg_flange1t +2*pipg_s3leng +2*pipg_conelen +2*pipg_s4leng
* Calculate the center position of the bell transition cone
      Z4 = Z2 + pipg_flange1t + 2*pipg_s3leng + pipg_conelen 
* calculate mother PCON radii
      R1 = pipg_S2OutR
      R2 = pipg_s3outr
*
      Create   PIPE
      Position PIPE in CAVE 
      Position PIPE in CAVE  ThetaZ=180
*
* -----------------------------------------------------------------------------
Block PIPE is the STAR cave 
      Material  Air
      Medium    Standard
      Attribute Pipe   Seen=0  colo=1
      SHAPE     PCON   phi1=0  Dphi=360  Nz=4,
          zi = { 0,        Z1-pipg_flange1t,     Z1-pipg_flange1t ,       Z3 },
          Rmn= { 0,                  0,                  0,                0 },
          Rmx= { r1,                r1,                 r2,               r2 }
*
      Create and Position PIPC z=pipg_BeLeng/2           " center Be section  "
      Create and Position PIPO z=pipg_beleng+pipg_S1Leng " 8 cm steel section "
      Create and Position PIPI z=(Z1+Z2)/2               " Bellows steel pipe "
      Create and Position PIPT z=Z2+pipg_flange1t+pipg_s3leng  "Short SS pipe "
      Create and Position PIPB z=Z4                      " Bell reducer cone  "
      Create and Position PFLO z=Z1                      " 1st set of flanges "
      Create and Position PFLT z=Z2                      " 2nd set of flanges "
      Create and Position PIPS z=Z3-pipg_S4Leng          " 5 inch steel sectn "
      Create and position PRIS z=(Z1+Z2)/2               " Steel Bellow Ribs  "
endblock
* -----------------------------------------------------------------------------

Block PIPC is the Central Beam PIPe Volume
       Material  Berillium
       Attribute Pipc      Seen=1  colo=2
       Shape     TUBE      Rmin=0  Rmax=pipg_BeOutR,
                           Dz=pipg_BeLeng/2 
       Create and Position PVAC
EndBlock
*
Block PVAC is the Vacuum Volume of Be section of pipe
       Material  Vacuum
       Shape     TUBE      Rmax=pipg_BeInnR  
EndBlock
* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

Block PIPO is Steel pipe from Be to 1st flanges
       Material  Iron
       Attribute Pipo      Seen=1  colo=3
       Shape     TUBE      Rmin=0  Rmax=pipg_S1OutR,
                           Dz=pipg_S1Leng
       Create and Position PVAO
EndBlock
*
Block PVAO is its cavity
       Material  Vacuum
       Shape     TUBE      Rmax=pipg_S1InnR 
EndBlock

* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
Block PIPI is Steel pipe of the Bellow section
       Material  Iron
       Attribute Pipi      Seen=1  colo=3
       Shape     TUBE      Rmin=0  Rmax=pipg_S2OutR,
                           Dz=pipg_S2Leng
       Create and Position PVAI
EndBlock
*
Block PVAI is its cavity
       Material  Vacuum
       Shape     TUBE      Rmax=pipg_S2InnR 
EndBlock
* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
Block PIPT is short Steel pipe of the transition section
       Material  Iron
       Attribute Pipt      Seen=1  colo=3
       Shape     TUBE      Rmin=0  Rmax=pipg_S3OutR,
                           Dz=pipg_S3Leng
       Create and Position PVAT
EndBlock
*
Block PVAT is its cavity
       Material  Vacuum
       Shape     TUBE      Rmax=pipg_S3InnR 
EndBlock
* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
Block PIPB is the beam pipe Bell reducing section 
       Material  Iron
       Attribute Pipb      Seen=1  colo=3
       Shape     CONE      rmn1=0  rmx1=pipg_S2OutR,
                           rmn2=0  rmx2=pipg_S4OutR,
                           dz=pipg_ConeLen
       Create and Position PVAB  
EndBlock
*
Block PVAB is its cavity
       Material  Vacuum
       Shape     CONE      rmx1=pipg_S3InnR,
                           rmx2=pipg_S4InnR
EndBlock
* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

Block PIPS 5 inch OD steel beam pipe starting ~4.5 m from IR
       Material  Iron
       Attribute Pips      Seen=1  colo=3
       Shape     TUBE      Rmin=0  Rmax=pipg_S4OutR,
                           Dz=pipg_S4Leng
       Create and Position PVAS 
EndBlock
*
Block PVAS is its cavity
       Material  Vacuum
       Shape     TUBE      Rmax=pipg_S4InnR
EndBlock
* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

Block PFLO is the 1st set of flanges at ~3.9 m from IR 
       Material  Iron
       Attribute Pflo      Seen=1  colo=4
       Shape     TUBE      Rmin=pipg_S2OutR  Rmax=pipg_Flange1R,
                           Dz=pipg_Flange1T
EndBlock
*
Block PFLT is the 2nd set of flanges at ~4.2 m from IR 
       Material  Iron
       Attribute Pflt      Seen=1  colo=4
       Shape     TUBE      Rmin=pipg_S2OutR  Rmax=pipg_Flange1R,
                           Dz=pipg_Flange1T
EndBlock
*
* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
Block PRIS is the Bellow Steel Rib Set
       Material  Air
       Attribute PRIB      Seen=0   Colo=2
       Shape     TUBE      Rmin=pipg_S2OutR  Rmax=pipg_RibOutR, 
                           Dz=pipg_RibNum*pipg_RibSpa/2.
       create and position PRID 
EndBlock
*
Block PRID is a Rib section
       Shape     division  Iaxis=3  Ndiv=pipg_RibNum
       Create and position PRIB
EndBlock
*
Block PRIB is a Rib of Steel Bellows 
       Material  Iron
       Attribute PRIB      Seen=1   Colo=7
       Shape     TUBE      Dz=pipg_RibThk 
EndBlock
*
      END

