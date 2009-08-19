******************************************************************************
Module PIPEGEO is the geometry  of the STAR beam pipe.
  Created  30-03-99
  Author   W.B.Christie

* $Id: pipegeo.g,v 1.15 2009/08/19 22:47:24 perev Exp $
*
* $Log: pipegeo.g,v $
* Revision 1.15  2009/08/19 22:47:24  perev
* Jan: thinner beam pipe for upgr16
*
* Revision 1.14  2007/11/15 22:37:14  perev
* MLI=3MIL mylar + 1.5MIL alum defined
*
* Revision 1.13  2007/11/13 21:36:21  perev
* ALKAP fixed and lengths of shields swapped
*
* Revision 1.12  2006/10/06 22:33:42  potekhin
* Remove the pipe and svt shields that are unnecessary
* in the RandD "upgr" geometries. Done based on the config
* flag. Crude but effective.
*
* Revision 1.11  2005/07/14 22:11:22  potekhin
* Added a thinner version of the already customized pipe
* for the pixel detector, which will require an exoskeleton
* integrated with the detector (R&D request from Kai et al)
*
* Revision 1.10  2004/02/11 22:32:21  potekhin
* correct a typo in the standard pipe positioning, ThetaZ=180 was
* erroneously missing
*
* Revision 1.9  2004/01/22 00:26:48  potekhin
* Allow for optional positioning with MANY
*
* Revision 1.8  2004/01/19 22:51:36  potekhin
* Will need to rework the code, because the thinner
* mother volume for the pipe is clashing with the pixel
* detector -- for now just checking in the dimensions
*
* Revision 1.7  2003/12/29 17:51:59  potekhin
* Completely fill out the data struct for the new slim pipe
* (as opposed to relying on the prior default values),
* thus removing ambiguity and with it, an annoying bug.
*
* Revision 1.6  2003/12/17 22:18:09  potekhin
* a) Put in CVS tags so you can read this
* b) Added a control structure for encapsulated
* versioning off the steering file geometry.g
* c) Added the aluminum pipe config as as
* separate entry, as opposed to setting
* parameters directly from geometry.g
* d) Added a version with a slim central
* pipe section, so futher the studies related
* to the PIXL detector
*
******************************************************************************
+CDE,AGECOM,GCUNIT.
*
      Content  PIPE,PIPC,PIPO,PIPS,PIPB,PIPT,PFLO,PFLT,PVAC,PVAO,PVAS,
               PVAB,PRIS,PRID,PRIB,PIPI,PVAI,PVAT,PWRP,PSLD

      Structure PIPV {version,  int pipeConfig, int pipeFlag}
      Structure PIPG {config,
                      BeInnR,   BeOutR,   BeLeng,   char Material,
                      S1InnR,   S1OutR,   S1Leng,   S2InnR,   S2OutR,  S2Leng,
                      S3InnR,   S3OutR,   S3Leng,
                      S4InnR,   S4OutR,   S4Leng,
                      Flange1T, Flange1R, ConeLen, 
                      RibNum,   RibSpa,   RibThk,   RibOutR,  RibCent,
                      WrpInnR,  WrpOutR,  WrpLeng, 
                      SldInnR,  SldOutR,  SldLeng}

      Real    Z1,Z2,Z3,Z4,R1,R2,vacuum/1.e-5/ ! local variables for section positioning
      Real    WrpThk,SldThk
* -----------------------------------------------------------------------------
*
   WrpThk = (3.+1.5)*2*MIL_p
   SldThk = (3.+0.1)  *MIL_p
   FILL PIPV    !  Beam pipe version
      version    =  1    ! geometry version
      pipeConfig =  2    ! pipe version (2 is the default, unfortunately)
      pipeFlag   =  0   ! 1=PWRP !2=PSLD
   endFill

* note that in the following configs 1 and 2 have been historically swapped

   FILL PIPG    !  Beam Pipe data
      config   =  2     ! both material and geo params
      BeInnR    = 3.9   ! Berillium section inner radius
      BeOutR    = 4.0   ! Berillium section outer radius
      BeLeng    = 76.2  ! Berillium section half length
      material  ='ALUM' ! pipe main section material 
      S1InnR    = 3.875 ! first Aluminum section inner radius
      S1OutR    = 4.0   ! first Aluminum section outer radius
      S1Leng    = 153.4 ! first Aluminum section half length
      S2InnR    = 3.875 ! second Aluminum section inner radius
      S2OutR    = 4.00  ! second Aluminum section outer radius
      S2Leng    = 18.0  ! second Aluminum section half length
      S3InnR    = 3.875 ! Transition Stub Aluminum section inner radius
      S3OutR    = 4.0   ! Transition Stub Aluminum section outer radius
      S3Leng    = 1.0   ! Transition Stub Aluminum section half length
      S4InnR    = 6.20  ! Large OD Aluminum section inner radius
      S4OutR    = 6.35  ! Large OD Aluminum section outer radius
      S4Leng    = 150.0 ! Large OD Aluminum section half length
      ConeLen   = 12.5  ! half length of the Bell Reducer Cone
      Flange1T  = 2.0   ! flange SET half thickness
      Flange1R  = 5.85  ! flange outer radius
      RibNum    = 8     ! number of Ribs
      RibSpa    = 1.75  ! spacing between Ribs
      RibThk    = 0.05  ! Rib half thickness
      RibOutR   = 4.8   ! Rib Outer Radius
      RibCent   = 454.5 ! Rib Set center 
      WrpInnR   = 4.0   ! inner radius of beampipe multi layer insulation
      WrpOutR   = 4.0+WrpThk 		! outer radius of beampipe multi layer insulation
      WrpLeng   = 300   		! length of beampipe multi layer insulation
      SldInnR   = 4.0+WrpThk 		! inner radius of SVT beam pipe shield
      SldOutR   = 4.0+WrpThk+SldThk	! outer radius of SVT beam pipe shield
      SldLeng   = 56    		! length of SVT beam pipe shield

* a pretty uninteresting old config with pipes'o'steel:

   FILL PIPG    !  Beam Pipe data
      config   =  1     ! both material and geo params
      material  ='IRON' ! material is steel

   endfill

* some seriously obsolete config
   FILL PIPG    !  Beam Pipe data
      config   = 3      ! both material and geo params
      BeLeng   = 0      ! Berillium section half length
      S1Leng   = 230    ! first Aluminum section half length
   endfill

* The new pipe according to Kai
   FILL PIPG    !  Beam Pipe data
      config   =  4     ! both material and geo params
      BeInnR    = 1.374 ! Berillium section inner radius
      BeOutR    = 1.450 ! Berillium section outer radius
      material  ='ALUM' ! pipe main section material 

   endfill
*
* The thinner new pipe, which will require an exoskeleton (in pipgeo3)
   FILL PIPG    !  Beam Pipe data
      config   =  5     ! both material and geo params
      BeInnR    = 1.374 ! Berillium section inner radius
      BeOutR    = 1.424 ! Berillium section outer radius
   endfill
!//  upgr16.Jan 
!//  based on recent mail from Fleming (startup-hn) this is the answer:
!//    the new beam-pipe for HFT is supposed to be ID 2 cm and have a 
!//    thickness of 0.76 mm. 
!//  Jan correction:
!//   please use :  BeInnR=2.00cm, BeOutR=2.076cm .
!//Where R means Radius.
   FILL PIPG    !  Beam Pipe data
      config   =  6       ! both material and geo params
      BeInnR    = 2.0     ! Berillium section inner radius (Jan upgr16)
      BeOutR    = 2.076   ! Berillium section outer radius
   endfill
*
*
      USE      PIPV
      USE      PIPG config=PIPV_pipeConfig;

      prin1  pipg_material; (' beam pipe material - ',a4)
      write(*,*)  'Inner radius of the Beam Pipe: ', pipg_BeInnR
      write(*,*)  'Half length  of the Beam Pipe: ', pipg_BeLeng

* calculate positions of the 1st, 2nd, and 3rd breaks in the expected pipe.
      Z1 = pipg_BeLeng + 2*pipg_S1Leng + pipg_flange1t
      Z2 = Z1 - 2*pipg_flange1t + 2*pipg_s2leng
      Z3 = Z2 + pipg_flange1t +2*pipg_s3leng +2*pipg_conelen +2*pipg_s4leng

* Calculate the center position of the bell transition cone
      Z4 = Z2 + pipg_flange1t + 2*pipg_s3leng + pipg_conelen 

* calculate mother PCON radii
      R1 = pipg_SldOutR
      R2 = pipg_S4outR

      Create   PIPE
      if(PIPV_pipeConfig>=4) then ! customized positioning
         Position PIPE in CAVE             Konly='MANY'
         Position PIPE in CAVE  ThetaZ=180 Konly='MANY'
      else ! deault, no need to 'MANY'
         Position PIPE in CAVE
         Position PIPE in CAVE  ThetaZ=180
      endif
*
* -----------------------------------------------------------------------------
Block PIPE is the STAR beam pipe mother volume
      Material  Air
      Medium    Standard
      Attribute Pipe   Seen=1  colo=2
      SHAPE     PCON   phi1=0  Dphi=360  Nz=4,
          zi = { 0,   Z1-pipg_flange1t,   Z1-pipg_flange1t,            Z3 },
          Rmn= { 0,                  0,                  0,             0 },
          Rmx= { r1,                r1,                 r2,            r2 }

* debug: write(*,*) 'Z1-pipg_flange1t,',Z1-pipg_flange1t,'    Z3,',Z3,' r1,',r1,'  r2,',r2
*
*     select material for outer part
      if (pipg_material=='IRON') then    
          material Iron
      else
          material Aluminium
      endif
*                        call this material 'pipe'
      material pipe dens=ag_dens
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
      if(iand(PIPV_pipeFlag,1).ne.0) then
          Create and position PWRP z=pipg_WrpLeng/2          " beampipe wrap "
      endif
      if(iand(PIPV_pipeFlag,2).ne.0) then
          Create and position PSLD z=pipg_SldLeng/2          " svt beam shield "
      endif
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
       Material  Air
       Material  PVacuum   dens=ag_dens*Vacuum,
                           Radl=ag_RadL/Vacuum, AbsL=ag_AbsL/Vacuum
                           
       Shape     TUBE      Rmax=pipg_BeInnR  
EndBlock
* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

Block PIPO is Steel pipe from Be to 1st flanges
       Material  pipe
       Attribute Pipo      Seen=1  colo=3
       Shape     TUBE      Rmin=0  Rmax=pipg_S1OutR,
                           Dz=pipg_S1Leng
       Create and Position PVAO
EndBlock
*
Block PVAO is its cavity
       Material  Pvacuum
       Shape     TUBE      Rmax=pipg_S1InnR 
EndBlock

* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
Block PIPI is Steel pipe of the Bellow section
       Material  pipe
       Attribute Pipi      Seen=1  colo=3
       Shape     TUBE      Rmin=0  Rmax=pipg_S2OutR,
                           Dz=pipg_S2Leng
       Create and Position PVAI
EndBlock
*
Block PVAI is its cavity
       Material  Pvacuum
       Shape     TUBE      Rmax=pipg_S2InnR 
EndBlock
* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
Block PIPT is short Steel pipe of the transition section
       Material  Pipe
       Attribute Pipt      Seen=1  colo=3
       Shape     TUBE      Rmin=0  Rmax=pipg_S3OutR,
                           Dz=pipg_S3Leng
       Create and Position PVAT
EndBlock
*
Block PVAT is its cavity
       Material  Pvacuum
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
       Material  Pvacuum
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
       Material  Pvacuum
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
* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Block PWRP is the beampipe wrap of Kapton and aluminum 
* Special mylar mixture 3MIL of mylar + 1.5MIL alum
       Component C5     A=12    Z=6  W=5
       Component H4     A=1     Z=1  W=4
       Component O2     A=16    Z=8  W=2
       Component Al     A=27    Z=13 W=3.45327
       Mixture   MLI    Dens=1.82667
       Attribute Pwrp      Seen=1  colo=3
       Shape     TUBE      Rmin=pipg_WrpInnR  Rmax=pipg_WrpOutR,
                           Dz=pipg_WrpLeng/2 
EndBlock
* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Block PSLD is the svt beampipe shield 
* use mylar mixture from svttgeo instead of kapton
       Component C5     A=12    Z=6  W=5
       Component H4     A=1     Z=1  W=4
       Component O2     A=16    Z=8  W=2
       Component Al     A=27    Z=13 W=0.2302
       Mixture   ALKAP  Dens=1.432
       Attribute PSLD   Seen=1  colo=3
       Shape     TUBE   Rmin=pipg_SldInnR  Rmax=pipg_SldOutR,
                        Dz=pipg_SldLeng/2 
EndBlock
*
      END

