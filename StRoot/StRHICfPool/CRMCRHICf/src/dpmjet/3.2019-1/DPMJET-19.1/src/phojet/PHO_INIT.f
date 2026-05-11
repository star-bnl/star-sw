
      SUBROUTINE PHO_INIT(Linp,Lout,Irej)
C*
C***********************************************************************
C
C     main subroutine to configure and manage PHOJET calculations
C
C     input:  LINP       input unit to read from
C                        -1 to skip reading of input file
C             LOUT       output unit to write to
C
C     output: IREJ       0  success
C                        1  failure
C
C***********************************************************************
      IMPLICIT NONE
      DOUBLE PRECISION a , aeb , dum , e1 , e2 , ee , ee1 , ee2 , ep , 
     &                 parnew , phi , PHO_PMASS , plab , 
     &                 pl_lam_1 , pl_lam_2 , pmass1 , pmass2 , pvir
      DOUBLE PRECISION pvirt2 , rho , scale2 , sigx , sigy , sigz , 
     &                 sig_gg , sig_tot , theta , xsub , x_1 , x_2 , 
     &                 ypsi
      INTEGER i , icut , id , idebf , idebn , idlev , idpdg , idum , 
     &        iext , ifl1 , ifl2 , impro , ion , ip , ipar , Irej , 
     &        iset , itmp , ival , kc
      INTEGER kk , Linp , Lout , na , nev , nz
      SAVE 
 
C  input/output channels
      INCLUDE 'inc/poinou'
C  event debugging information
      INCLUDE 'inc/podebg'
C  model switches and parameters
      INCLUDE 'inc/pomdls'
C  current beam selection
      INCLUDE 'inc/pobeam'
C  general process information
      INCLUDE 'inc/poprcs'
C  global event kinematics and particle IDs
      INCLUDE 'inc/pogcms'
C  nucleon-nucleus / nucleus-nucleus interface to DPMJET
      INCLUDE 'inc/pohdfl'
C  integration precision for hard cross sections (obsolete)
      INCLUDE 'inc/pogaup'
C  some hadron information, will be deleted in future versions
      INCLUDE 'inc/pohdrn'
C  obsolete cut-off information
      INCLUDE 'inc/pocut1'
C  photon flux kinematics and cuts
      INCLUDE 'inc/pofcut'
C  cut probability distribution
#ifndef FOR_CORSIKA
      INCLUDE 'inc/poprob'
#else
      INCLUDE 'inc/poprob50'
#endif
C  event weights and generated cross section
      INCLUDE 'inc/powght'
C  names of hard scattering processes
      INCLUDE 'inc/pohpro'
C  hard cross sections and MC selection weights
      INCLUDE 'inc/pohrcs'
 
      INCLUDE 'inc/pydat1'
 
      INCLUDE 'inc/pydat2'
 
      INCLUDE 'inc/pydat3'
 
Cf2py intent(out) irej
 
      INTEGER PYCOMP
      EXTERNAL PYCOMP, PHO_PMASS


      DIMENSION itmp(0:11)
      CHARACTER*10 cname
C     CHARACTER*70 NUMBER,FILENA
      CHARACTER*70 number
C15   FORMAT(A12)
 
C  define input/output units
      IF ( Linp.GE.0 ) THEN
         LPRi = 10
         CALL DT_RNDMST(22,54,76,92)
         LI = Linp
#ifndef FOR_FLUKA
Cinitialize random number generator in standalone (-2) mode
      ELSE IF ( Linp.EQ.-2 ) THEN
         CALL DT_RNDMST(22,54,76,92)
        ! CALL DT_RNDMTE(1)
         LPRi = 10
#endif
      ELSE
         LPRi = Irej
         LI = 5
      END IF
 
      LO = Lout
      Irej = 0
 
      IF ( LPRi.GT.4 ) WRITE (LO,*)
      IF ( LPRi.GT.4 ) WRITE (LO,*) 
     &            ' ==================================================='
      IF ( LPRi.GT.4 ) WRITE (LO,*) 
     &            '                                                    '
      IF ( LPRi.GT.4 ) WRITE (LO,*) 
     &            '   ----        PHOJET  19.1.0       ----   '
      IF ( LPRi.GT.4 ) WRITE (LO,*) 
     &            '                                                    '
      IF ( LPRi.GT.4 ) WRITE (LO,*) 
     &            ' ==================================================='
      IF ( LPRi.GT.4 ) WRITE (LO,*)
     &                         '     Authors: Ralph Engel         (KIT)'
      IF ( LPRi.GT.4 ) WRITE (LO,*) 
     &                        '              Anatoli Fedynitch   (ICRR)'
      IF ( LPRi.GT.4 ) WRITE (LO,*) 
     &                '              Johannes Ranft      (Siegen Univ.)'
      IF ( LPRi.GT.4 ) WRITE (LO,*) 
     &                        '              Stefan Roesler      (CERN)'
      IF ( LPRi.GT.4 ) WRITE (LO,*) 
     &            ' ---------------------------------------------------'
      IF ( LPRi.GT.4 ) WRITE (LO,*) 
     &                    '   bug reports, support and updates on:'
      IF ( LPRi.GT.4 ) WRITE (LO,*)
     &                    '     https://github.com/afedynitch/dpmjet'
      IF ( LPRi.GT.4 ) WRITE (LO,*) 
     &            ' ==================================================='
      IF ( LPRi.GT.4 ) WRITE (LO,*) '   Date: 2019/07/09'
      IF ( LPRi.GT.4 ) WRITE (LO,*) '   Revision: 19.1.0'
 
      IF ( LPRi.GT.4 ) WRITE (LO,*)
     &                         '   Code with interface to PYTHIA 6.4.27'
 
      IF ( LPRi.GT.4 ) WRITE (LO,*) 
     &            ' ==================================================='
      IF ( LPRi.GT.4 ) WRITE (LO,*)
 
C  standard initializations
      CALL PHO_DATINI
      CALL PHO_PARDAT
      IPRoce = 0
      dum = PHO_PMASS(0,-1)
 
C  multiparticle mapping tables (default selection)
      CALL PHO_INITPMAP(-1,0,0,0)
 
C  initialize standard PDFs
C  proton
      CALL PHO_SETPDF(2212,idum,2,1,0,0,-1)
      CALL PHO_SETPDF(-2212,idum,2,1,0,0,-1)
C  neutron
      CALL PHO_SETPDF(2112,idum,2,1,0,0,-1)
      CALL PHO_SETPDF(-2112,idum,2,1,0,0,-1)
C  lambda
      CALL PHO_SETPDF(3122,idum,2,1,0,0,-1)
      CALL PHO_SETPDF(-3122,idum,2,1,0,0,-1)
C  lambda
      CALL PHO_SETPDF(3122,idum,2,1,0,0,-1)
      CALL PHO_SETPDF(-3122,idum,2,1,0,0,-1)
C  sigmamp
      CALL PHO_SETPDF(3112,idum,2,1,0,0,-1)
      CALL PHO_SETPDF(-3112,idum,2,1,0,0,-1)
C  photon
      CALL PHO_SETPDF(22,idum,5,3,0,0,-1)
C  pomeron
      CALL PHO_SETPDF(990,idum,4,0,0,0,-1)
C  pions
      CALL PHO_SETPDF(211,idum,5,2,0,0,-1)
      CALL PHO_SETPDF(-211,idum,5,2,0,0,-1)
      CALL PHO_SETPDF(111,idum,5,2,0,0,-1)
C  kaons
      CALL PHO_SETPDF(321,idum,5,2,0,0,-1)
      CALL PHO_SETPDF(-321,idum,5,2,0,0,-1)
      CALL PHO_SETPDF(311,idum,5,2,0,0,-1)
      CALL PHO_SETPDF(-311,idum,5,2,0,0,-1)
      CALL PHO_SETPDF(130,idum,5,2,0,0,-1)
      CALL PHO_SETPDF(310,idum,5,2,0,0,-1)
 
C  nothing to be done
      IF ( Linp.LT.0 ) RETURN
 
C  main loop to read input cards
 100  READ (Linp,99010,END=200) cname , number
 
99010 FORMAT (A10,A69)
      IF ( cname.NE.'ENDINPUT  ' ) THEN
         IF ( cname.EQ.'STOP      ' ) THEN
            IF ( LPRi.GT.4 ) WRITE (LO,*) 'STOP'
            STOP
         ELSE IF ( cname.EQ.'COMMENT   ' ) THEN
            IF ( LPRi.GT.4 ) WRITE (LO,'(1X,A10,A69)') 'COMMENT   ' , 
     &           number
         ELSE IF ( cname(1:1).EQ.'*' ) THEN
            IF ( LPRi.GT.4 ) WRITE (LO,'(1X,A10,A69)') cname , number
         ELSE IF ( cname.EQ.'PTCUT     ' ) THEN
            READ (number,*) PARmdl(36) , PARmdl(37) , PARmdl(38) , 
     &                      PARmdl(39)
            IF ( LPRi.GT.4 ) WRITE (LO,*) 'PTCUT     ' , PARmdl(36) , 
     &           PARmdl(37) , PARmdl(38) , PARmdl(39)
         ELSE IF ( cname.EQ.'PROCESS   ' ) THEN
            READ (number,*) (IPRon(kk,1),kk=1,8)
            IF ( LPRi.GT.4 ) WRITE (LO,*) 'PROCESS   ' , 
     &           (IPRon(kk,1),kk=1,8)
         ELSE IF ( cname.EQ.'DIFF-PROC ' ) THEN
            READ (number,*) (itmp(kk),kk=0,11)
            IF ( LPRi.GT.4 ) WRITE (LO,*) 'DIFF-PROC ' , 
     &           (itmp(kk),kk=0,8)
            DO kk = 1 , 8
               IPRon(kk,itmp(0)) = itmp(kk)
            END DO
         ELSE IF ( cname.EQ.'SUBPROCESS' ) THEN
            READ (number,*) impro , ip , ion
            IF ( LPRi.GT.4 ) WRITE (LO,*) 'SUBPROCESS' , impro , ip , 
     &           ion
            MH_pro_on(impro,ip,IDXmpar) = ion
         ELSE IF ( cname.EQ.'PARTICLE1 ' ) THEN
            READ (number,*) idpdg , pvir
            IHFls(1) = 1
            XPSub = 1.D0
            CALL PHO_SETPAR(1,idpdg,0,pvir)
            IF ( LPRi.GT.4 ) WRITE (LO,*) 'PARTICLE1  ' , idpdg , pvir
         ELSE IF ( cname.EQ.'PARTICLE2 ' ) THEN
            READ (number,*) idpdg , pvir
            IHFls(2) = 1
            XTSub = 1.D0
            CALL PHO_SETPAR(2,idpdg,0,pvir)
            IF ( LPRi.GT.4 ) WRITE (LO,*) 'PARTICLE2  ' , idpdg , pvir
         ELSE IF ( cname.EQ.'REMNANT1  ' ) THEN
            READ (number,*) idpdg , ifl1 , ifl2 , ival , xsub
            IHFls(1) = ival
            IHFld(1,1) = ifl1
            IHFld(1,2) = ifl2
            XPSub = xsub
            pvir = 0.D0
            CALL PHO_SETPAR(1,idpdg,-1,pvir)
            IF ( LPRi.GT.4 ) WRITE (LO,*) 'REMNANT1   ' , idpdg , ifl1 , 
     &           ifl2 , ival , xsub
         ELSE IF ( cname.EQ.'REMNANT2  ' ) THEN
            READ (number,*) idpdg , ifl1 , ifl2 , ival , xsub
            IHFls(2) = ival
            IHFld(2,1) = ifl1
            IHFld(2,2) = ifl2
            XTSub = xsub
            pvir = 0.D0
            CALL PHO_SETPAR(2,idpdg,-1,pvir)
            IF ( LPRi.GT.4 ) WRITE (LO,*) 'REMNANT2   ' , idpdg , ifl1 , 
     &           ifl2 , ival , xsub
         ELSE IF ( cname.EQ.'PDF       ' ) THEN
            READ (number,*) idpdg , ipar , iset , iext
            IF ( LPRi.GT.4 ) WRITE (LO,*) 'PDF        ' , idpdg , ipar , 
     &           iset , iext
            CALL PHO_SETPDF(idpdg,idum,ipar,iset,iext,0,-1)
         ELSE IF ( cname.EQ.'SETMODEL  ' ) THEN
            READ (number,*) i , ival
            IF ( LPRi.GT.4 ) WRITE (LO,*) 'SETMODEL   ' , i , ival
            CALL PHO_SETMDL(i,ival,1)
         ELSE IF ( cname.EQ.'SETPARAM  ' ) THEN
            READ (number,*) i , parnew
            IF ( LPRi.GT.4 ) WRITE (LO,*) 'SETPARAM   ' , i , parnew
            PARmdl(i) = parnew
         ELSE IF ( cname.EQ.'DEBUG     ' ) THEN
            READ (number,*) idebf , idebn , idlev
            IF ( LPRi.GT.4 ) WRITE (LO,*) 'DEBUG      ' , idebf , 
     &           idebn , idlev
            CALL PHO_TRACE(idebf,idebn,idlev)
         ELSE IF ( cname.EQ.'TRACE     ' ) THEN
            READ (number,*) idebf , idlev
            IF ( LPRi.GT.4 ) WRITE (LO,*) 'TRACE      ' , idebf , idlev
            IDEb(idebf) = idlev
         ELSE IF ( cname.EQ.'SETICUT   ' ) THEN
            READ (number,*) i , icut
            IF ( LPRi.GT.4 ) WRITE (LO,*) 'SETICUT    ' , i , icut
            ISWcut(i) = icut
         ELSE IF ( cname.EQ.'SETFCUT   ' ) THEN
            READ (number,*) i , parnew
            IF ( LPRi.GT.4 ) WRITE (LO,*) 'SETFCUT    ' , i , parnew
            HSWcut(i) = parnew
         ELSE IF ( cname.EQ.'LUND-MSTU ' ) THEN
            READ (number,*) i , ival
            IF ( LPRi.GT.4 ) WRITE (LO,*) 'LUND-MSTU  ' , i , ival
            MSTu(i) = ival
         ELSE IF ( cname.EQ.'LUND-MSTJ ' ) THEN
            READ (number,*) i , ival
            IF ( LPRi.GT.4 ) WRITE (LO,*) 'LUND-MSTJ  ' , i , ival
            MSTj(i) = ival
         ELSE IF ( cname.EQ.'LUND-PARJ ' ) THEN
            READ (number,*) i , ee
            IF ( LPRi.GT.4 ) WRITE (LO,*) 'LUND-PARJ  ' , i , ee
            PARj(i) = REAL(ee)
         ELSE IF ( cname.EQ.'LUND-PARU ' ) THEN
            READ (number,*) i , ee
            IF ( LPRi.GT.4 ) WRITE (LO,*) 'LUND-PARU  ' , i , ee
            PARu(i) = REAL(ee)
         ELSE IF ( cname.EQ.'LUND-DECAY' ) THEN
            READ (number,*) id , ion
            IF ( LPRi.GT.4 ) WRITE (LO,*) 'LUND-DECAY ' , id , ion
 
            kc = PYCOMP(id)
 
            MDCy(kc,1) = ion
         ELSE IF ( cname.EQ.'PSOFTMIN  ' ) THEN
            READ (number,*) PSOmin
            IF ( LPRi.GT.4 ) WRITE (LO,*) 'PSOFTMIN   ' , PSOmin
         ELSE IF ( cname.EQ.'INTPREC   ' ) THEN
            READ (number,*) NGAup1 , NGAup2 , NGAuet , NGAuin , NGAuso
            IF ( LPRi.GT.4 ) WRITE (LO,*) 'INTPREC    ' , NGAup1 , 
     &           NGAup2 , NGAuet , NGAuin , NGAuso
 
C  PDF test utility
         ELSE IF ( cname.EQ.'PDFTEST   ' ) THEN
            READ (number,*) idpdg , scale2 , pvirt2
            pvirt2 = ABS(pvirt2)
            IF ( LPRi.GT.4 ) WRITE (LO,*) 'PDFTEST   ' , idpdg , ' ' , 
     &           scale2 , ' ' , pvirt2
            CALL PHO_PDFTST(idpdg,scale2,pvirt2)
 
C  mass cut on gamma-gamma or gamma-hadron system
         ELSE IF ( cname.EQ.'ECMS-CUT  ' ) THEN
            READ (number,*) ECMin , ECMax
            IF ( LPRi.GT.4 ) WRITE (LO,*) 'ECMS-CUT  ' , ECMin , ECMax
 
C  beam lepton (anti-)tagging system
         ELSE IF ( cname.EQ.'TAG-METHOD' ) THEN
            READ (number,*) ITAg1 , ITAg2
            IF ( LPRi.GT.4 ) WRITE (LO,*) 'TAG-METHOD' , ITAg1 , ITAg2
         ELSE IF ( cname.EQ.'E-TAG1    ' ) THEN
            READ (number,*) EEMin1 , YMIn1 , YMAx1 , Q2Min1 , Q2Max1 , 
     &                      THMin1 , THMax1
            IF ( LPRi.GT.4 ) WRITE (LO,*) 'E-TAG1    ' , EEMin1 , 
     &           YMIn1 , YMAx1 , Q2Min1 , Q2Max1 , THMin1 , THMax1
         ELSE IF ( cname.EQ.'E-TAG2    ' ) THEN
            READ (number,*) EEMin2 , YMIn2 , YMAx2 , Q2Min2 , Q2Max2 , 
     &                      THMin2 , THMax2
            IF ( LPRi.GT.4 ) WRITE (LO,*) 'E-TAG2    ' , EEMin2 , 
     &           YMIn2 , YMAx2 , Q2Min2 , Q2Max2 , THMin2 , THMax2
 
C  sampling of gamma-p events in ep (HERA)
         ELSE IF ( (cname.EQ.'WW-HERA   ') .OR. (cname.EQ.'GP-HERA   ')
     &             ) THEN
            READ (number,*) ee1 , ee2 , nev
            IF ( LPRi.GT.4 ) WRITE (LO,*) 'GP-HERA   ' , ee1 , ee2 , nev
            IF ( YMAx2.LT.0.D0 ) THEN
               IF ( LPRi.GT.4 ) WRITE (LO,*)
     &               ' PHO_INIT:ERROR:ELECTRON TAGGER NOT SET'
            ELSE
               CALL PHO_GPHERA(nev,ee1,ee2)
               KEVent = 0
            END IF
 
C  sampling of gamma-gamma events in e+e- (LEP)
         ELSE IF ( (cname.EQ.'GG-EPEM   ') .OR. (cname.EQ.'WW-EPEM   ')
     &             ) THEN
            READ (number,*) ee1 , ee2 , nev
            IF ( LPRi.GT.4 ) WRITE (LO,*) 'GG-EPEM   ' , ee1 , ee2 , nev
            IF ( (YMAx1.LT.0.D0) .OR. (YMAx2.LT.0.D0) ) THEN
               IF ( LPRi.GT.4 ) WRITE (LO,*)
     &               ' PHO_INIT:ERROR:ELECTRON TAGGERS NOT SET'
            ELSE
               CALL PHO_GGEPEM(-1,ee1,ee2)
               CALL PHO_GGEPEM(nev,ee1,ee2)
               CALL PHO_GGEPEM(-2,sig_tot,sig_gg)
               KEVent = 0
            END IF
 
C  sampling of gamma-gamma in heavy-ion collisions
         ELSE IF ( cname.EQ.'GG-HION-F ' ) THEN
            READ (number,*) ee , na , nz , nev
            IF ( LPRi.GT.4 ) WRITE (LO,*) 'GG-HION-F ' , ee , na , nz , 
     &           nev
            IF ( (YMAx1.LT.0.D0) .OR. (YMAx2.LT.0.D0) ) THEN
               IF ( LPRi.GT.4 ) WRITE (LO,*)
     &               ' PHO_INIT:ERROR:Y RANGE FOR PHOTONS NOT SET'
            ELSE
               CALL PHO_GGHIOF(nev,ee,na,nz)
               KEVent = 0
            END IF
         ELSE IF ( cname.EQ.'GG-HION-G ' ) THEN
            READ (number,*) ee , na , nz , nev
            IF ( LPRi.GT.4 ) WRITE (LO,*) 'GG-HION-G ' , ee , na , nz , 
     &           nev
            IF ( (YMAx1.LT.0.D0) .OR. (YMAx2.LT.0.D0) ) THEN
               IF ( LPRi.GT.4 ) WRITE (LO,*)
     &               ' PHO_INIT:ERROR:Y RANGE FOR PHOTONS NOT SET'
            ELSE
               CALL PHO_GGHIOG(nev,ee,na,nz)
               KEVent = 0
            END IF
 
C  sampling of gamma-hadron events in heavy ion collisions
         ELSE IF ( cname.EQ.'GH-HION-F ' ) THEN
            READ (number,*) ee , na , nz , nev
            IF ( LPRi.GT.4 ) WRITE (LO,*) 'GH-HION-F ' , ee , na , nz , 
     &           nev
            IF ( (YMAx1.LT.0.D0) .OR. (YMAx2.LT.0.D0) ) THEN
               IF ( LPRi.GT.4 ) WRITE (LO,*)
     &               ' PHO_INIT:ERROR:Y RANGE FOR PHOTONS NOT SET'
            ELSE
               CALL PHO_GHHIOF(nev,ee,na,nz)
               KEVent = 0
            END IF
 
C  sampling of hadron-gamma events in hadron - heavy ion collisions
         ELSE IF ( cname.EQ.'HG-HIAS-F ' ) THEN
            READ (number,*) ep , ee , na , nz , nev
            IF ( LPRi.GT.4 ) WRITE (LO,*) 'HG-HIAS-F ' , ep , ee , na , 
     &           nz , nev
            IF ( YMAx2.LT.0.D0 ) THEN
               IF ( LPRi.GT.4 ) WRITE (LO,*)
     &               ' PHO_INIT:ERROR:Y RANGE FOR PHOTONS NOT SET'
            ELSE
               CALL PHO_GHHIAS(nev,ep,ee,na,nz)
               KEVent = 0
            END IF
 
C  sampling of photoproduction events e+e-, backscattered laser
         ELSE IF ( cname.EQ.'BLASER    ' ) THEN
            READ (number,*) ee1 , ee2 , pl_lam_1 , pl_lam_2 , x_1 , 
     &                      x_2 , rho , a , nev
            IF ( LPRi.GT.4 ) WRITE (LO,*) 'BLASER    ' , ee1 , ee2 , 
     &           pl_lam_1 , pl_lam_2 , x_1 , x_2 , rho , a , nev
            CALL PHO_GGBLSR(nev,ee1,ee2,pl_lam_1,pl_lam_2,x_1,x_2,rho,a)
            KEVent = 0
 
C  sampling of photoproduction events beamstrahlung
         ELSE IF ( cname.EQ.'BEAMST    ' ) THEN
            READ (number,*) ee1 , ypsi , sigx , sigy , sigz , aeb , nev
            IF ( LPRi.GT.4 ) WRITE (LO,*) 'BEAMST    ' , ee1 , ypsi , 
     &           sigx , sigy , sigz , aeb , nev
            IF ( YMAx1.LT.0.D0 ) THEN
               IF ( LPRi.GT.4 ) WRITE (LO,*)
     &               ' PHO_INIT:ERROR:ELECTRON TAGGER 1 NOT SET'
            ELSE
               CALL PHO_GGBEAM(nev,ee1,ypsi,sigx,sigy,sigz,aeb)
               KEVent = 0
            END IF
 
C  fixed-energy events in LAB system of particle 2
         ELSE IF ( cname.EQ.'EVENT-LAB ' ) THEN
            READ (number,*) plab , nev
            IF ( LPRi.GT.4 ) WRITE (LO,*) 'EVENT-LAB ' , plab , nev
            CALL PHO_FIXLAB(plab,nev)
            KEVent = 0
 
C  fixed-energy events in CM system
         ELSE IF ( cname.EQ.'EVENT-CMS ' ) THEN
            READ (number,*) ECM , nev
            IF ( LPRi.GT.4 ) WRITE (LO,*) 'EVENT-CMS ' , ECM , nev
            pmass1 = PHO_PMASS(IFPab(1),0) - SQRT(PVIrt(1))
            pmass2 = PHO_PMASS(IFPab(2),0) - SQRT(PVIrt(2))
            CALL PHO_PECMS(1,pmass1,pmass2,ECM,PCM,ee)
            e1 = ee
            e2 = ECM - ee
            theta = 0.D0
            phi = 0.D0
            CALL PHO_FIXCOL(e1,e2,theta,phi,nev)
            KEVent = 0
 
C  fixed-energy events for collider setup with crossing angle
         ELSE IF ( cname.EQ.'EVENT-COLL' ) THEN
            READ (number,*) e1 , e2 , theta , phi , nev
            IF ( LPRi.GT.4 ) WRITE (LO,*) 'EVENT-COLL' , e1 , e2 , 
     &           theta , phi , nev
            CALL PHO_FIXCOL(e1,e2,theta,phi,nev)
            KEVent = 0
 
C  unknown data card
         ELSE
            IF ( LPRi.GT.4 ) WRITE (LO,*)
     &            'PHO_INIT: unknown data card: ' , cname , number
         END IF
 
         GOTO 100
      END IF
 200  IF ( LPRi.GT.4 ) WRITE (LO,*) ' RETURN'
 
      END SUBROUTINE
