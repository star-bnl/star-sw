      SUBROUTINE TAUOLA(MODE,KEYPOL) 
C     *************************************
C general tauola interface, should work in every case until 
C hepevt is OK, does not check if hepevt is 'clean'
C in particular will decay decayed taus...
C only longitudinal spin effects are included.
C in W decay v-a vertex is assumed
C date: 12 DEC 1998. date: 21 June 1999. date: 24 Jan 2001 date: 24 Aug 2001
#include "../../include/HEPEVT.h"
      COMMON /TAUPOS/ NP1, NP2 
      REAL*4 PHOI(4),PHOF(4)
      double precision Q1(4),Q2(4),P1(4),P2(4),P3(4),P4(4)
      COMMON / MOMDEC / Q1,Q2,P1,P2,P3,P4
* tauola, photos and jetset overall switches
      COMMON /LIBRA/ JAK1,JAK2,ITDKRC,IFPHOT,IFHADM,IFHADP
      REAL*4 RRR(1)
      LOGICAL IFPSEUDO
      common /pseudocoup/ csc,ssc
      REAL*4 csc,ssc
      save pseudocoup
      COMMON / INOUT / INUT,IOUT

C to switch tau polarization OFF in taus 
      DIMENSION POL1(4), POL2(4)
      double precision POL1x(4), POL2x(4)
      INTEGER ION(3)
      DATA  POL1 /0.0,0.0,0.0,0.0/
      DATA  POL2 /0.0,0.0,0.0,0.0/
      DATA PI /3.141592653589793238462643D0/

C store decay vertexes 
      DIMENSION IMOTHER (20)
      INTEGER KFHIGGS(3)

C store daughter pointers
      INTEGER ISON
      COMMON /ISONS_TAU/ISON(2)
      SAVE /ISONS_TAU/

      IF(MODE.EQ.-1) THEN
C     ***********************

         JAK1  =  0     ! decay mode first tau 
         JAK2  =  0     ! decay mode second tau
         ITDKRC=1.0     ! switch of radiative corrections in decay
         IFPHOT=1.0     ! PHOTOS switch
         IFHADM=1.0
         IFHADP=1.0
         POL=1.0        ! tau polarization dipswitch must be 1 or 0

         KFHIGGS(1) = 25
         KFHIGGS(2) = 35
         KFHIGGS(3) = 36
         KFHIGCH = 37
         KFZ0    = 23
         KFGAM   = 22
         KFTAU   = 15
         KFNUE   = 16
C couplings of the 'pseudoscalar higgs' as in CERN-TH/2003-166
         psi=0.5*PI ! 0.15*PI
         xmtau=1.777 ! tau mass
         xmh=120     ! higgs boson mass
         betah=sqrt(1d0-4*xmtau**2/xmh**2)
         csc=cos(psi)*betah
         ssc=sin(psi)
C         write(*,*) ' scalar component=',csc,' pseudo-scalar component=',ssc
         IF (IFPHOT.EQ.1) CALL  PHOINI  ! this if PHOTOS was not initialized earlier
         CALL  INIETC(JAK1,JAK2,ITDKRC,IFPHOT)
         CALL  INIMAS
         CALL  INIPHX(0.01d0)
         CALL  INITDK
C activation of pi0 and eta decays: (-1,1) means on,  (-1,0) off
         ION(1)=1
         ION(2)=1
         ION(3)=1
         CALL  TAUPI0(-1,1,ION)
         CALL DEKAY(-1,POL1x)
           WRITE(IOUT,7001) pol,psi,ION(1),ION(2),ION(3)
      ELSEIF(MODE.EQ.0) THEN
C     ***********************
C
C..... find tau-s and fill common block /TAUPOS/
C this is to avoid LUND history fillings. This call is optional
      CALL PHYFIX(NSTOP,NSTART)
C clear mothers of the previous event
      DO II=1,20
       IMOTHER(II)=0
      ENDDO

      DO II=1,2
         ISON(II)=0
      ENDDO
C ... and to find mothers giving taus.
      NDEC    = 0
C(BPK)--> LOOK FOR MOTHER, CHECK THAT IT IS NOT THE HISTORY ENTRY (E.G. MSTP(128)=0)
      DO I=NSTART,NHEP
        IF(ABS(IDHEP(I)).EQ.KFTAU.AND.ISTHEP(I).EQ.1.AND.
     $        (ISTHEP(I).GE.125.OR.ISTHEP(I).LT.120)) THEN
           IMOTH=JMOHEP(1,I)
           DO WHILE (ABS(IDHEP(IMOTH)).EQ.KFTAU) ! KEEP WALKING UP
              IMOTH=JMOHEP(1,IMOTH)
           ENDDO
           IF (ISTHEP(IMOTH).EQ.3.OR.
     $        (ISTHEP(IMOTH).GE.120.AND.ISTHEP(IMOTH).LE.125)) THEN 
              DO J=NSTART,NHEP  ! WE HAVE WALKED INTO HARD RECORD
                 IF (IDHEP(J).EQ.IDHEP(IMOTH).AND.
     $               JMOHEP(1,J).EQ.IMOTH.AND.
     $               ISTHEP(J).EQ.2) THEN
                    JMOTH=J
                    GOTO 66
                 ENDIF
              ENDDO
           ELSE
              JMOTH=IMOTH
           ENDIF
 66        CONTINUE
           DO II=1,NDEC
            IF(JMOTH.EQ.IMOTHER(II)) GOTO 9999
           ENDDO
C(BPK)--<
           NDEC=NDEC+1
           IMOTHER(NDEC)= JMOTH
        ENDIF
 9999   CONTINUE
      ENDDO  

C ... taus of every mother are treated in this main loop
      DO II=1,NDEC
         IM=IMOTHER(II)
         NCOUNT=0
         NP1=0
         NP2=0


C(BPK)--> 
C CORRECTING HEPEVT IS OUT OF QUESTION AT THIS POINT..
         IM0=IM
         IF (IDHEP(JMOHEP(1,IM0)).EQ.IDHEP(IM0)) IM0=JMOHEP(1,IM0)
         ISEL=-1
         DO I=NSTART,NHEP
            IF (ISTHEP(I).EQ.3.OR.
     $           (ISTHEP(I).GE.120.AND.ISTHEP(I).LE.125)) THEN ! HARD RECORD
               GOTO 76
            ENDIF
            IMOTH=JMOHEP(1,I)
            DO WHILE (IDHEP(IMOTH).EQ.IDHEP(I).OR.ABS(IDHEP(IMOTH)).EQ.KFTAU) ! KEEP WALKING UP
               IMOTH=JMOHEP(1,IMOTH)
            ENDDO
            IF ((IMOTH.EQ.IM0.OR.IMOTH.EQ.IM).AND.ISEL.EQ.-1) THEN
               ISON(1)=I
               ISEL=0
            ELSEIF ((IMOTH.EQ.IM0.OR.IMOTH.EQ.IM).AND.ISEL.EQ.0) THEN
               ISON(2)=I
            ELSEIF ((IMOTH.NE.IM0.AND.IMOTH.NE.IM).AND.ISEL.EQ.0) THEN
               ISEL=1
               GOTO 77
            ENDIF
 76         CONTINUE
         ENDDO
 77      CONTINUE
C(BPK)--< 


C ... we correct HEPEVT (fix developped with Catherine BISCARAT)
c         IF (JDAHEP(2,IM).EQ.0) THEN                      ! ID of second daughter was missing
c          ISECU=1
c          DO I=JDAHEP(1,IM)+1,NHEP                        ! OK lets look for it
c           IF (JMOHEP(1,I).EQ.IM.AND.ISECU.EQ.1) THEN     ! we have found one
c              JDAHEP(2,IM)=I
c           ELSEIF (JMOHEP(1,I).EQ.IM.AND.ISECU.NE.1) THEN ! we have found one after there
c              JDAHEP(2,IM)=0                              ! was something else, lets kill game
c           ENDIF
c           IF (JMOHEP(1,I).NE.IM) ISECU=0                 ! other stuff starts
c          ENDDO
c         ENDIF

C ... we check whether there are just two or more tau-likes 
         DO I=ISON(1),ISON(2)
            IF(IDHEP(I).EQ.-KFTAU.OR.IDHEP(I).EQ.-KFNUE) NCOUNT=NCOUNT+1
            IF(IDHEP(I).EQ. KFTAU.OR.IDHEP(I).EQ. KFNUE) NCOUNT=NCOUNT+1
         ENDDO

C ... if there will be more we will come here again
 666     CONTINUE

C(BPK)--> 
         DO I=MAX(NP1+1,ISON(1)),ISON(2) 
C(BPK)--< 
            IF(IDHEP(I).EQ.-KFTAU.OR.IDHEP(I).EQ.-KFNUE) NP1=I
         ENDDO
C(BPK)--> 
         DO I=MAX(NP2+1,ISON(1)),ISON(2)
C(BPK)--< 
            IF(IDHEP(I).EQ. KFTAU.OR.IDHEP(I).EQ. KFNUE) NP2=I
         ENDDO
         DO I=1,4
            P1(I)= PHEP(I,NP1)    !momentum of tau+
            P2(I)= PHEP(I,NP2)    !momentum of tau-
            Q1(I)= P1(I)+P2(I)
         ENDDO

         POL1(3)=  0D0
         POL2(3)=  0D0
         
         IF(KEYPOL.EQ.1) THEN
c.....include polarisation effect
         CALL RANMAR(RRR,1)

         IF(IDHEP(IM).EQ.KFHIGGS(1).OR.IDHEP(IM).EQ.KFHIGGS(2).OR.
     $    IDHEP(IM).EQ.KFHIGGS(3)) THEN   ! case of Higgs 
            IF(RRR(1).LT.0.5) THEN
               POL1(3)= POL
               POL2(3)=-POL
            ELSE     
               POL1(3)=-POL
               POL2(3)= POL
            ENDIF
         ELSEIF((IDHEP(IM).EQ.KFZ0).OR.(IDHEP(IM).EQ.KFGAM)) THEN ! case of gamma/Z 
C there is no angular dependence in gamma/Z polarization 
C there is no s-dependence in gamma/Z polarization at all
C there is even no Z polarization in any form
C main reason is that nobody asked ...
C but it is prepared and longitudinal correlations 
C can be included up to KORALZ standards

            POLZ0=PLZAPX(.true.,IM,NP1,NP2)
            IF(RRR(1).LT.POLZ0) THEN
               POL1(3)= POL
               POL2(3)= POL
            ELSE     
               POL1(3)=-POL
               POL2(3)=-POL
            ENDIF
         ELSEIF(IDHEP(NP1).EQ.-IDHEP(NP2))THEN ! undef orig. only s-dep poss.
            POLZ0=PLZAPX(.true.,IM,NP1,NP2)
            IF(RRR(1).LT.POLZ0) THEN
               POL1(3)= POL
               POL2(3)= POL
            ELSE     
               POL1(3)=-POL
               POL2(3)=-POL
            ENDIF
         ELSEIF(ABS(IDHEP(IM)).EQ.KFHIGCH) THEN ! case of charged Higgs 
            POL1(3)=  POL
            POL2(3)=  POL
         ELSE ! case of W+ or W-
            POL1(3)= -POL
            POL2(3)= -POL
         ENDIF
c.....include polarisation effect
         ENDIF  

         IF(IDHEP(IM).EQ.KFHIGGS(1).OR.IDHEP(IM).EQ.KFHIGGS(2).OR.
     $   IDHEP(IM).EQ.KFHIGGS(3)) THEN
           IF(IDHEP(NP1).EQ.-KFTAU                           .AND.
     $       (JDAHEP(1,NP1).LE.NP1.OR.JDAHEP(1,NP1).GT.NHEP) .AND.
     $        IDHEP(NP2).EQ. KFTAU                            .AND.
     $       (JDAHEP(1,NP2).LE.NP2.OR.JDAHEP(1,NP2).GT.NHEP)
     $                                                       ) THEN
               IF     (IDHEP(IM).EQ.KFHIGGS(1)) THEN
                 IFPSEUDO= .FALSE.
               ELSEIF (IDHEP(IM).EQ.KFHIGGS(2)) THEN
                 IFPSEUDO= .FALSE.
               ELSEIF (IDHEP(IM).EQ.KFHIGGS(3)) THEN
                 IFPSEUDO= .TRUE.
               ELSE
                 WRITE(*,*) 'Warning from TAUOLA:'
                 WRITE(*,*) 'I stop this run, wrong IDHEP(IM)=',IDHEP(IM)
                 STOP
               ENDIF
               CALL SPINHIGGS(IM,NP1,NP2,IFPSEUDO,Pol1,Pol2)
               IF (IFPHOT.EQ.1) CALL PHOTOS(IM)  ! Bremsstrahlung in Higgs decay
                                                 ! AFTER adding taus !!


           ENDIF
         ELSE
           IF(IDHEP(NP1).EQ.-KFTAU.AND.
     $       (JDAHEP(1,NP1).LE.NP1.OR.JDAHEP(1,NP1).GT.NHEP)) THEN
C            here check on if NP1 was not decayed should be verified
             CALL DEXAY(1,POL1)
             IF (IFPHOT.EQ.1) CALL PHOTOS(NP1)
             CALL TAUPI0(0,1,ION)
           ENDIF

           IF(IDHEP(NP2).EQ. KFTAU.AND.
     $       (JDAHEP(1,NP2).LE.NP2.OR.JDAHEP(1,NP2).GT.NHEP)) THEN
C            here check on if NP2 was not decayed should be added
             CALL DEXAY(2,POL2)
             IF (IFPHOT.EQ.1) CALL PHOTOS(NP2)
             CALL TAUPI0(0,2,ION)
           ENDIF
         ENDIF
         NCOUNT=NCOUNT-2
         IF (NCOUNT.GT.0) GOTO 666
      ENDDO

      ELSEIF(MODE.EQ.1) THEN
C     ***********************
C
      CALL DEXAY(100,POL1)
      CALL DEKAY(100,POL1x)
           WRITE(IOUT,7002)
      ENDIF
C     *****
 7001 FORMAT(///1X,15(5H*****)
     $ /,' *',     25X,'*****TAUOLA UNIVERSAL INTERFACE: ******',9X,1H*,
     $ /,' *',     25X,'*****VERSION 1.22, April 2009 (gfort)**',9X,1H*,
     $ /,' *',     25X,'**AUTHORS: P. Golonka, B. Kersevan, ***',9X,1H*,
     $ /,' *',     25X,'**T. Pierzchala, E. Richter-Was, ******',9X,1H*,
     $ /,' *',     25X,'****** Z. Was, M. Worek ***************',9X,1H*,
     $ /,' *',     25X,'**USEFUL DISCUSSIONS, IN PARTICULAR ***',9X,1H*,
     $ /,' *',     25X,'*WITH C. Biscarat and S. Slabospitzky**',9X,1H*,
     $ /,' *',     25X,'****** are warmly acknowledged ********',9X,1H*,
     $ /,' *',     25X,'                                       ',9X,1H*,
     $ /,' *',     25X,'********** INITIALIZATION  ************',9X,1H*,
     $ /,' *',F20.5,5X,'tau polarization switch must be 1 or 0 ',9X,1H*,
     $ /,' *',F20.5,5X,'Higs scalar/pseudo mix CERN-TH/2003-166',9X,1H*,
     $ /,' *',I10, 15X,'PI0 decay switch must be 1 or 0        ',9X,1H*,
     $ /,' *',I10, 15X,'ETA decay switch must be 1 or 0        ',9X,1H*,
     $ /,' *',I10, 15X,'K0S decay switch must be 1 or 0        ',9X,1H*,
     $  /,1X,15(5H*****)/)

 7002 FORMAT(///1X,15(5H*****)
     $ /,' *',     25X,'*****TAUOLA UNIVERSAL INTERFACE: ******',9X,1H*,
     $ /,' *',     25X,'*****VERSION 1.22, April 2009 (gfort)**',9X,1H*,
     $ /,' *',     25X,'**AUTHORS: P. Golonka, B. Kersevan, ***',9X,1H*,
     $ /,' *',     25X,'**T. Pierzchala, E. Richter-Was, ******',9X,1H*,
     $ /,' *',     25X,'****** Z. Was, M. Worek ***************',9X,1H*,
     $ /,' *',     25X,'**USEFUL DISCUSSIONS, IN PARTICULAR ***',9X,1H*,
     $ /,' *',     25X,'*WITH C. Biscarat and S. Slabospitzky**',9X,1H*,
     $ /,' *',     25X,'****** are warmly acknowledged ********',9X,1H*,
     $ /,' *',     25X,'****** END OF MODULE OPERATION ********',9X,1H*,
     $  /,1X,15(5H*****)/)

      END

      SUBROUTINE SPINHIGGS(IM,NP1,NP2,IFPSEUDO,Pol1,Pol2)
      LOGICAL IFPSEUDO
      REAL*8 HH1,HH2,wthiggs
      DIMENSION POL1(4), POL2(4),HH1(4),HH2(4), RRR(1)
!             CALL DEXAY(1,POL1)  ! Kept for tests
!             CALL DEXAY(2,POL2)  ! Kept for tests
      INTEGER ION(3)
 10   CONTINUE
         CALL RANMAR(RRR,1)
         CALL DEKAY(1,HH1)
         CALL DEKAY(2,HH2)
         wt=wthiggs(IFPSEUDO,HH1,HH2)
      IF (RRR(1).GT.WT) GOTO 10
      CALL DEKAY(1+10,HH1)
      CALL TAUPI0(0,1,ION)
      CALL DEKAY(2+10,HH2)
      CALL TAUPI0(0,2,ION)
      END
      FUNCTION wthiggs(IFPSEUDO,HH1,HH2)
      LOGICAL IFPSEUDO
      common /pseudocoup/ csc,ssc
      REAL*4 csc,ssc
      save pseudocoup
      REAL*8 HH1(4),HH2(4),R(4,4),wthiggs
      DO K=1,4
       DO L=1,4
        R(K,L)=0
       ENDDO
      ENDDO
      WTHIGGS=0D0
      
      R(4,4)= 1D0    !  unpolarized part
      R(3,3)=-1D0    !  longitudinal
                     !  other missing
      IF (IFPSEUDO) THEN
        R(1,1)=-1
        R(2,2)= -1
        R(1,1)=(csc**2-ssc**2)/(csc**2+ssc**2)
        R(2,2)=(csc**2-ssc**2)/(csc**2+ssc**2)
        R(1,2)=2*csc*ssc/(csc**2+ssc**2)
        R(2,1)=-2*csc*ssc/(csc**2+ssc**2)
      ELSE
        R(1,1)=1
        R(2,2)=1
      ENDIF



      DO K=1,4
       DO L=1,4
        WTHIGGS=WTHIGGS+R(K,L)*HH1(K)*HH2(L)
       ENDDO
      ENDDO
        WTHIGGS=WTHIGGS/4D0
      END

      FUNCTION PLZAPX(HOPEin,IM0,NP1,NP2)
C     IM0 NP1 NP2 are the positions of Z/gamma tau tau in hepevt common block.
C     the purpose of this routine is to calculate polarization of Z along tau direction.
C     this is highly non-trivial due to necessity of reading infromation from hard process
C     history in HEPEVT, which is often written not up to the gramatic rules.
      REAL*8 PLZAP0,SVAR,COSTHE,sini,sfin,ZPROP2,
     $       P1(4),P2(4),Q1(4),Q2(4),QQ(4),PH(4),PD1(4),PD2(4),
     $       PQ1(4),PQ2(4),PB(4),PA(4)
      REAL*4 PLZAPX
      INTEGER IM
      LOGICAL HOPE,HOPEin
#include "../../include/HEPEVT.h"

C(BPK)--> BROTHERS OF TAU ALREADY FOUND
      INTEGER ISON
      COMMON /ISONS_TAU/ISON(2)
C(BPK)--<
C >>
C >> STEP 1: find where are particles in hepevent and pick them up
C >>
             HOPE=HOPEin
C sometimes shade Z of Z is its mother ...
            IM=IM0
            IM00=JMOHEP(1,IM0)
C to protect against check on mother of beam particles.
            IF (IM00.GT.0) THEN
              IF (IDHEP(IM0).EQ.IDHEP(IM00)) IM=JMOHEP(1,IM0)
            ENDIF                                                              
C
C find (host generator-level) incoming beam-bare-particles which form Z and co.
            IMO1=JMOHEP(1,IM)
            IMO2=JMOHEP(2,IM)

C(BPK)--> IN HERWIG THE POINTER MIGHT BE TO HARD CMS
            IM00=IMO1
            IF (ISTHEP(IM00).EQ.120) THEN
               IMO1=JMOHEP(1,IM00)
               IMO2=JMOHEP(2,IM00)
            ENDIF
C(BPK)--<

            IFFULL=0
C case when it was like e+e- --> tau+tau- gammas and e+e- were 'first' in hepevt.
      IF (IMO1.EQ.0.AND.IMO2.EQ.0) THEN
         IMO1=JMOHEP(1,NP1)
C(BPK)-->
         IF (IDHEP(IMO1).EQ.IDHEP(NP1)) IMO1=JMOHEP(1,IMO1) ! PROTECT AGAINST COPIES
C(BPK)--<
         IMO2=JMOHEP(2,NP1)
C(BPK)-->
         IF (IDHEP(IMO2).EQ.IDHEP(NP2)) IMO2=JMOHEP(1,IMO2) ! PROTECT AGAINST COPIES
C(BPK)--<
         IFFULL=1
C case when it was like  qq --> tau+tau- gammas and qq were NOT 'first' in hepevt.

      ELSEIF (IDHEP(IM).NE.22.AND.IDHEP(IM).NE.23) THEN
         IMO1=JMOHEP(1,NP1)
C(BPK)-->
         IF (IDHEP(IMO1).EQ.IDHEP(NP1)) IMO1=JMOHEP(1,IMO1) ! PROTECT AGAINST COPIES
C(BPK)--<
         IMO2=JMOHEP(2,NP1)
C(BPK)-->
         IF (IDHEP(IMO2).EQ.IDHEP(NP2)) IMO2=JMOHEP(1,IMO2) ! PROTECT AGAINST COPIES
C(BPK)--<
         IFFULL=1
      ENDIF

            
C and check if it really happened
            IF (IMO1.EQ.0) HOPE=.FALSE.
            IF (IMO2.EQ.0) HOPE=.FALSE.
            IF (IMO1.EQ.IMO2) HOPE=.FALSE.

C 
         DO I=1,4
            Q1(I)= PHEP(I,NP1)              !momentum of tau+
            Q2(I)= PHEP(I,NP2)              !momentum of tau-
         ENDDO

C corrections due to possible differences in 4-momentum of shadow vs true Z.
C(BPK)-->
      IF (IM.EQ.JMOHEP(1,IM0).AND.
     $     (IDHEP(IM).EQ.22.OR.IDHEP(IM).EQ.23)) THEN
         DO K=1,4
            PB(K)=PHEP(K,IM)
            PA(K)=PHEP(K,IM0)
         ENDDO
C(BPK)--<

               CALL BOSTDQ( 1,PA, Q1, Q1)
               CALL BOSTDQ( 1,PA, Q2, Q2)
               CALL BOSTDQ(-1,PB, Q1, Q1)
               CALL BOSTDQ(-1,PB, Q2, Q2)

              ENDIF

         DO I=1,4                                                                                   
            QQ(I)= Q1(I)+Q2(I)              !momentum of Z
            IF (HOPE) P1(I)=PHEP(I,IMO1)    !momentum of beam1
            IF (HOPE) P2(I)=PHEP(I,IMO2)    !momentum of beam2
            PH(I)=0D0
            PD1(I)=0D0
            PD2(I)=0D0
         ENDDO
!        These momenta correspond to  quarks, gluons photons or taus
                   IDFQ1=IDHEP(NP1)
                   IDFQ2=IDHEP(NP2)
         IF (HOPE) IDFP1=IDHEP(IMO1)
         IF (HOPE) IDFP2=IDHEP(IMO2)

         SVAR=QQ(4)**2-QQ(3)**2-QQ(2)**2-QQ(1)**2
         IF (.NOT.HOPE) THEN
C options which may be useful in some cases of two heavy boson production
C need individual considerations. To be developed.
C           PLZAPX=PLZAP0(11,IDFQ1,SVAR,0D0)  ! gamma/Z mixture as if produced from e beam
C           PLZAPX=PLZAP0(12,IDFQ1,SVAR,0D0)  ! pure Z
           PLZAPX=0.5                         ! pure gamma
           RETURN
         ENDIF
C >>
C >> STEP 2 look for brothers of Z which have to be included in effective incoming particles
C >>
C let us define beginning and end of particles which are produced in parallel to Z
C in principle following should work

C(BPK)--> ACCOMMODATE FOR HERWIG - IM00 POINTS TO BEAM PARTICLE OR HARD CMS
         NX1=JDAHEP(1,IM00)
         NX2=JDAHEP(2,IM00)
C but ...
         INBR=IM ! OK, HARD RECORD Z/GAMMA 
         IF (IFFULL.EQ.1) INBR=NP1  ! OK, NO Z/GAMMA
         IF (IDHEP(JMOHEP(1,INBR)).EQ.IDHEP(INBR)) INBR=JMOHEP(1,INBR) ! FORCE HARD RECORD
C(BPK)--<
         IF(NX1.EQ.0.OR.NX2.EQ.0) THEN
           NX1=INBR
           NX2=INBR
           DO K=1,INBR-1
             IF(JMOHEP(1,INBR-K).EQ.JMOHEP(1,INBR)) THEN
              NX1=INBR-K
             ELSE
                GOTO 7
             ENDIF
           ENDDO
 7         CONTINUE

           DO K=INBR+1,NHEP
             IF(JMOHEP(1,K).EQ.JMOHEP(1,INBR)) THEN
              NX2=K
             ELSE
                GOTO 8
             ENDIF
           ENDDO
 8         CONTINUE
         ENDIF

C case of annihilation of two bosons is hopeles
         IF (ABS(IDFP1).GE.20.AND.ABS(IDFP2).GE.20) HOPE=.FALSE.
C case of annihilation of non-matching flavors is hopeless
         IF (ABS(IDFP1).LE.20.AND.ABS(IDFP2).LE.20.AND.IDFP1+IDFP2.NE.0)
     $       HOPE=.FALSE.
         IF (.NOT.HOPE) THEN
C options which may be useful in some cases of two heavy boson production
C need individual considerations. To be developed.
C           PLZAPX=PLZAP0(11,IDFQ1,SVAR,0D0)  ! gamma/Z mixture as if produced from e beam
C           PLZAPX=PLZAP0(12,IDFQ1,SVAR,0D0)  ! pure Z
           PLZAPX=0.5                         ! pure gamma
           RETURN
         ENDIF
         IF (ABS(IDFP1).LT.20) IDE= IDFP1
         IF (ABS(IDFP2).LT.20) IDE=-IDFP2


C >>
C >> STEP 3  we combine gluons, photons  into incoming effective beams
C >>

C in the following we will ignore the possibility of photon emission from taus
C however at certain moment it will be necessary to take care of

           DO L=1,4
            PD1(L)=P1(L)
            PD2(L)=P2(L)
           ENDDO 

                DO L=1,4
                PQ1(L)=Q1(L)
                PQ2(L)=Q2(L)
                ENDDO 

         IFLAV=min(ABS(IDFP1),ABS(IDFP2))

*--------------------------------------------------------------------------
c                  IFLAV=min(ABS(IDFP1),ABS(IDFP2))
c that means that always quark or lepton i.e. process like
c      f g(gamma) --> f Z0 --> tau tau
c we glue  fermions to effective  beams that is f f --> Z0 --> tau tau
c  with  gamma/g emission from initial fermion.
*---------------------------------------------------------------------------
 
         IF (ABS(IDFP1).GE.20) THEN
           DO k=NX1,NX2
             IDP=IDHEP(k)
             IF (ABS(IDP).EQ.IFLAV) THEN
               DO L=1,4
                 PD1(L)=-PHEP(L,K)
               ENDDO
             ENDIF
           ENDDO
         ENDIF

         IF (ABS(IDFP2).GE.20) THEN
           DO k=NX1,NX2
             IDP=IDHEP(k)
             IF (ABS(IDP).EQ.IFLAV) THEN
               DO L=1,4
                 PD2(L)=-PHEP(L,K)
               ENDDO
             ENDIF
           ENDDO
         ENDIF

C if first beam was boson: gluing

         IF (ABS(IDFP1).GE.20) THEN
           DO L=1,4
             PH(L)=P1(L)
           ENDDO
           xm1=abs((PD1(4)+PH(4))**2-(PD1(3)+PH(3))**2
     $            -(PD1(2)+PH(2))**2-(PD1(1)+PH(1))**2)
           xm2=abs((PD2(4)+PH(4))**2-(PD2(3)+PH(3))**2
     $            -(PD2(2)+PH(2))**2-(PD2(1)+PH(1))**2)
          IF (XM1.LT.XM2) THEN
             DO L=1,4
               PD1(L)=PD1(L)+P1(L)
             ENDDO
           ELSE
             DO L=1,4
               PD2(L)=PD2(L)+P1(L)
             ENDDO
           ENDIF
         ENDIF

C if second beam was boson: gluing 


         IF (ABS(IDFP2).GE.20) THEN
           DO L=1,4
             PH(L)=P2(L)
           ENDDO
           xm1=abs((PD1(4)+PH(4))**2-(PD1(3)+PH(3))**2
     $            -(PD1(2)+PH(2))**2-(PD1(1)+PH(1))**2)
           xm2=abs((PD2(4)+PH(4))**2-(PD2(3)+PH(3))**2
     $            -(PD2(2)+PH(2))**2-(PD2(1)+PH(1))**2)
           IF (XM1.LT.XM2) THEN
             DO L=1,4
               PD1(L)=PD1(L)+P2(L)
             ENDDO
           ELSE
             DO L=1,4
               PD2(L)=PD2(L)+P2(L)
             ENDDO
           ENDIF
         ENDIF

C now spectators ...

C(BPK)-->
      NPH1=NP1
      NPH2=NP2
      IF (IDHEP(JMOHEP(1,NP1)).EQ.IDHEP(NP1)) NPH1=JMOHEP(1,NP1) ! SHOULD PUT US IN HARD REC.
      IF (IDHEP(JMOHEP(1,NP2)).EQ.IDHEP(NP2)) NPH2=JMOHEP(1,NP2) ! SHOULD PUT US IN HARD REC.
C(BPK)--<

         DO k=NX1,NX2
         IF (ABS(IDHEP(K)).NE.IFLAV.AND.K.NE.IM.AND.
C(BPK)-->
     $       K.NE.NPH1.AND.K.NE.NPH2) THEN 
C(BPK)--<
          IF(IDHEP(K).EQ.22.AND.IFFULL.EQ.1) THEN
            DO L=1,4
              PH(L)=PHEP(L,K)
            ENDDO
            xm1=abs((PD1(4)-PH(4))**2-(PD1(3)-PH(3))**2
     $             -(PD1(2)-PH(2))**2-(PD1(1)-PH(1))**2)
            xm2=abs((PD2(4)-PH(4))**2-(PD2(3)-PH(3))**2
     $             -(PD2(2)-PH(2))**2-(PD2(1)-PH(1))**2)
           xm3=abs((PQ1(4)+PH(4))**2-(PQ1(3)+PH(3))**2
     $            -(PQ1(2)+PH(2))**2-(PQ1(1)+PH(1))**2)
           xm4=abs((PQ2(4)+PH(4))**2-(PQ2(3)+PH(3))**2
     $            -(PQ2(2)+PH(2))**2-(PQ2(1)+PH(1))**2)

  
            sini=abs((PD1(4)+PD2(4)-PH(4))**2-(PD1(3)+PD2(3)-PH(3))**2
     $              -(PD1(2)+PD2(2)-PH(2))**2-(PD1(1)+PD2(1)-PH(1))**2)
            sfin=abs((PD1(4)+PD2(4)      )**2-(PD1(3)+PD2(3)      )**2
     $              -(PD1(2)+PD2(2)      )**2-(PD1(1)+PD2(1)      )**2)

           FACINI=ZPROP2(sini)
           FACFIN=ZPROP2(sfin)

           XM1=XM1/FACINI
           XM2=XM2/FACINI
           XM3=XM3/FACFIN
           XM4=XM4/FACFIN

           XM=MIN(XM1,XM2,XM3,XM4)
                  IF      (XM1.EQ.XM) THEN 
                     DO L=1,4
                       PD1(L)=PD1(L)-PH(L)
                     ENDDO
                  ELSEIF   (XM2.EQ.XM) THEN 
                     DO L=1,4
                       PD2(L)=PD2(L)-PH(L)
                     ENDDO
                  ELSEIF   (XM3.EQ.XM) THEN 
                     DO L=1,4
                        Q1(L)=PQ1(L)+PH(L)
                     ENDDO
                  ELSE
                     DO L=1,4
                        Q2(L)=PQ2(L)+PH(L)
                     ENDDO
                  ENDIF
           ELSE
            DO L=1,4
              PH(L)=PHEP(L,K)
            ENDDO
            xm1=abs((PD1(4)-PH(4))**2-(PD1(3)-PH(3))**2
     $             -(PD1(2)-PH(2))**2-(PD1(1)-PH(1))**2)
            xm2=abs((PD2(4)-PH(4))**2-(PD2(3)-PH(3))**2
     $             -(PD2(2)-PH(2))**2-(PD2(1)-PH(1))**2)
            IF (XM1.LT.XM2) THEN
              DO L=1,4
                PD1(L)=PD1(L)-PH(L)
              ENDDO
            ELSE
              DO L=1,4
                PD2(L)=PD2(L)-PH(L)
              ENDDO
            ENDIF
           ENDIF
          ENDIF
         ENDDO


C >>
C >> STEP 4 look for brothers of tau (sons of Z!) which have to be included in 
c >>          effective outcoming taus
C >>
C let us define beginning and end of particles which are produced in 
c  parallel to tau



C find outcoming particles which come from Z   

       
 

C(BPK)--> OK, IT WOULD HAVE TO BE ALONG TAUS IN HARD RECORD WITH THE SAME MOTHER       
      IF (ABS(IDHEP(IM0)).EQ.22.OR.abs(IDHEP(IM0)).EQ.23) THEN
         DO K=ISON(1),ISON(2)
            IF(ABS(IDHEP(K)).EQ.22) THEN
C(BPK)--< 

              do l=1,4
              ph(l)=phep(l,k)
              enddo

           xm3=abs((PQ1(4)+PH(4))**2-(PQ1(3)+PH(3))**2
     $            -(PQ1(2)+PH(2))**2-(PQ1(1)+PH(1))**2)
           xm4=abs((PQ2(4)+PH(4))**2-(PQ2(3)+PH(3))**2
     $            -(PQ2(2)+PH(2))**2-(PQ2(1)+PH(1))**2)  

           XM=MIN(XM3,XM4) 

                  IF   (XM3.EQ.XM) THEN 
                     DO L=1,4
                        Q1(L)=PQ1(L)+PH(L)
                     ENDDO
                  ELSE
                     DO L=1,4
                        Q2(L)=PQ2(L)+PH(L)
                     ENDDO
                  ENDIF
            endif
          enddo
          ENDIF



*------------------------------------------------------------------------


C out of effective momenta we calculate COSTHE and later polarization
      CALL ANGULU(PD1,PD2,Q1,Q2,COSTHE)
     
      PLZAPX=PLZAP0(IDE,IDFQ1,SVAR,COSTHE)
      END

      SUBROUTINE ANGULU(PD1,PD2,Q1,Q2,COSTHE)
      REAL*8 PD1(4),PD2(4),Q1(4),Q2(4),COSTHE,P(4),QQ(4),QT(4)
C take effective beam which is less massive, it should be irrelevant
C but in case HEPEVT is particulary dirty may help.
C this routine calculate reduced system transver and cosine of scattering 
C angle.

      XM1=ABS(PD1(4)**2-PD1(3)**2-PD1(2)**2-PD1(1)**2)
      XM2=ABS(PD2(4)**2-PD2(3)**2-PD2(2)**2-PD2(1)**2)
      IF (XM1.LT.XM2) THEN
        SIGN=1D0
        DO K=1,4
          P(K)=PD1(K)
        ENDDO
      ELSE
        SIGN=-1D0
        DO K=1,4
          P(K)=PD2(K)
        ENDDO
      ENDIF
C calculate space like part of P (in Z restframe)
      DO K=1,4
       QQ(K)=Q1(k)+Q2(K)
       QT(K)=Q1(K)-Q2(K)
      ENDDO

       XMQQ=SQRT(QQ(4)**2-QQ(3)**2-QQ(2)**2-QQ(1)**2)

       QTXQQ=QT(4)*QQ(4)-QT(3)*QQ(3)-QT(2)*QQ(2)-QT(1)*QQ(1)
      DO K=1,4
       QT(K)=QT(K)-QQ(K)*QTXQQ/XMQQ**2
      ENDDO

       PXQQ=P(4)*QQ(4)-P(3)*QQ(3)-P(2)*QQ(2)-P(1)*QQ(1)
      DO K=1,4
       P(K)=P(K)-QQ(K)*PXQQ/XMQQ**2
      ENDDO
C calculate costhe
       PXP  =SQRT(p(1)**2+p(2)**2+p(3)**2-p(4)**2)
       QTXQT=SQRT(QT(3)**2+QT(2)**2+QT(1)**2-QT(4)**2)
       PXQT =P(3)*QT(3)+P(2)*QT(2)+P(1)*QT(1)-P(4)*QT(4)
       COSTHE=PXQT/PXP/QTXQT
       COSTHE=COSTHE*SIGN
      END

      FUNCTION PLZAP0(IDE,IDF,SVAR,COSTH0)
C this function calculates probability for the helicity +1 +1 configuration
C of taus for given Z/gamma transfer and COSTH0 cosine of scattering angle
      REAL*8 PLZAP0,SVAR,COSTHE,COSTH0,T_BORN

      COSTHE=COSTH0
C >>>>>      IF (IDE*IDF.LT.0) COSTHE=-COSTH0 ! this is probably not needed ID
C >>>>>      of first beam is used by T_GIVIZ0 including sign

      IF (IDF.GT.0) THEN
        CALL INITWK(IDE,IDF,SVAR)
      ELSE
        CALL INITWK(-IDE,-IDF,SVAR)
      ENDIF
      PLZAP0=T_BORN(0,SVAR,COSTHE,1D0,1D0)
     $  /(T_BORN(0,SVAR,COSTHE,1D0,1D0)+T_BORN(0,SVAR,COSTHE,-1D0,-1D0))

!      PLZAP0=0.5
      END
      FUNCTION T_BORN(MODE,SVAR,COSTHE,TA,TB)
C ----------------------------------------------------------------------
C THIS ROUTINE PROVIDES BORN CROSS SECTION. IT HAS THE SAME         
C STRUCTURE AS FUNTIS AND FUNTIH, THUS CAN BE USED AS SIMPLER       
C EXAMPLE OF THE METHOD APPLIED THERE                               
C INPUT PARAMETERS ARE: SVAR    -- transfer
C                       COSTHE  -- cosine of angle between tau+ and 1st beam
C                       TA,TB   -- helicity states of tau+ tau-
C
C     called by : BORNY, BORAS, BORNV, WAGA, WEIGHT
C ----------------------------------------------------------------------
      IMPLICIT REAL*8(A-H,O-Z)
      COMMON / T_BEAMPM / ENE ,AMIN,AMFIN,IDE,IDF
      REAL*8              ENE ,AMIN,AMFIN
      COMMON / T_GAUSPM /SS,POLN,T3E,QE,T3F,QF
     &                  ,XUPGI   ,XUPZI   ,XUPGF   ,XUPZF
     &                  ,NDIAG0,NDIAGA,KEYA,KEYZ
     &                  ,ITCE,JTCE,ITCF,JTCF,KOLOR
      REAL*8             SS,POLN,T3E,QE,T3F,QF
     &                  ,XUPGI(2),XUPZI(2),XUPGF(2),XUPZF(2)
      REAL*8            SEPS1,SEPS2
C=====================================================================
      COMMON / T_GSWPRM /SWSQ,AMW,AMZ,AMH,AMTOP,GAMMZ
      REAL*8             SWSQ,AMW,AMZ,AMH,AMTOP,GAMMZ
C     SWSQ        = sin2 (theta Weinberg)
C     AMW,AMZ     = W & Z boson masses respectively
C     AMH         = the Higgs mass
C     AMTOP       = the top mass
C     GAMMZ       = Z0 width
      COMPLEX*16 ABORN(2,2),APHOT(2,2),AZETT(2,2)
      COMPLEX*16 XUPZFP(2),XUPZIP(2)
      COMPLEX*16 ABORNM(2,2),APHOTM(2,2),AZETTM(2,2)
      COMPLEX*16 PROPA,PROPZ
      COMPLEX*16 XR,XI
      COMPLEX*16 XUPF,XUPI,XFF(4),XFEM,XFOTA,XRHO,XKE,XKF,XKEF
      COMPLEX*16 XTHING,XVE,XVF,XVEF
      DATA XI/(0.D0,1.D0)/,XR/(1.D0,0.D0)/
      DATA MODE0 /-5/
      DATA IDE0 /-55/
      DATA SVAR0,COST0 /-5.D0,-6.D0/
      DATA PI /3.141592653589793238462643D0/
      DATA SEPS1,SEPS2 /0D0,0D0/
C
C MEMORIZATION =========================================================
      IF ( MODE.NE.MODE0.OR.SVAR.NE.SVAR0.OR.COSTHE.NE.COST0
     $    .OR.IDE0.NE.IDE)THEN
C
        KEYGSW=1
C ** PROPAGATORS
        IDE0=IDE
        MODE0=MODE
        SVAR0=SVAR
        COST0=COSTHE
        SINTHE=SQRT(1.D0-COSTHE**2)
        BETA=SQRT(MAX(0D0,1D0-4D0*AMFIN**2/SVAR))
C I MULTIPLY AXIAL COUPLING BY BETA FACTOR.
        XUPZFP(1)=0.5D0*(XUPZF(1)+XUPZF(2))+0.5*BETA*(XUPZF(1)-XUPZF(2))
        XUPZFP(2)=0.5D0*(XUPZF(1)+XUPZF(2))-0.5*BETA*(XUPZF(1)-XUPZF(2))
        XUPZIP(1)=0.5D0*(XUPZI(1)+XUPZI(2))+0.5*(XUPZI(1)-XUPZI(2))
        XUPZIP(2)=0.5D0*(XUPZI(1)+XUPZI(2))-0.5*(XUPZI(1)-XUPZI(2))
C FINAL STATE VECTOR COUPLING
        XUPF     =0.5D0*(XUPZF(1)+XUPZF(2))
        XUPI     =0.5D0*(XUPZI(1)+XUPZI(2))
        XTHING   =0D0

        PROPA =1D0/SVAR
        PROPZ =1D0/DCMPLX(SVAR-AMZ**2,SVAR/AMZ*GAMMZ)
        IF (KEYGSW.EQ.0) PROPZ=0.D0
        DO 50 I=1,2
         DO 50 J=1,2
          REGULA= (3-2*I)*(3-2*J) + COSTHE
          REGULM=-(3-2*I)*(3-2*J) * SINTHE *2.D0*AMFIN/SQRT(SVAR)
          APHOT(I,J)=PROPA*(XUPGI(I)*XUPGF(J)*REGULA)
          AZETT(I,J)=PROPZ*(XUPZIP(I)*XUPZFP(J)+XTHING)*REGULA
          ABORN(I,J)=APHOT(I,J)+AZETT(I,J)
          APHOTM(I,J)=PROPA*DCMPLX(0D0,1D0)*XUPGI(I)*XUPGF(J)*REGULM
          AZETTM(I,J)=PROPZ*DCMPLX(0D0,1D0)*(XUPZIP(I)*XUPF+XTHING)*REGULM
          ABORNM(I,J)=APHOTM(I,J)+AZETTM(I,J)
   50   CONTINUE
      ENDIF
C
C******************
C* IN CALCULATING CROSS SECTION ONLY DIAGONAL ELEMENTS
C* OF THE SPIN DENSITY MATRICES ENTER (LONGITUD. POL. ONLY.)
C* HELICITY CONSERVATION EXPLICITLY OBEYED
      POLAR1=  (SEPS1)
      POLAR2= (-SEPS2)
      BORN=0D0
      DO 150 I=1,2
       HELIC= 3-2*I
       DO 150 J=1,2
        HELIT=3-2*J
        FACTOR=KOLOR*(1D0+HELIC*POLAR1)*(1D0-HELIC*POLAR2)/4D0
        FACTOM=FACTOR*(1+HELIT*TA)*(1-HELIT*TB)
        FACTOR=FACTOR*(1+HELIT*TA)*(1+HELIT*TB)

        BORN=BORN+CDABS(ABORN(I,J))**2*FACTOR
C      MASS TERM IN BORN
        IF (MODE.GE.1) THEN
         BORN=BORN+CDABS(ABORNM(I,J))**2*FACTOM
        ENDIF

  150 CONTINUE
C************
      FUNT=BORN
      IF(FUNT.LT.0.D0)  FUNT=BORN

C
      IF (SVAR.GT.4D0*AMFIN**2) THEN
C PHASE SPACE THRESHOLD FACTOR
        THRESH=SQRT(1-4D0*AMFIN**2/SVAR)
        T_BORN= FUNT*SVAR**2*THRESH
      ELSE
        THRESH=0.D0
        T_BORN=0.D0
      ENDIF
C ZW HERE WAS AN ERROR 19. 05. 1989
!      write(*,*) 'KKKK ',PROPA,PROPZ,XUPGI,XUPGF,XUPZI,XUPZF
!      write(*,*) 'KKKK X',svar,costhe,TA,TB,T_BORN
      END

      SUBROUTINE INITWK(IDEX,IDFX,SVAR)
! initialization routine coupling masses etc.
      IMPLICIT REAL*8 (A-H,O-Z)
      COMMON / T_BEAMPM / ENE ,AMIN,AMFIN,IDE,IDF
      REAL*8              ENE ,AMIN,AMFIN
      COMMON / T_GAUSPM /SS,POLN,T3E,QE,T3F,QF
     &                  ,XUPGI   ,XUPZI   ,XUPGF   ,XUPZF
     &                  ,NDIAG0,NDIAGA,KEYA,KEYZ
     &                  ,ITCE,JTCE,ITCF,JTCF,KOLOR
      REAL*8             SS,POLN,T3E,QE,T3F,QF
     &                  ,XUPGI(2),XUPZI(2),XUPGF(2),XUPZF(2)
      COMMON / T_GSWPRM /SWSQ,AMW,AMZ,AMH,AMTOP,GAMMZ
      REAL*8             SWSQ,AMW,AMZ,AMH,AMTOP,GAMMZ
C     SWSQ        = sin2 (theta Weinberg)
C     AMW,AMZ     = W & Z boson masses respectively
C     AMH         = the Higgs mass
C     AMTOP       = the top mass
C     GAMMZ       = Z0 width
C
      ENE=SQRT(SVAR)/2
      AMIN=0.511D-3
      SWSQ=0.23147
      AMZ=91.1882
      GAMMZ=2.4952
      IF     (IDFX.EQ. 15) then       
        IDF=2  ! denotes tau +2 tau-
        AMFIN=1.77703 !this mass is irrelevant if small, used in ME only
      ELSEIF (IDFX.EQ.-15) then
        IDF=-2  ! denotes tau -2 tau-
        AMFIN=1.77703 !this mass is irrelevant if small, used in ME only
      ELSE
        WRITE(*,*) 'INITWK: WRONG IDFX'
        STOP
      ENDIF

      IF     (IDEX.EQ. 11) then      !electron
        IDE= 2
        AMIN=0.511D-3
      ELSEIF (IDEX.EQ.-11) then      !positron
        IDE=-2
        AMIN=0.511D-3
      ELSEIF (IDEX.EQ. 13) then      !mu+
        IDE= 2
        AMIN=0.105659
      ELSEIF (IDEX.EQ.-13) then      !mu-
        IDE=-2
        AMIN=0.105659
      ELSEIF (IDEX.EQ.  1) then      !d
        IDE= 4
        AMIN=0.05
      ELSEIF (IDEX.EQ.- 1) then      !d~
        IDE=-4
        AMIN=0.05
      ELSEIF (IDEX.EQ.  2) then      !u
        IDE= 3
        AMIN=0.02
      ELSEIF (IDEX.EQ.- 2) then      !u~
        IDE=-3
        AMIN=0.02
      ELSEIF (IDEX.EQ.  3) then      !s
        IDE= 4
        AMIN=0.3
      ELSEIF (IDEX.EQ.- 3) then      !s~
        IDE=-4
        AMIN=0.3
      ELSEIF (IDEX.EQ.  4) then      !c
        IDE= 3
        AMIN=1.3
      ELSEIF (IDEX.EQ.- 4) then      !c~
        IDE=-3
        AMIN=1.3
      ELSEIF (IDEX.EQ.  5) then      !b
        IDE= 4
        AMIN=4.5
      ELSEIF (IDEX.EQ.- 5) then      !b~
        IDE=-4
        AMIN=4.5
      ELSEIF (IDEX.EQ.  12) then     !nu_e
        IDE= 1
        AMIN=0.1D-3
      ELSEIF (IDEX.EQ.- 12) then     !nu_e~
        IDE=-1
        AMIN=0.1D-3
      ELSEIF (IDEX.EQ.  14) then     !nu_mu
        IDE= 1
        AMIN=0.1D-3
      ELSEIF (IDEX.EQ.- 14) then     !nu_mu~
        IDE=-1
        AMIN=0.1D-3
      ELSEIF (IDEX.EQ.  16) then     !nu_tau
        IDE= 1
        AMIN=0.1D-3
      ELSEIF (IDEX.EQ.- 16) then     !nu_tau~
        IDE=-1
        AMIN=0.1D-3

      ELSE
        WRITE(*,*) 'INITWK: WRONG IDEX'
        STOP
      ENDIF

C ----------------------------------------------------------------------
C
C     INITIALISATION OF COUPLING CONSTANTS AND FERMION-GAMMA / Z0 VERTEX
C
C     called by : KORALZ
C ----------------------------------------------------------------------
      ITCE=IDE/IABS(IDE)
      JTCE=(1-ITCE)/2
      ITCF=IDF/IABS(IDF)
      JTCF=(1-ITCF)/2
      CALL T_GIVIZO( IDE, 1,AIZOR,QE,KDUMM)
      CALL T_GIVIZO( IDE,-1,AIZOL,QE,KDUMM)
      XUPGI(1)=QE
      XUPGI(2)=QE
      T3E    = AIZOL+AIZOR
      XUPZI(1)=(AIZOR-QE*SWSQ)/SQRT(SWSQ*(1-SWSQ))
      XUPZI(2)=(AIZOL-QE*SWSQ)/SQRT(SWSQ*(1-SWSQ))
      CALL T_GIVIZO( IDF, 1,AIZOR,QF,KOLOR)
      CALL T_GIVIZO( IDF,-1,AIZOL,QF,KOLOR)
      XUPGF(1)=QF
      XUPGF(2)=QF
      T3F    =  AIZOL+AIZOR
      XUPZF(1)=(AIZOR-QF*SWSQ)/SQRT(SWSQ*(1-SWSQ))
      XUPZF(2)=(AIZOL-QF*SWSQ)/SQRT(SWSQ*(1-SWSQ))
C
      NDIAG0=2
      NDIAGA=11
      KEYA  = 1
      KEYZ  = 1
C
C
      RETURN
      END

      SUBROUTINE T_GIVIZO(IDFERM,IHELIC,SIZO3,CHARGE,KOLOR)
C ----------------------------------------------------------------------
C PROVIDES ELECTRIC CHARGE AND WEAK IZOSPIN OF A FAMILY FERMION
C IDFERM=1,2,3,4 DENOTES NEUTRINO, LEPTON, UP AND DOWN QUARK
C NEGATIVE IDFERM=-1,-2,-3,-4, DENOTES ANTIPARTICLE
C IHELIC=+1,-1 DENOTES RIGHT AND LEFT HANDEDNES ( CHIRALITY)
C SIZO3 IS THIRD PROJECTION OF WEAK IZOSPIN (PLUS MINUS HALF)
C AND CHARGE IS ELECTRIC CHARGE IN UNITS OF ELECTRON CHARGE
C KOLOR IS A QCD COLOUR, 1 FOR LEPTON, 3 FOR QUARKS
C
C     called by : EVENTE, EVENTM, FUNTIH, .....
C ----------------------------------------------------------------------
      IMPLICIT REAL*8(A-H,O-Z)
C
      IF(IDFERM.EQ.0.OR.IABS(IDFERM).GT.4) GOTO 901
      IF(IABS(IHELIC).NE.1)                GOTO 901
      IH  =IHELIC
      IDTYPE =IABS(IDFERM)
      IC  =IDFERM/IDTYPE
      LEPQUA=INT(IDTYPE*0.4999999D0)
      IUPDOW=IDTYPE-2*LEPQUA-1
      CHARGE  =(-IUPDOW+2D0/3D0*LEPQUA)*IC
      SIZO3   =0.25D0*(IC-IH)*(1-2*IUPDOW)
      KOLOR=1+2*LEPQUA
C** NOTE THAT CONVENTIONALY Z0 COUPLING IS
C** XOUPZ=(SIZO3-CHARGE*SWSQ)/SQRT(SWSQ*(1-SWSQ))
      RETURN
 901  PRINT *,' STOP IN GIVIZO: WRONG PARAMS.'
      STOP
      END
#include "../../include/phyfix.h"
      SUBROUTINE FILHEP(N,IST,ID,JMO1,JMO2,JDA1,JDA2,P4,PINV,PHFLAG)
C ----------------------------------------------------------------------
C this subroutine fills one entry into the HEPEVT common
C and updates the information for affected mother entries
C
C written by Martin W. Gruenewald (91/01/28)
C
C     called by : ZTOHEP,BTOHEP,DWLUxy
C ----------------------------------------------------------------------
C
C this is the hepevt class in old style. No d_h_ class pre-name
#include "../../include/HEPEVT.h"
      LOGICAL PHFLAG
C
      REAL*4  P4(4)
C
C check address mode
      IF (N.EQ.0) THEN
C
C append mode
        IHEP=NHEP+1
      ELSE IF (N.GT.0) THEN
C
C absolute position
        IHEP=N
      ELSE
C
C relative position
        IHEP=NHEP+N
      END IF
C
C check on IHEP
      IF ((IHEP.LE.0).OR.(IHEP.GT.NMXHEP)) RETURN
C
C add entry
      NHEP=IHEP
      ISTHEP(IHEP)=IST
      IDHEP(IHEP)=ID
      JMOHEP(1,IHEP)=JMO1
      IF(JMO1.LT.0)JMOHEP(1,IHEP)=JMOHEP(1,IHEP)+IHEP
      JMOHEP(2,IHEP)=JMO2
      IF(JMO2.LT.0)JMOHEP(2,IHEP)=JMOHEP(2,IHEP)+IHEP
      JDAHEP(1,IHEP)=JDA1
      JDAHEP(2,IHEP)=JDA2
C
      DO I=1,4
        PHEP(I,IHEP)=P4(I)
C
C KORAL-B and KORAL-Z do not provide vertex and/or lifetime informations
        VHEP(I,IHEP)=0.0
      END DO
      PHEP(5,IHEP)=PINV
C FLAG FOR PHOTOS...
      QEDRAD(IHEP)=PHFLAG
C
C update process:
      DO IP=JMOHEP(1,IHEP),JMOHEP(2,IHEP)
        IF(IP.GT.0)THEN
C
C if there is a daughter at IHEP, mother entry at IP has decayed
          IF(ISTHEP(IP).EQ.1)ISTHEP(IP)=2
C
C and daughter pointers of mother entry must be updated
          IF(JDAHEP(1,IP).EQ.0)THEN
            JDAHEP(1,IP)=IHEP
            JDAHEP(2,IP)=IHEP
          ELSE
            JDAHEP(2,IP)=MAX(IHEP,JDAHEP(2,IP))
          END IF
        END IF
      END DO
C
      RETURN
      END


      FUNCTION IHEPDIM(DUM) 
C this is the hepevt class in old style. No d_h_ class pre-name
#include "../../include/HEPEVT.h"
      IHEPDIM=NHEP
      END
      FUNCTION ZPROP2(S)
      IMPLICIT REAL*8(A-H,O-Z)
      COMPLEX*16 CPRZ0,CPRZ0M
      AMZ=91.1882
      GAMMZ=2.49
      CPRZ0=DCMPLX((S-AMZ**2),S/AMZ*GAMMZ)
      CPRZ0M=1/CPRZ0
      ZPROP2=(ABS(CPRZ0M))**2
      END

      SUBROUTINE TAUPI0(MODE,JAK,ION)
C no initialization required. Must be called once after every:
C   1)    CALL DEKAY(1+10,...)
C   2)    CALL DEKAY(2+10,...)
C   3)    CALL DEXAY(1,...)
C   4)    CALL DEXAY(2,...)
C subroutine to decay originating from TAUOLA's taus: 
C 1) etas (with CALL TAUETA(JAK))
C 2) later pi0's from taus.
C 3) extensions to other applications possible. 
C this routine belongs to >tauola universal interface<, but uses 
C routines from >tauola< utilities as well.  25.08.2005      
#include "../../include/HEPEVT.h"

C position of taus, must be defined by host program:
      COMMON /TAUPOS/ NP1,NP2
c
      REAL  PHOT1(4),PHOT2(4)
      REAL*8  R,X(4),Y(4),PI0(4)
      INTEGER JEZELI(3),ION(3)
      DATA JEZELI /0,0,0/
      SAVE JEZELI
      IF (MODE.EQ.-1) THEN
        JEZELI(1)=ION(1)
        JEZELI(2)=ION(2)
        JEZELI(3)=ION(3)
        RETURN
      ENDIF
      IF (JEZELI(1).EQ.0) RETURN
      IF (JEZELI(2).EQ.1) CALL TAUETA(JAK)
      IF (JEZELI(3).EQ.1) CALL TAUK0S(JAK)
C position of decaying particle:
      IF((KTO.EQ. 1).OR.(KTO.EQ.11)) THEN
        NPS=NP1
      ELSE
        NPS=NP2
      ENDIF
      nhepM=nhep                ! to avoid infinite loop
      DO K=JDAHEP(1,NPS),nhepM  ! we search for pi0's from tau till eor.
       IF (IDHEP(K).EQ.111.AND.JDAHEP(1,K).LE.K) THEN ! IF we found pi0
        DO L=1,4
          PI0(L)= phep(L,K)
        ENDDO
! random 3 vector on the sphere, masless
        R=SQRT(PI0(4)**2-PI0(3)**2-PI0(2)**2-PI0(1)**2)/2D0
        CALL SPHERD(R,X)
        X(4)=R
        Y(4)=R
        
        Y(1)=-X(1)
        Y(2)=-X(2)
        Y(3)=-X(3)
! boost to lab and to real*4
        CALL bostdq(-1,PI0,X,X)
        CALL bostdq(-1,PI0,Y,Y)
        DO L=1,4
         PHOT1(L)=X(L)
         PHOT2(L)=Y(L)
        ENDDO
C to hepevt
        CALL FILHEP(0,1,22,K,K,0,0,PHOT1,0.0,.TRUE.)
        CALL FILHEP(0,1,22,K,K,0,0,PHOT2,0.0,.TRUE.)
       ENDIF
      ENDDO
C
      END
      SUBROUTINE TAUETA(JAK)
C subroutine to decay etas's from taus. 
C this routine belongs to tauola universal interface, but uses 
C routines from tauola utilities. Just flat phase space, but 4 channels.
C it is called at the beginning of SUBR. TAUPI0(JAK)
C and as far as hepevt search it is basically the same as TAUPI0.  25.08.2005    
#include "../../include/HEPEVT.h"
      COMMON / PARMAS / AMTAU,AMNUTA,AMEL,AMNUE,AMMU,AMNUMU
     *                 ,AMPIZ,AMPI,AMRO,GAMRO,AMA1,GAMA1
     *                 ,AMK,AMKZ,AMKST,GAMKST
*
      REAL*4            AMTAU,AMNUTA,AMEL,AMNUE,AMMU,AMNUMU
     *                 ,AMPIZ,AMPI,AMRO,GAMRO,AMA1,GAMA1
     *                 ,AMK,AMKZ,AMKST,GAMKST

C position of taus, must be defined by host program:
      COMMON /TAUPOS/ NP1,NP2
c
      REAL  RRR(1),BRSUM(3), RR(2)
      REAL  PHOT1(4),PHOT2(4),PHOT3(4)
      REAL*8    X(4),    Y(4),    Z(4)
      REAL                                YM1,YM2,YM3
      REAL*8  R,RU,PETA(4),XM1,XM2,XM3,XM,XLAM
      REAL*8 a,b,c
      XLAM(a,b,c)=SQRT(ABS((a-b-c)**2-4.0*b*c))
C position of decaying particle:
      IF((KTO.EQ. 1).OR.(KTO.EQ.11)) THEN
        NPS=NP1
      ELSE
        NPS=NP2
      ENDIF
      nhepM=nhep                ! to avoid infinite loop              
      DO K=JDAHEP(1,NPS),nhepM  ! we search for etas's from tau till eor.
       IF (IDHEP(K).EQ.221.AND.JDAHEP(1,K).LE.K) THEN ! IF we found eta
        DO L=1,4
          PETA(L)= phep(L,K)  ! eta 4 momentum
        ENDDO
C       eta cumulated branching ratios:
        BRSUM(1)=0.389  ! gamma gamma
        BRSUM(2)=BRSUM(1)+0.319  ! 3 pi0
        BRSUM(3)=BRSUM(2)+0.237  ! pi+ pi- pi0 rest is thus pi+pi-gamma
        CALL RANMAR(RRR,1) 
        
        IF (RRR(1).LT.BRSUM(1)) THEN ! gamma gamma channel exactly like pi0
! random 3 vector on the sphere, masless   
         R=SQRT(PETA(4)**2-PETA(3)**2-PETA(2)**2-PETA(1)**2)/2D0
         CALL SPHERD(R,X) 
         X(4)=R
         Y(4)=R
        
         Y(1)=-X(1)
         Y(2)=-X(2)
         Y(3)=-X(3)
! boost to lab and to real*4
         CALL bostdq(-1,PETA,X,X)  
         CALL bostdq(-1,PETA,Y,Y)
         DO L=1,4
          PHOT1(L)=X(L)
          PHOT2(L)=Y(L)
         ENDDO
C to hepevt
         CALL FILHEP(0,1,22,K,K,0,0,PHOT1,0.0,.TRUE.)
         CALL FILHEP(0,1,22,K,K,0,0,PHOT2,0.0,.TRUE.)
        ELSE ! 3 body channels
         IF(RRR(1).LT.BRSUM(2)) THEN  ! 3 pi0
          ID1= 111
          ID2= 111
          ID3= 111
          XM1=AMPIZ ! masses
          XM2=AMPIZ
          XM3=AMPIZ
         ELSEIF(RRR(1).LT.BRSUM(3)) THEN ! pi+ pi- pi0
          ID1= 211
          ID2=-211
          ID3= 111
          XM1=AMPI ! masses
          XM2=AMPI
          XM3=AMPIZ
         ELSE                            ! pi+ pi- gamma 
          ID1= 211
          ID2=-211
          ID3=  22
          XM1=AMPI ! masses
          XM2=AMPI
          XM3=0.0
         ENDIF
 7       CONTINUE  ! we generate mass of the first pair:
          CALL RANMAR(RR,2)
          R=SQRT(PETA(4)**2-PETA(3)**2-PETA(2)**2-PETA(1)**2)
          AMIN=XM1+XM2
          AMAX=R-XM3
          AM2=SQRT(AMIN**2+RR(1)*(AMAX**2-AMIN**2))
C         weight for flat phase space
          WT=XLAM(1D0*R**2,1D0*AM2**2,1D0*XM3**2)
     &      *XLAM(1D0*AM2**2,1D0*XM1**2,1D0*XM2**2)
     &           /R**2                    /AM2**2
         IF (RR(2).GT.WT) GOTO 7

         RU=XLAM(1D0*AM2**2,1D0*XM1**2,1D0*XM2**2)/AM2/2  ! momenta of the
                                              ! first two products
                                              ! in the rest frame of that pair
         CALL SPHERD(RU,X)
         X(4)=SQRT(RU**2+XM1**2)
         Y(4)=SQRT(RU**2+XM2**2)
        
         Y(1)=-X(1)
         Y(2)=-X(2)
         Y(3)=-X(3)
C generate momentum of that pair in rest frame of eta:
         RU=XLAM(1D0*R**2,1D0*AM2**2,1D0*XM3**2)/R/2
         CALL SPHERD(RU,Z)
         Z(4)=SQRT(RU**2+AM2**2)
C and boost first two decay products to rest frame of eta.
         CALL bostdq(-1,Z,X,X)
         CALL bostdq(-1,Z,Y,Y)
C redefine Z(4) to 4-momentum of the last decay product: 
         Z(1)=-Z(1)
         Z(2)=-Z(2)
         Z(3)=-Z(3)
         Z(4)=SQRT(RU**2+XM3**2)
C boost all to lab and move to real*4; also masses
         CALL bostdq(-1,PETA,X,X)
         CALL bostdq(-1,PETA,Y,Y)
         CALL bostdq(-1,PETA,Z,Z)
         DO L=1,4
          PHOT1(L)=X(L)
          PHOT2(L)=Y(L)
          PHOT3(L)=Z(L)
         ENDDO
         YM1=XM1
         YM2=XM2
         YM3=XM3
C to hepevt
         CALL FILHEP(0,1,ID1,K,K,0,0,PHOT1,YM1,.TRUE.)
         CALL FILHEP(0,1,ID2,K,K,0,0,PHOT2,YM2,.TRUE.)
         CALL FILHEP(0,1,ID3,K,K,0,0,PHOT3,YM3,.TRUE.)
        ENDIF

       ENDIF
      ENDDO
C
      END
      SUBROUTINE TAUK0S(JAK)
C subroutine to decay K0S's from taus. 
C this routine belongs to tauola universal interface, but uses 
C routines from tauola utilities. Just flat phase space, but 4 channels.
C it is called at the beginning of SUBR. TAUPI0(JAK)
C and as far as hepevt search it is basically the same as TAUPI0.  25.08.2005    
#include "../../include/HEPEVT.h"

      COMMON / PARMAS / AMTAU,AMNUTA,AMEL,AMNUE,AMMU,AMNUMU
     *                 ,AMPIZ,AMPI,AMRO,GAMRO,AMA1,GAMA1
     *                 ,AMK,AMKZ,AMKST,GAMKST
*
      REAL*4            AMTAU,AMNUTA,AMEL,AMNUE,AMMU,AMNUMU
     *                 ,AMPIZ,AMPI,AMRO,GAMRO,AMA1,GAMA1
     *                 ,AMK,AMKZ,AMKST,GAMKST

C position of taus, must be defined by host program:
      COMMON /TAUPOS/ NP1,NP2
c
      REAL  RRR(1),BRSUM(3), RR(2)
      REAL  PHOT1(4),PHOT2(4),PHOT3(4)
      REAL*8    X(4),    Y(4),    Z(4)
      REAL                                YM1,YM2,YM3
      REAL*8  R,RU,PETA(4),XM1,XM2,XM3,XM,XLAM
      REAL*8 a,b,c
      XLAM(a,b,c)=SQRT(ABS((a-b-c)**2-4.0*b*c))
C position of decaying particle:
      IF((KTO.EQ. 1).OR.(KTO.EQ.11)) THEN
        NPS=NP1
      ELSE
        NPS=NP2
      ENDIF
      nhepM=nhep                ! to avoid infinite loop              
      DO K=JDAHEP(1,NPS),nhepM  ! we search for K0S's from tau till eor.
       IF (IDHEP(K).EQ.310.AND.JDAHEP(1,K).LE.K) THEN ! IF we found K0S

      
        DO L=1,4
          PETA(L)= phep(L,K)  ! K0S 4 momentum  (this is cloned from eta decay)
        ENDDO
C       K0S cumulated branching ratios:
        BRSUM(1)=0.313  ! 2 PI0
        BRSUM(2)=1.0 ! BRSUM(1)+0.319  ! Pi+ PI-
        BRSUM(3)=BRSUM(2)+0.237  ! pi+ pi- pi0 rest is thus pi+pi-gamma
        CALL RANMAR(RRR,1) 

         IF(RRR(1).LT.BRSUM(1)) THEN  ! 2 pi0
          ID1= 111
          ID2= 111
          XM1=AMPIZ ! masses
          XM2=AMPIZ
         ELSEIF(RRR(1).LT.BRSUM(2)) THEN ! pi+ pi- 
          ID1= 211
          ID2=-211
          XM1=AMPI ! masses
          XM2=AMPI
         ELSE                            ! gamma gamma unused !!!
          ID1= 22
          ID2= 22
          XM1= 0.0 ! masses
          XM2= 0.0
         ENDIF
        
! random 3 vector on the sphere, of equal mass !!  
         R=SQRT(PETA(4)**2-PETA(3)**2-PETA(2)**2-PETA(1)**2)/2D0
         R4=R
         R=SQRT(ABS(R**2-XM1**2))
         CALL SPHERD(R,X) 
         X(4)=R4
         Y(4)=R4
        
         Y(1)=-X(1)
         Y(2)=-X(2)
         Y(3)=-X(3)
! boost to lab and to real*4
         CALL bostdq(-1,PETA,X,X)  
         CALL bostdq(-1,PETA,Y,Y)
         DO L=1,4
          PHOT1(L)=X(L)
          PHOT2(L)=Y(L)
         ENDDO

         YM1=XM1
         YM2=XM2
C to hepevt
         CALL FILHEP(0,1,ID1,K,K,0,0,PHOT1,YM1,.TRUE.)
         CALL FILHEP(0,1,ID2,K,K,0,0,PHOT2,YM2,.TRUE.)

C
       ENDIF
      ENDDO

      END
