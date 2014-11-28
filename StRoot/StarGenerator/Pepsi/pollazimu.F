#define RLU(x) RLUSTAR(x)
*72*********************************************************************
      SUBROUTINE POLLAZIMU(XP,ZP)

      IMPLICIT NONE

C...Choose azimuthal angle (PHI) according to QCD matrix elements.


*
* to avoid variable conflictions, a second keep element is necessary
* with the same common block name (see LPTOU2)
*
*
* to avoid variable conflictions, a second keep element is necessary
* with the same common block name (see LPTOU2)
*
      COMMON /LEPTOU/ CUT(14),LST(40),PARL(30),
     &                X,Y,W2,Q2,U
      REAL CUT,PARL,X,Y,W2,Q2,U
      INTEGER LST
      SAVE /LEPTOU/

      COMMON /LINTER/ PARI(50),EWQC(2,2,8),QC(8),ZL(2,4),ZQ(2,8),PQ(17)
      REAL  PARI,EWQC,QC,ZL,ZQ,PQ
      SAVE /LINTER/


*...Two additional (local) arrays to store the values of the
*...parton distriutions at XI=X/XP
      INTEGER J,IFL,I,IH,IEVFL
      REAL XPQ1,XP,ZP,DQCD,POLDQCD,A,B,C,XDPQ1,PHIMAX,PHI,
     +XI,SGN
      REAL RLU,RLUSTAR

      DIMENSION XPQ1(-6:6),XDPQ1(-6:6)
*---

      J=LST(24)-1
      SGN=SIGN(1.,2.5-LST(24))
      IFL=LST(25)
      I=IABS(IFL)
      IH=1
      IF(LST(30).EQ.1) IH=2
      
      IF(LST(23).EQ.2) THEN
        A=PARI(24)*DQCD(0,J,1,XP,ZP,Y)+PARI(25)*DQCD(0,J,2,XP,ZP,Y)
     &  -LST(30)*ISIGN(1,IFL)*PARI(26)*DQCD(0,J,3,XP,ZP,Y)
        B=DQCD(1,J,1,XP,ZP,Y)
     &  +SGN*LST(30)*ISIGN(1,IFL)*DQCD(1,J,3,XP,ZP,Y)
        C=DQCD(2,J,1,XP,ZP,Y)

      ELSEIF(LST(23).EQ.1) THEN
       XI=X/XP
       CALL LNSTRF(XI,Q2,XPQ1)
        CALL DNSTRF(XI,Q2,XDPQ1)

       IF(LST(24).EQ.2)THEN
         IEVFL=IFL
       ELSEIF(LST(24).EQ.3)THEN
         IEVFL=0
       ENDIF

        A= XPQ1(IEVFL)* 
     &     (PARI(24)*DQCD(0,J,1,XP,ZP,Y)+PARI(25)*DQCD(0,J,2,XP,ZP,Y))
     &  + (FLOAT(LST(40))*XDPQ1(IEVFL)*PARI(26)*POLDQCD(0,J,3,XP,ZP,Y))
       B= XPQ1(IEVFL)   *DQCD(1,J,1,XP,ZP,Y)
     &   +FLOAT(LST(40))*XDPQ1(IEVFL)*POLDQCD(1,J,3,XP,ZP,Y)
       C= XPQ1(IEVFL)   *DQCD(2,J,1,XP,ZP,Y)

      ENDIF

      PHIMAX=ABS(A)+ABS(B)+ABS(C)
   
      IF(XDPQ1(IEVFL).gt. XPQ1(IEVFL).and.
     &     .NOT.(LST(15).ge.107.and.LST(15).le.109)) THEN
         WRITE(*,*) 'Trouble in POLLAZIMU!'
         STOP 
      ENDIF
      
 100  PHI=6.2832*RLU(0)
      IF(A+B*COS(PHI)+C*COS(2.*PHI).LT.RLU(0)*PHIMAX) GOTO 100
      CALL LUROBO(0.,PHI,0.,0.,0.)

CAE.Store ME variables
      PARL(28)=XP
      PARL(29)=ZP
      PARL(30)=PHI
CAE
      RETURN
      END
