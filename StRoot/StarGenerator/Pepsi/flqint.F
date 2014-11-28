
C **********************************************************************

      FUNCTION FLQINT(Z)

C...Quark contribution integrand to QCD longitudinal structure function.


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

      COMMON /LINTEG/ NTOT,NPASS
      INTEGER NTOT,NPASS
      SAVE /LINTEG/


      INTEGER I
      REAL XPQ,Z,FLQINT,PI
      DIMENSION XPQ(-6:6)
      DATA PI/3.14159/
      NTOT=NTOT+1
      CALL LNSTRF(Z,Q2,XPQ)
      FLQINT=0.
      DO 100 I=-LST(12),LST(12)
      IF(I.EQ.0) GOTO 100
      FLQINT=FLQINT+QC(IABS(I))**2*XPQ(I)
  100 CONTINUE
      FLQINT=4./3.*PARL(25)/PI*(X/Z)**2*FLQINT/Z
      NPASS=NPASS+1

      RETURN
      END
