
      SUBROUTINE DT_SIGGAT(Q2i,Ecmi,Stot,Nt)
 
C***********************************************************************
C Total/inelastic photon-nucleus cross sections.                       *
C Uses pre-tabulated cross section.                                    *
C This version dated 29.07.96 is written by S. Roesler                 *
C***********************************************************************
 
      IMPLICIT NONE
      DOUBLE PRECISION Ecmi , ONE , Q2i , rate , ratq , Stot , TINY10 , 
     &                 TINY14 , TWO , ZERO
      INTEGER i , i1 , i2 , j1 , j2 , Nt , ntarg
      SAVE 
 
      INCLUDE 'inc/dtflka'
 
      PARAMETER (TINY10=1.0D-10,TINY14=1.0D-14,ZERO=0.0D0,ONE=1.0D0,
     &           TWO=2.0D0)
 
C emulsion treatment
      INCLUDE 'inc/dtcomp'
 
C Glauber formalism: cross sections
      INCLUDE 'inc/dtglxs'
 
      ntarg = ABS(Nt)
      i1 = 1
      i2 = 1
      rate = ONE
      IF ( NEBini.GT.1 ) THEN
         IF ( Ecmi.GE.ECMnn(NEBini) ) THEN
            i1 = NEBini
            i2 = NEBini
            rate = ONE
         ELSE IF ( Ecmi.GT.ECMnn(1) ) THEN
            DO i = 2 , NEBini
               IF ( Ecmi.LT.ECMnn(i) ) THEN
                  i1 = i - 1
                  i2 = i
                  rate = (Ecmi-ECMnn(i1))/(ECMnn(i2)-ECMnn(i1))
                  GOTO 100
               END IF
            END DO
         END IF
      END IF
 100  j1 = 1
      j2 = 1
      ratq = ONE
      IF ( NQBini.GT.1 ) THEN
         IF ( Q2i.GE.Q2G(NQBini) ) THEN
            j1 = NQBini
            j2 = NQBini
            ratq = ONE
         ELSE IF ( Q2i.GT.Q2G(1) ) THEN
            DO i = 2 , NQBini
               IF ( Q2i.LT.Q2G(i) ) THEN
                  j1 = i - 1
                  j2 = i
                  ratq = LOG10(Q2i/MAX(Q2G(j1),TINY14))
     &                   /LOG10(Q2G(j2)/MAX(Q2G(j1),TINY14))
C                 RATQ = (Q2I-Q2G(J1))/(Q2G(J2)-Q2G(J1))
                  GOTO 200
               END IF
            END DO
         END IF
      END IF
 
 200  Stot = XSTot(i1,j1,ntarg)
     &       + rate*(XSTot(i2,j1,ntarg)-XSTot(i1,j1,ntarg))
     &       + ratq*(XSTot(i1,j2,ntarg)-XSTot(i1,j1,ntarg))
     &       + rate*ratq*(XSTot(i2,j2,ntarg)-XSTot(i1,j2,ntarg)
     &       +XSTot(i1,j1,ntarg)-XSTot(i2,j1,ntarg))
 
      END SUBROUTINE
