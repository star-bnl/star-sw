
C * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
C                                                                 *
C           G R S - LO - VIRTUAL PHOTON PARAMETRIZATIONS          *
C                                                                 *
C                 FOR A DETAILED EXPLANATION SEE                  *
C                M. GLUECK, E.REYA, M. STRATMANN :                *
C                    PHYS. REV. D51 (1995) 3220                   *
C                                                                 *
C   THE PARAMETRIZATIONS ARE FITTED TO THE EVOLVED PARTONS FOR    *
C        Q**2 / GEV**2  BETWEEN   0.6   AND  5.E4                 *
C                       AND (!)  Q**2 > 5 P**2                    *
C        P**2 / GEV**2  BETWEEN   0.0   AND  10.                  *
C                       P**2 = 0  <=> REAL PHOTON                 *
C             X         BETWEEN  1.E-4  AND   1.                  *
C                                                                 *
C   HEAVY QUARK THRESHOLDS  Q(H) = M(H)  IN THE BETA FUNCTION :   *
C                   M(C)  =  1.5,  M(B)  =  4.5                   *
C   CORRESPONDING LAMBDA(F) VALUES IN GEV FOR  Q**2 > M(H)**2 :   *
C      LO :   LAMBDA(3)  =  0.232,   LAMBDA(4)  =  0.200,         *
C             LAMBDA(5)  =  0.153,                                *
C   THE NUMBER OF ACTIVE QUARK FLAVOURS IS  NF = 3  EVERYWHERE    *
C   EXCEPT IN THE BETA FUNCTION, I.E. THE HEAVY QUARKS C,B,...    *
C   ARE NOT PRESENT AS PARTONS IN THE Q2-EVOLUTION.               *
C                                                                 *
C   PLEASE REPORT ANY STRANGE BEHAVIOUR TO :                      *
C                  Marco.Stratmann@durham.ac.uk                   *
C * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
C
C...INPUT PARAMETERS :
C
C    X   = MOMENTUM FRACTION
C    Q2  = SCALE Q**2 IN GEV**2
C    P2  = VIRTUALITY OF THE PHOTON IN GEV**2
C
C...OUTPUT (ALWAYS X TIMES THE DISTRIBUTION DIVIDED BY ALPHA_EM) :
C
C*******************************************************
C     subroutine grspar(x,q2,p2,ugam,dgam,sgam,ggam)
      SUBROUTINE PHO_DORGLV(X,Q2,P2,Ugam,Dgam,Sgam,Ggam)
      IMPLICIT NONE
      DOUBLE PRECISION Dgam , Ggam , P2 , Q2 , Sgam , Ugam , X
      SAVE 
 
C  input/output channels
      INCLUDE 'inc/poinou'
 
      INTEGER check
C
C     check limits :
C
      check = 0
      IF ( X.LT.0.0001D0 ) check = 1
      IF ( (Q2.LT.0.6D0) .OR. (Q2.GT.50000.D0) ) check = 1
      IF ( Q2.LT.5.D0*P2 ) check = 1
C
C     calculate distributions
C
      IF ( check.EQ.0 ) THEN
         CALL PHO_GRSCALC(X,Q2,P2,Ugam,Dgam,Sgam,Ggam)
      ELSE
         IF ( LPRi.GT.4 ) WRITE (LO,*)
     &         'GRS PDF parametrization: x/q2/p2 limits exceeded'
         IF ( LPRi.GT.4 ) WRITE (LO,'(1X,A,1P,3E12.3)')
     &         'current X, Q2, P2:' , X , Q2 , P2
      END IF
 
      END SUBROUTINE
