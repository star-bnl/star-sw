      SUBROUTINE KALEX(STEP,RL,PL,CL,R,P,C,IDGEANT,IRET)
*-----------------------------------------------------------------------------
*--
*--   tracking to an inactive detector surface
*--
*--   Input:
*--     RL     ... current radius
*--     PL(5)  ... current reference track
*--     CL(15) ... current cov. matrix
*--     R      ... new radius (<=RL)
*--
*--   Output:
*--     P(5)   ... new reference track
*--     C(15)  ... new cov. matrix
*--
*--   P(1) ...  phi*R
*--   P(2) ...  z
*--   P(3) ...  tanl
*--   P(4) ...  psi
*--   P(5) ...  q/pt
*-- 

C* the correspondence between the COV(15) and COV(5,5) is
C* ( 1  2  3  4  5)  -  phi*R
C* ( 2  6  7  8  9)  -  z
C* ( 3  7 10 11 12)  -  tanl
C* ( 4  8 11 13 14)  -  psi
C* ( 5  9 12 14 15)  -  q/pt

*--   A.C. Saulys June, 1999
*--
*------------------------------------------------------------------------------
*--

      IMPLICIT NONE

      INTEGER IGPART ! GEANT particle ID
      COMMON/GPARID/IGPART

*--
      REAL PL(*),P(*),CL(*)
      REAL R,RL,DR,X,COSL,REMAIN,R1,R2,STEP
      REAL DER(5,5),CMS(15),STEP,P1(5),P2(5)
      DOUBLE PRECISION C1(15),C2(15),C(*),DOUBDER(5,5)

      INTEGER IRET,IDGEANT,IERR,NSTEPS,I,J,ISTEP
*--
      IF (RL.LT.R) THEN
         IRET=100
         RETURN
      ENDIF

      IGPART = IDGEANT
*--
*--   track model along reference surface
*--
*--   track in steps of STEP in r so that E loss can be accounted

      NSTEPS = (RL-R)/STEP
      REMAIN = (RL-R)-NSTEPS*STEP
      R1 = RL
      DO I = 1,5
       P1(I) = PL(I)
      ENDDO
      DO I = 1,15
       C1(I) = CL(I)
      ENDDO
      COSL = 1./SQRT(1.+PL(3)**2)

      DO ISTEP=1,NSTEPS+1
       IF(ISTEP.LE.NSTEPS)THEN
        DR = STEP
       ELSE
        DR = REMAIN
       ENDIF
       R2 = R1 - DR
       CALL VXBTRD(R1,P1,R2,P2,DER,IERR)
       IF (IERR.GT.0) THEN
        IRET = 200+IERR
        RETURN
       ENDIF

       DO I = 1,5
       DO J = 1,5
         DOUBDER(I,J) = DER(I,J)
       ENDDO
       ENDDO

*--    Return propagated covariance matrix
       CALL KMFDC5(DOUBDER,C1,C2)

*--    Calculate multiple scattering contribution
       CALL TPCSCA(R2,R1,P2,P1,CMS)

*--    Update parameter vector
       DO I=1,5
        P1(I)=P2(I)
       ENDDO
       R1 = R2

*--    Add MS
       C1(1) = C2(1) + CMS(1)
       C1(6) = C2(6) + CMS(6)
       C1(10) = C2(10) + CMS(10)
       C1(13) = C2(13) + CMS(13)
       C1(15) = C2(15) + CMS(15)

      ENDDO

      DO I=1,15
        C(I)=C1(I)
      ENDDO
      DO I=1,5
        P(I)=P1(I)
      ENDDO

      IRET = 0
      RETURN
*--
      END

