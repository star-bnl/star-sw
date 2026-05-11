
      SUBROUTINE DT_FER4M(Pferm,Pbind,Pxt,Pyt,Pzt,Et,Kt)
 
C***********************************************************************
C Sampling of nucleon Fermi-momenta from distributions at T=0.         *
C                                   processed by S. Roesler, 17.10.95  *
C***********************************************************************
 
      IMPLICIT NONE
      DOUBLE PRECISION cfe , cxta , cyta , czta , Et , pabs , Pbind , 
     &                 Pferm , polc , pols , Pxt , Pyt , Pzt , sfe
      INTEGER iloop , Kt
      SAVE 
 
      INCLUDE 'inc/dtflka'
 
      LOGICAL lstart
 
C particle properties (BAMJET index convention)
      INCLUDE 'inc/dtpart'
C nuclear potential
      INCLUDE 'inc/dtnpot'
 
      DATA lstart/.TRUE./
 
      iloop = 0
      IF ( LFErmi ) THEN
         IF ( lstart ) THEN
 
            IF ( LPRi.GT.4 ) WRITE (LOUt,99010)
99010       FORMAT (/,1X,'FER4M:   sampling of Fermi-momenta activated')
            lstart = .FALSE.
         END IF
C   1    CONTINUE
         CALL DT_DFERMI(pabs)
         pabs = Pferm*pabs
C        IF (PABS.GE.PBIND) THEN
C           ILOOP = ILOOP+1
C           IF (MOD(ILOOP,500).EQ.0) THEN
C              WRITE(LOUT,1001) PABS,PBIND,ILOOP
C1001          FORMAT(1X,'FER4M:    Fermi-mom. corr. for binding',
C    &                ' energy ',2E12.3,I6)
C           ENDIF
C           GOTO 1
C        ENDIF
         CALL DT_DPOLI(polc,pols)
         CALL DT_DSFECF(sfe,cfe)
         cxta = pols*cfe
         cyta = pols*sfe
         czta = polc
         Et = SQRT(pabs*pabs+AAM(Kt)**2)
         Pxt = cxta*pabs
         Pyt = cyta*pabs
         Pzt = czta*pabs
      ELSE
         Et = AAM(Kt)
         Pxt = 0.0D+00
         Pyt = 0.0D+00
         Pzt = 0.0D+00
      END IF
 
      END SUBROUTINE
