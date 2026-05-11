
      SUBROUTINE DT_NUC2CM
 
C***********************************************************************
C Lorentz-transformation of all wounded nucleons from Lab. to nucl.-   *
C nucl. cms. (This subroutine replaces NUCMOM.)                        *
C This version dated 15.01.95 is written by S. Roesler                 *
C***********************************************************************
 
      IMPLICIT NONE
      INTEGER i , id , ist , mode , nend
      DOUBLE PRECISION pe , px , py , pz , TINY3 , ZERO
      SAVE 
 
      INCLUDE 'inc/dtflka'
 
      PARAMETER (ZERO=0.0D0,TINY3=1.0D-3)
 
C event history
 
 
      INCLUDE 'inc/dtevt1'
C extended event history
      INCLUDE 'inc/dtevt2'
C statistics
      INCLUDE 'inc/dtsta1'
C properties of photon/lepton projectiles
      INCLUDE 'inc/dtgpro'
C particle properties (BAMJET index convention)
      INCLUDE 'inc/dtpart'
C Glauber formalism: collision properties
      INCLUDE 'inc/dtglcp'
C*temporary
C statistics: Glauber-formalism
      INCLUDE 'inc/dtsta3'
C*
 
      ICWp = 0
      ICWt = 0
      NWTacc = 0
      NWAacc = 0
      NWBacc = 0
 
      NPOint(1) = NHKk + 1
      nend = NHKk
      DO i = 1 , nend
         IF ( (ISThkk(i).EQ.11) .OR. (ISThkk(i).EQ.12) ) THEN
            IF ( ISThkk(i).EQ.11 ) NWAacc = NWAacc + 1
            IF ( ISThkk(i).EQ.12 ) NWBacc = NWBacc + 1
            mode = ISThkk(i) - 9
C            IF (IDHKK(I).EQ.22) THEN
C* VDM assumption
C               PEIN = AAM(33)
C               IDB  = 33
C            ELSE
C               PEIN = PHKK(4,I)
C               IDB  = IDBAM(I)
C            ENDIF
C            CALL DT_LTRANS(PHKK(1,I),PHKK(2,I),PHKK(3,I),PEIN,
C     &           PX,PY,PZ,PE,IDB,MODE)
            IF ( PHKk(5,i).GT.ZERO ) THEN
               CALL DT_LTRANS(PHKk(1,i),PHKk(2,i),PHKk(3,i),PHKk(4,i),
     &                        px,py,pz,pe,IDBam(i),mode)
            ELSE
               px = PGAmm(1)
               py = PGAmm(2)
               pz = PGAmm(3)
               pe = PGAmm(4)
            END IF
            ist = ISThkk(i) - 2
            id = IDHkk(i)
C* VDM assumption
C            IF (ID.EQ.22) ID = 113
            CALL DT_EVTPUT(ist,id,i,0,px,py,pz,pe,0,0,0)
            IF ( ISThkk(i).EQ.11 ) ICWp = ICWp + 1
            IF ( ISThkk(i).EQ.12 ) ICWt = ICWt + 1
         END IF
      END DO
 
      NWTacc = MAX(NWAacc,NWBacc)
      ICDpr = ICDpr + ICWp
      ICDta = ICDta + ICWt
C*temporary
      IF ( (ICWp.EQ.0) .OR. (ICWt.EQ.0) ) THEN
         CALL DT_EVTOUT(4)
         STOP
      END IF
 
      END SUBROUTINE
