
      SUBROUTINE DT_SWPPHO(Ilab)
 
      IMPLICIT NONE
      DOUBLE PRECISION ela , ONE , pla , plab , TINY14 , TWO , umo , 
     &                 ZERO
      INTEGER i , icount , idp , idt , IDT_ICIHAD , Ilab , k
      SAVE 
 
      INCLUDE 'inc/dtflka'
 
      PARAMETER (ZERO=0.0D0,ONE=1.0D0,TWO=2.0D0,TINY14=1.0D-14)
 
      LOGICAL lstart
 
C event history
 
 
      INCLUDE 'inc/dtevt1'
C extended event history
      INCLUDE 'inc/dtevt2'
C flags for input different options
      INCLUDE 'inc/dtflg1'
C properties of photon/lepton projectiles
      INCLUDE 'inc/dtgpro'
 
C  standard particle data interface
      INCLUDE 'inc/poevt1'
C  extension to standard particle data interface (PHOJET specific)
      INCLUDE 'inc/poevt2'
 
C  global event kinematics and particle IDs
      INCLUDE 'inc/pogcms'
C*
      DATA icount/0/
 
      DATA lstart/.TRUE./
 
C     IF ((IFRAME.EQ.1).AND.(ILAB.EQ.0).AND.LSTART) THEN
      IF ( (IFRame.EQ.1) .AND. lstart ) THEN
         umo = ECM
         ela = ZERO
         pla = ZERO
         idp = IDT_ICIHAD(IFPap(1))
         idt = IDT_ICIHAD(IFPap(2))
         VIRt = PVIrt(1)
         CALL DT_LTINI(idp,idt,ela,pla,umo,0)
         plab = pla
         lstart = .FALSE.
      END IF
 
      NHKk = 0
      icount = icount + 1
C     NEVHKK = NEVHEP
      NEVhkk = icount
 
      IF ( LPRi.GT.4 .AND. MOD(icount,500).EQ.0 ) WRITE (LOUt,*)
     &      ' SWPPHO: event # ' , icount
      DO i = 3 , NHEp
         IF ( ISThep(i).EQ.1 ) THEN
            NHKk = NHKk + 1
            ISThkk(NHKk) = 1
            IDHkk(NHKk) = IDHep(i)
            JMOhkk(1,NHKk) = 0
            JMOhkk(2,NHKk) = 0
            JDAhkk(1,NHKk) = 0
            JDAhkk(2,NHKk) = 0
            DO k = 1 , 4
               PHKk(k,NHKk) = PHEp(k,i)
               VHKk(k,NHKk) = ZERO
               WHKk(k,NHKk) = ZERO
            END DO
            IF ( (IFRame.EQ.1) .AND. (Ilab.EQ.0) )
     &           CALL DT_LTNUC(PHEp(3,i),PHEp(4,i),PHKk(3,NHKk),
     &           PHKk(4,NHKk),-3)
            PHKk(5,NHKk) = PHEp(5,i)
            IDRes(NHKk) = 0
            IDXres(NHKk) = 0
            NOBam(NHKk) = 0
            IDBam(NHKk) = IDT_ICIHAD(IDHep(i))
            IDCh(NHKk) = 0
         END IF
      END DO
 
      END SUBROUTINE
