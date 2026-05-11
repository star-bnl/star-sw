
      SUBROUTINE DT_EVTPUT(Ist,Id,M1,M2,Px,Py,Pz,E,Idr,Idxr,Idc)
 
      IMPLICIT NONE
      DOUBLE PRECISION E , ptot , Px , Py , Pz , SQTINF , TINY10 , 
     &                 TINY2 , TINY3 , TINY4 , ZERO
      INTEGER i , Id , Idc , idchk , idmo1 , idmo2 , Idr , IDT_ICIHAD , 
     &        Idxr , Ist , M1 , M2 , mo1 , mo2
      SAVE 
 
      INCLUDE 'inc/dtflka'
 
      PARAMETER (TINY10=1.0D-10,TINY4=1.0D-4,TINY3=1.0D-3,TINY2=1.0D-2,
     &           SQTINF=1.0D+15,ZERO=0.0D0)
 
C event history
 
 
      INCLUDE 'inc/dtevt1'
C extended event history
      INCLUDE 'inc/dtevt2'
C Lorentz-parameters of the current interaction
      INCLUDE 'inc/dtltra'
C particle properties (BAMJET index convention)
      INCLUDE 'inc/dtpart'
 
C     IF (MODE.GT.100) THEN
C        WRITE(LOUT,'(1X,A,I5,A,I5)')
C    &        'EVTPUT: reset NHKK = ',NHKK,' to NHKK =',NHKK-MODE+100
C        NHKK = NHKK-MODE+100
C        RETURN
C     ENDIF
      mo1 = M1
      mo2 = M2
      NHKk = NHKk + 1
 
      IF ( NHKk.GT.NMXHKK ) THEN
 
         IF ( LPRi.GT.4 ) WRITE (LOUt,99010) NHKk
99010    FORMAT (1X,'EVTPUT: NHKK exeeds NMXHKK = ',I7,
     &           '! program execution stopped..')
         STOP
      END IF
      IF ( M1.LT.0 ) mo1 = NHKk + M1
      IF ( M2.LT.0 ) mo2 = NHKk + M2
      ISThkk(NHKk) = Ist
      IDHkk(NHKk) = Id
      JMOhkk(1,NHKk) = mo1
      JMOhkk(2,NHKk) = mo2
      JDAhkk(1,NHKk) = 0
      JDAhkk(2,NHKk) = 0
      IDRes(NHKk) = Idr
      IDXres(NHKk) = Idxr
      IDCh(NHKk) = Idc
C* here we need to do something..
      IF ( Id.EQ.88888 ) THEN
         idmo1 = ABS(IDHkk(mo1))
         idmo2 = ABS(IDHkk(mo2))
         IF ( (idmo1.LT.100) .AND. (idmo2.LT.100) ) NOBam(NHKk) = 3
         IF ( (idmo1.LT.100) .AND. (idmo2.GT.100) ) NOBam(NHKk) = 4
         IF ( (idmo1.GT.100) .AND. (idmo2.GT.100) ) NOBam(NHKk) = 5
         IF ( (idmo1.GT.100) .AND. (idmo2.LT.100) ) NOBam(NHKk) = 6
      ELSE
         NOBam(NHKk) = 0
      END IF
      IDBam(NHKk) = IDT_ICIHAD(Id)
 
      IF ( mo1.GT.0 ) THEN
         IF ( JDAhkk(1,mo1).NE.0 ) THEN
            JDAhkk(2,mo1) = NHKk
         ELSE
            JDAhkk(1,mo1) = NHKk
         END IF
      END IF
      IF ( mo2.GT.0 ) THEN
         IF ( JDAhkk(1,mo2).NE.0 ) THEN
            JDAhkk(2,mo2) = NHKk
         ELSE
            JDAhkk(1,mo2) = NHKk
         END IF
      END IF
C      IF ((IDBAM(NHKK).GT.0).AND.(IDBAM(NHKK).NE.7)) THEN
C         PTOT   = SQRT(PX**2+PY**2+PZ**2)
C         AM0    = SQRT(ABS( (E-PTOT)*(E+PTOT) ))
C         AMRQ   = AAM(IDBAM(NHKK))
C         AMDIF2 = (AM0-AMRQ)*(AM0+AMRQ)
C         IF ((ABS(AMDIF2).GT.TINY3).AND.(E.LT.SQTINF).AND.
C     &       (PTOT.GT.ZERO)) THEN
C            DELTA = -AMDIF2/(2.0D0*(E+PTOT))
CC           DELTA = (AMRQ2-AM2)/(2.0D0*(E+PTOT))
C            E     = E+DELTA
C            PTOT1 = PTOT-DELTA
C            PX    = PX*PTOT1/PTOT
C            PY    = PY*PTOT1/PTOT
C            PZ    = PZ*PTOT1/PTOT
C         ENDIF
C      ENDIF
      PHKk(1,NHKk) = Px
      PHKk(2,NHKk) = Py
      PHKk(3,NHKk) = Pz
      PHKk(4,NHKk) = E
      ptot = SQRT(Px**2+Py**2+Pz**2)
      IF ( (IDHkk(NHKk).GE.22) .AND. (IDHkk(NHKk).LE.24) ) THEN
         PHKk(5,NHKk) = PHKk(4,NHKk)**2 - ptot**2
         PHKk(5,NHKk) = SIGN(SQRT(ABS(PHKk(5,NHKk))),PHKk(5,NHKk))
      ELSE
         PHKk(5,NHKk) = (PHKk(4,NHKk)-ptot)*(PHKk(4,NHKk)+ptot)
C        IF ((PHKK(5,NHKK).LT.0.0D0).AND.(ABS(PHKK(5,NHKK)).GT.TINY4))
C    &      WRITE(LOUT,'(1X,A,G10.3)')
C    &        'EVTPUT: negative mass**2 ',PHKK(5,NHKK)
         PHKk(5,NHKk) = SQRT(ABS(PHKk(5,NHKk)))
      END IF
      idchk = Id/10000
      IF ( ((idchk.EQ.7) .OR. (idchk.EQ.8)) .AND. (Id.NE.80000) ) THEN
C special treatment for chains:
C    z coordinate of chain in Lab  = pos. of target nucleon
C    time of chain-creation in Lab = time of passage of projectile
C                                    nucleus at pos. of taget nucleus
C        VHKK(1,NHKK) = 0.5D0*(VHKK(1,MO1)+VHKK(1,MO2))
C        VHKK(2,NHKK) = 0.5D0*(VHKK(2,MO1)+VHKK(2,MO2))
         VHKk(1,NHKk) = VHKk(1,mo2)
         VHKk(2,NHKk) = VHKk(2,mo2)
         VHKk(3,NHKk) = VHKk(3,mo2)
         VHKk(4,NHKk) = VHKk(3,mo2)/BLAb - VHKk(3,mo1)/BGLab
C        WHKK(1,NHKK) = 0.5D0*(WHKK(1,MO1)+WHKK(1,MO2))
C        WHKK(2,NHKK) = 0.5D0*(WHKK(2,MO1)+WHKK(2,MO2))
         WHKk(1,NHKk) = WHKk(1,mo1)
         WHKk(2,NHKk) = WHKk(2,mo1)
         WHKk(3,NHKk) = WHKk(3,mo1)
         WHKk(4,NHKk) = -WHKk(3,mo1)/BLAb + WHKk(3,mo2)/BGLab
      ELSE IF ( mo1.GT.0 ) THEN
         DO i = 1 , 4
            VHKk(i,NHKk) = VHKk(i,mo1)
            WHKk(i,NHKk) = WHKk(i,mo1)
         END DO
      ELSE
         DO i = 1 , 4
            VHKk(i,NHKk) = ZERO
            WHKk(i,NHKk) = ZERO
         END DO
      END IF
 
      END SUBROUTINE
