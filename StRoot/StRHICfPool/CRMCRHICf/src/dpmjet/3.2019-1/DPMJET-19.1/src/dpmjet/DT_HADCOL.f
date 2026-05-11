
      SUBROUTINE DT_HADCOL(Idproj,Ppn,Idxtar,Irej)
 
C***********************************************************************
C Interface to the HADRIN-routines for inelastic and elastic           *
C scattering. This subroutine samples hadron-nucleus interactions      *
C below DPM-threshold.                                                 *
C      IDPROJ        BAMJET-index of projectile hadron                 *
C      PPN           projectile momentum in target rest frame          *
C      IDXTAR        DTEVT1-index of target nucleon undergoing         *
C                    interaction with projectile hadron                *
C This subroutine replaces HADHAD.                                     *
C This version dated 5.5.95 is written by S. Roesler                   *
C***********************************************************************
 
      IMPLICIT NONE
      DOUBLE PRECISION DT_RNDM , dumzer , ecms , ONE , pcms , pnuc , 
     &                 Ppn , pproj , sigel , sigin , sigtot , tausav , 
     &                 TINY10 , TINY3 , ZERO
      INTEGER i , idhad , idnuc , idnuc1 , idpro1 , Idproj , 
     &        IDT_IPDGHA , IDT_MCHAD , Idxtar , iloop , iproc , Irej , 
     &        irej1 , ist , k
      SAVE 
 
      INCLUDE 'inc/dtflka'
 
      PARAMETER (ZERO=0.0D0,TINY10=1.0D-10,TINY3=1.0D-3,ONE=1.0D0)
 
      LOGICAL lstart
 
C event history
 
 
      INCLUDE 'inc/dtevt1'
C extended event history
      INCLUDE 'inc/dtevt2'
C nuclear potential
      INCLUDE 'inc/dtnpot'
C interface HADRIN-DPM
      INCLUDE 'inc/hnthre'
C parameter for intranuclear cascade
      INCLUDE 'inc/dtfoti'
C final state after inc step
      INCLUDE 'inc/dtcapa'
C particle properties (BAMJET index convention)
      INCLUDE 'inc/dtpart'
 
      DIMENSION pproj(5) , pnuc(5)
 
      DATA lstart/.TRUE./
 
      Irej = 0
 
      NPOint(1) = NHKk + 1
 
      tausav = TAUfor
C*sr 6/9/01 commented
C     TAUFOR = TAUFOR/2.0D0
C*
      IF ( lstart ) THEN
 
         IF ( LPRi.GT.4 ) WRITE (LOUt,99010)
99010    FORMAT (/,1X,'HADCOL:  Scattering handled by HADRIN')
 
         IF ( LPRi.GT.4 ) WRITE (LOUt,99020) TAUfor
99020    FORMAT (/,1X,'HADCOL:  Formation zone parameter set to ',F5.1,
     &           ' fm/c')
         lstart = .FALSE.
      END IF
 
      idnuc = IDBam(Idxtar)
      idnuc1 = IDT_MCHAD(idnuc)
      idpro1 = IDT_MCHAD(Idproj)
 
      IF ( (INThad.EQ.1) .OR. (INThad.EQ.2) ) THEN
         iproc = INThad
      ELSE
C*
C        CALL DT_SIHNIN(IDPRO1,IDNUC1,PPN,SIGIN)
C        CALL DT_SIHNEL(IDPRO1,IDNUC1,PPN,SIGEL)
         dumzer = ZERO
         CALL DT_XSHN(idpro1,idnuc1,Ppn,dumzer,sigtot,sigel)
         sigin = sigtot - sigel
C        SIGTOT = SIGIN+SIGEL
C*
         iproc = 1
         IF ( DT_RNDM(sigin).LT.sigel/sigtot ) iproc = 2
      END IF
 
      pproj(1) = ZERO
      pproj(2) = ZERO
      pproj(3) = Ppn
      pproj(5) = AAM(Idproj)
      pproj(4) = SQRT(pproj(5)**2+pproj(3)**2)
      DO k = 1 , 5
         pnuc(k) = PHKk(k,Idxtar)
      END DO
 
      iloop = 0
 100  iloop = iloop + 1
      IF ( iloop.LE.100 ) THEN
 
         CALL DT_HADRIN(Idproj,pproj,idnuc,pnuc,iproc,irej1)
         IF ( irej1.NE.1 ) THEN
 
            IF ( irej1.GT.1 ) THEN
C no interaction possible
C   require Pauli blocking
               IF ( (Idproj.EQ.1) .AND. (pproj(4).LE.PFErmp(2)+pproj(5))
     &              ) GOTO 100
               IF ( (Idproj.EQ.8) .AND. (pproj(4).LE.PFErmn(2)+pproj(5))
     &              ) GOTO 100
               IF ( (IIBar(Idproj).NE.1) .AND. 
     &              (pproj(4).LE.EPOt(2,Idproj)+pproj(5)) ) GOTO 100
C   store incoming particle as final state particle
               CALL DT_LTNUC(pproj(3),pproj(4),pcms,ecms,3)
               CALL DT_EVTPUT(1,Idproj,1,0,pproj(1),pproj(2),pcms,ecms,
     &                        0,0,0)
               NPOint(4) = NHKk
            ELSE
C require Pauli blocking for final state nucleons
               DO i = 1 , NFSp
                  IF ( (IDFsp(i).EQ.1) .AND. 
     &                 (PFSp(4,i).LE.PFErmp(2)+AAM(IDFsp(i))) ) GOTO 100
                  IF ( (IDFsp(i).EQ.8) .AND. 
     &                 (PFSp(4,i).LE.PFErmn(2)+AAM(IDFsp(i))) ) GOTO 100
                  IF ( (IIBar(IDFsp(i)).NE.1) .AND. 
     &                 (PFSp(4,i).LE.EPOt(2,IDFsp(i))+AAM(IDFsp(i))) )
     &                 GOTO 100
               END DO
C store final state particles
               DO i = 1 , NFSp
                  ist = 1
                  IF ( (IIBar(IDFsp(i)).EQ.1) .AND. 
     &                 (PFSp(4,i).LE.EPOt(2,IDFsp(i))+AAM(IDFsp(i))) )
     &                 ist = 16
                  idhad = IDT_IPDGHA(IDFsp(i))
                  CALL DT_LTNUC(PFSp(3,i),PFSp(4,i),pcms,ecms,3)
                  CALL DT_EVTPUT(ist,idhad,1,Idxtar,PFSp(1,i),PFSp(2,i),
     &               pcms,ecms,0,0,0)
                  IF ( i.EQ.1 ) NPOint(4) = NHKk
                  VHKk(1,NHKk) = 0.5D0*(VHKk(1,1)+VHKk(1,Idxtar))
                  VHKk(2,NHKk) = 0.5D0*(VHKk(2,1)+VHKk(2,Idxtar))
                  VHKk(3,NHKk) = VHKk(3,Idxtar)
                  VHKk(4,NHKk) = VHKk(4,Idxtar)
                  WHKk(1,NHKk) = 0.5D0*(WHKk(1,1)+WHKk(1,Idxtar))
                  WHKk(2,NHKk) = 0.5D0*(WHKk(2,1)+WHKk(2,Idxtar))
                  WHKk(3,NHKk) = WHKk(3,1)
                  WHKk(4,NHKk) = WHKk(4,1)
               END DO
            END IF
            TAUfor = tausav
            RETURN
         END IF
      END IF
 
      Irej = 1
      TAUfor = tausav
      END SUBROUTINE
