
      SUBROUTINE DT_KKINC(Npmass,Npchar,Ntmass,Ntchar,Idp,Epn,Kkmat,
     &                    Irej)
 
C***********************************************************************
C Treatment of complete nucleus-nucleus or hadron-nucleus scattering   *
C This subroutine is an update of the previous version written         *
C by J. Ranft/ H.-J. Moehring.                                         *
C This version dated 19.11.95 is written by S. Roesler                 *
C***********************************************************************
 
      IMPLICIT NONE
      DOUBLE PRECISION cdum , dum , Epn , ONE , pdum , TINY2 , TINY3 , 
     &                 TINY5 , what , ZERO
      INTEGER Idp , iflag , iloop , Irej , irej1 , Kkmat , nloop , 
     &        Npchar , Npmass , Ntchar , Ntmass
      SAVE 
 
      INCLUDE 'inc/dtflka'
 
      PARAMETER (ZERO=0.0D0,ONE=1.0D0,TINY5=1.0D-5,TINY2=1.0D-2,
     &           TINY3=1.0D-3)
 
      LOGICAL lfzc
 
C event history
 
 
      INCLUDE 'inc/dtevt1'
C extended event history
      INCLUDE 'inc/dtevt2'
C particle properties (BAMJET index convention)
      INCLUDE 'inc/dtpart'
C properties of interacting particles
      INCLUDE 'inc/dtprta'
C Lorentz-parameters of the current interaction
      INCLUDE 'inc/dtltra'
C flags for input different options
      INCLUDE 'inc/dtflg1'
C flags for particle decays
      INCLUDE 'inc/dtfrpa'
C cuts for variable energy runs
      INCLUDE 'inc/dtvare'
C Glauber formalism: flags and parameters for statistics
      INCLUDE 'inc/dtglgp'
      INCLUDE 'inc/dtimpa'
      DIMENSION what(6)
#ifndef FOR_FLUKA
Cf2py intent(out) IREJ
#endif
      Irej = 0
      iloop = 0
 100  IF ( iloop.EQ.4 ) THEN
 
         IF ( LPRi.GT.4 ) WRITE (LOUt,99010) NEVhkk
99010    FORMAT (1X,'KKINC: event ',I8,' rejected!')
         Irej = 1
         GOTO 99999
      END IF
      iloop = iloop + 1
 
C variable energy-runs, recalculate parameters for LT's
      IF ( (ABS(VARehi).GT.ZERO) .OR. (IOGlb.EQ.100) ) THEN
         pdum = ZERO
         cdum = ZERO
         CALL DT_LTINI(Idp,1,Epn,pdum,cdum,1)
      END IF
      IF ( Epn.GT.EPRoj ) THEN
 
         IF ( LPRi.GT.4 ) WRITE (LOUt,'(A,E9.3,2A,E9.3,A)')
     &         ' Requested energy (' , Epn , 'GeV) exceeds' , 
     &        ' initialization energy (' , EPRoj , 'GeV) !'
         STOP
      END IF
 
C re-initialize /DTPRTA/
      IP = Npmass
      IPZ = Npchar
      IT = Ntmass
      ITZ = Ntchar
      IJProj = Idp
      IBProj = IIBar(IJProj)
 
C anfe 01.10.2015 patch for non-proton target
      IF ( (IT.EQ.1) .AND. (ITZ.EQ.1) ) THEN
         IJTarg = 1
      ELSE IF ( (IT.EQ.1) .AND. (ITZ.EQ.0) ) THEN
         IJTarg = 8
      ELSE IF ( (IT.EQ.1) .AND. (ITZ.EQ.-1) ) THEN
         IJTarg = -2
      END IF
      IBTarg = IIBar(IJTarg)
 
C calculate nuclear potentials (common /DTNPOT/)
      CALL DT_NCLPOT(IPZ,IP,ITZ,IT,ZERO,ZERO,0)
 
C initialize treatment for residual nuclei
      CALL DT_RESNCL(Epn,nloop,1)
 
C sample hadron/nucleus-nucleus interaction
      CALL DT_KKEVNT(Kkmat,irej1)
      IF ( irej1.GT.0 ) THEN
 
         IF ( LPRi.GT.4 .AND. IOUlev(1).GT.0 ) WRITE (LOUt,*)
     &         'rejected 1 in KKINC'
         Irej = 1
         GOTO 99999
      END IF
 
      IF ( (Npmass.GT.1) .OR. (Ntmass.GT.1) ) THEN
 
C intranuclear cascade of final state particles for KTAUGE generations
C of secondaries
         CALL DT_FOZOCA(lfzc,irej1)
         IF ( irej1.GT.0 ) THEN
 
            IF ( LPRi.GT.4 .AND. IOUlev(1).GT.0 ) WRITE (LOUt,*)
     &            'rejected 2 in KKINC'
            Irej = 1
            GOTO 99999
         END IF
 
C baryons unable to escape the nuclear potential are treated as
C excited nucleons (ISTHKK=15,16)
         CALL DT_SCN4BA
 
C decay of resonances produced in intranuclear cascade processes
C*sr 15-11-95 should be obsolete
C        IF (LFZC) CALL DT_DECAY1
 
C treatment of residual nuclei
C       added j.r. 28.5.06
 150     IF ( ICEntr.NE.-100 ) CALL DT_RESNCL(Epn,nloop,2)
 
C evaporation / fission / fragmentation
C (if intranuclear cascade was sampled only)
         IF ( lfzc .AND. ICEntr.NE.-100 ) THEN
            CALL DT_FICONF(IJProj,IP,IPZ,IT,ITZ,nloop,irej1)
            IF ( irej1.GT.1 ) GOTO 150
            IF ( irej1.EQ.1 ) GOTO 100
         END IF
 
      END IF
 
Cc ooooo Siegen June-01
Cc -----
Cc -----  temporary -til Stefan will fix the problem
Cc -----  june'01
Cc -----> ------- <---- ----> kludge <---- ----> kludge <-----
C                12.6.01 J.R. reject p,n with too large energy
C       MISTC=0
C *  Disabled by AF on 1-Aug-2015:
C *     DO 8936 IHKK=1,NHKK
C       DO 8936 IHKK=1,-NHKK
C C         IF((ISTHKK(IHKK).EQ.1.OR.ISTHKK(IHKK).EQ.-1).AND.
C C    *    (IDHKK(IHKK).EQ.2112.OR.IDHKK(IHKK).EQ.2212))THEN
C           IF((ISTHKK(IHKK).EQ.1.OR.ISTHKK(IHKK).EQ.-1).AND.
C      *    (IDHKK(IHKK).NE.80000)
C      *    )THEN
C           IF(ABS(PHKK(4,IHKK)).GT.(UMO/2.D0)*1.4D0)THEN
C             PMIST=PHKK(4,IHKK)
C             MISTX=1
C C        WRITE(Iae,*)' PHKK(4,IHKK).GT.(UMO/2.D0)*1.4D0',
C C    *   ' rejection ',
C C    *   'PHKK(4,IHKK)=',PMIST,ISTHKK(IHKK),IDHKK(IHKK),UMO/2.D0
C           ENDIF
C           IF(ABS(PHKK(4,IHKK)).GT.(UMO/2.D0)*1.2D0)THEN
C             PMIST=PHKK(4,IHKK)
C             XXXMIS=(PHKK(4,IHKK)-(UMO/2.D0)*1.2D0)/EPROJ
C             XXXXMI=XXXMIS*5.D0
C             IF(XXXXMI.GT.DT_RNDM(VV))THEN
C               PMIST=PHKK(4,IHKK)
C               MISTX=1
C C        WRITE(Iae,*)' PHKK(4,IHKK).GT.(UMO/2.D0)*1.2D0',
C C    *   ' rejection ',
C C    *   'PHKK(4,IHKK)=',PMIST,ISTHKK(IHKK),IDHKK(IHKK),UMO/2.D0
C             ENDIF
C           ENDIF
C           ENDIF
C  8936  CONTINUE
C        IF(MISTX.EQ.1)THEN
C           WRITE(Iae,*)' PHKK(4,IHKK).GT.EPROJ*1.1D0',
C      *   ' rejection ',
C      *   'PHKK(4,IHKK)=',PMIST,EPROJ
C C         GO TO 100
C        ENDIF
C                12.6.01 J.R. reject p,n with too large energy
Cc -----> ------- <---- ----> kludge <---- ----> kludge <-----
Cc -----
Cc ooooo Siegen June-01
 
 
C transform finale state into Lab.
      iflag = 2
      CALL DT_BEAMPR(what,dum,iflag)
 
      IF ( (IFRame.EQ.1) .AND. (iflag.EQ.-1) ) CALL DT_LT2LAB
 
C     IF (NEVHKK.EQ.5) CALL DT_EVTOUT(4)
C     WRITE(LOUT,*)'KKINC RETURN IREJ,NHKK=',IREJ,NHKK
 
      IF ( IPI0.EQ.1 ) CALL DT_DECPI0
C     WRITE(LOUT,*)'KKINC 9999 IREJ=',IREJ
99999 END SUBROUTINE
