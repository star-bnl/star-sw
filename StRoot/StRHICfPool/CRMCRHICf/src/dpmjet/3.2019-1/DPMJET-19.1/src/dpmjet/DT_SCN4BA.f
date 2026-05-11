
      SUBROUTINE DT_SCN4BA
 
C***********************************************************************
C SCan /DTEVT1/ 4 BAryons which are not able to escape nuclear pot.    *
C This version dated 12.12.95 is written by S. Roesler.                *
C***********************************************************************
 
      IMPLICIT NONE
      DOUBLE PRECISION aferp , afert , ekin , ONE , pcms , plab , 
     &                 plabt , TINY10 , TINY2 , TINY3 , ZERO
      INTEGER i , id , iprcl , ipzrcl , irej , iresp , irest , isglpr , 
     &        isglta , ist , itrcl , itzrcl , j , k , MAXINT , MAXNCL , 
     &        MAXSQU , MAXVQU , npotp , npott
      INTEGER npstck , ntstck
      SAVE 
 
      INCLUDE 'inc/dtflka'
 
      PARAMETER (ZERO=0.0D0,ONE=1.0D0,TINY3=1.0D-3,TINY2=1.0D-2,
     &           TINY10=1.0D-10)
 
      PARAMETER (MAXNCL=260,MAXVQU=MAXNCL,MAXSQU=20*MAXVQU,
     &           MAXINT=MAXVQU+MAXSQU)
 
C event history
 
 
      INCLUDE 'inc/dtevt1'
C extended event history
      INCLUDE 'inc/dtevt2'
C particle properties (BAMJET index convention)
      INCLUDE 'inc/dtpart'
C properties of interacting particles
      INCLUDE 'inc/dtprta'
C nuclear potential
      INCLUDE 'inc/dtnpot'
C treatment of residual nuclei: wounded nucleons
      INCLUDE 'inc/dtwoun'
C treatment of residual nuclei: 4-momenta
      INCLUDE 'inc/dtrnu1'
 
      DIMENSION plab(2,5) , pcms(4)
 
      irej = 0
 
C get number of wounded nucleons
      NPW = 0
      NPW0 = 0
      NPCw = 0
      npstck = 0
      NTW = 0
      NTW0 = 0
      NTCw = 0
      ntstck = 0
 
      isglpr = 0
      isglta = 0
      LRClpr = .FALSE.
      LRClta = .FALSE.
 
C     DO 2 I=1,NHKK
      DO i = 1 , NPOint(1)
C projectile nucleons wounded in primary interaction and in fzc
         IF ( (ISThkk(i).EQ.11) .OR. (ISThkk(i).EQ.17) ) THEN
            NPW = NPW + 1
            IPW(NPW) = i
            npstck = npstck + 1
            IF ( IDHkk(i).EQ.2212 ) NPCw = NPCw + 1
            IF ( ISThkk(i).EQ.11 ) NPW0 = NPW0 + 1
C           IF (IP.GT.1) THEN
            DO k = 1 , 4
               TRClpr(k) = TRClpr(k) - PHKk(k,i)
            END DO
C           ENDIF
C target nucleons wounded in primary interaction and in fzc
         ELSE IF ( (ISThkk(i).EQ.12) .OR. (ISThkk(i).EQ.18) ) THEN
            NTW = NTW + 1
            ITW(NTW) = i
            ntstck = ntstck + 1
            IF ( IDHkk(i).EQ.2212 ) NTCw = NTCw + 1
            IF ( ISThkk(i).EQ.12 ) NTW0 = NTW0 + 1
            IF ( IT.GT.1 ) THEN
               DO k = 1 , 4
                  TRClta(k) = TRClta(k) - PHKk(k,i)
               END DO
            END IF
         ELSE IF ( ISThkk(i).EQ.13 ) THEN
            isglpr = i
         ELSE IF ( ISThkk(i).EQ.14 ) THEN
            isglta = i
         END IF
      END DO
 
      DO i = NPOint(4) , NHKk
C baryons which are unable to escape the nuclear potential of proj.
         IF ( ISThkk(i).EQ.15 ) THEN
            isglpr = i
            npstck = npstck - 1
            IF ( IIBar(IDBam(i)).NE.0 ) THEN
               NPW = NPW - 1
               IF ( IICh(IDBam(i)).GT.0 ) NPCw = NPCw - 1
            END IF
            DO k = 1 , 4
               TRClpr(k) = TRClpr(k) + PHKk(k,i)
            END DO
C baryons which are unable to escape the nuclear potential of targ.
         ELSE IF ( ISThkk(i).EQ.16 ) THEN
            isglta = i
            ntstck = ntstck - 1
            IF ( IIBar(IDBam(i)).NE.0 ) THEN
               NTW = NTW - 1
               IF ( IICh(IDBam(i)).GT.0 ) NTCw = NTCw - 1
            END IF
            DO k = 1 , 4
               TRClta(k) = TRClta(k) + PHKk(k,i)
            END DO
         END IF
      END DO
 
C residual nuclei so far
      iresp = IP - npstck
      irest = IT - ntstck
 
C ckeck for "residual nuclei" consisting of one nucleon only
C treat it as final state particle
      IF ( iresp.EQ.1 ) THEN
         id = IDBam(isglpr)
         ist = ISThkk(isglpr)
         CALL DT_LTRANS(PHKk(1,isglpr),PHKk(2,isglpr),PHKk(3,isglpr),
     &                  PHKk(4,isglpr),pcms(1),pcms(2),pcms(3),pcms(4),
     &                  id,2)
         IF ( ist.EQ.13 ) THEN
            ISThkk(isglpr) = 11
         ELSE
            ISThkk(isglpr) = 2
         END IF
         CALL DT_EVTPUT(1,IDHkk(isglpr),isglpr,0,pcms(1),pcms(2),pcms(3)
     &                  ,pcms(4),IDRes(isglpr),IDXres(isglpr),
     &                  IDCh(isglpr))
         NOBam(NHKk) = NOBam(isglpr)
         JDAhkk(1,isglpr) = NHKk
         DO k = 1 , 4
            TRClpr(k) = TRClpr(k) - PHKk(k,isglpr)
         END DO
      END IF
      IF ( irest.EQ.1 ) THEN
         id = IDBam(isglta)
         ist = ISThkk(isglta)
         CALL DT_LTRANS(PHKk(1,isglta),PHKk(2,isglta),PHKk(3,isglta),
     &                  PHKk(4,isglta),pcms(1),pcms(2),pcms(3),pcms(4),
     &                  id,3)
         IF ( ist.EQ.14 ) THEN
            ISThkk(isglta) = 12
         ELSE
            ISThkk(isglta) = 2
         END IF
         CALL DT_EVTPUT(1,IDHkk(isglta),isglta,0,pcms(1),pcms(2),pcms(3)
     &                  ,pcms(4),IDRes(isglta),IDXres(isglta),
     &                  IDCh(isglta))
         NOBam(NHKk) = NOBam(isglta)
         JDAhkk(1,isglta) = NHKk
         DO k = 1 , 4
            TRClta(k) = TRClta(k) - PHKk(k,isglta)
         END DO
      END IF
 
C get nuclear potential corresp. to the residual nucleus
      iprcl = IP - NPW
      ipzrcl = IPZ - NPCw
      itrcl = IT - NTW
      itzrcl = ITZ - NTCw
      CALL DT_NCLPOT(ipzrcl,iprcl,itzrcl,itrcl,ZERO,ZERO,1)
 
C baryons unable to escape the nuclear potential are treated as
C excited nucleons (ISTHKK=15,16)
      DO i = NPOint(4) , NHKk
         IF ( ISThkk(i).EQ.1 ) THEN
            id = IDBam(i)
            IF ( ((id.EQ.1) .OR. (id.EQ.8)) .AND. (NOBam(i).NE.3) ) THEN
C   final state n and p not being outside of both nuclei are considered
               npotp = 1
               npott = 1
               IF ( (IP.GT.1) .AND. (iresp.GT.1) .AND. (NOBam(i).NE.1)
     &              .AND. (NPW.GT.0) ) THEN
C     Lorentz-trsf. into proj. rest sys. for those being inside proj.
                  CALL DT_LTRANS(PHKk(1,i),PHKk(2,i),PHKk(3,i),PHKk(4,i)
     &               ,plab(1,1),plab(1,2),plab(1,3),plab(1,4),id,-2)
                  plabt = SQRT(plab(1,1)**2+plab(1,2)**2+plab(1,3)**2)
                  plab(1,5) = SQRT(ABS((plab(1,4)-plabt)*(plab(1,4)+
     &                        plabt)))
                  ekin = plab(1,4) - plab(1,5)
                  IF ( ekin.LE.EPOt(1,id) ) npotp = 15
                  IF ( (id.EQ.1) .AND. (NPCw.LE.0) ) npotp = 1
               END IF
               IF ( (IT.GT.1) .AND. (irest.GT.1) .AND. (NOBam(i).NE.2)
     &              .AND. (NTW.GT.0) ) THEN
C     Lorentz-trsf. into targ. rest sys. for those being inside targ.
                  CALL DT_LTRANS(PHKk(1,i),PHKk(2,i),PHKk(3,i),PHKk(4,i)
     &               ,plab(2,1),plab(2,2),plab(2,3),plab(2,4),id,-3)
                  plabt = SQRT(plab(2,1)**2+plab(2,2)**2+plab(2,3)**2)
                  plab(2,5) = SQRT(ABS((plab(2,4)-plabt)*(plab(2,4)+
     &                        plabt)))
                  ekin = plab(2,4) - plab(2,5)
                  IF ( ekin.LE.EPOt(2,id) ) npott = 16
                  IF ( (id.EQ.1) .AND. (NTCw.LE.0) ) npott = 1
               END IF
               IF ( PHKk(3,i).GE.ZERO ) THEN
                  ISThkk(i) = npott
                  IF ( npotp.NE.1 ) ISThkk(i) = npotp
               ELSE
                  ISThkk(i) = npotp
                  IF ( npott.NE.1 ) ISThkk(i) = npott
               END IF
               IF ( ISThkk(i).NE.1 ) THEN
                  j = ISThkk(i) - 14
                  DO k = 1 , 5
                     PHKk(k,i) = plab(j,k)
                  END DO
                  IF ( ISThkk(i).EQ.15 ) THEN
                     NPW = NPW - 1
                     IF ( id.EQ.1 ) NPCw = NPCw - 1
                     DO k = 1 , 4
                        TRClpr(k) = TRClpr(k) + PHKk(k,i)
                     END DO
                  ELSE IF ( ISThkk(i).EQ.16 ) THEN
                     NTW = NTW - 1
                     IF ( id.EQ.1 ) NTCw = NTCw - 1
                     DO k = 1 , 4
                        TRClta(k) = TRClta(k) + PHKk(k,i)
                     END DO
                  END IF
               END IF
            END IF
         END IF
      END DO
 
C again: get nuclear potential corresp. to the residual nucleus
      iprcl = IP - NPW
      ipzrcl = IPZ - NPCw
      itrcl = IT - NTW
      itzrcl = ITZ - NTCw
C      AFERP = 1.2D0*FERMOD*(ONE+(DBLE(IP+10-NPW0)/DBLE(IP+10))**1.1D0)
CC     AFERP = 1.21D0*FERMOD*(ONE+(DBLE(IP+40-NPW0)/DBLE(IP+40))**1.1D0)
C     &             *(0.94D0+0.3D0*EXP(-DBLE(NPW0)/5.0D0)) /2.0D0
C     AFERP = 0.0D0
C      AFERT = 1.2D0*FERMOD*(ONE+(DBLE(IT+10-NTW0)/DBLE(IT+10))**1.1D0)
CC     AFERT = 1.21D0*FERMOD*(ONE+(DBLE(IT+40-NTW0)/DBLE(IT+40))**1.1D0)
C     &             *(0.94D0+0.3D0*EXP(-DBLE(NTW0)/5.0D0)) /2.0D0
C     AFERT = 0.0D0
C     IF (AFERP.LT.FERMOD) AFERP = FERMOD+0.1
C     IF (AFERT.LT.FERMOD) AFERT = FERMOD+0.1
C     IF (AFERP.GT.0.85D0) AFERP = 0.85D0
C     IF (AFERT.GT.0.85D0) AFERT = 0.85D0
      aferp = FERmod + 0.1D0
      afert = FERmod + 0.1D0
 
      CALL DT_NCLPOT(ipzrcl,iprcl,itzrcl,itrcl,aferp,afert,1)
 
      END SUBROUTINE
