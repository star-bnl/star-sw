
      SUBROUTINE DT_DENSITY
C***********************************************************************
Cc additions to DPMJET3 : chain fusion, H. Ranft may 2006
C***********************************************************************
      IMPLICIT NONE
      DOUBLE PRECISION aabbfus , daaa , rkl , rkm , rlm , rrkein , 
     &                 ZAKein
      INTEGER i , ichaim , idh , idk , idk1 , idk2 , idl , idl1 , idl2 , 
     &        idm , idm1 , idm2 , idmm1 , idmm2 , iikein , iix , iiy , 
     &        iqq1 , iqq2 , irej
      INTEGER jjkein , kchaim , kk , kki , kkk , ll , lll , mm , mmm , 
     &        n22222 , n66666 , nalll , nchain , nchain21 , nchain22 , 
     &        nchain31 , nchain32 , nchain33 , nchain34 , nchainn
      INTEGER ncount
      SAVE 
C event history
 
 
      INCLUDE 'inc/dtevt1'
C extended event history
      INCLUDE 'inc/dtevt2'
      INCLUDE 'inc/dtflka'
C Lorentz-parameters of the current interaction
      INCLUDE 'inc/dtltra'
C flags
      INCLUDE 'inc/dtflg1'
C
      DIMENSION ichaim(30,30) , kchaim(30,30,600)
      COMMON /ZAZAKE/ ZAKein(100)
C---------------------
C
      DATA ncount/0/
C.     AABBFUS=0.81D0*(1.D0-5.D0/SQRT(UMO))
      aabbfus = 0.45D0
C       parameters im dpm3304
      IF ( UMO.GT.100.D0 ) aabbfus = 0.45D0
      IF ( UMO.LT.20.D0 ) aabbfus = 0.0D0
      IF ( UMO.GT.20.D0 .AND. UMO.LT.100.D0 )
     &     aabbfus = 0.45D0*(UMO-20.D0)/80.D0
      IF ( LPRi.GT.4 ) THEN
         IF ( ncount.LE.5 ) WRITE (6,*) " UMO,AABBFUS =" , UMO , aabbfus
      END IF
C        paarameters in dpm3influka
      IF ( UMO.GT.100.D0 ) aabbfus = 0.53D0
      IF ( UMO.LT.20.D0 ) aabbfus = 0.0D0
      IF ( UMO.GT.20.D0 .AND. UMO.LT.100.D0 )
     &     aabbfus = 0.53D0*(UMO-20.D0)/80.D0
      IF ( LPRi.GT.4 ) THEN
         IF ( ncount.LE.5 ) WRITE (6,*) " UMO,AABBFUS =" , UMO , aabbfus
      END IF
      nchain = 0
      nchainn = 0
      nchain31 = 0
      nchain32 = 0
      nchain33 = 0
      nchain34 = 0
      nchain21 = 0
      nchain22 = 0
      ncount = ncount + 1
      daaa = 0.9D0
      daaa = 1.D0
      daaa = 1.2D0
      daaa = 1.5D0
      daaa = 1.8D0
 
C     First round of fusions
 
C     WRITE(6,*)' entry density'
      DO iix = 1 , 30
         DO iiy = 1 , 30
            ichaim(iix,iiy) = 0
            DO kki = 1 , 600
               kchaim(iix,iiy,kki) = 0
            END DO
         END DO
      END DO
      DO jjkein = 1 , 100
         ZAKein(jjkein) = 0.D0
      END DO
C
      DO kk = 1 , NHKk
         idh = IDHkk(kk)/10000
         IF ( idh.GE.7 ) THEN
            IF ( idh.LE.8 ) THEN
C           WRITE(6,'(2I4,I6,4I4,5F10.2,2I3,I2,I4,4E12.3)')
C    *         KK,ISTHKK(KK),IDHKK(KK),
C    *         JMOHKK(1,KK),JMOHKK(2,KK),JDAHKK(1,KK),JDAHKK(2,KK),
C    *    (PHKK(LL,KK),LL=1,5),IDRES(KK),IDXRES(KK),NOBAM(KK),IDBAM(KK),
C    *    (VHKK(JJ,KK),JJ=1,4)
               rrkein = 1.D12*SQRT(VHKk(1,kk)**2+VHKk(2,kk)**2)
               iikein = rrkein/0.2D0 + 2.D0
               DO jjkein = iikein , 100
                  ZAKein(jjkein) = ZAKein(jjkein) + 1
               END DO
            END IF
         END IF
      END DO
C
C
      DO kk = 1 , NHKk
         idh = IDHkk(kk)/10000
         IF ( idh.GE.7 ) THEN
            IF ( idh.LE.8 ) THEN
C          WRITE(6,'(2I4,I6,4I4,5F10.2,2I3,I2,I4,4E12.3)')
C    *     KK,ISTHKK(KK),IDHKK(KK),
C    *     JMOHKK(1,KK),JMOHKK(2,KK),JDAHKK(1,KK),JDAHKK(2,KK),
C    *    (PHKK(LL,KK),LL=1,5),IDRES(KK),IDXRES(KK),NOBAM(KK),IDBAM(KK),
C    *    (VHKK(JJ,KK),JJ=1,4)
               iix = 15.D0 + 1.D12*VHKk(1,kk)/daaa
               iiy = 15.D0 + 1.D12*VHKk(2,kk)/daaa
               IF ( iix.LT.2 ) iix = 2
               IF ( iiy.LT.2 ) iiy = 2
               IF ( iix.GT.29 ) iix = 29
               IF ( iiy.GT.29 ) iiy = 29
               ichaim(iix,iiy) = ichaim(iix,iiy) + 1
               nchain = nchain + 1
               kkk = ichaim(iix,iiy)
               kchaim(iix,iiy,kkk) = kk
            END IF
         END IF
      END DO
C
      IF ( ncount.LE.3 ) THEN
         IF ( LPRi.GT.4 ) THEN
            WRITE (6,*) ' JJJ '
            DO iiy = 1 , 30
               WRITE (6,'(30I3)') (ichaim(iix,iiy),iix=1,30)
            END DO
            WRITE (6,*) ' ZAKEIN '
            DO jjkein = 1 , 100
               WRITE (6,*) jjkein , ZAKein(jjkein)
            END DO
         END IF
      END IF
 
      IF ( IFLow.EQ.1 ) THEN
C        WRITE(6,*)' and now KETWW '
C        CALL KETWW(ZAKEIN)
      END IF
 
C     First round of fusions
 
      DO iix = 8 , 22
         DO iiy = 8 , 22
            IF ( ichaim(iix,iiy).GT.1 ) THEN
               DO kkk = 1 , ichaim(iix,iiy) - 1
                  DO lll = kkk + 1 , ichaim(iix,iiy)
                     kk = IDHkk(kchaim(iix,iiy,kkk))/10000
                     IF ( kk.LT.7 ) GOTO 10
                     IF ( kk.GT.8 ) GOTO 10
                     ll = IDHkk(kchaim(iix,iiy,lll))/10000
                     IF ( ll.GE.7 ) THEN
                        IF ( ll.LE.8 ) THEN
                           rkl = 1.D12*SQRT
     &                        ((VHKk(1,kchaim(iix,iiy,kkk))-
     &                        VHKk(1,kchaim(iix,iiy,lll)))
     &                        **2+(VHKk(2,kchaim(iix,iiy,kkk))
     &                        -VHKk(2,kchaim(iix,iiy,lll)))**2)
                           IF ( rkl.LE.aabbfus ) THEN
                              idk = IDHkk(kchaim(iix,iiy,kkk))
                              idk1 = IDHkk(kchaim(iix,iiy,kkk)-2)
                              idk2 = IDHkk(kchaim(iix,iiy,kkk)-1)
                              idl = IDHkk(kchaim(iix,iiy,lll))
                              idl1 = IDHkk(kchaim(iix,iiy,lll)-2)
                              idl2 = IDHkk(kchaim(iix,iiy,lll)-1)
                              DO mmm = 1 , ichaim(iix,iiy)
                               mm = IDHkk(kchaim(iix,iiy,mmm))/10000
                               IF ( mm.GE.7 ) THEN
                               IF ( mm.LE.8 ) THEN
                               ll = IDHkk(kchaim(iix,iiy,lll))/10000
                               IF ( ll.LT.7 ) GOTO 5
                               IF ( ll.GT.8 ) GOTO 5
                               kk = IDHkk(kchaim(iix,iiy,kkk))/10000
                               IF ( kk.LT.7 ) GOTO 10
                               IF ( kk.GT.8 ) GOTO 10
                               IF ( mmm.NE.kkk .AND. mmm.NE.lll ) THEN
                               rkm = 1.D12*SQRT
     &                            ((VHKk(1,kchaim(iix,iiy,kkk))
     &                            -VHKk(1,kchaim(iix,iiy,mmm)))
     &                            **2+(VHKk(2,kchaim(iix,iiy,kkk))
     &                            -VHKk(2,kchaim(iix,iiy,mmm)))**2)
                               rlm = 1.D12*SQRT
     &                            ((VHKk(1,kchaim(iix,iiy,lll))
     &                            -VHKk(1,kchaim(iix,iiy,mmm)))
     &                            **2+(VHKk(2,kchaim(iix,iiy,lll))
     &                            -VHKk(2,kchaim(iix,iiy,mmm)))**2)
                               IF ( rkm.LE.aabbfus .OR. rlm.LE.aabbfus )
     &                            THEN
                               idm = IDHkk(kchaim(iix,iiy,mmm))
                               idm1 = IDHkk(kchaim(iix,iiy,mmm)-2)
                               idm2 = IDHkk(kchaim(iix,iiy,mmm)-1)
                               IF ( ((idk1.LE.4 .AND. idk1.GE.1) .AND. (
     &                            idk2.GE.-4 .AND. idk2.LE.-1)) .AND. 
     &                            ((idl1.LE.-1 .AND. idl1.GE.-4) .AND. 
     &                            (idl2.GE.1 .AND. idl2.LE.4)) ) THEN
                               IF ( (idk1.EQ.-idl1) .AND. 
     &                            (idk2.EQ.-idl2) ) THEN
C                     q-aq and aq-q chains
C                          WRITE(6,*)IIX,IIY,KKK,LLL,MMM,
C    *            ' RKL,RKM ',RKL,RKM,IDK1,IDK2,IDL1,IDL2,IDM1,IDM2,
C    *              KCHAIM(IIX,IIY,KKK),KCHAIM(IIX,IIY,LLL),
C    *              KCHAIM(IIX,IIY,MMM),IDK,IDL,IDM
                               CALL DT_JOIN3(kchaim(iix,iiy,kkk),
     &                            kchaim(iix,iiy,lll),
     &                            kchaim(iix,iiy,mmm),irej)
                               IF ( irej.EQ.0 ) nchain31 = nchain31 + 1
C                          IF(IREJ.NE.0)
C    *                       WRITE(6,*)'Rejection in JOIN3'
                               END IF
                               END IF
                               kk = IDHkk(kchaim(iix,iiy,kkk))/10000
                               IF ( kk.LT.7 ) GOTO 10
                               IF ( kk.GT.8 ) GOTO 10
                               ll = IDHkk(kchaim(iix,iiy,lll))/10000
                               IF ( ll.LT.7 ) GOTO 5
                               IF ( ll.GT.8 ) GOTO 5
                               kk = IDHkk(kchaim(iix,iiy,kkk))/10000
                               IF ( kk.LT.7 ) GOTO 10
                               IF ( kk.GT.8 ) GOTO 10
                               IF ( ((idk2.LE.4 .AND. idk2.GE.1) .AND. (
     &                            idk1.GE.-4 .AND. idk1.LE.-1)) .AND. 
     &                            ((idl2.LE.-1 .AND. idl2.GE.-4) .AND. 
     &                            (idl1.GE.1 .AND. idl1.LE.4)) ) THEN
                               IF ( (idk1.EQ.-idl1) .AND. 
     &                            (idk2.EQ.-idl2) ) THEN
C                     aq-q and q-aq chains
C                          WRITE(6,*)IIX,IIY,KKK,LLL,MMM,
C    *               ' RKL,RKM ',RKL,RKM,IDK1,IDK2,IDL1,IDL2,IDM1,IDM2,
C    *     KCHAIM(IIX,IIY,KKK),KCHAIM(IIX,IIY,LLL),KCHAIM(IIX,IIY,MMM),
C    *     IDK,IDL,IDM
                               CALL DT_JOIN3(kchaim(iix,iiy,kkk),
     &                            kchaim(iix,iiy,lll),
     &                            kchaim(iix,iiy,mmm),irej)
                               IF ( irej.EQ.0 ) nchain31 = nchain31 + 1
C                          IF(IREJ.NE.0)
C    *                        WRITE(6,*)'Rejection in JOIN3'
                               END IF
                               END IF
                               END IF
                               END IF
                               END IF
                               END IF
                              END DO
                           END IF
                        END IF
                     END IF
 5                END DO
 10            END DO
            END IF
         END DO
      END DO
 
C     First' round of fusions
 
C     WRITE(6,*)' entry density'
      DO iix = 1 , 30
         DO iiy = 1 , 30
            ichaim(iix,iiy) = 0
            DO kki = 1 , 600
               kchaim(iix,iiy,kki) = 0
            END DO
         END DO
      END DO
C
      DO kk = 1 , NHKk
         idh = IDHkk(kk)/10000
         IF ( idh.GE.7 ) THEN
            IF ( idh.LE.8 ) THEN
C        WRITE(6,'(2I4,I6,4I4,5F10.2,2I3,I2,I4,4E12.3)')
C    *               KK,ISTHKK(KK),IDHKK(KK),
C    *          JMOHKK(1,KK),JMOHKK(2,KK),JDAHKK(1,KK),JDAHKK(2,KK),
C    *    (PHKK(LL,KK),LL=1,5),IDRES(KK),IDXRES(KK),NOBAM(KK),IDBAM(KK),
C    *    (VHKK(JJ,KK),JJ=1,4)
               iix = 15.D0 + 1.D12*VHKk(1,kk)/daaa
               iiy = 15.D0 + 1.D12*VHKk(2,kk)/daaa
               IF ( iix.LT.2 ) iix = 2
               IF ( iiy.LT.2 ) iiy = 2
               IF ( iix.GT.29 ) iix = 29
               IF ( iiy.GT.29 ) iiy = 29
               ichaim(iix,iiy) = ichaim(iix,iiy) + 1
C        NCHAIN=NCHAIN+1
               kkk = ichaim(iix,iiy)
               kchaim(iix,iiy,kkk) = kk
            END IF
         END IF
      END DO
C
      IF ( ncount.LE.20 ) THEN
         IF ( LPRi.GT.4 ) THEN
            WRITE (6,*) ' JJJ '
            DO iiy = 1 , 30
               WRITE (6,'(30I3)') (ichaim(iix,iiy),iix=1,30)
            END DO
         END IF
      END IF
 
C     First' round of fusions include 66666 chains
 
      DO iix = 8 , 22
         DO iiy = 8 , 22
            IF ( ichaim(iix,iiy).GT.1 ) THEN
               DO kkk = 1 , ichaim(iix,iiy) - 1
                  DO lll = kkk + 1 , ichaim(iix,iiy)
                     kk = IDHkk(kchaim(iix,iiy,kkk))/10000
                     IF ( kk.LT.6 ) GOTO 20
                     IF ( kk.GT.8 ) GOTO 20
                     ll = IDHkk(kchaim(iix,iiy,lll))/10000
                     IF ( ll.GE.7 ) THEN
                        IF ( ll.LE.8 ) THEN
                           rkl = 1.D12*SQRT
     &                        ((VHKk(1,kchaim(iix,iiy,kkk))-
     &                        VHKk(1,kchaim(iix,iiy,lll)))
     &                        **2+(VHKk(2,kchaim(iix,iiy,kkk))
     &                        -VHKk(2,kchaim(iix,iiy,lll)))**2)
                           IF ( rkl.LE.aabbfus ) THEN
                              idk = IDHkk(kchaim(iix,iiy,kkk))
                              idk1 = IDHkk(kchaim(iix,iiy,kkk)-2)
                              idk2 = IDHkk(kchaim(iix,iiy,kkk)-1)
                              idl = IDHkk(kchaim(iix,iiy,lll))
                              idl1 = IDHkk(kchaim(iix,iiy,lll)-2)
                              idl2 = IDHkk(kchaim(iix,iiy,lll)-1)
                              DO mmm = 1 , ichaim(iix,iiy)
                               mm = IDHkk(kchaim(iix,iiy,mmm))/10000
                               IF ( mm.GE.6 ) THEN
                               IF ( mm.LE.8 ) THEN
                               ll = IDHkk(kchaim(iix,iiy,lll))/10000
                               IF ( ll.LT.6 ) GOTO 15
                               IF ( ll.GT.8 ) GOTO 15
                               kk = IDHkk(kchaim(iix,iiy,kkk))/10000
                               IF ( kk.LT.6 ) GOTO 20
                               IF ( kk.GT.8 ) GOTO 20
                               IF ( mmm.NE.kkk .AND. mmm.NE.lll ) THEN
                               rkm = 1.D12*SQRT
     &                            ((VHKk(1,kchaim(iix,iiy,kkk))
     &                            -VHKk(1,kchaim(iix,iiy,mmm)))
     &                            **2+(VHKk(2,kchaim(iix,iiy,kkk))
     &                            -VHKk(2,kchaim(iix,iiy,mmm)))**2)
                               rlm = 1.D12*SQRT
     &                            ((VHKk(1,kchaim(iix,iiy,lll))
     &                            -VHKk(1,kchaim(iix,iiy,mmm)))
     &                            **2+(VHKk(2,kchaim(iix,iiy,lll))
     &                            -VHKk(2,kchaim(iix,iiy,mmm)))**2)
                               IF ( rkm.LE.aabbfus .OR. rlm.LE.aabbfus )
     &                            THEN
                               idm = IDHkk(kchaim(iix,iiy,mmm))
                               idm1 = IDHkk(kchaim(iix,iiy,mmm)-2)
                               idm2 = IDHkk(kchaim(iix,iiy,mmm)-1)
                               IF ( ((idk1.LE.4 .AND. idk1.GE.1) .AND. (
     &                            idk2.GE.-4 .AND. idk2.LE.-1)) .AND. 
     &                            ((idl1.LE.-1 .AND. idl1.GE.-4) .AND. 
     &                            (idl2.GE.1 .AND. idl2.LE.4)) ) THEN
                               idmm1 = idm2/1000
                               idmm2 = (idm2-idmm1*1000)/100
                               IF ( ((idk2.EQ.-idmm1) .OR. (idk2.EQ.-
     &                            idmm2)) .AND. (idl1.EQ.-idm1) ) THEN
C                     q-aq and aq-q chains  plus q-qq chain
C                          WRITE(6,*)IIX,IIY,KKK,LLL,MMM,
C    *                ' RKL,RKM ',RKL,RKM,IDK1,IDK2,IDL1,IDL2,IDM1,IDM2,
C    *      KCHAIM(IIX,IIY,KKK),KCHAIM(IIX,IIY,LLL),KCHAIM(IIX,IIY,MMM),
C    *      IDK,IDL,IDM
                               CALL DT_JOIN33(kchaim(iix,iiy,kkk),
     &                            kchaim(iix,iiy,lll),
     &                            kchaim(iix,iiy,mmm),irej)
                               IF ( irej.EQ.0 ) nchain33 = nchain33 + 1
C                          IF(IREJ.NE.0)WRITE(6,*)'Rejection in JOIN33'
                               END IF
                               END IF
                               kk = IDHkk(kchaim(iix,iiy,kkk))/10000
                               IF ( kk.LT.6 ) GOTO 20
                               IF ( kk.GT.8 ) GOTO 20
                               ll = IDHkk(kchaim(iix,iiy,lll))/10000
                               IF ( ll.LT.6 ) GOTO 15
                               IF ( ll.GT.8 ) GOTO 15
                               kk = IDHkk(kchaim(iix,iiy,kkk))/10000
                               IF ( kk.LT.6 ) GOTO 20
                               IF ( kk.GT.8 ) GOTO 20
                               IF ( ((idk2.LE.4 .AND. idk2.GE.1) .AND. (
     &                            idk1.GE.-4 .AND. idk1.LE.-1)) .AND. 
     &                            ((idl2.LE.-1 .AND. idl2.GE.-4) .AND. 
     &                            (idl1.GE.1 .AND. idl1.LE.4)) ) THEN
                               idmm1 = idm1/1000
                               idmm2 = (idm1-idmm1*1000)/100
                               IF ( ((idk1.EQ.-idmm1) .OR. (idk1.EQ.-
     &                            idmm2)) .AND. (idl2.EQ.-idm2) ) THEN
C                     aq-q and q-aq chains  and qq-q chain
C                          WRITE(6,*)IIX,IIY,KKK,LLL,MMM,
C    *                ' RKL,RKM ',RKL,RKM,IDK1,IDK2,IDL1,IDL2,IDM1,IDM2,
C    *      KCHAIM(IIX,IIY,KKK),KCHAIM(IIX,IIY,LLL),KCHAIM(IIX,IIY,MMM),
C    *      IDK,IDL,IDM
                               CALL DT_JOIN333(kchaim(iix,iiy,kkk),
     &                            kchaim(iix,iiy,lll),
     &                            kchaim(iix,iiy,mmm),irej)
                               IF ( irej.EQ.0 ) nchain33 = nchain33 + 1
C                          IF(IREJ.NE.0)WRITE(6,*)'Rejection in JOIN333'
                               END IF
                               END IF
                               END IF
                               END IF
                               END IF
                               END IF
                              END DO
                           END IF
                        END IF
                     END IF
 15               END DO
 20            END DO
            END IF
         END DO
      END DO
 
 
C     First'' round of fusions
 
C     WRITE(6,*)' entry density'
      DO iix = 1 , 30
         DO iiy = 1 , 30
            ichaim(iix,iiy) = 0
            DO kki = 1 , 600
               kchaim(iix,iiy,kki) = 0
            END DO
         END DO
      END DO
C
      DO kk = 1 , NHKk
         idh = IDHkk(kk)/10000
         IF ( idh.GE.7 ) THEN
            IF ( idh.LE.8 ) THEN
C        WRITE(6,'(2I4,I6,4I4,5F10.2,2I3,I2,I4,4E12.3)')
C    *                     KK,ISTHKK(KK),IDHKK(KK),
C    *             JMOHKK(1,KK),JMOHKK(2,KK),JDAHKK(1,KK),JDAHKK(2,KK),
C    *   (PHKK(LL,KK),LL=1,5),IDRES(KK),IDXRES(KK),NOBAM(KK),IDBAM(KK),
C    *   (VHKK(JJ,KK),JJ=1,4)
               iix = 15.D0 + 1.D12*VHKk(1,kk)/daaa
               iiy = 15.D0 + 1.D12*VHKk(2,kk)/daaa
               IF ( iix.LT.2 ) iix = 2
               IF ( iiy.LT.2 ) iiy = 2
               IF ( iix.GT.29 ) iix = 29
               IF ( iiy.GT.29 ) iiy = 29
               ichaim(iix,iiy) = ichaim(iix,iiy) + 1
C        NCHAIN=NCHAIN+1
               kkk = ichaim(iix,iiy)
               kchaim(iix,iiy,kkk) = kk
            END IF
         END IF
      END DO
C
      IF ( ncount.LE.20 ) THEN
         IF ( LPRi.GT.4 ) THEN
            WRITE (6,*) ' JJJ '
            DO iiy = 1 , 30
               WRITE (6,'(30I3)') (ichaim(iix,iiy),iix=1,30)
            END DO
         END IF
      END IF
 
C     First'' round of fusions include 66666 chains
 
      DO iix = 8 , 22
         DO iiy = 8 , 22
            IF ( ichaim(iix,iiy).GT.1 ) THEN
               DO kkk = 1 , ichaim(iix,iiy) - 1
                  DO lll = kkk + 1 , ichaim(iix,iiy)
                     kk = IDHkk(kchaim(iix,iiy,kkk))/10000
                     IF ( kk.LT.6 ) GOTO 30
                     IF ( kk.GT.8 ) GOTO 30
                     ll = IDHkk(kchaim(iix,iiy,lll))/10000
                     IF ( ll.GE.7 ) THEN
                        IF ( ll.LE.8 ) THEN
                           rkl = 1.D12*SQRT
     &                        ((VHKk(1,kchaim(iix,iiy,kkk))-
     &                        VHKk(1,kchaim(iix,iiy,lll)))
     &                        **2+(VHKk(2,kchaim(iix,iiy,kkk))
     &                        -VHKk(2,kchaim(iix,iiy,lll)))**2)
                           IF ( rkl.LE.aabbfus ) THEN
                              idk = IDHkk(kchaim(iix,iiy,kkk))
                              idk1 = IDHkk(kchaim(iix,iiy,kkk)-2)
                              idk2 = IDHkk(kchaim(iix,iiy,kkk)-1)
                              idl = IDHkk(kchaim(iix,iiy,lll))
                              idl1 = IDHkk(kchaim(iix,iiy,lll)-2)
                              idl2 = IDHkk(kchaim(iix,iiy,lll)-1)
                              DO mmm = 1 , ichaim(iix,iiy)
                               mm = IDHkk(kchaim(iix,iiy,mmm))/10000
                               IF ( mm.GE.6 ) THEN
                               IF ( mm.LE.8 ) THEN
                               ll = IDHkk(kchaim(iix,iiy,lll))/10000
                               IF ( ll.LT.6 ) GOTO 25
                               IF ( ll.GT.8 ) GOTO 25
                               kk = IDHkk(kchaim(iix,iiy,kkk))/10000
                               IF ( kk.LT.6 ) GOTO 30
                               IF ( kk.GT.8 ) GOTO 30
                               IF ( mmm.NE.kkk .AND. mmm.NE.lll ) THEN
                               rkm = 1.D12*SQRT
     &                            ((VHKk(1,kchaim(iix,iiy,kkk))
     &                            -VHKk(1,kchaim(iix,iiy,mmm)))
     &                            **2+(VHKk(2,kchaim(iix,iiy,kkk))
     &                            -VHKk(2,kchaim(iix,iiy,mmm)))**2)
                               rlm = 1.D12*SQRT
     &                            ((VHKk(1,kchaim(iix,iiy,lll))
     &                            -VHKk(1,kchaim(iix,iiy,mmm)))
     &                            **2+(VHKk(2,kchaim(iix,iiy,lll))
     &                            -VHKk(2,kchaim(iix,iiy,mmm)))**2)
                               IF ( rkm.LE.aabbfus .OR. rlm.LE.aabbfus )
     &                            THEN
                               idm = IDHkk(kchaim(iix,iiy,mmm))
                               idm1 = IDHkk(kchaim(iix,iiy,mmm)-2)
                               idm2 = IDHkk(kchaim(iix,iiy,mmm)-1)
                               IF ( ((idk1.LE.4 .AND. idk1.GE.1) .AND. (
     &                            idk2.GE.-4 .AND. idk2.LE.-1)) .AND. 
     &                            ((idl1.LE.-1 .AND. idl1.GE.-4) .AND. 
     &                            (idl2.GE.1 .AND. idl2.LE.4)) ) THEN
 
                               IF ( (idl2.EQ.-idm2) .AND. 
     &                            (idk1.EQ.-idl1) ) THEN
C                     q-aq and aq-q chains  plus q-aq chain
C                          WRITE(6,*) IIX,IIY,KKK,LLL,MMM,' RKL,RKM ',
C    *                            RKL,RKM,IDK1,IDK2,IDL1,IDL2,IDM1,IDM2,
C    *      KCHAIM(IIX,IIY,KKK),KCHAIM(IIX,IIY,LLL),KCHAIM(IIX,IIY,MMM),
C    *                                                      IDK,IDL,IDM
                               CALL DT_JOIN34(kchaim(iix,iiy,kkk),
     &                            kchaim(iix,iiy,lll),
     &                            kchaim(iix,iiy,mmm),irej)
                               IF ( irej.EQ.0 ) nchain34 = nchain34 + 1
C                          IF(IREJ.NE.0)WRITE(6,*)'Rejection in JOIN34'
                               END IF
                               END IF
                               kk = IDHkk(kchaim(iix,iiy,kkk))/10000
                               IF ( kk.LT.6 ) GOTO 30
                               IF ( kk.GT.8 ) GOTO 30
                               ll = IDHkk(kchaim(iix,iiy,lll))/10000
                               IF ( ll.LT.6 ) GOTO 25
                               IF ( ll.GT.8 ) GOTO 25
                               kk = IDHkk(kchaim(iix,iiy,kkk))/10000
                               IF ( kk.LT.6 ) GOTO 30
                               IF ( kk.GT.8 ) GOTO 30
                               IF ( ((idk2.LE.4 .AND. idk2.GE.1) .AND. (
     &                            idk1.GE.-4 .AND. idk1.LE.-1)) .AND. 
     &                            ((idl2.LE.-1 .AND. idl2.GE.-4) .AND. 
     &                            (idl1.GE.1 .AND. idl1.LE.4)) ) THEN
 
                               IF ( (idl2.EQ.-idk2) .AND. 
     &                            (idl1.EQ.-idm1) ) THEN
C                     aq-q and q-aq chains  and aq-q chain
C                          WRITE(6,*) IIX,IIY,KKK,LLL,MMM,' RKL,RKM ',
C    *                            RKL,RKM,IDK1,IDK2,IDL1,IDL2,IDM1,IDM2,
C    *      KCHAIM(IIX,IIY,KKK),KCHAIM(IIX,IIY,LLL),KCHAIM(IIX,IIY,MMM),
C    *                                                      IDK,IDL,IDM
                               CALL DT_JOIN344(kchaim(iix,iiy,kkk),
     &                            kchaim(iix,iiy,lll),
     &                            kchaim(iix,iiy,mmm),irej)
                               IF ( irej.EQ.0 ) nchain34 = nchain34 + 1
C                          IF(IREJ.NE.0)WRITE(6,*)'Rejection in JOIN344'
                               END IF
                               END IF
                               END IF
                               END IF
                               END IF
                               END IF
                              END DO
                           END IF
                        END IF
                     END IF
 25               END DO
 30            END DO
            END IF
         END DO
      END DO
 
C     Second round of fusions include 66666 chains
 
 
      ncount = ncount + 1
C     WRITE(6,*)' entry density'
      DO iix = 1 , 30
         DO iiy = 1 , 30
            ichaim(iix,iiy) = 0
            DO kki = 1 , 600
               kchaim(iix,iiy,kki) = 0
            END DO
         END DO
      END DO
C
      DO kk = 1 , NHKk
         idh = IDHkk(kk)/10000
         IF ( idh.GE.6 ) THEN
            IF ( idh.LE.8 ) THEN
C        WRITE(6,'(2I4,I6,4I4,5F10.2,2I3,I2,I4,4E12.3)')
C    *                                          KK,ISTHKK(KK),IDHKK(KK),
C    *              JMOHKK(1,KK),JMOHKK(2,KK),JDAHKK(1,KK),JDAHKK(2,KK),
C    *    (PHKK(LL,KK),LL=1,5),IDRES(KK),IDXRES(KK),NOBAM(KK),IDBAM(KK),
C    *    (VHKK(JJ,KK),JJ=1,4)
               iix = 15.D0 + 1.D12*VHKk(1,kk)/daaa
               iiy = 15.D0 + 1.D12*VHKk(2,kk)/daaa
               IF ( iix.LT.2 ) iix = 2
               IF ( iiy.LT.2 ) iiy = 2
               IF ( iix.GT.29 ) iix = 29
               IF ( iiy.GT.29 ) iiy = 29
               ichaim(iix,iiy) = ichaim(iix,iiy) + 1
               kkk = ichaim(iix,iiy)
               kchaim(iix,iiy,kkk) = kk
            END IF
         END IF
      END DO
C
      IF ( ncount.LE.20 ) THEN
         IF ( LPRi.GT.4 ) THEN
            WRITE (6,*) ' JJJ '
            DO iiy = 1 , 30
               WRITE (6,'(30I3)') (ichaim(iix,iiy),iix=1,30)
            END DO
         END IF
      END IF
 
      DO iix = 8 , 22
         DO iiy = 8 , 22
            IF ( ichaim(iix,iiy).GT.1 ) THEN
               DO kkk = 1 , ichaim(iix,iiy) - 1
                  DO lll = kkk + 1 , ichaim(iix,iiy)
                     kk = IDHkk(kchaim(iix,iiy,kkk))/10000
                     IF ( kk.LT.6 ) GOTO 40
                     IF ( kk.GT.8 ) GOTO 40
                     ll = IDHkk(kchaim(iix,iiy,lll))/10000
                     IF ( ll.GE.6 ) THEN
                        IF ( ll.LE.8 ) THEN
                           rkl = 1.D12*SQRT
     &                        ((VHKk(1,kchaim(iix,iiy,kkk))-
     &                        VHKk(1,kchaim(iix,iiy,lll)))
     &                        **2+(VHKk(2,kchaim(iix,iiy,kkk))
     &                        -VHKk(2,kchaim(iix,iiy,lll)))**2)
                           IF ( rkl.LE.aabbfus ) THEN
                              idk = IDHkk(kchaim(iix,iiy,kkk))
                              idk1 = IDHkk(kchaim(iix,iiy,kkk)-2)
                              idk2 = IDHkk(kchaim(iix,iiy,kkk)-1)
                              idl = IDHkk(kchaim(iix,iiy,lll))
                              idl1 = IDHkk(kchaim(iix,iiy,lll)-2)
                              idl2 = IDHkk(kchaim(iix,iiy,lll)-1)
                              IF ( ((idk1.LE.4 .AND. idk1.GE.1) .AND. (
     &                           idk2.GE.1000 .AND. idk2.LE.4000)) .AND. 
     &                           ((idl1.LE.4 .AND. idl1.GE.1) .AND. 
     &                           (idl2.GE.-4 .AND. idl2.LE.-1)) ) THEN
C                      q-qq and  q-aq chains
C                    WRITE(6,*)IIX,IIY,KKK,LLL,
C    *               ' RKL ',RKL,IDK1,IDK2,IDL1,IDL2,
C    *               KCHAIM(IIX,IIY,KKK),KCHAIM(IIX,IIY,LLL),IDK,IDL
                               iqq1 = idk2/1000
                               iqq2 = (idk2-iqq1*1000)/100
                               kk = IDHkk(kchaim(iix,iiy,kkk))/10000
                               IF ( kk.LT.6 ) GOTO 40
                               IF ( kk.GT.8 ) GOTO 40
                               IF ( iqq1.EQ.-idl2 .OR. iqq2.EQ.-idl2 )
     &                            THEN
C                       WRITE(6,*)' IQQ1,IQQ2,IDL2 ',IQQ1,IQQ2,IDL2
                               CALL DT_JOIN2(kchaim(iix,iiy,kkk),
     &                            kchaim(iix,iiy,lll),irej)
                               IF ( irej.EQ.0 ) nchain21 = nchain21 + 1
C                       IF(IREJ.NE.0)WRITE(6,*)'Rejection in JOIN2'
                               END IF
                              END IF
                              IF ( ((idk1.LE.4 .AND. idk1.GE.1) .AND. (
     &                           idk2.GE.-4 .AND. idk2.LE.-1)) .AND. 
     &                           ((idl1.LE.4 .AND. idl1.GE.1) .AND. 
     &                           (idl2.GE.1000 .AND. idl2.LE.4000)) )
     &                           THEN
C                     q-aq and q-qq chains
C                   WRITE(6,*)IIX,IIY,KKK,LLL,
C    *              ' RKL ',RKL,IDK1,IDK2,IDL1,IDL2,
C    *              KCHAIM(IIX,IIY,KKK),KCHAIM(IIX,IIY,LLL),IDK,IDL
                               iqq1 = idl2/1000
                               iqq2 = (idl2-iqq1*1000)/100
                               kk = IDHkk(kchaim(iix,iiy,kkk))/10000
                               IF ( kk.LT.6 ) GOTO 40
                               IF ( kk.GT.8 ) GOTO 40
                               ll = IDHkk(kchaim(iix,iiy,lll))/10000
                               IF ( ll.GE.6 ) THEN
                               IF ( ll.LE.8 ) THEN
                               IF ( iqq1.EQ.-idk2 .OR. iqq2.EQ.-idk2 )
     &                            THEN
C                       WRITE(6,*)' IQQ1,IQQ2,IDK2 ',IQQ1,IQQ2,IDK2
                               CALL DT_JOIN2(kchaim(iix,iiy,lll),
     &                            kchaim(iix,iiy,kkk),irej)
                               IF ( irej.EQ.0 ) nchain21 = nchain21 + 1
C                       IF(IREJ.NE.0)WRITE(6,*)'Rejection in JOIN2'
                               END IF
                               END IF
                               END IF
                              END IF
                           END IF
                        END IF
                     END IF
                  END DO
 40            END DO
            END IF
         END DO
      END DO
C
 
C     Second' round of fusions include 66666 chains
 
      ncount = ncount + 1
C     WRITE(6,*)' entry density'
      DO iix = 1 , 30
         DO iiy = 1 , 30
            ichaim(iix,iiy) = 0
            DO kki = 1 , 600
               kchaim(iix,iiy,kki) = 0
            END DO
         END DO
      END DO
C
      DO kk = 1 , NHKk
         idh = IDHkk(kk)/10000
         IF ( idh.GE.6 ) THEN
            IF ( idh.LE.8 ) THEN
C        WRITE(6,'(2I4,I6,4I4,5F10.2,2I3,I2,I4,4E12.3)')
C    *     KK,ISTHKK(KK),IDHKK(KK),
C    *     JMOHKK(1,KK),JMOHKK(2,KK),JDAHKK(1,KK),JDAHKK(2,KK),
C    *    (PHKK(LL,KK),LL=1,5),IDRES(KK),IDXRES(KK),NOBAM(KK),IDBAM(KK),
C    *    (VHKK(JJ,KK),JJ=1,4)
               iix = 15.5D0 + 1.D12*VHKk(1,kk)/daaa
               iiy = 15.5D0 + 1.D12*VHKk(2,kk)/daaa
               IF ( iix.LT.2 ) iix = 2
               IF ( iiy.LT.2 ) iiy = 2
               IF ( iix.GT.29 ) iix = 29
               IF ( iiy.GT.29 ) iiy = 29
               ichaim(iix,iiy) = ichaim(iix,iiy) + 1
               kkk = ichaim(iix,iiy)
               kchaim(iix,iiy,kkk) = kk
            END IF
         END IF
      END DO
C
      IF ( ncount.LE.20 ) THEN
         IF ( LPRi.GT.4 ) THEN
            WRITE (6,*) ' JJJ '
            DO iiy = 1 , 30
               WRITE (6,'(30I3)') (ichaim(iix,iiy),iix=1,30)
            END DO
         END IF
      END IF
 
      DO iix = 8 , 22
         DO iiy = 8 , 22
            IF ( ichaim(iix,iiy).GT.1 ) THEN
               DO kkk = 1 , ichaim(iix,iiy) - 1
                  DO lll = kkk + 1 , ichaim(iix,iiy)
                     kk = IDHkk(kchaim(iix,iiy,kkk))/10000
                     IF ( kk.LT.6 ) GOTO 50
                     IF ( kk.GT.8 ) GOTO 50
                     ll = IDHkk(kchaim(iix,iiy,lll))/10000
                     IF ( ll.GE.6 ) THEN
                        IF ( ll.LE.8 ) THEN
                           rkl = 1.D12*SQRT
     &                        ((VHKk(1,kchaim(iix,iiy,kkk))-
     &                        VHKk(1,kchaim(iix,iiy,lll)))
     &                        **2+(VHKk(2,kchaim(iix,iiy,kkk))
     &                        -VHKk(2,kchaim(iix,iiy,lll)))**2)
                           IF ( rkl.LE.aabbfus ) THEN
                              idk = IDHkk(kchaim(iix,iiy,kkk))
                              idk1 = IDHkk(kchaim(iix,iiy,kkk)-2)
                              idk2 = IDHkk(kchaim(iix,iiy,kkk)-1)
                              idl = IDHkk(kchaim(iix,iiy,lll))
                              idl1 = IDHkk(kchaim(iix,iiy,lll)-2)
                              idl2 = IDHkk(kchaim(iix,iiy,lll)-1)
                              IF ( ((idk1.LE.4 .AND. idk1.GE.1) .AND. (
     &                           idk2.GE.1000 .AND. idk2.LE.4000)) .AND. 
     &                           ((idl1.LE.4 .AND. idl1.GE.1) .AND. 
     &                           (idl2.GE.-4 .AND. idl2.LE.-1)) ) THEN
C                      q-qq and  q-aq chains
C                    WRITE(6,*)IIX,IIY,KKK,LLL,
C    *                       ' RKL ',RKL,IDK1,IDK2,IDL1,IDL2,
C    *                   KCHAIM(IIX,IIY,KKK),KCHAIM(IIX,IIY,LLL),IDK,IDL
                               iqq1 = idk2/1000
                               iqq2 = (idk2-iqq1*1000)/100
                               kk = IDHkk(kchaim(iix,iiy,kkk))/10000
                               IF ( kk.LT.6 ) GOTO 50
                               IF ( kk.GT.8 ) GOTO 50
                               IF ( iqq1.EQ.-idl2 .OR. iqq2.EQ.-idl2 )
     &                            THEN
C                       WRITE(6,*)' IQQ1,IQQ2,IDL2 ',IQQ1,IQQ2,IDL2
                               CALL DT_JOIN2(kchaim(iix,iiy,kkk),
     &                            kchaim(iix,iiy,lll),irej)
                               IF ( irej.EQ.0 ) nchain21 = nchain21 + 1
C                       IF(IREJ.NE.0)WRITE(6,*)'Rejection in JOIN2'
                               END IF
                              END IF
                              IF ( ((idk1.LE.4 .AND. idk1.GE.1) .AND. (
     &                           idk2.GE.-4 .AND. idk2.LE.-1)) .AND. 
     &                           ((idl1.LE.4 .AND. idl1.GE.1) .AND. 
     &                           (idl2.GE.1000 .AND. idl2.LE.4000)) )
     &                           THEN
C                     q-aq and q-qq chains
C                    WRITE(6,*)IIX,IIY,KKK,LLL,
C    *                       ' RKL ',RKL,IDK1,IDK2,IDL1,IDL2,
C    *               KCHAIM(IIX,IIY,KKK),KCHAIM(IIX,IIY,LLL),IDK,IDL
                               iqq1 = idl2/1000
                               iqq2 = (idl2-iqq1*1000)/100
                               kk = IDHkk(kchaim(iix,iiy,kkk))/10000
                               IF ( kk.LT.6 ) GOTO 50
                               IF ( kk.GT.8 ) GOTO 50
                               ll = IDHkk(kchaim(iix,iiy,lll))/10000
                               IF ( ll.GE.6 ) THEN
                               IF ( ll.LE.8 ) THEN
                               IF ( iqq1.EQ.-idk2 .OR. iqq2.EQ.-idk2 )
     &                            THEN
C                       WRITE(6,*)' IQQ1,IQQ2,IDK2 ',IQQ1,IQQ2,IDK2
                               CALL DT_JOIN2(kchaim(iix,iiy,lll),
     &                            kchaim(iix,iiy,kkk),irej)
                               IF ( irej.EQ.0 ) nchain21 = nchain21 + 1
C                       IF(IREJ.NE.0)WRITE(6,*)'Rejection in JOIN2'
                               END IF
                               END IF
                               END IF
                              END IF
                           END IF
                        END IF
                     END IF
                  END DO
 50            END DO
            END IF
         END DO
      END DO
C
 
C     Third round of fusions include 66666 chains
 
      ncount = ncount + 1
C     WRITE(6,*)' entry density'
      DO iix = 1 , 30
         DO iiy = 1 , 30
            ichaim(iix,iiy) = 0
            DO kki = 1 , 600
               kchaim(iix,iiy,kki) = 0
            END DO
         END DO
      END DO
C
      DO kk = 1 , NHKk
         idh = IDHkk(kk)/10000
         IF ( idh.GE.6 ) THEN
            IF ( idh.LE.8 ) THEN
C        WRITE(6,'(2I4,I6,4I4,5F10.2,2I3,I2,I4,4E12.3)')
C    *     KK,ISTHKK(KK),IDHKK(KK),
C    *     JMOHKK(1,KK),JMOHKK(2,KK),JDAHKK(1,KK),JDAHKK(2,KK),
C    *    (PHKK(LL,KK),LL=1,5),IDRES(KK),IDXRES(KK),NOBAM(KK),IDBAM(KK),
C    *    (VHKK(JJ,KK),JJ=1,4)
               iix = 15.5D0 + 1.D12*VHKk(1,kk)/daaa
               iiy = 15.5D0 + 1.D12*VHKk(2,kk)/daaa
               IF ( iix.LT.2 ) iix = 2
               IF ( iiy.LT.2 ) iiy = 2
               IF ( iix.GT.29 ) iix = 29
               IF ( iiy.GT.29 ) iiy = 29
               ichaim(iix,iiy) = ichaim(iix,iiy) + 1
               kkk = ichaim(iix,iiy)
               kchaim(iix,iiy,kkk) = kk
            END IF
         END IF
      END DO
C
      IF ( ncount.LE.20 ) THEN
         IF ( LPRi.GT.4 ) THEN
            WRITE (6,*) ' JJJ '
            DO iiy = 1 , 30
               WRITE (6,'(30I3)') (ichaim(iix,iiy),iix=1,30)
            END DO
         END IF
      END IF
 
      DO iix = 8 , 22
         DO iiy = 8 , 22
            IF ( ichaim(iix,iiy).GT.1 ) THEN
               DO kkk = 1 , ichaim(iix,iiy) - 1
                  DO lll = kkk + 1 , ichaim(iix,iiy)
                     kk = IDHkk(kchaim(iix,iiy,kkk))/10000
                     IF ( kk.LT.6 ) GOTO 60
                     IF ( kk.GT.8 ) GOTO 60
                     ll = IDHkk(kchaim(iix,iiy,lll))/10000
                     IF ( ll.GE.6 ) THEN
                        IF ( ll.LE.8 ) THEN
                           rkl = 1.D12*SQRT
     &                        ((VHKk(1,kchaim(iix,iiy,kkk))-
     &                        VHKk(1,kchaim(iix,iiy,lll)))
     &                        **2+(VHKk(2,kchaim(iix,iiy,kkk))
     &                        -VHKk(2,kchaim(iix,iiy,lll)))**2)
                           IF ( rkl.LE.aabbfus ) THEN
                              idk = IDHkk(kchaim(iix,iiy,kkk))
                              idk1 = IDHkk(kchaim(iix,iiy,kkk)-2)
                              idk2 = IDHkk(kchaim(iix,iiy,kkk)-1)
                              idl = IDHkk(kchaim(iix,iiy,lll))
                              idl1 = IDHkk(kchaim(iix,iiy,lll)-2)
                              idl2 = IDHkk(kchaim(iix,iiy,lll)-1)
                              DO mmm = 1 , ichaim(iix,iiy)
                               kk = IDHkk(kchaim(iix,iiy,kkk))/10000
                               IF ( kk.LT.6 ) GOTO 60
                               IF ( kk.GT.8 ) GOTO 60
                               ll = IDHkk(kchaim(iix,iiy,lll))/10000
                               IF ( ll.LT.6 ) GOTO 55
                               IF ( ll.GT.8 ) GOTO 55
                               mm = IDHkk(kchaim(iix,iiy,mmm))/10000
                               IF ( mm.GE.6 ) THEN
                               IF ( mm.LE.8 ) THEN
                               IF ( mmm.NE.kkk .AND. mmm.NE.lll ) THEN
                               rkm = 1.D12*SQRT
     &                            ((VHKk(1,kchaim(iix,iiy,kkk))
     &                            -VHKk(1,kchaim(iix,iiy,mmm)))
     &                            **2+(VHKk(2,kchaim(iix,iiy,kkk))
     &                            -VHKk(2,kchaim(iix,iiy,mmm)))**2)
                               rlm = 1.D12*SQRT
     &                            ((VHKk(1,kchaim(iix,iiy,lll))
     &                            -VHKk(1,kchaim(iix,iiy,mmm)))
     &                            **2+(VHKk(2,kchaim(iix,iiy,lll))
     &                            -VHKk(2,kchaim(iix,iiy,mmm)))**2)
                               IF ( rkm.LE.aabbfus .OR. rlm.LE.aabbfus )
     &                            THEN
                               idm = IDHkk(kchaim(iix,iiy,mmm))
                               idm1 = IDHkk(kchaim(iix,iiy,mmm)-2)
                               idm2 = IDHkk(kchaim(iix,iiy,mmm)-1)
                               kk = IDHkk(kchaim(iix,iiy,kkk))/10000
                               IF ( kk.LT.6 ) GOTO 60
                               IF ( kk.GT.8 ) GOTO 60
                               ll = IDHkk(kchaim(iix,iiy,lll))/10000
                               IF ( ll.LT.6 ) GOTO 55
                               IF ( ll.GT.8 ) GOTO 55
                               mm = IDHkk(kchaim(iix,iiy,mmm))/10000
                               IF ( mm.GE.6 ) THEN
                               IF ( mm.LE.8 ) THEN
                               IF ( ((idk1.LE.4 .AND. idk1.GE.1) .AND. (
     &                            idk2.GE.-4 .AND. idk2.LE.-1)) .AND. 
     &                            ((idl1.LE.-1 .AND. idl1.GE.-4) .AND. 
     &                            (idl2.GE.1 .AND. idl2.LE.4)) ) THEN
                               IF ( (idk1.EQ.-idl1) .AND. 
     &                            (idk2.EQ.-idl2) ) THEN
C                     q-aq and aq-q chains
C                       WRITE(6,*)IIX,IIY,KKK,LLL,MMM,' RKL,RKM ',
C    *                  RKL,RKM,IDK1,IDK2,IDL1,IDL2,IDM1,IDM2,
C    *      KCHAIM(IIX,IIY,KKK),KCHAIM(IIX,IIY,LLL),KCHAIM(IIX,IIY,MMM),
C    *      IDK,IDL,IDM
                               CALL DT_JOIN3(kchaim(iix,iiy,kkk),
     &                            kchaim(iix,iiy,lll),
     &                            kchaim(iix,iiy,mmm),irej)
                               IF ( irej.EQ.0 ) nchain32 = nchain32 + 1
C                          IF(IREJ.NE.0)WRITE(6,*)'Rejection in JOIN3'
                               END IF
                               END IF
                               kk = IDHkk(kchaim(iix,iiy,kkk))/10000
                               IF ( kk.LT.6 ) GOTO 60
                               IF ( kk.GT.8 ) GOTO 60
                               ll = IDHkk(kchaim(iix,iiy,lll))/10000
                               IF ( ll.LT.6 ) GOTO 55
                               IF ( ll.GT.8 ) GOTO 55
                               mm = IDHkk(kchaim(iix,iiy,mmm))/10000
                               IF ( mm.GE.6 ) THEN
                               IF ( mm.LE.8 ) THEN
                               IF ( ((idk2.LE.4 .AND. idk2.GE.1) .AND. (
     &                            idk1.GE.-4 .AND. idk1.LE.-1)) .AND. 
     &                            ((idl2.LE.-1 .AND. idl2.GE.-4) .AND. 
     &                            (idl1.GE.1 .AND. idl1.LE.4)) ) THEN
                               IF ( (idk1.EQ.-idl1) .AND. 
     &                            (idk2.EQ.-idl2) ) THEN
C                     aq-q and q-aq chains
C                         WRITE(6,*)IIX,IIY,KKK,LLL,MMM,' RKL,RKM ',
C    *                    RKL,RKM,IDK1,IDK2,IDL1,IDL2,IDM1,IDM2,
C    *      KCHAIM(IIX,IIY,KKK),KCHAIM(IIX,IIY,LLL),KCHAIM(IIX,IIY,MMM),
C    *      IDK,IDL,IDM
                               CALL DT_JOIN3(kchaim(iix,iiy,kkk),
     &                            kchaim(iix,iiy,lll),
     &                            kchaim(iix,iiy,mmm),irej)
                               IF ( irej.EQ.0 ) nchain32 = nchain32 + 1
C                          IF(IREJ.NE.0)WRITE(6,*)'Rejection in JOIN3'
                               END IF
                               END IF
                               END IF
                               END IF
                               END IF
                               END IF
                               END IF
                               END IF
                               END IF
                               END IF
                              END DO
                           END IF
                        END IF
                     END IF
 55               END DO
 60            END DO
            END IF
         END DO
      END DO
 
 
C
 
C     Fourth round of fusions include 66666 chains
 
      ncount = ncount + 1
C     WRITE(6,*)' entry density'
      DO iix = 1 , 30
         DO iiy = 1 , 30
            ichaim(iix,iiy) = 0
            DO kki = 1 , 400
               kchaim(iix,iiy,kki) = 0
            END DO
         END DO
      END DO
C
      DO kk = 1 , NHKk
         idh = IDHkk(kk)/10000
         IF ( idh.GE.6 ) THEN
            IF ( idh.LE.8 ) THEN
C        WRITE(6,'(2I4,I6,4I4,5F10.2,2I3,I2,I4,4E12.3)')
C    *     KK,ISTHKK(KK),IDHKK(KK),
C    *     JMOHKK(1,KK),JMOHKK(2,KK),JDAHKK(1,KK),JDAHKK(2,KK),
C    *    (PHKK(LL,KK),LL=1,5),IDRES(KK),IDXRES(KK),NOBAM(KK),IDBAM(KK),
C    *    (VHKK(JJ,KK),JJ=1,4)
               iix = 15.D0 + 1.D12*VHKk(1,kk)/daaa
               iiy = 15.D0 + 1.D12*VHKk(2,kk)/daaa
               IF ( iix.LT.2 ) iix = 2
               IF ( iiy.LT.2 ) iiy = 2
               IF ( iix.GT.29 ) iix = 29
               IF ( iiy.GT.29 ) iiy = 29
               ichaim(iix,iiy) = ichaim(iix,iiy) + 1
               nchainn = nchainn + 1
               kkk = ichaim(iix,iiy)
               kchaim(iix,iiy,kkk) = kk
            END IF
         END IF
      END DO
C
      IF ( ncount.LE.20 ) THEN
         IF ( LPRi.GT.4 ) THEN
            WRITE (6,*) ' JJJ '
            DO iiy = 1 , 30
               WRITE (6,'(30I3)') (ichaim(iix,iiy),iix=1,30)
            END DO
         END IF
      END IF
 
      DO iix = 8 , 22
         DO iiy = 8 , 22
            IF ( ichaim(iix,iiy).GT.1 ) THEN
               DO kkk = 1 , ichaim(iix,iiy) - 1
                  DO lll = kkk + 1 , ichaim(iix,iiy)
                     kk = IDHkk(kchaim(iix,iiy,kkk))/10000
                     IF ( kk.LT.6 ) GOTO 70
                     IF ( kk.GT.8 ) GOTO 70
                     ll = IDHkk(kchaim(iix,iiy,lll))/10000
                     IF ( ll.GE.6 ) THEN
                        IF ( ll.LE.8 ) THEN
                           rkl = 1.D12*SQRT
     &                        ((VHKk(1,kchaim(iix,iiy,kkk))-
     &                        VHKk(1,kchaim(iix,iiy,lll)))
     &                        **2+(VHKk(2,kchaim(iix,iiy,kkk))
     &                        -VHKk(2,kchaim(iix,iiy,lll)))**2)
                           IF ( rkl.LE.0.40D0 ) THEN
                              idk = IDHkk(kchaim(iix,iiy,kkk))
                              idk1 = IDHkk(kchaim(iix,iiy,kkk)-2)
                              idk2 = IDHkk(kchaim(iix,iiy,kkk)-1)
                              idl = IDHkk(kchaim(iix,iiy,lll))
                              idl1 = IDHkk(kchaim(iix,iiy,lll)-2)
                              idl2 = IDHkk(kchaim(iix,iiy,lll)-1)
                              kk = IDHkk(kchaim(iix,iiy,kkk))/10000
                              IF ( kk.LT.6 ) GOTO 70
                              IF ( kk.GT.8 ) GOTO 70
                              IF ( ((idk1.LE.4 .AND. idk1.GE.1) .AND. (
     &                           idk2.GE.-4 .AND. idk2.LE.-1)) .AND. 
     &                           ((idl1.LE.4 .AND. idl1.GE.1) .AND. 
     &                           (idl2.GE.-4 .AND. idl2.LE.-1)) ) THEN
C                    two q-aq chains
C                    WRITE(6,*)IIX,IIY,KKK,LLL,
C    *             ' RKL ',RKL,IDK1,IDK2,IDL1,IDL2,
C    *               KCHAIM(IIX,IIY,KKK),KCHAIM(IIX,IIY,LLL),IDK,IDL
                               CALL DT_JOIN(kchaim(iix,iiy,kkk),
     &                            kchaim(iix,iiy,lll),irej)
                               IF ( irej.EQ.0 ) nchain22 = nchain22 + 1
C                    IF(IREJ.NE.0)WRITE(6,*)'Rejection in JOIN '
                              END IF
                              kk = IDHkk(kchaim(iix,iiy,kkk))/10000
                              IF ( kk.LT.6 ) GOTO 70
                              IF ( kk.GT.8 ) GOTO 70
                              ll = IDHkk(kchaim(iix,iiy,lll))/10000
                              IF ( ll.GE.6 ) THEN
                               IF ( ll.LE.8 ) THEN
                               IF ( ((idk2.LE.4 .AND. idk2.GE.1) .AND. (
     &                            idk1.GE.-4 .AND. idk1.LE.-1)) .AND. 
     &                            ((idl2.LE.4 .AND. idl2.GE.1) .AND. 
     &                            (idl1.GE.-4 .AND. idl1.LE.-1)) ) THEN
C                    two aq-q chains
C                    WRITE(6,*)IIX,IIY,KKK,LLL,
C    *             ' RKL ',RKL,IDK1,IDK2,IDL1,IDL2,
C    *               KCHAIM(IIX,IIY,KKK),KCHAIM(IIX,IIY,LLL),IDK,IDL
                               CALL DT_JOIN(kchaim(iix,iiy,kkk),
     &                            kchaim(iix,iiy,lll),irej)
                               IF ( irej.EQ.0 ) nchain22 = nchain22 + 1
C                    IF(IREJ.NE.0)WRITE(6,*)'Rejection in JOIN '
                               END IF
                               END IF
                              END IF
                           END IF
                        END IF
                     END IF
                  END DO
 70            END DO
            END IF
         END DO
      END DO
C
C     WRITE(6,*)'NCHAIN,NCHAINN,NCHAIN31,NCHAIN32,NCHAIN33,NCHAIN34',
C    * 'NCHAIN21,NCHAIN22 ',
C    *NCHAIN,NCHAINN,NCHAIN31,NCHAIN32,NCHAIN33,NCHAIN34,
C    *NCHAIN21,NCHAIN22
C                       Count 22222 chains
      n66666 = 0
      DO i = 1 , NHKk
         IF ( IDHkk(i).EQ.66666 ) n66666 = n66666 + 1
      END DO
C     WRITE(6,*)' N66666 ',N66666
      n22222 = 0
      DO i = 1 , NHKk
         IF ( IDHkk(i).EQ.22222 ) n22222 = n22222 + 1
      END DO
C      WRITE(6,*)' N22222 ',N22222
      nalll = 0
      DO i = 1 , NHKk
         IF ( IDHkk(i)/10000.EQ.6 ) nalll = nalll + 1
         IF ( IDHkk(i)/10000.EQ.7 ) nalll = nalll + 1
         IF ( IDHkk(i)/10000.EQ.8 ) nalll = nalll + 1
      END DO
C     WRITE(6,*)' NALLL ',NALLL
 
      END SUBROUTINE
