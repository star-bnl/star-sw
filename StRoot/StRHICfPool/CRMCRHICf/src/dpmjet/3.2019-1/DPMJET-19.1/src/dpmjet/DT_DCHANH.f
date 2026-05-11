
      SUBROUTINE DT_DCHANH
 
      IMPLICIT NONE
      DOUBLE PRECISION am111 , am222 , ams , amss , hv , hwk , hwt , 
     &                 sinorc , sio , sis , SI
      INTEGER i , ie , iee , ieo , iika , iiki , ik , ik1 , ik2 , ike , 
     &        inrk1 , inrk2 , inrkk , inrko , inzk1 , inzk2 , inzk3 , 
     &        ire , ireg , iwk
      INTEGER iwko , j , ji
      SAVE 
 
      INCLUDE 'inc/dtflka'
 
C particle properties (BAMJET index convention),
C (dublicate of DTPART for HADRIN)
      INCLUDE 'inc/hnablt'
      INCLUDE 'inc/hnspli'
      INCLUDE 'inc/hnredv'
      INCLUDE 'inc/hnreac'
 
      DIMENSION hwt(460) , hwk(40) , SI(5184)
      EQUIVALENCE (WK(1),SI(1))
C--------------------
C--------------------------
      ireg = 16
      DO ire = 1 , ireg
         iwko = IRIi(ire)
         iee = IEIi(ire+1) - IEIi(ire)
         ike = IKIi(ire+1) - IKIi(ire)
         ieo = IEIi(ire) + 1
         iika = IKIi(ire)
C   modifications to suppress elestic scattering  24/07/91
         DO ie = 1 , iee
            sis = 1.D-14
            sinorc = 0.0D0
            DO ik = 1 , ike
               iwk = iwko + iee*(ik-1) + ie
               IF ( NRK(2,iika+ik).EQ.0 ) sinorc = 1.0D0
               sis = sis + SI(iwk)*sinorc
            END DO
            SIIn(ieo+ie-1) = sis
            sio = 0.D0
            IF ( sis.LT.1.D-12 ) THEN
               sis = 1.D0
               sio = 1.D0
            END IF
            sinorc = 0.0D0
            DO ik = 1 , ike
               iwk = iwko + iee*(ik-1) + ie
               IF ( NRK(2,iika+ik).EQ.0 ) sinorc = 1.0D0
               sio = sio + SI(iwk)*sinorc/sis
               hwk(ik) = sio
            END DO
            DO ik = 1 , ike
               iwk = iwko + iee*(ik-1) + ie
               WK(iwk) = hwk(ik)
            END DO
            iiki = IKIi(ire)
            DO ik = 1 , ike
               am111 = 0.D0
               inrk1 = NRK(1,iiki+ik)
               IF ( inrk1.GT.0 ) am111 = AMH(inrk1)
               am222 = 0.D0
               inrk2 = NRK(2,iiki+ik)
               IF ( inrk2.GT.0 ) am222 = AMH(inrk2)
               THResh(iiki+ik) = am111 + am222
               IF ( inrk2.LT.1 ) THEN
                  inrkk = K1H(inrk1)
                  amss = 5.D0
                  inrko = K2H(inrk1)
                  DO inrk1 = inrkk , inrko
                     inzk1 = NZKi(inrk1,1)
                     inzk2 = NZKi(inrk1,2)
                     inzk3 = NZKi(inrk1,3)
                     IF ( inzk1.GT.0 .AND. inzk1.LE.110 ) THEN
                        IF ( inzk2.GT.0 .AND. inzk2.LE.110 ) THEN
C     WRITE (6,310)INRK1,INZK1,INZK2,INZK3
                           IF ( inzk3.GT.0 .AND. inzk3.LE.110 ) THEN
C1000 FORMAT (4I10)
                              ams = AMH(inzk1) + AMH(inzk2)
                              IF ( inzk3.GE.1 ) ams = ams + AMH(inzk3)
                              IF ( amss.GT.ams ) amss = ams
                           END IF
                        END IF
                     END IF
                  END DO
                  ams = amss
                  IF ( ams.LT.UMO(ieo) ) ams = UMO(ieo)
                  THResh(iiki+ik) = ams
               END IF
            END DO
         END DO
      END DO
      DO j = 1 , 460
         hwt(j) = 0.D0
      END DO
      DO i = 1 , 110
         ik1 = K1H(i)
         ik2 = K2H(i)
         hv = 0.D0
         IF ( ik2.GT.460 ) ik2 = 460
         IF ( ik1.LE.0 ) ik1 = 1
         DO j = ik1 , ik2
            hv = hv + WTI(j)
            hwt(j) = hv
            ji = j
         END DO
 
         IF ( LPRi.GT.4 .AND. ABS(hv-1.0D0).GT.1.D-4 )
     &        WRITE (LOUt,99010) i , ji , hv
99010    FORMAT (' ERROR IN HWT, FALSE USE OF CHANWH ',2I6,F10.2)
      END DO
      DO j = 1 , 460
         WTI(j) = hwt(j)
      END DO
      END SUBROUTINE
