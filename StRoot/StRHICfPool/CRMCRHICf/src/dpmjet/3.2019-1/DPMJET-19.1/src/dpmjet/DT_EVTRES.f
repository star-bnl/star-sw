
      SUBROUTINE DT_EVTRES(Irej)
 
C***********************************************************************
C This version dated 14.12.94 is written by S. Roesler                 *
C***********************************************************************
 
      IMPLICIT NONE
      DOUBLE PRECISION am1 , am2 , amch1 , amch1n , amch2 , amch2n , 
     &                 ammx , pp1 , pp2 , pt1 , pt2 , TINY10 , TINY5
      INTEGER i , idch2 , idr2 , IDT_IPDG2B , idxr2 , ifp , ifpr1 , 
     &        ifpr2 , ift , ifta1 , ifta2 , immx , imo11 , imo12 , 
     &        imo21 , imo22 , Irej , irej1 , j , k
      SAVE 
 
      INCLUDE 'inc/dtflka'
 
      PARAMETER (TINY5=1.0D-5,TINY10=1.0D-10)
 
C event history
 
 
      INCLUDE 'inc/dtevt1'
C extended event history
      INCLUDE 'inc/dtevt2'
C flags for input different options
      INCLUDE 'inc/dtflg1'
C particle properties (BAMJET index convention)
      INCLUDE 'inc/dtpart'
 
      DIMENSION pp1(4) , pp2(4) , pt1(4) , pt2(4) , ifp(2) , ift(2)
 
      Irej = 0
 
      DO i = NPOint(3) , NHKk
         IF ( ABS(IDRes(i)).GE.100 ) THEN
            ammx = 0.0D0
            DO j = NPOint(3) , NHKk
               IF ( IDHkk(j).EQ.88888 ) THEN
                  IF ( PHKk(5,j).GT.ammx ) THEN
                     ammx = PHKk(5,j)
                     immx = j
                  END IF
               END IF
            END DO
            IF ( IDRes(immx).NE.0 ) THEN
               IF ( IOUlev(3).GT.0 ) THEN
 
C                 GOTO 6
                  IF ( LPRi.GT.4 ) WRITE (LOUt,'(1X,A)')
     &                  'EVTRES: no chain for correc. found'
               END IF
               GOTO 200
            END IF
            imo11 = JMOhkk(1,i)
            imo12 = JMOhkk(2,i)
            IF ( PHKk(3,imo11).LT.0.0D0 ) THEN
               imo11 = JMOhkk(2,i)
               imo12 = JMOhkk(1,i)
            END IF
            imo21 = JMOhkk(1,immx)
            imo22 = JMOhkk(2,immx)
            IF ( PHKk(3,imo21).LT.0.0D0 ) THEN
               imo21 = JMOhkk(2,immx)
               imo22 = JMOhkk(1,immx)
            END IF
            amch1 = PHKk(5,i)
            amch1n = AAM(IDXres(i))
 
            ifpr1 = IDHkk(imo11)
            ifpr2 = IDHkk(imo21)
            ifta1 = IDHkk(imo12)
            ifta2 = IDHkk(imo22)
            DO j = 1 , 4
               pp1(j) = PHKk(j,imo11)
               pp2(j) = PHKk(j,imo21)
               pt1(j) = PHKk(j,imo12)
               pt2(j) = PHKk(j,imo22)
            END DO
C store initial configuration for energy-momentum cons. check
C correct kinematics of second chain
            IF ( LEMcck ) CALL DT_EMC1(pp1,pp2,pt1,pt2,1,1,irej1)
            CALL DT_CHKINE(pp1,ifpr1,pp2,ifpr2,pt1,ifta1,pt2,ifta2,
     &                     amch1,amch1n,amch2,irej1)
            IF ( irej1.NE.0 ) GOTO 200
C check now this chain for resonance mass
            ifp(1) = IDT_IPDG2B(ifpr2,1,2)
            ifp(2) = 0
            IF ( ABS(ifpr2).GE.1000 ) ifp(2) = IDT_IPDG2B(ifpr2,2,2)
            ift(1) = IDT_IPDG2B(ifta2,1,2)
            ift(2) = 0
            IF ( ABS(ifta2).GE.1000 ) ift(2) = IDT_IPDG2B(ifta2,2,2)
            idch2 = 2
            IF ( (ifp(2).EQ.0) .AND. (ift(2).EQ.0) ) idch2 = 1
            IF ( (ifp(2).NE.0) .AND. (ift(2).NE.0) ) idch2 = 3
            CALL DT_CH2RES(ifp(1),ifp(2),ift(1),ift(2),idr2,idxr2,amch2,
     &                     amch2n,idch2,irej1)
            IF ( (irej1.NE.0) .OR. (idr2.NE.0) ) THEN
 
               IF ( IOUlev(1).GT.0 .AND. LPRi.GT.4 ) WRITE (LOUt,*)
     &               ' correction for resonance not poss.'
C*sr test
C              GOTO 1
C              GOTO 9999
C*
            END IF
C store final configuration for energy-momentum cons. check
            IF ( LEMcck ) THEN
               CALL DT_EMC1(pp1,pp2,pt1,pt2,-2,1,irej1)
               CALL DT_EMC1(pp1,pp2,pt1,pt2,3,1,irej1)
               IF ( irej1.NE.0 ) GOTO 200
            END IF
            DO j = 1 , 4
               PHKk(j,imo11) = pp1(j)
               PHKk(j,imo21) = pp2(j)
               PHKk(j,imo12) = pt1(j)
               PHKk(j,imo22) = pt2(j)
            END DO
C correct entries of chains
            DO k = 1 , 4
               PHKk(k,i) = PHKk(k,imo11) + PHKk(k,imo12)
               PHKk(k,immx) = PHKk(k,imo21) + PHKk(k,imo22)
            END DO
            am1 = PHKk(4,i)**2 - PHKk(1,i)**2 - PHKk(2,i)**2 - PHKk(3,i)
     &            **2
            am2 = PHKk(4,immx)**2 - PHKk(1,immx)**2 - PHKk(2,immx)
     &            **2 - PHKk(3,immx)**2
C ?? the following should now be obsolete
C*sr test
C           IF ((AM1.LT.0.0D0).OR.(AM2.LT.1.0D0)) THEN
            IF ( (am1.LT.0.0D0) .OR. (am2.LT.0.0D0) ) THEN
C*
 
               IF ( LPRi.GT.4 ) WRITE (LOUt,'(1X,A,4G10.3)')
     &               'EVTRES: inonsistent mass-corr.' , am1 , am2
C              GOTO 9999
               GOTO 100
            END IF
            PHKk(5,i) = SQRT(am1)
            PHKk(5,immx) = SQRT(am2)
            IDRes(i) = IDRes(i)/100
            IF ( (ABS(PHKk(5,i)-amch1n).GT.TINY5) .OR. 
     &           (ABS(PHKk(5,immx)-amch2).GT.TINY5) ) THEN
 
               IF ( LPRi.GT.4 ) WRITE (LOUt,'(1X,A,4G10.3)')
     &               'EVTRES: inconsistent chain-masses' , PHKk(5,i) , 
     &              amch1n , PHKk(5,immx) , amch2
               GOTO 200
            END IF
         END IF
 100  END DO
C   6 CONTINUE
      RETURN
 
 200  Irej = 1
      END SUBROUTINE
