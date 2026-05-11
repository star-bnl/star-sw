
      SUBROUTINE DT_JOIN333(Idx1,Idx2,Idx3,Irej)
 
C***********************************************************************
C This subroutine joins a aq-q and a q-aq chain to a third qq-q chain.          *
C     IDX1, IDX2 ,IDX3      DTEVT1 indices of chains to be joined           *
C This version dated 11.01.95 is written by S. Roesler.                *
C***********************************************************************
 
      IMPLICIT NONE
      INTEGER i , idd , idmm1 , idmm2 , idx , Idx1 , Idx2 , Idx3 , if1 , 
     &        if2 , Irej , irej1 , ist1 , ist2 , j , k , kch , mo
      DOUBLE PRECISION p1 , p1pt , p2 , p2pt , pch , pp , pppt , pptt1 , 
     &                 pptt2 , pptt3 , pptt4 , pptt5 , pptt6 , ppttmax , 
     &                 pt , ptpt , PYMASS , xm1 , xm2
      SAVE 
      INCLUDE 'inc/dtflka'
 
C event history
      INCLUDE 'inc/dtevt1'
C extended event history
      INCLUDE 'inc/dtevt2'
 
C flags for input different options
      INCLUDE 'inc/dtflg1'
 
C statistics
      INCLUDE 'inc/dtsta1'
 
      DIMENSION mo(3,2) , idx(3) , pch(4) , pp(4) , pt(4) , p1(4) , 
     &          p2(4)
      DIMENSION idd(3,2)
      Irej = 0
 
      idx(1) = Idx1
      idx(2) = Idx2
      idx(3) = Idx3
      IF ( JMOhkk(1,Idx1).NE.Idx1-2 ) THEN
         Irej = 1
         RETURN
      END IF
      IF ( JMOhkk(2,Idx1).NE.Idx1-1 ) THEN
         Irej = 1
         RETURN
      END IF
      IF ( JMOhkk(1,Idx2).NE.Idx2-2 ) THEN
         Irej = 1
         RETURN
      END IF
      IF ( JMOhkk(2,Idx2).NE.Idx2-1 ) THEN
         Irej = 1
         RETURN
      END IF
      IF ( JMOhkk(1,Idx3).NE.Idx3-2 ) THEN
         Irej = 1
         RETURN
      END IF
      IF ( JMOhkk(2,Idx3).NE.Idx3-1 ) THEN
         Irej = 1
         RETURN
      END IF
      pptt1 = PHKk(1,Idx1-2)**2 + PHKk(2,Idx1-2)**2
      pptt2 = PHKk(1,Idx1-1)**2 + PHKk(2,Idx1-1)**2
      pptt3 = PHKk(1,Idx2-2)**2 + PHKk(2,Idx2-2)**2
      pptt4 = PHKk(1,Idx2-1)**2 + PHKk(2,Idx2-1)**2
      pptt5 = PHKk(1,Idx3-2)**2 + PHKk(2,Idx3-2)**2
      pptt6 = PHKk(1,Idx3-1)**2 + PHKk(2,Idx3-1)**2
      ppttmax = MAX(pptt1,pptt2,pptt3,pptt4,pptt5,pptt6)
C     IF(PPTTMAX.GT.2.D0)THEN
      IF ( ppttmax.GT.402.D0 ) THEN
         Irej = 1
         RETURN
C       ENDIF
C        IREJ=1
C        RETURN
      END IF
      DO i = 1 , 3
         DO j = 1 , 2
            mo(i,j) = JMOhkk(j,idx(i))
            idd(i,j) = IDHkk(mo(i,j))
         END DO
      END DO
C     WRITE(6,*)'Join333'
C    * ,IDD(1,1),IDD(1,2),IDD(2,1),IDD(2,2),IDD(3,1),IDD(3,2),'NN',
C    *IDX1,MO(1,1),MO(1,2),IDX2,MO(2,1),MO(2,2),IDX3,MO(3,1),MO(3,2)
 
C join chains
      DO k = 1 , 4
         pp(k) = PHKk(k,mo(1,1)) + PHKk(k,mo(2,1)) + PHKk(k,mo(3,1))
         pt(k) = PHKk(k,mo(1,2)) + PHKk(k,mo(2,2)) + PHKk(k,mo(3,2))
      END DO
      ist1 = ISThkk(mo(3,1))
      ist2 = ISThkk(mo(3,2))
      if2 = idd(1,2)
      idmm1 = idd(3,1)/1000
      idmm2 = (idd(3,1)-idmm1*1000)/100
      IF ( idd(1,1).EQ.-idmm1 ) THEN
         IF ( idd(2,1).GE.idmm2 ) if1 = 1000*idd(2,1) + 100*idmm2 + 3
         IF ( idmm2.GT.idd(2,1) ) if1 = 1000*idmm2 + 100*idd(2,1) + 3
      ELSE IF ( idd(1,1).EQ.-idmm2 ) THEN
         IF ( idd(2,1).GE.idmm1 ) if1 = 1000*idd(2,1) + 100*idmm1 + 3
         IF ( idmm1.GT.idd(2,1) ) if1 = 1000*idmm1 + 100*idd(2,1) + 3
      END IF
C     WRITE(6,*)'IF1,IF2 ',IF1,IF2
C     RETURN
C put partons again on mass shell
      xm1 = 0.0D0
      xm2 = 0.0D0
      IF ( IMShl.EQ.1 ) THEN
 
         xm1 = PYMASS(if1)
         xm2 = PYMASS(if2)
 
      END IF
C     WRITE(6,*)'effect of mashel'
      pppt = pp(1)**2 + pp(2)**2
      ptpt = pt(1)**2 + pt(2)**2
C     WRITE(6,*)'PP,PPPT ',PP,PPPT
C     WRITE(6,*)'PT,PTPT ',PT,PTPT
      CALL DT_MASHEL(pp,pt,xm1,xm2,p1,p2,irej1)
      IF ( irej1.NE.0 ) THEN
 
         Irej = 1
      ELSE
         p1pt = p1(1)**2 + p1(2)**2
         p2pt = p2(1)**2 + p2(2)**2
         DO i = 1 , 4
            pp(i) = p1(i)
            pt(i) = p2(i)
         END DO
C     WRITE(6,*)'P1,P1PT ',P1,P1PT
C     WRITE(6,*)'P2,P2PT ',P2,P2PT
         IF ( p1pt.GE.pppt+0.4D0 ) THEN
            Irej = 1
         ELSE IF ( p2pt.GE.ptpt+0.4D0 ) THEN
            Irej = 1
         ELSE
 
C store new partons in DTEVT1
            CALL DT_EVTPUT(ist1,if1,mo(1,1),mo(2,1),pp(1),pp(2),pp(3),
     &                     pp(4),0,0,0)
            CALL DT_EVTPUT(ist2,if2,mo(1,2),mo(2,2),pt(1),pt(2),pt(3),
     &                     pt(4),0,0,0)
            DO k = 1 , 4
               pch(k) = pp(k) + pt(k)
            END DO
 
 
            ICChai(2,9) = ICChai(2,9) + 1
C store new chain in DTEVT1
            kch = 191
            CALL DT_EVTPUT(kch,66666,-2,-1,pch(1),pch(2),pch(3),pch(4),
     &                     0,0,9)
            IDHkk(idx(1)) = 22222
            IDHkk(idx(2)) = 22222
            IDHkk(idx(3)) = 22222
C special treatment for space-time coordinates
            DO k = 1 , 4
               VHKk(k,NHKk) = (VHKk(k,idx(1))+VHKk(k,idx(2))+VHKk(k,idx(
     &                        3)))/3.0D0
               WHKk(k,NHKk) = (WHKk(k,idx(1))+WHKk(k,idx(2))+WHKk(k,idx(
     &                        3)))/3.0D0
            END DO
C     WRITE(6,*)'Join3 chains IDX1,IDX2,IDX3,NHKK ',IDX1,IDX2,IDX3,NHKK
            RETURN
         END IF
      END IF
      END SUBROUTINE
