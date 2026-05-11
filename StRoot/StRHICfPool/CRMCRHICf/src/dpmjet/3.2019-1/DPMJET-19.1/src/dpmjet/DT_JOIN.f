
      SUBROUTINE DT_JOIN(Idx1,Idx2,Irej)
 
C***********************************************************************
C This subroutine joins two q-aq chains to one qq-aqaq chain.          *
C     IDX1, IDX2       DTEVT1 indices of chains to be joined           *
C This version dated 11.01.95 is written by S. Roesler.                *
C***********************************************************************
 
      IMPLICIT NONE
      DOUBLE PRECISION amch , amchn , p1 , p1pt , p2 , p2pt , pch , pp , 
     &                 pppt , pptt1 , pptt2 , pptt3 , pptt4 , ppttmax , 
     &                 pt , ptpt , PYMASS , xm1 , xm2
      INTEGER i , id , IDT_IB2PDG , IDT_IPDG2B , idum , idx , Idx1 , 
     &        Idx2 , if1 , if2 , Irej , irej1 , ist1 , ist2 , j , k , 
     &        kch , mo
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
 
 
      DIMENSION mo(2,2) , id(2,2) , idx(2) , pch(4) , pp(4) , pt(4) , 
     &          p1(4) , p2(4)
 
      Irej = 0
 
      idx(1) = Idx1
      idx(2) = Idx2
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
      pptt1 = PHKk(1,Idx1-2)**2 + PHKk(2,Idx1-2)**2
      pptt2 = PHKk(1,Idx1-1)**2 + PHKk(2,Idx1-1)**2
      pptt3 = PHKk(1,Idx2-2)**2 + PHKk(2,Idx2-2)**2
      pptt4 = PHKk(1,Idx2-1)**2 + PHKk(2,Idx2-1)**2
      ppttmax = MAX(pptt1,pptt2,pptt3,pptt4)
C     IF(PPTTMAX.GT.2.D0)THEN
      IF ( ppttmax.GT.402.D0 ) THEN
         Irej = 1
         RETURN
C       ENDIF
C        IREJ=1
C        RETURN
      END IF
      DO i = 1 , 2
         DO j = 1 , 2
            mo(i,j) = JMOhkk(j,idx(i))
            id(i,j) = IDT_IPDG2B(IDHkk(mo(i,j)),1,2)
         END DO
      END DO
 
C check consistency
      IF ( (ABS(id(1,1)).GT.6) .OR. (ABS(id(1,2)).GT.6) .OR. 
     &     (ABS(id(2,1)).GT.6) .OR. (ABS(id(2,2)).GT.6) .OR. 
     &     ((id(1,1)*id(2,1)).LT.0) .OR. ((id(1,2)*id(2,2)).LT.0) ) THEN
 
         IF ( LPRi.GT.4 ) WRITE (LOUt,99010) idx(1) , mo(1,1) , mo(1,2)
     &        , idx(2) , mo(2,1) , mo(2,2)
99010    FORMAT (1X,'JOIN: incons. chain system! chain ',I4,':',2I5,
     &           ' chain ',I4,':',2I5)
      END IF
 
C join chains
      DO k = 1 , 4
         pp(k) = PHKk(k,mo(1,1)) + PHKk(k,mo(2,1))
         pt(k) = PHKk(k,mo(1,2)) + PHKk(k,mo(2,2))
      END DO
      IF ( id(1,1).EQ.4 ) id(1,1) = 3
      IF ( id(1,2).EQ.4 ) id(1,2) = 3
      IF ( id(2,1).EQ.4 ) id(2,1) = 3
      IF ( id(2,2).EQ.4 ) id(2,2) = 3
      IF ( id(1,1).EQ.-4 ) id(1,1) = -3
      IF ( id(1,2).EQ.-4 ) id(1,2) = -3
      IF ( id(2,1).EQ.-4 ) id(2,1) = -3
      IF ( id(2,2).EQ.-4 ) id(2,2) = -3
      if1 = IDT_IB2PDG(id(1,1),id(2,1),2)
      if2 = IDT_IB2PDG(id(1,2),id(2,2),2)
      ist1 = ISThkk(mo(1,1))
      ist2 = ISThkk(mo(1,2))
 
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
 
 
C check new chain for lower mass limit
            IF ( (IREsco.EQ.1) .OR. (IFRag(1).EQ.1) ) THEN
               amch = SQRT(ABS(pch(4)**2-pch(1)**2-pch(2)**2-pch(3)**2))
               CALL DT_CH2RES(id(1,1),id(2,1),id(1,2),id(2,2),idum,idum,
     &                        amch,amchn,3,irej1)
               IF ( irej1.NE.0 ) THEN
                  NHKk = NHKk - 2
                  Irej = 1
                  GOTO 99999
               END IF
            END IF
            ICChai(2,9) = ICChai(2,9) + 1
C store new chain in DTEVT1
            kch = 191
            CALL DT_EVTPUT(kch,66666,-2,-1,pch(1),pch(2),pch(3),pch(4),
     &                     0,0,9)
            IDHkk(idx(1)) = 22222
            IDHkk(idx(2)) = 22222
C special treatment for space-time coordinates
            DO k = 1 , 4
               VHKk(k,NHKk) = (VHKk(k,idx(1))+VHKk(k,idx(2)))/2.0D0
               WHKk(k,NHKk) = (WHKk(k,idx(1))+WHKk(k,idx(2)))/2.0D0
            END DO
C     WRITE(6,*)'Join chains  IDX1,IDX2 ',IDX1,IDX2
            RETURN
         END IF
      END IF
99999 END SUBROUTINE
