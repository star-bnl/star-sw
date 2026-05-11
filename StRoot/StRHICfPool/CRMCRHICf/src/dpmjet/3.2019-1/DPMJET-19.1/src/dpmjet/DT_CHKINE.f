
      SUBROUTINE DT_CHKINE(Pp1i,Ifp1,Pp2i,Ifp2,Pt1i,Ift1,Pt2i,Ift2,
     &                     Amch1,Amch1n,Amch2,Irej)
 
C***********************************************************************
C This subroutine replaces CORMOM.                                     *
C This version dated 05.01.95 is written by S. Roesler                 *
C***********************************************************************
 
      IMPLICIT NONE
      DOUBLE PRECISION Amch1 , Amch1n , Amch2 , amch22 , ech , p1 , p2 , 
     &                 pch , pp1 , Pp1i , pp2 , Pp2i , pt1 , Pt1i , 
     &                 pt2 , Pt2i , PYMASS , scale , TINY10 , xm1
      DOUBLE PRECISION xm2
      INTEGER i , Ifp1 , Ifp2 , Ift1 , Ift2 , Irej , irej1 , jmshl
      SAVE 
 
      INCLUDE 'inc/dtflka'
 
      PARAMETER (TINY10=1.0D-10)
 
C flags for input different options
      INCLUDE 'inc/dtflg1'
C rejection counter
      INCLUDE 'inc/dtrejc'
 
      DIMENSION pp1(4) , pp2(4) , pt1(4) , pt2(4) , p1(4) , p2(4) , 
     &          Pp1i(4) , Pp2i(4) , Pt1i(4) , Pt2i(4)
 
      Irej = 0
      jmshl = IMShl
 
      scale = Amch1n/MAX(Amch1,TINY10)
      DO i = 1 , 4
         pp1(i) = Pp1i(i)
         pp2(i) = Pp2i(i)
         pt1(i) = Pt1i(i)
         pt2(i) = Pt2i(i)
         pp2(i) = pp2(i) + (1.0D0-scale)*pp1(i)
         pt2(i) = pt2(i) + (1.0D0-scale)*pt1(i)
         pp1(i) = scale*pp1(i)
         pt1(i) = scale*pt1(i)
      END DO
      IF ( (pp1(4).GE.0.0D0) .AND. (pp2(4).GE.0.0D0) .AND. 
     &     (pt1(4).GE.0.0D0) .AND. (pt2(4).GE.0.0D0) ) THEN
 
         ech = pp2(4) + pt2(4)
         pch = SQRT((pp2(1)+pt2(1))**2+(pp2(2)+pt2(2))
     &         **2+(pp2(3)+pt2(3))**2)
         amch22 = (ech-pch)*(ech+pch)
         IF ( amch22.LT.0.0D0 ) THEN
 
            IF ( IOUlev(1).GT.0 .AND. LPRi.GT.4 ) WRITE (LOUt,'(1X,A)')
     &            'CHKINE: inconsistent treatment!'
            GOTO 200
         END IF
 
         Amch1 = Amch1n
         Amch2 = SQRT(amch22)
 
C put partons again on mass shell
 50      xm1 = 0.0D0
         xm2 = 0.0D0
         IF ( jmshl.EQ.1 ) THEN
 
            xm1 = PYMASS(Ifp1)
            xm2 = PYMASS(Ift1)
 
         END IF
         CALL DT_MASHEL(pp1,pt1,xm1,xm2,p1,p2,irej1)
         IF ( irej1.NE.0 ) THEN
            IF ( jmshl.EQ.0 ) GOTO 300
            jmshl = 0
            GOTO 50
         END IF
         jmshl = IMShl
         DO i = 1 , 4
            pp1(i) = p1(i)
            pt1(i) = p2(i)
         END DO
 100     xm1 = 0.0D0
         xm2 = 0.0D0
         IF ( jmshl.EQ.1 ) THEN
 
            xm1 = PYMASS(Ifp2)
            xm2 = PYMASS(Ift2)
 
         END IF
         CALL DT_MASHEL(pp2,pt2,xm1,xm2,p1,p2,irej1)
         IF ( irej1.NE.0 ) THEN
            IF ( jmshl.EQ.0 ) GOTO 300
            jmshl = 0
            GOTO 100
         END IF
         DO i = 1 , 4
            pp2(i) = p1(i)
            pt2(i) = p2(i)
         END DO
         DO i = 1 , 4
            Pp1i(i) = pp1(i)
            Pp2i(i) = pp2(i)
            Pt1i(i) = pt1(i)
            Pt2i(i) = pt2(i)
         END DO
         RETURN
      END IF
 
 200  IRChki(1) = IRChki(1) + 1
C*sr
C     GOTO 9999
      Irej = -1
      RETURN
C*
 300  IRChki(2) = IRChki(2) + 1
C*af
C9999 CONTINUE
 
      IF ( LPRi.GT.4 .AND. IOUlev(1).GT.0 ) WRITE (LOUt,*)
     &      'rejected 1 in CHKINE'
      Irej = 1
      END SUBROUTINE
