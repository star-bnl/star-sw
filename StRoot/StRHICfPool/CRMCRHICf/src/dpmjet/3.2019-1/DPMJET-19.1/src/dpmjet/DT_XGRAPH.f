
      SUBROUTINE DT_XGRAPH(N,Iarg,X,Y1,Y2)
C***********************************************************************
C
C     calculate quasi graphic picture with 25 lines and 79 columns
C     ranges will be chosen automatically
C
C     input     N          dimension of input fields
C               IARG       number of curves (fields) to plot
C               X          field of X
C               Y1         field of Y1
C               Y2         field of Y2
C
C This subroutine is written by R. Engel.
C***********************************************************************
      IMPLICIT NONE
      DOUBLE PRECISION EPS , X , xmax , xmin , xzoom , Y1 , Y2 , ymax , 
     &                 ymin , ypos , yzoom
      INTEGER i , Iarg , IBREIT , id , ii , ilast , ispalt , itest , 
     &        IXRAST , IYRAST , IZEIL , k , kk , l , ld , llast , N
      SAVE 
 
      INCLUDE 'inc/dtflka'
 
C
      DIMENSION X(N) , Y1(N) , Y2(N)
      PARAMETER (EPS=1.D-30)
      PARAMETER (IYRAST=5,IXRAST=10,IBREIT=79,IZEIL=20)
      CHARACTER symb(5)
      CHARACTER col(0:149,0:49)
C
      DATA symb/'0' , 'e' , 'z' , '#' , 'x'/
C
      ispalt = IBREIT - 10
C
C
      xmax = X(1)
      xmin = X(1)
      DO i = 1 , N
         xmax = MAX(X(i),xmax)
         xmin = MIN(X(i),xmin)
      END DO
      xzoom = (xmax-xmin)/DBLE(ispalt)
C
      itest = 0
      DO k = 0 , IZEIL - 1
         itest = itest + 1
         IF ( itest.EQ.IYRAST ) THEN
            DO l = 1 , ispalt - 1
               col(l,k) = '-'
            END DO
            col(ispalt,k) = '+'
            itest = 0
            DO l = 0 , ispalt - 1 , IXRAST
               col(l,k) = '+'
            END DO
         ELSE
            DO l = 1 , ispalt - 1
               col(l,k) = ' '
            END DO
            DO l = 0 , ispalt - 1 , IXRAST
               col(l,k) = '|'
            END DO
            col(ispalt,k) = '|'
         END IF
      END DO
C
C
      ymax = Y1(1)
      ymin = Y1(1)
      DO i = 1 , N
         ymax = MAX(Y1(i),ymax)
         ymin = MIN(Y1(i),ymin)
      END DO
      IF ( Iarg.GT.1 ) THEN
         DO i = 1 , N
            ymax = MAX(Y2(i),ymax)
            ymin = MIN(Y2(i),ymin)
         END DO
      END IF
      ymax = (ymax-ymin)/40.0D0 + ymax
      ymin = ymin - (ymax-ymin)/40.0D0
      yzoom = (ymax-ymin)/DBLE(IZEIL)
      IF ( yzoom.LT.EPS ) THEN
 
         IF ( LPRi.GT.4 ) WRITE (LOUt,'(1X,A)')
     &         'XGRAPH:WARNING: MIN = MAX, OUTPUT SUPPRESSED'
         RETURN
      END IF
C
C
      ilast = -1
      llast = -1
      DO k = 1 , N
         l = NINT((X(k)-xmin)/xzoom)
         i = NINT((ymax-Y1(k))/yzoom)
         IF ( ilast.GE.0 ) THEN
            ld = l - llast
            id = i - ilast
            DO ii = 0 , ld , SIGN(1,ld)
               DO kk = 0 , id , SIGN(1,id)
                  col(ii+llast,kk+ilast) = symb(1)
               END DO
            END DO
         ELSE
            col(l,i) = symb(1)
         END IF
         ilast = i
         llast = l
      END DO
C
      IF ( Iarg.GT.1 ) THEN
C
C
         DO k = 1 , N
            l = NINT((X(k)-xmin)/xzoom)
            i = NINT((ymax-Y2(k))/yzoom)
            col(l,i) = symb(2)
         END DO
      END IF
C
C
 
      IF ( LPRi.GT.4 ) WRITE (LOUt,'(1X,79A)') ('-',i=1,IBREIT)
C
C
      xzoom = (xmax-xmin)/DBLE(7)
 
      IF ( LPRi.GT.4 ) WRITE (LOUt,99020) (xzoom*DBLE(i-1)+xmin,i=1,7)
C
      DO k = 0 , IZEIL - 1
         ypos = ymax - ((DBLE(k)+0.5D0)*yzoom)
 
         IF ( LPRi.GT.4 ) WRITE (LOUt,99010) ypos , 
     &        (col(i,k),i=0,ispalt)
99010    FORMAT (1X,1PE9.2,70A1)
      END DO
C
C
      xzoom = (xmax-xmin)/DBLE(7)
 
      IF ( LPRi.GT.4 ) WRITE (LOUt,99020) (xzoom*DBLE(i-1)+xmin,i=1,7)
 
      IF ( LPRi.GT.4 ) WRITE (LOUt,'(1X,79A)') ('-',i=1,IBREIT)
99020 FORMAT (6X,7(1PE10.3))
      END SUBROUTINE
