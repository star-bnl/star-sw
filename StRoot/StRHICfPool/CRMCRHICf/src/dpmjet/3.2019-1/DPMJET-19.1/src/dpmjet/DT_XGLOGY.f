
      SUBROUTINE DT_XGLOGY(N,Iarg,X,Y1,Y2)
C***********************************************************************
C
C     calculate quasi graphic picture with 25 lines and 79 columns
C     logarithmic y axis
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
C
      IMPLICIT NONE
      DOUBLE PRECISION DEPS , EPS , X , xmax , xmin , xzoom , xzoom1 , 
     &                 Y1 , Y2 , yma , ymax , ymi , ymin , ypos , yzoom
      INTEGER i , Iarg , IBREIT , id , ii , ilast , ispalt , itest , 
     &        IXRAST , IYRAST , IZEIL , k , kk , l , ld , llast , N
      SAVE 
 
      INCLUDE 'inc/dtflka'
 
      DIMENSION X(N) , Y1(N) , Y2(N)
      PARAMETER (EPS=1.D-30)
      PARAMETER (IYRAST=5,IXRAST=10,IBREIT=79,IZEIL=20)
      CHARACTER symb(5)
      CHARACTER col(0:149,0:49)
      PARAMETER (DEPS=1.D-10)
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
      ymin = MAX(Y1(1),EPS)
      DO i = 1 , N
         ymax = MAX(Y1(i),ymax)
         IF ( Y1(i).GT.EPS ) THEN
            IF ( ymin.EQ.EPS ) THEN
               ymin = Y1(i)/10.D0
            ELSE
               ymin = MIN(Y1(i),ymin)
            END IF
         END IF
      END DO
      IF ( Iarg.GT.1 ) THEN
         DO i = 1 , N
            ymax = MAX(Y2(i),ymax)
            IF ( Y2(i).GT.EPS ) THEN
               IF ( ymin.EQ.EPS ) THEN
                  ymin = Y2(i)
               ELSE
                  ymin = MIN(Y2(i),ymin)
               END IF
            END IF
         END DO
      END IF
C
      DO i = 1 , N
         Y1(i) = MAX(Y1(i),ymin)
      END DO
      IF ( Iarg.GT.1 ) THEN
         DO i = 1 , N
            Y2(i) = MAX(Y2(i),ymin)
         END DO
      END IF
C
      IF ( ymax.LE.ymin ) THEN
 
         IF ( LPRi.GT.4 ) WRITE (LOUt,'(/1X,A,2E12.3,/)')
     &         'XGLOGY:ERROR:YMIN,YMAX ' , ymin , ymax
 
         IF ( LPRi.GT.4 ) WRITE (LOUt,'(1X,A)')
     &         'MIN = MAX, OUTPUT SUPPRESSED'
         RETURN
      END IF
C
      yma = (LOG10(ymax)-LOG10(ymin))/20.0D0 + LOG10(ymax)
      ymi = LOG10(ymin) - (LOG10(ymax)-LOG10(ymin))/20.0D0
      yzoom = (yma-ymi)/DBLE(IZEIL)
      IF ( yzoom.LT.EPS ) THEN
 
         IF ( LPRi.GT.4 ) WRITE (LOUt,'(1X,A)')
     &         'XGLOGY:WARNING: MIN = MAX, OUTPUT SUPPRESSED'
         RETURN
      END IF
C
C
      ilast = -1
      llast = -1
      DO k = 1 , N
         l = NINT((X(k)-xmin)/xzoom)
         i = NINT((yma-LOG10(Y1(k)))/yzoom)
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
            i = NINT((yma-LOG10(Y2(k)))/yzoom)
            col(l,i) = symb(2)
         END DO
      END IF
C
C
 
      IF ( LPRi.GT.4 ) WRITE (LOUt,'(2X,A)') '(LOGARITHMIC Y AXIS)'
 
      IF ( LPRi.GT.4 ) WRITE (LOUt,'(1X,79A)') ('-',i=1,IBREIT)
C
C
      xzoom1 = (xmax-xmin)/DBLE(7)
 
      IF ( LPRi.GT.4 ) WRITE (LOUt,99020) (xzoom1*DBLE(i-1)+xmin,i=1,7)
C
      DO k = 0 , IZEIL - 1
         ypos = 10.D0**(yma-((DBLE(k)+0.5D0)*yzoom))
 
         IF ( LPRi.GT.4 ) WRITE (LOUt,99010) ypos , 
     &        (col(i,k),i=0,ispalt)
99010    FORMAT (1X,1PE9.2,70A1)
      END DO
C
C
 
      IF ( LPRi.GT.4 ) WRITE (LOUt,99020) (xzoom1*DBLE(i-1)+xmin,i=1,7)
 
      IF ( LPRi.GT.4 ) WRITE (LOUt,'(1X,79A)') ('-',i=1,IBREIT)
99020 FORMAT (6X,7(1PE10.3))
C
      END SUBROUTINE
