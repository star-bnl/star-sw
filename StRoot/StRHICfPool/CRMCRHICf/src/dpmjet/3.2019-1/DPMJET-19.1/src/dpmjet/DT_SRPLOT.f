
      SUBROUTINE DT_SRPLOT(X,Y,N,M,Mm,Xo,Dx,Yo,Dy)
 
      IMPLICIT NONE
      DOUBLE PRECISION ai , aix , aiy , Dx , Dy , X , Xo , xx , Y , Yo , 
     &                 yy
      INTEGER i , ii , iii , ix , iy , j , jj , jjj , M , Mm , mmn , 
     &        mn , N
      SAVE 
 
      INCLUDE 'inc/dtflka'
 
C
C     initial version
C     J. Ranft, (FORTRAN-Programmierung,J.R.,Teubner, Leipzig, 72)
C     This is a subroutine of fluka to plot Y across the page
C     as a function of X down the page. Up to 37 curves can be
C     plotted in the same picture with different plotting characters.
C     Output of first 10 overprinted characters addad by FB 88
C  - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
C     Input Variables:
C        X   = array containing the values of X
C        Y   = array containing the values of Y
C        N   = number of values in X and in Y
C              can exceed the fixed number of lines
C        M   = number of different curves X,Y are containing
C        MM  = number of points in each curve i.e. N=M*MM
C        XO  = smallest value of X to be plotted
C        DX  = increment of X between subsequent lines
C        YO  = smallest value of Y to be plotted
C        DY  = increment of Y between subsequent character spaces
C
C        other variables used inside:
C        XX  = numbers along the X-coordinate axis
C        YY  = numbers along the Y-coordinate axis
C        LL  = ten lines temporary storage for the plot
C        L   = character set used to plot different curves
C        LOV = memorizes overprinted symbols
C              the first 10 overprinted symbols are printed on
C              the end of the line to avoid ambiguities
C              (added by FB as considered quite helpful)
C
C********************************************************************
C
      DIMENSION xx(61) , yy(61) , ll(101,10)
      DIMENSION X(N) , Y(N) , l(40) , lov(40,10)
      CHARACTER l , ll , lov
      DATA l/'*' , '2' , '3' , '4' , '5' , '6' , '7' , '8' , '9' , 'Z' , 
     &     '+' , 'A' , 'O' , 'B' , 'C' , 'D' , 'E' , 'F' , 'G' , 'H' , 
     &     'I' , 'J' , 'K' , 'L' , 'M' , 'N' , 'O' , 'P' , 'Q' , 'R' , 
     &     'S' , 'T' , 'U' , 'V' , 'W' , 'X' , 'Y' , '1' , '-' , ' '/
C
C
      mn = 51
      DO i = 1 , mn
         ai = i - 1
         xx(i) = Xo + ai*Dx
      END DO
      DO i = 1 , 11
         ai = i - 1
         yy(i) = Yo + 10.0D0*ai*Dy
      END DO
 
      IF ( LPRi.GT.4 ) WRITE (LOUt,99030) (yy(i),i=1,11)
      mmn = mn - 1
C
C
      DO jj = 1 , mmn , 10
         jjj = jj - 1
         DO i = 1 , 101
            DO j = 1 , 10
               ll(i,j) = l(40)
            END DO
         END DO
         DO i = 1 , 101
            ll(i,1) = l(39)
         END DO
         DO i = 1 , 101 , 10
            DO j = 1 , 10
               ll(i,j) = l(38)
            END DO
         END DO
         DO i = 1 , 40
            DO j = 1 , 10
               lov(i,j) = l(40)
            END DO
         END DO
C
C
         DO i = 1 , M
            DO j = 1 , Mm
               ii = j + (i-1)*Mm
               aix = (X(ii)-(Xo-Dx/2.0D0))/Dx + 1.0D0
               aiy = (Y(ii)-(Yo-Dy/2.0D0))/Dy + 1.0D0
               aix = aix - DBLE(jjj)
C           changed Sept.88 by FB to avoid INTEGER OVERFLOW
               IF ( aix.GT.1.D0 .AND. aix.LT.11.D0 .AND. 
     &              aiy.GT.1.D0 .AND. aiy.LT.102.D0 ) THEN
                  ix = INT(aix)
                  iy = INT(aiy)
                  IF ( ix.GT.0 .AND. ix.LE.10 .AND. iy.GT.0 .AND. 
     &                 iy.LE.101 ) THEN
                     IF ( ll(iy,ix).NE.l(38) .AND. ll(iy,ix).NE.l(39) )
     &                    lov(i,ix) = ll(iy,ix)
                     ll(iy,ix) = l(i)
                  END IF
               END IF
            END DO
         END DO
C
C
         DO i = 1 , 10
            ii = i + jjj
            iii = ii + 1
 
            IF ( LPRi.GT.4 ) WRITE (LOUt,99010) xx(ii) , xx(iii) , 
     &           (ll(j,i),j=1,101) , (lov(j,i),j=1,10)
99010       FORMAT (1X,2(1PE10.2),101A1,' ',10A1)
         END DO
      END DO
C
C
 
      IF ( LPRi.GT.4 ) WRITE (LOUt,99020)
99020 FORMAT (20X,10('1---------'),'1')
 
      IF ( LPRi.GT.4 ) WRITE (LOUt,99030) (yy(i),i=1,11)
      RETURN
C
99030 FORMAT (11X,11(1PE10.2),'OVERPRINTED')
      END SUBROUTINE
