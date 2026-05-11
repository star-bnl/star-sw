
      SUBROUTINE DT_JOIHIS(Ih1,Ih2,Coper,Fac1,Fac2,Kevt,Norm,Ilogy,Mode)
 
C***********************************************************************
C                                                                      *
C     Operation on histograms.                                         *
C                                                                      *
C     input:  IH1,IH2      histogram indices to be joined              *
C             COPER        character defining the requested operation, *
C                          i.e. '+', '-', '*', '/'                     *
C             FAC1,FAC2    factors for joining, i.e.                   *
C                          FAC1*histo1 COPER FAC2*histo2               *
C                                                                      *
C This version dated 23.4.95 is written  by S. Roesler.                *
C***********************************************************************
 
      IMPLICIT NONE
      DOUBLE PRECISION ddx , dxlow , dy , Fac1 , Fac2 , OHALF , ONE , 
     &                 RLARGE , SMALL , TINY8 , xhi , xhi1 , xhi2 , 
     &                 xlow , xlow1 , xlow2 , xmean , xmean1 , xmean2 , 
     &                 xx
      DOUBLE PRECISION xx1 , yerr1 , yerr2 , yhi , ylow , ymean , 
     &                 ymean1 , ymean2 , yy , yy1 , ZERO
      INTEGER i , ibin2 , Ih1 , Ih2 , ii , Ilogy , k , Kevt , Mode , 
     &        NDIM2 , Norm , nsize
      SAVE 
 
      INCLUDE 'inc/dtflka'
 
      CHARACTER Coper*1
 
      PARAMETER (ZERO=0.0D0,ONE=1.0D0,OHALF=0.5D0,TINY8=1.0D-8,
     &           SMALL=-1.0D8,RLARGE=1.0D8)
 
C histograms
 
 
      INCLUDE 'inc/dthis1'
 
      PARAMETER (NDIM2=2*NDIM)
      DIMENSION xx(NDIM2) , yy(NDIM2) , yy1(NDIM) , xx1(NDIM)
 
      CHARACTER*43 cnorm(0:6)
      DATA cnorm/'no further normalization                   ' , 
     &     'per event and bin width                    ' , 
     &     'per entry and bin width                    ' , 
     &     'per bin entry                              ' , 
     &     'per event and "bin width" x1^2...x2^2      ' , 
     &     'per event and "log. bin width" ln x1..ln x2' , 
     &     'per event                                  '/
 
C check histogram indices
      IF ( (Ih1.LT.1) .OR. (Ih2.LT.1) .OR. (Ih1.GT.IHIsl) .OR. 
     &     (Ih2.GT.IHIsl) ) THEN
 
         IF ( LPRi.GT.4 ) WRITE (LOUt,99010) Ih1 , Ih2 , IHIsl
99010    FORMAT (1X,'JOIHIS:   warning!  inconsistent histogram ',
     &           'indices (',I3,',',I3,'),',/,21X,'valid range:  1,',I3)
         GOTO 99999
      END IF
 
C check bin structure of histograms to be joined
      IF ( IBIns(Ih1).NE.IBIns(Ih2) ) THEN
 
         IF ( LPRi.GT.4 ) WRITE (LOUt,99020) Ih1 , Ih2 , IBIns(Ih1) , 
     &        IBIns(Ih2)
99020    FORMAT (1X,'JOIHIS:   warning!  joining histograms ',I3,
     &           ' and ',I3,' failed',/,21X,
     &           'due to different numbers of bins (',I3,',',I3,')')
         GOTO 99999
      END IF
      DO k = 1 , IBIns(Ih1) + 1
         IF ( ABS(HISt(1,Ih1,k)-HISt(1,Ih2,k)).GT.TINY8 ) THEN
 
            IF ( LPRi.GT.4 ) WRITE (LOUt,99030) Ih1 , Ih2 , k , 
     &           HISt(1,Ih1,k) , HISt(1,Ih2,k)
99030       FORMAT (1X,'JOIHIS:   warning!  joining histograms ',I3,
     &              ' and ',I3,' failed at bin edge ',I3,/,21X,
     &              'X1,X2 = ',2E11.4)
            GOTO 99999
         END IF
      END DO
 
 
      IF ( LPRi.GT.4 ) WRITE (LOUt,99040) Ih1 , Ih2 , Coper , Fac1 , 
     &                        Fac2
99040 FORMAT (1X,'JOIHIS:   joining histograms ',I3,',',I3,' with ',
     &        'operation ',A,/,11X,'and factors ',2E11.4)
 
      IF ( LPRi.GT.4 ) WRITE (LOUt,99050) cnorm(Norm)
99050 FORMAT (1X,'normalization: ',A,/)
 
      DO k = 1 , IBIns(Ih1)
         CALL DT_GETBIN(Ih1,k,Kevt,Norm,xlow1,xhi1,xmean1,ymean1,yerr1)
         CALL DT_GETBIN(Ih2,k,Kevt,Norm,xlow2,xhi2,xmean2,ymean2,yerr2)
         xlow = xlow1
         xhi = xhi1
         xmean = OHALF*(xmean1+xmean2)
         IF ( Coper.EQ.'+' ) THEN
            ymean = Fac1*ymean1 + Fac2*ymean2
         ELSE IF ( Coper.EQ.'*' ) THEN
            ymean = Fac1*ymean1*Fac2*ymean2
         ELSE IF ( Coper.EQ.'/' ) THEN
            IF ( ymean2.EQ.ZERO ) THEN
               ymean = ZERO
            ELSE
               IF ( Fac2.EQ.ZERO ) Fac2 = ONE
               ymean = Fac1*ymean1/(Fac2*ymean2)
            END IF
         ELSE
            GOTO 100
         END IF
 
         IF ( LPRi.GT.4 ) WRITE (LOUt,99070) xlow , xmean , ymean , 
     &        HISt(2,Ih1,k) , HISt(2,Ih2,k)
 
         IF ( LPRi.GT.4 ) WRITE (LOUt,99070) xhi , xmean , ymean , 
     &        HISt(2,Ih1,k) , HISt(2,Ih2,k)
C    small frame
         ii = 2*k
         xx(ii-1) = HISt(1,Ih1,k)
         xx(ii) = HISt(1,Ih1,k+1)
         yy(ii-1) = ymean
         yy(ii) = ymean
C    wide frame
         xx1(k) = xmean
         IF ( (ISWi(Ih1).EQ.3) .OR. (ISWi(Ih1).EQ.4) ) xx1(k)
     &        = LOG10(xmean)
         yy1(k) = ymean
      END DO
 
C plot small frame
      IF ( ABS(Mode).EQ.1 ) THEN
         ibin2 = 2*IBIns(Ih1)
 
         IF ( LPRi.GT.4 ) WRITE (LOUt,'(/,1X,A)') 'Preview:'
         IF ( Ilogy.EQ.1 ) THEN
            CALL DT_XGLOGY(ibin2,1,xx,yy,yy)
         ELSE
            CALL DT_XGRAPH(ibin2,1,xx,yy,yy)
         END IF
      END IF
 
C plot wide frame
      IF ( ABS(Mode).EQ.2 ) THEN
 
         IF ( LPRi.GT.4 ) WRITE (LOUt,'(/,1X,A)') 'Preview:'
         nsize = NDIM
         dxlow = HISt(1,Ih1,1)
         ddx = ABS(HISt(1,Ih1,2)-HISt(1,Ih1,1))
         ylow = RLARGE
         yhi = SMALL
         DO i = 1 , NDIM
            IF ( yy1(i).LT.ylow ) THEN
               IF ( Ilogy.EQ.1 ) THEN
                  IF ( yy1(i).GT.ZERO ) ylow = yy1(i)
               ELSE
                  ylow = yy1(i)
               END IF
            END IF
            IF ( yy1(i).GT.yhi ) yhi = yy1(i)
         END DO
         dy = (yhi-ylow)/DBLE(NDIM)
         IF ( dy.LE.ZERO ) THEN
 
            IF ( LPRi.GT.4 ) WRITE (LOUt,'(1X,A,2I4,A,2E12.4)')
     &            'JOIHIS:   warning! zero bin width for histograms ' , 
     &           Ih1 , Ih2 , ': ' , ylow , yhi
            RETURN
         END IF
         IF ( Ilogy.EQ.1 ) THEN
            ylow = LOG10(ylow)
            dy = (LOG10(yhi)-ylow)/100.0D0
            DO i = 1 , NDIM
               IF ( yy1(i).LE.ZERO ) THEN
                  yy1(i) = ylow
               ELSE
                  yy1(i) = LOG10(yy1(i))
               END IF
            END DO
         END IF
         CALL DT_SRPLOT(xx1,yy1,nsize,1,NDIM,dxlow,ddx,ylow,dy)
      END IF
 
      RETURN
 
 
 100  IF ( LPRi.GT.4 ) WRITE (LOUt,99060) Coper
99060 FORMAT (1X,'JOIHIS:   unknown operation ',A)
99070 FORMAT (1X,5E11.3)
 
99999 END SUBROUTINE
