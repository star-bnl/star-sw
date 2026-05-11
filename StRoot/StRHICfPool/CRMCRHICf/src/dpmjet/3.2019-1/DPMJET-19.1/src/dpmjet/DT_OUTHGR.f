
      SUBROUTINE DT_OUTHGR(I1,I2,I3,I4,I5,I6,Chead,Ihead,Nevts,Fac,
     &                     Ilogy,Inorm,Nmode)
 
C***********************************************************************
C                                                                      *
C     Plot histogram(s) to standard output unit                        *
C                                                                      *
C         I1..6         indices of histograms to be plotted            *
C         CHEAD,IHEAD   header string,integer                          *
C         NEVTS         number of events                               *
C         FAC           scaling factor                                 *
C         ILOGY   = 1   logarithmic y-axis                             *
C         INORM         normalization                                  *
C                 = 0   no further normalization (FAC is obsolete)     *
C                 = 1   per event and bin width                        *
C                 = 2   per entry and bin width                        *
C                 = 3   per bin entry                                  *
C                 = 4   per event and "bin width" x1^2...x2^2          *
C                 = 5   per event and "log. bin width" ln x1..ln x2    *
C                 = 6   per event                                      *
C         MODE    = 0   no output but normalization applied            *
C                 = 1   all valid histograms separately (small frame)  *
C                       all valid histograms separately (small frame)  *
C                 = -1  and tables as histograms                       *
C                 = 2   all valid histograms (one plot, wide frame)    *
C                       all valid histograms (one plot, wide frame)    *
C                 = -2  and tables as histograms                       *
C                                                                      *
C                                                                      *
C     Note: All histograms to be plotted with one call to this         *
C           subroutine and |MODE|=2 must have the same bin structure!  *
C           There is no test included ensuring this fact.              *
C                                                                      *
C This version dated 23.4.95 is written  by S. Roesler.                *
C***********************************************************************
 
      IMPLICIT NONE
      DOUBLE PRECISION ddx , dxlow , dy , EPS , Fac , OHALF , ONE , 
     &                 RLARGE , SMALL , TINY , TWO , xhi , xlow , 
     &                 xmean , xx , xx1 , yerr , yhi , ylow , ymean
      DOUBLE PRECISION yy , yy1 , ZERO
      INTEGER i , I1 , I2 , I3 , I4 , I5 , I6 , ibin2 , idx , idx1 , 
     &        Ihead , ii , Ilogy , Inorm , IZERO , j , k , mode , n , 
     &        NDIM2
      INTEGER Nevts , nhi , NHISTO , Nmode , nsize
      SAVE 
 
      INCLUDE 'inc/dtflka'
 
      CHARACTER*72 Chead
 
      PARAMETER (ZERO=0.0D0,IZERO=0,ONE=1.0D0,TWO=2.0D0,OHALF=0.5D0,
     &           EPS=1.0D-5,TINY=1.0D-8,SMALL=-1.0D8,RLARGE=1.0D8)
 
C histograms
 
 
      INCLUDE 'inc/dthis1'
 
      PARAMETER (NDIM2=2*NDIM)
      DIMENSION xx(NDIM2) , yy(NDIM2)
 
      PARAMETER (NHISTO=6)
      DIMENSION yy1(NDIM,NHISTO) , xx1(NDIM,NHISTO) , idx1(NHISTO) , 
     &          idx(NHISTO)
 
      CHARACTER*43 cnorm(0:8)
      DATA cnorm/'no further normalization                   ' , 
     &     'per event and bin width                    ' , 
     &     'per entry1 and bin width                   ' , 
     &     'per bin entry                              ' , 
     &     'per event and "bin width" x1^2...x2^2      ' , 
     &     'per event and "log. bin width" ln x1..ln x2' , 
     &     'per event                                  ' , 
     &     'per bin entry1                             ' , 
     &     'per entry2 and bin width                   '/
 
      idx1(1) = I1
      idx1(2) = I2
      idx1(3) = I3
      idx1(4) = I4
      idx1(5) = I5
      idx1(6) = I6
 
      mode = Nmode
 
C initialization if "wide frame" is requested
      IF ( ABS(mode).EQ.2 ) THEN
         DO i = 1 , NHISTO
            DO j = 1 , NDIM
               xx1(j,i) = ZERO
               yy1(j,i) = ZERO
            END DO
         END DO
      END IF
 
C plot header
 
      IF ( LPRi.GT.4 ) WRITE (LOUt,'(/1X,A,I3,/,1X,70A1)') Chead , 
     &                        Ihead , ('=',ii=1,70)
 
C check histogram indices
      nhi = 0
      DO i = 1 , NHISTO
         IF ( (idx1(i).GE.1) .AND. (idx1(i).LE.IHIsl) ) THEN
            IF ( ISWi(idx1(i)).NE.0 ) THEN
               IF ( DENtry(1,idx1(i)).LT.ONE ) THEN
 
                  IF ( LPRi.GT.4 ) WRITE (LOUt,99010) idx1(i) , 
     &                 UNDerf(idx1(i)) , OVErf(idx1(i))
99010             FORMAT (/,1X,'OUTHGR:   warning!  no entries in',
     &                    ' histogram ',I3,/,21X,'underflows:',F10.0,
     &                    '   overflows:  ',F10.0)
               ELSE
                  nhi = nhi + 1
                  idx(nhi) = idx1(i)
               END IF
            END IF
         END IF
      END DO
      IF ( nhi.EQ.0 ) THEN
 
         IF ( LPRi.GT.4 ) WRITE (LOUt,99020)
99020    FORMAT (/,1X,'OUTHGR:   warning!  histogram indices not valid')
         RETURN
      END IF
 
C check normalization request
      IF ( ((Fac.EQ.ZERO) .AND. (Inorm.NE.0)) .OR. 
     &     ((Nevts.LT.1) .AND. ((Inorm.EQ.1) .OR. (Inorm.EQ.4) .OR. 
     &     (Inorm.EQ.5) .OR. (Inorm.EQ.6))) .OR. (Inorm.LT.0) .OR. 
     &     (Inorm.GT.8) ) THEN
 
         IF ( LPRi.GT.4 ) WRITE (LOUt,99030) Nevts , Inorm , Fac
99030    FORMAT (/,1X,'OUTHGR:   warning!  normalization request not ',
     &           'valid',/,21X,'NEVTS = ',I7,4X,'INORM = ',I2,4X,
     &           'FAC = ',E11.4)
         RETURN
      END IF
 
 
      IF ( LPRi.GT.4 ) WRITE (LOUt,'(/,1X,A,I8)') 'number of events:' , 
     &                        Nevts
 
C apply normalization
      DO n = 1 , nhi
 
         i = idx(n)
 
         IF ( ISWi(i).EQ.1 ) THEN
 
            IF ( LPRi.GT.4 ) WRITE (LOUt,99070) i , HISt(1,i,1) , 
     &           HISt(1,i,IBIns(i)+1) , IBIns(i)
         ELSE IF ( ISWi(i).EQ.2 ) THEN
 
            IF ( LPRi.GT.4 ) WRITE (LOUt,99070) i , HISt(1,i,1) , 
     &           HISt(1,i,IBIns(i)+1) , IBIns(i)
 
            IF ( LPRi.GT.4 ) WRITE (LOUt,99080)
         ELSE IF ( ISWi(i).EQ.3 ) THEN
 
            IF ( LPRi.GT.4 ) WRITE (LOUt,99090) i , 10**HISt(1,i,1) , 
     &           10**HISt(1,i,IBIns(i)+1) , IBIns(i)
         ELSE IF ( ISWi(i).EQ.4 ) THEN
 
            IF ( LPRi.GT.4 ) WRITE (LOUt,99090) i , 10**HISt(1,i,1) , 
     &           10**HISt(1,i,IBIns(i)+1) , IBIns(i)
 
            IF ( LPRi.GT.4 ) WRITE (LOUt,99080)
         ELSE
 
            IF ( LPRi.GT.4 ) WRITE (LOUt,99040) ISWi(i)
99040       FORMAT (/,1X,'warning!  inconsistent bin structure flag ',
     &              I4)
         END IF
 
         IF ( LPRi.GT.4 ) WRITE (LOUt,99050) DENtry(1,i) , DENtry(2,i) , 
     &        UNDerf(i) , OVErf(i)
99050    FORMAT (13X,'entries:',2F9.0,' underfl.:',F8.0,' overfl.:',
     &           F8.0)
 
         IF ( LPRi.GT.4 ) WRITE (LOUt,99060) cnorm(Inorm)
99060    FORMAT (1X,'normalization: ',A,/)
 
         DO k = 1 , IBIns(i)
            CALL DT_GETBIN(i,k,Nevts,Inorm,xlow,xhi,xmean,ymean,yerr)
            ymean = Fac*ymean
            yerr = Fac*yerr
 
            IF ( LPRi.GT.4 ) WRITE (LOUt,99100) xlow , xmean , ymean , 
     &           yerr , HISt(2,i,k)
 
            IF ( LPRi.GT.4 ) WRITE (LOUt,99100) xhi , xmean , ymean , 
     &           yerr , HISt(2,i,k)
C    small frame
            ii = 2*k
            xx(ii-1) = HISt(1,i,k)
            xx(ii) = HISt(1,i,k+1)
            yy(ii-1) = ymean
            yy(ii) = ymean
C    wide frame
            xx1(k,n) = xmean
            IF ( (ISWi(i).EQ.3) .OR. (ISWi(i).EQ.4) ) xx1(k,n)
     &           = LOG10(xmean)
            yy1(k,n) = ymean
         END DO
 
C plot small frame
         IF ( ABS(mode).EQ.1 ) THEN
            ibin2 = 2*IBIns(i)
 
            IF ( LPRi.GT.4 ) WRITE (LOUt,'(/,1X,A)') 'Preview:'
            IF ( Ilogy.EQ.1 ) THEN
               CALL DT_XGLOGY(ibin2,1,xx,yy,yy)
            ELSE
               CALL DT_XGRAPH(ibin2,1,xx,yy,yy)
            END IF
         END IF
 
      END DO
 
C plot wide frame
      IF ( ABS(mode).EQ.2 ) THEN
 
         IF ( LPRi.GT.4 ) WRITE (LOUt,'(/,1X,A)') 'Preview:'
         nsize = NDIM*NHISTO
         dxlow = HISt(1,idx(1),1)
         ddx = ABS(HISt(1,idx(1),2)-HISt(1,idx(1),1))
         ylow = RLARGE
         yhi = SMALL
         DO i = 1 , NHISTO
            DO j = 1 , NDIM
               IF ( yy1(j,i).LT.ylow ) THEN
                  IF ( Ilogy.EQ.1 ) THEN
                     IF ( yy1(j,i).GT.ZERO ) ylow = yy1(j,i)
                  ELSE
                     ylow = yy1(j,i)
                  END IF
               END IF
               IF ( yy1(j,i).GT.yhi ) yhi = yy1(j,i)
            END DO
         END DO
         dy = (yhi-ylow)/DBLE(NDIM)
         IF ( dy.LE.ZERO ) THEN
 
            IF ( LPRi.GT.4 ) WRITE (LOUt,'(1X,A,6I4,A,2E12.4)')
     &            'OUTHGR:   warning! zero bin width for histograms ' , 
     &           idx , ': ' , ylow , yhi
            RETURN
         END IF
         IF ( Ilogy.EQ.1 ) THEN
            ylow = LOG10(ylow)
            dy = (LOG10(yhi)-ylow)/100.0D0
            DO i = 1 , NHISTO
               DO j = 1 , NDIM
                  IF ( yy1(j,i).LE.ZERO ) THEN
                     yy1(j,i) = ylow
                  ELSE
                     yy1(j,i) = LOG10(yy1(j,i))
                  END IF
               END DO
            END DO
         END IF
         CALL DT_SRPLOT(xx1,yy1,nsize,NHISTO,NDIM,dxlow,ddx,ylow,dy)
      END IF
99070 FORMAT (/,1X,'histo.',I4,', linear binning from',2X,E10.4,' to',
     &        2X,E10.4,',',2X,I3,' bins')
99080 FORMAT (1X,'user defined bin structure')
99090 FORMAT (/,1X,'histo.',I4,', logar. binning from',2X,E10.4,' to',
     &        2X,E10.4,',',2X,I3,' bins')
99100 FORMAT (1X,5E11.3)
 
      END SUBROUTINE
