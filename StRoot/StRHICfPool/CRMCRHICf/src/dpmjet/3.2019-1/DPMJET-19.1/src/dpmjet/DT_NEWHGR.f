
      SUBROUTINE DT_NEWHGR(Xlim1,Xlim2,Xlim3,Xlimb,Ibin,Irefn)
 
C***********************************************************************
C                                                                      *
C     Histogram initialization.                                        *
C                                                                      *
C     input:  XLIM1/XLIM2  lower/upper edge of histogram-window        *
C             XLIM3        bin size                                    *
C             IBIN    > 0  number of bins in equidistant lin. binning  *
C                     = -1 reset histograms                            *
C                     < -1 |IBIN| number of bins in equidistant log.   *
C                          binning or log. binning in user def. struc. *
C             XLIMB(*)     user defined bin structure                  *
C                                                                      *
C     The bin structure is sensitive to                                *
C             XLIM1, XLIM3, IBIN     if     XLIM3 > 0   (lin.)         *
C             XLIM1, XLIM2, IBIN     if     XLIM3 = 0   (lin. & log.)  *
C             XLIMB, IBIN            if     XLIM3 < 0                  *
C                                                                      *
C                                                                      *
C     output: IREFN        histogram index                             *
C                          (= -1 for inconsistent histogr. request)    *
C                                                                      *
C This subroutine is based on a original version by R. Engel.          *
C This version dated 22.4.95 is written  by S. Roesler.                *
C***********************************************************************
 
      IMPLICIT NONE
      DOUBLE PRECISION dx , TINY , xhi , Xlim1 , Xlim2 , Xlim3 , Xlimb , 
     &                 xlow , ZERO
      INTEGER i , Ibin , ihis , Irefn , k
      SAVE 
 
      INCLUDE 'inc/dtflka'
 
      LOGICAL lstart
 
      PARAMETER (ZERO=0.0D0,TINY=1.0D-10)
 
      DIMENSION Xlimb(*)
 
C histograms
 
 
      INCLUDE 'inc/dthis1'
C auxiliary common for histograms
      INCLUDE 'inc/dthis2'
 
      DATA lstart/.TRUE./
 
C reset histogram counter
      IF ( lstart .OR. (Ibin.EQ.-1) ) THEN
         IHIsl = 0
         IF ( Ibin.EQ.-1 ) RETURN
         lstart = .FALSE.
      END IF
 
      ihis = IHIsl + 1
C check for maximum number of allowed histograms
      IF ( ihis.GT.NHIS ) THEN
 
         IF ( LPRi.GT.4 ) WRITE (LOUt,99010) ihis , NHIS , ihis
99010    FORMAT (1X,'NEWHGR:   warning!  number of histograms (',I4,
     &           ') exceeds array size (',I4,')',/,21X,'histogram',I3,
     &           ' skipped!')
 
         Irefn = -1
         GOTO 99999
      END IF
 
      Irefn = ihis
      IBIns(ihis) = ABS(Ibin)
C check requested number of bins
      IF ( IBIns(ihis).GE.NDIM ) THEN
 
         IF ( LPRi.GT.4 ) WRITE (LOUt,99020) Ibin , NDIM , NDIM
99020    FORMAT (1X,'NEWHGR:   warning!  number of bins (',I3,
     &           ') exceeds array size (',I3,')',/,21X,
     &           'and will be reset to ',I3)
         IBIns(ihis) = NDIM
      END IF
      IF ( IBIns(ihis).EQ.0 ) THEN
 
         IF ( LPRi.GT.4 ) WRITE (LOUt,99030) Ibin , ihis
99030    FORMAT (1X,'NEWHGR:   warning!  inconsistent number of',
     &           ' bins (',I3,')',/,21X,'histogram',I3,' skipped!')
         Irefn = -1
         GOTO 99999
      END IF
 
C initialize arrays
      DO i = 1 , NDIM
         DO k = 1 , 3
            HISt(k,ihis,i) = ZERO
            HISt(k+3,ihis,i) = ZERO
            TMPhis(k,ihis,i) = ZERO
         END DO
         HISt(7,ihis,i) = ZERO
      END DO
      DENtry(1,ihis) = ZERO
      DENtry(2,ihis) = ZERO
      OVErf(ihis) = ZERO
      UNDerf(ihis) = ZERO
      TMPufl(ihis) = ZERO
      TMPofl(ihis) = ZERO
 
C bin str. sensitive to lower edge, bin size, and numb. of bins
      IF ( Xlim3.GT.ZERO ) THEN
         DO k = 1 , IBIns(ihis) + 1
            HISt(1,ihis,k) = Xlim1 + DBLE(k-1)*Xlim3
         END DO
         ISWi(ihis) = 1
C bin str. sensitive to lower/upper edge and numb. of bins
      ELSE IF ( Xlim3.EQ.ZERO ) THEN
C   linear binning
         IF ( Ibin.GT.0 ) THEN
            xlow = Xlim1
            xhi = Xlim2
            IF ( Xlim2.LE.Xlim1 ) THEN
 
               IF ( LPRi.GT.4 ) WRITE (LOUt,99040) Xlim1 , Xlim2
99040          FORMAT (1X,'NEWHGR:   warning!  inconsistent x-range',/,
     &                 21X,'(XLIM1,XLIM2 = ',2E11.4,')')
               Irefn = -1
               GOTO 99999
            END IF
            ISWi(ihis) = 1
         ELSE IF ( Ibin.LT.-1 ) THEN
C   logarithmic binning
            IF ( (Xlim1.LE.ZERO) .OR. (Xlim2.LE.ZERO) ) THEN
 
               IF ( LPRi.GT.4 ) WRITE (LOUt,99050) Xlim1 , Xlim2
99050          FORMAT (1X,'NEWHGR:   warning!  inconsistent log. ',
     &                 'binning',/,21X,'(XLIM1,XLIM2 = ',2E11.4,')')
               Irefn = -1
               GOTO 99999
            END IF
            IF ( Xlim2.LE.Xlim1 ) THEN
 
               IF ( LPRi.GT.4 ) WRITE (LOUt,99060) Xlim1 , Xlim2
99060          FORMAT (1X,'NEWHGR:   warning!  inconsistent x-range',/,
     &                 21X,'(XLIM1,XLIM2 = ',2E11.4,')')
               Irefn = -1
               GOTO 99999
            END IF
            xlow = LOG10(Xlim1)
            xhi = LOG10(Xlim2)
            ISWi(ihis) = 3
         END IF
         dx = ABS(xhi-xlow)/DBLE(MAX(IBIns(ihis),1))
         DO k = 1 , IBIns(ihis) + 1
            HISt(1,ihis,k) = xlow + DBLE(k-1)*dx
         END DO
      ELSE
C user defined bin structure
         DO k = 1 , IBIns(ihis) + 1
            IF ( Ibin.GT.0 ) THEN
               HISt(1,ihis,k) = Xlimb(k)
               ISWi(ihis) = 2
            ELSE IF ( Ibin.LT.-1 ) THEN
               HISt(1,ihis,k) = LOG10(Xlimb(k))
               ISWi(ihis) = 4
            END IF
         END DO
      END IF
 
C histogram accepted
      IHIsl = ihis
 
99999 END SUBROUTINE
