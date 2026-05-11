
      SUBROUTINE DT_FILHGR(Xi,Yi,Ihis,Nevt)
 
C***********************************************************************
C                                                                      *
C     Scoring for histogram IHIS.                                      *
C                                                                      *
C This subroutine is based on a original version by R. Engel.          *
C This version dated 23.4.95 is written  by S. Roesler.                *
C***********************************************************************
 
      IMPLICIT NONE
      DOUBLE PRECISION dx , ONE , TINY , x , Xi , y , Yi , ZERO
      INTEGER i1 , idum , Ihis , kk , kmax , kmin , ncevt , Nevt
      SAVE 
 
      INCLUDE 'inc/dtflka'
 
      PARAMETER (ZERO=0.0D0,ONE=1.0D0,TINY=1.0D-10)
 
C histograms
 
 
      INCLUDE 'inc/dthis1'
C auxiliary common for histograms
      INCLUDE 'inc/dthis2'
 
      DATA ncevt/1/
 
      x = Xi
      y = Yi
 
C dump content of temorary arrays into histograms
      IF ( (Nevt.NE.ncevt) .OR. (Nevt.LT.0) ) THEN
         CALL DT_EVTHIS(idum)
         ncevt = Nevt
      END IF
 
C check histogram index
      IF ( Ihis.EQ.-1 ) RETURN
C        WRITE(LOUT,1000) IHIS,IHISL
C1000    FORMAT(1X,'FILHGR:   warning!  histogram index',I4,
C    &          ' out of range (1..',I3,')')
      IF ( (Ihis.LT.1) .OR. (Ihis.GT.IHIsl) ) RETURN
 
      IF ( (ISWi(Ihis).EQ.1) .OR. (ISWi(Ihis).EQ.3) ) THEN
C bin structure not explicitly given
         IF ( (ISWi(Ihis).EQ.3) .AND. (x.GT.ZERO) ) x = LOG10(x)
         dx = ABS(HISt(1,Ihis,2)-HISt(1,Ihis,1))
         IF ( x.LT.HISt(1,Ihis,1) ) THEN
            i1 = 0
         ELSE
            i1 = INT((x-HISt(1,Ihis,1))/MAX(dx,TINY)) + 1
         END IF
 
      ELSE IF ( (ISWi(Ihis).EQ.2) .OR. (ISWi(Ihis).EQ.4) ) THEN
C user defined bin structure
         IF ( (ISWi(Ihis).EQ.4) .AND. (x.GT.ZERO) ) x = LOG10(x)
         IF ( x.LT.HISt(1,Ihis,1) ) THEN
            i1 = 0
         ELSE IF ( x.GT.HISt(1,Ihis,IBIns(Ihis)+1) ) THEN
            i1 = IBIns(Ihis) + 1
         ELSE
C   binary sort algorithm
            kmin = 0
            kmax = IBIns(Ihis) + 1
            DO WHILE ( (kmax-kmin).NE.1 )
               kk = (kmax+kmin)/2
               IF ( x.LE.HISt(1,Ihis,kk) ) THEN
                  kmax = kk
               ELSE
                  kmin = kk
               END IF
            END DO
            i1 = kmin
         END IF
 
      ELSE
 
         IF ( LPRi.GT.4 ) WRITE (LOUt,99010)
99010    FORMAT (1X,'FILHGR:   warning!  histogram not initialized')
         RETURN
      END IF
 
C scoring
      IF ( i1.LE.0 ) THEN
         TMPufl(Ihis) = TMPufl(Ihis) + ONE
      ELSE IF ( i1.LE.IBIns(Ihis) ) THEN
         TMPhis(1,Ihis,i1) = TMPhis(1,Ihis,i1) + ONE
         IF ( (ISWi(Ihis).EQ.3) .OR. (ISWi(Ihis).EQ.4) ) THEN
            TMPhis(2,Ihis,i1) = TMPhis(2,Ihis,i1) + 10**x
         ELSE
            TMPhis(2,Ihis,i1) = TMPhis(2,Ihis,i1) + x
         END IF
         TMPhis(3,Ihis,i1) = TMPhis(3,Ihis,i1) + y
      ELSE
         TMPofl(Ihis) = TMPofl(Ihis) + ONE
      END IF
 
      END SUBROUTINE
