
      DOUBLE PRECISION FUNCTION PHO_DBFINT(Narg,Arg,Na,Ent,Table)
C***********************************************************************
C
C     routine based on CERN library E104
C
C     multi-dimensional interpolation routine, needed for PHOJET
C     internal cross section tables and several PDF sets (GRV98 and AGL)
C
C     changed to avoid recursive function calls (R.Engel, 09/98)
C
C***********************************************************************
      IMPLICIT NONE
      DOUBLE PRECISION dbfint , eta , h , oned , x , zerod
      INTEGER i , ishift , istep , k , knots , lmax , lmin , loca , 
     &        locb , locc , n , Narg , ndim
      SAVE 
 
      INTEGER Na(Narg) , index(32)
      DOUBLE PRECISION Arg(Narg) , Ent(Narg) , Table(*) , weight(32)
 
      DATA zerod/0.D0/
      DATA oned/1.D0/
 
      dbfint = zerod
      PHO_DBFINT = zerod
      IF ( Narg.LT.1 .OR. Narg.GT.5 ) RETURN
 
      lmax = 0
      istep = 1
      knots = 1
      index(1) = 1
      weight(1) = oned
      DO n = 1 , Narg
         x = Arg(n)
         ndim = Na(n)
         loca = lmax
         lmin = lmax + 1
         lmax = lmax + ndim
         IF ( ndim.GT.2 ) THEN
            locb = lmax + 1
         ELSE
            IF ( ndim.EQ.1 ) GOTO 300
            h = x - Ent(lmin)
            IF ( h.EQ.zerod ) GOTO 200
            ishift = istep
            IF ( x-Ent(lmin+1).EQ.zerod ) GOTO 100
            ishift = 0
            eta = h/(Ent(lmin+1)-Ent(lmin))
            GOTO 150
         END IF
 50      locc = (loca+locb)/2
         IF ( x.LT.Ent(locc) ) THEN
            locb = locc
         ELSE IF ( x.EQ.Ent(locc) ) THEN
            ishift = (locc-lmin)*istep
            GOTO 100
         ELSE
            loca = locc
         END IF
         IF ( locb-loca.GT.1 ) GOTO 50
         loca = MIN(MAX(loca,lmin),lmax-1)
         ishift = (loca-lmin)*istep
         eta = (x-Ent(loca))/(Ent(loca+1)-Ent(loca))
         GOTO 150
 100     DO k = 1 , knots
            index(k) = index(k) + ishift
         END DO
         GOTO 200
 150     DO k = 1 , knots
            index(k) = index(k) + ishift
            index(k+knots) = index(k) + istep
            weight(k+knots) = weight(k)*eta
            weight(k) = weight(k) - weight(k+knots)
         END DO
         knots = 2*knots
 200     istep = istep*ndim
 300  END DO
      DO k = 1 , knots
         i = index(k)
         dbfint = dbfint + weight(k)*Table(i)
      END DO
 
      PHO_DBFINT = dbfint
 
      END FUNCTION
