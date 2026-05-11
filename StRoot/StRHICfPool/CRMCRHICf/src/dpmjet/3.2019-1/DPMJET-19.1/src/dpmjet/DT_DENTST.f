
      SUBROUTINE DT_DENTST
 
      IMPLICIT NONE
      DOUBLE PRECISION dr , DT_DENSIT , f , fmax , r , rmax , rmin
      INTEGER ia , ir , nbins
      SAVE 
 
      OPEN (40,FILE='dentst.out',STATUS='UNKNOWN')
      OPEN (41,FILE='denmax.out',STATUS='UNKNOWN')
 
      rmin = 0.0D0
      rmax = 8.0D0
      nbins = 500
      dr = (rmax-rmin)/DBLE(nbins)
      DO ia = 5 , 18
         fmax = 0.0D0
         DO ir = 1 , nbins + 1
            r = rmin + DBLE(ir-1)*dr
            f = DT_DENSIT(ia,r,r)
            IF ( f.GT.fmax ) fmax = f
            WRITE (40,'(1X,I3,2E15.5)') ia , r , f
         END DO
         WRITE (41,'(1X,I3,E15.5)') ia , fmax
      END DO
 
      CLOSE (40)
      CLOSE (41)
 
      END SUBROUTINE
