
      SUBROUTINE DT_GETBXS(Xsfrac,Blo,Bhi,Nidx)
 
C***********************************************************************
C Biasing in impact parameter space.                                   *
C     XSFRAC = 0 :  BLO    - minimum impact parameter  (input)         *
C                   BHI    - maximum impact parameter  (input)         *
C                   XSFRAC - fraction of cross section corresponding   *
C                            to impact parameter range (BLO,BHI)       *
C                                                      (output)        *
C     XSFRAC > 0 :  XSFRAC - fraction of cross section (input)         *
C                   BHI    - maximum impact parameter giving requested *
C                            fraction of cross section in impact       *
C                            parameter range (0,BMAX)  (output)        *
C This version dated 17.03.00  is written by S. Roesler                *
C***********************************************************************
 
      IMPLICIT NONE
      DOUBLE PRECISION Bhi , Blo , fac , frchi , frclo , Xsfrac
      INTEGER i , ihi , ilo , Nidx , ntarg
      SAVE 
 
      INCLUDE 'inc/dtflka'
 
C emulsion treatment
      INCLUDE 'inc/dtcomp'
C Glauber formalism: parameters
      INCLUDE 'inc/dtglam'
 
      ntarg = ABS(Nidx)
      IF ( Xsfrac.LE.0.0D0 ) THEN
         ilo = MIN(NSIteb-1,INT(Blo/BSTep(ntarg)))
         ihi = MIN(NSIteb-1,INT(Bhi/BSTep(ntarg)))
         IF ( ilo.GE.ihi ) THEN
            Xsfrac = 0.0D0
            RETURN
         END IF
         IF ( ilo.EQ.NSIteb-1 ) THEN
            frclo = BSIte(0,1,ntarg,NSIteb)
         ELSE
            frclo = BSIte(0,1,ntarg,ilo+1) + (Blo-ilo*BSTep(ntarg))
     &              /BSTep(ntarg)
     &              *(BSIte(0,1,ntarg,ilo+2)-BSIte(0,1,ntarg,ilo+1))
         END IF
         IF ( ihi.EQ.NSIteb-1 ) THEN
            frchi = BSIte(0,1,ntarg,NSIteb)
         ELSE
            frchi = BSIte(0,1,ntarg,ihi+1) + (Bhi-ihi*BSTep(ntarg))
     &              /BSTep(ntarg)
     &              *(BSIte(0,1,ntarg,ihi+2)-BSIte(0,1,ntarg,ihi+1))
         END IF
         Xsfrac = frchi - frclo
      ELSE
         Blo = 0.0D0
         Bhi = BMAx(ntarg)
         DO i = 1 , NSIteb - 1
            IF ( Xsfrac.LT.BSIte(0,1,ntarg,i+1) ) THEN
               fac = (Xsfrac-BSIte(0,1,ntarg,i))
     &               /(BSIte(0,1,ntarg,i+1)-BSIte(0,1,ntarg,i))
               Bhi = DBLE(i-1)*BSTep(ntarg) + BSTep(ntarg)*fac
               GOTO 99999
            END IF
         END DO
      END IF
 
99999 END SUBROUTINE
