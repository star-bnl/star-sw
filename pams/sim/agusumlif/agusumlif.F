      subroutine agUsumlif( sumlifx, tlifex )


#include "geant321/gcbank.inc"
#include "geant321/gckine.inc"
#include "geant321/gcnum.inc"
#include "geant321/gconsp.inc"
#include "geant321/gcphys.inc"
#include "geant321/gcstak.inc"
#include "geant321/gctmed.inc"
#include "geant321/gctrak.inc"
#include "geant321/gcvolu.inc"

      real, parameter :: rmin = 50.0;
      real, parameter :: rmax = 100.0;
      real            :: sumlifx
      real            :: tlifex
      real            :: rndm(1)

      sumlifx = 0

 1000 CALL GRNDM(RNDM,1)

      sumlifx = -clight * tlifex * log( rndm(1) ) 
      if ( ipart.eq.11 .or. ipart.eq.12 ) then
         r = sumlifx* vect(7) / amass
         if ( r .lt. rmin .or. r .gt. rmax ) goto 1000
         write (*,*) ipart, clight, tlifex, sumlifx, r
      endif

      return
      end subroutine agUsumlif

