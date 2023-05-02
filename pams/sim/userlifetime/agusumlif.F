      subroutine agUsumlif( sumlifx, tlifex, vx, vy, vz )

#include "geant321/gcbank.inc"
#include "geant321/gckine.inc"
#include "geant321/gcnum.inc"
#include "geant321/gconsp.inc"
#include "geant321/gcphys.inc"
#include "geant321/gcstak.inc"
#include "geant321/gctmed.inc"
#include "geant321/gctrak.inc"
#include "geant321/gcvolu.inc"

      real            :: vx, vy, vz     ! initial track position
      real, parameter :: rmin =   46.6; ! TPC Inner field cage Rmin
      real, parameter :: rmax =  200.0; ! Outer radius of the sensitive volume
      real, parameter :: zmin = -210.0; ! Length of TPC
      real, parameter :: zmax = +210.0; ! Length of TPC
      real            :: sumlifx
      real            :: tlifex
      real            :: rndm(1)
      real            :: dist, dist_p, dist_l

      sumlifx = 0

 1000 CALL GRNDM(RNDM,1)

      sumlifx = -clight * tlifex * log( rndm(1) ) 
      if ( ipart.eq.11 .or. ipart.eq.12 ) then

         ! Predicted path length to the decay point
         dist = sumlifx * vect(7) / amass

         dist_p = sumlifx * vect(7) * sqrt( vect(4)*vect(4)+vect(5)*vect(5) ) / amass
         dist_l = sumlifx * vect(7) * vect(6) / amass

         ! Resample if predicted decay point is outside of the bounds of the TPC.
         ! n.b. Not good for low energy secondaries (loopers)
         ! n.b. We ignore the initial radial position of the track

         if ( dist_p .lt. rmin .or. dist_p .gt. rmax ) goto 1000

         ! Correct for vertex z position
         if ( dist_l .lt. zmin - vz .or. dist_l .gt. zmax - vz ) goto 1000

         !write (*,*) ipart, clight, tlifex, sumlifx, r
         write(*,*) dist_p, dist_l, vert(1:3)

      endif

      return
      end subroutine agUsumlif

