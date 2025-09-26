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

      real :: vx, vy, vz     ! initial track position
      real :: rmin =   46.6; ! TPC Inner field cage Rmin
      real :: rmax =  200.0; ! Outer radius of the sensitive volume
      real :: zmin = -210.0; ! Length of TPC
      real :: zmax = +210.0; ! Length of TPC
      real            :: sumlifx
      real            :: tlifex
      real            :: rndm(1)
      real            :: dist, dist_p, dist_l

      integer, parameter        :: nmaxp = 10
      integer, dimension(nmaxp) :: particles=(/0,0,0,0,0,0,0,0,0,0 /)
      integer                   :: nparticles = 0
      integer                   :: i

      character*10 :: command
      integer      :: npar

!     common /aguslctrl / nparticles, particles

      sumlifx = 0
      
 1000 CALL GRNDM(RNDM,1)      
      sumlifx = -clight * tlifex * log( rndm(1) ) 

      do i=1,nparticles
      if ( ipart==particles(i) ) then

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
      enddo

      return
c//____________________________________________________________________________________________
      entry agusl_set
      call kugetr( rmin )
      call kugetr( rmax )
      call kugetr( zmax )
      zmin = -zmax
      write(*,*) 'Particles ', particles(1:nparticles)
      write(*,*) '... will be decayed w/in cylinder:'
      write(*,*) '... zmin=', zmin, ' zmax=', zmax
      write(*,*) '... rmin=', rmin, ' zmax=', rmax
      return
c//____________________________________________________________________________________________
      entry agusl_part
      if ( nparticles .le. 10 ) then
         nparticles=nparticles+1
         call kugeti( particles(nparticles) )
      endif      
      write(*,*) 'Particles ', particles(1:nparticles)
      write(*,*) '... will be decayed w/in cylinder:'
      write(*,*) '... zmin=', zmin, ' zmax=', zmax
      write(*,*) '... rmin=', rmin, ' zmax=', rmax   
      return
c//____________________________________________________________________________________________
      end subroutine agUsumlif

