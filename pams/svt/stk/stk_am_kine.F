      INTEGER*4 FUNCTION STK_AM_KINE(
     1               vtx_h,               vtx ,
     2            groups_h,            groups ,
     3             track_h,             track ,
     4              kine_h,              kine ) 
      IMPLICIT NONE
C     SYNOPSIS:
C     calculate kine observables according to helix parameters
C     
C     DESCRIPTION:
C     Calculates the kine observables of all reconstructed SVT tracks. 
C     Currently, all tracks are assumed to originate from the main vertex.
c     So the py, pz and ph angle are calculated after extrapoaltion to the 
C     main vertex.
C     
C     AUTHOR:
C     C. Pruneau,  Wayne State University
C     
C     DATE:
C     18-OCT-1993
C     
C     ARGUMENTS:
C     vtx_h    header to table vtx          primary vertex location
C     vtx      rows of table   vtx
C     track_h  header to table track        reconstructed tracks
C     track    rows of table   track
C     kine_h   header to table kine
C     kine     rows of table   kine
C     ---------------------------------------------------------------------
      
C     argument declaration
C     --------------------
#include "stk_am_kine.inc"
      
C     Local Declarations:
C     -------------------
      integer  itrack, ikine
      logical  done

      integer   pid
      real      tanl,  psi,  invpt       ! momentum info
      real      x0, y0, z0, r0, phi0       ! 1st point position (reference
C                                            cylinder)
      real      xh,  yh,  rh, phi, ch
      real     konst, bfield
      real     x1, y1, z1
      real     ptot, pt, p(3), costh, sinth, cosl, sinl, ph, th
      real     infinity/1.e12/

c     error/message
      
      character msg*132
      integer   msg_id(20)
      logical   first
      data      first/.true./
      save      first
      
C     RETURN:
C     ------
C     stafcv_ok    : normal completion
C     stafcv_bad : abort event upon abnormal input
C     
C     FORMAT:
C     -------
 1001 format(1x,a)

C     EXECUTABLE CODE:
C     ----------------
      WRITE(*,*) 'STK_AM_KINE: Starting ###############################'

      stk_am_kine = stafcv_ok

c     set error id 
      
c     check if inputs are valid, if not return with abort event status
      
      if (vtx_h.nok.le.0) then
         stk_am_kine = stafcv_bad
         msg = 'stk_am_kine_npvi-e :'//
     &        ' no primary vertex information '
         call message(msg,1,msg_id(1))
         return
      else if (track_h.nok.le.0) then
         stk_am_kine = stafcv_bad
         msg = 'stk_am_kine_nt-e : no rows in track '
         call message(msg,1,msg_id(3))
         return
      end if
      
c     loop on all track candidates pointed to by track

      itrack = 0
      ikine  = 0
      done   = .false.

      do while (.not.done .and. itrack.lt.track_h.nok)
         itrack = itrack + 1

         if (ch .gt. 0.) then
            psi  = phi - 90.
         else
            psi  = phi + 90.
         end if
         
c     assume uniform field of 0.5 tesla = 5 kgauss
         
         invpt = track(itrack).invpt 
         if (abs(invpt) .gt. 0.000001 .and. 
     &        abs(invpt).le.infinity ) then
            bfield  = 5.        ! kgauss
            konst   = 2.99792458e-4 ! GeV c-1 kgauss-1 cm-1
            
            pid  = track(itrack).pid  

c     reference point coordinate

            phi0 = track(itrack).phi0 
            r0   = track(itrack).r0   
            z0   = track(itrack).z0   
            y0   = r0 * sind(phi0)
            x0   = r0 * cosd(phi0)

c     vertex  - where the extrapolation goes...

            x1= vtx(1).x(1)
            y1= vtx(1).x(2)
            z1= vtx(1).x(3)

c     polar angle

            tanl  = track(itrack).tanl 
            cosl  = 1./sqrt(1.+tanl**2)
            sinl  = tanl/sqrt(1.+tanl**2)
            costh = sinl
            sinth = sqrt(1.-costh**2)
            
c     track sign

            if (invpt .gt. 0.) then
               ch = 1.
            else
               ch = -1.
            end if

c     radius of curvature and momentum

            rh   = 1./abs(invpt*konst*bfield)
            pt   = 1./abs(invpt)

c     azimuthal angle

            psi  = track(itrack).psi  
            if (ch .gt. 0.) then
               phi0 = psi + 90.
            else
               phi0 = psi - 90.
            end if

c     x0 = first space point; x1 = vertex

            xh   = x0 - rh*cosd(phi0)
            yh   = y0 - rh*sind(phi0)
            phi  = atan2d(y1-yh,x1-xh)
c            if (yh.gt.0.) then
c               phi = -acosd(cosd(phi0) + (x1-x0)/rh)
c            else
c               phi =  acosd(cosd(phi0) + (x1-x0)/rh)
c            end if

            if (ch .gt. 0.) then
               psi  = phi - 90.
            else
               psi  = phi + 90.
            end if
            ph   = psi

            ptot = pt/cosl
            p(1) = ptot * sinth*cosd(ph)
            p(2) = ptot * sinth*sind(ph)
            p(3) = ptot * costh
            th   = acosd(costh)
cccc  pt   = sqrt(p(1)**2+p(2)**2)

            ikine = ikine + 1
            if (ikine.lt.kine_h.maxlen) then
               kine(ikine).id     = ikine
               kine(ikine).pid    = pid ! particle id (Geant Convention)
               kine(ikine).id_vtx = 1 ! assume primary vertex for now
               kine(ikine).ptot   = ptot ! momentum 
               kine(ikine).p(1)   = p(1) ! 3-momentum
               kine(ikine).p(2)   = p(2)
               kine(ikine).p(3)   = p(3)
               kine(ikine).pt     = pt ! transverse momentum
               kine(ikine).th     = th ! transverse momentum
               kine(ikine).ph     = ph ! transverse momentum
               kine(ikine).sec    = track(ikine).sec 
            else
               msg = 'stk_am_kine_tmt-e : '//
     &              'number of tracks exceeds max '//
     &              'lenght of table kine'
               call message(msg,1,msg_id(4))
               done = .true.
            end if ! ikine
         else

c     track with bad parameters - store it with "infinity"

            msg = 'stk_am_kine_invpt-e : '//
     &           'found track with |invpt| out of bound'
            call message(msg,1,msg_id(5))
            ikine = ikine + 1
            if (ikine.lt.kine_h.maxlen) then
               kine(ikine).id     = ikine
               kine(ikine).pid    = pid ! particle id (Geant Convention)
               kine(ikine).id_vtx = 1 ! assume primary vertex for now
               kine(ikine).ptot   = -infinity
               kine(ikine).p(1)   = -infinity
               kine(ikine).p(2)   = -infinity
               kine(ikine).p(3)   = -infinity
               kine(ikine).pt     = -infinity
               kine(ikine).th     = -infinity
               kine(ikine).ph     = -infinity
               kine(ikine).sec    = 0
            else
               msg = 'stk_am_kine_temlts-e : '//
     &              'number of tracks exceeds max length '//
     &              'of table kine '
               call message(msg,1,msg_id(6))
               done = .true.
            end if ! ikine
         end if    ! invpt
      end do

c     update nok 

      kine_h.nok       = ikine

      WRITE(*,*) 'STK_AM_KINE: Stopping ##############################'
      STK_AM_KINE=STAFCV_OK
      RETURN
      END
