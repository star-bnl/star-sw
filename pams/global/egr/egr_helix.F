      INTEGER*4 FUNCTION EGR_HELIX_FIT(ipt,trk,xcir,ycir,zcir,wcir,
     +     wlin,ncir,egrpar_h,egrpar,covar)
 
C>--------------------------------------------------------------------
C     Input arguments
C     ipt - track id
C     xcir - x coordinates of the hits
C     ycir - y coordinates of the hits
C     zcir - z coordinates of the hits
C     wcir - weights for the circle fit
C     wlin - weights for the linear regresion fit
C     ncir - number of points 	INCLUDEd in fit
C
C     Output arguments
C     trk - track parameters
C
C     Functional description
C     Circle fit in the x-y plane and the linear regression in s-z
C
C<--------------------------------------------------------------------
      IMPLICIT NONE
#include "PAM.inc"
#include "tpt_track.inc"
#include "egr_egrpar.inc"
#include "egr_track_pointers.inc"
C____________________________________________________________________
      RECORD /track_pointers/ trk(*)
      RECORD /table_head_st/    egrpar_h
      RECORD /egr_egrpar_st/    egrpar(*)
 
      INTEGER ier,ncir,ipt,egr_utcfit,ks,egr_lswlf
      REAL    sgn, rad, phi0, x0, y0, z0, tanl, chit, chil
      REAL    acent, bcent, charge,dpsi,scir(mxpnt),rr,zint
      REAL    dtanl,dzint
      REAL    covar(3,3)
      REAL    xcir(*),ycir(*),wcir(*),zcir(*),wlin(*)
      LOGICAL good
      CHARACTER*132 errmsg
      INTEGER imsg,imsg1
      DATA imsg,imsg1 /2*0/
C___________________________________________________________________
      egr_helix_fit =  STAFCV_OK

      ier = 0
      sgn = 0
      rad = 0
      phi0 = 0
      x0 = 0
      y0 = 0
      z0 = 0
      tanl = 0
      chit = 999999.0
      chil = 999999.0
      ier =egr_utcfit(xcir,ycir,wcir,ncir,sgn,rad,acent,bcent,chit)
c      iercov = egr_circov(xcir,ycir,wcir,ncir,acent,bcent,rad,covar)
      if(ier.ne.0) then
         write(errmsg,'(A45)') 'EGR_HELIX_FIT-W: Exit with error'
         call message(errmsg,1,imsg)
         egr_helix_fit =  STAFCV_BAD
      else
C     evaluate charge
         if(egrpar(1).debug(2).eq.1) then
            write(errmsg,'(A60)') 
     +           'EGR_HELIX_FIT-I: No error from egr_utcfit'
            call message(errmsg,1,imsg)
         endif
         charge = (acent-xcir(ncir/2))*(ycir(ncir/2)-ycir(ncir/2-1))
     >          - (bcent-ycir(ncir/2))*(xcir(ncir/2)-xcir(ncir/2-1))
         if(charge.gt.0) sgn = 1.0
         if(charge.lt.0) sgn = -1.0
         phi0 = atan2(ycir(1)-bcent,xcir(1)-acent)
         scir(1)=0.
         rr=abs(rad)
         ks=1
         good = .true.
         do while(ks.lt.ncir.and.good)
             ks = ks + 1
             dpsi=sqrt((ycir(ks)-ycir(ks-1))**2
     >             +   (xcir(ks)-xcir(ks-1))**2)/(2.*rr)
             if(dpsi.le.1.0) then
                scir(ks) = scir(ks-1) + 2.0*rr*asin(dpsi)
             else
                ncir = ks-1
                good = .false.
             endif
         end do
         chil = 999999.0
         if(ncir.gt.egrpar(1).minfit)then
            ier =egr_lswlf(scir,zcir,wlin,ncir,tanl,zint,chil,dtanl,
     +           dzint)
         else
            ier = 1
            egr_helix_fit = STAFCV_BAD
         endif
         if(ier.ne.0) then
             egr_helix_fit =  STAFCV_BAD
             write(errmsg,'(A70)')
     +         'EGR_HELIX_FIT-W: Out of linear fit with an error'   
             call message(errmsg,1,imsg)
         else
C            calculate parameters of the first fitted point
             if(egrpar(1).debug(2).eq.1) then
               write (errmsg,'(A50,I5,I5,F7.5,I5)')
     +              ' EGR_HELIX_FIT-I: initializing track:',
     +               ipt,ncir,chil,trk(ipt).nhit
               call message(errmsg,1,imsg1)

             endif
             trk(ipt).p(1) = 1
             trk(ipt).p(2) = sgn*rr
             trk(ipt).p(3) = acent
             trk(ipt).p(4) = bcent
             trk(ipt).p(5) = phi0
             trk(ipt).p(6) = zint
             trk(ipt).p(7) = tanl
             trk(ipt).p(8) = chit
             trk(ipt).p(9) = chil
         endif
      endif
 
      end
