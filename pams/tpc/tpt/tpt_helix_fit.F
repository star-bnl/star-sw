* $Id: tpt_helix_fit.F,v 1.3 1998/05/26 04:15:13 sakrejda Exp $
* $Log: tpt_helix_fit.F,v $
* Revision 1.3  1998/05/26 04:15:13  sakrejda
* message type set to E to limit printing
*
* Revision 1.2  1998/01/27 00:35:47  fisyak
* Split sources
*
      INTEGER FUNCTION TPT_HELIX_FIT(ipt,trk,xcir,ycir,zcir,
     >                               wcir,wlin,ncir,tpar_h,tpar,ipass)
C>----------------------------------------------------------------------
C
C TPT_HELIX_FIT
C<----------------------------------------------------------------------

      IMPLICIT NONE
C     
C     Input arguments
C     ipt - track id
C     xcir - x coordinates of the hits
C     ycir - y coordinates of the hits
C     zcir - z coordinates of the hits
C     wcir - weights for the circle fit
C     wlin - weights for the linear regresion fit
C     ncir - number of points included in fit
C
C     Output arguments
C     trk - track parameters
C
C     Functional description
C     Circle fit in the x-y plane and the linear regression in s-z
C
C___________________________________________________________________
#include "PAM.inc"

#include "tpt_pars.inc"
#include "tpt_pointers.inc"
C____________________________________________________________________
      RECORD /track_pointers/ trk(*)
      RECORD /table_head_st/    tpar_h
      RECORD /tpt_pars_st/    tpar(*)
      
      INTEGER ier,ncir,ipt,utcfit,ks,lswlf,circov
      REAL    sgn, rad, phi0, x0, y0, z0, tanl, chit, chil
      REAL    acent, bcent, charge,dpsi,scir(mxpnt),rr,zint
      REAL    dtanl,dzint,h(3,3)
      REAL    xcir(*),ycir(*),wcir(*),zcir(*),wlin(*)
      LOGICAL good
      CHARACTER*80 eromes
      INTEGER ier1,ipass
C___________________________________________________________________
      DATA ier1 /0/
C___________________________________________________________________
      tpt_helix_fit =  STAFCV_OK
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
C
      ier =utcfit(xcir,ycir,wcir,ncir,sgn,rad,acent,bcent,chit)
      if(ier.ne.0) then
          if(tpar(ipass).debug(2).eq.1) then
             write(6,*) 'out with error'
          endif
          tpt_helix_fit =  STAFCV_BAD
      else
C        eveluate charge
         if(tpar(ipass).debug(2).eq.1) then
            write(6,*) 'out from utcfit - no error'
         endif
         charge = (acent-xcir(ncir/2))*(ycir(ncir/2)-ycir(ncir/2-1))
     >          - (bcent-ycir(ncir/2))*(xcir(ncir/2)-xcir(ncir/2-1))
         if(charge.gt.0) sgn = 1.0
         if(charge.lt.0) sgn = -1.0
C        evaluate errors
         ier = circov(xcir,ycir,wcir,ncir,acent,bcent,rad,h)
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
         if(ncir.ge.3)then
            ier =lswlf(scir,zcir,wlin,ncir,tanl,zint,chil,dtanl,dzint) 
         else
            ier = 1
            tpt_helix_fit = STAFCV_BAD
         endif
         if(ier.ne.0) then
             tpt_helix_fit =  STAFCV_BAD 
             if(tpar(ipass).debug(2).eq.1) then
             write(eromes,'(a50,i5)') 
     >    'TPT_HELIX_FIT-E1 linear fit error, track',ipt
             call message(eromes, 1, ier1)
             endif
         else
C            calculate parameters of the first fitted point
             if(tpar(ipass).debug(2).eq.1) then
               write(6,*) 'initialising track', ipt,ncir
               write(6,*) chil,trk(ipt).nhit
             endif
             trk(ipt).flag=1
             trk(ipt).p(1) = 1
             trk(ipt).p(2) = sgn*rr
             trk(ipt).p(3) = acent
             trk(ipt).p(4) = bcent
             trk(ipt).p(5) = phi0
             trk(ipt).p(6) = zint
             trk(ipt).p(7) = tanl
             trk(ipt).p(8) = chit
             trk(ipt).p(9) = chil
             trk(ipt).dpar(1) = h(1,1)
             trk(ipt).dpar(2) = h(1,2)
             trk(ipt).dpar(3) = h(1,3)
             trk(ipt).dpar(4) = h(2,2)
             trk(ipt).dpar(5) = h(2,3)
             trk(ipt).dpar(6) = h(3,3)
             trk(ipt).dpar(7) = 0.0
             trk(ipt).dpar(8) = dtanl
             trk(ipt).dpar(9) = dzint
         endif
      endif

      end
