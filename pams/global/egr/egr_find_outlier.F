C>--------------------------------------------------------------------
C
C EGR_FIND_OUTLIER - identifies position of the outlier on a track
C
C DESCRIPTION:
C For every point used in the fit weighted residua in both projections
C are calculated and the one with the worst chisq is identified.
C
C INPUT PARAMETERS:
C  ipt  - track id
C  trk  - auxiliary tracking table
C  xcir - x coordinates of the hits
C  ycir - y coordinates of the hits
C  zcir - z coordinates of the hits
C  wcir - weights for the circle fit
C  wlin - weights for the linear regresion fit
C  ncir - number of points 	INCLUDEd in fit
C
C OUTPUT PARAMETERS:
C  ibad - id of a point with the worst chisq contribution
C
C AUTHOR:
C Iwona Sakrejda
C<---------------------------------------------------------------------
      INTEGER*4 FUNCTION EGR_FIND_OUTLIER(ipt,trk,xcir,ycir,zcir
     >                  ,wcir,wlin,ncir,ibad,egrpar_h,egrpar)
 
      IMPLICIT NONE
#include "PAM.inc"
#include "egr_egrpar.inc"
#include "egr_track_pointers.inc"
C__________________________________________________________________
      RECORD /track_pointers/ trk(*)
      RECORD /table_head_st/    egrpar_h
      RECORD /egr_egrpar_st/    egrpar(*)
C_________________________________________________________________
      INTEGER ibad, ipt, ncir, i, ibig
      REAL    xcir(*),ycir(*),zcir(*),wcir(*),wlin(*)
      REAL    zn1,resbig,restot,phi0,dz,ri,rr
      REAL    stot,dsxy,dsz,acent,bcent,xn1,yn1
      CHARACTER*132 errmsg
      INTEGER imsg,imsg1
      DATA imsg,imsg1 /2*0/
C_________________________________________________________________

      if(egrpar(1).debug(6).eq.1) then
         write (errmsg,'(A60),I5')
     +        ' EGR_FIND_OUTLIER-I: track id ',ipt
         call message(errmsg,1,imsg)
      endif
      ibig = 0
      resbig  = 0.0
      stot = 0.0
      phi0 = trk(ipt).p(5)      ! angle in the helix coor. system
      rr = abs(trk(ipt).p(2))
      acent = trk(ipt).p(3)
      bcent = trk(ipt).p(4)
      zn1 = trk(ipt).p(6)
      yn1 = ycir(1)
      xn1 = xcir(1)
      do i=1,ncir
         ri = abs(sqrt((xcir(i)-acent)**2+(ycir(i)-bcent)**2)- rr)
         if(abs(trk(ipt).p(7)).lt.0.0000001) then
            egr_find_outlier = STAFCV_BAD
            return
         endif
         dsz = (zcir(i) - zn1)/trk(ipt).p(7)
         if(dsz.ge.2.0*3.141592653589*rr) then
            egr_find_outlier = STAFCV_BAD
*     it's a very special case
         else
            dsxy = 0.5*sqrt((ycir(i)-yn1)**2+(xcir(i)-xn1)**2)/rr
            if( abs(dsxy) .lt. 1.0) then
               stot = stot + asin(dsxy)*rr*2.0
               dz = trk(ipt).p(6) + stot*trk(ipt).p(7)-zcir(i)
               restot = dz**2*wlin(i)+ri*wcir(i)
               if(restot.gt.resbig) then
                  ibig=i
                  resbig = restot
               endif
               zn1 = zcir(i)
               yn1 = ycir(i)
               xn1 = xcir(i)
            else
               egr_find_outlier = STAFCV_BAD
               return
            endif
         endif
      end do
      ibad = ibig
      if( ibad .eq. 0) then
         egr_find_outlier = STAFCV_BAD
      else
         egr_find_outlier = STAFCV_OK
      endif
      return
      end
      
