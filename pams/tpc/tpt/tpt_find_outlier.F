* $Id: tpt_find_outlier.F,v 1.7 2000/03/10 05:02:07 sakrejda Exp $
* $Log: tpt_find_outlier.F,v $
* Revision 1.7  2000/03/10 05:02:07  sakrejda
* Track length calculation fixed, was not going past 90 deg before....
*
* Revision 1.6  2000/01/21 07:55:46  sakrejda
* Calculation of track length along the helix improved. Makes chisq and residuals symmetric.
*
* Revision 1.5  1999/09/14 00:55:46  liq
* use tpt_track_pointers.inc instead of tpt_pointers.inc in order to used by C++ codes
*
* Revision 1.4  1998/05/26 04:15:12  sakrejda
* message type set to E to limit printing
*
* Revision 1.3  1998/04/06 19:23:45  sakrejda
* write replaced by message calls
*
* Revision 1.2  1998/01/27 00:35:44  fisyak
* Split sources
*
      INTEGER FUNCTION TPT_FIND_OUTLIER(ipt,trk
     >       ,xcir,ycir,zcir,wcir,wlin,ncir,tpar_h,tpar,ibad,ipass)
C>--------------------------------------------------------------------
C
C TPT_FIND_OUTLIER - identifies position of the outlier on a track
C
C DESCRIPTION:
C For every point used in the fit weighted residua in both projections
C are calculated and the one with the worst chisq contribution is 
C identified.
C
C INPUT PARAMETERS:
C  ipt  - track id
C  trk  - auxiliary tracking table
C  xcir - x coordinates of the hits
C  ycir - y coordinates of the hits
C  zcir - z coordinates of the hits
C  wcir - weights for the circle fit
C  wlin - weights for the linear regresion fit
C  ncir - number of points included in fit
C  tpar_h    - header for the tptpar (tracking parameters) table
C  tpar     - tracking parameters table
C
C OUTPUT PARAMETERS:
C  ibad - id of a point with the worst chisq contribution
C
C AUTHOR:
C Iwona Sakrejda
C<---------------------------------------------------------------------

      IMPLICIT NONE
C_________________________________________________________________

#include "PAM.inc"
#include "tpt_pars.inc"
#include "tpt_track_pointers.inc"
#include "math_constants.inc"

C__________________________________________________________________
      RECORD /tpt_track_pointers_st/   trk(*)
      RECORD /table_head_st/    tpar_h
      RECORD /tpt_pars_st/      tpar(*)

C_________________________________________________________________
      INTEGER ibad, ipt, ncir, i, ibig,ipass
      REAL    xcir(*),ycir(*),zcir(*),wcir(*),wlin(*)
      REAL    zn1,resbig,restot,phi0,dz,ri,rr
      REAL    stot,dsxy,stot_old,dsz,acent,bcent
      REAL    refx,refy,refmod,curx,cury,curmod
      integer idI1, idI2, idI3       ! Message ID
      character*132 m132  ! And some message-generating space.
      character*132 m132by100(100)
      data idI1, idI2, idI3 /0, 0, 0/
      save idI1, idI2, idI3
      logical chiz
cc      real chisqz, chisqr
C_________________________________________________________________
c
      ibig = 0
      resbig  = 0.0 
      stot = 0.0
cc      chisqz=0
cc      chisqr=0
      phi0 = trk(ipt).p(5) ! angle in the helix coor. system
      rr = abs(trk(ipt).p(2))
      acent = trk(ipt).p(3)
      bcent = trk(ipt).p(4)
      zn1 = trk(ipt).p(6)
      refx=xcir(1)-acent
      refy=ycir(1)-bcent
      refmod=sqrt(refx**2+refy**2)
      dz  = 0.0
      chiz=trk(ipt).p(8)/(ncir-3).lt.trk(ipt).p(9)/(ncir-2)
      stot_old = -1.0
      do i=1,ncir
        if(chiz)then
          if(abs(trk(ipt).p(7)).lt.0.0000001) then
             write(m132,*)'TPTOUT1-E1 Midrapidity track'
             call Message( m132, 1, idI1 )
             tpt_find_outlier = STAFCV_BAD
             return
          endif
          dsz = (zcir(i) - zn1)/trk(ipt).p(7)
          if(dsz.ge.C_PI*rr) then
*            it's a very special case
             write (m132,*)'TPTOUT2-E1 special case',dsz,rr,trk(ipt).p(7)
             call Message( m132, 1, idI2 )
             tpt_find_outlier = STAFCV_BAD
             return
          else
*            write(6,*) 'got HERE!!!!!'
             curx=xcir(i)-acent
             cury=ycir(i)-bcent
             curmod=sqrt(curx**2+cury**2)
             dsxy = abs(refx*cury-refy*curx)/(refmod*curmod)
             stot = asin(dsxy)*rr
             if(stot.lt.stot_old) then
                 stot = rr*(C_PI-dsxy)
             endif
             dz = trk(ipt).p(6) + stot*trk(ipt).p(7)-zcir(i)
             restot=dz**2*wlin(i)
CC             chisqz=chisqz+restot
             zn1 = zcir(i)
          endif
        else
          ri = abs(sqrt((xcir(i)-acent)**2+(ycir(i)-bcent)**2)- rr)
          restot=ri**2*wcir(i)
CC          chisqr=chisqr+restot
        endif
        if(restot.ge.resbig) then
          ibig=i
          resbig = restot
        endif
      end do
CCC      write(6,*)'chisq diff',trk(ipt).p(9),chisqz,trk(ipt).p(8),chisqr
      ibad = ibig
      if(ibad.eq.0) then
          write(m132by100(1),*)'TPTOUT3-E1',ibad, ipt, ncir, ri, dz
          do i=1,ncir
          write(m132by100(i+1),*)wlin(i),wcir(i)
          end do
          call Message( m132by100, ncir+1, idI3 )
          tpt_find_outlier = STAFCV_BAD
      else
          tpt_find_outlier = STAFCV_OK
      endif
      return
      end
