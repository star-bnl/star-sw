      INTEGER FUNCTION TPT_MAKE_SEGMENT(ntrk,hit_h,hit,tpar_h,tpar,
     >                                        pad_pl_h,pad_pl,trk,
     >                        next_hit,slice_loc,slice_pnt,ipass)
C
C Modified: 13-NOV-1997 by R.Bossingham
C           Eliminate unused variables.
C
      IMPLICIT NONE

#include "PAM.inc"

#include "tpt_pars.inc"
#include "tcl_tphit.inc"
#include "tpt_pointers.inc"
#include "tpg_pad_plane.inc"

#include "tpt_mxdim.inc"
#include "tpt_mxhit.inc"

C______________________________________________________________________

      RECORD /table_head_st/    tpar_h
      RECORD /tpt_pars_st/      tpar(*)
      RECORD /table_head_st/    hit_h
      RECORD /tcl_tphit_st/     hit(*)
      RECORD /table_head_st/    pad_pl_h
      RECORD /tpg_pad_plane_st/     pad_pl

      RECORD /track_pointers/   trk(*)
C_____________________________________________________________________
C
C     Local variables
      INTEGER ntrk,ir1,ir2,npnt,ip2,ip3,ip4,isect,nfound,iret
      INTEGER irow_last,irow_start,irow_step,ngap,isec1
      INTEGER tgc_row_par, tpt_ptcrs
      INTEGER next_hit(tpt_mxhit),slice_loc(mxsector,mxrow,mxslic)
      INTEGER slice_pnt(2,mxsector*mxrow*mxslic)
      INTEGER nf,drow,i,nh1,ilc
      INTEGER tpt_fit_track
      INTEGER is_pnt,islic,ipass,tte_skip
      INTEGER tpt_find_slic, irets
      integer tgc_global_to_local
      REAL    ax,ay,az,ar,bx,by,bz,br,dydx,oxy,dzdr,as,bs,xp1,yp1
      REAL    zp1,rp1,drp,dis0,dy,dx,dz,dfir,tolr2,disr2,r2
      real    xlocal(3), xglobal(3), dummy

C     Find what was the innermost row number and continue the search 
C     towards the inside of the chamber
      irow_start = mod(hit(trk(ntrk).ipnt(trk(ntrk).nhit)).row,100)-1
      isect      = hit(trk(ntrk).ipnt(trk(ntrk).nhit)).row/100
      irow_last  = tpar(ipass).irow_seg_end
      irow_step = -1
C

      ngap = 0
      nf = 0

      ir1 = irow_start
      do while (ir1.ge.irow_last.and.ngap.le.tpar(ipass).nhol)
         if(ngap.eq.0) then
C           find linear parametrisation for the segment
C           if there was a gap parametrisation for a row won't change
            npnt = trk(ntrk).nhit+nf
            ip4 = trk(ntrk).ipnt(npnt-2)
            ip3 = trk(ntrk).ipnt(npnt-1)
            ip2 = trk(ntrk).ipnt(npnt)
            ir2 = mod(hit(ip2).row,100)
            r2 = sqrt(hit(ip2).x**2+hit(ip2).y**2)
            ax = hit(ip3).x-hit(ip2).x
            ay = hit(ip3).y-hit(ip2).y
            az = hit(ip3).z-hit(ip2).z
            ar = sqrt(hit(ip3).x**2+hit(ip3).y**2)-
     >           sqrt(hit(ip2).x**2+hit(ip2).y**2)
            bx = hit(ip4).x-hit(ip3).x
            by = hit(ip4).y-hit(ip3).y
            bz = hit(ip4).z-hit(ip3).z
            br = sqrt(hit(ip4).x**2+hit(ip4).y**2)-
     >           sqrt(hit(ip3).x**2+hit(ip3).y**2)
            dydx = -(hit(ip3).y-hit(ip2).y)/
     >            (hit(ip2).y*hit(ip3).x-hit(ip3).y*hit(ip2).x)
            oxy =  (hit(ip3).x-hit(ip2).x)/
     >            (hit(ip2).y*hit(ip3).x-hit(ip3).y*hit(ip2).x)

            dzdr = az/ar
         endif
C        find what is the parametrisation for the row ir1 in sector 
C        isect
         iret = tgc_row_par(isect,real(ir1),as,bs,dummy,dummy)
C        find what is the prediction for this row
         iret = tpt_ptcrs(as,bs,dydx,oxy,xp1,yp1)
C        and check whether it lies in the same sector
         xglobal(1)=xp1
         xglobal(2)=yp1
         xglobal(3)=10.0
         iret =  tgc_global_to_local(isec1,xglobal,xlocal)
         if(isec1.gt.12) isec1=24-isec1
         if(isec1.eq.0) isec1=12
         if( isect.eq.isec1 ) then
           rp1 = sqrt(xp1**2+yp1**2)
           drp = r2 - rp1
           zp1 = hit(ip2).z - dzdr*drp
           if(abs(zp1).lt.210.0)then
           irets=tpt_find_slic(zp1,tpar(ipass).nzslic,islic)

C          check there are hits here
           is_pnt = slice_loc(isect,ir1,islic)
           nh1 = slice_pnt(2,is_pnt)
           if(nh1.ne.0)then
             dis0 = 10000000.0
             nfound = 0

             i = slice_pnt(1,is_pnt)
             do ilc = 1,nh1
               if(tpar(ipass).debug(5).eq.1) write(6,*) 
     >         'looking for a matching point'
               if(hit(i).track.eq.0) then
                 drow = ir2 - ir1
                 if(abs(hit(i).z-zp1)
     >           .lt.tpar(ipass).dzplim*hit(i).zrf) then
                   dy = hit(i).y-yp1
                   dx = hit(i).x-xp1
                   dz = hit(i).z-zp1
                   dfir = dx**2+dy**2
                   tolr2 = (tpar(ipass).drplim*hit(i).prf)**2
                   if(dfir.lt.tolr2) then
                     disr2 = dy**2+dx**2+dz**2
                     if(disr2.lt.dis0) then
                       dis0 = disr2
                       nfound = nfound+1
                       if(nfound.eq.1) nf = nf+1
                       trk(ntrk).ipnt(3+nf)=i
                     endif
                   endif
                 endif
               endif
               i = next_hit(i)
             enddo
           endif
           endif
           if( nfound .eq. 0 ) then
C            no candidate found in this row
             ngap = ngap+1
           else
C            candidate found reset gap counter
             ngap = 0
           endif
         else
            ngap = ngap+1
         endif
         ir1 = ir1 + irow_step
      end do   
      if(nf.ne.0) then
C         succesfull search - fit a track
          trk(ntrk).nhit = 3+nf
          tte_skip=0
          iret =
     >    tpt_fit_track(ntrk,trk,hit_h,hit,tpar_h,tpar,tte_skip,ipass)
          if(iret.eq.STAFCV_OK) then
             tpt_make_segment = STAFCV_OK
          else
             tpt_make_segment = STAFCV_BAD
          endif
      else
          tpt_make_segment = STAFCV_BAD
      end if
      end
