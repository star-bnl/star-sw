* $Id: tpt_make_segment.F,v 1.4 1998/09/01 13:32:36 sakrejda Exp $
* $Log: tpt_make_segment.F,v $
* Revision 1.4  1998/09/01 13:32:36  sakrejda
* restored sector numbering 1-24 in tphit.row
*
* Revision 1.3  1998/03/23 18:26:01  sakrejda
* tpg_pad-plane removed from the tpt module, it was not used there anyway
*
* Revision 1.2  1998/01/27 00:35:49  fisyak
* Split sources
*
      INTEGER FUNCTION TPT_MAKE_SEGMENT(ntrk,hit_h,hit,tpar_h,tpar,trk,
     >                        next_hit,slice_loc,slice_pnt,ipass)
C>----------------------------------------------------------------------
C
C TPT_MAKE_SEGMENT - generate one segment starting from a given root
C
C DESCRIPTION:
C Routine generates a track segment asuming it starts at the vertex 
C and uses linear extrapolation to update track parameters. Once a
C new point is found the outermost point is dropped and the new one
C is used to evaluate track parameters.
C 
C INPUT PARAMETERS:
C  tpar_h    - header for the tptpar (tracking parameters) table
C  tpar     - tracking parameters table
C  hit_h     - header for the tphit (tpc hits) table
C  hit      - rows of the tphit table
C  loc_hit  - pointers to the tphit table sorted according to the 
C             row/sec field
C  tpstart  - pointers to first hits in every row/sector
C  tpcount  - # of hits in a given row/sector
C  ntrk     - current track number
C
C OUTPUT ARGUMENTS:
C  trk      - structure of reconstructed segments
C<---------------------------------------------------------------------
C
C Modified: 13-NOV-1997 by R.Bossingham
C           Eliminate unused variables.
C
      IMPLICIT NONE

#include "PAM.inc"

#include "tpt_pars.inc"
#include "tcl_tphit.inc"
#include "tpt_pointers.inc"
#include "tpt_mxdim.inc"
#include "tpt_mxhit.inc"

C______________________________________________________________________

      RECORD /table_head_st/    tpar_h
      RECORD /tpt_pars_st/      tpar(*)
      RECORD /table_head_st/    hit_h
      RECORD /tcl_tphit_st/     hit(*)

      RECORD /track_pointers/   trk(*)
C_____________________________________________________________________
C
C     Local variables
      INTEGER ntrk,ir1,ir2,npnt,ip2,ip3,ip4,nfound,iret
      INTEGER isect, isect_trunc, isec1
      INTEGER irow_last,irow_start,irow_step,ngap
      INTEGER tgc_row_par, tpt_ptcrs
      INTEGER next_hit(tpt_mxhit),slice_loc(mxsector,mxrow,mxslic)
      INTEGER slice_pnt(2,mxsector*mxrow*mxslic)
      INTEGER nf,drow,i,nh1,ilc
      INTEGER tpt_fit_track
      INTEGER is_pnt,islic,ipass,tte_skip
      INTEGER tpt_find_slic, irets

      INTEGER tgc_global_to_sector
      INTEGER tgc_sec24_to_sec12

      REAL    ax,ay,az,ar,bx,by,bz,br,dydx,oxy,dzdr,as,bs,xp1,yp1
      REAL    zp1,rp1,drp,dis0,dy,dx,dz,dfir,tolr2,disr2,r2
      REAL    xglobal(3), dummy

C     Find what was the innermost row number and continue the search 
C     towards the inside of the chamber
      irow_start = mod(hit(trk(ntrk).ipnt(trk(ntrk).nhit)).row,100)-1
      isect      = hit(trk(ntrk).ipnt(trk(ntrk).nhit)).row/100
      iret       = tgc_sec24_to_sec12(isect,isect_trunc)
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
         iret = tgc_row_par(isect_trunc,real(ir1),as,bs,dummy,dummy)
C        find what is the prediction for this row
         iret = tpt_ptcrs(as,bs,dydx,oxy,xp1,yp1)
C        and check whether it lies in the same sector
         xglobal(1)=xp1
         xglobal(2)=yp1
         xglobal(3)=10.0
         iret =  tgc_global_to_sector(isec1,xglobal)
         if( isect_trunc.eq.isec1 ) then
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
