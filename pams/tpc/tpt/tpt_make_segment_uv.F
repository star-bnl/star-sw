* $Id: tpt_make_segment_uv.F,v 1.7 1998/09/01 13:32:36 sakrejda Exp $
* $Log: tpt_make_segment_uv.F,v $
* Revision 1.7  1998/09/01 13:32:36  sakrejda
* restored sector numbering 1-24 in tphit.row
*
* Revision 1.6  1998/04/22 20:02:19  didenko
* replacing real to float
*
* Revision 1.5  1998/04/22 15:57:58  sakrejda
* write to unit 6 replaced by message calls
*
* Revision 1.4  1998/03/31 17:00:54  sakrejda
* sector boundry crossing calculation corrected-was using old sector naming convention
*
* Revision 1.3  1998/03/23 18:26:02  sakrejda
* tpg_pad-plane removed from the tpt module, it was not used there anyway
*
* Revision 1.2  1998/01/27 00:35:50  fisyak
* Split sources
*
      INTEGER FUNCTION TPT_MAKE_SEGMENT_UV(ntrk,hit_h,hit,tpar_h,tpar,trk,
     >                          next_hit,slice_loc,slice_pnt,ipass )
C>---------------------------------------------------------------------
C
C TPT_MAKE_SEGMENT_UV - generate one segment starting from a given root
C
C DESCRIPTION:
C Routine generates a track segment asuming it starts at the vertex 
C and uses linear regression in the mapped coordinates to update
C track parameters.
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
C
C<---------------------------------------------------------------------
C
C Modified: 13-NOV-1997 by R.Bossingham
C           Eliminate unused variables.
C
      IMPLICIT NONE

#include "PAM.inc"
#include "math_constants.inc"
#include "tcl_tphit.inc"
#include "tpt_pars.inc"
#include "tpt_mxdim.inc"
#include "tpt_pointers.inc"
#include "tpt_mxhit.inc"

C______________________________________________________________________

      RECORD /table_head_st/    tpar_h
      RECORD /tpt_pars_st/      tpar(*)

      RECORD /table_head_st/    hit_h
      RECORD /tcl_tphit_st/     hit(*)

      RECORD /track_pointers/   trk(*)
C______________________________________________________________________
C
C     Local variables
      INTEGER ntrk,ir1,irlast,nfound,iret,ip
      INTEGER irow_last,irow_start,irow_step,ngap
      INTEGER isec1, isect, isect_trunc

      INTEGER ns,nf,drow,i,nh1,ilc,lsec

      INTEGER tpt_fit_track, tpt_ptcrs_uv_p,tte_skip
      INTEGER tpt_find_slic, irets, tpt_fit_uv,i_s, tpt_fit_sz

      INTEGER islic,is_pnt,ipass
      INTEGER next_hit(tpt_mxhit),slice_loc(mxsector,mxrow,mxslic)
      INTEGER slice_pnt(2,mxsector*mxrow*mxslic)

      INTEGER tgc_global_to_sector
      INTEGER tgc_sec24_to_sec12
      INTEGER tgc_row_par

      REAL    as,bs,xp1,yp1,xp(2),yp(2),xlast,ylast,zlast
      REAL    zp1,sr1,dis0,dy,dx,dz,dfir,tolr2,disr2,dist1,dist2,fact
      REAL    at,bt,az,bz,stot
      REAL    phi,x0,y0,rc,da,ds
      REAL    xglobal(3), dummy

      LOGICAL sector_change
      

C     Find what was the innermost row number and continue the search 
C     towards the inside of the chamber
      irow_start = mod(hit(trk(ntrk).ipnt(trk(ntrk).nhit)).row,100)-1
      isect      = hit(trk(ntrk).ipnt(trk(ntrk).nhit)).row/100
      iret       = tgc_sec24_to_sec12(isect,isect_trunc)
      irow_last  = tpar(ipass).irow_seg_end
      irow_step  = -1
C
C

      ngap = 0
      nf = 0
C
      ip=trk(ntrk).ipnt(1)
      x0=hit(ip).x
      y0=hit(ip).y
      xlast=x0
      ylast=y0
      i_s=2
      if(tpar(ipass).form_uv.gt.1)then
        x0=0.0
        y0=0.0
        i_s=1
      endif 
      irets=tpt_fit_uv(x0,y0,0,hit(ip).x,hit(ip).y,at,bt)
      irets=tpt_fit_sz(x0,y0,at,bt,xlast,ylast,0,hit(ip).x,hit(ip).y,
     >                 hit(ip).z, az,bz,stot)
      do i=i_s,3
         ip=trk(ntrk).ipnt(i)
         irets=tpt_fit_uv(x0,y0,1,hit(ip).x,hit(ip).y,at,bt)
      end do
      do i=2,3
         ip=trk(ntrk).ipnt(i)
         irets=tpt_fit_sz(x0,y0,at,bt,xlast,ylast,1,
     >         hit(ip).x,hit(ip).y,hit(ip).z, az,bz,stot)
         xlast=hit(ip).x
         ylast=hit(ip).y
         zlast=hit(ip).z
      end do
C
      ir1 = irow_start
      irlast=ir1-1
      do while (ir1.ge.irow_last.and.ngap.le.tpar(ipass).nhol)
         ns = 0
         sector_change=.false.
         do while (ns.eq.0)
C           find what is the parametrisation for the row ir1 in 
C           sector isect
            iret = tgc_row_par(isect_trunc,float(ir1),as,bs,dummy,dummy)
C           find what is the prediction for this row
            iret = tpt_ptcrs_uv_p(as,bs,-1.0,at,bt,x0,y0,xp,yp)
C           check whether there is a crossing point
            if(iret.eq.STAFCV_OK) then
C              find which solution is closer
               dist1=(xlast-xp(1))**2+(ylast-yp(1))**2
               dist2=(xlast-xp(2))**2+(ylast-yp(2))**2
               ns=1
               if(dist2.lt.dist1) ns=2
               xp1=xp(ns)
               yp1=yp(ns)
               xglobal(1)=xp1
               xglobal(2)=yp1
               xglobal(3)=10.0
C              and check whether it lies in the same sector
               iret = tgc_global_to_sector(isec1,xglobal)
               if(isect_trunc.eq.isec1) then
                  ns=1
               else
                  ns=0
               endif
               if(ns.gt.0) sector_change=.false.
            else
               ns=-1
            endif
            if(ns.eq.0) then
              if(.not.sector_change) then
C              try to look into other sectors, find crossings with the 
C	       boundaries and find which boundary is closer
               sector_change=.true.
               lsec=15-isect_trunc
               if(lsec.ge.12) lsec=lsec-12
               phi=float(lsec)*C_PI/6.0+C_PI/12.0
               dist1=-1.0
               dist2=-1.0
               iret = tpt_ptcrs_uv_p(-tan(phi),1.0,0,at,bt,x0,y0,xp,yp)
               if(iret.eq.STAFCV_OK)then
                 dist1=(xlast-xp(1))**2+(ylast-yp(1))**2
                 dist2=(xlast-xp(2))**2+(ylast-yp(2))**2
                 if(dist1.gt.dist2) then
                  dist1=dist2
                 endif 
               endif
               phi=float(lsec-1)*C_PI/6.0+C_PI/12.0
               iret = tpt_ptcrs_uv_p(-tan(phi),1.0,0,at,bt,x0,y0,xp,yp)
               if(iret.eq.STAFCV_OK)then
                 dist2=(xlast-xp(1))**2+(ylast-yp(1))**2
                 if(dist2.gt.(xlast-xp(2))**2+(ylast-yp(2))**2)then
                    dist2=(xlast-xp(2))**2+(ylast-yp(2))**2
                 endif
               endif
               if(dist1.gt.0.and.dist2.gt.0.and.dist1.lt.dist2.or.
     >         dist1.gt.0.and.dist2.lt.0) then
                      isec1=isect_trunc-1
                      if(isec1.eq.0)isec1=12
                      isect_trunc=isec1
               else if(dist2.gt.0) then
                      isec1=isect_trunc+1
                      if(isec1.eq.13)isec1=1
                      isect_trunc=isec1
               else
                      ns=-1
               endif
              else
               ns=-1
              endif
            endif
         end do 
         if(ns.gt.0) then
C          prediction in the same sector, find 3 adjacent z-slices
           da=sqrt((xlast-xp1)**2+(ylast-yp1)**2)*0.5
           rc = 0.5*sqrt((at**2+1.0)/bt**2)
           nfound = 0
           if(da/rc.lt.1.0)then
           ds=2.0*rc*asin(da/rc)
           sr1=stot+ds
           zp1 = az*sr1+bz
           if(abs(zp1).lt.210.0)then
           irets=tpt_find_slic(zp1,tpar(ipass).nzslic,islic)
C
C          check there are hits here
           is_pnt = slice_loc(isect_trunc,ir1,islic)
           nh1 = slice_pnt(2,is_pnt)
           if( nh1 .ne. 0 ) then
             dis0 = 10000000.0

             is_pnt = slice_loc(isect_trunc,ir1,islic) 
             i = slice_pnt(1,is_pnt)
             do ilc = 1,slice_pnt(2,is_pnt)
               if(hit(i).track.eq.0) then
                 drow = irlast - ir1
                 fact=1.0
                 if(ir1.lt.14.or.drow.gt.1) fact=2.0
                 if(abs(hit(i).z-zp1)
     >           .lt.fact*tpar(ipass).dzplim*hit(i).zrf)then
                   dy = hit(i).y-yp1
                   dx = hit(i).x-xp1
                   dz = hit(i).z-zp1
                   dfir = dx**2+dy**2
                   fact=1.0
                   if(ir1.lt.14.or.drow.gt.1) fact=2.0
                   tolr2 = (fact*tpar(ipass).drplim*hit(i).prf)**2
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
               i=next_hit(i)
             end do
           endif
           endif
           endif
           if(nfound.eq.0) then
C            no candidate found in this row
             ngap = ngap+1
           else
C            candidate found reset gap counter
             ngap = 0
             ip = trk(ntrk).ipnt(3+nf)
             irets=tpt_fit_uv(x0,y0,1,hit(ip).x,hit(ip).y,at,bt)
             irets=tpt_fit_sz(x0,y0,at,bt,xlast,ylast,1,
     >         hit(ip).x,hit(ip).y,hit(ip).z, az,bz,stot)
             xlast=hit(ip).x
             ylast=hit(ip).y
             zlast=hit(ip).z
             irlast=ir1
           endif
         else
           ngap=ngap+1
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
             tpt_make_segment_uv = STAFCV_OK
          else
             tpt_make_segment_uv = STAFCV_BAD
          endif
      else
          tpt_make_segment_uv = STAFCV_BAD
      endif
      end
