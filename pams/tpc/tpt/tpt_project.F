* $Id: tpt_project.F,v 1.13 2000/02/13 22:45:49 sakrejda Exp $
* $Log: tpt_project.F,v $
* Revision 1.13  2000/02/13 22:45:49  sakrejda
* CVo ----------------------------------------------------------------------
* nly truly edge pads left out
* 
*
* Revision 1.12  1999/12/17 16:30:53  sakrejda
* Introduced additional integer to avit having mod(short,int).
* It was not harmful, but confusing and debuggers complained about it.
*
* Revision 1.11  1999/11/07 05:53:39  sakrejda
* Calculation of the dip and crossing angle moved to tpt_load_track.F
*
* Revision 1.10  1999/11/01 07:09:05  sakrejda
* Information about the first point on a track updated.
*
* Revision 1.9  1999/10/15 00:54:08  nevski
* pgf->g77 cleanup
*
* Revision 1.8  1999/09/14 00:55:51  liq
* use tpt_track_pointers.inc instead of tpt_pointers.inc in order to used by C++ codes
*
* Revision 1.7  1999/05/05 21:52:22  sakrejda
* local arrays converted to dynamic ones (with malloc)
*
* Revision 1.6  1999/03/30 15:55:03  love
*  Spelling.  make_segment_uv removed doubling of tolerance on row skip.
*  row_sect_point remove check on sector z position
*
* Revision 1.5  1998/09/01 13:32:38  sakrejda
* restored sector numbering 1-24 in tphit.row
*
* Revision 1.4  1998/08/26 17:03:06  sakrejda
* calculation of the dip angle for hits on tracks added
*
* Revision 1.3  1998/03/23 18:26:03  sakrejda
* tpg_pad-plane removed from the tpt module, it was not used there anyway
*
* Revision 1.2  1998/01/27 00:35:50  fisyak
* Split sources
*
      INTEGER FUNCTION TPT_PROJECT(itrk,hit_h,hit,track_h,track,
     >                 tpar_h,tpar,trk,next_hit,
     >                 slice_loc,slice_pnt,ipass)
C>--------------------------------------------------------------------
C
C TPT_PROJECT - adds more points to the itrk track
C
C DESCRIPTION:
C Two neighbouring sectors as well as the one in which the segment is
C are searched for hits that are within tolerances.
C
C INPUT ARGUMENTS:
C  itrk     - track number of a track to be extended
C  hit_h     - header for the tphit (tpc hits) table
C  hit      - rows of the tphit table
C  track_h   - header for the tptrack (reconstructed tracks) table
C  track    - rows of the tptrack table
C  tpar_h    - header for the tptpar (tracking parameters) table
C  tpar     - tracking parameters table
C  loc_hit  - pointers to the tphit table sorted according to the row/sec field
C  tpstart  - pointers to first hits in every row/sector
C  tpcount  - count of hits for every row/sector
C  trk      - structure of reconstructed segments
C
C OUTPUT ARGUMENTS:
C  trk      - structure of reconstructed segments
C AUTHOR:
C<---------------------------------------------------------------------
C
C Modified: 13-NOV-1997 by R.Bossingham
C           Eliminate unused variables.
C
      IMPLICIT NONE

#include "PAM.inc"

#include "tpt_pars.inc"
#include "tpt_track.inc"
#include "tcl_tphit.inc"
#include "tpt_track_pointers.inc"
#include "tpt_mxdim.inc"

C______________________________________________________________________
      RECORD /table_head_st/    tpar_h
      RECORD /tpt_pars_st/      tpar(*)
      RECORD /table_head_st/    track_h
      RECORD /tpt_track_st/     track(*)
      RECORD /table_head_st/    hit_h
      RECORD /tcl_tphit_st/     hit(*)
      RECORD /tpt_track_pointers_st/   trk(*)
C_____________________________________________________________________
C
C     Local variables
      INTEGER next_hit(*),slice_loc(mxsector,mxrow,mxslic)
      INTEGER slice_pnt(2,mxsector*mxrow*mxslic)
      INTEGER itrk,irow,ient,isect,isect_full,is(2),i,np,ixpnt
      INTEGER iret, tpt_helix_pred ,l,ip_in,ip_out,row_in,row_out,llc
      INTEGER row_start(2),row_end(2),st(2),mdir,ip(2)
      INTEGER nf,lk,imem,ib,tpt_fit_track,tpt_cross_fact
      INTEGER tpt_load_track
      INTEGER islic,is_pnt,ipass,tte_skip
      INTEGER tpt_find_slic, irets,save_flag
      INTEGER tgc_sec24_to_sec12
      INTEGER longrow, irow_last

      LOGICAL more
      REAL    xp(2),yp(2),devsum0,dist,dc,alpha,devsum
      REAL    xold,yold,cf,zp, row_fact
      real    atand
C_____________________________________________________________________
C

c      ient     = trk(itrk).ipnt(1)
c      isect_full = hit(ient).row/100
c      iret       = tgc_sec24_to_sec12(isect_full,isect)
c      is(1)      = isect
c      ient     = trk(itrk).ipnt()
c      is(2)      = isect-1
c      if(is(2).eq.0) is(2)=12
c      is(3)      = isect+1
c      if(is(3).gt.12) is(3)=1
C
      ixpnt = trk(itrk).nhit
      ip_in = trk(itrk).ipnt(ixpnt)
      longrow = hit(ip_in).row
      row_in = mod(longrow,100)
      isect_full = hit(ip_in).row/100
      iret       = tgc_sec24_to_sec12(isect_full,isect)
      is(1)=isect
      ip_out = trk(itrk).ipnt(1)
      longrow = hit(ip_out).row
      row_out =  mod(longrow,100)
      isect_full = hit(ip_out).row/100
      iret       = tgc_sec24_to_sec12(isect_full,isect)
      is(2)=isect
      row_start(1) = row_in-1
      row_start(2) = row_out+1
      row_end(1) = 1
      row_end(2) = mxrow
      st(1)=-1
      st(2)=1
      ip(1) = ip_in
      ip(2) = ip_out

C     extend toward inner-row and then toward outer-row
      do mdir = 1,2
         irow_last=0
C        loop over rows
         irow =row_start(mdir)
         more=.true.
         if(irow.lt.1.or.irow.gt.45) more=.false.
         isect = is(mdir)
         do while(more)
           row_fact=1.0
           if(irow.lt.14)row_fact=1.5
C          find predictions for this row
           iret = tpt_helix_pred(itrk,trk,isect,
     >            irow,tpar_h,tpar,np,xp,yp,ipass)
C
           if(np.eq.0) then
              isect = is(mdir)+1
              if(isect.eq.13) isect=1
              iret = tpt_helix_pred(itrk,trk,isect,
     >               irow,tpar_h,tpar,np,xp,yp,ipass)
              if(np.eq.0) then
                 isect = is(mdir)-1
                 if(isect.eq.0) isect=1
                 iret = tpt_helix_pred(itrk,trk,isect,
     >                  irow,tpar_h,tpar,np,xp,yp,ipass)
              endif
           endif
           if(np.ne.0) then
             nf=0
             devsum0=10000000000.0
             do lk=1,np
               dc = sqrt((hit(ip(mdir)).x-xp(lk))**2+
     >                   (hit(ip(mdir)).y-yp(lk))**2)
               if(abs(trk(itrk).p(2))-0.5*dc.lt.0)
     >         dc=2.0*abs(trk(itrk).p(2))
               alpha = asin(0.5*dc/abs(trk(itrk).p(2)))
               zp = hit(ip(mdir)).z+st(mdir)*
     >         2.0*alpha*abs(trk(itrk).p(2))*trk(itrk).p(7)
               if(abs(zp).lt.210.0)then
               irets=tpt_find_slic(zp,tpar(ipass).nzslic,islic)
               is_pnt = slice_loc(isect,irow,islic) 
               l = slice_pnt(1,is_pnt)
C              loop over all hits within a z-slice
               do llc=1,slice_pnt(2,is_pnt)
                 if(hit(l).track.lt.1) then
                   dist = (hit(l).x-xp(lk))**2+(hit(l).y-yp(lk))**2
                   if(dist.lt.row_fact*(tpar(ipass).drstr*hit(l).prf)**2) then
C                    check z
                     if(abs(zp-hit(l).z).lt.sqrt(hit(l).zrf**2+
     >               hit(ip(mdir)).zrf**2)*tpar(ipass).dzstr) then
C                        a good candidate found
                         devsum = (zp-hit(l).z)**2+dist
                         if(devsum.lt.devsum0) then
C                          memorize it
                           imem = l
                           nf=nf+1
                           devsum0=devsum
                         endif
                     endif
                   endif
                 endif
                 l = next_hit(l)
               enddo
               endif
             enddo
             if(nf.ne.0) then
C              a hit found
               if(mdir.eq.2) then
                  do ib=trk(itrk).nhit,1,-1
                     trk(itrk).ipnt(ib+1)=trk(itrk).ipnt(ib)
                  end do
                  trk(itrk).ipnt(1)=imem
               else
                  ixpnt = trk(itrk).nhit+1
                  trk(itrk).ipnt(ixpnt) = imem
               endif
               trk(itrk).nhit = trk(itrk).nhit + 1
             else if(slice_pnt(2,is_pnt).gt.0) then
               save_flag=trk(itrk).flag
               iret=tpt_fit_track(itrk,trk,hit_h,hit,tpar_h,tpar,tte_skip,ipass)
               if((iret.eq.STAFCV_OK).and.(irow.ne.irow_last)) then
                  trk(itrk).flag=save_flag
                  iret = tpt_load_track(itrk,track_h,track,trk,hit_h,hit)
                  tpt_project = STAFCV_OK
                  irow_last=irow
                  irow=irow-st(mdir)
               endif
             endif
           endif
           irow=irow+st(mdir)
           if(irow.lt.1.or.irow.gt.45) more=.false.
         end do
      end do
      tte_skip=0
      save_flag=trk(itrk).flag
      iret=tpt_fit_track(itrk,trk,hit_h,hit,tpar_h,tpar,tte_skip,ipass)
      if(iret.eq.STAFCV_OK) then
         trk(itrk).flag=save_flag
         iret = tpt_load_track(itrk,track_h,track,trk,hit_h,hit)
         tpt_project = STAFCV_OK
      else
         tpt_project = STAFCV_BAD
      endif
      end
