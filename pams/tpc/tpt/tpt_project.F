* $Id: tpt_project.F,v 1.5 1998/09/01 13:32:38 sakrejda Exp $
* $Log: tpt_project.F,v $
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
C Two neighbouring sectors as wee as the one in which the segment is
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
#include "tpt_pointers.inc"
#include "tpt_mxdim.inc"
#include "tpt_mxhit.inc"

C______________________________________________________________________
      RECORD /table_head_st/    tpar_h
      RECORD /tpt_pars_st/      tpar(*)
      RECORD /table_head_st/    track_h
      RECORD /tpt_track_st/     track(*)
      RECORD /table_head_st/    hit_h
      RECORD /tcl_tphit_st/     hit(*)
      RECORD /track_pointers/   trk(*)
C_____________________________________________________________________
C
C     Local variables
      INTEGER next_hit(tpt_mxhit),slice_loc(mxsector,mxrow,mxslic)
      INTEGER slice_pnt(2,mxsector*mxrow*mxslic)
      INTEGER itrk,irow,ient,isect,isect_full,is(3),i,np,ixpnt
      INTEGER iret, tpt_helix_pred ,l,ip_in,ip_out,row_in,row_out,llc
      INTEGER row_start(2),row_end(2),st(2),mdir,ip(2)
      INTEGER nf,lk,imem,ib,tpt_fit_track,tpt_cross_fact
      INTEGER tpt_load_track
      INTEGER islic,is_pnt,ipass,tte_skip
      INTEGER tpt_find_slic, irets,save_flag
      INTEGER tgc_sec24_to_sec12

      LOGICAL more
      REAL    xp(2),yp(2),devsum0,dist,dc,alpha,devsum
      REAL    xold,yold,cf,zp
C_____________________________________________________________________
C

      ient       = trk(itrk).ipnt(1)
      isect_full = hit(ient).row/100
      iret       = tgc_sec24_to_sec12(isect_full,isect)
      is(1)      = isect
      is(2)      = isect-1
      if(is(2).eq.0) is(2)=12
      is(3)      = mod(isect+1,12)
C
      do i=1,3
         isect = is(i)
         ixpnt = trk(itrk).nhit
         ip_in = trk(itrk).ipnt(ixpnt)
         row_in = mod(hit(ip_in).row,100)
         ip_out = trk(itrk).ipnt(1)
         row_out =  mod(hit(ip_out).row,100)
         row_start(1) = row_in-1
         row_start(2) = row_out+1
         row_end(1) = 1
         row_end(2) = mxrow
         st(1)=-1
         st(2)=1
         ip(1) = ip_in
         ip(2) = ip_out

C        extend toward inner-row and then toward outer-row
         do mdir = 1,2

C        loop over rows
         irow =row_start(mdir)
         more=.true.
         if(irow.lt.1.or.irow.gt.45) more=.false.
         do while(more)
C          find predictions for this row
           iret = tpt_helix_pred(itrk,trk,isect,
     >            irow,tpar_h,tpar,np,xp,yp,ipass)
C
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
                   if(dist.lt.(tpar(ipass).drstr*hit(l).prf)**2) then
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
             endif
           endif
           irow=irow+st(mdir)
           if(irow.lt.1.or.irow.gt.45) more=.false.
         end do
         end do
      end do
      tte_skip=0
      save_flag=trk(itrk).flag
      iret=tpt_fit_track(itrk,trk,hit_h,hit,tpar_h,tpar,tte_skip,ipass)
      if(iret.eq.STAFCV_OK) then
         ixpnt = trk(itrk).nhit
         l = trk(itrk).ipnt(ixpnt)
         xold = hit(l).x
         yold = hit(l).y
         do i=trk(itrk).nhit-1,1,-1
            ient = trk(itrk).ipnt(i)
            iret = tpt_cross_fact(hit(ient).row,hit(ient).x,
     >          hit(ient).y,xold,yold,cf,hit(ient).alpha)
            xold = hit(ient).x
            yold = hit(ient).y
            hit(ient).lambda=atand(trk(itrk).p(7))
         end do
         hit( trk(itrk).ipnt(trk(itrk).nhit)).alpha =
     >   hit( trk(itrk).ipnt(trk(itrk).nhit-1)).alpha
         hit( trk(itrk).ipnt(trk(itrk).nhit)).lambda =
     >   hit( trk(itrk).ipnt(trk(itrk).nhit-1)).lambda
         trk(itrk).flag=save_flag
         iret = tpt_load_track(itrk,track_h,track,trk)
         tpt_project = STAFCV_OK
      else
         tpt_project = STAFCV_BAD
      endif
      end
