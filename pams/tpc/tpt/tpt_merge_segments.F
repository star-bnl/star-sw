* $Id: tpt_merge_segments.F,v 1.17 2000/03/19 04:20:33 sakrejda Exp $
* $Log: tpt_merge_segments.F,v $
* Revision 1.17  2000/03/19 04:20:33  sakrejda
* An option to use straigth line to fit lasers even if the field is on.
*
* Revision 1.16  2000/03/10 08:15:07  sakrejda
* Merged tracks arranged based on order of the end-points in Z
*
* Revision 1.15  2000/03/07 04:39:32  sakrejda
* Track merging in field cleaned up.
*
* Revision 1.14  2000/03/06 04:19:24  sakrejda
* Fixed bug in track indexing in the merge part.
*
* Revision 1.13  2000/03/05 01:32:05  sakrejda
* New version of spiral merging
*
* Revision 1.12  1999/12/17 16:30:53  sakrejda
* Introduced additional integer to avit having mod(short,int).
* It was not harmful, but confusing and debuggers complained about it.
*
* Revision 1.11  1999/11/03 03:25:28  sakrejda
* When two tracks were merged, hits were ordered on track one by one.
* Those that were already placed, were marked as such, but were not removed
* from the further considerations. This problem was fixed.
*
* Revision 1.10  1999/09/14 00:53:12  liq
* add tpt_mxdim.inc, use tpt_track_pointers.inc instead of tpt_pointers.inc in order to used by C++ codes
*
* Revision 1.9  1999/07/19 19:15:55  genevb
* Use StMessMgr
*
* Revision 1.8  1999/05/05 21:52:19  sakrejda
* local arrays converted to dynamic ones (with malloc)
*
* Revision 1.7  1998/09/01 13:32:37  sakrejda
* restored sector numbering 1-24 in tphit.row
*
* Revision 1.6  1998/07/07 14:27:26  sakrejda
* tracks, hits, id defined as integers
*
* Revision 1.5  1998/06/01 06:41:01  sakrejda
* merging improved so it does not create ghost tracks
*
* Revision 1.4  1998/04/09 23:11:36  sakrejda
* changes from Joachim Nystrand added
*
* Revision 1.3  1998/04/06 19:20:01  sakrejda
* quiks replaced by tls_Quick_Sort_r in merging routine
*
* Revision 1.2  1998/01/27 00:35:50  fisyak
* Split sources
*
      INTEGER FUNCTION TPT_MERGE_SEGMENTS(track_h,track,trk,hit_h,hit,
     >   tpar,tpar_h,ipass)
C>---------------------------------------------------------------------
C
C TPT_MERGE_SEGMENTS - merges pieces of a track
C
C DESCRIPTION:
C 
C INPUT ARGUMENTS:
C OUTPUT ARGUMENTS:
C<---------------------------------------------------------------------
C
C Modified: 13-NOV-1997 by R.Bossingham
C           Eliminate unused variables.
C
      IMPLICIT NONE
#include "PAM.inc"

#include "tpt_track.inc"
#include "tpt_track_pointers.inc"
#include "tpt_mxdim.inc"
#include "tcl_tphit.inc"
#include "tpt_pars.inc"
#include "math_constants.inc"
C______________________________________________________________________
c
c Define local structure to sort radius
      STRUCTURE /track_merge/
                integer lpnt           ! location of a track with given radius
                real    rad            ! radius of a track
                real    xclose(2)
                real    yclose(2)
      END STRUCTURE
C______________________________________________________________________
      RECORD /table_head_st/    track_h
      RECORD /tpt_track_st/     track(*)
      RECORD /table_head_st/    hit_h
      RECORD /tcl_tphit_st/     hit(*)
      RECORD /tpt_track_pointers_st/   trk(*)
      RECORD /table_head_st/    tpar_h
      RECORD /tpt_pars_st/      tpar(*)

      RECORD /track_merge/      merge(mxtrack)
C_________________________________________________________________
      INTEGER ltot,tls_Quick_Sort_r,iret
      INTEGER i,j,k,ipass, lxi, lxj, lvmin
      INTEGER lstep1, lstep2, lstart1, lstart2, lx1, lx2
      REAL    min(5),cuta,cutb
      INTEGER tpt_fit_track, tpt_load_track
      REAL    radi,radj
      REAL    varb,varc

      REAL    temp,zedge(4)
      INTEGER dummy

      CHARACTER*132 m132
      logical enough
C______________________________________________________________________

c Order the good tracks according to their helix radius.
c The array lpnt will point to each of these tracks in order.

      ltot = 0
      do i=1,track_h.nok
         if(trk(i).flag.ge.0) then
            ltot=ltot+1
            merge(ltot).rad=abs(trk(i).p(2))
            merge(ltot).lpnt=i
            merge(ltot).xclose(1)=trk(i).p(3)*
     >                            (1.0+merge(ltot).rad/sqrt(trk(i).p(3)**2+
     >                            trk(i).p(4)**2))
            merge(ltot).xclose(2)=trk(i).p(3)*
     >                            (1.0-merge(ltot).rad/sqrt(trk(i).p(3)**2+
     >                            trk(i).p(4)**2))
            merge(ltot).yclose(1)=trk(i).p(4)*
     >                            (1.0+merge(ltot).rad/sqrt(trk(i).p(3)**2+
     >                            trk(i).p(4)**2))
            merge(ltot).yclose(2)=trk(i).p(4)*
     >                            (1.0-merge(ltot).rad/sqrt(trk(i).p(3)**2+
     >                            trk(i).p(4)**2))
            if((merge(ltot).xclose(1)**2+merge(ltot).yclose(1)**2).gt.
     >      (merge(ltot).xclose(2)**2+merge(ltot).yclose(2)**2)) then
               temp=merge(ltot).xclose(1)
               merge(ltot).xclose(1)=merge(ltot).xclose(2)
               merge(ltot).xclose(2)=temp
               temp=merge(ltot).yclose(1)
               merge(ltot).yclose(1)=merge(ltot).yclose(2)
               merge(ltot).yclose(2)=temp
            endif
         endif
      enddo
C      go to 999
      iret=tls_Quick_Sort_r(ltot, merge(1).rad, merge(2).rad, merge)


c Minimum difference values for merging tracks 
c (min(1)=rr,min(2)=average of acent & bcent, min(3)=ltan, min(4)=phi0):
c Adjust these parameters to change the merging criteria.
      min(1)=0.0001! radius ratio
      min(2)=0.05  ! approach average
      min(3)=0.02  ! tanl
      min(4)=0.5   ! phi0 (deactivated)
      min(5)=10.0  ! Cut for avoiding merging parallel tracks 

c Loop over track radii in order. Check if radii match. Go on if yes.
c radd is the larger of the radii being compared.
      do i=1,ltot-1
         j=i+1
         lxi=merge(i).lpnt
         if(trk(lxi).flag.gt.0) enough = .false.
         do while (.not.enough)
         radi = merge(i).rad
         radj = merge(j).rad
         if(radi.gt.1500.0) then
            enough = abs(radi-radj)/radj.gt.min(1)*radi
         else
            enough = abs(radi-radj)/radj.gt.0.1
         endif
         lxj=merge(j).lpnt
         if(.not.enough.and.trk(lxj).flag.gt.0) then
c           Now compare the average of the differences of the 
c           x and y for the closest apptoach to the origin.
            cuta = abs(merge(i).xclose(1)-merge(j).xclose(1))/merge(i).rad
            cutb = abs(merge(i).xclose(1)-merge(j).xclose(1))/merge(i).rad
            if (cuta.lt.min(2).and.cutb.lt.min(2)) then
c               Now compare their dip angles.
                varc=abs(abs(trk(lxi).p(7))-abs(trk(lxj).p(7)))
                if (abs(varc).lt.min(3)) then
                   track_h.nok=track_h.nok+1
                   zedge(1)=hit(trk(lxi).ipnt(trk(lxi).nhit)).z
                   zedge(2)=hit(trk(lxi).ipnt(1)).z
                   zedge(3)=hit(trk(lxj).ipnt(trk(lxj).nhit)).z
                   zedge(4)=hit(trk(lxj).ipnt(1)).z
                   iret =lvmin(zedge,4)
                   if(iret.lt.3) then
                      lx1=lxi
                      lx2=lxj
                      if(zedge(1).gt.zedge(2))then
                           lstart1=0
                           lstep1=1
                      else
                           lstart1=trk(lx1).nhit+1
                           lstep1=-1
                      endif
                      if(zedge(3).gt.zedge(4))then
                           lstart2=0
                           lstep2=1
                      else
                           lstart2=trk(lx2).nhit+1
                           lstep2=-1
                      endif
                   else
                      lx1=lxj
                      lx2=lxi
                      if(zedge(3).gt.zedge(4))then
                           lstart1=0
                           lstep1=1
                      else
                           lstart1=trk(lx1).nhit+1
                           lstep1=-1
                      endif
                      if(zedge(1).gt.zedge(2))then
                           lstart2=0
                           lstep2=1
                      else
                           lstart2=trk(lx2).nhit+1
                           lstep2=-1
                      endif
                   endif
C                  store hits from both tracks in a local struct
                   do k=1,trk(lx1).nhit
                      trk(track_h.nok).ipnt(k)=trk(lx1).ipnt(lstart1+lstep1*k)
                   end do
                   do k=1,trk(lx2).nhit
                      trk(track_h.nok).ipnt(k+trk(lx1).nhit)=trk(lx2).ipnt(lstart2+lstep2*k)
                   end do
                   trk(track_h.nok).nhit=trk(lxi).nhit+trk(lxj).nhit
C
                   do  k=1,trk(track_h.nok).nhit
                      hit(trk(track_h.nok).ipnt(k)).track=1000*track_h.nok+k
                   end do
c                  refit track
c                  call the fitting and loading
                   iret = tpt_fit_track(track_h.nok,trk,hit_h,hit,tpar_h,tpar,dummy,ipass)
                   if(trk(track_h.nok).flag.gt.0)then
                     iret = tpt_load_track(track_h.nok,track_h,track,trk,hit_h,hit,tpar_h,tpar,ipass)
C                    flag the contributing track as "admerged"
                     trk(lxj).flag=-77
                     track(lxj).flag=-77
                     trk(lxi).flag=-77
                     track(lxi).flag=-77
                   else
C                   if fit fails
                    track_h.nok=track_h.nok-1
                    do  k=1,trk(lxi).nhit
                      hit(trk(lxi).ipnt(k)).track=1000*lxi+k
                    end do
                    do  k=1,trk(lxj).nhit
                      hit(trk(lxj).ipnt(k)).track=1000*lxj+k
                    end do
                  endif

              endif   ! p(7) or tanl
            endif   ! cuta,b
         endif! track that passes the radius criterion already merged 
         j=j+1
         if(j.gt.ltot)enough=.true.
         enddo   ! rad
      enddo   ! i  
c
 999  continue
      tpt_merge_segments = STAFCV_OK
      end
