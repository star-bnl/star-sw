      INTEGER FUNCTION TPT_MERGE_LINE(track_h,track,trk,hit_h,hit,
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
C____________________________________________________________________
c Define local structure to match tracks
      STRUCTURE /track_merge/
                integer lpnt           ! location within the track structure
                real    xa             ! x position of the closest approach
                real    ya
                real    za
      END STRUCTURE

      STRUCTURE /local_hits/
                integer lhit
                integer ltrack
                real    x
                real    y
                real    z
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
      RECORD /local_hits/       rhits(1000)
C_________________________________________________________________

      INTEGER ltot,i, ipass, ltr1, ltr2, j, iret, k, lastp
      INTEGER tls_quick_sort_r, tpt_fit_track_line, tpt_load_track
      INTEGER ipos, lvmax,nhits1, nhits2, ipnt1_low, ipnt1_high
C
      REAL    sl,langle, xalimit, yalimit, zalimit
      REAL    psilimit, tanllimit, dif(3)
      LOGICAL more


      DATA xalimit,yalimit,zalimit,psilimit,tanllimit/1.5,1.5,1.5,0.5,.01/

C loop over all the tracks and calculate parameters at the closest
C approach to the origin

      ltot = 0
      do i=1,track_h.nok
         if(trk(i).flag.ge.0) then
            ltot=ltot+1
            lastp=trk(i).ipnt(1)
            merge(ltot).lpnt=i
            merge(ltot).xa=-track(i).r0*sind(track(i).phi0-track(i).psi)*sind(track(i).psi)
            merge(ltot).ya=track(i).r0*sind(track(i).phi0-track(i).psi)*cosd(track(i).psi)
            sl=track(i).r0*abs(cosd(track(i).phi0-track(i).psi))
            langle=(hit(lastp).x-track(i).r0*cosd(track(i).phi0))*
     >             (merge(ltot).xa-track(i).r0*cosd(track(i).phi0))+
     >             (hit(lastp).y-track(i).r0*sind(track(i).phi0))*
     >             (merge(ltot).ya-track(i).r0*sind(track(i).phi0))
c
c           if the closest approach point and the last point are on
c           opposite sides of a track, change the sign of the track length
            if(langle.lt.0) sl=-sl
            merge(ltot).za=track(i).z0+sl*track(i).tanl
         endif
      enddo
      if(ltot.le.1) return
c     now sort according to x
      iret=tls_Quick_Sort_r(ltot, merge(1).xa, merge(2).xa, merge)
c     loop over and try to merge
      do i=1,ltot-1
         more=.true.
         j=i
         ltr1=merge(i).lpnt
         if(track(ltr1).flag.gt.0) then
         do while(more.and.(j.lt.ltot))
           j=j+1
           ltr2=merge(j).lpnt
           if(abs(merge(i).xa-merge(j).xa).lt.xalimit) then
              if(track(ltr2).flag.gt.0.and.
     >           abs(merge(i).ya-merge(j).ya).lt.yalimit.and.
     >           abs(merge(i).za-merge(j).za).lt.zalimit.and.
     >           ((abs(track(ltr1).psi-track(ltr2).psi).lt.psilimit.and.
     >           abs(track(ltr1).tanl-track(ltr2).tanl).lt.tanllimit)
     >           .or.
     >           (abs(abs(track(ltr1).psi-track(ltr2).psi)-180.0).lt.psilimit.and.
     >           abs(track(ltr1).tanl+track(ltr2).tanl).lt.tanllimit))) then
c                we've got a match
                 more =.false.
c                oreder them so that they span in the direction where they span most
                 nhits1=track(ltr1).nrec
                 do k=1,nhits1
                    rhits(k).ltrack=track(ltr1).id
                    rhits(k).lhit  = trk(ltr1).ipnt(k)
                    rhits(k).x     = hit(rhits(k).lhit).x
                    rhits(k).y     = hit(rhits(k).lhit).y
                    rhits(k).z     = hit(rhits(k).lhit).z
                 enddo
                 nhits2=track(ltr2).nrec
                 do k=1,nhits2
                    rhits(k+nhits1).ltrack=track(ltr2).id
                    rhits(k+nhits1).lhit  = trk(ltr2).ipnt(k)
                    rhits(k+nhits1).x     = hit(rhits(k+nhits1).lhit).x
                    rhits(k+nhits1).y     = hit(rhits(k+nhits1).lhit).y
                    rhits(k+nhits1).z     = hit(rhits(k+nhits1).lhit).z
                 enddo

c                find edge hits for the first track
                 ipnt1_low=trk(ltr1).ipnt(1)
                 ipnt1_high=trk(ltr1).ipnt(nhits1)
                 dif(1) = abs(hit(ipnt1_low).x-hit(ipnt1_high).x)
                 dif(2) = abs(hit(ipnt1_low).y-hit(ipnt1_high).y)
                 dif(3) = abs(hit(ipnt1_low).z-hit(ipnt1_high).z)
                 ipos=lvmax(dif,3)
                 if(ipos.eq.1) then
c                  sort them on x
                   iret=tls_Quick_Sort_r(nhits1+nhits2, rhits(1).x, rhits(2).x, rhits)
                 elseif(ipos.eq.2) then
c                  sort them on y
                   iret=tls_Quick_Sort_r(nhits1+nhits2, rhits(1).y, rhits(2).y, rhits)
                 else
c                  sort them on z
                   iret=tls_Quick_Sort_r(nhits1+nhits2, rhits(1).z, rhits(2).z, rhits)
                 endif
c                fill the trk struct with the new info
                 trk(ltr1).nhit=nhits1+nhits2
                 do k=1,nhits1+nhits2
                     trk(ltr1).ipnt(k)=rhits(k).lhit
                     hit(rhits(k).lhit).track=ltr1*1000+k
                 enddo
c                call the fitting and loading
                 iret = tpt_fit_track_line(ltr1,trk,hit_h,hit,tpar_h,tpar,ipass)
                 iret = tpt_load_track(ltr1,track_h,track,trk,hit_h,hit,tpar_h,tpar,ipass)
c                get rid of the second track
                 track(ltr2).flag=-77                
                 more = .false.
              endif
           else
             more=.false.
           endif
         enddo
         endif
      enddo
      tpt_merge_line= STAFCV_OK
      end
