* $Id: tte_track.F,v 1.4 1998/09/18 21:57:19 perev Exp $
* $Log: tte_track.F,v $
* Revision 1.4  1998/09/18 21:57:19  perev
* WRONG calling sequence for tls_sort
*
* Revision 1.3  1998/02/13 19:33:06  fisyak
* zerov => vzero
*
* Revision 1.2  1998/01/27 02:18:52  fisyak
* Add README
*
C<----------------------------------------------------------------------------
      INTEGER FUNCTION tte_track(track_h,       track,
     >                           hit_h,         hit,
     >                           g2t_hit_h,     g2t_hit,
     >                           g2t_track_h,   g2t_track,
     >                           tindex_h,      tindex,
     >                           tindex_type_h, tindex_type)

      IMPLICIT NONE

#include "tte_track.inc"

C DESCRIPTION:
C tte evaluates performance of the tpt package by comparing it's output
C with the output from gstar
C

C INPUT ARGUMENTS:
C  trackh    - header for the tptrack (reconstructed tracks) table
C  track     - rows of the tptrack table
C  hith      - header for the tphit (tpc hits) table
C  hit       - rows of the tphit table
C  g2t_hith  - header for the mc hit table
C  g2t_hit   - mc hit table
C  g2t_trackh- header for the mc track table
C  g2t_track - mc track table
C  tpc_indexh   - header of the matching table
C  tpc_index    - matching table
C  tindex_typeh,tindex_type,
C
C OUTPUT ARGUMENTS:

C_____________________________________________________________________________

      integer   tls_Quick_Sort_i              !integer sort (tls pkg)
      integer   tls_Quick_Sort_r              !real sort (tls pkg)

      integer   list_size                     !size of the list array
      parameter (list_size=1000000)
      integer   list_index(list_size)
*                                             !address mc hit in the index tb
      integer   ng2t                          !count of cross-indexed hits
      integer   list_g2t_hits(list_size)      !mc hit address in g2t_tpc_hit

      integer   hit_size
      parameter (hit_size=500000)
*                                             
      integer   list_hits(hit_size)           !rec. hits address in hit
      integer   long_list(hit_size)           !list of rec hits for 1 mc track
      integer   long_list_nok

      integer   len_short_list
      parameter (len_short_list=2000)
      integer   short_list(len_short_list)    !list of real_id's assigned
     
      integer   track_size
      parameter (track_size=100000)
      integer   list_g2t_track(track_size)
*                                             !track address in g2t_track
      integer   i,j                           !loop index
      integer   iret                          !function call status

      integer   ip                            !pointer to the next point on
c                                             !the mc track
      integer   index_mc                      !pointer to the mc hit in the
c                                             !index table
c
      integer   ip_real                       !pointer to the rec hit
      integer   istart                        !beginning of the mc_hit
c                                              <-> real hit assignement
      integer   iloc,ii
      integer   evmc_nok                      !number of tracks with hits in 
c                                             !in the chamber
      integer   ip_first                      !pointer to the first point
c                                             !on a track
      integer   id_first_real                 !id of a first reconstructed
c                                             !hit
      real      tof_first                     !tof to the first point
C______________________________________________________________________
      STRUCTURE /track_local/
                REAL    tof
                INTEGER ipr
                INTEGER ipmc
      END STRUCTURE
      RECORD  /track_local/ tl(len_short_list)
C______________________________________________________________________

c     set up an index table to address an mc hit by its hit id
c     but first clean up the list
c
      call vzero(list_g2t_hits,list_size)
C
      do i=1,g2t_hit_h.nok
         if(g2t_hit(i).id.gt.list_size.or.g2t_hit(i).id.lt.1) then
             tte_track=STAFCV_BAD
             write(6,*) 'list_size not big enough ','i = ', i,
     >      'list_size = ', list_size, 'g2t_hit(i).id = ', g2t_hit(i).id
         else
             list_g2t_hits(g2t_hit(i).id)=i
         endif   
      end do
      if(tte_track.eq.STAFCV_BAD) return

c
c     set up an index table to address a reconstructed hit by its hit id
c     but first clean up the list
      call vzero(list_hits,hit_size)
      do i=1,hit_h.nok
         if(hit(i).id.gt.list_size.or.hit(i).id.lt.1) then
             tte_track=STAFCV_BAD
             write(6,*)'list_size not big enough ','i = ', i,
     >      'list_size = ', list_size, 'hit(i).id = ', hit(i).id
         else
             list_hits(hit(i).id)=i
         endif
      end do 
      if(tte_track.eq.STAFCV_BAD) return
c
c     set up an index table to address mc tracks in g2t_track table by the 
c     track id
      call vzero(list_g2t_track,track_size)
      do i=1,g2t_track_h.nok
         if(g2t_track(i).id.gt.track_size.or.g2t_track(i).id.lt.1) then
             tte_track=STAFCV_BAD
             write(6,*) 'track_size not big enough, i = ',i,
     >       ' track_size = ',track_size,' g2t_track(i).id = ',
     >        g2t_track(i).id
              return
         endif
         list_g2t_track(g2t_track(i).id)=i
      end do
c
C     sort the index table according to the type key
      iret = tls_Quick_Sort_i(tindex_h.nok,tindex(1).type,
     >       tindex(2).type,tindex)
c
c     find where the first mc_hit <-> real hit assignement is and
c     count them

      ng2t=0
      do i=1,tindex_h.nok
         if(tindex(i).type.eq.tindex_type(1).tphit_mhitstpc) then
            ng2t=ng2t+1
            if(ng2t.eq.1) istart = i
         endif
      end do
c     write(6,*) 'number of mc hits used to create tfs hits = ',ng2t
c
c     sort on the mc hit id (key1)
      iret = tls_Quick_Sort_i(ng2t,tindex(istart).key1,
     >       tindex(istart+1).key1,tindex(istart))
c
c
c     set up a list so the monte carlo hit can be found in the
C     translation table, clean up and zero the list_index first
      call vzero(list_index,list_size)
      do i=istart,istart+ng2t-1
        list_index(tindex(i).key1)=i
      end do 

c     loop over all the mc tracks and start to fill the
c     reconstructed track table

c
      evmc_nok=0
      do i=1,g2t_track_h.nok
*     skip  if there is no hits on this track
      if(g2t_track(i).n_tpc_hit.ne.0) then

*          find a pointer to the first hit on this track
           ip = list_g2t_hits(g2t_track(i).hit_tpc_p)
           ip_first = 0
           id_first_real = 0
           tof_first = 1000000.0
           long_list_nok=0
           do while (ip.ne.0)
c          skip "extra points that are generated for tss only
           if(g2t_hit(ip).volume_id.le.2445) then
c            find out whether this point was used in a reconstructed hit
             index_mc=list_index(g2t_hit(ip).id)
             if(index_mc.ne.0) then

c            this point was used to create a hit
c            but it could have been deleted later on in the process of
c            merging and filtering.....
c            so check whether we have this id on a list of reconstructed
c            hits.................
             if(list_hits(tindex(index_mc).key2).gt.0) then
                if(tindex(index_mc).key2.gt.hit_size.or.
     >             tindex(index_mc).key2.lt.1) then
                   tte_track=STAFCV_BAD
                   write(6,*) 'long_list not long enough, hit_size = ',
     >             hit_size, ' tindex(index_mc).key2 = ',
     >             tindex(index_mc).key2
                   return
                endif

c               find whether this hit is on our short list
                iloc=0
                do ii=1,long_list_nok
                   if(short_list(ii).eq.tindex(index_mc).key2)iloc=ii
                end do
c               check whether our short list is long enough.....
c               if yes, take the hit, if not, just drop it.....
                if(long_list_nok.lt.len_short_list) then
                   if(iloc.eq.0)then
                     long_list_nok=long_list_nok+1
c                    memorize the id of a reconstructed hit
                     short_list(long_list_nok)=tindex(index_mc).key2
                     long_list(tindex(index_mc).key2)=0
c                    store-info about the tof
                     tl(long_list_nok).tof=g2t_hit(ip).tof
                     tl(long_list_nok).ipr=tindex(index_mc).key2
                     tl(long_list_nok).ipmc=ip
c                    check whether this could be our "first" hit
                     if(tof_first.gt.g2t_hit(ip).tof) then
                       ip_first=ip
                       tof_first=g2t_hit(ip).tof
                       id_first_real=tindex(index_mc).key2
                     endif
                   endif
                   long_list(tindex(index_mc).key2)=
     >             long_list(tindex(index_mc).key2)+1
                endif
c             else
c                write(6,*) 'this hit was dropped',
c     >          tindex(index_mc).key2
             endif
             endif
           endif
           ip= g2t_hit(ip).next_tr_hit_p
           if(ip.ne.0) ip=list_g2t_hits(ip)
c          went through all the points for 1 track
           end do
c          sort the time of flight info
           iret = tls_Quick_Sort_r(long_list_nok,tl(1).tof,
     >            tl(2).tof,tl)
c
c          go through all the points on the long_list list
           if(track_h.maxlen.gt.track_size) then
               write(6,*) 'track_size not big enough', track_size,
     >         track_h.maxlen 
               track_h.maxlen=track_size
           endif
c
           if(long_list_nok.gt.0) then
             evmc_nok=evmc_nok+1
             if(evmc_nok.gt.track_h.maxlen) then
                write(6,*) ' track array not big enough....'
                track_h.nok=evmc_nok-1
                tte_track=STAFCV_BAD
                return
             endif
             do j=1,long_list_nok
                ip_real=list_hits(tl(j).ipr)
c               assign this point to a track
                hit(ip_real).track = evmc_nok*1000+long_list_nok+1-j
                if(j.gt.1.and.tl(j).tof.eq.tl(1).tof) 
     >          write(63,*) evmc_nok,tl(j).tof
             end do
             track(evmc_nok).id=evmc_nok
             track(evmc_nok).flag=1
             track(evmc_nok).hitid=id_first_real
             track(evmc_nok).nfit=long_list_nok
             track(evmc_nok).nrec=long_list_nok
             track(evmc_nok).q=g2t_track(i).charge
             track(evmc_nok).chisq(1)=1.0
             track(evmc_nok).chisq(2)=1.0
             track(evmc_nok).invp=1.0/sqrt(g2t_hit(ip_first).p(1)**2+
     >                          g2t_hit(ip_first).p(2)**2)
             track(evmc_nok).phi0=atan2(g2t_hit(ip_first).x(2),
     >                    g2t_hit(ip_first).x(1))*57.2957795 
c             if(track(evmc_nok).phi0.lt.0.0)
c     >       track(evmc_nok).phi0 = track(evmc_nok).phi0 + 360.0
             track(evmc_nok).psi=atan2(g2t_hit(ip_first).p(2),
     >                               g2t_hit(ip_first).p(1))*57.2957795
c             if(track(evmc_nok).psi.lt.0)
c     >       track(evmc_nok).psi=track(evmc_nok).psi + 360.0
             track(evmc_nok).r0=sqrt(g2t_hit(ip_first).x(2)**2+
     >                             g2t_hit(ip_first).x(1)**2)
             track(evmc_nok).tanl=g2t_hit(ip_first).p(3)*
     >                          track(evmc_nok).invp
             track(evmc_nok).z0=g2t_hit(ip_first).x(3)
          endif
      endif
      end do

      track_h.nok=evmc_nok
      tte_track=STAFCV_OK
999   continue
      end
