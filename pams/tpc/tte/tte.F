* $Id: tte.F,v 1.18 1998/08/20 21:27:27 sakrejda Exp $
* $Log: tte.F,v $
* Revision 1.18  1998/08/20 21:27:27  sakrejda
* protection against tracks that cannot be associated with a mc track
*
* Revision 1.17  1998/07/22 18:31:23  sakrejda
* added check on mc hits that were not used to create reconstructed hits
*
* Revision 1.16  1998/07/10 20:45:55  sakrejda
* take care of cases where a reconstructed hit cannot be matched with a mc hit
*
* Revision 1.15  1998/07/09 12:47:31  sakrejda
* printing of warnings from tte reduced
*
* Revision 1.14  1998/07/02 19:20:17  sakrejda
* B field obtained via gufld call
*
* Revision 1.13  1998/06/05 19:02:28  sakrejda
* protection agains bad track numbers added
*
* Revision 1.12  1998/04/22 15:54:55  sakrejda
* estimate of purity for reconstructed tracks fixed
*
* Revision 1.11  1998/04/20 21:57:25  sakrejda
* count of reconstructed points per mc track corrected
*
* Revision 1.10  1998/04/17 10:48:04  didenko
*  _ is added to event_number
*
* Revision 1.9  1998/04/16 21:16:11  sakrejda
* primary vertex field of tte_mctrack updated following changes in gstar
*
* Revision 1.8  1998/04/14 23:24:48  sakrejda
* filling tte_mctrack structure thoroughly debugged
*
* Revision 1.7  1998/04/13 16:02:55  sakrejda
* calls to message added, protected against overflows, comments added,
* cleaned up - still needs work
*
* Revision 1.6  1998/03/19 01:06:14  sakrejda
* float added in the qfact calculation thanks to Joachim Nystrand
*
* Revision 1.5  1998/03/04 21:45:04  hardtke
* change pid in tte_eval to long and fill table entry
*
* Revision 1.4  1998/02/13 19:33:05  fisyak
* zerov => vzero
*
* Revision 1.3  1998/02/11 19:49:44  sakrejda
* tte.F charge retrieved from tptrack.q, previously it was attached tp invpt
*
* Revision 1.2  1998/01/27 02:18:51  fisyak
* Add README
*
C<----------------------------------------------------------------------------
      INTEGER FUNCTION tte(      track_h,       track,
     >                           hit_h,         hit,
     >                           g2t_hit_h,     g2t_hit,
     >                           g2t_track_h,   g2t_track,
     >                           tindex_h,      tindex,
     >                           tindex_type_h, tindex_type,
     >                           eval_h,        eval,
     >                           evmc_h,        evmc,
     >                           everes_h,      everes,
     >                           cntrl_h,       cntrl)

      IMPLICIT NONE

#include "tte.inc"
#include "math_constants.inc"
#include "phys_constants.inc"

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
C  evalh    - header of the tpeval (tpc evaluation) table
C  eval     - rows of the tpeval (tpc evaluation) table
C  evmch    - header of the tp_mctrk (mc tracks in TPC) table
C  evmc     - rows of the tp_mctrk (mc tracks in TPC) table
C  everesh  - header of the everes (fitting evaluation) table
C  everes   - rows of the everes table
C
C
C      Update history:
C      02/11/98 Noticed by Joachim nystrand
C      eval(i).ptvg was calculated twice, one of them removed
C      sign of particle is no longer attached to invpt
C      should be retrieved from tptrack.q
C      it was affecting sence of rotation in the momentum extrapolation 
C      and reconstructed charge entered into the evaluation tables
C_____________________________________________________________________________

      INTEGER   tls_Index_Sort_i              !integer sort (tls pkg)
      integer   tls_qsort_herb_i              !integer indexed sort

      integer   list_size                     !size of the list arrays used 
c                                              to address hits by id
c                                              in the index table, mc g2t_hit
      parameter (list_size=1000000)
      integer   list_index_mc(list_size)      !address mc hit in tindex
      integer   list_index_rec(list_size)     !address rec hits in tindex
      integer   list_g2t_hits(list_size)      !mc hit address in g2t_tpc_hit
      integer   ng2t                          !count of cross-indexed hits

      integer   hit_size                      !size of array used to index 
c                                              reconstructed hits
c                                              there are always less 
c                                              reconstructed hits so it saves
c                                              some space
      parameter (hit_size=500000)
      integer   list_hits(hit_size)           !rec. hits address in hit
      integer   loc_hit(hit_size)             !list of hits ordered by track
c                                              number
      integer   max_track_length
      parameter (max_track_length=2000)
      integer   rec_hit_on_mc_track_nok       !number of rec hits on a mc track
      integer   rec_hit_on_mc_track(2,max_track_length) 
c     first element creates list of real_id's assigned to a monte carlo track
c     the second counts how many mc hits went into this reconstructed hit.
      real      dedx_list(max_track_length)  
     
      integer   track_size
      parameter (track_size=100000)
      integer   list_tracks(track_size)      !address 
      integer   list_g2t_tracks(track_size)  !address by id for mc tracks

      integer   maxtrack                  !max # of mixed up tracks
      parameter (maxtrack=500)
      integer   rec_track_on_mc_track_nok
      integer   rec_track_on_mc_track(2,maxtrack)    
      integer   mc_track_on_rec_track_nok
      integer   mc_track_on_rec_track(2,maxtrack)    
c     compact list of reconstructed tracks

*                                             !track address in g2t_track
      integer   i,j                           !loop index
      integer   iret                          !function call status
      integer   ntrack                        !number of tracks in 1 mc track

      integer   ip                            !pointer to the next point on
c                                             !the mc track
      integer   index_mc                      !pointer to the mc hit in the
c                                             !index table
c
      integer   ip_real                       !pointer to the rec hit
      integer   id_track                      !id of a reconstructed track
      integer   it,iph                        !auxiliary integer...
      real      px,py                         !momentum components
      logical   more_points                   !control end of track
      integer   istart                        !beginning of the mc_hit
c                                              <-> real hit assignement
      integer   iloc,ii
      integer   track_num, track_num_local
      integer   id_mc, ip_mc, mc_track

      integer   my_charge(100)
      real      radius,hrot,xc,yc,psic,xs,ys,x0,y0,phis,psis,psisc
      integer   il, ie, ibig, isubtract
      real      dedxbig, dedxsum
      integer   event_number
      data      event_number /0/
      save      event_number

c     message related variables
      character*132 m132(10)                ! for messages
      integer idI1, idI2, idI3, idI4, idI5, idI6, idI7, idI8, idI9
      integer idI10, idI11, idI12, idI13, idI14
      data idI1, idI2, idI3, idI4, idI5, idI6, idI7, idI8, idI9 /9*0/
      data idI10, idI11, idI12, idI13, idI14 /5*0/
      save idI1, idI2, idI3, idI4, idI5, idI6, idI7, idI8, idI9
      save idI10, idI11, idI12, idI13, idI14

      integer  ndrop
      integer  bad_track
      data my_charge /0,1,-1,0,1,-1,0,1,-1,0,1,-1,0,1,-1,0,0,0,1,0,
     >                -1,0,-1,-1,0,0,-1,0,1,0,1,1,0,0,0,0,0,0,0,0,0,
     >                 0,0,0,1,1,2,0,2,0,50*0/

      real xlocal(3), bfield(3)
      data xlocal /3*0/

C_____________________________________________________________________________
      tte=STAFCV_OK
      call gufld(xlocal,bfield)
      call Msg_Enable( 'TTE-I1' )
      call Msg_Enable( 'TTE-I2' )
      call Msg_Enable( 'TTE-I3' )
      call Msg_Enable( 'TTE-I4' )
      call Msg_Enable( 'TTE-I5' )
      call Msg_Enable( 'TTE-I6' )
      call Msg_Enable( 'TTE-I7' )
      call Msg_Enable( 'TTE-I8' )
*      call Msg_Enable( 'TTEI9-E2' )
      call Msg_Enable( 'TTE-I10' )
      call Msg_Enable( 'TTE-I11' )
*      call Msg_Enable( 'TTEI12-E1' )
      call Msg_Enable( 'TTE-I13' )
      call Msg_Enable( 'TTE-I14' )
      event_number=event_number+1

      bad_track=0

c     set number of entries in the tables to 0

      eval_h.nok=0
      evmc_h.nok=0
      
c     set up an index table to address an mc hit by its hit id
c     but first clean up the list
c
      call vzero(list_g2t_hits,list_size)
C
      do i=1,g2t_hit_h.nok
         if(g2t_hit(i).id.gt.list_size.or.g2t_hit(i).id.lt.1) then
             tte=STAFCV_BAD
             write(m132(1),*) 'TTE-I1 list_size not big enough ','i= ',i
             write(m132(2),*) 'list_size = ',list_size,'g2t_hit(i).id = '
     >                         ,g2t_hit(i).id
             call Message (m132, 2, idI1 )
             return
         else
             list_g2t_hits(g2t_hit(i).id)=i
         endif   
      end do
c
c     set up an index table to address a reconstructed hit by its hit id
c     but first clean up the list
      call vzero(list_hits,hit_size)
      do i=1,hit_h.nok
         if(hit(i).id.gt.list_size.or.hit(i).id.lt.1) then
             tte=STAFCV_BAD
             write(m132(1),*) 'TTE-I2 list_size not big enough ','i= ',i
             write(m132(2),*) 'list_size = ', list_size, 'hit(i).id = ',
     >                         hit(i).id
             call Message (m132, 2, idI2 )
             return
         else
             list_hits(hit(i).id)=i
         endif
      end do 

c
c     set up an index table to address mc tracks in g2t_track table by the 
c     track id
      call vzero(list_g2t_tracks,track_size)
      do i=1,g2t_track_h.nok
         if(g2t_track(i).id.gt.track_size.or.g2t_track(i).id.lt.1) then
             tte=STAFCV_BAD
             write(m132(1),*) 'TTE-I3 track_size not big enough, i = ',i
             write(m132(2),*) 'track_size = ',track_size,
     >                        ' g2t_track(i).id = ',g2t_track(i).id
             call Message (m132, 2, idI3 )
             return
         endif
         list_g2t_tracks(g2t_track(i).id)=i
      end do
c
c     set up an index table to address rec tracks by the rec track id
c     
      call vzero(list_tracks,track_size)
      do i=1,track_h.nok
         if(track(i).id.gt.track_size.or.track(i).id.lt.1) then
             tte=STAFCV_BAD
             write(m132(1),*) 'TPT-I4 track_size not big enough, i = ',i
             write(m132(2),*) 'list_size = ',list_size,' track(i).id= ',
     >                        track(i).id
             call Message (m132, 2, idI4 )
             return
         endif
         if(track(i).flag.gt.0) list_tracks(track(i).id)=i 
      end do  
    
C     sort the index table according to the type key
      iret = tls_qsort_herb_i(tindex_h.nok,tindex(1).type,
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
      iret = tls_qsort_herb_i(ng2t,tindex(istart).key1,
     >       tindex(istart+1).key1,tindex)
c
c
c     set up a list so the monte carlo hit can be found in the
C     translation table, clean up and zero the list_index_mc first
      call vzero(list_index_mc,list_size)
      do i=istart,istart+ng2t-1
        list_index_mc(tindex(i).key1)=i
      end do 

c     loop over all the mc tracks and start to fill the
c     evaluation table

c
      evmc_h.nok=0
c     let's count dropped mc hits
      ndrop=0
      do i=1,g2t_track_h.nok
*     skip  if there is no hits on this track
*     at this time we can also calculate a dedx for the track
      if(g2t_track(i).n_tpc_hit.ne.0) then
          call vzero(dedx_list,max_track_length)
           evmc_h.nok=evmc_h.nok+1
           ntrack=0
           evmc(evmc_h.nok).evnum=  event_number
           evmc(evmc_h.nok).mcid=   g2t_track(i).id
           evmc(evmc_h.nok).nfst=   0
           evmc(evmc_h.nok).nfstm=  0
           evmc(evmc_h.nok).ngnt=   0
           evmc(evmc_h.nok).ptr =   0.0
           evmc(evmc_h.nok).ptvr=   0.0
           evmc(evmc_h.nok).pzr =   0.0
           evmc(evmc_h.nok).pzvr =  0.0
           evmc(evmc_h.nok).dedxr = 0.0
           evmc(evmc_h.nok).qfact = 0.0
           if(hit_h.maxlen.gt.hit_size) then
		write(m132(1),*) 'TTE-I5 hit_size not big enough'
                write(m132(2),*) hit_h.maxlen,hit_size
                call Message (m132, 2, idI5 )
                tte = STAFCV_BAD
                return
           endif
c
*          find a pointer to the first mc hit for this track
           ip = list_g2t_hits(g2t_track(i).hit_tpc_p)
           rec_hit_on_mc_track_nok=0
           do while (ip.ne.0)
c          use only points from real (not "shadow") padrows
           if(g2t_hit(ip).volume_id.le.2445 ) then
c            add this point
             evmc(evmc_h.nok).ngnt=evmc(evmc_h.nok).ngnt+1
c            store the dedx information for the hit
             dedx_list(evmc(evmc_h.nok).ngnt)=
     >                 abs(g2t_hit(ip).de)/g2t_hit(ip).ds
c            find out whether this point was used in a reconstructed hit
c            find it's location in the index table
             index_mc=list_index_mc(g2t_hit(ip).id)
             if(index_mc.ne.0) then
c            this point was used to create a hit
c            it could be that hit simulation was not ran for that sector
             if(tindex(index_mc).key2.gt.0) then
c              but it could have been deleted later on in the process of
c              merging and filtering.....
c              so check whether we have this id on a list of reconstructed
c              hits
               if(list_hits(tindex(index_mc).key2).gt.0) then
                 if(tindex(index_mc).key2.gt.hit_size) then
                   tte=STAFCV_BAD
                   write(m132(1),*) 'TTE-I6 long_list not long enough,'
     >                              //'hit_size ='
                   write(m132(2),*) hit_size, 'tindex(index_mc).key2 =',
     >             tindex(index_mc).key2
                   call Message (m132, 2, idI6 )
                   return
                 endif
c                find whether this hit is on our short list
                 iloc=0
                 do ii=1,rec_hit_on_mc_track_nok
                   if(rec_hit_on_mc_track(1,ii).eq.
     >                tindex(index_mc).key2)iloc=ii
                 end do
                 if(max_track_length.gt.rec_hit_on_mc_track_nok) then
                 if(iloc.eq.0) then
                   rec_hit_on_mc_track_nok=rec_hit_on_mc_track_nok+1
                   iloc=rec_hit_on_mc_track_nok
c                  memorize the id of a reconstructed hit
                   rec_hit_on_mc_track(1,iloc)=tindex(index_mc).key2
                   rec_hit_on_mc_track(2,iloc)=0

                 endif
                 rec_hit_on_mc_track(2,iloc)=
     >           rec_hit_on_mc_track(2,iloc)+1
                 endif
               else
*                 write(m132(1),*) 'TTE-I10 this hit was dropped',
*     >           tindex(index_mc).key2
*                 call Message (m132, 1, idI10 )
                  ndrop = ndrop+1
               endif
             endif
             endif
           endif
           ip= g2t_hit(ip).next_tr_hit_p
c          and make sure there is room in the track array
c          if not, terminate the loop
           if(max_track_length.eq.evmc(evmc_h.nok).ngnt) ip=0
           if(ip.ne.0) ip=list_g2t_hits(ip)
           end do
c
c          went through all the points mc for 1 track
c          so now it's time to calculate dedx for this track
           isubtract=0.3*real(evmc(evmc_h.nok).ngnt)
           if(isubtract.gt.0) then
             do ie=1,isubtract
               ibig=0
               dedxbig=0.0
               do il=1,evmc(evmc_h.nok).ngnt
                  if(dedx_list(il).gt.dedxbig) then
                    dedxbig= dedx_list(il)
                    ibig=il
                  endif 
               end do
               dedx_list(ibig)=0.0
             end do
             dedxsum=0.0
             do ie=1,evmc(evmc_h.nok).ngnt
               dedxsum=dedxsum+dedx_list(ie)
             end do
             evmc(evmc_h.nok).dedxg=
     >       dedxsum/real(evmc(evmc_h.nok).ngnt-isubtract)
           else
             evmc(evmc_h.nok).dedxg=-1.0 
           endif

c          go through all the points on the long_list list
           if(track_h.maxlen.gt.track_size) then
               write(m132(1),*) 'TTE-I7 track_size not big enough', 
     >         track_size, track_h.maxlen 
               call Message (m132, 1, idI7 )
               track_h.maxlen=track_size
           endif
*
c
           rec_track_on_mc_track_nok = 0
           do j=1,rec_hit_on_mc_track_nok
              ip_real=list_hits(rec_hit_on_mc_track(1,j))
c             add a reconstructed hit

              evmc(evmc_h.nok).nfst=evmc(evmc_h.nok).nfst+1
              if(rec_hit_on_mc_track(2,j).gt.1) evmc(evmc_h.nok).nfstm=
     >        evmc(evmc_h.nok).nfstm+1

c             check whether this point was assigned to a track
              id_track=hit(ip_real).track/1000
              if(id_track.gt.track_size) then
                 write(m132(1),*) 'TTE-I8 track_size not big enough', 
     >           hit(ip_real).track, ip_real
                 call Message (m132, 1, idI8 )
                 tte=STAFCV_BAD
                 return
              endif
c
c             make sure id is non zero - possible problem with bad 
c             track numbering 
              if(id_track.gt.0) then
c             check whether it's flag is ok
              if(list_tracks(id_track).eq.0) then
c                bad track
c                 write(m132(1),*) 'TTE-I11 bad track', id_track
c                 call Message (m132, 1, idI11 )
                 bad_track=bad_track+1
              endif

c
              if(list_tracks(id_track).gt.0) then
c                check whether we have it on our rec_track_on_mc_track list
                 iloc=0
                 do ii=1,rec_track_on_mc_track_nok
                     if(rec_track_on_mc_track(1,ii).eq.id_track) iloc=ii
                 end do
                 if(iloc.eq.0) then
                    if(rec_track_on_mc_track_nok.lt.maxtrack)then
                      rec_track_on_mc_track_nok=
     >                rec_track_on_mc_track_nok+1
                      rec_track_on_mc_track(1,rec_track_on_mc_track_nok)
     >                =id_track
                      rec_track_on_mc_track(2,rec_track_on_mc_track_nok)
     >                =0
                      iloc=rec_track_on_mc_track_nok
                    endif
                 endif
                 rec_track_on_mc_track(2,iloc)=
     >           rec_track_on_mc_track(2,iloc)+1
              endif
              endif
           end do
c          went through all the reconstructed hits, now it's time to deal
c          with tracks
           evmc(evmc_h.nok).nseg=0
           evmc(evmc_h.nok).nrec1=0
           evmc(evmc_h.nok).nrec2=0
           evmc(evmc_h.nok).recid=0
           do j=1,rec_track_on_mc_track_nok
             it=rec_track_on_mc_track(1,j)
c            make sure that this mc track is important in the reconstructed 
c            track
             if(real(rec_track_on_mc_track(2,j))/real(track(it).nrec)
     >       .gt.0.5) then
             evmc(evmc_h.nok).nseg=evmc(evmc_h.nok).nseg+1
             if(rec_track_on_mc_track(2,j).gt.
     >       evmc(evmc_h.nok).nrec1) then
                if(evmc(evmc_h.nok).nrec1.gt.evmc(evmc_h.nok).nrec2) 
     >          evmc(evmc_h.nok).nrec2=evmc(evmc_h.nok).nrec1
                evmc(evmc_h.nok).nrec1=rec_track_on_mc_track(2,j)
                evmc(evmc_h.nok).recid=it
             else if(rec_track_on_mc_track(2,j).gt.
     >       evmc(evmc_h.nok).nrec2) then
                evmc(evmc_h.nok).nrec2=rec_track_on_mc_track(2,j)
             endif
             endif
           end do
      endif
      end do
      write(m132(1),*) 'TTE-I10 reconstructed hits were dropped', ndrop
      call Message (m132, 1, idI10 )

      write(m132(1),*) 'TTE-I11 bad track total', bad_track
      call Message (m132, 1, idI11 )

c
c     go through the evaluation table and fill it with the mc information
      do i=1,evmc_h.nok
         it=list_g2t_tracks(evmc(i).mcid)
         evmc(i).pid=g2t_track(it).ge_pid
         if(g2t_track(it).eg_label.gt.0) then
            evmc(i).vid=1.0
         else
            evmc(i).vid=g2t_track(it).start_vertex_p
         endif
c
c        this should be replaced, themc hit here is not necessarily the
c        first hit in the chamber
         iph=list_g2t_hits(g2t_track(it).hit_tpc_p)
         px=g2t_hit(iph).p(1)
         py=g2t_hit(iph).p(2)
         evmc(i).ptg=sqrt(px**2+py**2)
         evmc(i).pzg=g2t_hit(iph).p(3)
         px=g2t_track(it).p(1)
         py=g2t_track(it).p(2)
         evmc(i).ptvg=sqrt(px**2+py**2)
         evmc(i).pzvg=g2t_track(it).p(3)
c        and the reconstructed information
c        provided the track was reconstructed........
         if(evmc(i).recid.gt.0) then
            it=list_tracks(evmc(i).recid)
            evmc(i).ptr = 1.0/track(it).invp
            evmc(i).ptvr=evmc(i).ptr
            evmc(i).pzr = evmc(i).ptr*track(it).tanl
            evmc(i).pzvr = evmc(i).pzr
c           and calculate the quality factor
            evmc(i).qfact = real(evmc(i).nrec1)/real(track(it).nrec)
c           add the dedx information
            evmc(i).dedxr = track(it).dedx(1)
         endif

      end do
c
c     now go over the mc tracks and check whether they share reconstructed 
c     track
      do i=1,evmc_h.nok-1
         if(evmc(i).nseg.gt.0) then
         do j=i+1,evmc_h.nok
            if(evmc(i).recid.eq.evmc(j).recid) then
               if(evmc(i).nrec1.le.evmc(j).nrec1) then
                   evmc(i).nseg = -abs(evmc(i).nseg)
               else
                   evmc(j).nseg = -abs(evmc(j).nseg)
               endif
            endif
         end do
         endif
      end do
c     done with the mc tracks now go to the real tracks
c
c     sort on the index table on reconstructed hit id (key2)
      iret = tls_qsort_herb_i(ng2t,tindex(istart).key2,
     >       tindex(istart+1).key2,tindex)
c
c
c     set up a list so the reconstructed hit can be found in the
c     translation table, clean up and zero the list_index_rec first
      call vzero(list_index_rec,list_size)
      do i=istart,istart+ng2t-1
        if(tindex(i).key2.gt.0.and.tindex(i).key2.lt.list_size) then
          list_index_rec(tindex(i).key2)=i
        else if(tindex(i).key2.eq.0) then
          write(m132,*)'TTEI12-E1 no reconstructed partner'
          call Message(m132, 1, idI12)
        else
          write(m132,*)'TTEI12-E1 list size not big enough', list_size,
     >    tindex(i).key2                     
          call Message(m132, 1, idI12)
        endif
      end do 

C     Sort the hit array according to the track number
      
      iret = tls_Index_Sort_i(hit_h.nok,hit(1).track,
     >       hit(2).track,loc_hit,
     >       hit_h.maxlen)
        
c     go through all the hits sorted by the track number and relate them
c     to the mc tracks
      i=1
      bad_track=0
      do while (i.le.hit_h.nok)
c        skip hits that do not belong to any track or to a bad track
         track_num=0
         if(hit(loc_hit(i)).track.gt.0)
     >   track_num=int(hit(loc_hit(i)).track/1000)
c        double-check, whether this is a good track, loop only through good tracks
         if(track_num.gt.0.and.list_tracks(track_num).eq.0) then
           track_num=0
           bad_track=bad_track+1
         endif
         if(track_num.gt.0) then 
           track_num_local=track_num
c          loop through all the points until this track is done
           eval_h.nok=eval_h.nok+1
           if(eval_h.nok.le.eval_h.maxlen) then
             eval(eval_h.nok).rtrk=track_num
c            fill the evaluation table with the info about this track:
             eval(eval_h.nok).nfit=track(list_tracks(track_num)).nfit
             eval(eval_h.nok).nrec=track(list_tracks(track_num)).nrec
             eval(eval_h.nok).inhit=track(list_tracks(track_num)).hitid
c            find a mc hit associated with this reconstructed hit
c            and get it's momentum information.
             id_mc=tindex(list_index_rec(eval(eval_h.nok).inhit)).key1
             if(id_mc.gt.0) then
               ip_mc=list_g2t_hits(id_mc)
               eval(eval_h.nok).pxg=g2t_hit(ip_mc).p(1)
               eval(eval_h.nok).pyg=g2t_hit(ip_mc).p(2)
               eval(eval_h.nok).pzg=g2t_hit(ip_mc).p(3)
               eval(eval_h.nok).ptg=sqrt(g2t_hit(ip_mc).p(1)**2+
     >         g2t_hit(ip_mc).p(2)**2)
             else
               eval(eval_h.nok).pxg=-999.0
               eval(eval_h.nok).pyg=-999.0
               eval(eval_h.nok).pzg=-999.0
               eval(eval_h.nok).ptg=-999.0
             endif
             eval(eval_h.nok).qr=track(list_tracks(track_num)).q
             eval(eval_h.nok).ptr=1.0/track(list_tracks(track_num)).invp
             eval(eval_h.nok).ptvr=1./track(list_tracks(track_num)).invp
             eval(eval_h.nok).chisq(1)=
     >       track(list_tracks(track_num)).chisq(1)
             eval(eval_h.nok).chisq(2)=
     >       track(list_tracks(track_num)).chisq(2)
             eval(eval_h.nok).pxr=eval(eval_h.nok).ptr*
     >       cos(track(list_tracks(track_num)).psi*C_RAD_PER_DEG)
             eval(eval_h.nok).pyr=eval(eval_h.nok).ptr*
     >       sin(track(list_tracks(track_num)).psi*C_RAD_PER_DEG)
             eval(eval_h.nok).pzr=eval(eval_h.nok).ptr*
     >       track(list_tracks(track_num)). tanl
             eval(eval_h.nok).pzvr=eval(eval_h.nok).ptr*
     >       track(list_tracks(track_num)).tanl
*            try to get the closest approach:
*            first get the track radius
*            hrot is the sence of rotation
             hrot=-track(list_tracks(track_num)).q
             radius=1.0/(track(list_tracks(track_num)).invp*bfield(3)
     >       *C_D_CURVATURE)
             psic=C_RAD_PER_DEG*track(list_tracks(track_num)).psi-
     >       hrot*0.25*C_2PI
             if(psic.gt.C_2PI) psic=psic-C_2PI
             if(psic.lt.0)     psic=psic+C_2PI
             x0=track(list_tracks(track_num)).r0*
     >       cos(C_RAD_PER_DEG*track(list_tracks(track_num)).phi0) 
             y0=track(list_tracks(track_num)).r0*
     >       sin(C_RAD_PER_DEG*track(list_tracks(track_num)).phi0)       
             xc=x0 - radius*cos(psic)
             yc=y0 - radius*sin(psic)
             eval(eval_h.nok).rdca=radius-sqrt(xc**2+yc**2)
             phis=atan2(yc,xc)
             if(phis.lt.0) phis=phis+C_2PI
             xs=eval(eval_h.nok).rdca*cos(phis)
             ys=eval(eval_h.nok).rdca*sin(phis)
             psisc=atan2(ys-yc,xs-xc)
             if(psisc.lt.0) psisc=psisc+C_2PI
             eval(eval_h.nok).zdca=track(list_tracks(track_num)).z0+
     >       hrot*radius*(psisc-psic)*
     >       track(list_tracks(track_num)).tanl 
             psis=atan2(ys-yc,xs-xc)+hrot*0.25*C_2PI
             if(psis.gt.C_2PI) psis=psis-C_2PI
             eval(eval_h.nok).pxvr=eval(eval_h.nok).ptr*cos(psis)
             eval(eval_h.nok).pyvr=eval(eval_h.nok).ptr*sin(psis)
*            set to 0 all the info that is not filled so far
             eval(eval_h.nok).abpullav(1)=0.0
             eval(eval_h.nok).abpullav(2)=0.0
             eval(eval_h.nok).dpt=0.0
             eval(eval_h.nok).dpx=0.0
             eval(eval_h.nok).dpy=0.0
             eval(eval_h.nok).dpz=0.0
             eval(eval_h.nok).pullav(1)=0.0
             eval(eval_h.nok).pullav(2)=0.0
             eval(eval_h.nok).pullmx(1)=0.0
             eval(eval_h.nok).pullmx(2)=0.0
             j=i
             mc_track_on_rec_track_nok=0

         do while(track_num_local.eq.track_num)
c          find a mc track associated with this hit
c          to do it take a real hit id
           ip=hit(loc_hit(j)).id
c          look up its position in the index table and take
c          the mc hit id from this location
           id_mc=tindex(list_index_rec(ip)).key1
           if(id_mc.gt.0) then
c            find a pointer for this id
             ip_mc=list_g2t_hits(id_mc)
c            find a parent track for this point
             mc_track=g2t_hit(ip_mc).track_p
c            let's see how many different tracks got mixed up in this one...
             iloc=0
             do ii=1,mc_track_on_rec_track_nok
                if(mc_track_on_rec_track(1,ii).eq.mc_track) iloc=ii
             end do
             if(iloc.eq.0) then
               if(mc_track_on_rec_track_nok.lt.maxtrack)then
                 mc_track_on_rec_track_nok=mc_track_on_rec_track_nok+1
                 mc_track_on_rec_track(1,mc_track_on_rec_track_nok)=
     >           mc_track
                 mc_track_on_rec_track(2,mc_track_on_rec_track_nok)=0
                 iloc=mc_track_on_rec_track_nok
               endif
             endif
             mc_track_on_rec_track(2,iloc)=
     >       mc_track_on_rec_track(2,iloc)+1
           endif
           j=j+1
           if(j.le.hit_h.nok) then
             track_num_local=hit(loc_hit(j)).track/1000
           else
             track_num_local=-1
           endif
         end do

c        let's find the biggest contribution
         if(mc_track_on_rec_track_nok.gt.0) then
         ibig=0
         iloc=1
         do ii=1,mc_track_on_rec_track_nok
            if(mc_track_on_rec_track(2,ii).gt.ibig) then
              ibig=mc_track_on_rec_track(2,ii)
              iloc=ii
            endif
         end do
         eval(eval_h.nok).mtrk=mc_track_on_rec_track(1,iloc)
         eval(eval_h.nok).ngood=ibig
         ii=list_g2t_tracks(mc_track)
         eval(eval_h.nok).pxvg=g2t_track(ii).p(1)
         eval(eval_h.nok).pyvg=g2t_track(ii).p(2)
         eval(eval_h.nok).pzvg=g2t_track(ii).p(3)
         eval(eval_h.nok).ptvg=sqrt(g2t_track(ii).p(1)**2+
     >                              g2t_track(ii).p(2)**2)
         eval(eval_h.nok).qfact=real(ibig)/real(eval(eval_h.nok).nrec)
         eval(eval_h.nok).vid=g2t_track(ii).start_vertex_p
         eval(eval_h.nok).pid=g2t_track(ii).ge_pid
         if(g2t_track(ii).ge_pid.le.100) then
              eval(eval_h.nok).qg=my_charge(g2t_track(ii).ge_pid)
         else
              write(m132(1),*) 'TTEI9-E2 pid too big',
     >        g2t_track(ii).ge_pid
              call Message (m132, 1, idI9 )
              eval(eval_h.nok).qg=0
         endif
         else
         eval(eval_h.nok).mtrk=-999
         eval(eval_h.nok).ngood=-999
         eval(eval_h.nok).pxvg=-999.0
         eval(eval_h.nok).pyvg=-999.0
         eval(eval_h.nok).pzvg=-999.0
         eval(eval_h.nok).ptvg=-999.0
         eval(eval_h.nok).qfact=-999.0
         eval(eval_h.nok).vid=-999.0
         eval(eval_h.nok).pid=-999.0
         eval(eval_h.nok).qg=-999.0
         endif
         i=j
         else
              write(m132(1),*) 'TTE-I13 eval not big enough'
              call Message (m132, 1, idI13 )
              tte=STAFCV_BAD
              return
         endif
         else
         i=i+1
         endif
      end do
      if(bad_track.gt.0) then
         write(m132(1),*) 
     >   'TTE-I14 hit assigned to a bad reconstructed track', bad_track
         call message (m132, 1 ,idI14)
      endif
      tte=STAFCV_OK
999   continue
      end
