* $Id: tte.F,v 1.6 1998/03/19 01:06:14 sakrejda Exp $
* $Log: tte.F,v $
* Revision 1.6  1998/03/19 01:06:14  sakrejda
* float added in the qfact calculation thanks to Joachim Nystrand
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

#include "tpt_const.inc"

#include "tte.inc"
#include "math_constants.inc"

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
      integer   tls_Quick_Sort_i              !integer sort (tls pkg)
      integer   tls_Index_Sort_i              !integer indexed sort

      integer   list_size                     !size of the list array
      integer   list_size                     !size of the list arrays used 
      integer   list(list_size)               !list used to sort the index tb
      integer   list_index(list_size)
*                                             !address mc hit in the index tb
      integer   list_index_mc(list_size)      !address mc hit in tindex
      integer   list_g2t_hits(list_size)      !mc hit address in g2t_tpc_hit
      integer   list_index_rec(list_size)     !address rec hits in tindex
      integer   hit_size
c                                              there are always less 
*                                             
c                                              reconstructed hits so it saves
      integer   loc_hit(hit_size)             !array for the indexed sort
      integer   long_list(hit_size)           !list of rec hits for 1 mc track
      integer   long_list_nok
      integer   len_short_list
      parameter (len_short_list=2000)
      integer   short_list(len_short_list)    !list of real_id's assigned
      real      dedx_list(len_short_list)
c     first element creates list of real_id's assigned to a monte carlo track
c     the second counts how many mc hits went into this reconstructed hit.
      real      dedx_list(max_track_length)  
      integer   long_track(track_size)    !list of rec tracks for 1 mc track
      integer   long_track_nok            !count of rec tracks mixed in 1 mc
                                          !and later a count of mc tracks 
                                          !mixed in real track
      integer   list_tracks(track_size)      !address 
      integer   list_g2t_tracks(track_size)  !address by id for mc tracks
      integer   short_track(maxtrack)     !compact list of reconstructed tracks
      integer   list_trk(track_size)      !rec track address in tptrack
      integer   list_g2t_track(track_size)
      integer   rec_track_on_mc_track(2,maxtrack)    
c     compact list of reconstructed tracks

*                                             !track address in g2t_track
      integer   i,j                           !loop index
      integer   list_track(maxtrack)          !list of mixed up tracks
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
      character CHPart*20
      real      Ub(10), Amass, Charge, Tlife
      integer   Ipart, ITRTyp ,Nb
      integer   track_num, track_num_local
      integer   id_mc, ip_mc, mc_track
      real      twopi,degtorad

      integer   my_charge(100)

      integer  ndrop
      integer  bad_track
      data my_charge /0,1,-1,0,1,-1,0,1,-1,0,1,-1,0,1,-1,0,0,0,1,0,
     >                -1,0,-1,-1,0,0,-1,0,1,0,1,1,0,0,0,0,0,0,0,0,0,
     >                 0,0,0,1,1,2,0,2,0,50*0/

      twopi=atan(1.0)*8.0
      degtorad=atan(1.0)/45.0

c     set number of entries in the tables to 0

      eval_h.nok=0
      evmc_h.nok=0
      
c     set up an index table to address an mc hit by its hit id
c     but first clean up the list
c
      call vzero(list_g2t_hits,list_size)
C
             write(6,*) 'list_size not big enough ','i = ', i,
     >      'list_size = ', list_size, 'g2t_hit(i).id = ', g2t_hit(i).id
     >                         ,g2t_hit(i).id
             call Message (m132, 2, idI1 )
             return
         else
      if(tte.eq.STAFCV_BAD) return

             list_g2t_hits(g2t_hit(i).id)=i
         endif   
      end do
c
c     set up an index table to address a reconstructed hit by its hit id
c     but first clean up the list
      call vzero(list_hits,hit_size)
             write(6,*)'list_size not big enough ','i = ', i,
     >      'list_size = ', list_size, 'hit(i).id = ', hit(i).id
     >                         hit(i).id
             call Message (m132, 2, idI2 )
             return
         else
      if(tte.eq.STAFCV_BAD) return
         endif
      end do 

      call vzero(list_g2t_track,track_size)
c     set up an index table to address mc tracks in g2t_track table by the 
c     track id
      call vzero(list_g2t_tracks,track_size)
             write(6,*) 'track_size not big enough, i = ',i,
     >       ' track_size = ',track_size,' g2t_track(i).id = ',
     >        g2t_track(i).id
              return
     >                        ' g2t_track(i).id = ',g2t_track(i).id
         list_g2t_track(g2t_track(i).id)=i
             return
         endif
         list_g2t_tracks(g2t_track(i).id)=i
      end do
      call vzero(list_trk,track_size)
c     set up an index table to address rec tracks by the rec track id
c     
      call vzero(list_tracks,track_size)
             write(6,*) 'track_size not big enough, i = ',i,
     >       ' list_size = ',list_size,' track(i).id = ',
     >        track(i).id
              return
     >                        track(i).id
        list_trk(track(i).id)=i 
             return
         endif
         if(track(i).flag.gt.0) list_tracks(track(i).id)=i 
      iret = tls_Quick_Sort_i(tindex_h.nok,tindex(1).type,
    
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
         endif
      end do

      iret = tls_Quick_Sort_i(ng2t,tindex(istart).key1,
c
c     sort on the mc hit id (key1)
      iret = tls_qsort_herb_i(ng2t,tindex(istart).key1,
     >       tindex(istart+1).key1,tindex)
C     translation table, clean up and zero the list_index first
      call vzero(list_index,list_size)
c     set up a list so the monte carlo hit can be found in the
        list_index(tindex(i).key1)=i
      call vzero(list_index_mc,list_size)
      do i=istart,istart+ng2t-1
        list_index_mc(tindex(i).key1)=i
      end do 

c     loop over all the mc tracks and start to fill the
c     evaluation table
      evmc_h.nok=0
c     let's count dropped mc hits
      ndrop=0
      call vzero(dedx_list,len_short_list)
      do i=1,g2t_track_h.nok
*     at this time we can also calculate a dedx for the track
      if(g2t_track(i).n_tpc_hit.ne.0) then
           evmc(evmc_h.nok).mcid=g2t_track(i).id
           evmc(evmc_h.nok).nfst=0
           evmc(evmc_h.nok).nfstm=0
           evmc(evmc_h.nok).ngnt=0
           evmc(evmc_h.nok).pzvr =  0.0
		write(6,*) 'hit_size not big enough',hit_h.maxlen,hit_size
		write(m132(1),*) 'TTE-I5 hit_size not big enough'
                write(m132(2),*) hit_h.maxlen,hit_size
                call Message (m132, 2, idI5 )
                tte = STAFCV_BAD
*          find a pointer to the first hit
           endif
           long_list_nok=0
*          find a pointer to the first mc hit for this track
c          skip "extra points that are generated for tss only
           if(g2t_hit(ip).volume_id.le.2445.and.
c          ............ or if there are too many points on a track
     >     len_short_list.gt.evmc(evmc_h.nok).ngnt) then
c          use only points from real (not "shadow") padrows
c            store the dedx infor for the hit
c            add this point
             evmc(evmc_h.nok).ngnt=evmc(evmc_h.nok).ngnt+1
c            store the dedx information for the hit
             index_mc=list_index(g2t_hit(ip).id)
c            find out whether this point was used in a reconstructed hit

c            this point was used to create a hit
c            but it could have been deleted later on in the process of
c            merging and filtering.....
c            so check whether we have this id on a list of reconstructed
c            hits.................
             if(list_hits(tindex(index_mc).key2).gt.0) then
                if(tindex(index_mc).key2.gt.hit_size.or.
c              hits
               if(list_hits(tindex(index_mc).key2).gt.0) then
                   write(6,*) 'long_list not long enough, hit_size = ',
     >             hit_size, ' tindex(index_mc).key2 = ',
                   write(m132(1),*) 'TTE-I6 long_list not long enough,'
                   write(m132(2),*) hit_size, 'tindex(index_mc).key2 =',
                endif
c               find whether this hit is on our short list
                iloc=0
                do ii=1,long_list_nok
                   if(short_list(ii).eq.tindex(index_mc).key2)iloc=ii
                end do
                if(len_short_list.gt.long_list_nok) then
                if(iloc.eq.0) then
                   long_list_nok=long_list_nok+1
                 if(max_track_length.gt.rec_hit_on_mc_track_nok) then
                   short_list(long_list_nok)=tindex(index_mc).key2
                   long_list(tindex(index_mc).key2)=0
                endif
                long_list(tindex(index_mc).key2)=
     >          long_list(tindex(index_mc).key2)+1
                endif
c             else
c                write(6,*) 'this hit was dropped',
c     >          tindex(index_mc).key2
             endif
*                 call Message (m132, 1, idI10 )
                  ndrop = ndrop+1
               endif
c          and make sure there is room in the track array
c          if not, terminate the loop
c          went through all the points for 1 track
           end do
c
c          went through all the points mc for 1 track
           do ie=1,isubtract
              ibig=0
              dedxbig=0.0
              do il=1,evmc(evmc_h.nok).ngnt
               ibig=0
                  dedxbig= dedx_list(il)
                  ibig=il
                  if(dedx_list(il).gt.dedxbig) then
              end do
              dedx_list(ibig)=0.0
           end do
           dedxsum=0.0
           do ie=1,evmc(evmc_h.nok).ngnt
              dedxsum=dedxsum+dedx_list(ie)
           end do
             do ie=1,evmc(evmc_h.nok).ngnt
               dedxsum=dedxsum+dedx_list(ie)
             end do
             evmc(evmc_h.nok).dedxg=
     >       dedxsum/real(evmc(evmc_h.nok).ngnt-isubtract)
           else
             evmc(evmc_h.nok).dedxg=-1.0 
           endif
               write(6,*) 'track_size not big enough', track_size,
     >         track_h.maxlen 
               write(m132(1),*) 'TTE-I7 track_size not big enough', 
     >         track_size, track_h.maxlen 
               call Message (m132, 1, idI7 )
               track_h.maxlen=track_size
           long_track_nok = 0
           do j=1,long_list_nok
              ip_real=list_hits(short_list(j))
           rec_track_on_mc_track_nok = 0
              ip_real=list_hits(rec_hit_on_mc_track(1,j))
              if(long_list(short_list(j)).gt.1) evmc(evmc_h.nok).nfstm=

              if(rec_hit_on_mc_track(2,j).gt.1) evmc(evmc_h.nok).nfstm=
     >        evmc(evmc_h.nok).nfstm+1

                 write(6,*) 'track_size not big enough', 
              id_track=hit(ip_real).track/1000
                 write(m132(1),*) 'TTE-I8 track_size not big enough', 
     >           hit(ip_real).track, ip_real
                 call Message (m132, 1, idI8 )
              if(id_track.ne.0) then
c                check whether we have it on our short_track list
c
                 do ii=1,long_track_nok
                     if(short_track(ii).eq.id_track) iloc=ii
                 iloc=0
                 do ii=1,rec_track_on_mc_track_nok
                    if(long_track_nok.lt.maxtrack)then
                       long_track_nok=long_track_nok+1
                       short_track(long_track_nok)=id_track
                       long_track(id_track)=0
     >                =id_track
                      rec_track_on_mc_track(2,rec_track_on_mc_track_nok)
                 long_track(id_track)=long_track(id_track)+1
                 endif
                 rec_track_on_mc_track(2,rec_track_on_mc_track_nok)=
     >           rec_track_on_mc_track(2,rec_track_on_mc_track_nok)+1
              endif
           evmc(evmc_h.nok).nseg=long_track_nok
c          went through all the reconstructed hits, now it's time to deal
c          with tracks
           evmc(evmc_h.nok).nseg=rec_track_on_mc_track_nok
           do j=1,long_track_nok
             it=short_track(j)
             if(long_track(it).gt.evmc(evmc_h.nok).nrec1) then
             it=rec_track_on_mc_track(1,j)
             if(rec_track_on_mc_track(2,j).gt.
                evmc(evmc_h.nok).nrec1=long_track(it)
                if(evmc(evmc_h.nok).nrec1.gt.evmc(evmc_h.nok).nrec2) 
             else if(long_track(it).gt.evmc(evmc_h.nok).nrec2) then
                evmc(evmc_h.nok).nrec2=long_track(it)
             else if(rec_track_on_mc_track(2,j).gt.
     >       evmc(evmc_h.nok).nrec2) then
                evmc(evmc_h.nok).nrec2=rec_track_on_mc_track(2,j)
             endif
      write(m132(1),*) 'TTE-I11 bad track total', bad_track
      call Message (m132, 1, idI11 )

         it=list_g2t_track(evmc(i).mcid)
c     go through the evaluation table and fill it with the mc information
         evmc(i).vid=g2t_track(it).start_vertex_p
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
            it=list_trk(evmc(i).recid)
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
         endif
      end do
      iret = tls_Quick_Sort_i(ng2t,tindex(istart).key2,
c
c     sort on the index table on reconstructed hit id (key2)
      iret = tls_qsort_herb_i(ng2t,tindex(istart).key2,
     >       tindex(istart+1).key2,tindex)
c     translation table, clean up and zero the list_index first
      call vzero(list_index,list_size)
c     set up a list so the monte carlo hit can be found in the
        list_index(tindex(i).key2)=i
     >    tindex(i).key2                     
          call Message(m132, 1, idI12)
        endif
      end do 

C     Sort the hit array according to the track number
      
      iret = tls_Index_Sort_i(hit_h.nok,hit(1).track,
     >       hit(2).track,loc_hit,
     >       hit_h.maxlen)
        
c     to the mc tracks
c     skip hits that do not belong to any track
      if(hit(loc_hit(i)).track.ne.0) then
c        get the track number
         track_num=int(hit(loc_hit(i)).track/1000)
         track_num_local=track_num
c        loop through all the points until this track is done
         eval_h.nok=eval_h.nok+1
         eval(eval_h.nok).rtrk=track_num
c        fill the evaluation table with the info about this track:
         eval(eval_h.nok).nfit=track(list_trk(track_num)).nfit
         eval(eval_h.nok).nrec=track(list_trk(track_num)).nrec
         eval(eval_h.nok).inhit=track(list_trk(track_num)).hitid
c        find a mc hit associated with this reconstructed hit
c        and get it's momentum information.
         id_mc=tindex(list_index(eval(eval_h.nok).inhit)).key1
         ip_mc=list_g2t_hits(id_mc)
         eval(eval_h.nok).pxg=g2t_hit(ip_mc).p(1)
         eval(eval_h.nok).pyg=g2t_hit(ip_mc).p(2)
         eval(eval_h.nok).pzg=g2t_hit(ip_mc).p(3)
         eval(eval_h.nok).ptg=sqrt(g2t_hit(ip_mc).p(1)**2+
     >                             g2t_hit(ip_mc).p(2)**2)
         eval(eval_h.nok).qr=track(list_trk(track_num)).q
         eval(eval_h.nok).ptr=1.0/track(list_trk(track_num)).invp
         eval(eval_h.nok).ptvr=1.0/track(list_trk(track_num)).invp
         eval(eval_h.nok).chisq(1)=track(list_trk(track_num)).chisq(1)
         eval(eval_h.nok).chisq(2)=track(list_trk(track_num)).chisq(2)
         eval(eval_h.nok).pxr=eval(eval_h.nok).ptr*
     >   cos(track(list_trk(track_num)).psi*dtorad)
         eval(eval_h.nok).pyr=eval(eval_h.nok).ptr*
     >   sin(track(list_trk(track_num)).psi*dtorad)
         eval(eval_h.nok).pzr=eval(eval_h.nok).ptr*
     >                       track(list_trk(track_num)). tanl
         eval(eval_h.nok).pzvr=eval(eval_h.nok).ptr*
     >                       track(list_trk(track_num)).tanl
*        try to get the closest approach:
*        first get the track radius
*        hrot is the sence of rotation
         hrot=-track(list_trk(track_num)).q
         radius=1.0/(track(list_trk(track_num)).invp*0.5
     >               *0.00299792458)
         psic=degtorad*track(list_trk(track_num)).psi-
     >   hrot*0.25*twopi
         if(psic.gt.twopi) psic=psic-twopi
         if(psic.lt.0)     psic=psic+twopi
         x0=track(list_trk(track_num)).r0*
     >   cos(degtorad*track(list_trk(track_num)).phi0) 
         y0=track(list_trk(track_num)).r0*
     >   sin(degtorad*track(list_trk(track_num)).phi0)       
         xc=x0 - radius*cos(psic)
         yc=y0 - radius*sin(psic)
         eval(eval_h.nok).rdca=radius-sqrt(xc**2+yc**2)
         phis=atan2(yc,xc)
         if(phis.lt.0) phis=phis+twopi

         xs=eval(eval_h.nok).rdca*cos(phis)
         ys=eval(eval_h.nok).rdca*sin(phis)
         psisc=atan2(ys-yc,xs-xc)
         if(psisc.lt.0)
     >   psisc=psisc+twopi

         eval(eval_h.nok).zdca=track(list_trk(track_num)).z0+
     >   hrot*radius*(psisc-psic)*
     >   track(list_trk(track_num)).tanl 
         psis=atan2(ys-yc,xs-xc)+hrot*0.25*twopi
         if(psis.gt.twopi) psis=psis-twopi
         eval(eval_h.nok).pxvr=eval(eval_h.nok).ptr*cos(psis)
         eval(eval_h.nok).pyvr=eval(eval_h.nok).ptr*sin(psis)
*        set to 0 all the info that is not filled so far
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
         long_track_nok=0
             eval(eval_h.nok).pullmx(2)=0.0
             j=i
             rec_track_on_mc_track_nok=0

         do while(track_num_local.eq.track_num)
c          find a mc track associated with this hit
c          to do it take a real hit id
           id_mc=tindex(list_index(ip)).key1
c          look up its position in the index table and take
c          the mc hit id from this location
           id_mc=tindex(list_index_rec(ip)).key1
c          find a pointer for this id
           ip_mc=list_g2t_hits(id_mc)
c          find a parent track for this point
           do ii=1,long_track_nok
              if(short_track(ii).eq.mc_track) iloc=ii
           iloc=0
           do ii=1,rec_track_on_mc_track_nok
             if(long_track_nok.lt.maxtrack)then
                long_track_nok=long_track_nok+1
                short_track(long_track_nok)=mc_track
                long_track(mc_track)=0
                rec_track_on_mc_track(1,rec_track_on_mc_track_nok)=
     >          mc_track
           long_track(mc_track)=long_track(mc_track)+1
           endif
           rec_track_on_mc_track(2,rec_track_on_mc_track_nok)=
     >     rec_track_on_mc_track(2,rec_track_on_mc_track_nok)+1
           j=j+1
           track_num_local=hit(loc_hit(j)).track/1000
         end do

         do ii=1,long_track_nok
            if(long_track(short_track(ii)).gt.ibig) then
              ibig=long_track(short_track(ii))
         do ii=1,rec_track_on_mc_track_nok
            if(rec_track_on_mc_track(2,ii).gt.ibig) then
              ibig=rec_track_on_mc_track(2,ii)
         eval(eval_h.nok).mtrk=short_track(iloc)
            endif
         ii=list_g2t_track(mc_track)
         eval(eval_h.nok).mtrk=rec_track_on_mc_track(1,iloc)
         eval(eval_h.nok).ngood=ibig
         ii=list_g2t_tracks(mc_track)
         eval(eval_h.nok).pxvg=g2t_track(ii).p(1)
         eval(eval_h.nok).pyvg=g2t_track(ii).p(2)
         eval(eval_h.nok).pzvg=g2t_track(ii).p(3)
         eval(eval_h.nok).ptvg=sqrt(g2t_track(ii).p(1)**2+
     >                              g2t_track(ii).p(2)**2)
         Ipart=g2t_track(ii).ge_pid
c         write(6,*) 'particle_id', ipart
c         call GFPART(Ipart,CHPart,ITRTyp,Amass,Charge,Tlife,Ub,Nb)
         if(ipart.le.100) then
              eval(eval_h.nok).qg=my_charge(ipart)
         eval(eval_h.nok).pid=g2t_track(ii).ge_pid
              write(6,*) 'pid too big'
         else
              write(m132(1),*) 'TTE-I9 pid too big'
      else
              return
         endif
         else
     >   'TTE-I14 hit assigned to a bad reconstructed track', bad_track
         call message (m132, 1 ,idI14)
      endif
      tte=STAFCV_OK
999   continue
      end
