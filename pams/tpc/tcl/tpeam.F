*23456789*123456789*123456789*123456789*123456789*123456789*123456789*12345678
c        1         2         3         4         5         6         7
C----------------------------------------------------------------------
C
C                               *
C                              ***
C                             ** **
C                            **   **
C                    *********     *********
C                      ****           ****
C                        ***         ***
C                        **     *     **
C                       **    *****    **
C                      **   **     **   **
C                     **                 **
C
C----------------------------------------------------------------------
C
      INTEGER FUNCTION TPEAM( 
     +                        tphit_h, tphit 
     +,                       g2t_track_h,g2t_track
     +,                       g2t_tpc_hit_h,g2t_tpc_hit
     +,                       tsspar_h, tsspar 
     +,			      tpcluster_h, tpcluster
     +,			      tpseq_h, tpseq
     +,			      tppixel_h, tppixel 
     +,			      tpmcpix_h, tpmcpix 
     +,			      pad_plane_h, pad_plane
     +,			      detector_h, detector,
     +	                      tclpar_h, tclpar,
     +                        tpc_index_h, tpc_index,
     +                        tpc_index_type_h, tpc_index_type)

      IMPLICIT NONE

C   Input arguments  (One to a line with definition after ! ) 
C
C     tphit_h:
C     tphit:
C
C   Output arguments : 
c
c	Output is the tpc_index table, used for correlating MC and
c	  reconstructed hits.
c
c	Also, the following ntuples used for diagnostics
c	  300+event# "Compare Info": MC and Rec hit positions, uncertainties
c	  400+event# "Track Info": Not really used
c	  500+event# "Hit Pixel Info": x,z,adc,cluster#,row for pixels
c	  600+event# "Local Hit Info": local x,z,hit#,cluster#,row for recon hits
c	  650+event# "Local MC Hit Info": local x,z,MCtrack#,row for MC hits
c
c       And the following histograms
c	  1100+event# "MC Neighbor Distance- Inner"
c	  1200+event# "MC Neighbor Distance- Outer"
c	  2100+event# "Rec Neighbor Distance- Inner"
c	  2200+event# "Rec Neighbor Distance- Outer"
c
C
C   Functional Description : 
C
C   evaluation module converting cluster data into an ntuple...
C
C   Moved into TAS 15-Apr-1994   Richard Morse
C   Copyright 1994    Lawrence Berkeley Laboratory
c   
c   modificattions:
c   ==============
C
C   1 Dec MA Lisa
C	- added ntuple 500+event# to show pixel information for each hit
C
C   14Dec MA Lisa
C	- account for the fact that # Geant volumes over each row
C	  may vary
C	  through use of numbv_row array
C
C   24mar95 malisa
C       - now unpack data in the new way
C	  datum = adc+'1000'X*tdc+'1000000'X*numseq
c
c   8mar96 malisa
c       - code upgraded to interface with new geant scheme (gstar)
c         as well as new geometry routines tgc.
c       - old code (that works with mct and tpg) is at tpeam.F.pre_gstar
C
c   23may96 malisa
c	- now fill pixel visualization ntuples even when we deal with
c	  real data (requires reading tclpar to know if it is a MC event or
c	  not) So need fill ntuples 300,500,600
c
C   Error conditions : 
C   ================
C-----------------------------------------------------------------------
#include "tpeam.inc"
C-----------------------------------------------------------------------
c
c        functions
c
c        integer lochar        ! Obsolete
      integer lenocc,icloc
c
C-----------------------------------------------------------------------
c
c        HBOOK variables...
c
ccccc         common/quest/iquest
ccccc         integer iquest(100)

         
         integer nquota,nvar,icycle,nvar2,nvar3,nvar4,nvar5,nvar6
         parameter (nvar=9,nvar2=27,nvar3=5,nvar4=5,nvar5=5,nvar6=4)
         character*4 chtags(nvar),chtags2(nvar2)
         character*4 chtags3(nvar3),chtags4(nvar4),chtags5(nvar5)
         character*4 chtags6(nvar6)
C-----------------------------------------------------------------------
         logical lstat,first

         real vec(27)
         real vec2(4)

         integer istat
         integer indx
         integer isect,irow,iwire,nseq,numbv
         integer kdummy,idummy,jdummy

         integer max_incidents
         parameter (max_incidents=100) ! this is a lot, but sometimes necc.

         integer mxhits
         parameter (mxhits = 45) ! this is max # of row-crossings
                                 ! a track makes on one pass thru
                                 ! the sector (>45 b/c of possible
                                 ! row-grazing incedence)
         
         integer nmctrk,nhitrk,itrk,nmchit,oldmchit
         integer nhitrk2,nmchit2
         character*80 outfile
         integer iper,ihisto
        
         integer wire_hits,kwir
c         real tphit_q(mxhits),mchit_q(mxhits) ****

         integer numpix_factor/'100000'X/
         integer tdc_factor/'400'X/
         integer khit,kclus,nseq_hit,ktpc_row,kseq_count
         integer ksector,krow,kseq,kpix,kdatum,kpad
         integer numpix_seq,kadc,ktdc,kpix_count
         integer kmcid

         integer kstatus,tgc_global_to_local,tgc_global_to_local_p
	 integer tgc_pad_to_x

         real invec(3),outvec(3)

         real time_bucket
         integer mxrow
         parameter(mxrow=45)
         integer imal

         real desum

         logical testcond

         integer mcptr(mxhits*24,1:max_incidents)
         integer dtptr(mxhits*24,0:max_incidents) ! indx1=row or volume
                                                  ! indx2 = pass# for 
						  ! this track
                                                  ! (index2=0 gives 
						  !  # incedences)

         logical used_recon_hit(1:max_incidents) ! keeps track, for a given
                                ! sector/row/track whether all reconstructed
                                ! hits claiming to belong to that track have
                                ! been associated with mc hit

         integer i_incidence
         integer n_incidence(1:24,1:mxrow) ! # times a mc track shows up
                                           ! on *center* wire over
					   ! this row
                                           ! in this sector
         real closest,distance
         integer status,ihit_recon,ihit_mc
	 integer tls_quick_sort_i
	 logical scv_state
	 character*80  mess

c this is to keep track of *all* hits and mc hits (pad-row crossings)
c  regardless of mcid so as to make a distribution of distance to all
c  neighboring hits in the sector/padrow  (comparing histograms of the
c  ntuples 800+ and 850+ will tell us what is the "merging" distance)
         integer max_nhits_in_row
         parameter (max_nhits_in_row=600)
         integer mc_ptr_list(mxhits*24,0:max_nhits_in_row)
         integer rec_ptr_list(mxhits*24,0:max_nhits_in_row)
c (as usual index2=0 tells *number* of hits in the list for that sector/padrow)
         integer index1,index2,histid
         real weight,xydist,zdist
c
C-----------------------------------------------------------------------
         data chtags/'itrk','itcl','nrow','x','y','z','prf','zrf','q'/
         data chtags2/'itrk','mcx ','mcy ','mcz ','tpx ','tpy ','tpz '
     +,              'delx','dely','delz','nrow', 'clus','nseq',
     +                'dx','dy','dz','mhit','nmc',
     +                'phi','rlam','malf','ralf','rhit','flag','q','prf',
     +		      'mlam'/
         data chtags3/'prec','mce','tpe','rde','pre2'/
*	 data chtags4/'hit ','clus','sect','row ','pad ',
*     +		      'tdc ','adc ','mcid','xpix','ypix','zpix',
*     +		      'xloc','yloc','zloc','xh_l','yh_l','zh_l'/
	 data chtags4/'xloc','zloc','adc ','clus','row '/
         data chtags5/'xh_l','zh_l','hit ','clus','row '/
         data chtags6/'xh_l','zh_l','mtrk','row '/
         data first/.true./,ihisto/0/
C-----------------------------------------------------------------------

c these are for determining how many mc tracks contributed pixels to a
c  given cluster... added 23jun95 mal
         integer iclstr,isqu,ipx,ipx_first
         integer idat,npx,jmc,imc_found,nmc_found
         integer nmc_found_max
         parameter (nmc_found_max=10)
         integer mc_found(1:nmc_found_max)

cc for cleanup -- try to match up ghost and lost hits...
         integer n_maybe_ghost(1080),n_maybe_lost(1080)
         integer n_maybe_ghost_max
         parameter (n_maybe_ghost_max=400)
         integer maybe_ghost(n_maybe_ghost_max,1080)
         integer n_maybe_lost_max
         parameter (n_maybe_lost_max=400)
         integer maybe_lost(n_maybe_lost_max,1080)
         integer i_maybe_lost,i_maybe_ghost
         logical lost_is_found(n_maybe_lost_max),ghost_is_saved(n_maybe_ghost_max)
         integer ptr_lost,ptr_ghost

         real max_sep_same_mcid/3.0/ ! these are used as maximum seperation
         real max_sep_diff_mcid/2.0/ ! cuts for associating mc and
                                     ! recon hits with same/diff mc track id

c
c        set status
c
         tpeam = STAFCV_OK

         if(first)then
            first = .false.

c
c           open rz file for ntuple...
c
            outfile = 'tpe.rzd'

C

c things needed for the ntuple 500+ihisto...
 	    time_bucket = detector(1).drift_length/tsspar(1).ntime

         endif


         ihisto = ihisto+1

         if (ihisto.eq.1) then
            call hropen(23,'NTUP',outfile,'N',5120,ISTAT)
         else
            call hropen(23,'NTUP',outfile,'U',5120,ISTAT)
         endif
         if(istat.ne.0)write(*,*)' no rz file'


c         call hcdir('//NTUP',' ')

c ntuple 200 as defined has no more information than the hits table...
*         call hbookn(200+ihisto,'Hit Info',nvar,'NTUP',8000,
*     +           chtags)
         call hbookn(300+ihisto,'Compare Info',nvar2,'NTUP',27000,
     +              chtags2)
         if (tclpar(1).mc.eq.1)
     +        call hbookn(400+ihisto,'Track Info',nvar3,'NTUP',4000,
     +        chtags3)
         call hbookn(500+ihisto,'Hit Pixel Info',nvar4,'NTUP',5000,
     +              chtags4)
         call hbookn(600+ihisto,'LOCAL Hit Info',nvar5,'NTUP',5000,
     +              chtags5)
         if (tclpar(1).mc.eq.1) 
     +        call hbookn(650+ihisto,'LOCAL MC Hit Info',nvar6,'NTUP',4000,
     +        chtags6)
         if (tclpar(1).mc.eq.1)  then
            call hbook2(1100+ihisto,'MC Neighbor Distance -Inner',
     +           40,0.0,5.0,40,0.0,5.0,-1.0)
            call hbook2(1200+ihisto,'MC Neighbor Distance -Outer',
     +           40,0.0,5.0,40,0.0,5.0,-1.0)
         endif
         call hbook2(2100+ihisto,'Rec Neighbor Distance -Inner',
     +              40,0.0,5.0,40,0.0,5.0,-1.0)
         call hbook2(2200+ihisto,'Rec Neighbor Distance -Outer',
     +              40,0.0,5.0,40,0.0,5.0,-1.0)

c********** 23may96 * if this is real data, then things are simpler
c******************** we only want visualization ntuples 500,300,600
         icycle = 0
         if (tclpar(1).mc.eq.0) then
c fill ntuple 500 = pixel information
	WRITE(6,*)'OFFSETTING PIXELS BY ...',
     +		3.0*tsspar(1).tau*tsspar(1).v_drift
            do kclus=1,tpcluster_h.nok
               vec(4)=kclus
               nseq_hit=tpcluster(kclus).nseq
               ktpc_row=tpcluster(kclus).tpc_row
               ksector = ktpc_row/100
               krow = ktpc_row - ksector*100
               
               vec(5) = ktpc_row
               kseq = tpcluster(kclus).jseq
*     
               do kseq_count=1,nseq_hit
                  kpad = tpseq(kseq).secpad
                  kstatus=tgc_pad_to_x(float(kpad),float(krow),
     +                 vec(1),pad_plane_h,pad_plane)
                  kpix = tpseq(kseq).jpix
                  kdatum=tppixel(kpix).datum
                  numpix_seq=kdatum/numpix_factor ! 24mar95 malisa
                  do kpix_count=1,numpix_seq
                     kdatum=tppixel(kpix).datum
                     ktdc = (kdatum - numpix_factor*numpix_seq)/tdc_factor
                     kadc = kdatum - numpix_factor*numpix_seq - 
     +                    tdc_factor*ktdc
                     ktdc = ktdc+1 ! first bucket is coded as tdc=0
                     kmcid = tpmcpix(kpix).mcid
                     vec(3)=kadc
                     vec(2) = ktdc*time_bucket
c I'm tired of answering the same question of why there is a time
c  offset between the pixels and the hits, so offset the pixels too
                     vec(2) = vec(2) - 
     +                    3.0*tsspar(1).tau*tsspar(1).v_drift
                     call hfn(500+ihisto,vec)
                     kpix=kpix+1
                  enddo
                  kseq = tpseq(kseq).next
               enddo
            enddo
            call hrout(500+ihisto,icycle,' ')
c fill a much simpler version of ntuple 300, containing no correlation to MC data...
            do ihit_recon=1,tphit_h.nok
               vec(1) = tphit(ihit_recon).track ! may be useful some day even w/o MC
               vec(2) = 0.0
               vec(3) = 0.0
               vec(4) = 0.0
               vec(5) = tphit(ihit_recon).x
               vec(6) = tphit(ihit_recon).y
               vec(7) = tphit(ihit_recon).z
               vec(8) = 0.0
               vec(9) = 0.0
               vec(10) = 0.0
               vec(11) = tphit(ihit_recon).row
               kclus = tphit(ihit_recon).cluster
               vec(12) = kclus
               vec(13) = tpcluster(kclus).nseq
               vec(14) = tphit(ihit_recon).dx
               vec(15) = tphit(ihit_recon).dy
               vec(16) = tphit(ihit_recon).dz
               vec(17) = 0      ! mc hit
               vec(18) = 0      ! nmc hit
               vec(19) = tphit(ihit_recon).phi
               vec(20) = tphit(ihit_recon).lambda
               vec(21) = 0.0    ! mc alpha
               vec(22) = tphit(ihit_recon).alpha
               vec(23) = tphit(ihit_recon).id
               vec(24) = tphit(ihit_recon).flag
               vec(25) = tphit(ihit_recon).q
               vec(26) = tphit(ihit_recon).prf
               vec(27) = 0.0    ! mc lambda
               call hfn(300+ihisto,vec)
            enddo
            call hrout(300+ihisto,icycle,' ')
c fill ntuple 600, local hit information
            do khit=1,tphit_h.nok
               vec(3) = tphit(khit).id
               kclus = tphit(khit).cluster
               vec(4) = kclus
               invec(1) = tphit(khit).x
               invec(2) = tphit(khit).y
               invec(3) = tphit(khit).z
               ktpc_row = tpcluster(kclus).tpc_row
               vec(5) = tphit(khit).row
               ksector = ktpc_row/100
               krow = ktpc_row - ksector*100
               kstatus=tgc_global_to_local(ksector,invec,outvec)
               vec(1) = outvec(1)
               vec(2) = outvec(3)
               call hfn(600+ihisto,vec)
            enddo
            call hrout(600+ihisto,icycle,' ')
            call hrend('NTUP')
            close(23)
            return
         endif

c clear overall pointer list numbers...
         do isect=1,24
            do irow=1,45
               idummy=(isect-1)*45+irow
               mc_ptr_list(idummy,0) = 0
               rec_ptr_list(idummy,0) = 0
            enddo
         enddo

         status=tls_quick_sort_i(tphit_h.nok,tphit(1).track,
     +        tphit(2).track,tphit)
         if (.not.scv_state(status,'i')) then
            call scv_messages(status,mess)
            write(6,*)' error upon tphit sort'
         endif
         status=tls_quick_sort_i(g2t_tpc_hit_h.nok,
     +        g2t_tpc_hit(1).track_p,
     +        g2t_tpc_hit(2).track_p,g2t_tpc_hit)
         if (.not.scv_state(status,'i')) then
            call scv_messages(status,mess)
            write(6,*)' error upon g2t_tpc_hit sort'
         endif
         
c         ihit_recon=0
c must skip past any reconstructed hits with track=0
c (these are "ghost" or "noise" hits mal 22jun95)
         do ihit_recon=1,tphit_h.nok
            if (tphit(ihit_recon).track.ne.0) goto 19
c but still store it in overall pointer list
            idummy = int(tphit(ihit_recon).row)
            idummy = (idummy/100-1)*45+mod(idummy,100)
            rec_ptr_list(idummy,0) = rec_ptr_list(idummy,0) + 1
            if (rec_ptr_list(idummy,0).gt.max_nhits_in_row) then
               write(6,*)'TPE-overflow of rec_ptr_list0',idummy
               rec_ptr_list(idummy,0) = max_nhits_in_row
            endif
            rec_ptr_list(idummy,rec_ptr_list(idummy,0))=ihit_recon
         enddo
 19      ihit_recon=ihit_recon-1
c
         ihit_mc =0

         do isect=1,24
            do irow=1,45
               idummy=(isect-1)*45+irow
               n_maybe_lost(idummy)=0
               n_maybe_ghost(idummy)=0
            enddo
         enddo

         do itrk = 1, g2t_track_h.nok
            nhitrk = 0
            nhitrk2 = 0
            nmctrk = 0
            nmchit = 0
            nmchit2 = 0
            oldmchit = 0
            
            call vzero(n_incidence,24*mxrow)
            
            do idummy=1,mxhits*24
               dtptr(idummy,0) = 0
            enddo
c     
c     build pointers to mc hits on itrk
c     
 1          ihit_mc=ihit_mc+1
            if (ihit_mc.gt.g2t_tpc_hit_h.nok) goto 2
            if (g2t_tpc_hit(ihit_mc).track_p.eq.itrk) then
               numbv = g2t_tpc_hit(ihit_mc).volume_id
c only consider those hits that are actually on padrows (not on
c  "ghost" padrows that precede and follow inner sector padrows)
               if (numbv.lt.100000) then
                  isect = numbv/100
                  irow = (numbv-100*isect)

                  idummy=(isect-1)*mxrow+irow
                  n_incidence(isect,irow)=n_incidence(isect,irow)+1
                  if (n_incidence(isect,irow).gt.max_incidents) then
                     write(6,*)'Too many mc spirals! -skip trak >',
     +                    max_incidents,itrk
 881                 ihit_mc = ihit_mc+1
                     if (g2t_tpc_hit(ihit_mc).track_p.eq.itrk) goto 881
                     ihit_mc = ihit_mc-1
c also make sure skip over track in recon hits
 883                 ihit_recon=ihit_recon+1
                     if (tphit(ihit_recon).track.eq.itrk) goto 883
                     ihit_recon=ihit_recon-1
                     goto 99
                  endif
                  mcptr(idummy,n_incidence(isect,irow))=ihit_mc
c also store in overall pointer list
                  mc_ptr_list(idummy,0)=mc_ptr_list(idummy,0)+1
                  if (mc_ptr_list(idummy,0).gt.max_nhits_in_row) then
                     mc_ptr_list(idummy,0)=max_nhits_in_row
                     write(6,*)' INCREASE MAX_NHITS_IN_ROW'
                  else
                     mc_ptr_list(idummy,mc_ptr_list(idummy,0))=ihit_mc
                  endif
                  nmctrk = nmctrk + 1
               endif
               goto 1
            else
               ihit_mc=ihit_mc-1
            endif
 2          continue
            
c     
c     build pointers to reconstructed hits on itrk
c     
 3          ihit_recon=ihit_recon+1
            if (ihit_recon.gt.tphit_h.nok) goto 4
            if (tphit(ihit_recon).track.eq.itrk) then
               nhitrk = nhitrk + 1
               if(tphit(ihit_recon).row.ge.40)then
                  nhitrk2 = nhitrk2 + 1
               endif
c     
c     stupidity for HPUX compiler
c     
c     dtptr(tphit(ihit).row) = ihit
               kdummy = tphit(ihit_recon).row
               kdummy = mod(kdummy,100) ! now row=100*SECTOR+ROW
					! mal 1mar95
               idummy = 
     +		mxhits*(int(tphit(ihit_recon).row-kdummy)/100 - 1)
     +		 + kdummy
               dtptr(idummy,0) = dtptr(idummy,0) + 1
               if (dtptr(idummy,0).gt.max_incidents) then
                  write(6,*)'Too many recon spirals!! - skip trak >',
     +                 max_incidents,itrk
 882              ihit_recon=ihit_recon+1
                  if (tphit(ihit_recon).track.eq.itrk) goto 882
                  ihit_recon = ihit_recon-1
                  goto 99
               endif
               dtptr(idummy,dtptr(idummy,0)) = ihit_recon
c also store in overall pointer list
               rec_ptr_list(idummy,0)=rec_ptr_list(idummy,0)+1
               if (rec_ptr_list(idummy,0).gt.max_nhits_in_row) then
                  rec_ptr_list(idummy,0)=max_nhits_in_row
                  write(6,*)' INCREASE MAX_NHITS_IN_ROW!'
               else
                  rec_ptr_list(idummy,rec_ptr_list(idummy,0))=ihit_recon
               endif
               goto 3
            else
               ihit_recon=ihit_recon-1
            endif
 4          continue
            
            
c     collect comparison statistics for all mc/rec hits on itrk
c     
            do isect=tsspar(1).min_sect,tsspar(1).max_sect    ! only make evaluation
               do irow = tsspar(1).min_row,tsspar(1).max_row  ! for part that is simulated
c keep track of whether all hits claiming to belong to this track that are
c   in this sector/row end up being associated with a mc hit.... if not, then
c   they are a "ghost" hit, and get put in ntuple with clus.ne.0 but mhit=0
c
                  idummy=mxhits*(isect-1)+irow
                  do i_incidence=1,dtptr(idummy,0)
                     used_recon_hit(i_incidence)=.false.
                  enddo
                  do i_incidence=1,n_incidence(isect,irow)
                     nmchit = nmchit + 1
                     if(irow.ge.40)then
                        nmchit2 = nmchit2 + 1
                     endif
                     
                     if (nmchit .ne. oldmchit)then
                        oldmchit = nmchit
                        
                        vec(11) = real(irow)+100*real(isect)
                        vec(1) = real(itrk)
                        idummy=(isect-1)*mxrow+irow
                        vec(2)=g2t_tpc_hit(mcptr(idummy,i_incidence)).x(1)
                        vec(3)=g2t_tpc_hit(mcptr(idummy,i_incidence)).x(2)
                        vec(4)=g2t_tpc_hit(mcptr(idummy,i_incidence)).x(3)

c----------------------------------------------------------------
c for filling ntuple with LOCAL MC hit info...
                        invec(1)=vec(2)
                        invec(2)=vec(3)
                        invec(3)=vec(4)
                        kstatus=tgc_global_to_local(isect,invec,outvec)
                        vec2(1)=outvec(1)
                        vec2(2)=outvec(3)
                        vec2(3)=
     +                       g2t_tpc_hit(mcptr(idummy,i_incidence)).track_p
                        vec2(4)=real(100*isect+irow)
                        call hfn(650+ihisto,vec2)
c----------------------------------------------------------------

c a similar transformation is also needed for me to calculate the
c  crossing angle alpha of the track, which is by definition in local
c  coordinates
                        invec(1)=g2t_tpc_hit(mcptr(idummy,i_incidence)).p(1)
                        invec(2)=g2t_tpc_hit(mcptr(idummy,i_incidence)).p(2)
                        invec(3)=g2t_tpc_hit(mcptr(idummy,i_incidence)).p(3)

                        kstatus=tgc_global_to_local_p(isect,invec,outvec)
                        if (outvec(2).ne.0.0) then
                           vec(21)=atand(outvec(1)/outvec(2))
                           vec(27)=atand(outvec(3)/outvec(2))
                        else
                           vec(21)=90.0
                           vec(23)=90.0
                        endif

                        vec(17)=real(
     +			  g2t_tpc_hit(mcptr(idummy,i_incidence)).id)
                        
                        
c     now must find the reconstructed hit on this row and sector
c     associated with this track
c     that is *closest* to the generated mc hit 
c     (but it must be *somewhat* close - i.e.
c     within 2 cm.)
                        idummy=mxhits*(isect-1)+irow
                        closest=100.0
                        do kdummy=1,dtptr(idummy,0)
                           if (.not.used_recon_hit(kdummy)) then ! 20jul95 mal
                              distance=sqrt( 
     +                             (tphit(dtptr(idummy,kdummy)).x-
     +                             vec(2))**2 +
     +                             (tphit(dtptr(idummy,kdummy)).y-
     +                             vec(3))**2 +
     +                             (tphit(dtptr(idummy,kdummy)).z-
     +                             vec(4))**2)
                              if (distance.lt.closest) then
                                 closest=distance
                                 jdummy=kdummy
                              endif
                           endif
                        enddo
c     if closest>2.0) then there is no 
c     nearby reconstructed hit with same MCID
                        
                        if(closest.lt.max_sep_same_mcid)then
c-------------------------------------------
c store the correlated pair in the tpc_index table..- ** new 24jul95 mal
                           tpc_index_h.nok = tpc_index_h.nok+1
                           tpc_index(tpc_index_h.nok).type = 
     +                          tpc_index_type(1).tphit_mhitstpc
                           tpc_index(tpc_index_h.nok).key1 =
     +                          tphit(dtptr(idummy,jdummy)).id
                           tpc_index(tpc_index_h.nok).key2 = 
     +                          g2t_tpc_hit(mcptr(idummy,i_incidence)).id
c-------------------------------------------                           
                           used_recon_hit(jdummy)=.true.
                           vec(5) = tphit(dtptr(idummy,jdummy)).x
                           vec(6) = tphit(dtptr(idummy,jdummy)).y
                           vec(7) = tphit(dtptr(idummy,jdummy)).z
                           vec(8) = vec(2)-vec(5)
                           vec(9) = vec(3)-vec(6)
                           vec(10) = vec(4)-vec(7)
                           vec(12) = real(
     +			    tphit(dtptr(idummy,jdummy)).cluster)
                           vec(13) = real(tpcluster(int(vec(12))).nseq)
                           vec(14) = tphit(dtptr(idummy,jdummy)).dx
                           vec(15) = tphit(dtptr(idummy,jdummy)).dy
                           vec(16) = tphit(dtptr(idummy,jdummy)).dz
                           vec(19) = tphit(dtptr(idummy,jdummy)).phi
                           vec(20) = tphit(dtptr(idummy,jdummy)).lambda
                           vec(22) = tphit(dtptr(idummy,jdummy)).alpha
                           vec(23) = real(
     +                          tphit(dtptr(idummy,jdummy)).id)
                           vec(24) = real(
     +                          tphit(dtptr(idummy,jdummy)).flag)
                           vec(25) = tphit(dtptr(idummy,jdummy)).q
                           vec(26) = tphit(dtptr(idummy,jdummy)).prf
c ok we have found a reconstructed hit... how many mc tracks contributed
c  to its cluster???
                           nmc_found=0
                           iclstr=tphit(dtptr(idummy,jdummy)).cluster
                           isqu=tpcluster(iclstr).jseq
 32                        ipx_first=tpseq(isqu).jpix
                           idat=tppixel(ipx_first).datum
                           npx=idat/numpix_factor
                           do ipx=ipx_first,ipx_first+npx-1
                              jmc=tpmcpix(ipx).mcid
                              if (jmc.eq.0) goto 33
                              do imc_found=1,nmc_found
                                 if (jmc.eq.mc_found(imc_found)) goto 33
                              enddo
                              nmc_found=nmc_found+1
                              if (nmc_found.gt.nmc_found_max) then
                                 nmc_found=nmc_found_max
                                 goto 34 ! done searching this cluster
                              endif
                              mc_found(nmc_found)=jmc
 33                           continue ! already found that one
                           enddo
                           isqu=tpseq(isqu).next
                           if (isqu.ne.0) goto 32
 34                        continue ! ok, done searching cluster...
                           vec(18) = real(nmc_found)
                           call hfn(300+ihisto,vec)
                        else
c now, instead of declaring such a hit lost, we save it for the
c  clean-up stage which follows, which tries to link up 
c     "lost" and "ghost"
c  hits...
                           n_maybe_lost(idummy) = n_maybe_lost(idummy)+1
                           if (n_maybe_lost(idummy).gt.n_maybe_lost_max) then
                              write(6,*)
     +                             'Increase Possible Lost Array!!!'
                              goto 99
                           endif
                           maybe_lost(n_maybe_lost(idummy),idummy)=
     +                          mcptr(idummy,i_incidence)
c d                          vec(5) = 0.
c  o                          vec(6) = 0.
c                            vec(7) = 0.
c    n                       vec(8) = 0.
c     o                       vec(9) = 0.
c      t                      vec(10) = 0.
c                            vec(12) = 0.
c        s                   vec(13) = 0.
c         t                   vec(14) = 0.
c          o                 vec(15) = 0.
c           r                vec(16) = 0.
c            e               vec(18) = 0.
                        endif
                        
                     endif      ! if(nmchit .ne. oldmchit)
                  enddo         ! do i_incidence=1,n_incidence
c ok, that's the end of this track 
c on this sector/row, so any hit who claims
c  to belong to the current track, but has 
c not been associated with a mc hit,
c MAY BE a "ghost" hit... 
c fill the ntuple with these guys AFTER CLEAN-UP
                  idummy=mxhits*(isect-1)+irow
                  do i_incidence=1,dtptr(idummy,0)
                     if (.not.used_recon_hit(i_incidence)) then
                        n_maybe_ghost(idummy)=n_maybe_ghost(idummy)+1
                        if (n_maybe_ghost(idummy).gt.
     +                       n_maybe_ghost_max) then
                           write(6,*)'Increase Possible Ghost Array!!!'
                           goto 99
                        endif
                        maybe_ghost(n_maybe_ghost(idummy),idummy)=
     +                       dtptr(idummy,i_incidence)
c so now we store it as a POSSIBLE ghost (of type 2) and fill in
c  a ghost entry (if appropriate) in ntuple 300+ihisto after clean-up
                     endif
                  enddo
               enddo            ! do irow = min_row,max_row
            enddo               ! do isect= min_sect,max_sect
            
***********
            
            if(nmchit.gt.0)then
               vec(1) = float(nhitrk)/float(nmchit)
            else
               vec(1) = 0.
            endif
            if(nmchit2.gt.0)then
               vec(5) = float(nhitrk2)/float(nmchit2)
            else
               vec(5) = 0.
            endif
            
            call hfn(400+ihisto,vec)
 99         continue
         enddo                  ! do itrk = 1, g2t_track_h.nok
         
         call hrout(400+ihisto,icycle,' ')
         call hrout(650+ihisto,icycle,' ')

c now, run through a clean-up stage, where match-up between
c maybe "lost" or 
c  "ghost" hits is attempted; (hits that should be associated 
c with each other
c  may wind up as "lost"+"ghost" if the  MCID for the 
c reconstructed hit is
c  incorrectly assigned, which is easy enough)

         do isect=tsspar(1).min_sect,tsspar(1).max_sect   ! only make evaluation
            do irow = tsspar(1).min_row,tsspar(1).max_row ! for part that is simulated
               idummy=(isect-1)*mxrow+irow
               do i_maybe_lost=1,n_maybe_lost(idummy)
                  lost_is_found(i_maybe_lost)=.false.
               enddo
               do i_maybe_ghost=1,n_maybe_ghost(idummy)
                  ghost_is_saved(i_maybe_ghost)=.false.
               enddo
               vec(11) = 100*isect+irow
c if a ghost and lost hit are within 1 cm of each other,
c they get associated
               do i_maybe_lost=1,n_maybe_lost(idummy)
                  ptr_lost=maybe_lost(i_maybe_lost,idummy)
                  vec(1) = g2t_tpc_hit(ptr_lost).track_p
                  vec(2) = g2t_tpc_hit(ptr_lost).x(1)
                  vec(3) = g2t_tpc_hit(ptr_lost).x(2)
                  vec(4) = g2t_tpc_hit(ptr_lost).x(3)
                  vec(17) = g2t_tpc_hit(ptr_lost).id
c a similar transformation is also needed for me to calculate the
c  crossing angle alpha of the track, which is by definition in local
c  coordinates
                  invec(1)=g2t_tpc_hit(mcptr(idummy,i_incidence)).p(1)
                  invec(2)=g2t_tpc_hit(mcptr(idummy,i_incidence)).p(2)
                  invec(3)=g2t_tpc_hit(mcptr(idummy,i_incidence)).p(3)
                  kstatus=tgc_global_to_local_p(isect,invec,outvec)
                  if (outvec(2).ne.0.0) then
                     vec(21)=atand(outvec(1)/outvec(2))
                     vec(27)=atand(outvec(3)/outvec(2))
                  else
                     vec(21)=90.0
                     vec(27)=90.0
                  endif
                  closest=100.0
                  do i_maybe_ghost=1,n_maybe_ghost(idummy)
                     if (.not.ghost_is_saved(i_maybe_ghost)) then
                        ptr_ghost=maybe_ghost(i_maybe_ghost,idummy)
                        distance=sqrt(
     +                       (vec(2)-tphit(ptr_ghost).x)**2 +
     +                       (vec(3)-tphit(ptr_ghost).y)**2 +
     +                       (vec(4)-tphit(ptr_ghost).z)**2)
                        if (distance.lt.closest) then
                           closest=distance
                           jdummy=i_maybe_ghost
                        endif
                     endif
                  enddo
                  if (closest.lt.max_sep_diff_mcid) then 
c not lost after all..
                     ghost_is_saved(jdummy)=.true.
                     lost_is_found(i_maybe_lost)=.true.
                     ptr_ghost=maybe_ghost(jdummy,idummy)
c-------------------------------------------
c store the correlated pair in the tpc_index table... -- ** new 24jul95 mal
                     tpc_index_h.nok = tpc_index_h.nok+1
                     tpc_index(tpc_index_h.nok).type = 
     +                    tpc_index_type(1).tphit_mhitstpc
                     tpc_index(tpc_index_h.nok).key1 =
     +                    tphit(ptr_ghost).id
                     tpc_index(tpc_index_h.nok).key2 = 
     +                    g2t_tpc_hit(ptr_lost).id
c-------------------------------------------
                     vec(5)=tphit(ptr_ghost).x
                     vec(6)=tphit(ptr_ghost).y
                     vec(7)=tphit(ptr_ghost).z
                     vec(8)=vec(2)-vec(5)
                     vec(9)=vec(3)-vec(6)
                     vec(10)=vec(4)-vec(7)
                     vec(12)=tphit(ptr_ghost).cluster
                     vec(13)=tpcluster(int(vec(12))).nseq
                     vec(14)=tphit(ptr_ghost).dx
                     vec(15)=tphit(ptr_ghost).dy
                     vec(16)=tphit(ptr_ghost).dz
                     vec(19) = tphit(ptr_ghost).phi
                     vec(20) = tphit(ptr_ghost).lambda
                     vec(22) = tphit(ptr_ghost).alpha
                     vec(23) = real(tphit(ptr_ghost).id)
                     vec(24) = real(tphit(ptr_ghost).flag)
                     vec(25) = tphit(ptr_ghost).q
                     vec(26) = tphit(ptr_ghost).prf
c find out how many mc tracks contributed to this hit's cluster...
                     nmc_found=0
                     iclstr=tphit(ptr_ghost).cluster
                     isqu=tpcluster(iclstr).jseq
 42                  ipx_first=tpseq(isqu).jpix
                     idat=tppixel(ipx_first).datum
                     npx=idat/numpix_factor
                     do ipx=ipx_first,ipx_first+npx-1
                        jmc=tpmcpix(ipx).mcid
                        if (jmc.eq.0) goto 43
                        do imc_found=1,nmc_found
                           if (jmc.eq.mc_found(imc_found)) goto 43
                        enddo
                        nmc_found=nmc_found+1
                        if (nmc_found.gt.nmc_found_max) then
                           nmc_found=nmc_found_max
                           goto 44 ! done searching for this cluster
                        endif
                        mc_found(nmc_found)=jmc
 43                     continue ! already found that one
                     enddo
                     isqu=tpseq(isqu).next
                     if (isqu.ne.0) goto 42
 44                  continue   ! ok, done searching cluster...
                     vec(18) = real(nmc_found)
                  else          ! it really is lost...
c-------------------------------------------
c store the correlated pair in the tpc_index table... -- ** new 24jul95 mal
                     tpc_index_h.nok = tpc_index_h.nok+1
                     tpc_index(tpc_index_h.nok).type = 
     +                    tpc_index_type(1).tphit_mhitstpc
                     tpc_index(tpc_index_h.nok).key1 = 0
                     tpc_index(tpc_index_h.nok).key2 = 
     +                    g2t_tpc_hit(mcptr(idummy,i_incidence)).id
c-------------------------------------------
                     vec(5)=0.0
                     vec(6)=0.0
                     vec(7)=0.0
                     vec(8)=0.0
                     vec(9)=0.0
                     vec(10)=0.0
                     vec(12)=0.0
                     vec(13)=0.0
                     vec(14)=0.0
                     vec(15)=0.0
                     vec(16)=0.0
                     vec(18)=0.0
                     vec(19)=0.0
                     vec(20)=0.0
                     vec(22)=0.0
                     vec(23)=0.0
                     vec(24)=0.0
                     vec(25)=0.0
                     vec(26)=0.0
                  endif
                  call hfn(300+ihisto,vec)
               enddo            ! i_maybe_lost
c ok, any reconstructed maybe-ghost hit left on this sector/row that
c  has no dancing partner NOW is out of luck.
c
               do i_maybe_ghost=1,n_maybe_ghost(idummy)
                  if (.not.ghost_is_saved(i_maybe_ghost)) then
                     ptr_ghost=maybe_ghost(i_maybe_ghost,idummy)
c-------------------------------------------
c store the correlated pair in the tpc_index table... -- ** new 24jul95 mal
                     tpc_index_h.nok = tpc_index_h.nok+1
                     tpc_index(tpc_index_h.nok).type = 
     +                    tpc_index_type(1).tphit_mhitstpc
                     tpc_index(tpc_index_h.nok).key1 =
     +                    tphit(ptr_ghost).id
                     tpc_index(tpc_index_h.nok).key2 = 0
c-------------------------------------------
                     vec(1)=tphit(ptr_ghost).track
                     vec(2)=0.0
                     vec(3)=0.0
                     vec(4)=0.0
                     vec(5)=tphit(ptr_ghost).x
                     vec(6)=tphit(ptr_ghost).y
                     vec(7)=tphit(ptr_ghost).z
                     vec(8)=0.0
                     vec(9)=0.0
                     vec(10)=0.0
                     vec(12)=tphit(ptr_ghost).cluster
                     vec(13)=tpcluster(int(vec(12))).nseq
                     vec(14)=tphit(ptr_ghost).dx
                     vec(15)=tphit(ptr_ghost).dy
                     vec(16)=tphit(ptr_ghost).dz
                     vec(17)=0.0
                     vec(21)=0.0
                     vec(27)=0.0
                     vec(19) = tphit(ptr_ghost).phi
                     vec(20) = tphit(ptr_ghost).lambda
                     vec(22) = tphit(ptr_ghost).alpha
                     vec(23) = real(tphit(ptr_ghost).id)
c find out how many mc tracks contributed to this hit's cluster...
                     nmc_found=0
                     iclstr=tphit(ptr_ghost).cluster
                     isqu=tpcluster(iclstr).jseq
 52                  ipx_first=tpseq(isqu).jpix
                     idat=tppixel(ipx_first).datum
                     npx=idat/numpix_factor
                     do ipx=ipx_first,ipx_first+npx-1
                        jmc=tpmcpix(ipx).mcid
                        if (jmc.eq.0) goto 53
                        do imc_found=1,nmc_found
                           if (jmc.eq.mc_found(imc_found)) goto 53
                        enddo
                        nmc_found=nmc_found+1
                        if (nmc_found.gt.nmc_found_max) then
                           nmc_found=nmc_found_max
                           goto 54 ! done searching for this cluster
                        endif
                        mc_found(nmc_found)=jmc
 53                     continue ! already found that one
                     enddo
                     isqu=tpseq(isqu).next
                     if (isqu.ne.0) goto 52
 54                  continue   ! ok, done searching cluster...
                     vec(18) = real(nmc_found)
                     call hfn(300+ihisto,vec)
                  endif
               enddo            ! i_maybe_ghost
            enddo               ! irow=min_row,max_row
         enddo                  ! isect=min_sect,max_sect

         call hrout(300+ihisto,icycle,' ')


c now make histos that will allow us to determine merging distance
c
c for MC and for recon hits, fill hisogram with distance to all neighbors
c  on a padrow for each hit on that padrow...

         do isect=tsspar(1).min_sect,tsspar(1).max_sect   ! only make evaluation
            do irow = tsspar(1).min_row,tsspar(1).max_row ! for part that is simulated
               idummy=(isect-1)*45+irow
c first do MC hits on this padrow/sector
               if (mc_ptr_list(idummy,0).ne.0) 
     +              weight=1.0/float(mc_ptr_list(idummy,0))
               do index1=1,mc_ptr_list(idummy,0)-1
                  do index2=index1+1,mc_ptr_list(idummy,0)
                     xydist = sqrt(
     +                  (g2t_tpc_hit(mc_ptr_list(idummy,index1)).x(1)-
     +                  g2t_tpc_hit(mc_ptr_list(idummy,index2)).x(1))**2 +
     +                  (g2t_tpc_hit(mc_ptr_list(idummy,index1)).x(2)-
     +                  g2t_tpc_hit(mc_ptr_list(idummy,index2)).x(2))**2)
                     zdist = abs(
     +                    g2t_tpc_hit(mc_ptr_list(idummy,index1)).x(3)-
     +                    g2t_tpc_hit(mc_ptr_list(idummy,index2)).x(3))
                     if (irow.lt.14) then
                        histid=1100+ihisto
                        call hfill(histid,xydist,zdist,weight)
                     else
                        histid=1200+ihisto
                        call hfill(histid,xydist,zdist,weight)
                     endif
                  enddo
               enddo
c now do same with reconstructed point
               if (rec_ptr_list(idummy,0).ne.0) 
     +              weight=1.0/float(rec_ptr_list(idummy,0))
               do index1=1,rec_ptr_list(idummy,0)-1
                  do index2=index1+1,rec_ptr_list(idummy,0)
                     xydist = sqrt(
     +                    (tphit(rec_ptr_list(idummy,index1)).x-
     +                     tphit(rec_ptr_list(idummy,index2)).x)**2 +
     +                    (tphit(rec_ptr_list(idummy,index1)).y-
     +                     tphit(rec_ptr_list(idummy,index2)).y)**2)
                     zdist = abs(tphit(rec_ptr_list(idummy,index1)).z-
     +                    tphit(rec_ptr_list(idummy,index2)).z)
                     if (irow.lt.14) then
                        histid=2100+ihisto
                        call hfill(histid,xydist,zdist,weight)
                     else
                        histid=2200+ihisto
                        call hfill(histid,xydist,zdist,weight)
                     endif
                  enddo
               enddo
            enddo
         enddo

         call hrout(1100+ihisto,icycle,' ')
         call hrout(1200+ihisto,icycle,' ')
         call hrout(2100+ihisto,icycle,' ')
         call hrout(2200+ihisto,icycle,' ')

c now fill pixel data for each hit - added 28nov94 mal
c	goto 119

c actually, fill pixel data for each CLUSTER in ntuple 500+ihisto...
c  mal 12jul95
         do kclus=1,tpcluster_h.nok
            vec(4)=kclus
            nseq_hit=tpcluster(kclus).nseq
            ktpc_row=tpcluster(kclus).tpc_row
            ksector = ktpc_row/100
            krow = ktpc_row - ksector*100

            vec(5) = ktpc_row
            kseq = tpcluster(kclus).jseq
*
            do kseq_count=1,nseq_hit
               kpad = tpseq(kseq).secpad
               kstatus=tgc_pad_to_x(float(kpad),float(krow),
     +              vec(1),pad_plane_h,pad_plane)
               kpix = tpseq(kseq).jpix
               kdatum=tppixel(kpix).datum
               numpix_seq=kdatum/numpix_factor ! 24mar95 malisa
               do kpix_count=1,numpix_seq
                  kdatum=tppixel(kpix).datum
                  ktdc = (kdatum - numpix_factor*numpix_seq)/tdc_factor
                  kadc = kdatum - numpix_factor*numpix_seq - 
     +                 tdc_factor*ktdc
                  ktdc = ktdc+1 ! first bucket is coded as tdc=0
                  kmcid = tpmcpix(kpix).mcid
                  vec(3)=kadc
                  vec(2) = ktdc*time_bucket
                  call hfn(500+ihisto,vec)
                  kpix=kpix+1
               enddo
               kseq = tpseq(kseq).next
            enddo
         enddo
         call hrout(500+ihisto,icycle,' ')

c now fill another ntuple (600+ihisto) with "LOCAL" hit coordinates

         do khit=1,tphit_h.nok
            vec(3) = tphit(khit).id
            kclus = tphit(khit).cluster
            vec(4) = kclus
            invec(1) = tphit(khit).x
            invec(2) = tphit(khit).y
            invec(3) = tphit(khit).z
            ktpc_row = tpcluster(kclus).tpc_row
	    vec(5) = tphit(khit).row
            ksector = ktpc_row/100
            krow = ktpc_row - ksector*100
            kstatus=tgc_global_to_local(ksector,invec,outvec)
            vec(1) = outvec(1)
            vec(2) = outvec(3)
            call hfn(600+ihisto,vec)
         enddo
         call hrout(600+ihisto,icycle,' ')

 989     continue

         call hrend('NTUP')
         close(23)
         return
         end

C----------------------------------------------------------------------

        SUBROUTINE TPE_CLOSE

        Implicit None

C----------------------------------------------------------------------

        call hcdir('//NTUP',' ')
        call hrend('NTUP')
        close(23)

        return
        end


C----------------------------------------------------------------------



