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
     +                        tphith, tphit 
     +,                       mctrlh, mctrl
     +,                       mkineh, mkine
     +,                       mhits_tpch, mhits_tpc 
     +,                       tssparh, tsspar 
     +,			      tpclusterh, tpcluster
     +,			      tpseqh, tpseq
     +,			      tppixelh, tppixel 
     +,			      tpmcpixh, tpmcpix 
     +,			      pad_planeh, pad_plane
     +,			      detectorh, detector,
     +                        tpc_indexh, tpc_index,
     +                        tpc_index_typeh, tpc_index_type)

      IMPLICIT NONE

C   Input arguments  (One to a line with definition after ! ) 
C
C     tphith:
C     tphit:
C
C   Output arguments : 
C     None: this module fills a hit data ntuple for subsequent
C           analysis
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
C
C   Error conditions : 
C   ================
C-----------------------------------------------------------------------
#include "tas_structures.inc"
#include "tas_user_codes.inc"

#include "tcl_tphit_pars.inc"
#include "tcl_tphit_st.inc"

#include "tss_tppixel_pars.inc"
#include "tss_tppixel_st.inc"

#include "tss_tpmcpix_pars.inc"
#include "tss_tpmcpix_st.inc"

#include "tcl_tpcluster_pars.inc"
#include "tcl_tpcluster_st.inc"

#include "tcl_tpseq_pars.inc"
#include "tcl_tpseq_st.inc"

#include "mct_mctrl_pars.inc"
#include "mct_mctrl_st.inc"
#include "mct_mkine_pars.inc"
#include "mct_mkine_st.inc"
#include "mct_mhits_tpc_pars.inc"
#include "mct_mhits_tpc_st.inc"

#include "tss_tsspar_pars.inc"
#include "tss_tsspar_st.inc"

#include "tpg_pad_plane_pars.inc"
#include "tpg_pad_plane_st.inc"

#include "tpg_detector_pars.inc"
#include "tpg_detector_st.inc"

#include "tcl_tpc_index_pars.inc"
#include "tcl_tpc_index_st.inc"

#include "tcl_tpc_index_type_pars.inc"
#include "tcl_tpc_index_type_st.inc"
C-----------------------------------------------------------------------
      RECORD/ table_head_st/ tphith
      RECORD/ tphit_row_st/ tphit(*)

      RECORD/ table_head_st/ mctrlh
      RECORD/ mctrl_row_st/ mctrl(*)
      RECORD/ table_head_st/ mkineh
      RECORD/ mkine_row_st/ mkine(*)
      RECORD/ table_head_st/ mhits_tpch
      RECORD/ mhits_tpc_row_st/ mhits_tpc(*)

      RECORD/ table_head_st/ tssparh
      RECORD/ tsspar_row_st/ tsspar(*)

      RECORD/ table_head_st/ tpclusterh
      RECORD/ tpcluster_row_st/ tpcluster(*)

      RECORD/ table_head_st/ tpseqh
      RECORD/ tpseq_row_st/ tpseq(*)
      
      RECORD/ table_head_st/ tppixelh
      RECORD/ tppixel_row_st/ tppixel(*)
      
      RECORD/ table_head_st/ tpmcpixh
      RECORD/ tpmcpix_row_st/ tpmcpix(*)
      
      RECORD/ table_head_st/ pad_planeh
      RECORD/ pad_plane_row_st/ pad_plane(*)
      
      RECORD/ table_head_st/ detectorh
      RECORD/ detector_row_st/ detector(*)

      RECORD/ table_head_st/ tpc_indexh
      RECORD/ tpc_index_row_st/ tpc_index(*)

      RECORD/ table_head_st/ tpc_index_typeh
      RECORD/ tpc_index_type_row_st/ tpc_index_type(*)

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
         common/quest/iquest
         integer iquest(100)

         
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
         integer ksector,krow,kseq,kpix,kdatum,kpad,ksector_hack
         integer numpix_seq,kadc,ktdc,kpix_count
         integer kmcid,krowtemp,ksectortemp
         logical kstatus,tpg_local_to_global,tpg_global_to_local
         logical tpg_global_to_local_p
         double precision tpg_pad_to_x,tpg_row_to_y
         double precision invec(3),outvec(3)
         real time_bucket
         integer mxrow
         parameter(mxrow=45)
         integer id_pad(mxrow)
         integer imal

         real desum

c to account for the fact that the number or volumes per row
c	has changed..

         integer numbv_row(mxrow),numbv_row_max
         integer numbv_row_2mm(mxrow),numbv_row_4mm(mxrow)
         data numbv_row_2mm/13,13,13,13,13,13,13, ! inner sector
     +        13,13,13,13,13,13,
     +        10,10,10,10,10,10,10,10,10,10,10,	! outer sector
     +        10,10,10,10,10,10,10,10,10,10,10,
     +        10,10,10,10,10,10,10,10,10,10/
         data numbv_row_4mm/9,9,9,9,9,9,9,9,9,9,9,9,9, ! inner sector
     +        5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5, ! outer sector
     +        5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5/
         parameter (numbv_row_max=13)
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
         parameter (max_nhits_in_row=300)
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
         integer maybe_lost(200,1080)
         integer i_maybe_lost,i_maybe_ghost
         logical lost_is_found(200),ghost_is_saved(n_maybe_ghost_max)
         integer ptr_lost,ptr_ghost

         real max_sep_same_mcid/3.0/ ! these are used as maximum seperation
         real max_sep_diff_mcid/2.0/ ! cuts for associating mc and
                                     ! recon hits with same/diff mc track id

c
c        set status
c
         tpeam = tucod_ok

         if(first)then
            first = .false.

c	    set up for 2mm or 4mm geant volumes...
	    if (tsspar(1).geant_volm.eq.0.4) then
	      do irow=1,mxrow
		numbv_row(irow) = numbv_row_4mm(irow)
	      enddo
	    elseif (tsspar(1).geant_volm.eq.0.2) then
	      do irow=1,mxrow
		numbv_row(irow) = numbv_row_2mm(irow)
	      enddo
	    else
	      write(6,*)'TPEAM -- Nonsense Geant volumes!!'
	      do irow=1,mxrow
		numbv_row(irow) = numbv_row_4mm(irow)
	      enddo
	    endif
c
c           open rz file for ntuple...
c
            if(mctrl(1).infile .eq. '""')then
               outfile = 'tpe.rzd'
            else
               outfile = mctrl(1).infile
c              iper = lochar(1H.,1,mctrl(1).infile,80)
               iper = icloc('.',1,mctrl(1).infile,1,80)
               if(iper.gt.0)then
                  outfile(iper+1:iper+7) = 'tpe_rzd'
               else
                  outfile = 'tpe.rzd'
               endif
            endif

c            open(unit=23,file=outfile,form='unformatted',
c     +        recl=1024,access='direct',
c     +        status='new',iostat=istat)
c            nquota=10000
c            if(istat.eq.0)then
c               iquest(10)=nquota
c               call hrfile(23,'NTUP','NQ')
c            endif


C

c things needed for the ntuple 500+ihisto...
 	    time_bucket = detector(1).drift_length/tsspar(1).ntime
            id_pad(1) = 0
            do irow = 2, mxrow
               id_pad(irow) = id_pad(irow-1)
     +                        + pad_plane(1).npads(irow-1)
            enddo


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
         call hbookn(400+ihisto,'Track Info',nvar3,'NTUP',4000,
     +              chtags3)
         call hbookn(500+ihisto,'Hit Pixel Info',nvar4,'NTUP',5000,
     +              chtags4)
         call hbookn(600+ihisto,'LOCAL Hit Info',nvar5,'NTUP',5000,
     +              chtags5)
         call hbookn(650+ihisto,'LOCAL MC Hit Info',nvar6,'NTUP',4000,
     +              chtags6)
         call hbook2(1100+ihisto,'MC Neighbor Distance -Inner',
     +              40,0.0,5.0,40,0.0,5.0,-1.0)
         call hbook2(1200+ihisto,'MC Neighbor Distance -Outer',
     +              40,0.0,5.0,40,0.0,5.0,-1.0)
         call hbook2(2100+ihisto,'Rec Neighbor Distance -Inner',
     +              40,0.0,5.0,40,0.0,5.0,-1.0)
         call hbook2(2200+ihisto,'Rec Neighbor Distance -Outer',
     +              40,0.0,5.0,40,0.0,5.0,-1.0)
c
c ok now we try the above suggestion....
c

c clear overall pointer list numbers...
         do isect=1,24
            do irow=1,45
               idummy=(isect-1)*45+irow
               mc_ptr_list(idummy,0) = 0
               rec_ptr_list(idummy,0) = 0
            enddo
         enddo

         status=tls_quick_sort_i(tphith.nok,tphit(1).track,
     +        tphit(2).track,tphit)
         if (.not.scv_state(status,'i')) then
            call scv_messages(status,mess)
            write(6,*)' error upon tphit sort'
         endif
         status=tls_quick_sort_i(mhits_tpch.nok,mhits_tpc(1).itrk,
     +        mhits_tpc(2).itrk,mhits_tpc)
         if (.not.scv_state(status,'i')) then
            call scv_messages(status,mess)
            write(6,*)' error upon mhits_tpc sort'
         endif
         
c         ihit_recon=0
c must skip past any reconstructed hits with track=0
c (these are "ghost" or "noise" hits mal 22jun95)
         do ihit_recon=1,tphith.nok
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

         do itrk = 1, mkineh.nok
c            write(6,*)'working on track ',itrk
            nhitrk = 0
            nhitrk2 = 0
            nmctrk = 0
            nmchit = 0
            nmchit2 = 0
            oldmchit = 0
c     call vzero(dtptr,45)
c     call vzero(mcptr,180)
            
            call vzero(n_incidence,24*mxrow)
            
c     call vzero(tphit_q,45)
c     call vzero(mchit_q,45)
c     call vzero(mcptr,numbv_row_max*mxhits)
            do idummy=1,mxhits*24
               dtptr(idummy,0) = 0
            enddo
c     
c     build pointers to mc hits on itrk
c     
 1          ihit_mc=ihit_mc+1
            if (ihit_mc.gt.mhits_tpch.nok) goto 2
            if (mhits_tpc(ihit_mc).itrk.eq.itrk) then
               numbv = mhits_tpc(ihit_mc).numbv
c note that iwire no longer refers to wire# - it is subvolume over row.
               iwire = numbv/10000
               isect = (numbv-10000*iwire)/100
               irow = (numbv-10000*iwire-100*isect)   
               if (iwire.eq.numbv_row(irow)/2+1) then ! only look at
						      ! *CENTRAL* wire
						      ! over row
                  idummy=(isect-1)*mxrow+irow
                  n_incidence(isect,irow)=n_incidence(isect,irow)+1
                  if (n_incidence(isect,irow).gt.max_incidents) then
                     write(6,*)'Too many mc spirals! -skip trak >',
     +                    max_incidents
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
c     mcptr((irow-1)*numbv_row_max+iwire) = ihit_mc
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
            if (ihit_recon.gt.tphith.nok) goto 4
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
     +                 max_incidents
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
c     dtptr(kdummy) = ihit_recon
c     tphit_q(nhitrk)=tphit(ihit_recon).q
               goto 3
            else
               ihit_recon=ihit_recon-1
            endif
 4          continue
            
            
cccc            if (nmctrk.eq.0) goto 99
            
c     collect comparison statistics for all mc/rec hits on itrk
c     
            do isect=1,24
               do irow = 1, 45
c keep track of whether all hits claiming to belong to this track that are
c   in this sector/row end up being associated with a mc hit.... if not, then
c   they are a "ghost" hit, and get put in ntuple with clus.ne.0 but mhit=0
c
                  idummy=mxhits*(isect-1)+irow
                  do i_incidence=1,dtptr(idummy,0)
                     used_recon_hit(i_incidence)=.false.
                  enddo
                  do i_incidence=1,n_incidence(isect,irow)
c     no need for testcond check below to see if track crossed row now
c       - mal 27may95
c     testcond = .false.
c     do iwire=1,numbv_row(irow)
c     testcond = (testcond.or.
c     +                      (mcptr((irow-1)*numbv_row_max+iwire).ne.0))
c     enddo
c     if (testcond) then
                     nmchit = nmchit + 1
                     if(irow.ge.40)then
                        nmchit2 = nmchit2 + 1
                     endif
c     endif
                     
                     if (nmchit .ne. oldmchit)then
                        oldmchit = nmchit
                        
                        vec(11) = real(irow)+100*real(isect)
                        vec(1) = real(itrk)
C     we now only look at mc hit on *center* wire over a padrow
C	--> no need to average
                        idummy=(isect-1)*mxrow+irow
                        vec(2)=mhits_tpc(mcptr(idummy,i_incidence)).x(1)
                        vec(3)=mhits_tpc(mcptr(idummy,i_incidence)).x(2)
                        vec(4)=mhits_tpc(mcptr(idummy,i_incidence)).x(3)

c for filling ntuple with LOCAL MC hit info...
c----------------------------------------------------------------
c this row/sector definition shit is going to drive me INSANE!!!!!!
                        if (isect.gt.12) then
                           ksectortemp = isect-12
                        else
                           ksectortemp = isect+12
                        endif
                        if (irow.gt.pad_plane(1).nrow_in) then
                           krowtemp = irow-pad_plane(1).nrow_in
                           ksectortemp = ksectortemp*2
                        else
                           krowtemp = irow
                           ksectortemp = ksectortemp*2 - 1
                        endif
ccc                        invec(2)=tpg_row_to_y(krowtemp,ksectortemp)
                        invec(1)=dble(vec(2))
                        invec(2)=dble(vec(3))
                        invec(3)=dble(vec(4))
                        kstatus=
     +                       tpg_global_to_local(ksectortemp,
     +                       invec,outvec)
                        vec2(1)=real(outvec(1))
                        vec2(2)=real(outvec(3))
                        vec2(3)=
     +                       mhits_tpc(mcptr(idummy,i_incidence)).itrk
                        vec2(4)=real(100*isect+irow)
                        call hfn(650+ihisto,vec2)
c----------------------------------------------------------------

c a similar transformation is also needed for me to calculate the
c  crossing angle alpha of the track, which is by definition in local
c  coordinates-- NOTE THAT I USE THE TPG ROUTINE NOW ON MOMENTA, NOT
C  COORDINATES... looking at source code, this should work, but this
c  kind of stuff can be tricky
                        invec(1) =
     +                       dble(
     +                       mhits_tpc(mcptr(idummy,i_incidence)).p(1))
                        invec(2) =
     +                       dble(
     +                       mhits_tpc(mcptr(idummy,i_incidence)).p(2))
                        invec(3) =
     +                       dble(
     +                       mhits_tpc(mcptr(idummy,i_incidence)).p(3))
                        kstatus=
     +                       tpg_global_to_local_p(ksectortemp,
     +                       invec,outvec)
                        if (real(outvec(2)).ne.0.0) then
                           vec(21)=atand(sngl(outvec(1)/outvec(2)))
                           vec(27)=atand(sngl(outvec(3)/outvec(2)))
                        else
                           vec(21)=90.0
                           vec(23)=90.0
                        endif

                        vec(17)=real(
     +			  mhits_tpc(mcptr(idummy,i_incidence)).id)
                        
                        
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
                           tpc_indexh.nok = tpc_indexh.nok+1
                           tpc_index(tpc_indexh.nok).type = 
     +                          tpc_index_type(1).tphit_mhitstpc
                           tpc_index(tpc_indexh.nok).key1 =
     +                          tphit(dtptr(idummy,jdummy)).id
                           tpc_index(tpc_indexh.nok).key2 = 
     +                          mhits_tpc(mcptr(idummy,i_incidence)).id
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
                           if (n_maybe_lost(idummy).gt.200) then
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
c                        vec(1) = real(itrk)
c                        vec(2) = 0.           ! mcx
c                        vec(3) = 0.           ! mcy
c                        vec(4) = 0.           ! mcz
c                        vec(5) = tphit(dtptr(idummy,i_incidence)).x
c                        vec(6) = tphit(dtptr(idummy,i_incidence)).y
c                        vec(7) = tphit(dtptr(idummy,i_incidence)).z
c                        vec(8) = 0.           ! delx
c                        vec(9) = 0.           ! dely
c                        vec(10)= 0.           ! delz
c                        vec(11)= real(irow)+100.0*real(isect)
c                        vec(12)= real(
c     +			  tphit(dtptr(idummy,i_incidence)).cluster)
c                        vec(13)= real(tpcluster(int(vec(12))).nseq)
c                        vec(14)= tphit(dtptr(idummy,i_incidence)).dx
c                        vec(15)= tphit(dtptr(idummy,i_incidence)).dy
c                        vec(16)= tphit(dtptr(idummy,i_incidence)).dz
c                        vec(17)= 0.           ! mc hit id
c find out how many mc tracks contributed to this hit's cluster...
c                        nmc_found=0
c                        iclstr=tphit(dtptr(idummy,i_incidence)).cluster
c                        isqu=tpcluster(iclstr).jseq
c 42                     ipx_first=tpseq(isqu).jpix
c                        idat=tppixel(ipx_first).datum
c                        npx=idat/numpix_factor
c                        do ipx=ipx_first,ipx_first+npx-1
c                           jmc=tpmcpix(ipx).mcid
c                           if (jmc.eq.0) goto 43
c                           do imc_found=1,nmc_found
c                              if (jmc.eq.mc_found(imc_found)) goto 43
c                           enddo
c                           nmc_found=nmc_found+1
c                           if (nmc_found.gt.nmc_found_max) then
c                              nmc_found=nmc_found_max
c                              goto 44 ! done searching for this cluster
c                           endif
c                           mc_found(nmc_found)=jmc
c 43                        continue ! already found that one
c                        enddo
c                        isqu=tpseq(isqu).next
c                        if (isqu.ne.0) goto 42
c 44                     continue ! ok, done searching cluster...
c                        vec(18) = real(nmc_found)
c                        call hfn(300+ihisto,vec)
                     endif
                  enddo
c
               enddo            ! do irow = 1, 45
            enddo               ! do isect=1,24
            
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
c     
c     perform truncation try lower 60%
c     
c     do indx = 1, nint(0.6*float(nmchit))
c     vec(2) = vec(2) + mchit_q(indx)
c     enddo
c     vec(2) = vec(2)/float(nint(0.6*float(nmchit)))
            
c     do indx = 1, nint(0.6*float(nhitrk))
c     vec(3) = vec(3) + tphit_q(indx)
c     enddo
c     vec(3) = vec(3)/float(nint(0.6*float(nhitrk)))
            
c     
c     it really only makes sense to plot dE/E since 
c     noise is constant wrt the min. ion. peak
c     
c     vec(4) = (vec(2)-vec(3))/vec(2)
            
            call hfn(400+ihisto,vec)
 99         continue
         enddo                  ! do itrk = 1, mkineh.nok
         
         call hrout(400+ihisto,icycle,' ')
         call hrout(650+ihisto,icycle,' ')

c now, run through a clean-up stage, where match-up between
c maybe "lost" or 
c  "ghost" hits is attempted; (hits that should be associated 
c with each other
c  may wind up as "lost"+"ghost" if the  MCID for the 
c reconstructed hit is
c  incorrectly assigned, which is easy enough)

         do isect=1,24
            do irow=1,45
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
                  vec(1) = mhits_tpc(ptr_lost).itrk
                  vec(2) = mhits_tpc(ptr_lost).x(1)
                  vec(3) = mhits_tpc(ptr_lost).x(2)
                  vec(4) = mhits_tpc(ptr_lost).x(3)
                  vec(17) = mhits_tpc(ptr_lost).id
c a similar transformation is also needed for me to calculate the
c  crossing angle alpha of the track, which is by definition in local
c  coordinates-- NOTE THAT I USE THE TPG ROUTINE NOW ON MOMENTA, NOT
C  COORDINATES... looking at source code, this should work, but this
c  kind of stuff can be tricky
                  invec(1) =
     +                 dble(mhits_tpc(mcptr(idummy,i_incidence)).p(1))
                  invec(2) =
     +                 dble(mhits_tpc(mcptr(idummy,i_incidence)).p(2))
                  invec(3) =
     +                 dble(mhits_tpc(mcptr(idummy,i_incidence)).p(3))
                  kstatus=tpg_global_to_local_p(ksectortemp,invec,outvec)
                  if (real(outvec(2)).ne.0.0) then
                     vec(21)=atand(sngl(outvec(1)/outvec(2)))
                     vec(27)=atand(sngl(outvec(3)/outvec(2)))
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
                     tpc_indexh.nok = tpc_indexh.nok+1
                     tpc_index(tpc_indexh.nok).type = 
     +                    tpc_index_type(1).tphit_mhitstpc
                     tpc_index(tpc_indexh.nok).key1 =
     +                    tphit(ptr_ghost).id
                     tpc_index(tpc_indexh.nok).key2 = 
     +                    mhits_tpc(ptr_lost).id
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
                     tpc_indexh.nok = tpc_indexh.nok+1
                     tpc_index(tpc_indexh.nok).type = 
     +                    tpc_index_type(1).tphit_mhitstpc
                     tpc_index(tpc_indexh.nok).key1 = 0
                     tpc_index(tpc_indexh.nok).key2 = 
     +                    mhits_tpc(mcptr(idummy,i_incidence)).id
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
                     tpc_indexh.nok = tpc_indexh.nok+1
                     tpc_index(tpc_indexh.nok).type = 
     +                    tpc_index_type(1).tphit_mhitstpc
                     tpc_index(tpc_indexh.nok).key1 =
     +                    tphit(ptr_ghost).id
                     tpc_index(tpc_indexh.nok).key2 = 0
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
            enddo               ! irow
         enddo                  ! isect

         call hrout(300+ihisto,icycle,' ')


c now make histos that will allow us to determine merging distance
c
c for MC and for recon hits, fill hisogram with distance to all neighbors
c  on a padrow for each hit on that padrow...

         do isect=1,24
            do irow=1,45
               idummy=(isect-1)*45+irow
c first do MC hits on this padrow/sector
               if (mc_ptr_list(idummy,0).ne.0) 
     +              weight=1.0/float(mc_ptr_list(idummy,0))
               do index1=1,mc_ptr_list(idummy,0)-1
                  do index2=index1+1,mc_ptr_list(idummy,0)
                     xydist = sqrt(
     +                  (mhits_tpc(mc_ptr_list(idummy,index1)).x(1)-
     +                  mhits_tpc(mc_ptr_list(idummy,index2)).x(1))**2 +
     +                  (mhits_tpc(mc_ptr_list(idummy,index1)).x(2)-
     +                  mhits_tpc(mc_ptr_list(idummy,index2)).x(2))**2)
                     zdist = abs(
     +                    mhits_tpc(mc_ptr_list(idummy,index1)).x(3)-
     +                    mhits_tpc(mc_ptr_list(idummy,index2)).x(3))
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
         do kclus=1,tpclusterh.nok
            vec(4)=kclus
            nseq_hit=tpcluster(kclus).nseq
            ktpc_row=tpcluster(kclus).tpc_row
            ksector = ktpc_row/100
            krow = ktpc_row - ksector*100
* this bullshit with the row number definitions has got to stop!
            if (ksector.gt.12) then
               ksector_hack=ksector-12
            else
               ksector_hack=ksector+12
            endif
            if (krow.gt.pad_plane(1).nrow_in) then
               krowtemp = krow-pad_plane(1).nrow_in
               ksectortemp = ksector_hack*2
            else
               krowtemp = krow
               ksectortemp=ksector_hack*2 - 1
            endif
            vec(5) = 100*ksector_hack + krow
            kseq = tpcluster(kclus).jseq
*
            do kseq_count=1,nseq_hit
               kpad = tpseq(kseq).secpad - id_pad(krow)
               invec(1)=tpg_pad_to_x(kpad,krowtemp,ksectortemp)
               vec(1) = real(invec(1))
               kpix = tpseq(kseq).jpix
               kdatum=tppixel(kpix).datum
               numpix_seq=kdatum/numpix_factor ! 24mar95 malisa
               do kpix_count=1,numpix_seq
                  kdatum=tppixel(kpix).datum
                  ktdc = (kdatum - numpix_factor*numpix_seq)/tdc_factor
                  kadc = kdatum - numpix_factor*numpix_seq - 
     +                 tdc_factor*ktdc
                  kmcid = tpmcpix(kpix).mcid
                  vec(3)=kadc
                  invec(3) = dble(ktdc*time_bucket)
                  vec(2) = real(invec(3))
                  call hfn(500+ihisto,vec)
                  kpix=kpix+1
               enddo
               kseq = tpseq(kseq).next
            enddo
         enddo
         call hrout(500+ihisto,icycle,' ')

c now fill another ntuple (600+ihisto) with "LOCAL" hit coordinates

         do khit=1,tphith.nok
            vec(3) = tphit(khit).id
            kclus = tphit(khit).cluster
            vec(4) = kclus
            invec(1) = dble(tphit(khit).x)
            invec(2) = dble(tphit(khit).y)
            invec(3) = dble(tphit(khit).z)
            ktpc_row = tpcluster(kclus).tpc_row
	    vec(5) = tphit(khit).row
            ksector = ktpc_row/100
c     DAMN the STUPID conventions in tpg!!!! Now I must "undo" the
c     **correction** I do at the end of TPH!!!
            krow = ktpc_row - ksector*100
c     if (ksector.gt.12) then
c     ksector_hack=ksector-12
c     else
c     ksector_hack=ksector+12
c     endif
            ksector_hack = ksector
c     invec(3)=-invec(3)
            if (krow.gt.pad_plane(1).nrow_in) then
               krowtemp = krow-pad_plane(1).nrow_in
               ksectortemp = ksector_hack*2
            else
               krowtemp = krow
               ksectortemp=ksector_hack*2 - 1
            endif
            kstatus=tpg_global_to_local(ksectortemp,invec,outvec)
            vec(1) = real(outvec(1))
            vec(2) = real(outvec(3))
            call hfn(600+ihisto,vec)
         enddo
         call hrout(600+ihisto,icycle,' ')

         goto 989


        do khit=1,tphith.nok
	  kclus = tphit(khit).cluster
	  invec(1)=dble(tphit(khit).x)
	  invec(2)=dble(tphit(khit).y)
	  invec(3)=dble(tphit(khit).z)
	  nseq_hit = tpcluster(kclus).nseq
	  kseq = tpcluster(kclus).jseq
	  ktpc_row = tpcluster(kclus).tpc_row
	  ksector = ktpc_row/100
c DAMN the STUPID conventions in tpg!!!! Now I must "undo" the
c  **correction** I do at the end of TPH!!!
	  krow = ktpc_row - ksector*100
c	  if (ksector.gt.12) then
c             ksector_hack=ksector-12
c          else
c             ksector_hack=ksector+12
c          endif
	  ksector_hack = ksector
c	  invec(3)=-invec(3)
	  if (krow.gt.pad_plane(1).nrow_in) then
	    krowtemp = krow-pad_plane(1).nrow_in
            ksectortemp = ksector_hack*2
	  else
	    krowtemp = krow
            ksectortemp=ksector_hack*2 - 1
	  endif
	  kstatus=tpg_global_to_local(ksectortemp,invec,outvec)
	  vec(15)=real(outvec(1))
	  vec(16)=real(outvec(2))
	  vec(17)=real(outvec(3))
	  vec(1)=khit
	  vec(2)=kclus
	  vec(3)=ksector
	  vec(4)=krow
	  invec(2)=tpg_row_to_y(krowtemp,ksectortemp)
	  vec(13) = real(invec(2))
	  do kseq_count=1,nseq_hit
	    kpad = tpseq(kseq).secpad - id_pad(krow)
	    kpix = tpseq(kseq).jpix
	    kdatum=tppixel(kpix).datum
c	    numpix_seq=ibits(kdatum,8,6)
            numpix_seq=kdatum/numpix_factor ! 24mar95 malisa
	    vec(5)=kpad
	    invec(1)=tpg_pad_to_x(kpad,krowtemp,ksectortemp)
	    vec(12) = real(invec(1))
	    do kpix_count=1,numpix_seq
	      kdatum=tppixel(kpix).datum
c	      kadc = ibits(kdatum,16,16)
c	      ktdc = ibits(kdatum,0,8) + 256*ibits(kdatum,14,2)
              ktdc = (kdatum - numpix_factor*numpix_seq)/tdc_factor
              kadc = kdatum - numpix_factor*numpix_seq - tdc_factor*ktdc
	      kmcid = tpmcpix(kpix).mcid
	      vec(6)=ktdc
	      vec(7)=kadc
	      vec(8)=kmcid
	      invec(3) = dble(ktdc*time_bucket)
	      vec(14) = real(invec(3))
	      kstatus=tpg_local_to_global(ksectortemp,invec,outvec)
	      if (.not.kstatus)
     +		write(6,*)'Global transform failed!!',ksector
	      vec(9)=real(outvec(1))
	      vec(10)=real(outvec(2))
	      vec(11)=real(outvec(3))
	      call hfn(500+ihisto,vec)
	      kpix=kpix+1
	    enddo
	    kseq = tpseq(kseq).next
	  enddo
	enddo

 119	continue

         call hrout(500+ihisto,icycle,' ')


 989     continue

c        if( mctrl(1).nevt .eq. mctrl(1).mxevt)then
c           call hrend('ntup')
c           close(23)
c        endif
c
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



