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
     +,			      detectorh, detector )

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

         integer nquota,nvar,icycle,nvar2,nvar3
         character*4 chtags(9),chtags2(18),chtags3(5)
C-----------------------------------------------------------------------
         logical lstat,first

         real vec(18)

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
         integer nvar4
         character*4 chtags4(17)
         integer khit,kclus,nseq_hit,ktpc_row,kseq_count
         integer ksector,krow,kseq,kpix,kdatum,kpad,ksector_hack
         integer numpix_seq,kadc,ktdc,kpix_count
         integer kmcid,krowtemp,ksectortemp
         logical kstatus,tpg_local_to_global,tpg_global_to_local
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

C-----------------------------------------------------------------------
         data nvar/9/,nvar2/18/,nvar3/5/,nvar4/17/
         data chtags/'itrk','itcl','nrow','x','y','z','prf','zrf','q'/
         data chtags2/'itrk','mcx ','mcy ','mcz ','tpx ','tpy ','tpz '
     +,              'delx','dely','delz','nrow', 'clus','nseq',
     +                'dx','dy','dz','mhit','nmc'/
         data chtags3/'prec','mce','tpe','rde','pre2'/
	 data chtags4/'hit ','clus','sect','row ','pad ',
     +		      'tdc ','adc ','mcid','xpix','ypix','zpix',
     +		      'xloc','yloc','zloc','xh_l','yh_l','zh_l'/
         data first/.true./,ihisto/0/
C-----------------------------------------------------------------------

c these are for determining how many mc tracks contributed pixels to a
c  given cluster... added 23jun95 mal
         integer iclstr,isqu,ipx,ipx_first
         integer idat,npx,jmc,imc_found,nmc_found
         integer nmc_found_max
         parameter (nmc_found_max=10)
         integer mc_found(1:nmc_found_max)


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

          call hropen(23,'NTUP',outfile,'N',5120,ISTAT)
	  if(status.ne.0)write(*,*)' no rz file'
C

c things needed for the ntuple 500+ihisto...
 	    time_bucket = detector(1).drift_length/tsspar(1).ntime
            id_pad(1) = 0
            do irow = 2, mxrow
               id_pad(irow) = id_pad(irow-1)
     +                        + pad_plane(1).npads(irow-1)
            enddo


         endif

c         call hcdir('//NTUP',' ')

         ihisto = ihisto+1
c ntuple 200 as defined has no more information than the hits table...
*         call hbookn(200+ihisto,'Hit Info',nvar,'NTUP',8000,
*     +           chtags)
         call hbookn(300+ihisto,'Compare Info',nvar2,'NTUP',17000,
     +              chtags2)
         call hbookn(400+ihisto,'Track Info',nvar3,'NTUP',4000,
     +              chtags3)
         call hbookn(500+ihisto,'Hit Pixel Info',nvar4,'NTUP',17000,
     +              chtags4)
c        
c        proceed with evaluation...
c
*         do indx = 1, tphith.nok
c     
*            vec(1)=real(tphit(indx).track)
*            vec(2)=real(tphit(indx).cluster)
*            vec(3)=real(tphit(indx).row)
*            vec(4)=tphit(indx).x
*            vec(5)=tphit(indx).y
*            vec(6)=tphit(indx).z
*            vec(7)=tphit(indx).prf
*            vec(8)=tphit(indx).zrf
*            vec(9)=tphit(indx).q

*            call hfn(200+ihisto,vec)
*         enddo
c     
c->      close ntuple file...
c
*         call hrout(200+ihisto,icycle,' ')
c        call hrend('ntup')
c        close(23)
c
c        the following works only for sparse events!
c        more efficient and flexible would sort primary 
c        keys on itrk, etc...
c
c ok now we try the above suggestion....
c
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
         enddo
 19      ihit_recon=ihit_recon-1
c
         ihit_mc =0
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
                  nmctrk = nmctrk + 1
c     mcptr((irow-1)*numbv_row_max+iwire) = ihit_mc
               endif
               goto 1
            else
               ihit_mc=ihit_mc-1
            endif
 2          continue
            
c     do ihit = 1, mhits_tpch.nok
c     if(mhits_tpc(ihit).itrk.eq.itrk)then
c     nmctrk = nmctrk + 1
c     numbv = mhits_tpc(ihit).numbv
c note that iwire no longer refers to wire# - it is subvolume over row.
c     iwire = numbv/10000
c     isect = (numbv-10000*iwire)/100
c     irow = (numbv-10000*iwire-100*isect)
            
c     mcptr((irow-1)*numbv_row_max+iwire) = ihit
c     endif
c     enddo
c     write(6,*)' ',nmctrk,' mchits on itrk = ',itrk
c     
            
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
c     dtptr(kdummy) = ihit_recon
c     tphit_q(nhitrk)=tphit(ihit_recon).q
               goto 3
            else
               ihit_recon=ihit_recon-1
            endif
 4          continue
            
c     do ihit = 1, tphith.nok
c     if(tphit(ihit).track.eq.itrk)then
c     nhitrk = nhitrk + 1
c     if(tphit(ihit).row.ge.40)then
c     nhitrk2 = nhitrk2 + 1
c     endif
c     c
c     c     stupidity for HPUX compiler
c     c     
c     c     dtptr(tphit(ihit).row) = ihit
c     kdummy = tphit(ihit).row
c     kdummy = mod(kdummy,100) ! now row=100*SECTOR+ROW mal 1mar95
c     dtptr(kdummy) = ihit
c     tphit_q(nhitrk)=tphit(ihit).q
c     endif
c     enddo
c     write(6,*)' ',nhitrk,' tphits on itrk = ',itrk
            
            if (nmctrk.eq.0) goto 99
            
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
                        
                        vec(11) = real(irow)
                        vec(1) = real(itrk)
                        vec(2) = 0.
                        vec(3) = 0.
                        vec(4) = 0.
C     we now only look at mc hit on *center* wire over a padrow
C	--> no need to average
C       over wire positions as is done in commented-out section below
C---------------------
C     wire_hits = 0
Cc      a philosophical point: could weight the x,y,z averages
Cc      by dE.  To do this, remove the *'s
C     desum = 0.0
C     do kwir=1,numbv_row(irow)
C     if(mcptr((irow-1)*numbv_row_max+kwir).ne.0)then
C     vec(2)=vec(2)
C     +                            +mhits_tpc(mcptr((irow-1)*
C     + 			   numbv_row_max+kwir)).x(1)
C     *     +              *mhits_tpc(mcptr((irow-1)*numbv_row_max+kwir)).de
C     vec(3)=vec(3)
C     +                            +mhits_tpc(mcptr((irow-1)*numbv_row_max+kwir)).x(2)
C     *     +              *mhits_tpc(mcptr((irow-1)*numbv_row_max+kwir)).de
C     vec(4)=vec(4)
C     +                            +mhits_tpc(mcptr((irow-1)*numbv_row_max+kwir)).x(3)
C     *     +              *mhits_tpc(mcptr((irow-1)*numbv_row_max+kwir)).de
C     mchit_q(nmchit)=mchit_q(nmchit)
C     +                            +1.0e06*mhits_tpc(mcptr((irow-1)*numbv_row_max+kwir)).de
C     wire_hits = wire_hits + 1
C     desum = desum + 
C     +                            mhits_tpc(mcptr((irow-1)*numbv_row_max+kwir)).de
C     endif
C     enddo
C     *     if(desum.gt.0.0)then
C     if(wire_hits.gt.0)then
C     vec(2)=vec(2)/float(wire_hits)
C     vec(3)=vec(3)/float(wire_hits)
C     vec(4)=vec(4)/float(wire_hits)
C     *     vec(2)=vec(2)/desum
C     *     vec(3)=vec(3)/desum
C     *     vec(4)=vec(4)/desum
C     endif
C---------------------
                        idummy=(isect-1)*mxrow+irow
                        vec(2)=mhits_tpc(mcptr(idummy,i_incidence)).x(1)
                        vec(3)=mhits_tpc(mcptr(idummy,i_incidence)).x(2)
                        vec(4)=mhits_tpc(mcptr(idummy,i_incidence)).x(3)
                        vec(17)=real(
     +			  mhits_tpc(mcptr(idummy,i_incidence)).id)
                        
c     if(irow .le. pad_plane(1).nrow_in)then
c     if(irow .le. 13)then
c     mchit_q(nmchit)=mchit_q(nmchit)*tsspar(1).gain_in
c     else
c     mchit_q(nmchit)=mchit_q(nmchit)*tsspar(1).gain_out
c     endif
c     
c     correct for integral
c     
c     mchit_q(nmchit) = 8.*mchit_q(nmchit)
                        
c     now must find the reconstructed hit on this row and sector associated with this track
c     that is *closest* to the generated mc hit (but it must be *somewhat* close - i.e.
c     within 2 cm.)
                        idummy=mxhits*(isect-1)+irow
                        closest=100.0
                        do kdummy=1,dtptr(idummy,0)
                           distance=sqrt( 
     +                        (tphit(dtptr(idummy,kdummy)).x-vec(2))**2+
     +                        (tphit(dtptr(idummy,kdummy)).y-vec(3))**2+
     +                        (tphit(dtptr(idummy,kdummy)).z-vec(4))**2)
                           if (distance.lt.closest) then
                              closest=distance
                              jdummy=kdummy
                           endif
                        enddo
c     if closest>2.0) then there is no nearby reconstructed hit
                        
                        if(closest.lt.2.0)then
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
c ok we have found a reconstructed hit... how many mc tracks contributed
c  to its cluster???
                           nmc_found=0
                           iclstr=tphit(dtptr(idummy,jdummy)).cluster
                           isqu=tpcluster(iclstr).jseq
 32                        ipx_first=tpseq(isqu).jpix
                           idat=tppixel(ipx).datum
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
                                 goto 34 ! done searching for this cluster
                              endif
                              mc_found(nmc_found)=jmc
 33                           continue ! already found that one
                           enddo
                           isqu=tpseq(isqu).next
                           if (isqu.ne.0) goto 32
 34                        continue ! ok, done searching cluster...
                           vec(18) = real(nmc_found)
                        else
                           vec(5) = 0.
                           vec(6) = 0.
                           vec(7) = 0.
                           vec(8) = 0.
                           vec(9) = 0.
                           vec(10) = 0.
                           vec(12) = 0.
                           vec(13) = 0.
                           vec(14) = 0.
                           vec(15) = 0.
                           vec(16) = 0.
                           vec(18) = 0.
                        endif
                        call hfn(300+ihisto,vec)
                        
                     endif      ! if(nmchit .ne. oldmchit)
                  enddo         ! do i_incidence=1,n_incidence
c ok, that's the end of this track on this sector/row, so any hit who claims
c  to belong to the current track, but has not been associated with a mc hit,
c  is a "ghost" hit... fill the ntuple with these guys now.
                  idummy=mxhits*(isect-1)+irow
                  do i_incidence=1,dtptr(idummy,0)
                     if (.not.used_recon_hit(i_incidence)) then
                        vec(1) = real(itrk)
                        vec(2) = 0.           ! mcx
                        vec(3) = 0.           ! mcy
                        vec(4) = 0.           ! mcz
                        vec(5) = tphit(dtptr(idummy,i_incidence)).x
                        vec(6) = tphit(dtptr(idummy,i_incidence)).y
                        vec(7) = tphit(dtptr(idummy,i_incidence)).z
                        vec(8) = 0.           ! delx
                        vec(9) = 0.           ! dely
                        vec(10)= 0.           ! delz
                        vec(11)= real(irow)
                        vec(12)= real(
     +			  tphit(dtptr(idummy,i_incidence)).cluster)
                        vec(13)= real(tpcluster(int(vec(12))).nseq)
                        vec(14)= tphit(dtptr(idummy,i_incidence)).dx
                        vec(15)= tphit(dtptr(idummy,i_incidence)).dy
                        vec(16)= tphit(dtptr(idummy,i_incidence)).dz
                        vec(17)= 0.           ! mc hit id
c find out how many mc tracks contributed to this hit's cluster...
                        nmc_found=0
                        iclstr=tphit(dtptr(idummy,i_incidence)).cluster
                        isqu=tpcluster(iclstr).jseq
 42                     ipx_first=tpseq(isqu).jpix
                        idat=tppixel(ipx).datum
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
 43                        continue ! already found that one
                        enddo
                        isqu=tpseq(isqu).next
                        if (isqu.ne.0) goto 42
 44                     continue ! ok, done searching cluster...
                        vec(18) = real(nmc_found)
                        call hfn(300+ihisto,vec)
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
         call hrout(300+ihisto,icycle,' ')

c now fill pixel data for each hit - added 28nov94 mal
c	goto 119

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



