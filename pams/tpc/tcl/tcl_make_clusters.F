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
        INTEGER FUNCTION TCL_Make_Clusters( pad_plane_h, pad_plane,
     1     tppadh, tppad, tppixelh, tppixel, tpmcpixh, 
     1     tpmcpix, tpclusterh, tpcluster, tpseqh, tpseq )

	IMPLICIT NONE

C   Input arguments  (One to a line with definition after ! ) 
C
C     pad_planeh:
C     pad_plane:
C     tppad_h:
C     tppad:
C     tpppixelh
C     tppixel:
C     tpmcpixh
C     tpmcpix:
C
C   Output arguments : (values in the following tables)
C
C     tpclusterh
C     tpcluster:
C     tpseqh
C     tpseq:
C
C  Returns:
C       TAS user status code, defined in tas_user_codes_inc, one of:
C       tucod_ok       !General successful completion.
C       tucod_fail     !General failure.
C       tucod_abevt    !Abort event.
C       tucod_abgo     !Abort GO event looping.
C       tucod_exit     !Force program termination.
C       tucod_unkmode  !Unknown mode in call to TAE_USER.
C     
C     notes about output into tpcluster and tpseq...
C
C   Functional Description : 
C
C   top-level analysis module which groups together pad 
C   hits (sequences in the temporal dimension) into clusters 
C   which are subsequently converted into reconstructed 
C   tpc hit coordinates
C
C   TAS 22-25 April-1994   Richard Morse
C   Copyright 1994    Lawrence Berkeley Laboratory
c   
c   modificattions:
c   ==============
c   22-25 April started with framework provided by Bob 
c   Hackenberg (TCL_MAKE_CLUSTERS) and then proceeded to 
c   recode algorithm to use TCL_CLSROW and TCL_GET_ROW_SEQ; 
c   "sequence" orientation of algorithm is much more 
c   straight-forward and actually worked (in practice!) for 
c   multi-track events
c
c  19nov94 MALisa - jtppad no longer passed from here through tcl_clsrow
c			to tcl_get_row_seq
c
c  24mar95 malisa - now tppixel.datum packs data in the following form:
c 	datum = adc + tdc*'1000'X + nseqpix*'1000000'X
c
c  29mar95 malisa - routine that finds clusters in a padrow, tcl_clsrow
c       has been totally re-written.. the old routine was broken and
c       quite complicated==hard to fix.  The new one is much simpler and
c       more flexible, and not much slower.
c
c  30mar95 malisa - from now on, when filling sector,row information in
c       tables (like tpseq,tpcluster..) sector=1..24, row=1..45
c       But, from TSS, sector=1..48, row=1..45
c
c  31mar95 malisa - the name of the field in the cluster table TPCLUSTER that tells
c  	the row and sector of the cluster is changed from SECPAD to TPC_ROW
c
c  4apr95 malisa - TSS now outputs data in the agreed-upon format--> remove "front end"
c  	that was built into tcl_make_clusters, and also now read the data as
c	TPPIXEL.DATUM = adc + tdc*'400'X + nseqpix*'100000'X
c
#include "tas_structures.inc"
#include "tas_user_codes.inc"

#include "tpg_pad_plane_pars.inc"
#include "tpg_pad_plane_st.inc"

#include "tss_tppad_pars.inc"
#include "tss_tppad_st.inc"

#include "tss_tppixel_pars.inc"
#include "tss_tppixel_st.inc"

#include "tss_tpmcpix_pars.inc"
#include "tss_tpmcpix_st.inc"

#include "tcl_tpcluster_pars.inc"
#include "tcl_tpcluster_st.inc"

#include "tcl_tpseq_pars.inc"
#include "tcl_tpseq_st.inc"

*  Inputs:
        RECORD/ table_head_st      / pad_plane_h 
        RECORD/ pad_plane_row_st   / pad_plane(*)

        RECORD / table_head_st     / tppadh     !header for tppad table
        RECORD / tppad_row_st      / tppad(*)   !rows of tppad table
        RECORD / table_head_st     / tppixelh   !header for tppixel table
        RECORD / tppixel_row_st    / tppixel(*) !rows of tppixel table
        RECORD / table_head_st     / tpmcpixh   !header for tpmcpix table
        RECORD / tpmcpix_row_st    / tpmcpix(*) !rows of tpmcpix table

*  Outputs:
        RECORD / table_head_st     / tpclusterh   !header for cluster table
        RECORD / tpcluster_row_st  / tpcluster(*) !rows of cluster table
        RECORD / table_head_st     / tpseqh       !header for tpseq table
        RECORD / tpseq_row_st      / tpseq(*)     !rows of tpseq table

C----------------------------------------------------------------------

	INTEGER Nsectors, Nrows
	INTEGER Row
	INTEGER Sector
	INTEGER TAS_Code

	INTEGER TPG_Nrows_in_Sector
	INTEGER TPG_Nsectors
	INTEGER TCL_Make_Cluster_Row
	INTEGER TCL_ClsRow
	LOGICAL MSG_Enabled_Trace

	INTEGER IDE1
	SAVE    IDE1

	DATA    IDE1 /0/

        integer padptr(1:45,1:24),itpp

c     the following are just temporary
        integer itpp,jjpix,numseq,istart_seq,numpix,idatum
        integer iseq,iadc,itdc,ipix,itdc_last

C----------------------------------------------------------------------

	TCL_Make_Clusters = tucod_ok

*       CALL MESSAGE(
*    +               'TCL_Make_Clusters-I1  Making TCL table "cluster"'
*    +               //'from "tppad" et al.' , 1, -1 )

*	Need a call like this:
*	Nsectors = TPG_Nsectors()

*	But for now:
c	Nsectors = 48 !Inner/outer are 2 sectors.
	Nsectors = 24 !Inner/outer are 1 sector -- mal 30mar95

c--------------------------------------- FRONT END ------------------------------
c     OK, here, I build on a temporary "front-end" to the TCL package that
c     reformats the tppixel table into the new format that Wen and I agree
c     on.... eventually (soon), this will be done in TSS...
c
c FIRST, Wen now points to the *end* of the data on a pad with tppad.
c  this is wrong and will be corrected, but for now I correct the table myself

c	do itpp=tppadh.nok,2,-1
c           tppad(itpp).jpix = tppad(itpp-1).jpix +1
c        enddo
c	tppad(1).jpix = 1
c
c SECOND, Wen no longer fills tppad.nseq at al
c this has to be corrected, but for now, I do it myself...
c
c	do itpp=1,tppadh.nok
c           numseq = 0
c           itdc_last = -1
c           do ipix=tppad(itpp).jpix,tppad(itpp+1).jpix-1
c              idatum = tppixel(ipix).datum
c              itdc = idatum/'10000'X
c              if (itdc.ne.itdc_last+1) numseq=numseq+1
c              itdc_last = itdc
c           enddo
c           tppad(itpp).nseq = numseq
c        enddo
c
c THIRD, the information about npix_seq has been discarded from tppixel table
c this should be kept, but for now, I put it back myself...
c        do itpp=1,tppadh.nok
c           jjpix = tppad(itpp).jpix
c           numseq = tppad(itpp).nseq
c           do iseq=1,numseq
c              istart_seq = jjpix
c              numpix = 1
c              idatum = tppixel(jjpix).datum
c     first, count number of pixels in sequence....
c              itdc = idatum/'10000'X
c 1            jjpix=jjpix+1
c              itdc_last = itdc
c              idatum = tppixel(jjpix).datum
c              itdc = idatum/'10000'X
c              if ((itdc.eq.itdc_last+1).and.
c     +             (jjpix.lt.tppad(itpp+1).jpix)) then
c                 numpix = numpix+1
c                 goto 1
c              endif
cc     now reformat...
c              do ipix=istart_seq,jjpix-1
c                 idatum = tppixel(ipix).datum
c                 itdc = idatum/'10000'X
c                 iadc = idatum - itdc*'10000'X
c                 tppixel(ipix).datum = iadc + itdc*'1000'X + numpix*'1000000'X
c              enddo
c           enddo
c        enddo
c
c        write(6,*)'done with front end'
c--------------------------- END OF FRONT END --------------------------------


c     now we are filling the pad pointer array for each event in this top-level
c     routine, instead of once and for all in tcl_get_row_seq.  This is to allow
c     filling of TPPAD table only for those pads that have data on them, instead
c     of requiring that each of the 136560 pads have an entry.  This also allows
c     arbitrary ordering of pads in TPPADS, as well as readout of less than full
c     TPC (as in sector test) - 24mar95 malisa

c     Note that tppad uses sectors #1-48 but rows #1-13 for inner (odd number) sectors
c     and rows #14-45 for outer (even number) sectors, when it fills tppad.tpc_row .
c     Meanwhile, the routines here (and henceforth) use sectors #1-24, and
c     rows #1-48, to be consistent with current use after tphit table.
c     Meanwhile, the routines here (and henceforth) use sectors #1-48 and 
c     rows #1-13 for inner and rows #1-32 for outer sectors.
c     This is accounted for in following pointer array.

c     first, clear it....
        do sector=1,nsectors
c           nrows = tpg_nrows_in_sector(sector)
           nrows = 45           ! mal 30mar95
           do row=1,nrows
              padptr(row,sector)=0
           enddo
        enddo
c     then, fill it....
        do itpp=1,tppadh.nok
           sector = tppad(itpp).tpc_row/100
           row = tppad(itpp).tpc_row - 100*sector
           sector = (sector+1)/2                      ! to make range 1..24
c           if (row.gt.pad_plane(1).nrow_in) row=row - pad_plane(1).nrow_in
           if (padptr(row,sector).eq.0) padptr(row,sector) = itpp
        enddo
c     at this point, any element of padptr which =0 is a pad without 
c     valid data for this event.


	DO Sector = 1, Nsectors
c mal 30mar95           Nrows = TPG_Nrows_in_Sector( Sector )
           nrows=45

           DO Row = 1, Nrows

c	    TAS_Code = TCL_Make_Cluster_Row( Sector, Row 
c    1                   , pad_plane_h, pad_plane
c    1	                 , tppadh, tppad, tppixelh, tppixel, tpmcpixh
c    1	                 , tpmcpix, Jtppad
c    1	                 , tpclusterh, tpcluster, tpseqh, tpseq )

              TAS_Code = TCL_ClsRow( Sector, Row 
     1             , pad_plane_h, pad_plane
     1             , tppadh, tppad, tppixelh, tppixel, tpmcpixh
     1             , tpmcpix
     1             , tpclusterh, tpcluster, tpseqh, tpseq
     1             , padptr )
    
              IF ( TAS_Code .NE. tucod_ok ) THEN
               CALL Message( 'TCL_Make_Clusters-E1  Failed.', 1, IDE1 )
               TCL_Make_Clusters = TAS_Code
               RETURN
              END IF
              
           END DO               !Row = 1, Nrows

	END DO                  !Sector = 1, Nsectors

        
	RETURN
	END

C----------------------------------------------------------------------
C
        INTEGER FUNCTION TCL_Clsrow( Sector, Row
     1                 , pad_plane_h, pad_plane
     1	               , tppadh, tppad, tppixelh, tppixel, tpmcpixh
     1	               , tpmcpix
     1	               , tpclusterh, tpcluster, tpseqh, tpseq, padptr )

	IMPLICIT NONE

C   Input arguments  (One to a line with definition after ! ) 
C
C     Sector:
C     Row:
C     pad_planeh:
C     pad_plane:
C     tppad_h:
C     tppad:
C     tpppixelh
C     tppixel:
C     tpmcpixh
C     tpmcpix:
C
C   Output arguments : (values in the following tables)
C
C     tpclusterh
C     tpcluster:
C     tpseqh
C     tpseq:
C
C  Returns:
C       TAS user status code, defined in tas_user_codes_inc, one of:
C       tucod_ok       !General successful completion.
C       tucod_fail     !General failure.
C       tucod_abevt    !Abort event.
C       tucod_abgo     !Abort GO event looping.
C       tucod_exit     !Force program termination.
C       tucod_unkmode  !Unknown mode in call to TAE_USER.
C     
C     notes about output into tpcluster and tpseq...
C
C   Functional Description : 
C
C   low-level analysis module which groups together pad 
C   hits (sequences in the temporal dimension) into clusters 
C   for each individual SECTOR,ROW; these are subsequently 
C   converted into reconstructed tpc hit coordinates
C
C **This is a totally new clustering algorithm-- the old one did not
C   work, and was too complicated to debug.  This is a simpler algorithm
C   and should not be TOO slow.
C
C   TAS 30 Mar 1995 Mike Lisa
C   Copyright 1995    Lawrence Berkeley Laboratory
c   
c   modificattions:
c   ==============
c
#include "tas_structures.inc"
#include "tas_user_codes.inc"

#include "tpg_pad_plane_pars.inc"
#include "tpg_pad_plane_st.inc"
          
#include "tss_tppad_pars.inc"
#include "tss_tppad_st.inc"

#include "tss_tppixel_pars.inc"
#include "tss_tppixel_st.inc"

#include "tss_tpmcpix_pars.inc"
#include "tss_tpmcpix_st.inc"

#include "tcl_tpcluster_pars.inc"
#include "tcl_tpcluster_st.inc"

#include "tcl_tpseq_pars.inc"
#include "tcl_tpseq_st.inc"

*  Inputs:
	INTEGER                      Sector     !Sector no. of current data set.
	INTEGER                      Row        !Row no. of current data set.
        RECORD/ table_head_st      / pad_plane_h
        RECORD/ pad_plane_row_st   / pad_plane(*)
        RECORD / table_head_st     / tppadh     !header for tppad table
        RECORD / tppad_row_st      / tppad(*)   !rows of tppad table
        RECORD / table_head_st     / tppixelh   !header for tppixel table
        RECORD / tppixel_row_st    / tppixel(*) !rows of tppixel table
        RECORD / table_head_st     / tpmcpixh   !header for tpmcpix table
        RECORD / tpmcpix_row_st    / tpmcpix(*) !rows of tpmcpix table
        integer padptr(1:45,1:24)               !pointer to pads inside tppad table


*  Outputs:
        RECORD / table_head_st     / tpclusterh   !header for cluster table
        RECORD / tpcluster_row_st  / tpcluster(*) !rows of cluster table
        RECORD / table_head_st     / tpseqh       !header for tpseq table
        RECORD / tpseq_row_st      / tpseq(*)     !rows of tpseq table

C----------------------------------------------------------------------

        logical tcl_get_row_seq
        integer ide1,ide2

	INTEGER     PAD_TDC_max
	PARAMETER ( PAD_TDC_max = 1023 ) !Max. no. of TDC bins.

	INTEGER     Nrowseqs_max
	PARAMETER ( Nrowseqs_max = 40000 ) !Max. #sequences in one row.

	INTEGER Nclusters
	INTEGER Nrowseqs !No. of pixels in this row, unpacked by PADUNP.

*	The following arrays are returned by the routine tcl_get_row_seq
*	and describe sequences on this padrow...
*
	INTEGER Ipix( Nrowseqs_max ) !Offset into raw pixel table, tppixel.
	INTEGER Ipad( Nrowseqs_max ) !Pad nos. in row of each seq.
	INTEGER Itlo( Nrowseqs_max ) !TDC (lo) integer values of each seq.
	INTEGER Ithi( Nrowseqs_max ) !TDC (hi) integer values of each seq.
	INTEGER Itid( Nrowseqs_max ) !Track IDs of each seq.

c	The following are local arrays used to group the sequences into
c	clusters....
	integer max_nseqs_on_pad,max_pads_in_row
	parameter(max_nseqs_on_pad=100,max_pads_in_row=184)
c number of sequences on a pad...
	integer*2 nseqs_pad(1:max_pads_in_row)
c for sequence on each pad, a pointer into the arrays returned 
c   by tcl_get_row_seq (see above)...
	integer*2 pad_seqptr(1:max_pads_in_row,1:max_nseqs_on_pad)
c tells whether a sequence is already claimed by a cluster...
	logical used(1:max_pads_in_row,1:max_nseqs_on_pad)

        integer nseq_in_cluster,max_nseq_in_cluster
        parameter(max_nseq_in_cluster=200)
        integer seq_in_cluster(1:max_nseq_in_cluster)
        integer ipad_seed,itlo_seed,ithi_seed

	integer jpad,iseq,jpad_seq,seed_sequence
	integer pads_in_row,row_temp
        integer ipad_seq,iptr

	logical first/.true./
	integer jsect,jrow,secpad_offset(1:45)

c------ must set up pad offset array to take care of this stupid
c   secpad offset for each row....

	if (first) then
	  first = .false.
          secpad_offset(1) = 0
          do jrow=2,45
             secpad_offset(jrow)=secpad_offset(jrow-1)+
     +		pad_plane(1).npads(jrow-1)
          enddo
       endif
c-------------------

C----------------------------------------------------------------------
c
c       Must initialise value of tcl_clsrow!
c
	Tcl_Clsrow = tucod_ok     !Success

c see if it is worth it first...
        if (tpclusterh.nok.eq.tpcluster_len) then
           write(6,*)'TPCLUSTER table full - not searching sec,row',sector,row
           return
        endif
        if (tpseqh.nok.eq.tpseq_len) then
           write(6,*)'TPSEQ table full - not searching sec,row',sector,row
           return
        endif

	IF ( .NOT. TCL_Get_Row_Seq
     1                 ( Sector, Row, Nrowseqs_max
     1                 , pad_plane_h, pad_plane
     1	               , tppadh, tppad, tppixelh, tppixel, tpmcpixh
     1	               , tpmcpix
     1	               , Nrowseqs, Ipix, Ipad, Itlo, Ithi, Itid
     1	               , padptr )               ) THEN


	write(6,*)'Sect + row : ',sector,row,' nrowseqs ',nrowseqs

          CALL Message( 'Tcl_Clsrow-E1  Failed to load pixels'
     1                //'for a row.', 1, IDE1 )

	  Tcl_Clsrow = tucod_fail     !General failure.
	  RETURN

	END IF ! .NOT. TCL_Get_Row_Seq

	if (nrowseqs.eq.0) return	! nothing in this row

        pads_in_row = pad_plane(1).npads(row)

c remove secpad offset from ipad array returned by tcl_get_row_seq
c  (this offset will have to be put back when storing the cluster)
	do iseq=1,nrowseqs
	  ipad(iseq) = ipad(iseq)-secpad_offset(row)
          if ((ipad(iseq).lt.0).or.(ipad(iseq).gt.pads_in_row)) 
     +         write(6,*)'*********** ipad major confusion!!!'
	enddo

c reset nseqs_pad array...
	do jpad=1,pads_in_row
	  nseqs_pad(jpad) = 0
	enddo
c now fill local arrays
	do iseq=1,nrowseqs
	  jpad=ipad(iseq)
	  nseqs_pad(jpad)=nseqs_pad(jpad)+1
	  pad_seqptr(jpad,nseqs_pad(jpad))=iseq
	  used(jpad,nseqs_pad(jpad)) = .false.
	enddo

 100	continue
c now, start clustering, beginning at first pad...
c  find initial cluster seed....
	nseq_in_cluster = 0
	do jpad=1,pads_in_row
	  do jpad_seq=1,nseqs_pad(jpad)
             if (.not.used(jpad,jpad_seq)) then
		nseq_in_cluster=1
		seq_in_cluster(1) = pad_seqptr(jpad,jpad_seq)
		used(jpad,jpad_seq) = .true.
		seed_sequence = 1
		goto 101
             endif
	  enddo
	enddo

	return	! all are used... this should be the normal return point

 101	continue	! now cluster around current seed sequence

	ipad_seed = ipad(seq_in_cluster(seed_sequence))
	itlo_seed = itlo(seq_in_cluster(seed_sequence))
	ithi_seed = ithi(seq_in_cluster(seed_sequence))
c look to left of seed...
	if (ipad_seed.ne.1) then
	  do ipad_seq=1,nseqs_pad(ipad_seed-1)
	    if (.not.used(ipad_seed-1,ipad_seq)) then
	      iptr = pad_seqptr(ipad_seed-1,ipad_seq)
	      if (((itlo(iptr).ge.itlo_seed).and.
     +		(itlo(iptr).le.ithi_seed))
     +		.or.
     +		((ithi(iptr).ge.itlo_seed).and.
     +		(ithi(iptr).le.ithi_seed))
     +		.or.
     +		((ithi(iptr).ge.ithi_seed).and.
     +		(itlo(iptr).le.itlo_seed)))
     +		then
		  nseq_in_cluster = nseq_in_cluster+1
                  if (nseq_in_cluster.gt.max_nseq_in_cluster) then
                     nseq_in_cluster = max_nseq_in_cluster
                     CALL Message('TCL_Get_Row_Seq-E2 '
     +                    //'too many connected sequences - '
     +                    //'breaking into >1 cluster',1, IDE2 )
                     goto 91
                  endif
		  seq_in_cluster(nseq_in_cluster) = iptr
		  used(ipad_seed-1,ipad_seq) = .true.
	      endif
	    endif
	  enddo
	endif
c look to right of seed...
	if (ipad_seed.ne.pads_in_row) then
	  do ipad_seq=1,nseqs_pad(ipad_seed+1)
	    if (.not.used(ipad_seed+1,ipad_seq)) then
	      iptr = pad_seqptr(ipad_seed+1,ipad_seq)
	      if (((itlo(iptr).ge.itlo_seed).and.
     +		(itlo(iptr).le.ithi_seed))
     +		.or.
     +		((ithi(iptr).ge.itlo_seed).and.
     +		(ithi(iptr).le.ithi_seed))
     +		.or.
     +		((ithi(iptr).ge.ithi_seed).and.
     +		(itlo(iptr).le.itlo_seed)))
     +		then
		  nseq_in_cluster = nseq_in_cluster+1
                  if (nseq_in_cluster.gt.max_nseq_in_cluster) then
                     nseq_in_cluster = max_nseq_in_cluster
                     CALL Message('TCL_Get_Row_Seq-E2 '
     +                    //'too many connected sequences - '
     +                    //'breaking into >1 cluster',1, IDE2 )
                     goto 91
                  endif
		  seq_in_cluster(nseq_in_cluster) = iptr
		  used(ipad_seed+1,ipad_seq) = .true.
	      endif
	    endif
	  enddo
	endif

c ok, done clustering around that seed... is there anyone else
c  in the cluster (perhaps one that has just been added) that still needs
c  to be the seed?
	if (nseq_in_cluster.gt.seed_sequence) then  ! more waiting to be seed
	  seed_sequence=seed_sequence+1
	  goto 101
	endif

 91     continue                ! come here if too many sequences

c ok, at this point, all sequences in cluster have had their chance to be seed.
c  The cluster is finished... store it...

	if (nseq_in_cluster.eq.0) then	! should not happen!
	  write(6,*)'Hey--- ERROR in clustering algorithm! do not ignore!'
	endif

	if (nseq_in_cluster.ge.2) then	! do not store 1-sequence "clusters"
           if (tpclusterh.nok.eq.tpcluster_len) then
              write(6,*)'TPCLUSTER table full- abort sec,row',
     +             sector,row
              return
           endif
           tpclusterh.nok = tpclusterh.nok + 1
c     tpcluster(tpclusterh.nok).secpad=100*sector+row
           tpcluster(tpclusterh.nok).tpc_row=100*sector+row
           tpcluster(tpclusterh.nok).nseq=nseq_in_cluster
           tpcluster(tpclusterh.nok).jseq=tpseqh.nok+1
           do iseq=1,nseq_in_cluster
              if (tpseqh.nok.eq.tpseq_len) then
                 write(6,*)'TPSEQ table full- abort sec,row',
     +                sector,row
                 tpclusterh.nok = tpclusterh.nok -1 ! don't store half-baked cluster
                 return
              endif
              tpseqh.nok = tpseqh.nok+1
              tpseq(tpseqh.nok).jpix = ipix(seq_in_cluster(iseq))
              tpseq(tpseqh.nok).tpc_row = 100*sector+row
              tpseq(tpseqh.nok).secpad = ipad(seq_in_cluster(iseq))+
     +             secpad_offset(row) ! note we restore offset
              if (iseq.ne.nseq_in_cluster) 
     +             tpseq(tpseqh.nok).next = tpseqh.nok+1
           enddo
	endif

	goto 100	! find another cluster
        return
	end


C----------------------------------------------------------------------
C
        LOGICAL FUNCTION  TCL_Get_Row_Seq
     1                 ( Sector, Row, Nseqs_max
     1                 , pad_plane_h, pad_plane
     1	               , tppadh, tppad, tppixelh, tppixel, tpmcpixh
     1	               , tpmcpix
     1	               , Nseqs, Ipix, Ipad, Itlo, Ithi, Itid, padptr ) 

	IMPLICIT NONE

C   Input arguments  (One to a line with definition after ! ) 
C
C     Sector:     desired Sector (1-48 using TPG convention)
C     Row:        desired Row (using TPG convention 1-13; 1-32)
C     Nseqs_max:  dimensioning of Ipix,Ipad,Ito,Ithi,Itid from caller
C     pad_planeh:
C     pad_plane:
C     tppad_h:
C     tppad:
C     tpppixelh
C     tppixel:
C     tpmcpixh
C     tpmcpix:
C     padptr:   pointer inside tppad table for pads with valid data in this event
C
C   Output arguments : (values in the following tables)
C
C     Nseqs:      Number of sequences with data returned in following arrays
C     Ipix:       Offset into TPPIXEL table of start of each sequence
C     Ipad:       Unique sector-wide PAD id for sequence
C     Itlo:       Low TDC value for sequence
C     Ithi:       Hi TDC value for sequence
C     Itid:       MC Track id responsible for sequence
C
C
C  Returns:
C	.TRUE. if sequences in Sector, Row were found and loaded.
C	.FALSE. if no sequences are found in Sector, Row or if no room remains.
C
C   Functional Description : 
C
C   low-level analysis module which returns in caller arrays
C   useful information for all sequences on a given  
C   SECTOR,ROW in this event; these data are parsed and organised
C   into clusters in the caller (TCL_CLSROW)
C
C   TAS 22-25 April-1994   Richard Morse
C   Copyright 1994    Lawrence Berkeley Laboratory
c   
c   modificattions:
c   ==============
c   replacement for B. Hackenberg's original pixel-based 
c   sector,row data fetch routine...
c
c   19nov94 MALisa - small stuff: jtppad no longer passed, since it is internal
c				  also, inconsistency in row OR sector will
c				  result in failure now (was row AND sector)
c
c   18dec94 MALisa - fixed a major and insidious bug which did not
c		     properly index pixel pointer if there are more than
c		     one sequence on a pad
c

#include "tas_structures.inc"

#include "tpg_pad_plane_pars.inc"
#include "tpg_pad_plane_st.inc"

#include "tss_tppad_pars.inc"
#include "tss_tppad_st.inc"

#include "tss_tppixel_pars.inc"
#include "tss_tppixel_st.inc"

#include "tss_tpmcpix_pars.inc"
#include "tss_tpmcpix_st.inc"

*  Inputs:
	INTEGER                      Sector      !Sector no. of current data set.
	INTEGER                      Row         !Row no. of current data set.
	INTEGER                      Nseqs_max   !Max pixels in a sequence.
        RECORD/ table_head_st      / pad_plane_h !header for pad_plane table
        RECORD/ pad_plane_row_st   / pad_plane(*)!rows of pad_plane table
        RECORD / table_head_st     / tppadh      !header for tppad table
        RECORD / tppad_row_st      / tppad(*)    !rows of tppad table
        RECORD / table_head_st     / tppixelh    !header for tppixel table
        RECORD / tppixel_row_st    / tppixel(*)  !rows of tppixel table
        RECORD / table_head_st     / tpmcpixh    !header for tpmcpix table
        RECORD / tpmcpix_row_st    / tpmcpix(*)  !rows of tpmcpix table

*  Input/output: - no longer passed - mal 19nov94
	INTEGER                      Jtppad      !Pointer to tppad table, initially set by caller to 1.

*  Outputs:
	INTEGER                      Nseqs       !No. of sequences.
	INTEGER                      Ipix(*)     !Offset to raw pixel table of each sequence.
	INTEGER                      Ipad(*)     !Pad number of each sequence.
	INTEGER                      Itlo(*)     !TDC value (lo) of each sequence.
	INTEGER                      Ithi(*)     !TDC value (hi) of each sequence.
	INTEGER                      Itid(*)     !MC ID of each pixel in sequence.

C----------------------------------------------------------------------
	INTEGER TPG_Nrows_in_Sector
	INTEGER TPG_Nsectors

	LOGICAL Done
	LOGICAL Started
c        LOGICAL First
	INTEGER Ipixel, Nseqpix, Isequence, Nsequences, Iseqpix
	INTEGER Jpixel
	INTEGER TDC, ADC, ADCraw, PAD, MCid
	INTEGER Jsector, Jrow

c->RJM  this should eventually be fixed! (tppad is not "logically" packed)
        integer     mxsect,mxrow,isec,irow,ksec,lsec
        parameter   (mxsect = 48, mxrow = 45)
c->MAL        integer     padptr(mxrow,mxsect),saveptr,saveoff
        integer padptr(1:45,1:24)

c->RJM  necessary baggage for unpacking tppixel data
        integer ldatum
c        integer*2 idatum(2),tdc_lo,tdc_hi
c        equivalence(idatum(1),ldatum)
c        integer*2 seqmask,botmask
c        data seqmask/'3F00'X/,botmask/'FF'X/

c new necessary baggage for unpacking data
        integer nseq_factor/'100000'X/
        integer tdc_factor/'400'X/
        
        integer jndx,kseqpix,pix_offset

	INTEGER IDE1
	SAVE    IDE1
	DATA    IDE1 /0/

c        data first/.true./
C----------------------------------------------------------------------
c
c       initialise return!
c
        tcl_get_row_seq = .true.
c
c       set up pointers into tppad; note that tppad uses
c       sectors 1,48 but rows 1,45; handled below by 
c       subtracting off pad_plane(1).nrow_in for now!
c MAL -- not now!! now it is done at top level routine
c  and it can change for each event...
c        if(first)then
c           first = .false.
c            saveptr = 0
c            saveoff = 1
c            do isec = 1, mxsect/4
c               ksec = 2*isec-1
c               do irow = 1, tpg_nrows_in_sector(ksec)
c                  padptr(irow,ksec)=saveptr+saveoff
c                  padptr(irow,ksec+mxsect/2)=padptr(irow,ksec)
c     +                               +pad_plane(1).npads(irow)
c                  saveptr = padptr(irow,ksec+mxsect/2)
c                  saveoff = pad_plane(1).npads(irow)
c               enddo
c               lsec = 2*isec
c               do irow = 1, tpg_nrows_in_sector(lsec)
c                  padptr(irow,lsec)=saveptr+saveoff
c                  padptr(irow,lsec+mxsect/2)=padptr(irow,lsec)
c     +                  +pad_plane(1).npads(irow+pad_plane(1).nrow_in)
c                  saveptr = padptr(irow,lsec+mxsect/2)
c                  saveoff = pad_plane(1).npads(irow
c     +                                          +pad_plane(1).nrow_in)
c               enddo
c            enddo
c        endif

        Nseqs = 0
        Jtppad = padptr(row,sector)

        if (jtppad.eq.0) return ! no valid data for this row (now possible 27mar95)

        IF ( Jtppad .GT. tppadh.nok ) THEN !Table exhausted -- failure.
           TCL_Get_Row_Seq = .FALSE.
           RETURN
        END IF
c->RJM  1 April 1994; the convention for jsector adopted in tss 
c->RJM  starts at jsector = 1; must translate from tppads 1,45 rows
c->mal  30mar95; the convention for tss (sector=1-48,row=1-45) is still
c->mal  the same, but now we use (and henceforth) in these codes (sector=1-24
c->mal  row=1-45)
        Jrow    = MOD( tppad( Jtppad ).tpc_row , 100 )
c mal        if(Jrow .gt. pad_plane(1).nrow_in)then
c mal         Jrow = Jrow - pad_plane(1).nrow_in
c mal        endif
        Jsector =      tppad( Jtppad ).tpc_row / 100 
        jsector = (jsector+1)/2 !mal 30mar95
       IF     ( Jsector.NE.Sector .or. Jrow.NE.Row) THEN ! .and.->.or. 19nov94
           TCL_Get_Row_Seq = .FALSE. !Fail.
           RETURN
        END IF


*	Jtppad now points to the first pad-sequence in this sector/row, table tppad:
	Done   = .FALSE.
	DO WHILE ( .NOT. Done )
	  PAD        = tppad( Jtppad ).secpad !Unique pad ID within a sector.
	  Jpixel     = tppad( Jtppad ).jpix   !Offset into the tppixel table.
	  Nsequences = tppad( Jtppad ).nseq   !Number of sequences for this pad.

          nseqpix = 0
	  jndx = tppad(jtppad).jpix	! mal 18dec94

 	  DO Isequence = 1, Nsequences

c BUG BUG BUG! - found 18dec94 malisa     jndx = tppad(jtppad).jpix + nseqpix
	     jndx = jndx + nseqpix
             ldatum = tppixel(jndx).datum

c 24mar95 - now tppixel.datum packs data differently... malisa
c             nseqpix = iand(idatum(2),seqmask)/'100'X
c             adc = idatum(1)
c             tdc_lo = iand(idatum(2),botmask)
c             tdc_hi = ibits(idatum(2),14,2)*'100'X
c             tdc = tdc_lo + tdc_hi
             nseqpix = ldatum/nseq_factor
             tdc = (ldatum - nseq_factor*nseqpix)/tdc_factor
             adc = ldatum - nseq_factor*nseqpix - tdc_factor*tdc

             MCid           = tpmcpix( jndx ).mcid !This shadows tppixel (we hope).
c
c            keep only sequences with nseqpix.gt.1  !
c
             if(nseqpix.gt.1)then
                Nseqs        = Nseqs + 1 !One more pixel being stored in caller-arrays.

*	        Load the data from this sequence:

                IF ( ( Nseqs ) .GT. Nseqs_max ) THEN !No room left in the local array:
                   CALL Message('TCL_Get_Row_Seq-E1  Nseqs_max is too '
     +                    //'small.',1, IDE1 )
                   TCL_Get_Row_Seq = .FALSE. !Fail.
                   RETURN
                END IF
                Ipix( Nseqs ) = jndx !This item repeats, but it makes life easier.
                Ipad( Nseqs ) = PAD !This one, too...
                Itlo( Nseqs ) = TDC
                Ithi( Nseqs ) = TDC+nseqpix-1
                Itid( Nseqs ) = MCid        
             endif

*	    Take no special action to start the next sequence -- it follows this one
	  END DO !Isequence = 1, Nsequences

	  Jtppad = Jtppad + 1 !Next pad.
	  IF ( Jtppad .GT. tppadh.nok ) THEN !Table exhausted -- Done.
	    Done = .TRUE.                  !This is not a failure;
	    Jtppad = 1 !Reset.             !it's a normal end-of-data condition.
	  ELSE
c-> RJM     1 April 1994; the convention for jsector adopted in tss 
c-> RJM     starts at jsector = 1
	    Jrow    = MOD( tppad( Jtppad ).tpc_row , 100 )
c->mal            if(Jrow .gt. pad_plane(1).nrow_in)then
c->mal               Jrow = Jrow - pad_plane(1).nrow_in
c->mal            endif
            Jsector =      tppad( Jtppad ).tpc_row / 100 
            jsector = (jsector+1)/2
	    IF      ( Jsector .NE. Sector ) THEN !Passed it.
	      Done = .TRUE.                      !This sector (and row) is exahusted.
	    ELSE IF ( Jrow    .NE. Row    ) THEN !Passed it.
	      Done = .TRUE.                      !This row is exahusted.
	    END IF
	  END IF

	END DO !WHILE ( .NOT. Done )

	RETURN
	END
