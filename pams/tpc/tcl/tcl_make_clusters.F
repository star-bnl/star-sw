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
        INTEGER FUNCTION TCL_Make_Clusters(
     +     pad_plane_h, pad_plane,
     +     tppad_h, tppad,
     +     tppixel_h, tppixel,
     +     tpmcpix_h, tpmcpix,
     +     tpcluster_h, tpcluster,
     +     tpseq_h, tpseq )

	IMPLICIT NONE

C   Input arguments  (One to a line with definition after ! ) 
C
C     pad_planeh:
C     pad_plane:
C     tppad_h:
C     tppad:
C     tpppixelh
C     tppixel:
C     tpmcpix_h
C     tpmcpix:
C
C   Output arguments : (values in the following tables)
C
C     tpcluster_h
C     tpcluster:
C     tpseq_h
C     tpseq:
C
C  Returns:
C       Staf status code, either:
C         STAFCV_OK      !General successful completion.
C         STAFCV_BAD     !General failure.
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
c   modifications:
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
c  7mar96 malisa - TSS has been "upgraded" to interface with the new geant
c	scheme gstar, as well as the new geometry routines tgc.  This leads
c	to changes in the structures used in this package (no more mct) as
c	well as significant *improvements* in that there will be no more converting
c	between different geometry conventions in the code. :)
c	That is, no more #sectors=24/48, row#=1..13,1..32/1..45, or crap like that.
c     - I also just do some housecleaning.  The old clusterfinder, which 
c	works well, but with the old geant and geometry routines, is saved
c	as tcl_make_clusters.F.pre_gstar
c
c  4apr96 malisa - we now get rid of the old convention that numbered pads
c       in the *sector*.  We now number pads in the *row*.
c
c  03-NOV-1997 R.Bossingham
c       Eliminate a number of unused variables to reduce compilation warnings
c
c  10-NOV-1997 R.Bossingham
c       Sort tppad by index, rather than directly, for speed;
c       (but it's only faster if sorting actually occurs)
c

#include "tcl_make_clusters.inc"
#include "tls_ecvalues.inc"

C----------------------------------------------------------------------
c parameters:
c
c Need calls like:
c   Nrows=TPG_Nrows_in_Sector( Sector )
c   Nsectors=TPG_Nsectors()
c but, meanwhile, use parameters:

	integer max_pads_in_row
	INTEGER Nrows
	INTEGER Nsectors
	INTEGER n_index

	parameter (max_pads_in_row=182)
	parameter (Nrows=45)
	parameter (Nsectors=24)
	parameter (n_index=Nsectors*Nrows*max_pads_in_row)

C----------------------------------------------------------------------
c local variables:
c
	INTEGER IDE1
	integer index(n_index)
        integer itpp
        integer i_tppad
        integer padptr(Nrows,Nsectors)
        integer padptr_i
        integer pad_entries(Nrows,Nsectors)
        integer pad_entries_i
	INTEGER Row
	INTEGER Sector
	INTEGER TAS_Code

C----------------------------------------------------------------------
	SAVE    IDE1

C----------------------------------------------------------------------
	DATA    IDE1 /0/

C----------------------------------------------------------------------
c functions (some of which are only hypothetical):
c
	INTEGER TCL_ClsRow

        integer tls_index_sort_i

c	INTEGER TPG_Nrows_in_Sector
c	INTEGER TPG_Nsectors

C----------------------------------------------------------------------

	TCL_Make_Clusters = STAFCV_OK

c  now we are filling the pad pointer array for each event in this top-level
c  routine, instead of once and for all in tcl_get_row_seq.  This is to allow
c  filling of TPPAD table only for those pads that have data on them, instead
c  of requiring that each of the 136560 pads have an entry.  This also allows
c  arbitrary ordering of pads in TPPADS, as well as readout of less than full
c  TPC (as in sector test) - 24mar95 malisa
c
c  Because pad rows are often shared between readout boards, pads in a row
c  may not be sequential; however, these are essentially sorted later:
c
c ??? RRB: Note the following comment in the tls_quick_sort_r code,
c which may apply to tls_quick_sort_i, as well:
C             This implementation uses a heap sort algorithm.
C             Quicksort is generally faster unless the table is already
C             nearly sorted in which case it is very slow.
c
c We replace this with an index sort:
c
	if (n_index.lt.tppad_h.maxlen) then
           write(6,*)'TCL - index array too small!'
           write(6,*)'TCL - error sorting tppad table by row!'
           return
        end if
c
        if (tls_index_sort_i(
     +       tppad_h.nok,
     +       tppad(1).tpc_row,
     +       tppad(2).tpc_row,
     +       index,
     +       n_index)
     +       .ne.tls_sort_normal_cv) then
           write(6,*)'TCL - error sorting tppad table by row!'
           return
        endif
c
c First, clear padptr array...
        do sector=1,nsectors
           do row=1,nrows
              padptr(row,sector)=0
              pad_entries(row,sector)=0
           enddo
        enddo
c
c Then, fill it....
        do itpp=1,tppad_h.nok
           i_tppad = index(itpp)
           sector = tppad(i_tppad).tpc_row/100
           row = tppad(i_tppad).tpc_row - 100*sector
           if (padptr(row,sector).eq.0) padptr(row,sector) = itpp
           pad_entries(row,sector) = pad_entries(row,sector) + 1
        enddo
c
c Now, any =0 element of padptr is a padrow without valid data for this event.

	DO Sector = 1, Nsectors
           DO Row = 1, Nrows
c
c Look for clusters in any row with a sequence on at least one pad:
              pad_entries_i = pad_entries(row,sector)
              if (pad_entries_i.gt.0) then
                 padptr_i = padptr(row,sector)
c
c Find pixel clusters in each row:
                 TAS_Code = TCL_ClsRow( Sector, Row 
     1                , pad_plane_h, pad_plane
     1                , tppad_h, tppad, tppixel_h, tppixel, tpmcpix_h
     1                , tpmcpix
     1                , tpcluster_h, tpcluster, tpseq_h, tpseq
     1                , padptr, pad_entries_i, index)
    
                 IF ( TAS_Code .NE. STAFCV_OK ) THEN
                    CALL Message( 'TCL_Make_Clusters-E1  Failed.',
     1                   1, IDE1 )
                    TCL_Make_Clusters = TAS_Code
                    RETURN
                 END IF
              END IF
           END DO               !Row = 1, Nrows
	END DO                  !Sector = 1, Nsectors
        
	RETURN
	END

C----------------------------------------------------------------------
C
        INTEGER FUNCTION TCL_Clsrow( Sector, Row
     1                 , pad_plane_h, pad_plane
     1	               , tppad_h, tppad, tppixel_h, tppixel, tpmcpix_h
     1	               , tpmcpix
     1	               , tpcluster_h, tpcluster, tpseq_h, tpseq
     1                 , padptr, pad_entries_i, index )

	IMPLICIT NONE

C   Input arguments  (One to a line with definition after ! ) 
C
C     Sector:        Current sector number
C     Row:           Current row number
C     pad_plane_h:
C     pad_plane:
C     tppad_h:
C     tppad:
C     tppixel_h:
C     tppixel:
C     tpmcpix_h:
C     tpmcpix:
C     padptr:
C     pad_entries_i: Number of entries in tppad for this pad row
C     index:         tppad indices, in sector/row order
C
C   Output arguments : (values in the following tables)
C
C     tpcluster_h:
C     tpcluster:
C     tpseq_h:
C     tpseq:
C
C  Returns:
C       Staf status code, either:
C         STAFCV_OK      !General successful completion.
C         STAFCV_BAD     !General failure.
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
c   modifications:
c   ==============
c
C   03-Nov-1997 R.Bossingham
C     * Reduce max. length of pad rows from 184 to 182
C     * Eliminate some unused, but declared, variables
C     * Eliminate dead code "return" statement
C     * Check that pad id is not .le.0 (as opposed to .lt.0)
c
C   10-Nov-1997 R.Bossingham
c     * Add # tppad entries for pad row to calling arguments
c     * Add indices sorting tppad by sector/row to call
c

#include "PAM.inc"
#include "tpg_pad_plane.inc"
	RECORD /TABLE_HEAD_ST/PAD_PLANE_H
	RECORD /tpg_PAD_PLANE_ST/PAD_PLANE(*)
#include "tss_tppad.inc"
	RECORD /TABLE_HEAD_ST/TPPAD_H
	RECORD /TSS_TPPAD_ST/TPPAD(*)
#include "tss_tppixel.inc"
        RECORD      /TABLE_HEAD_ST/               tppixel_h
        RECORD     /TSS_TPPIXEL_ST/              tppixel(*)
#include "tss_tpmcpix.inc"
        RECORD      /TABLE_HEAD_ST/               tpmcpix_h
        RECORD     /TSS_TPMCPIX_ST/              tpmcpix(*)
#include "tcl_tpcluster.inc"
        RECORD      /TABLE_HEAD_ST/             tpcluster_h
        RECORD   /TCL_TPCLUSTER_ST/            tpcluster(*)
#include "tcl_tpseq.inc"
        RECORD      /TABLE_HEAD_ST/                 tpseq_h
        RECORD       /TCL_TPSEQ_ST/                tpseq(*)


C----------------------------------------------------------------------
c parameters:
c
c Need calls like:
c   Nrows=TPG_Nrows_in_Sector( Sector )
c   Nsectors=TPG_Nsectors()
c but, meanwhile, use parameters:

        integer max_nseq_in_cluster
	integer max_nseqs_on_pad
	integer max_pads_in_row
	INTEGER Nrows                ! Number of rows in sector
	INTEGER Nrowseqs_max         ! Max. #sequences in one row
	INTEGER Nsectors             ! Number of sectors in TPC

        parameter(max_nseq_in_cluster=200)
	parameter(max_nseqs_on_pad=100)
	parameter(max_pads_in_row=182)
	parameter (Nrows=45)
	PARAMETER (Nrowseqs_max=40000)
	parameter (Nsectors=24)

C----------------------------------------------------------------------
c passed variables:
c
        integer index(*)
        integer padptr(Nrows,Nsectors)
        integer pad_entries_i        ! # entries in tppad for pad row
	integer row
	integer sector

c local variables:
c
        integer ide1, ide2
        integer ipad_seq
        integer iptr
	integer iseq
        integer ipad_seed
        integer itlo_seed
        integer ithi_seed
	integer jpad                 ! Pad index
	integer jpad_hi              ! Upper limit on jpad
	integer jpad_lo              ! Lower limit on jpad
	integer jpad_lo_sav          ! Saved lower limit on jpad
	integer jpad_seq
	INTEGER Nrowseqs             ! # pixels in row, unpacked by PADUNP.
        integer nseq_in_cluster
	integer pads_in_row
	integer seed_sequence
        integer seq_in_cluster(max_nseq_in_cluster)

c These are returned by tcl_get_row_seq describing seq. on this padrow:
	INTEGER Ipix( Nrowseqs_max ) !Offset into raw pixel table, tppixel.
	INTEGER Ipad( Nrowseqs_max ) !Pad nos. in row of each seq.
	INTEGER Itlo( Nrowseqs_max ) !TDC (lo) integer values of each seq.
	INTEGER Ithi( Nrowseqs_max ) !TDC (hi) integer values of each seq.
	INTEGER Itid( Nrowseqs_max ) !Track IDs of each seq.

c The following are used to group the sequences into clusters....
	integer*2 nseqs_pad(max_pads_in_row)                   ! # seq. on pad
	integer*2 pad_seqptr(max_pads_in_row,max_nseqs_on_pad) ! pointer

	logical first
        logical tcl_get_row_seq
	logical used(max_pads_in_row,max_nseqs_on_pad)  ! .T. iff seq. in clus.

C----------------------------------------------------------------------
        save first

C----------------------------------------------------------------------
	data first /.true./

C----------------------------------------------------------------------
	if (first) then
           first = .false.
c any initialization stuff goes here...
        endif

C----------------------------------------------------------------------
c
c       Must initialise value of tcl_clsrow!
c
	Tcl_Clsrow = STAFCV_OK     !Success

c see if it is worth it first...
        if (tpcluster_h.nok.eq.tpcluster_h.maxlen) then
           write(6,*)'TPCLUSTER table full - not searching sec,row',sector,row
           return
        endif

        if (tpseq_h.nok.eq.tpseq_h.maxlen) then
           write(6,*)'TPSEQ table full - not searching sec,row',sector,row
           return
        endif
c
c Load sequences on this pad row into arrays:
	IF ( .NOT. TCL_Get_Row_Seq
     1                 ( Sector, Row, Nrowseqs_max
     1	               , tppad_h, tppad, tppixel_h, tppixel, tpmcpix_h
     1	               , tpmcpix
     1	               , Nrowseqs, Ipix, Ipad, Itlo, Ithi, Itid
     1	               , padptr, pad_entries_i, index ) ) THEN

           write(6,*)'Sect + row : ',sector,row,' nrowseqs ',nrowseqs

           CALL Message( 'Tcl_Clsrow-E1  Failed to load pixels'
     1          //'for a row.', 1, IDE1 )

           Tcl_Clsrow = STAFCV_BAD !General failure.
           RETURN

	END IF                  ! .NOT. TCL_Get_Row_Seq

c
c Return, if there are no sequences on this row:
	if (nrowseqs.eq.0) return

c reset nseqs_pad array...
        pads_in_row = pad_plane(1).npads(row)
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

c Find lowest, highest pad numbers with sequences:
	do jpad=1,pads_in_row,1
           if (nseqs_pad(jpad).ne.0) then
              jpad_lo_sav = jpad
              go to 80
           end if
        end do         ! jpad=1,pads_in_row,1

 80     continue
	do jpad=pads_in_row,1,-1
           if (nseqs_pad(jpad).ne.0) then
              jpad_hi = jpad
              go to 100
           end if
        end do         ! jpad=pads_in_row,1,-1

 100	continue
c now, start clustering, beginning at first pad...
c find initial cluster seed....
	nseq_in_cluster = 0
        jpad_lo = jpad_lo_sav
	do jpad=jpad_lo,jpad_hi
	  do jpad_seq=1,nseqs_pad(jpad)
             if (.not.used(jpad,jpad_seq)) then
		nseq_in_cluster=1
		seq_in_cluster(1) = pad_seqptr(jpad,jpad_seq)
		used(jpad,jpad_seq) = .true.
		seed_sequence = 1
c
c Update lower limit for pad with an unused sequence:
                jpad_lo_sav = jpad
c
		goto 101
             endif
	  enddo
	enddo

	return	! all are used... this should be the normal return point

 101	continue	! now cluster around current seed sequence
c
	ipad_seed = ipad(seq_in_cluster(seed_sequence))
	itlo_seed = itlo(seq_in_cluster(seed_sequence))
	ithi_seed = ithi(seq_in_cluster(seed_sequence))
c
c look to left of seed, unless this is already the left-most unused sequence:
	if (ipad_seed.gt.jpad_lo_sav) then
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

c look to right of seed, unless this is already the right-hand limit:
	if (ipad_seed.lt.jpad_hi) then
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
           if (tpcluster_h.nok.eq.tpcluster_h.maxlen) then
              write(6,*)'TPCLUSTER table full- abort sec,row',
     +             sector,row
              return
           endif
c
c Fill tcl_tpcluster:
c
c STRUCTURE /TCL_TPCLUSTER_ST/
c INTEGER*4 flag /* quality flag */
c INTEGER*4 jseq /* offset into the sequence table */
c INTEGER*4 nhits /* estimated # overlapping hits in cluster */
c INTEGER*4 nseq /* number of sequences in this cluster */
c INTEGER*4 tpc_row /* 100*sector + row */
c END STRUCTURE ! TCL_TPCLUSTER_ST !
c
           tpcluster_h.nok = tpcluster_h.nok + 1
           tpcluster(tpcluster_h.nok).jseq = tpseq_h.nok + 1
           tpcluster(tpcluster_h.nok).nseq = nseq_in_cluster
           tpcluster(tpcluster_h.nok).tpc_row = 100*sector + row
           do iseq=1,nseq_in_cluster
              if (tpseq_h.nok.eq.tpseq_h.maxlen) then
                 write(6,*)'TPSEQ table full- abort sec,row',
     +                sector,row
                 tpcluster_h.nok = tpcluster_h.nok -1 ! don't store half-baked cluster
                 return
              endif
              tpseq_h.nok = tpseq_h.nok+1
              tpseq(tpseq_h.nok).jpix = ipix(seq_in_cluster(iseq))
              tpseq(tpseq_h.nok).tpc_row = 100*sector+row
              tpseq(tpseq_h.nok).secpad = ipad(seq_in_cluster(iseq))
ccc no more!!!     +             secpad_offset(row) ! note we restore offset
              if (iseq.ne.nseq_in_cluster) 
     +             tpseq(tpseq_h.nok).next = tpseq_h.nok+1
           enddo
	endif

	goto 100	! find another cluster
	end


C----------------------------------------------------------------------
C
        LOGICAL FUNCTION  TCL_Get_Row_Seq
     1                 ( Sector, Row, Nseqs_max
     1	               , tppad_h, tppad, tppixel_h, tppixel, tpmcpix_h
     1	               , tpmcpix
     1	               , Nseqs, Ipix, Ipad, Itlo, Ithi, Itid
     1                 , padptr, pad_entries_i, index ) 

	IMPLICIT NONE

C   Input arguments  (One to a line with definition after ! ) 
C
C     Sector:     desired Sector 
C     Row:        desired Row 
C     Nseqs_max:  dimensioning of Ipix,Ipad,Ito,Ithi,Itid from caller
C     tppad_h:
C     tppad:
C     tppixel_h:
C     tppixel:
C     tpmcpix_h
C     tpmcpix:
C     padptr:        tppad table pointer to pads with valid data in this event
C     pad_entries_i: Number of entries in tppad for this pad row
C     index:         tppad indices, in sector/row order
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
c   modifications:
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
c   03nov97 R.Bossingham -
c		     Eliminate some declared, but unused, variables
c
c   10nov97 R.Bossingham -
c		     * Update comments
c                    * Make a few small speedups, w/o changing algorithms
c		     * Add # tppad entries for pad row to calling arguments
c                    * Add indices sorting tppad by sector/row to call
c
#include "PAM.inc"
#include "tss_tppad.inc"
        RECORD      /TABLE_HEAD_ST/                 tppad_h
        RECORD       /TSS_TPPAD_ST/                tppad(*)
#include "tss_tppixel.inc"
        RECORD      /TABLE_HEAD_ST/               tppixel_h
        RECORD     /TSS_TPPIXEL_ST/              tppixel(*)
#include "tss_tpmcpix.inc"
        RECORD      /TABLE_HEAD_ST/               tpmcpix_h
        RECORD     /TSS_TPMCPIX_ST/              tpmcpix(*)



*  Outputs:
	INTEGER Nseqs       !No. of sequences.
	INTEGER Ipix(*)     !Offset to raw pixel table of each sequence.
	INTEGER Ipad(*)     !Pad number of each sequence.
	INTEGER Itlo(*)     !TDC value (lo) of each sequence.
	INTEGER Ithi(*)     !TDC value (hi) of each sequence.
	INTEGER Itid(*)     !MC ID of each pixel in sequence.

C----------------------------------------------------------------------
c parameters:
c
        integer mxrow
        integer mxsect
        integer nseq_factor           ! Factor for data decoding
        integer tdc_factor            ! Factor for data decoding

        parameter (mxrow = 45)
        parameter (mxsect = 24)
        parameter (nseq_factor = '100000'X)
        parameter (tdc_factor = '400'X)

C----------------------------------------------------------------------
c passed variables:
c
        integer index(*)
        integer Nseqs_max
	integer padptr(mxrow,mxsect)
        integer pad_entries_i        ! # entries in tppad for pad row
        integer Row
        integer Sector

C----------------------------------------------------------------------
c local variables:
c
	INTEGER ADC
	INTEGER IDE1
	INTEGER Isequence             ! Index for pad sequences
        integer itpp
        integer itpp_1
        integer jndx
	INTEGER Jpixel
	INTEGER Jrow
	INTEGER Jsector
	INTEGER Jtppad                ! Pointer to tppad table
	INTEGER Jtppad_1              ! First Jtppad for row
        INTEGER ldatum                ! tppixel word
	INTEGER MCid
	INTEGER Nseqpix
	INTEGER Nsequences            ! Number of sequences on pad
	INTEGER PAD
	INTEGER TDC

C----------------------------------------------------------------------
c Hypothetical functions:
c	INTEGER TPG_Nrows_in_Sector
c	INTEGER TPG_Nsectors

C----------------------------------------------------------------------
	SAVE    IDE1

C----------------------------------------------------------------------
	DATA    IDE1 /0/

C----------------------------------------------------------------------
c
c       initialise return!
c
        tcl_get_row_seq = .true.

        Nseqs = 0
        itpp_1 = padptr(row,sector)
        Jtppad_1 = index(itpp_1)

c Check for no valid data for this row (now possible 27mar95):
        if (jtppad_1.eq.0) return

c Check for exhausted table -- failure:
        IF ( Jtppad_1 .GT. tppad_h.nok ) THEN
           TCL_Get_Row_Seq = .FALSE.
           RETURN
        END IF

c Check for internal consistency:
        Jsector = tppad( Jtppad_1 ).tpc_row / 100 
        Jrow    = tppad( Jtppad_1 ).tpc_row - 100*Jsector
        IF     ( Jsector.NE.Sector .or. Jrow.NE.Row) THEN ! .and.->.or. 19nov94
           TCL_Get_Row_Seq = .FALSE. !Fail.
           RETURN
        END IF


* itpp_1 now points to the index of the first pad-sequence
* in this sector/row, table tppad:
	DO itpp=itpp_1, itpp_1+pad_entries_i-1
           Jtppad = index(itpp)
           PAD        = tppad( Jtppad ).secpad !Unique pad ID within a sector.
           Jpixel     = tppad( Jtppad ).jpix !Offset into the tppixel table.
           Nsequences = tppad( Jtppad ).nseq !Number of sequences for this pad.

           nseqpix = 0
           jndx = tppad(jtppad).jpix ! mal 18dec94
           DO Isequence = 1, Nsequences

c BUG BUG BUG! - found 18dec94 malisa     jndx = tppad(jtppad).jpix + nseqpix
              jndx = jndx + nseqpix
              ldatum = tppixel(jndx).datum

              nseqpix = ldatum/nseq_factor
              tdc = (ldatum - nseq_factor*nseqpix)/tdc_factor
              adc = (ldatum - nseq_factor*nseqpix) - tdc_factor*tdc

              MCid = tpmcpix( jndx ).mcid !This shadows tppixel (we hope).
c
c Keep only sequences with nseqpix.gt.1  !
c ??? RRB: Not clear that this is desireable...
c
              if(nseqpix.gt.1)then
c
c Check for overrun of the local array:
                 IF ( Nseqs+1 .GT. Nseqs_max ) THEN
                    CALL Message('TCL_Get_Row_Seq-E1  Nseqs_max is too '
     +                   //'small.',1, IDE1 )
                    TCL_Get_Row_Seq = .FALSE. !Fail.
                    RETURN
                 END IF
c
c Load the data from this sequence:
                 Nseqs = Nseqs + 1
                 Ipix( Nseqs ) = jndx !This item repeats to make life easier.
                 Ipad( Nseqs ) = PAD  !This one, too...
                 Itlo( Nseqs ) = TDC
                 Ithi( Nseqs ) = TDC+nseqpix-1
                 Itid( Nseqs ) = MCid        
              endif

c Take no special action to start the next sequence -- it follows this one.
           END DO               ! Isequence = 1, Nsequences
	END DO                  ! Jtppad=jtppad_1, Jtppad_1+pad_entries_i-1

	RETURN
	END
