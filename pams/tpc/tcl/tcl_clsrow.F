* $Id: tcl_clsrow.F,v 1.2 1998/08/20 21:29:33 sakrejda Exp $
* $Log: tcl_clsrow.F,v $
* Revision 1.2  1998/08/20 21:29:33  sakrejda
* printing replaced by a message call
*
* Revision 1.1  1998/01/27 00:49:10  fisyak
* Split sources
*
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

c message call related variables
        integer IDE2
        data    IDE2 /0/
        save    IDE2

        character*132 m132
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

*           CALL Message( 'Tcl_Clsrow-E1  Failed to load pixels'
*     1          //'for a row.', 1, IDE1 )
            write(6,*) 'Failed to load pixels'

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
                     write(m132,*)'TCL_Get_Row_Seq-E2 '
     +                    //'too many connected sequences - '
     +                    //'breaking into >1 cluster'
                     call Message(m132,1, IDE2 )
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
                     write(m132,*)'TCL_Get_Row_Seq-E2 '
     +                    //'too many connected sequences - '
     +                    //'breaking into >1 cluster'
                     call Message(m132,1, IDE2 )
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
