* $Id: tcl_make_clusters.F,v 1.26 1998/01/27 00:49:12 fisyak Exp $
* $Log: tcl_make_clusters.F,v $
* Revision 1.26  1998/01/27 00:49:12  fisyak
* Split sources
*
        INTEGER FUNCTION tcl_make_clusters(
     +     pad_plane_h, pad_plane,
     +     tppad_h, tppad,
     +     tppixel_h, tppixel,
     +     tpmcpix_h, tpmcpix,
     +     tpcluster_h, tpcluster,
     +     tpseq_h, tpseq )
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
*                    CALL Message( 'TCL_Make_Clusters-E1  Failed.',
*     1                   1, IDE1 )
                    write(6,*) 'tcl_make_clusters failed'
                    TCL_Make_Clusters = TAS_Code
                    RETURN
                 END IF
              END IF
           END DO               !Row = 1, Nrows
	END DO                  !Sector = 1, Nsectors
        
	RETURN
	END
