* $Id: tcl.F,v 1.2 1998/05/22 00:18:14 hardtke Exp $
* $Log: tcl.F,v $
* Revision 1.2  1998/05/22 00:18:14  hardtke
* modified hit finder with new data format
*
* Revision 1.1  1998/05/21 01:10:47  hardtke
* cluster finder with new data structures
*
* Revision 1.26  1998/01/27 00:49:12  fisyak
* Split sources
*
        INTEGER FUNCTION tcl(
     +     pad_plane_h, pad_plane,
     +     tcl_sector_index_h, tcl_sector_index,
     +     raw_sec_m_h, raw_sec_m,
     +     raw_row_in_h, raw_row_in,
     +     raw_pad_in_h, raw_pad_in,
     +     raw_seq_in_h, raw_seq_in,
     +     pixel_data_in_h, pixel_data_in,
     +     raw_row_out_h, raw_row_out,
     +     raw_pad_out_h, raw_pad_out,
     +     raw_seq_out_h, raw_seq_out,
     +     pixel_data_out_h, pixel_data_out,
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
C     tcl_sector_index:     
C     raw_sec_m:
C     raw_row_in:
C     raw_pad_in:
C     raw_seq_in:
C     pixel_data_in:
C     raw_row_out:
C     raw_pad_out:
C     raw_seq_out:
C     pixel_data_out:
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
c  18-May-1998 D. Hardtke
c       Adopt the new tpc data format.  tcl_make_clusters.F becomes
c       tcl.F.  It now call tcl_clusters_in_row instead of tcl_clsrow.   
c       NOTE THAT THE LOOP OF SECTORS IS NO LONGER PERFORMED HERE.
c
c  21-May-1998 D. Hardtke
c       In order to facilitate hit finding, replace structre tcl_tpseq with
c       tcl_tp_seq.  This has a few added words, including a pointer into
c       the pixel table and the offset.

#include "tcl.inc"
#include "tls_ecvalues.inc"

C----------------------------------------------------------------------
c parameters:
c
c Need calls like:
c   Nrows=TPG_Nrows_in_Sector( Sector )
c   Nsectors=TPG_Nsectors()
c but, meanwhile, use parameters:

	integer max_pads_in_row
	INTEGER Nrows, Nrows_in, Nrows_out 
	INTEGER Nsectors
	INTEGER n_index

	parameter (max_pads_in_row=182)
	parameter (Nrows=45)
        parameter (Nrows_in=13)
        parameter (Nrows_out=32)
	parameter (Nsectors=24)
	parameter (n_index=Nsectors*Nrows*max_pads_in_row)

C----------------------------------------------------------------------
c local variables:
c
	INTEGER IDE1, IDE2
	integer index(n_index)
        integer itpp
        integer i_tppad
	INTEGER Row
	INTEGER Sector
	INTEGER TAS_Code
        INTEGER row_table
        CHARACTER*132 m132

C----------------------------------------------------------------------
	SAVE    IDE1, IDE2

C----------------------------------------------------------------------
	DATA    IDE1,IDE2 /0,0/
           
C----------------------------------------------------------------------
c functions (some of which are only hypothetical):
c
        INTEGER tcl_clusters_in_row


c	INTEGER TPG_Nrows_in_Sector
c	INTEGER TPG_Nsectors

C----------------------------------------------------------------------

	Tcl = STAFCV_OK
        call Msg_Enable( 'TCL-E1' )
        call Msg_Enable( 'TCL-E2' )

        sector = tcl_sector_index(1).CurrentSector
        If (raw_sec_m(sector).RowRefOut.ne.'N') then
CDH loop backwards now since the data is packed that way
           DO Row = Nrows,(Nrows_in+1),-1
c
c Look for clusters in any row with a sequence on at least one pad:
c
             row_table = Nrows - row + 1     
             if (raw_row_out(row_table).RowID.ne.row) then
                   write(m132,*) 'TCL-E2 raw_row_out table has 
     +                            incorrect format.' 
                    CALL Message( m132,
     1                   1, IDE2 )
              tcl = STAFCV_BAD
              return
             endif
             If (raw_row_out(row_table).padref.ne.'N') then
c Find pixel clusters in each row:
                 TAS_Code = tcl_clusters_in_row( Sector, Row 
     1                , pad_plane_h, pad_plane
     1                , raw_sec_m_h, raw_sec_m 
     1                , raw_row_out_h, raw_row_out 
     1                , raw_pad_out_h, raw_pad_out 
     1                , raw_seq_out_h, raw_seq_out 
     1                , pixel_data_out_h, pixel_data_out 
     1                , tpcluster_h, tpcluster, tpseq_h, tpseq)
    
                 IF ( TAS_Code .NE. STAFCV_OK ) THEN
                   write(m132,*) 'TCL-E1 Failed.' 
                    CALL Message( m132,
     1                   1, IDE1 )
                    Tcl = TAS_Code
                    RETURN
                 END IF
              END IF            ! padref.ne.'N'
           END DO               !Row = 1, Nrows
        Endif                   !RowRefOut.ne.'N', data exists for this sector
C
C now look at inner sector
C
        If (raw_sec_m(sector).RowRefIn.ne.'N') then
CDH loop backwards now since the data is packed that way
           DO Row = Nrows_in,1,-1
c
c Look for clusters in any row with a sequence on at least one pad:
c
             row_table = Nrows_in - row + 1     
             if (raw_row_in(row_table).RowID.ne.row) then
                   write(m132,*) 'TCL-E2 raw_row_in table has 
     +                            incorrect format.' 
                    CALL Message( m132,
     1                   1, IDE2 )
              tcl = STAFCV_BAD
              return
             endif
             If (raw_row_in(row_table).padref.ne.'N') then
c Find pixel clusters in each row:
                 TAS_Code = tcl_clusters_in_row( Sector, Row 
     1                , pad_plane_h, pad_plane
     1                , raw_sec_m_h, raw_sec_m 
     1                , raw_row_in_h, raw_row_in 
     1                , raw_pad_in_h, raw_pad_in 
     1                , raw_seq_in_h, raw_seq_in 
     1                , pixel_data_in_h, pixel_data_in 
     1                , tpcluster_h, tpcluster, tpseq_h, tpseq)
    
                 IF ( TAS_Code .NE. STAFCV_OK ) THEN
                   write(m132,*) 'TCL-E1 Failed.' 
                    CALL Message( m132,
     1                   1, IDE1 )
                    Tcl = TAS_Code
                    RETURN
                 END IF
              END IF            ! padref.ne.'N'
           END DO               !Row = 1, Nrows
        Endif                   !RowRefIn.ne.'N', data exists for this sector

        
	RETURN
	END
