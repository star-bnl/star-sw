
        LOGICAL FUNCTION  TCL_Get_Row_Pixels
     1                 ( Sector, Row, Npixels_max
     1                 , pad_plane_h, pad_plane
     1	               , tppadh, tppad, tppixelh, tppixel, tpmcpixh
     1	               , tpmcpix, Jtppad
     1	               , Npixels, Ipix, Ipad, Iadc, Itdc, Itid ) 

	IMPLICIT NONE

#include "tas_structures.inc"

#include "tpg_pad_plane_pars.inc"
#include "tpg_pad_plane_st.inc"

#include "tcl_tppad_pars.inc"
#include "tcl_tppad_st.inc"

#include "tcl_tppixel_pars.inc"
#include "tcl_tppixel_st.inc"

#include "tcl_tpmcpix_pars.inc"
#include "tcl_tpmcpix_st.inc"

*  Inputs:
	INTEGER                      Sector      !Sector no. of current data set.
	INTEGER                      Row         !Row no. of current data set.
	INTEGER                      Npixels_max !Max pixels in a sequence.
        RECORD/ table_head_st      / pad_plane_h
        RECORD/ pad_plane_row_st   / pad_plane(*)
        RECORD / table_head_st     / tppadh     !header for tppad table
        RECORD / tppad_row_st      / tppad(*)   !rows of tppad table
        RECORD / table_head_st     / tppixelh   !header for tppixel table
        RECORD / tppixel_row_st    / tppixel(*) !rows of tppixel table
        RECORD / table_head_st     / tpmcpixh   !header for tpmcpix table
        RECORD / tpmcpix_row_st    / tpmcpix(*) !rows of tpmcpix table

*  Input/output:
	INTEGER                      Jtppad        !Pointer to tppad table, initially set by caller to 1.

*  Outputs:
	INTEGER                      Npixels     !No. of pixels in sequence.
	INTEGER                      Ipix(*)     !Offset to raw pixel table of each pixel in sequence.
	INTEGER                      Ipad(*)     !Pad number of each pixel in sequence.
	INTEGER                      Iadc(*)     !ADC value of each pixel in sequence.
	INTEGER                      Itdc(*)     !TDC value of each pixel in sequence.
	INTEGER                      Itid(*)     !MC ID of each pixel in sequence.

*  Returns:
*	.TRUE. if pixels in Sector, Row were found and loaded.
*	.FALSE. if no more pixels remain in Sector, Row or if no room remains.

*  Brief description: Load all TPC pixels from a row into local arrays.

*  Description:
*	Load all TPC pixels from Sector, Row into the local arrays.
*	Note that no special action is needed to load adjacent sequences in
*	any special way.  Each pixel found simply gets stored in its very
*	own array element.  Further note that a sequence need not belong to
*	a single cluster.  No assumption is made at this time about hardware or
*	DAQ-routines deciding on whether a sequence starts or ends in the same
*	way as a cluster finder decides whether a cluster starts or ends.
*	No information reqarding which raw sequence owns a pixel is kept.


	INTEGER TPG_Nrows_in_Sector
	INTEGER TPG_Nsectors

	LOGICAL Done
	LOGICAL Started, First
	INTEGER Ipixel, Nseqpix, Isequence, Nsequences, Iseqpix
	INTEGER Jpixel
	INTEGER TDC, ADC, ADCraw, PAD, MCid
	INTEGER Jsector, Jrow

        integer ldatum
        integer*2 idatum(2),tdc_lo,tdc_hi
        equivalence(idatum(1),ldatum)
        integer*2 seqmask,botmask
        
        integer jndx,kseqpix,pix_offset

c->RJM  this should eventually be fixed! (tppad is not "logically" packed)
        integer     mxsect,mxrow,isec,irow,ksec,lsec
        parameter   (mxsect = 48, mxrow = 45)
        integer     padptr(mxrow,mxsect),saveptr,saveoff

	INTEGER IDE1
	SAVE    IDE1
	DATA    IDE1 /0/

        data seqmask/'3F00'X/,botmask/'FF'X/,first/.true./

c
c       set up pointers into tppad; note that tppad uses
c       sectors 1,48 but rows 1,45; handled below by 
c       subtracting off pad_plane(1).nrow_in for now!
c
        if(first)then
           first = .false.
            saveptr = 0
            saveoff = 1
            do isec = 1, mxsect/4
               ksec = 2*isec-1
               do irow = 1, tpg_nrows_in_sector(ksec)
                  padptr(irow,ksec)=saveptr+saveoff
                  padptr(irow,ksec+mxsect/2)=padptr(irow,ksec)
     +                               +pad_plane(1).npads(irow)
                  saveptr = padptr(irow,ksec+mxsect/2)
                  saveoff = pad_plane(1).npads(irow)
               enddo
               lsec = 2*isec
               do irow = 1, tpg_nrows_in_sector(lsec)
                  padptr(irow,lsec)=saveptr+saveoff
                  padptr(irow,lsec+mxsect/2)=padptr(irow,lsec)
     +                  +pad_plane(1).npads(irow+pad_plane(1).nrow_in)
                  saveptr = padptr(irow,lsec+mxsect/2)
                  saveoff = pad_plane(1).npads(irow
     +                                          +pad_plane(1).nrow_in)
               enddo
            enddo
        endif

	Npixels = 0

*	Jtppad is the starting point in the tppad table.  Initially (each event), the
*	caller sets it to one.  Afterwards it is maintained by this routine and is
*	used to save time in searching for the next row/sector.
*	IF ( Jtppad .LE. 1 ) THEN !This is just to be safe;  caller may set Jtppad = 0.
*	  Jtppad = 1
*	END IF

        Jtppad = padptr(row,sector)

*	Look for the requested row and sector -- find or fail:
*	Started = .FALSE.
*	DO WHILE ( .NOT. Started )
	  IF ( Jtppad .GT. tppadh.nok ) THEN !Table exhausted -- failure.
	    TCL_Get_Row_Pixels = .FALSE.
	    RETURN
	  END IF
c-> RJM   1 April 1994; the convention for jsector adopted in tss 
c-> RJM   starts at jsector = 1; must translate from tppads 1,45 rows
	  Jrow    = MOD( tppad( Jtppad ).tpc_row , 100 )
          if(Jrow .gt. pad_plane(1).nrow_in)then
             Jrow = Jrow - pad_plane(1).nrow_in
          endif
c         Jsector =      tppad( Jtppad ).tpc_row / 100 + 1
	  Jsector =      tppad( Jtppad ).tpc_row / 100 
*	  IF      ( Jsector .GT. Sector ) THEN !Passed it.
	  IF      ( Jsector .NE. Sector .and. Jrow .NE. Row) THEN 
	    TCL_Get_Row_Pixels = .FALSE.       !Fail.
	    RETURN
*	  ELSE IF ( Jsector .LT. Sector ) THEN !Not there yet.
*	    Jtppad = Jtppad + 1                !Next "pad".
*	  ELSE IF ( Jrow    .GT. Row    ) THEN !Passed it.
*	    TCL_Get_Row_Pixels = .FALSE.       !Fail.
*	    RETURN
*	  ELSE IF ( Jrow    .LT. Row    ) THEN !Not there yet.
*	    Jtppad = Jtppad + 1                !Next "pad".
*	  ELSE                                 !There.
*	    Started = .TRUE.                   !Find.
	  END IF
*	END DO !WHILE ( .NOT. Started )


*	Jtppad now points to the first pad-sequence in this sector/row, table tppad:
	Done   = .FALSE.
	DO WHILE ( .NOT. Done )
	  PAD        = tppad( Jtppad ).secpad !Unique pad ID within a sector.
	  Jpixel     = tppad( Jtppad ).jpix   !Offset into the tppixel table.
	  Nsequences = tppad( Jtppad ).nseq   !Number of sequences for this pad.

 	  DO Isequence = 1, Nsequences

             ldatum = tppixel(tppad(jtppad).jpix).datum
             nseqpix = iand(idatum(2),seqmask)/'100'X

             IF ( ( Npixels + Nseqpix ) .GT. Npixels_max ) THEN !No room left in the local array:
               CALL Message('TCL_Get_Row_Pixels-E1  Npixels_max is too '
     +                    //'small.',1, IDE1 )
 	      TCL_Get_Row_Pixels = .FALSE.                        !Fail.
 	      RETURN
 	    END IF

	    Ipixel = Jpixel                 !Save this pointer for the loop.
c	    Jpixel = Jpixel + 2             !This points at first ADC value.
*	    Load the pixels from this sequence:

            do jndx = tppad(jtppad).jpix,
     +              tppad(jtppad).jpix+nseqpix-1

               ldatum = tppixel(jndx).datum
               adc = idatum(1)
               kseqpix = iand(idatum(2),seqmask)/'100'X
               tdc_lo = iand(idatum(2),botmask)
               tdc_hi = ibits(idatum(2),14,2)*'100'X
               tdc = tdc_lo + tdc_hi
               MCid           = tpmcpix( jndx ).mcid !This shadows tppixel (we hope).
               Npixels        = Npixels + 1 !One more pixel being stored in caller-arrays.

               Ipix( Npixels ) = Ipixel !This item repeats, but it makes life easier.
               Ipad( Npixels ) = PAD !This one, too...
               Itdc( Npixels ) = TDC
               Iadc( Npixels ) = ADC
               Itid( Npixels ) = MCid
               Jpixel         = Jpixel + 1
 	    END DO !Iseqpix = 1, Nseqpix

*	    Take no special action to start the next sequence -- it follows this one;  Jpixel is ready to go.
	  END DO !Isequence = 1, Nsequences

	  Jtppad = Jtppad + 1 !Next pad.
	  IF ( Jtppad .GT. tppadh.nok ) THEN !Table exhausted -- Done.
	    Done = .TRUE.                  !This is not a failure;
	    Jtppad = 1 !Reset.             !it's a normal end-of-data condition.
	  ELSE
c-> RJM     1 April 1994; the convention for jsector adopted in tss 
c-> RJM     starts at jsector = 1
	    Jrow    = MOD( tppad( Jtppad ).tpc_row , 100 )
            if(Jrow .gt. pad_plane(1).nrow_in)then
               Jrow = Jrow - pad_plane(1).nrow_in
            endif
c	    Jsector =      tppad( Jtppad ).tpc_row / 100 + 1
            Jsector =      tppad( Jtppad ).tpc_row / 100 
	    IF      ( Jsector .NE. Sector ) THEN !Passed it.
	      Done = .TRUE.                      !This sector (and row) is exahusted.
	    ELSE IF ( Jrow    .NE. Row    ) THEN !Passed it.
	      Done = .TRUE.                      !This row is exahusted.
	    END IF
	  END IF

	END DO !WHILE ( .NOT. Done )

	RETURN
	END






        INTEGER FUNCTION TCL_Make_Clusters( pad_plane_h, pad_plane,
     1	                 tppadh, tppad, tppixelh, tppixel, tpmcpixh, 
     1	                 tpmcpix, tpclusterh, tpcluster, tpseqh, tpseq )

	IMPLICIT NONE

#include "tas_structures.inc"
#include "tas_user_codes.inc"

#include "tpg_pad_plane_pars.inc"
#include "tpg_pad_plane_st.inc"

#include "tcl_tppad_pars.inc"
#include "tcl_tppad_st.inc"

#include "tcl_tppixel_pars.inc"
#include "tcl_tppixel_st.inc"

#include "tcl_tpmcpix_pars.inc"
#include "tcl_tpmcpix_st.inc"

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

*  Returns:
*       TAS user status code, defined in tas_user_codes_inc, one of:
*       tucod_ok       !General successful completion.
*       tucod_fail     !General failure.
*       tucod_abevt    !Abort event.
*       tucod_abgo     !Abort GO event looping.
*       tucod_exit     !Force program termination.
*       tucod_unkmode  !Unknown mode in call to TAE_USER.

*  Brief description: Fill the tpcluster table from the all row's data.

*  Description:
*	Fill the tpcluster table with clusters of pad-hits from the tppixel and
*	tppad tables.

	CHARACTER*80 M80(3)
	CHARACTER*132 M132

 	INTEGER Jtppad
	INTEGER Nsectors, Nrows
	INTEGER Row
	INTEGER Sector
	INTEGER TAS_Code

	INTEGER TPG_Nrows_in_Sector
	INTEGER TPG_Nsectors
	INTEGER TCL_Make_Cluster_Row
	LOGICAL MSG_Enabled_Trace

	INTEGER ID501, ID502, ID504
	SAVE    ID501, ID502, ID504
	INTEGER ID1, ID2, ID3, ID4
	SAVE    ID1, ID2, ID3, ID4
	INTEGER IDE1
	SAVE    IDE1

	DATA    ID501, ID502, ID504 / 3*0 /
	DATA    ID1, ID2, ID3, ID4  / 4*0 /
	DATA    IDE1 /0/


*       CALL MESSAGE(
*    +               'TCL_Make_Clusters-I1  Making TCL table "cluster"'
*    +               //'from "tppad" et al.' , 1, -1 )

*	Need a call like this:
*	Nsectors = TPG_Nsectors()

*	But for now:
	Nsectors = 48 !Inner/outer are 2 sectors.

	Jtppad = 1 !Start looking at first row in tppad.

	DO Sector = 1, Nsectors
	  Nrows = TPG_Nrows_in_Sector( Sector )

	  DO Row = 1, Nrows

	    TAS_Code = TCL_Make_Cluster_Row( Sector, Row 
     1                   , pad_plane_h, pad_plane
     1	                 , tppadh, tppad, tppixelh, tppixel, tpmcpixh
     1	                 , tpmcpix, Jtppad
     1	                 , tpclusterh, tpcluster, tpseqh, tpseq )

	    IF ( TAS_Code .NE. tucod_ok ) THEN
	      CALL Message( 'TCL_Make_Clusters-E1  Failed.', 1, IDE1 )
	      TCL_Make_Clusters = TAS_Code
	      RETURN
	    END IF

	  END DO !Row = 1, Nrows

	END DO !Sector = 1, Nsectors

	TCL_Make_Clusters = tucod_ok

	RETURN
	END






        INTEGER FUNCTION TCL_Make_Cluster_Row( Sector, Row
     1                 , pad_plane_h, pad_plane
     1	               , tppadh, tppad, tppixelh, tppixel, tpmcpixh
     1	               , tpmcpix, Jtppad
     1	               , tpclusterh, tpcluster, tpseqh, tpseq )

	IMPLICIT NONE

#include "tas_structures.inc"
#include "tas_user_codes.inc"

#include "tpg_pad_plane_pars.inc"
#include "tpg_pad_plane_st.inc"

#include "tcl_tppad_pars.inc"
#include "tcl_tppad_st.inc"

#include "tcl_tppixel_pars.inc"
#include "tcl_tppixel_st.inc"

#include "tcl_tpmcpix_pars.inc"
#include "tcl_tpmcpix_st.inc"

#include "tcl_tpcluster_pars.inc"
#include "tcl_tpcluster_st.inc"

#include "tcl_tpseq_pars.inc"
#include "tcl_tpseq_st.inc"

	INTEGER     PAD_Itdc_tol
	PARAMETER ( PAD_Itdc_tol = 1 )   !TDC difference-tolerance to be in same cluster.
	INTEGER     PAD_TDC_max
	PARAMETER ( PAD_TDC_max = 1023 ) !Max. no. of TDC bins.

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

*  Input/output:
	INTEGER                      Jtppad       !Pointer to tppad table, initially set by caller to 1.

*  Outputs:
        RECORD / table_head_st     / tpclusterh   !header for cluster table
        RECORD / tpcluster_row_st  / tpcluster(*) !rows of cluster table
        RECORD / table_head_st     / tpseqh       !header for tpseq table
        RECORD / tpseq_row_st      / tpseq(*)     !rows of tpseq table

*  Returns:
*       TAS user status code, defined in tas_user_codes_inc, one of:
*       tucod_ok       !General successful completion.
*       tucod_fail     !General failure.
*       tucod_abevt    !Abort event.
*       tucod_abgo     !Abort GO event looping.
*       tucod_exit     !Force program termination.
*       tucod_unkmode  !Unknown mode in call to TAE_USER.

*  Brief description: Fill the tpcluster table from one row's data.

*  Description:
*	Fill the tpcluster table with clusters of pad-hits from the tppixel and
*	tppad tables.

*  Out of date description:
*	"Cluster" (sequence) finding algorithm -- passes clusters on to PADFIT
*	for actual hit (coordinate) finding.

*	The raw data is loaded in Ipad, Iadc, Itdc and Itid for one whole row at a time.

*	Itid contains Monte Carlo Track IDs for each pad - does not
*	affect anything.  Itid is used to form TRACK_ID for each hit
*	by PADFIT.

*	PADFIT_ROW must be called once for each row in each sector, and completely
*	"does" that row in one call.

*	Nclusters_max is the maximum number of clusters that the caller-provided
*	arrays can store, and is compared against Nclusters before Nclusters is
*	incremented and another fithit is stored.  If Nclusters reaches Nclusters_max,
*	a message is generated.


	CHARACTER*80 M80(3)
	CHARACTER*132 M132

	INTEGER     Nclusters_max
	PARAMETER ( Nclusters_max = tpcluster_len ) !Buffer this definition.
	INTEGER     Nrowpixels_max
	PARAMETER ( Nrowpixels_max = 40000 ) !Max. pixels in one row.

	INTEGER Nclusters
	INTEGER Nrowpixels !No. of pixels in this row, unpacked by PADUNP.

*	Pixels from an entire row:
	INTEGER Ipix( Nrowpixels_max ) !Offset into raw pixel table, tppixel.
	INTEGER Ipad( Nrowpixels_max ) !Pad nos. in row of each padhit.
	INTEGER Iadc( Nrowpixels_max ) !ADC integer values of each padhit.
	INTEGER Itdc( Nrowpixels_max ) !TDC integer values of each padhit.
	INTEGER Itid( Nrowpixels_max ) !Track IDs of each padhit.

*	Pixels in a single cluster:
	INTEGER Jpix( Nrowpixels_max ) !Offset into raw pixel table, tppixel.
	INTEGER Jpad( Nrowpixels_max ) !Pad nos. in row of each padhit.

	INTEGER Irh, IrhL, Irh0, Irh1, Idir
	INTEGER IT_max, IT_min
	INTEGER END_POINTER, Nclupix, Nclupix_lost, OPAD
	INTEGER Nrowpixels_local

	INTEGER H, I, J, K

	LOGICAL*1 RECPAD( Nrowpixels_max ) !Save space with LOGICAL*1.

	LOGICAL BEG_OF_SEQUENCE, END_OF_SEQUENCE, FOUND_NEXT, DONE

	LOGICAL TCL_Get_Row_Pixels
	LOGICAL TCL_Put_Cluster
	LOGICAL MSG_Enabled_Trace

	INTEGER ID501, ID502, ID504
	SAVE    ID501, ID502, ID504
	INTEGER ID1, ID2, ID3, ID4
	SAVE    ID1, ID2, ID3, ID4
	INTEGER IDE1
	SAVE    IDE1

	DATA    ID501, ID502, ID504 / 3*0 /
	DATA    ID1, ID2, ID3, ID4  / 4*0 /
	DATA    IDE1 /0/

501	FORMAT('TCL_Make_Cluster_Row-B1 YROW is .lt. zero!!!!!  '
     1       //'sec,row:'2I6)
502	FORMAT('TCL_Make_Cluster_Row-E2 Insufficient space for pads in '
     1       //'data, sector:'I5' row:'I5'  Lost pads:'I7/
     1	       '    Change  Nrowpixels_max  in subroutine '
     1      //'TCL_Make_Cluster_Row.' )
503	FORMAT('TCL_Make_Cluster_Row-E3 Pad charge-overflow, sector:
     1         ' I3' row:'I5' pad:'I5)
504	FORMAT('TCL_Make_Cluster_Row-E4 Insufficient space for hits in data'
     1       //', sector:' I5' row:'I5/
     1         '    pixels in row:'I7'.  Limit:'I7/
     1	       '    (Nrowpixels_max in padfit_inc).')

c       CALL MESSAGE('TCL_Make_Cluster_Row-I1  Making TCL table '
c    1      //'"cluster" from "tppad" et al.' , 1, -1 )

c
c       Must initialise value of tcl_make_cluster_row!
c
	TCL_Make_Cluster_Row = tucod_ok     !Success

	IF ( .NOT. TCL_Get_Row_Pixels
     1                 ( Sector, Row, Nrowpixels_max
     1                 , pad_plane_h, pad_plane
     1	               , tppadh, tppad, tppixelh, tppixel, tpmcpixh
     1	               , tpmcpix, Jtppad
     1	               , Nrowpixels, Ipix, Ipad, Iadc, Itdc, Itid ) 
     1	                              ) THEN

          CALL Message( 'TCL_Make_Cluster_Row-E1  Failed to load pixels'
     1                //'for a row.', 1, IDE1 )

	  TCL_Make_Cluster_Row = tucod_fail     !General failure.
	  RETURN

	END IF ! .NOT. TCL_Get_Row_Pixels

*	Initialize the table row-counts:
c	tpclusterh.nok = 0


	IF ( MSG_Enabled_Trace( 'TCL_Make_Cluster_Row-T1', ID1 ) ) THEN
	  WRITE( M132, 101 ) Sector, Row, Nrowpixels, Nrowpixels_max
	  CALL Message( M132, 1, ID1 )
	END IF
101	FORMAT('TCL_Make_Cluster_Row-T1 Sec:'I2' Row:'I3' Hits:'
     1         I5' Maxhits:'I5)

        Nrowpixels_local = Nrowpixels !Make a local copy.

        IF ( Nrowpixels .GT. Nrowpixels_max ) THEN
          Nrowpixels_local=Nrowpixels_max
          WRITE( M80, 504 ) Sector, Row, Nrowpixels, Nrowpixels_max
          CALL Message( M80, 3, ID504 )
        END IF

        DO I = 1, Nrowpixels_local !Clear all the "recorded padhit" flags:
           RECPAD( I ) = .FALSE.
        END DO !I = 1, Nrowpixels_local

        Nclusters    = 0
        Nclupix_lost = 0

        BEG_OF_SEQUENCE = .TRUE. !Indicates the begin-of-sequence.
        Irh0 = 1 !Start at first padhit in this row.

*	Search through the raw unpacked data for a begining and an end
*	of each sequence of pixels.
*	A "sequence" is the group of pixels all in the same time-slice,
*	and corresponding to one or two (or more) actual Nclusters with the
*	same Z coordinate.

        DONE = .FALSE. !Flag to terminate loop.
        DO WHILE ( .NOT. DONE )

*	  If this is the beginning of a sequence, find the first padhit
*	  that has not yet been recorded into a sequence and start there:
          IF ( BEG_OF_SEQUENCE ) THEN !Find 1st pad-hit in this sequence.

            BEG_OF_SEQUENCE = .FALSE. !Clear flag right away.
            END_OF_SEQUENCE = .FALSE. !Clear this flag, too.
            Idir = 1 !Go forward until a pad-change (see below).

	    DO WHILE ( RECPAD( Irh0 ) .AND. ( .NOT.DONE ) ) !Find 1st unrecorded padhit

	      IF ( Irh0 .GE. Nrowpixels_local ) THEN !No pixels remain.
	        DONE = .TRUE. !Really done.
	      ELSE !More pixels remain:
	        Irh0 = Irh0 + 1 !Try next one (Irh0 always goes forward).
	      END IF !Irh0 .GE. Nrowpixels_local

	    END DO !WHILE ( RECPAD( Irh0 ) .AND. ( .NOT. END_OF_SEQUENCE ) )

	    Irh     = Irh0 !Start each new sequence here.
	    Nclupix = 0


*	    Minimum and maximum TDC values for this sequence, so far.
	    IT_min = MAX( Itdc( Irh ) - PAD_Itdc_tol, 0 )
	    IT_max = MIN( Itdc( Irh ) + PAD_Itdc_tol, PAD_TDC_max )

	    IF ( MSG_Enabled_Trace( 'TCL_Make_Cluster_Row-T2', ID2 ) ) THEN
	      WRITE( M132, 102 ) Irh0, IT_min, IT_max
	      CALL Message( M132, 1, ID2 )
	    END IF
102	FORMAT('TCL_Make_Cluster_Row-T2 Beginning of cluster.  Irh0:'I4
     1	        ' IT_min, IT_max:'2I4)


	  ELSE !In a sequence - find next pad or end the sequence:

*	    Find next pad not recorded or find the end-of-sequence:
	    FOUND_NEXT = .FALSE.

	    DO WHILE (  ( .NOT. FOUND_NEXT )
     1	       .AND.  ( .NOT. END_OF_SEQUENCE )  )

	      IF ( Irh .GT. Nrowpixels_local ) THEN !Past last pad-hit in this row.
	        END_OF_SEQUENCE = .TRUE.

	      ELSE IF ( Irh .LE. Irh0 ) THEN !Went back too far.
	        Idir = 1 !Go forward again.
	        Irh  = Irh1 + Idir !Jump to padhit after 1st one found in this pad.

	      ELSE IF ( Ipad( Irh ) .LT. Ipad( IrhL ) ) THEN !Went back too far:
	        Idir = 1 !Go forward again.
	        Irh = Irh1 + Idir !Jump to padhit after 1st one found in this pad.

	      ELSE IF ( RECPAD( Irh ) ) THEN !Skip this one -- already recorded.
	        Irh = Irh + Idir

*off	      Allow for one dead pad between "neighbors" in a sequence:
*off	      (ie, compare ..+1+#deadpads => +2 for #deadpads=1)
*off	      ELSE IF (Ipad(Irh).GT.(Ipad(IrhL)+2)) THEN !Passed -- end_seq.
*off	        END_OF_SEQUENCE=.TRUE.

*	      Allow for no dead pads:
	      ELSE IF ( Ipad( Irh ) .GT. ( Ipad( IrhL ) + 1 ) ) THEN !Passed -- end_seq.
	        END_OF_SEQUENCE = .TRUE.

*	      Check the time:
	      ELSE IF ( Itdc( Irh ) .GT. IT_max ) THEN
*	        No good -- times are too far apart -- skip it:
	        Irh = Irh + Idir

	      ELSE IF ( Itdc( Irh ) .LT. IT_min ) THEN
*	        No good -- times are too far apart -- skip it:
	        Irh = Irh + Idir

*	      This hit must belong in this sequence to get to here:
	      ELSE IF ( Ipad( Irh ) .EQ. Ipad( IrhL ) ) THEN !Same pad.
	        FOUND_NEXT = .TRUE.
*	        Widen the window, if needed:
	        IT_min = MAX( MIN( Itdc( Irh ) - PAD_Itdc_tol, IT_min ), 0 )
	        IT_max = MIN( MAX( Itdc( Irh ) + PAD_Itdc_tol, IT_max )
     +                                                  , PAD_TDC_max )

	      ELSE IF ( Idir .EQ. 1 ) THEN !First hit on next pad.
	        FOUND_NEXT = .TRUE.
	        Irh1 = Irh !Save position of 1st padhit of current pad.
	        Idir = -1 !Go backwards until hit previous pad.
*	        Redefine the window:
	        IT_min = MAX( Itdc( Irh ) - PAD_Itdc_tol, 0 )
	        IT_max = MIN( Itdc( Irh ) + PAD_Itdc_tol, PAD_TDC_max )

	      END IF !Irh .GE. Nrowpixels_local/ELSE etc.

	      IF ( MSG_Enabled_Trace( 'TCL_Make_Cluster_Row-T3', ID3 ) ) THEN
	        WRITE( M132, 103 ) Irh, Irh1, Idir, IT_min, IT_max, FOUND_NEXT, 
     +                             END_OF_SEQUENCE
103	FORMAT(' TCL_Make_Cluster_Row-T3 Stepping through cluster. Irh:' I4
     1	   ' Irh1:' I4 ' Idir:' I2 ' IT_min, IT_max:' 2I4
     1	   ' Found:' L1 ' End:' L1 )
	        CALL Message( M132, 1, ID3 )
	      END IF


	    END DO !WHILE ( .NOT. FOUND_NEXT ) etc.

	  END IF !BEG_OF_SEQUENCE


	  IF (DONE) THEN !Skip the rest if done already.

	  ELSE IF ( END_OF_SEQUENCE ) THEN !Nothing to enter if end-of-seq.
	    IF ( Nclupix_lost .NE. 0 ) THEN
	      WRITE( M80, 502 ) Sector, Row, Nclupix_lost
	      CALL Message( M80, 2, ID502 )
	    END IF

*	    Enter this cluster of pixels in the cluster table:
	    IF ( .NOT. TCL_Put_Cluster( Sector, Row
     1	             , Nclupix, Jpix, Jpad
     1	             , tpclusterh, tpcluster, tpseqh, tpseq ) ) THEN
	
	      CALL Message( 'TCL_Make_Cluster_Row-E1  TCL_Put_Cluster failed.'
     +,                    1, -1 )
	      TCL_Make_Cluster_Row = tucod_fail     !General failure.
	      RETURN

	    END IF

*	    Set this for the next sequence:
	    BEG_OF_SEQUENCE=.TRUE. !Indicates the begin-of-sequence.

	  ELSE IF ( Nclupix .LT. Nrowpixels_max ) THEN !Enter padhit Irh in sequence:
	    RECPAD( Irh ) = .TRUE. !Flag this pad as "recorded".
	    Nclupix = Nclupix + 1

	    Jpix( Nclupix ) = Ipix( Irh ) !Record the pointer to tppixel table.
	    Jpad( Nclupix ) = Ipad( Irh ) !Record the sector-wide pad number.

	    IrhL = Irh !This pad now becomes the "pad on the left".
	    Irh  = Irh + Idir !Save a loop -- start looking here.

	    IF ( MSG_Enabled_Trace( 'TCL_Make_Cluster_Row-T4', ID4 ) ) THEN
	      WRITE( M132, 104 ) IrhL, Itid( IrhL ),Ipad( IrhL ), Iadc( IrhL )
     +,                          Itdc( IrhL ), Nclupix
	      CALL Message( M132, 1, ID4 )
	    END IF
104	FORMAT( 'TCL_Make_Cluster_Row-T4 Entered a padhit.  Irh:' I4
     +       ' ID:' I8 '  Pad:' I6 ' ADC:' I4 ' TDC' I4 ' pixels:' I4 )

	  ELSE !Would like to enter this padhit, but there is no more room:
	    Nclupix_lost  = Nclupix_lost + 1
	    RECPAD( Irh ) = .TRUE. !Flag this pad as "recorded", although skipped.
	    IrhL = Irh !This pad now becomes the "pad on the left".
	    Irh  = Irh + Idir !Save a loop -- start looking here.

	  END IF !DONE/ELSE IF END_OF_SEQUENCE/ELSE IF (Nclupix.LT.Nclupix_max)

	END DO !WHILE ( .NOT. DONE )


	RETURN
	END






        LOGICAL FUNCTION TCL_Put_Cluster( Sector, Row
     1	               , Nclupix, Jpix, Jpad
     1	               , tpclusterh, tpcluster, tpseqh, tpseq )

	IMPLICIT NONE

#include "tas_structures.inc"
#include "tas_user_codes.inc"

#include "tcl_tpcluster_pars.inc"
#include "tcl_tpcluster_st.inc"

#include "tcl_tpseq_pars.inc"
#include "tcl_tpseq_st.inc"

*  Inputs:
	INTEGER                      Sector     !Sector no. of current data set.
	INTEGER                      Row        !Row no. of current data set.
	INTEGER                      Nclupix    !Number of pixels in this cluster.

	                                        !Each pixel in cluster:
	INTEGER                      Jpix(*)    !Offset to raw pixel table.
	INTEGER                      Jpad(*)    !Pad number.


*  Outputs:
	                                          !TAS tables:
        RECORD / table_head_st     / tpclusterh   !header for cluster table
        RECORD / tpcluster_row_st  / tpcluster(*) !rows of cluster table
        RECORD / table_head_st     / tpseqh       !header for tpseq table
        RECORD / tpseq_row_st      / tpseq(*)     !rows of tpseq table

*  Returns:
*	.TRUE. Successful entry of a pixel into cluster Icluster.
*	.FALSE. Out of room -- failure.

*  Brief description: Put a cluster into the cluster table.

*  Description:
*	Put a cluster into the cluster table.

	INTEGER Nclusters, Nsequences
	INTEGER Iclupix, Jpix_last

        INTEGER oldseq

	INTEGER     Nclusters_max
	PARAMETER ( Nclusters_max = tpcluster_len ) !Buffer this definition.
	INTEGER     Nsequences_max
	PARAMETER ( Nsequences_max = tpseq_len ) !Buffer this definition.


	Nclusters  = tpclusterh.nok !Total clusters so far.
	Nclusters  = Nclusters + 1  !One more cluster.

	IF ( Nclusters .GT. Nclusters_max ) THEN
	  CALL Message( 'TCL_Put_Cluster-E1  Out of table space, table '
     +                //'[tpcluster].', 1, -1 )
	  TCL_Put_Cluster = .FALSE.
	  RETURN
	END IF

	Jpix_last  = 0 !Force first test to be true.
	Nsequences = tpseqh.nok     !Total cluster's sequences so far.
        oldseq = tpseqh.nok

*	Step through the cluster pixels, pointing to a new sequence whenever
*	the pixels come from a new sequence, as indicated by a changing Jpix.
	DO Iclupix = 1, Nclupix
	  IF ( Jpix( Iclupix ) .NE. Jpix_last ) THEN !New sequence.
	    Nsequences = Nsequences + 1
	    IF ( Nsequences .GT. Nsequences_max ) THEN
	      CALL Message( 'TCL_Put_Cluster-E1  Out of table space, table '
     +                    //'[tpseq].', 1, -1 )
	      TCL_Put_Cluster = .FALSE.
	      RETURN
	    END IF
	    tpseqh.nok = Nsequences
	    tpseq( Nsequences ).jpix   = Jpix( Iclupix ) !Point to the top of this seq. in tppixel.
	    tpseq( Nsequences ).secpad = Jpad( Iclupix )
            tpseq( Nsequences ).tpc_row = 100*sector + row
	    IF ( Jpix_last .GT. 0 ) THEN !Don't do this for the first sequence.
	      tpseq( Nsequences - 1 ).next = Nsequences  !Previous one points to this one.
	    END IF
	    Jpix_last = Jpix( Iclupix )
	  END IF
	END DO !Iclupix = 1, Nclupix
	tpseq( Nsequences ).next = 0 !Last one points to nothing.

	tpclusterh.nok = Nclusters  !Total clusters so far.
        tpcluster( Nclusters ).nseq = Nsequences - oldseq
        tpcluster( Nclusters ).jseq = oldseq + 1
        tpcluster( Nclusters ).secpad = 100*sector + row
c       tpcluster( Nclusters ).flag

	RETURN
        END


