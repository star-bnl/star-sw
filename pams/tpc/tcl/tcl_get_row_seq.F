* $Id: tcl_get_row_seq.F,v 1.2 1998/08/27 16:59:53 love Exp $
* $Log: tcl_get_row_seq.F,v $
* Revision 1.2  1998/08/27 16:59:53  love
* Check tpmcpix table before accessing
*
* Revision 1.1  1998/01/27 00:49:11  fisyak
* Split sources
*
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

		MCid=0
C Need a flag to know if this is Monte Carlo data
              if(tpmcpix_h.nok.ge.jndx)MCid = tpmcpix( jndx ).mcid 
c
c Keep only sequences with nseqpix.gt.1  !
c ??? RRB: Not clear that this is desireable...
c
              if(nseqpix.gt.1)then
c
c Check for overrun of the local array:
                 IF ( Nseqs+1 .GT. Nseqs_max ) THEN
*                    CALL Message('TCL_Get_Row_Seq-E1  Nseqs_max is too '
*     +                   //'small.',1, IDE1 )
                    write(6,*) 'Nseqs_max is too small'
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
