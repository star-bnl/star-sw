* $Id: xyz_newtab.F,v 1.1 1998/07/10 00:46:17 kathy Exp $
* $Log: xyz_newtab.F,v $
* Revision 1.1  1998/07/10 00:46:17  kathy
* put in xyz_newtab.idl & xyz_newtab.F - makes adcxyz table in new tpc table format - code & kumac by David Hardtke
*
* Revision 1.5  1998/01/27 01:14:52  fisyak
* Split sources
*
CC:>--------------------------------------------------------------------
CC: FILE:       xyz_newtab.F
CC: HISTORY:
CC:             00jan96-v000a-hpl- Created by stic Version
CC:		26nov96 - First coding by Nathan Stone
cc:		31aug97 - malisa
cc:			added mapping_arrays_inverted.inc to give
cc:			fee#,chip# for a given padrow,pad.  This
cc:			information is then put into the adcxyz table.
CC:             18jun98 - D Hardtke
CC:                     This version of xyz is modified to read the new
CC:                     data table format.
CC:<--------------------------------------------------------------------

      INTEGER*4 FUNCTION XYZ_NEWTAB(
     2         pad_plane_h,         pad_plane ,
     3         detector_h,          detector ,
     4         tcl_sector_index_h,   tcl_sector_index,
     5         raw_sec_m_h,         raw_sec_m,
     6         raw_row_in_h,        raw_row_in,
     7         raw_pad_in_h,        raw_pad_in,
     8         raw_seq_in_h,        raw_seq_in,
     9         pixel_data_in_h,     pixel_data_in,
     6         raw_row_out_h,       raw_row_out,
     7         raw_pad_out_h,       raw_pad_out,
     8         raw_seq_out_h,       raw_seq_out,
     9         pixel_data_out_h,    pixel_data_out,
     6         adcxyz_h,            adcxyz ) 
      IMPLICIT NONE
#include "xyz_newtab.inc"
#include "mapping_arrays_inverted.inc"
CC:>--------------------------------------------------------------------
CC: ROUTINE:    XYZ_NEWTAB
CC: DESCRIPTION:
CC:		This routine translates tppad and tppixel tables
CC:		into ADC and cartesian coordinates, for diagnostic purposes.
CC:
CC: AUTHOR:     N.T.B. Stone
CC: ARGUMENTS:
CC:          IN:
CC:        pad_plane     - Pad Plane description
CC:      pad_plane_h     - Header Structure for pad_plane
CC:         detector     - Detector hardware info
CC:       detector_h     - Header Structure for detector 
CC:      tcl_sector_index_h  - Header Structure for sector index
CC:      tcl_sector_index  - Current Sector
CC:      raw_sec_m_h       - Header Structure for sector table   
CC:      raw_sec_m         - Sector table
CC:      raw_row_in_h      - Header for row table  
CC:      raw_row_in        - Inner sector row table
CC:      raw_pad_in_h      - Header for pad table
CC:      raw_pad_in        - Inner sector pad table
CC:      raw_seq_in_h      - Header for sequence table        
CC:      raw_seq_in        - Inner sector sequence table
CC:      pixel_data_in_h   - header for pixel data table    
CC:      pixel_data_in     - Inner sector pixel table
CC:      raw_row_out_h      - Header for row table  
CC:      raw_row_out        - Outer sector row table
CC:      raw_pad_out_h      - Header for pad table
CC:      raw_pad_out        - Outer sector pad table
CC:      raw_seq_out_h      - Header for sequence table        
CC:      raw_seq_out        - Outer sector sequence table
CC:      pixel_data_out_h   - header for pixel data table    
CC:      pixel_data_out     - Outer sector pixel table
CC:       INOUT:
CC:         OUT:
CC:           adcxyz     - PIXel LOCations and ADC values
CC:         adcxyz_h     - Header Structure for adcxyz
CC: RETURNS:    STAF Condition Value
CC:>--------------------------------------------------------------------

	INTEGER Nrows, Nrows_in, Nrows_out  ! Number of rows in sector
	parameter (Nrows=45)
        parameter (Nrows_in=13)
        parameter (Nrows_out=32)


	integer		i, j, k
	integer		inok		! adcxyz_h.nok counter
	integer		offset		! offset in pixel table
	integer		nbuck		! number of buckets in a sequence
	integer		seq_fact	! reduction factor for sequences
	integer		buck_fact	! reduction factor for buckets
	integer		istatus, idatum
	real		time_bucket	! length (cm) of time bucket
	real		time_bin	! size (usec) of time bucket
	real		Xlocal(3)	! local cartesian coordinates
	real		Xglobal(3)	! global cartesian coordinates

	logical		first_call/.true./

c       local bookkeeping variables
	INTEGER Ipix                 !Offset into raw pixel table, tppixel.
	INTEGER Ipad                 !Pad nos. in row of each seq.
	INTEGER Itlo                 !TDC (lo) integer values of each seq.
	INTEGER Ithi                 !TDC (hi) integer values of each seq.
        integer nseq_in_cluster
	integer pads_in_row
        integer row_local
        integer row_pixeloffset, row_seqoffset, row_table
        integer pad_pixeloffset, pad_seqoffset, pad_table
        integer npixel_pad, number_of_pads, number_of_sequences
        integer iipad, sequence_break, length_of_sequence
        integer pad_break, irow, row, iseq, sector


c -----	TPG function calls
	integer		tgc_row_to_y
	integer		tgc_pad_to_x
	integer		tgc_local_to_global
        integer         tgc_time_to_z
	save		first_call

	if (first_call) then
	  first_call = .false.
	  time_bucket = detector(1).vdrift/detector(1).clock_frequency
	  time_bin = 1.0/detector(1).clock_frequency
	  write (6,*) 'XYZ: drift_length = ', detector(1).drift_length
	  write (6,'(1x,a,f5.2,a)') 'XYZ: '//
     2       'SCA clock frequency: ', detector(1).clock_frequency,
     3       '(x2) MHz'
	  write (6,*) '           '//
     2	    '(as of 11/96, SysTest uses a ~9(x2) MHz clock)'

	  seq_fact = 2**20
	  buck_fact = 2**10
	endif


	if ((pad_plane_h.maxlen.le.0) .or. 
     2	    (pad_plane_h.nok.gt.pad_plane_h.maxlen)) then
	  write (6,*) 'XYZ: !!! Poorly defined table: PAD_PLANE !!!'
	  goto 999
	endif


	if ((adcxyz_h.maxlen.le.0) .or. 
     2	    (adcxyz_h.nok.gt.adcxyz_h.maxlen)) then
	  write (6,*) 'XYZ: !!! Poorly defined table: ADCXYZ !!!'
	  goto 999
	endif

c -----	Unpack the outer sector data tables Tables
       sector = tcl_sector_index(1).CurrentSector
       if (raw_sec_m(sector).RowRefOut.ne.'N') then
	do irow = nrows, nrows_in + 1, -1
         row_table = nrows - irow + 1
         if (raw_row_out(row_table).RowId.ne.irow) then
	  write (6,*) 'XYZ: !!! Poorly defined table: RAW_ROW_OUT !!!'
	  goto 999
         endif
         row_pixeloffset = raw_row_out(row_table).ipixel
         row_seqoffset   = raw_row_out(row_table).iseq
         pad_break = raw_row_out(row_table).PadModBreak
          If (pad_break.lt.0) then
           pad_break = 256 + pad_break
          endif
         number_of_pads =raw_row_out(row_table).npad 
         if (number_of_pads.lt.0) then   
          number_of_pads = 256 + number_of_pads
         endif
	  do iipad = 1, number_of_pads
           pad_table = raw_row_out(row_table).ipad + iipad
           if ((iipad.le.pad_break).or.(pad_break.eq.0)) then
            pad_pixeloffset = raw_pad_out(pad_table).PadOffset 
           else
            pad_pixeloffset = raw_pad_out(pad_table).PadOffset + 65536
           endif
           pad_seqoffset = raw_pad_out(pad_table).SeqOffset
           npixel_pad = 0 
           number_of_sequences =raw_pad_out(pad_table).nseq 
           if (number_of_sequences.lt.0) then
            number_of_sequences = 256 + number_of_sequences
           endif 
           do iseq= 1,number_of_sequences
            ipad = raw_pad_out(pad_table).PadId
             if (ipad.lt.0) then
              ipad = 256 + ipad
             endif
             ipix = row_pixeloffset + pad_pixeloffset + 
     +            npixel_pad + 1     
             sequence_break = raw_pad_out(pad_table).SeqModBreak
             if (sequence_break.lt.0) then
              sequence_break = 256 + sequence_break
             endif
             itlo = raw_seq_out(row_seqoffset+pad_seqoffset+iseq).m 
             if (itlo.lt.0) then
              itlo = 256 + itlo
             endif
             itlo  = itlo + 1  ! want 1-512, not 0-511
             if (iseq.gt.sequence_break) then
              itlo = itlo + 256
             endif 
             length_of_sequence = 
     +           raw_seq_out(row_seqoffset+pad_seqoffset+iseq).i
             if (length_of_sequence.lt.0) then
              length_of_sequence = 256 + length_of_sequence
             endif
             ithi = itlo + length_of_sequence
             do nbuck = 0, length_of_sequence
	      if (adcxyz_h.nok.ge.adcxyz_h.maxlen) then
	        write (6,*) 'XYZ: !!! too many pixels !!!'
	        goto 999
	      endif
              adcxyz_h.nok = adcxyz_h.nok + 1
              adcxyz(adcxyz_h.nok).nseq = iseq
              adcxyz(adcxyz_h.nok).nbuck = length_of_sequence + 1
              adcxyz(adcxyz_h.nok).sector = sector
              adcxyz(adcxyz_h.nok).row = raw_row_out(row_table).RowId
              adcxyz(adcxyz_h.nok).pad = ipad
              adcxyz(adcxyz_h.nok).adc = pixel_data_out(nbuck+ipix).data
              adcxyz(adcxyz_h.nok).bucket = itlo + nbuck
c mal 31aug97
              adcxyz(adcxyz_h.nok).fee  = 
     +          fee_pad(adcxyz(adcxyz_h.nok).row,adcxyz(adcxyz_h.nok).pad)
              adcxyz(adcxyz_h.nok).chip = 
     +          chip_pad(adcxyz(adcxyz_h.nok).row,adcxyz(adcxyz_h.nok).pad)
            
	      istatus = tgc_pad_to_x (float(adcxyz(adcxyz_h.nok).pad), 
     +	        float(adcxyz(adcxyz_h.nok).row), Xlocal(1), 
     +          pad_plane_h, pad_plane)

	      istatus = tgc_row_to_y (float(adcxyz(adcxyz_h.nok).row), 
     + 	      Xlocal(2), pad_plane_h, pad_plane)

*	      Xlocal(3) = adcxyz(adcxyz_h.nok).bucket * time_bucket
              istatus = tgc_time_to_z(adcxyz(adcxyz_h.nok).bucket,1,1,1,
     +                                Xlocal(3))
	      istatus = tgc_local_to_global (adcxyz(adcxyz_h.nok).sector,
     +        Xlocal, Xglobal)

	      adcxyz(adcxyz_h.nok).x = Xglobal(1)
	      adcxyz(adcxyz_h.nok).y = Xglobal(2)
	      adcxyz(adcxyz_h.nok).z = Xglobal(3)
             enddo                   !  loop over buckets in seqence  
            npixel_pad = npixel_pad + length_of_sequence + 1
           enddo              ! loop over sequences
          enddo               ! loop over pads
        enddo                ! loop over rows
       ENDIF           ! Data exists for this sub-sector

c
c -----	Unpack the inner sector data tables Tables
c
       if (raw_sec_m(sector).RowRefIn.ne.'N') then
	do irow = nrows_in, 1, -1
         row_table = nrows_in - irow + 1
         if (raw_row_in(row_table).RowId.ne.irow) then
	  write (6,*) 'XYZ: !!! Poorly defined table: RAW_ROW_IN !!!'
	  goto 999
         endif
         row_pixeloffset = raw_row_in(row_table).ipixel
         row_seqoffset   = raw_row_in(row_table).iseq
         pad_break = raw_row_in(row_table).PadModBreak
          If (pad_break.lt.0) then
           pad_break = 256 + pad_break
          endif
         number_of_pads =raw_row_in(row_table).npad 
         if (number_of_pads.lt.0) then   
          number_of_pads = 256 + number_of_pads
         endif
	  do iipad = 1, number_of_pads
           pad_table = raw_row_in(row_table).ipad + iipad
           if ((iipad.le.pad_break).or.(pad_break.eq.0)) then
            pad_pixeloffset = raw_pad_in(pad_table).PadOffset 
           else
            pad_pixeloffset = raw_pad_in(pad_table).PadOffset + 65536
           endif
           pad_seqoffset = raw_pad_in(pad_table).SeqOffset
           npixel_pad = 0 
           number_of_sequences =raw_pad_in(pad_table).nseq 
           if (number_of_sequences.lt.0) then
            number_of_sequences = 256 + number_of_sequences
           endif 
           do iseq= 1,number_of_sequences
            ipad = raw_pad_in(pad_table).PadId
             if (ipad.lt.0) then
              ipad = 256 + ipad
             endif
             ipix = row_pixeloffset + pad_pixeloffset + 
     +            npixel_pad + 1     
             sequence_break = raw_pad_in(pad_table).SeqModBreak
             if (sequence_break.lt.0) then
              sequence_break = 256 + sequence_break
             endif
             itlo = raw_seq_in(row_seqoffset+pad_seqoffset+iseq).m 
             if (itlo.lt.0) then
              itlo = 256 + itlo
             endif
             itlo  = itlo + 1  ! want 1-512, not 0-511
             if (iseq.gt.sequence_break) then
              itlo = itlo + 256
             endif 
             length_of_sequence = 
     +           raw_seq_in(row_seqoffset+pad_seqoffset+iseq).i
             if (length_of_sequence.lt.0) then
              length_of_sequence = 256 + length_of_sequence
             endif
             ithi = itlo + length_of_sequence
             do nbuck = 0, length_of_sequence
	      if (adcxyz_h.nok.ge.adcxyz_h.maxlen) then
	        write (6,*) 'XYZ: !!! too many pixels !!!'
	        goto 999
	      endif
              adcxyz_h.nok = adcxyz_h.nok + 1
              adcxyz(adcxyz_h.nok).nseq = iseq
              adcxyz(adcxyz_h.nok).nbuck = length_of_sequence + 1
              adcxyz(adcxyz_h.nok).sector = sector
              adcxyz(adcxyz_h.nok).row = raw_row_in(row_table).RowId
              adcxyz(adcxyz_h.nok).pad = ipad
              adcxyz(adcxyz_h.nok).adc = pixel_data_in(nbuck+ipix).data
              adcxyz(adcxyz_h.nok).bucket = itlo + nbuck
c mal 31aug97
              adcxyz(adcxyz_h.nok).fee  = 
     +          fee_pad(adcxyz(adcxyz_h.nok).row,adcxyz(adcxyz_h.nok).pad)
              adcxyz(adcxyz_h.nok).chip = 
     +          chip_pad(adcxyz(adcxyz_h.nok).row,adcxyz(adcxyz_h.nok).pad)
            
	      istatus = tgc_pad_to_x (float(adcxyz(adcxyz_h.nok).pad), 
     +	        float(adcxyz(adcxyz_h.nok).row), Xlocal(1), 
     +          pad_plane_h, pad_plane)

	      istatus = tgc_row_to_y (float(adcxyz(adcxyz_h.nok).row), 
     + 	      Xlocal(2), pad_plane_h, pad_plane)

*	      Xlocal(3) = adcxyz(adcxyz_h.nok).bucket * time_bucket
              istatus = tgc_time_to_z(adcxyz(adcxyz_h.nok).bucket,1,1,1,
     +                                Xlocal(3))
	      istatus = tgc_local_to_global (adcxyz(adcxyz_h.nok).sector,
     +        Xlocal, Xglobal)

	      adcxyz(adcxyz_h.nok).x = Xglobal(1)
	      adcxyz(adcxyz_h.nok).y = Xglobal(2)
	      adcxyz(adcxyz_h.nok).z = Xglobal(3)
             enddo                   !  loop over buckets in seqence  
            npixel_pad = npixel_pad + length_of_sequence + 1
           enddo              ! loop over sequences
          enddo               ! loop over pads
        enddo                ! loop over rows
       ENDIF           ! Data exists for this sub-sector

c -----	Successful completion of analysis module...
998	XYZ_NEWTAB = STAFCV_OK
	RETURN

c -----	UnSuccessful completion of analysis module...
999	XYZ_NEWTAB = STAFCV_BAD
	RETURN

	END
