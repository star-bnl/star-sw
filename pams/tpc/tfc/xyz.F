CC:>--------------------------------------------------------------------
CC: FILE:       xyz.F
CC: HISTORY:
CC:             00jan96-v000a-hpl- Created by stic Version
CC:		26nov96 - First coding by Nathan Stone
CC:<--------------------------------------------------------------------

      INTEGER*4 FUNCTION XYZ(
     1            tsspar_h,            tsspar ,
     2         pad_plane_h,         pad_plane ,
     3          detector_h,          detector ,
     4           tppixel_h,           tppixel ,
     5             tppad_h,             tppad ,
     6            adcxyz_h,            adcxyz ) 
      IMPLICIT NONE
#include "xyz.inc"
CC:>--------------------------------------------------------------------
CC: ROUTINE:    XYZ
CC: DESCRIPTION:
CC:		This routine translates tppad and tppixel tables
CC:		into ADC and cartesian coordinates, for diagnostic purposes.
CC:
CC: AUTHOR:     N.T.B. Stone
CC: ARGUMENTS:
CC:          IN:
CC:           tsspar     - Slow Simulator switches/parameters
CC:         tsspar_h     - Header Structure for tsspar
CC:        pad_plane     - Pad Plane description
CC:      pad_plane_h     - Header Structure for pad_plane
CC:         detector     - Detector hardware info
CC:       detector_h     - Header Structure for detector 
CC:          tppixel     - Pixel table
CC:        tppixel_h     - Header Structure for tppixel
CC:            tppad     - Pad (sequence) information
CC:          tppad_h     - Header Structure for tppad
CC:       INOUT:
CC:         OUT:
CC:           adcxyz     - PIXel LOCations and ADC values
CC:         adcxyz_h     - Header Structure for adcxyz
CC: RETURNS:    STAF Condition Value
CC:>--------------------------------------------------------------------

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

c -----	TPG function calls
	integer		tgc_row_to_y
	integer		tgc_pad_to_x
	integer		tgc_local_to_global

	save		first_call

	if (first_call) then
	  first_call = .false.
	  time_bucket = detector(1).drift_length / float(tsspar(1).ntime)
	  time_bin = time_bucket / tsspar(1).v_drift
	  write (6,*) 'XYZ: drift_length = ', detector(1).drift_length
	  write (6,*) 'XYZ: ntime        = ', tsspar(1).ntime
	  write (6,*) 'XYZ: v_drift      = ', tsspar(1).v_drift
	  write (6,'(1x,a,f5.2,a)') 'XYZ: This implies '//
	2	'an SCA clock frequency of ', (1/time_bin), '(x2) MHz'
	  write (6,*) '           '//
	2	'(as of 11/96, SysTest uses a ~9(x2) MHz clock)'

	  seq_fact = 2**20
	  buck_fact = 2**10
	endif

c -----	Make sure incoming tables look OK
	if ((tsspar_h.maxlen.le.0) .or. 
	2	(tsspar_h.nok.gt.tsspar_h.maxlen)) then
	  write (6,*) 'XYZ: !!! Poorly defined table: TSSPAR !!!'
	  goto 999
	endif

	if ((pad_plane_h.maxlen.le.0) .or. 
	2	(pad_plane_h.nok.gt.pad_plane_h.maxlen)) then
	  write (6,*) 'XYZ: !!! Poorly defined table: PAD_PLANE !!!'
	  goto 999
	endif

	if ((tppixel_h.maxlen.le.0) .or. 
	2	(tppixel_h.nok.gt.tppixel_h.maxlen)) then
	  write (6,*) 'XYZ: !!! Poorly defined table: TPPIXEL !!!'
	  goto 999
	endif

	if ((tppad_h.maxlen.le.0) .or. 
	2	(tppad_h.nok.gt.tppad_h.maxlen)) then
	  write (6,*) 'XYZ: !!! Poorly defined table: TPPAD !!!'
	  goto 999
	endif

	if ((adcxyz_h.maxlen.le.0) .or. 
	2	(adcxyz_h.nok.gt.adcxyz_h.maxlen)) then
	  write (6,*) 'XYZ: !!! Poorly defined table: ADCXYZ !!!'
	  goto 999
	endif

	adcxyz_h.nok = 0
	inok = 0

c -----	Unpack the Pad/Pixel Tables
	do i = 1, tppad_h.nok
	  offset = 0
	  do j = 1, tppad(i).nseq
	    nbuck = tppixel(tppad(i).jpix+offset).datum / seq_fact
	    do k = 1, nbuck
	      inok = inok + 1
	      adcxyz_h.nok = inok
	      if (inok.gt.adcxyz_h.maxlen) then
	        write (6,*) 'XYZ: !!! too many pixels !!!'
	        goto 999
	      endif

c -----	Extract the encoded information
	      adcxyz(inok).nseq = tppad(i).nseq
	      adcxyz(inok).nbuck = nbuck

	      adcxyz(inok).sector = tppad(i).tpc_row / 100
	      adcxyz(inok).row = tppad(i).tpc_row - (100*adcxyz(inok).sector)
	      adcxyz(inok).pad = tppad(i).secpad

	      idatum = tppixel(tppad(i).jpix+offset).datum - nbuck*seq_fact
	      adcxyz(inok).bucket = idatum / buck_fact
	      adcxyz(inok).adc = idatum - adcxyz(inok).bucket*buck_fact

c -----	Translate to cartesian coordinates
	      istatus = tgc_pad_to_x (float(adcxyz(inok).pad), 
	2	float(adcxyz(inok).row), Xlocal(1), pad_plane_h, pad_plane)

	      istatus = tgc_row_to_y (float(adcxyz(inok).row), 
	2	Xlocal(2), pad_plane_h, pad_plane)

	      Xlocal(3) = adcxyz(inok).bucket * time_bucket

	      istatus = tgc_local_to_global (adcxyz(inok).sector,
	2	Xlocal, Xglobal)

	      adcxyz(inok).x = Xglobal(1)
	      adcxyz(inok).y = Xglobal(2)
	      adcxyz(inok).z = Xglobal(3)

	      offset = offset + 1
	    end do	! bucket loop
	  end do	! sequence loop
	end do		! pad loop

c -----	Successful completion of analysis module...
998	XYZ = STAFCV_OK
	RETURN

c -----	UnSuccessful completion of analysis module...
999	XYZ = STAFCV_BAD
	RETURN

	END
