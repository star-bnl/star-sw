* $Id: tstgain.F,v 1.2 1998/01/27 01:14:52 fisyak Exp $
* $Log: tstgain.F,v $
* Revision 1.2  1998/01/27 01:14:52  fisyak
* Split sources
*
CC:>--------------------------------------------------------------------
CC: FILE:       tstgain.F
CC: HISTORY:
CC:             00jan96-v000a-hpl- Created by stic Version
CC:  Id: idl.y,v 1.8 1996/10/15 18:33:35 ward Exp  
CC:<--------------------------------------------------------------------

      INTEGER*4 FUNCTION TSTGAIN(
     1            tstpar_h,            tstpar ,
     2      bad_channels_h,      bad_channels ,
     3          pedestal_h,          pedestal ,
     4            pulser_h,            pulser ,
     5          tpc_gain_h,          tpc_gain ) 
      IMPLICIT NONE
#include "tstgain.inc"
CC:>--------------------------------------------------------------------
CC: ROUTINE:    TSTGAIN
CC: DESCRIPTION:Calculate TPC gain from averaged pedestal-style
CC:		pedestal and pulser runs
CC:
CC: AUTHOR:     N.T.B. Stone
CC: ARGUMENTS:
CC:          IN:
CC:         pedestal     - averaged pedestal file
CC:       pedestal_h     - Header Structure for pedestal
CC:           pulser     - averaged "pedestals" from pulser run
CC:         pulser_h     - Header Structure for pulser
CC:       INOUT:
CC:           tstpar     - analysis switches
CC:         tstpar_h     - Header Structure for tstpar
CC:     bad_channels     - list of unused channels
CC:   bad_channels_h     - Header Structure for bad_channels
CC:         tpc_gain     - tpc gain table
CC:       tpc_gain_h     - Header Structure for tpc_gain
CC:         OUT:
CC: RETURNS:    STAF Condition Value
CC:>--------------------------------------------------------------------

	integer		nchan_per_rdo, nbuck
        parameter	(nchan_per_rdo=1152, nbuck=512)

	integer		i, ichan, ibuck
	integer		skip_flag(nchan_per_rdo)
	real		adc(nchan_per_rdo,nbuck)
	real		rawsignal, gain_ave

c -----	Make sure incoming tables look OK
	if ((tstpar_h.maxlen.le.0) .or. 
	2	(tstpar_h.nok.gt.tstpar_h.maxlen)) then
	  write (6,*) 'TSTGAIN: !!! Poorly defined table: TSTPAR !!!'
	  goto 999
	endif

	if ((pedestal_h.maxlen.le.0) .or. 
	2	(pedestal_h.nok.gt.pedestal_h.maxlen)) then
	  write (6,*) 'TSTGAIN: !!! Poorly defined table: PEDESTAL !!!'
	  goto 999
	endif

	if ((pulser_h.maxlen.le.0) .or. 
	2	(pulser_h.nok.gt.pulser_h.maxlen)) then
	  write (6,*) 'TSTGAIN: !!! Poorly defined table: PULSER !!!'
	  goto 999
	endif

	if ((tpc_gain_h.maxlen.le.0) .or. 
	2	(tpc_gain_h.nok.gt.tpc_gain_h.maxlen)) then
	  write (6,*) 'TSTGAIN: !!! Poorly defined table: TPC_GAIN !!!'
	  goto 999
	endif

	if (pedestal_h.nok .ne. pulser_h.nok) then
	  write (6,*) 'TSTGAIN: Pedestal and Pulser tables are different sizes !!!'
	  goto 999
	endif

c -----	Identify bad channels to ignore
        call vzero(skip_flag,nchan_per_rdo)
        do i=1,bad_channels_h.nok
           ichan = bad_channels(i).channel
           skip_flag(ichan) = 1
        enddo

c -----	Subtract the pedestals from this averaged pulser run
c	write (6,*) 'TSTGAIN: Subtracting pedestal values...'
	do i = 1, pulser_h.nok
	  adc(pulser(i).channel,pulser(i).bucket) = pulser(i).ave
	enddo

	do i = 1, pedestal_h.nok
	  adc(pedestal(i).channel,pedestal(i).bucket) = 
	2	adc(pedestal(i).channel,pedestal(i).bucket) -
	3	(pedestal(i).ave + pedestal(i).rms*tstpar(1).times_rms)
	  if (adc(pedestal(i).channel,pedestal(i).bucket).lt. 0.0)
	2	adc(pedestal(i).channel,pedestal(i).bucket) = 0.0
	enddo

c -----	Integrate the remaining signals
c	write (6,*) 'TSTGAIN: Integrating net pulser signals...'
	tpc_gain_h.nok = 0
	gain_ave = 0

	do ichan=1,nchan_per_rdo
	  if (skip_flag(ichan).eq.0) then
	    tpc_gain_h.nok = tpc_gain_h.nok + 1
	    i = tpc_gain_h.nok
	    tpc_gain(i).channel = ichan

	    rawsignal = 0
	    do ibuck = tstpar(1).first_bucket, tstpar(1).last_bucket
	      if (adc(ichan,ibuck).gt.tstpar(1).thresh) then
	        rawsignal = rawsignal + adc(ichan,ibuck)
	      endif
	    enddo
	    tpc_gain(i).gain = rawsignal
	    gain_ave = gain_ave + rawsignal
	  endif		! skip_flag
	enddo		! nchan_per_rdo

c -----	Get the normalization
c	write (6,*) 'TSTGAIN: Normalizing gain...'
	gain_ave = gain_ave / float(tpc_gain_h.nok)
	tstpar(1).ave_gain = gain_ave
	write (6,*) '>>> Average Raw Gain = ', tstpar(1).ave_gain

	do ichan = 1, tpc_gain_h.nok
	  tpc_gain(ichan).gain = tpc_gain(ichan).gain / gain_ave
	enddo

c -----	Successful completion of analysis module...
998	TSTGAIN = STAFCV_OK
	RETURN

c -----	UnSuccessful completion of analysis module...
999	TSTGAIN = STAFCV_BAD
	RETURN

	END
