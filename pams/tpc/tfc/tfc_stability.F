* $Id: tfc_stability.F,v 1.2 1998/01/27 01:14:51 fisyak Exp $
* $Log: tfc_stability.F,v $
* Revision 1.2  1998/01/27 01:14:51  fisyak
* Split sources
*
CC:>--------------------------------------------------------------------
CC: FILE:       tfc_stability.F
CC: HISTORY:
CC:             00jan96-v000a-hpl- Created by stic Version
CC:  Id: idl.y,v 1.8 1996/10/15 18:33:35 ward Exp  
CC:<--------------------------------------------------------------------

	INTEGER*4 FUNCTION TFC_STABILITY(
	1	pedestal_h,		pedestal ,
	2	gain_h,			gain ,
	3	pedstab_pars_h,		pedstab_pars ,
	4	pedstab_h,		pedstab )

	IMPLICIT NONE
#include "tfc_stability.inc"

CC:>--------------------------------------------------------------------
CC: ROUTINE:	TFC_STABILITY
CC: DESCRIPTION: This routine reads the pedestal tables created by
CC:		tstam.F and calculates mean values for each chip,
CC:		for plotting over many stability runs
CC:
CC:        Id: idl.y,v 1.8 1996/10/15 18:33:35 ward Exp  
CC:             from tfc_stability.idl.
CC:             Please edit comments and code.
CC: AUTHOR:	N.T.B. Stone
CC: ARGUMENTS:
CC:          IN:
CC:         pedestal     - tpc_pedestal table (bucket, chan, ave, rms)
CC:       pedestal_h     - Header Structure for pedestal
CC:     pedstab_pars     - pedstab calculation switches (buckets, etc.)
CC:   pedstab_pars_h     - Header Structure for pedstab_pars
CC:             gain     - tpc_gain table (channel, gain (each pad))
CC:           gain_h     - Header Structure for gain
CC:       INOUT:
CC:          pedstab     - average pedestal values for each chip
CC:        pedstab_h     - Header Structure for pedstab
CC:         OUT:
CC: RETURNS:    STAF Condition Value
CC:>--------------------------------------------------------------------

	integer		i, j
	real		channel, value
	integer		id, iresid

	integer		icard(72), chip(1152)
        Data icard /  1,  3,  0,  7, 12, 14,  0,  0,
     &                0,  1,  3,  0,  7, 12, 14,  0,
     &                0,  0,  2,  4,  0,  8, 13, 15,
     &                0,  0,  0,  2,  4,  0,  8, 13,
     &               15,  0,  0,  0,  5,  0,  0,  9,
     &               11,  0,  0,  0,  0,  5,  0,  0,
     &                9, 11,  0,  0,  0,  0,  0,  6,
     &                0, 10,  0, 16,  0,  0,  0,  0,
     &                6,  0, 10,  0, 16,  0,  0,  0/

	integer		ichip, buc_ent(30), inok
	integer		pad_ent(30)
	real		s2, meanped(30), meanrms(30), meangain(30)

        logical         first_call/.true./
        save            first_call, chip

c -----	Make sure incoming tables look OK
	if ((pedestal_h.maxlen.le.0) .or. 
	2	(pedestal_h.nok.gt.pedestal_h.maxlen)) then
	  write (6,*) 'STAB: !!! Poorly defined table: PEDESTAL !!!'
	  goto 999
	endif

	if ((gain_h.maxlen.le.0) .or. 
	2	(gain_h.nok.gt.gain_h.maxlen)) then
	  write (6,*) 'STAB: !!! Poorly defined table: GAIN !!!'
	  goto 999
	endif

	if ((pedstab_h.maxlen.le.0) .or. 
	2	(pedstab_h.nok.gt.pedstab_h.maxlen)) then
	  write (6,*) 'STAB: !!! Poorly defined table: PEDSTAB !!!'
	  goto 999
	endif

	if ((pedstab_pars_h.maxlen.le.0) .or. 
	2	(pedstab_pars_h.nok.gt.pedstab_pars_h.maxlen)) then
	  write (6,*) 'STAB: !!! Poorly defined table: PEDSTAB_PARS !!!'
	  goto 999
	endif

c ----- Make channel-chip lookup table
        if (first_call) then
	  first_call = .false.
	  write (6,*) 'STAB: Generating channel-chip lookup table...'
          do i = 1, 1152
            channel = FLOAT(i)

            value = (channel-1.)/16. + 1.
            ID = IFIX((channel-1.)/16.) + 1
            iresid = IFIX( 16.*(value-id) + 0.5 ) + 1

            if (id.ge.1 .and. id.le.72) then
              if (icard(id).ge.1 .and. icard(id).le.16) then
                if (iresid.le.8) then
                  chip(i) = 2*icard(id) - 1
                elseif (iresid.le.16)  then
                  chip(i) = 2*icard(id)
                endif
              endif
            endif

          enddo
        endif

c -----	Increment the counter on the stability data
	pedstab_h.nok = pedstab_h.nok + 1
	inok = pedstab_h.nok
	pedstab(inok).shift = inok

c -----	Zero out the tables
	do i = 1, 30
	  buc_ent(i) = 0
	  pad_ent(i) = 0
	  pedstab(inok).chip(i) = i
	  pedstab(inok).ped(i) = 0
	  pedstab(inok).ped_drift(i) = 0
	  pedstab(inok).rms(i) = 0
	  pedstab(inok).rms_drift(i) = 0
	  pedstab(inok).gain(i) = 0
	  pedstab(inok).gain_drift(i) = 0
	  pedstab(inok).gain_rms(i) = 0
	end do

c -----	Sum up all of the contributions (by bucket)
	do i = 1, pedestal_h.nok
	  ichip = chip(pedestal(i).channel)

	  if (pedestal(i).bucket.ge.pedstab_pars(1).first_bucket .and.
	2     pedestal(i).bucket.le.pedstab_pars(1).last_bucket .and.
	3     ichip.ge.1 .and. ichip.le.30 .and.
	4     pedestal(i).channel.ne.1) then

	    buc_ent(ichip) = buc_ent(ichip) + 1

c -----	Average Pedestal
	    pedstab(inok).ped(ichip) = 
	2	(pedstab(inok).ped(ichip)*(buc_ent(ichip)-1) +
	3	pedestal(i).ave) / buc_ent(ichip)

c -----	Average Pedestal RMS
	    pedstab(inok).rms(ichip) = ( pedstab(inok).rms(ichip) * 
	2	(buc_ent(ichip)-1) + pedestal(i).rms) / buc_ent(ichip)

	  endif
	enddo

c -----	Sum up all of the contrubutions (by pad)
	do i = 1, gain_h.nok
	  ichip = chip(gain(i).channel)

	  if (gain(i).channel.ne.1 .and.
	2     ichip.ge.1 .and. ichip.le.30 ) then
	    pad_ent(ichip) = pad_ent(ichip) + 1

c -----	Save previous variance
	    if (pad_ent(ichip).gt.1) then
	      s2 = pedstab(inok).gain_rms(ichip)**2 + 
	2	 pedstab(inok).gain(ichip)**2
	    else
	      s2 = 0
	    endif

c -----	Average Gain
	    pedstab(inok).gain(ichip) = 
	2	(pedstab(inok).gain(ichip)*(pad_ent(ichip)-1) +
	3       gain(i).gain) / pad_ent(ichip)

c -----	RMS in Average Gain
	    s2 = (s2*(pad_ent(ichip) - 1) + gain(i).gain**2) /
	2	pad_ent(ichip) - pedstab(inok).gain(ichip)**2
	    if (s2.gt.0.0) then
	      pedstab(inok).gain_rms(ichip) = sqrt(s2)
	    else
	      pedstab(inok).gain_rms(ichip) = 0
	    endif
	  endif
	enddo

c -----	Calculate mean means
	do i = 1, inok
	  do j = 1, 30
	    if (i.eq.1) then
	      meanped(j) = 0
	      meanrms(j) = 0
	      meangain(j)= 0
	    endif
	    meanped(j) = meanped(j) + pedstab(i).ped(j)
	    meanrms(j) = meanrms(j) + pedstab(i).rms(j)
	    meangain(j)= meangain(j)+ pedstab(i).gain(j)
	    if (i.eq.inok) then
	      meanped(j) = meanped(j) / float(inok)
	      meanrms(j) = meanrms(j) / float(inok)
	      meangain(j)= meangain(j)/ float(inok)
	    endif
	  enddo
	enddo

c -----	Calculate drifts/deviations
	do i = 1, inok
	  do j = 1, 30
	    pedstab(i).ped_dev(j)   = pedstab(i).ped(j) - meanped(j)
	    pedstab(i).rms_dev(j)   = pedstab(i).rms(j) - meanrms(j)
	    pedstab(i).gain_dev(j)  = pedstab(i).gain(j)/ meangain(j)
	    pedstab(i).ped_drift(j) = pedstab(i).ped(j) - pedstab(1).ped(j)
	    pedstab(i).rms_drift(j) = pedstab(i).rms(j) - pedstab(1).rms(j)
	    pedstab(i).gain_drift(j)= pedstab(i).gain(j)/ pedstab(1).gain(j)
	  enddo
	enddo

c -----	Successful completion of analysis module...
998	TFC_STABILITY = STAFCV_OK
	RETURN

c -----	UnSuccessful completion of analysis module...
999	TFC_STABILITY = STAFCV_BAD
	RETURN

	END
