* $Id: tstam.F,v 1.2 1998/01/27 01:14:51 fisyak Exp $
* $Log: tstam.F,v $
* Revision 1.2  1998/01/27 01:14:51  fisyak
* Split sources
*
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
      INTEGER FUNCTION tstam(
     +     tstpar_h, tstpar,
     +     minidaq_pix_h,minidaq_pix,
     +     tpc_pedestal_h, tpc_pedestal,
     +     tpc_gain_h, tpc_gain,
     +     readout_map_h, readout_map,
     +     bad_channels_h, bad_channels,
     +     tppad_h, tppad,
     +     tppixel_h, tppixel)

      IMPLICIT NONE

C   Input arguments  (One to a line with definition after ! ) 
C
C     tstpar      ! switch table controling functionality of analysis module
C     tpc_pedestal! table containing pedestal and rms (over buckets) for each channel
C     readout_map ! channel-to-(sector,row,pad) map
C     bad_channels! list of channels to ignore
C
C   Output arguments : (values in the following tables)
C
C     tpc_pedestal! this routine can also fill pedestal table (if switch is set)
C     tppad       !\
C     tppixel     !- these tables encode the pixel data for consumption of clusterfinder 
C
C  Returns:
C       TAS user status code, defined in tas_user_codes_inc, one of:
C       tucod_ok       !General successful completion.
C       tucod_fail     !General failure.
C       tucod_abevt    !Abort event.
C       tucod_abgo     !Abort GO event looping.
C       tucod_exit     !Force program termination.
C       tucod_unkmode  !Unknown mode in call to TAE_USER.
C     
C
C   Functional Description : 
C     This is an analysis module to interface the files produced by slow controls and
C     minidaq in the system test of 1996 to the existing tpc event reconstruction software.
C     Once the pixel information is in the tables TPPAD and TPPIXEL, SAS software can take
C     over
C     Two main functions:
C     1) fill pedestal table if TSTPAR.PED_EVENT=1
C     2) a) read pixel data from file (TSTPAR.FILIN)
C        b) subtract pedestals (if TSTPAR.PED_SUBTRACT=1)
C        c) fill TPPAD and TPPIXEL tables
C
C	History:
C	- Written by Mike Lisa 3 April 1996.
C	Modified:
C	- Nathan Stone (January 1997) 
C		- Included lists of ignore_range's, to accomodate several
C		  sys-test setups
C		- Extended mapping_arrays.inc to allow for decoding of 
C		  inner sectors (introduced feeMax)
C		- Un-commented the "Adding it to the bad_channels table" lines
C		  so that it would not quit upon finding odd FEE numbers

#include "tstam.inc"
#include "mapping_arrays.inc"

c-------- Defined "event types" -----------
c tstpar(1).ped_event = 1 --> Pedestal event
c tstpar(1).ped_event = 2 --> Pulser event (use for gain calibration)
c tstpar(1).ped_event = 0 --> Regular (fill tppixel table)
c tstpar(1).ped_event = 3 --> Regular + Overwrite Chuck's ADC table with Gain/Pedestal corrected
c tstpar(1).ped_event =-1 --> Geometry
c
c-----------------------------------------------------------

        integer ichan,ibuck,nchan_per_rdo,nbuck
        parameter(nchan_per_rdo=1152,nbuck=512)
        real adc(nchan_per_rdo,nbuck) ! this is 2 MB-- it's kind of wasteful, but...
        real rawsignal,peakheight ! for gain calibration...
        real gain_ave
        integer ngains
        integer lunin
        parameter(lunin=32)     ! change this if it stomps on someone...
        real mom1_new,mom2_new,mom1_old,mom2_old,rms_old,rms_new2
        integer itemp,last_buck,irow,ipad,itemp2,num_chan
        integer sector_row(nchan_per_rdo)
        integer pad(nchan_per_rdo)
        logical first/.true./
        integer nseq,npix_seq,iadc,id,ipix
        integer itdc(nbuck),iseq(nbuck)
        integer skip_flag(nchan_per_rdo)
cNTBS -	added feeMax (set in mapping_arrays.inc) to allow expansion
        integer ntimes_seen(0:feeMax)
	integer ifee,ipin	
cccc        integer ignore_fee(4)/40,49,88,123/

c--- fix for faulty electronics.. ---------


c -----	15-card setup
cNTBS	integer nbad_channel_ranges
cNTBS	parameter(nbad_channel_ranges=16)
cNTBS	integer ignore_range_lo(nbad_channel_ranges)
cNTBS	2	/33, 97,177,241,321,385,465,529,
cNTBS	3	593,657,737,801,897, 929,1041,1073/
cNTBS	integer ignore_range_hi(nbad_channel_ranges)
cNTBS	2	/48,144,192,288,336,432,480,576,
cNTBS	3	624,720,768,880,912,1024,1056,1152/

c -----	20-card setup
cNTBS	integer nbad_channel_ranges
cNTBS	parameter(nbad_channel_ranges=16)
cNTBS	integer ignore_range_lo(nbad_channel_ranges)
cNTBS	2	/33,97,177,241,321,385,465,529,
cNTBS	3	609,673,753,817,897,961,1041,1105/
cNTBS	integer ignore_range_hi(nbad_channel_ranges)
cNTBS	2	/48,144,192,288,336,432,480,576,
cNTBS	3	624,720,768,864,912,1008,1056,1152/

c -----	20-card setup (using sockets 1-5, 10-14)
	integer nbad_channel_ranges
	parameter(nbad_channel_ranges= 8)
	integer ignore_range_lo(nbad_channel_ranges)
	2	/   81,    225,    369,    513,
	3	    657,    801,    945,     1089/
	integer ignore_range_hi(nbad_channel_ranges)
	2	/   144,    288,    432,    576,
	3	    720,    864,    1008,     1152/

        save first,itdc,iseq,ignore_range_lo,ignore_range_hi
c-----------------------------------------------------------

        if (first) then
           first = .false.
           do ibuck=1,nbuck
              itdc(ibuck) = (ibuck-1)*2**10
              iseq(ibuck) = ibuck*2**20
           enddo
c complete the mapping array for memory-to-logical-pad-ordering...
           do itemp=1,72
              if (mod(memory_to_channel(itemp)-1,16).ne.0) then
                 write(6,*)'Hey- bad memory map initiator: ',itemp,
     +                memory_to_channel(itemp)
                 return
              endif
           enddo
           do itemp=73,1152
              itemp2=itemp-72
              memory_to_channel(itemp) = memory_to_channel(itemp2)+1
           enddo
           write(6,*)'>>>>> I am ignoring the following channel ranges:'
           do itemp=1,nbad_channel_ranges
              write(6,*)'channels between ',ignore_range_lo(itemp),ignore_range_hi(itemp)
           enddo
        endif

c first, read event

	if (tstpar(1).format.eq.0) then ! old "dumped" format
           open(unit=lunin,file=TSTPAR(1).FILIN,status='old',err=9)
           do ibuck=1,nbuck
              do itemp=1,nchan_per_rdo
c remap as Chuck would do...
                 ichan = memory_to_channel(itemp)
                 read(lunin,*,end=9)adc(ichan,ibuck)
              enddo
           enddo
           close(unit=lunin)
           tstam = STAFCV_OK
        else                    ! Chuck's minidaq format
c           open(unit=lunin,file=TSTPAR(1).FILIN,status='old',err=9)
c           do ichan=1,nchan_per_rdo
c              do ibuck=1,nbuck
c                 read(lunin,*,end=9)adc(ichan,ibuck)
c              enddo
c           enddo
c           close(unit=lunin)
           num_chan = minidaq_pix_h.nok
           write(6,*)'Looks like number of channels in table is : ',num_chan
           do ichan=1,num_chan
              do ibuck=1,nbuck
                 adc(ichan,ibuck) = minidaq_pix(ichan).data(ibuck)
              enddo
           enddo
           tstam = STAFCV_OK
        endif


c identify channels to ignore (assume for safety that this changes every event)
        call vzero(skip_flag,nchan_per_rdo)
        do itemp=1,bad_channels_h.nok
           ichan = bad_channels(itemp).channel
           skip_flag(ichan) = 1
        enddo

C========================== PEDESTAL EVENT =============================
        if (TSTPAR(1).PED_EVENT.eq.1) then ! pedestal event
c fill pedestal table
           tstpar(1).nped_samples = tstpar(1).nped_samples + 1
           if (tstpar(1).nped_samples.eq.1) then ! first pedestal sample.. fill fresh
cc              tpc_pedestal_h.nok = nchan_per_rdo*nbuck
              tpc_pedestal_h.nok = 0
              do ichan=1,nchan_per_rdo
                 if (skip_flag(ichan).eq.0) then
                    do ibuck=1,nbuck
                       tpc_pedestal_h.nok = tpc_pedestal_h.nok +1
                       itemp = tpc_pedestal_h.nok
                       tpc_pedestal(itemp).bucket = ibuck
                       tpc_pedestal(itemp).channel = ichan
                       tpc_pedestal(itemp).ave = adc(ichan,ibuck)
                       tpc_pedestal(itemp).rms = 0.0
                    enddo
                 endif
              enddo
           else                 ! not first pedestal event.. update table
              do itemp=1,tpc_pedestal_h.nok
                 ichan = tpc_pedestal(itemp).channel
                 if (skip_flag(ichan).eq.1) then
                    id = 0
                    call message(
     +            'TSTAM - Should reset TPC_PEDESTALs when BAD_CHANNELS changes! ',
     +                   1,id)
                 endif
                 ibuck = tpc_pedestal(itemp).bucket
                 mom1_old = tpc_pedestal(itemp).ave
                 mom1_new = (float(tstpar(1).nped_samples-1)*mom1_old +
     +                adc(ichan,ibuck))/float(tstpar(1).nped_samples)
                 rms_old = tpc_pedestal(itemp).rms
                 mom2_old = rms_old**2 + mom1_old**2
                 mom2_new = (float(tstpar(1).nped_samples-1)*mom2_old +
     +                adc(ichan,ibuck)**2)/float(tstpar(1).nped_samples)
                 rms_new2 = mom2_new - mom1_new**2
                 tpc_pedestal(itemp).ave = mom1_new
                 if (rms_new2.gt.0.0) then
                    tpc_pedestal(itemp).rms = sqrt(rms_new2)
                 else
                    tpc_pedestal(itemp).rms = 0.0
                 endif
              enddo
           endif                !nsamples > 1
C========================== PULSER EVENT - GET GAIN =============================
        elseif (TSTPAR(1).PED_EVENT.eq.2) then ! pulser event
           if (TSTPAR(1).PED_SUBTRACT.eq.1) then ! subtract pedestals from data
c find pedestal (if any) for each electronics channel and subtract from data
c  do not assume pedestal table is in any order...
              do itemp=1,tpc_pedestal_h.nok
                 ichan = tpc_pedestal(itemp).channel
                 ibuck = tpc_pedestal(itemp).bucket
                 adc(ichan,ibuck) = adc(ichan,ibuck)-
     +                (tpc_pedestal(itemp).ave )
cNTBS     +                tpc_pedestal(itemp).rms * tstpar(1).times_rms)
                 if (adc(ichan,ibuck).lt.0.0) adc(ichan,ibuck) = 0.0
              enddo
           endif                ! ped_subtract=1
c
c we calculate here relative gain: the average gain is defined as unity.
           ngains=0
           gain_ave=0.0
           do ichan=1,nchan_per_rdo
              if (skip_flag(ichan).eq.0) then
                 rawsignal = peakheight(adc,ichan)
                 gain_ave=gain_ave+rawsignal
                 ngains = ngains+1
              endif
           enddo
           gain_ave = gain_ave/float(ngains)	! this is average gain for this event

           tstpar(1).ngain_samples = tstpar(1).ngain_samples + 1
c
c fill tstpar table telling what is average gain over all events and over all channels
           if (tstpar(1).ngain_samples.eq.1) then ! first pulser sample.. fill fresh
              tstpar(1).ave_gain = gain_ave
           else
              tstpar(1).ave_gain = 
     +             (float(tstpar(1).ngain_samples-1)*tstpar(1).ave_gain +
     +             gain_ave)/float(tstpar(1).ngain_samples)
           endif
c
c fill gain table
           if (tstpar(1).ngain_samples.eq.1) then ! first pulser sample.. fill fresh
              tpc_gain_h.nok = 0
              do ichan=1,nchan_per_rdo
                 if (skip_flag(ichan).eq.0) then
                    rawsignal = peakheight(adc,ichan)
                    tpc_gain_h.nok = tpc_gain_h.nok + 1
                    itemp = tpc_gain_h.nok 
                    tpc_gain(itemp).channel = ichan
                    tpc_gain(itemp).gain = rawsignal/gain_ave
                 endif
              enddo
           else                 ! not first pulser... update table
              do itemp=1,tpc_gain_h.nok
                 ichan = tpc_gain(itemp).channel
                 if (skip_flag(ichan).eq.1) then
                    id = 0
                    call message(
     +               'TSTAM - Should reset TPC_GAINs when BAD_CHANNELS changes! ',
     +                   1,id)
                 endif
                 rawsignal = peakheight(adc,ichan)
                 mom1_old = tpc_gain(itemp).gain
                 mom1_new = (float(tstpar(1).ngain_samples-1)*mom1_old +
     +                rawsignal/gain_ave)/float(tstpar(1).ngain_samples)
                 tpc_gain(itemp).gain = mom1_new
              enddo
           endif
C========================== GEOMETRY EVENT =============================
        elseif (tstpar(1).ped_event.eq.-1) then ! geometry event
c configure the geometry... fill the readout_map and bad_channels tables...
           if (tstpar(1).format.eq.1) then ! minidaq format -> first instance of FEE
                                           !   card is "leftmost-outermost" pad of card

              write(6,*)'As per Fred - I subtract 768 from FEE address'
c keep track of #times a fee card is seen in data
              do ifee=0,feeMax
                 ntimes_seen(ifee) = 0
              enddo
              readout_map_h.nok = 0
              do ichan=1,nchan_per_rdo

c ignore our *special* guys... -- fix for bad electronics
                 do itemp2=1,nbad_channel_ranges
                    if ((ichan.ge.ignore_range_lo(itemp2)).and.
     +                   (ichan.le.ignore_range_hi(itemp2))) then
                       bad_channels_h.nok = bad_channels_h.nok+1
                       itemp = bad_channels_h.nok
                       bad_channels(itemp).channel = ichan
                       write(33,'(4(i4,2x),a)')ichan,ifee,-99,-99,
     +                      ' IGNORED'
                       
                       goto 11
                    endif
                 enddo
c------ end of fix -- anyone who makes it here should be good...

                 ifee = nint(adc(ichan,1)) - 768

c bail on wierd fee numbers...
                 if ((ifee.lt.0).or.(ifee.gt.feeMax)) then
                    write(6,*)'Unknown FEE card number: IFEE = ',ifee,
	2		', ICHAN = ', ichan
cNTBS                    write(6,*)' Quitting ...'
cNTBS                    return
                    write(6,*)'Adding it to the bad_channels table...'
                    bad_channels_h.nok = bad_channels_h.nok+1
                    itemp = bad_channels_h.nok
                    bad_channels(itemp).channel = ichan
                    goto 11
                 endif
                 ntimes_seen(ifee) = ntimes_seen(ifee) + 1
                 if (ntimes_seen(ifee).gt.32) then
                    write(6,*)'Fee card ',ifee,' shows up >32 times!! quitting'
                    write(6,*)'Channel = ',ichan
                    return
                 endif
                 ipin = pin(ntimes_seen(ifee)) ! 7,9,11,..37 then 8,10,12,...38
                 if (ipin.lt.0) then
                    write(6,*)'Mikes bug! ipin<0 for instance',ntimes_seen(ifee)
                    write(6,*)'Quitting...'
                    return
                 endif
                 irow = row_fee(ifee,ipin)
                 ipad = pad_fee(ifee,ipin)
                 if ((irow.lt.0).or.(ipad.lt.0)) then ! pin not connected to a pad = "bad"
                    bad_channels_h.nok = bad_channels_h.nok+1
                    itemp = bad_channels_h.nok
                    bad_channels(itemp).channel = ichan
                    write(33,'(4(i4,2x),a)')ichan,ifee,ntimes_seen(ifee),ipin,
     +                   ' unconnected'
                 else
                    readout_map_h.nok = readout_map_h.nok+1
                    itemp = readout_map_h.nok
                    readout_map(itemp).channel = ichan
                    readout_map(itemp).pad = ipad
                    readout_map(itemp).row = irow
                    readout_map(itemp).sector = 1
                    write(33,'(6(i4,2x))')ichan,ifee,ntimes_seen(ifee),ipin,irow,ipad
                 endif
 11              continue
              enddo
           else
              write(6,*)'I cannot configure geometry in this format! - quitting'
           endif
C========================== REGULAR EVENT =============================
        elseif ((tstpar(1).ped_event.eq.0).or.
     +          (tstpar(1).ped_event.eq.3)) then ! data event
c-------- Pedestal Subtraction ---------
           if (TSTPAR(1).PED_SUBTRACT.eq.1) then ! subtract pedestals from data
              do itemp=1,tpc_pedestal_h.nok
                 ichan = tpc_pedestal(itemp).channel
                 ibuck = tpc_pedestal(itemp).bucket
                 adc(ichan,ibuck) = adc(ichan,ibuck)-
     +                (tpc_pedestal(itemp).ave ) 
cNTBS     +                tpc_pedestal(itemp).rms * tstpar(1).times_rms)
                 if (adc(ichan,ibuck).lt.0.0) adc(ichan,ibuck) = 0.0
              enddo
           endif                ! ped_subtract=1
c--------- Gain Correction --------------
           if (TSTPAR(1).NGAIN_SAMPLES.gt.0) then ! gain-correct data
              do itemp=1,tpc_gain_h.nok
                 ichan=tpc_gain(itemp).channel
                 if (tpc_gain(itemp).gain.gt.0.0) then
                    do ibuck=1,nbuck
                       adc(ichan,ibuck) = adc(ichan,ibuck)/tpc_gain(itemp).gain
                    enddo
                 else
                    write(6,*)'Channel ',ichan,' has gain',tpc_gain(itemp).gain
c                    do ibuck=1,nbuck
c                       adc(ichan,ibuck) = 0.0
c                    enddo
                 endif
              enddo
           endif
c---------- Overwrite raw ADC table with corrected ADC table -------------
           if (tstpar(1).ped_event.eq.3) then
              do ichan=1,num_chan
                 do ibuck=1,nbuck
                     minidaq_pix(ichan).data(ibuck) = nint(adc(ichan,ibuck))
                 enddo
              enddo
           endif
c---------- Now fill tables used by cluster/hitfinder
c find out what sector,row,pad (if any) each channel maps to...
           call vzero(sector_row,nchan_per_rdo)
           call vzero(pad,nchan_per_rdo)
           do itemp=1,readout_map_h.nok
              ichan= readout_map(itemp).channel
              sector_row(ichan) = 100*readout_map(itemp).sector +
     +             readout_map(itemp).row
              irow = readout_map(itemp).row
              pad(ichan) = readout_map(itemp).pad
           enddo
c now fill tppad and tppixel, finding sequences on the way...
           do ichan=1,nchan_per_rdo
              if (skip_flag(ichan).eq.0) then
                 nseq = 0
                 last_buck=-100
                 do ibuck=1,nbuck
                    iadc = nint(adc(ichan,ibuck))
                    if (iadc.gt.tstpar(1).thresh) then
                       tppixel_h.nok=tppixel_h.nok+1
                       if (tppixel_h.nok.le.tppixel_h.maxlen) then
                          tppixel(tppixel_h.nok).datum =
     +                         itdc(ibuck) + iadc
                       else
                          id=0
                          call message('TSTAM - Pixel table full! ',1,id)
                          tstam = STAFCV_BAD
                          return
                       endif
                       if ((ibuck-last_buck).gt.1) then ! new sequence...
                          nseq = nseq+1
                          if (nseq.eq.1) then ! first sequence on pad
                             tppad_h.nok = tppad_h.nok+1
                             if (tppad_h.nok.le.tppad_h.maxlen) then
                                tppad(tppad_h.nok).tpc_row =
     +                               sector_row(ichan)
                                tppad(tppad_h.nok).secpad = pad(ichan)
                                tppad(tppad_h.nok).jpix = tppixel_h.nok
                             else
                                id=0
                                call message('TSTAM - TPPAD table full! ',1,id)
                                tstam = STAFCV_BAD
                                return
                             endif
                          else  ! not first sequence... finish tppixel.datum for old sequence
                             do ipix=1,npix_seq
                                tppixel(tppixel_h.nok-ipix).datum =
     +                               tppixel(tppixel_h.nok-ipix).datum + iseq(npix_seq)
                             enddo
                          endif
                          npix_seq = 0 ! restart #pixels-in-sequence counter
                       endif    ! if new sequence
                       npix_seq = npix_seq+1
                       last_buck = ibuck
                    endif       ! iadc.gt.thresh
                 enddo          ! ibuck
c done with this pad... clean up last sequence on this pad...
                 if (nseq.ne.0) then
                    do ipix=0,npix_seq-1
                       tppixel(tppixel_h.nok-ipix).datum =
     +                      tppixel(tppixel_h.nok-ipix).datum + iseq(npix_seq)
                    enddo
                    tppad(tppad_h.nok).nseq = nseq
                 endif
              endif
           enddo                ! ichan
        endif
        return
c
 9      continue                ! read error
        write(6,*)'Error reading file/lun : ',TSTPAR(1).FILIN,lunin
        tstam = STAFCV_BAD

        return
        end

c------------- routine for getting "peak height" from pulser event ----------------
      real function peakheight(adc,ichan)
c
c right now we use a rather naive extraction of signal size.
c Roy will do better in the i960.
c input is the pedestal-subtracted ADC value, output is the "signal size"
c
      implicit none
      integer ichan,ibuck,nchan_per_rdo,nbuck
      parameter(nchan_per_rdo=1152,nbuck=512)
      real adc(nchan_per_rdo,nbuck) ! this is 2 MB-- it's kind of wasteful, but...
      real adcmax
      integer buckmax

**      goto 1

      buckmax=-1
      adcmax = 0.0
*********      do ibuck=1,nbuck
      do ibuck=50,120           ! so that we don't encounter wierd ends
         if (adc(ichan,ibuck).gt.adcmax) then
            adcmax = adc(ichan,ibuck)
            buckmax = ibuck
         endif
      enddo
      if ((buckmax.le.2).or.(buckmax.ge.nbuck-1)) then
         write(6,*)'Hey-- biggest bucket is close to end: ',
     +        buckmax,ichan
         peakheight=0.0
      else
         peakheight = 
     +        adc(ichan,buckmax)+adc(ichan,buckmax-1)+adc(ichan,buckmax+1)+
     +        +adc(ichan,buckmax-2)+adc(ichan,buckmax+2)
      endif

** 1    continue                  ! do it roy's way...

c      peakheight = 0.0
c      do ibuck=1,nbuck
c         if (adc(ichan,ibuck).gt.2.0) peakheight=peakheight+adc(ichan,ibuck)
c      enddo

      end

