c:Copyright 1996, Wayne State University
c:>---------------------------------------------------------------------
c:FILE:         scp_ana_inj.F
c:DESCRIPTION:  SVT Drift Time Calculation from Injectors
c:AUTHOR:       Claude Pruneau
c:BUGS:         in development
c:HISTORY:      created Aug 15, 97
c:<---------------------------------------------------------------------
c:<---------------------------------------------------------------------
C:ROUTINE:	INTEGER*4 sci_ana_inj
C:DESCRIPTION:	
c:     This routines uses raw SVT, un-zero-suppressed, accumulated
c:     with "empty" triggers to evaluate pedestal averages and standard
c:     deviations. It accumulates the 0th, 1st, and 2nd moment of
c:     the pedestal of each SVT pixels.
c:     It assumes data from one Hybrid, i.e. one half wafer worth of data.
C:ARGUMENTS:	
c
C     par        : pedestal determination parameters
C     bad_anodes : list of bad anodes
C     raw        : raw data (Zero suppressed, pedestal subtracted)
C     cluster    : geographical info attached to "raw"
C     inj        : line cluster characteristics
C
C: Calls:
c:    logical function is_bad_anode(anode)  : a function that returns 
c:                                         true if anode is BAD 
C
C:RETURN VALUE:	TRUE or FALSE
c:<---------------------------------------------------------------------
      Integer function sci_ana_inj(
     &     par_h,        par,
     &     bad_anodes_h, bad_anodes,
     &     raw_h,        raw,
     &     clus_h,       clus,
     &     inj_h,        inj)

#include "sci_ana_inj.inc"

c     Local Declarations
c     ------------------
      logical OK
      integer ianode, injec, iline, iadc, itime, iclus, ntime
      integer iinj, injector, itime_peak
      real    sum_w, time_w, rms_w, sum_f, time_f, rms_f
      real    adc(128), adc_peak

c     External Declarations
c     ---------------------
      logical  is_bad_anode
      external is_bad_anode

c     FORMAT
c     ------
 1003 format(1x,'SCI_ANA_INJ - W1 : Line Cluster of zero length for iclus:',i6)
 1004 format(1x,'SCI_ANA_INJ - W2 : Bad Time Index for iclus:',i6, ' Time=',i6)
 1005 format(1x,'SCI_ANA_INJ - W3 : ADC Index out of range for iclus:',i6,' Index:',i12)
 1006 format(1x,'SCI_ANA_INJ - W4 : Anode Index out of range for iclus:',i6,' Anode=',i6)
 1007 format(1x,'SCI_ANA_INJ - W5 : IINJ counter exceeds "inj" table size, iinj=',i6)
 1008 format(1x,'SCI_ANA_INJ - I1 : Current cluster not used or rejected')

C     EXECUTABLE
c     ----------
C     Assume success!

      sci_ana_inj = STAFCV_OK

C     Sanity check

      if (raw_h.nok .lt. 1 .or.
     &     raw_h.nok .gt. raw_h.maxlen .or.
     &     clus_h.nok .lt. 1 .or.
     &     clus_h.nok .gt. clus_h.maxlen .or.
     &     inj_h.maxlen .lt. 960) then
         sci_ana_inj = STAFCV_BAD
         return
      end if

c     zero counters

      iinj = 0

C     Loop on all line clusters

      do iclus = 1, clus_h.NOK

c     sanity check, skip cluster if something is bad.

         if (clus(iclus).anode .lt. 0 .or.
     &           clus(iclus).anode .gt. 239) then
            write(6,1006) iclus, clus(iclus).anode
            goto 100
         else if (is_bad_anode(bad_anodes,bad_anodes_h,clus(iclus).anode)) then
c     no message issued for known bad anodes.
            goto 100
         else if (clus(iclus).n .lt. 1) then
            write(6,1003) iclus
            goto 100
         else if (clus(iclus).time .lt. 0 .or.
     &           clus(iclus).time  .gt. 127) then
            write(6,1004) iclus, clus(iclus).time
            goto 100
         else if (clus(iclus).adc_ndx .lt. 0 .or.
     &           clus(iclus).adc_ndx  .gt. raw_h.nok) then
            write(6,1005) iclus, clus(iclus).adc_ndx
            goto 100
         else 
         end if

c     copy the line cluster to a work array
c     for convenience. Accumulate moments.

         iadc  = clus(iclus).adc_ndx
         ntime = clus(iclus).n
         sum_w  = 0
         time_w  = 0
         rms_w  = 0
         adc_peak   = 0
         itime_peak = 0
         do itime = 1, ntime
            iadc = iadc + 1
            adc(itime) = raw(iadc).adc
            sum_w = sum_w + adc(itime)
            time_w = time_w + itime*adc(itime)
            rms_w  = rms_w + itime*itime*adc(itime)
            if ( raw(iadc).adc .gt. adc_peak) then
               adc_peak   = raw(iadc).adc
               itime_peak = itime
            end if
         end do

c     calculate weighted mean

         if (sum_w .gt.0.) then
            time_w = time_w/sum_w
            rms_w = rms_w/sum_w
            rms_w = rms_w - time_w*time_w
            if (rms_w .gt. 0.) then
               rms_w = sqrt(rms_w)
               OK    = .true.
            else
               rms_w = -1
               OK    = .false.
            end if
         else
            time_w = -1
            rms_w  = -1	
            OK   = .false.
         end if

c     if this is good candidate, find which injection
c     line it might correspond to and do better calculation

         if (OK .and. adc_peak .gt. par(1).peak_min) then
            if (time_w .ge. par(1).t_drift_min(1) .and. 
     &           time_w .le. par(1).t_drift_max(1) )  then
               injector = 0
            else if (time_w .ge. par(1).t_drift_min(1) .and. 
     &           time_w .le. par(1).t_drift_max(1) )  then
               injector = 1
            else if (time_w .ge. par(1).t_drift_min(1) .and. 
     &           time_w .le. par(1).t_drift_max(1) )  then
               injector = 2
            else if (time_w .ge. par(1).t_drift_min(1) .and. 
     &           time_w .le. par(1).t_drift_max(1) )  then
               injector = 3
            else
c     this cluster does not satisfy any of the lines 
               injector = -1
               OK = .false.
            end if
         end if

         if (OK) then
c     gauss fit goes here
         end if


c     store result to inj table

         if (OK) then
            iinj = iinj + 1
            if (iinj .gt. inj_h.maxlen) then
               write(6,1007) iinj
               iinj = iinj - 1
            else
               inj(iinj).id       = iinj
               inj(iinj).anode    = clus(iclus).anode
               inj(iinj).injector = injector
               inj(iinj).sum_w    = sum_w
               inj(iinj).time_w   = time_w
               inj(iinj).rms_w    = rms_w
               inj(iinj).sum_f    = sum_f
               inj(iinj).time_f   = time_f
               inj(iinj).rms_f    = rms_f
               inj(iinj).n        = 1
            end if
         else
            write(6,1008) 
         end if
 100     continue

      end do
      
      inj_h.nok = iinj

      return
      end
