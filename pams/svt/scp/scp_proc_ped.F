c:Copyright 1996, Wayne State University
c:>---------------------------------------------------------------------
c:FILE:         scp.f
c:DESCRIPTION:  SVT Pedestal Calculation
c:AUTHOR:       Claude Pruneau
c:BUGS:         in development
c:HISTORY:      created May 2, 1996
c:<---------------------------------------------------------------------
c:<---------------------------------------------------------------------
C:ROUTINE:	INTEGER*4 scp_proc_ped
C:DESCRIPTION:	
c:     This routines uses raw SVT, un-zero-suppressed, accumulated
c:     with "empty" triggers to evaluate pedestal averages and standard
c:     deviations. It accumulates the 0th, 1st, and 2nd moment of
c:     the pedestal of each SVT pixels.
C:ARGUMENTS:	
c
C     par        : pedestal determination parameters
C     ped        : current pedestal estimate
C     pedm       : accumulators for 0th, 1st, and 2nd moments
C     raw        : raw data - unzero suppressed
C     bad_anodes : list of bad anodes
C
C:NOTES:
C     The version as it stands assume that the pixel number
C     is continuous i.e. no mapping is done.
C:RETURN VALUE:	TRUE or FALSE
c:<---------------------------------------------------------------------
      Integer function scp_proc_ped(
     &     par_h,       par,
     &     ped_h,       ped,
     &     pedm_h,      pedm,
     &     raw_h,       raw,
     &     bad_anodes_h, bad_anodes)

#include "scp_proc_ped.inc"

c     Local Declarations
c     ------------------
      integer ipixel
      integer iadc

      integer   msg_id(5)
      character msg*132
      save      msg_id

c     FORMAT
c     ------
 1001 format('SCP_PROC_PED-W1 : pedestal estimate above maximum',
     &     ' permitted value - will be reset')
 1002 format('SCP_PROC_PED-W2 : pedestal estimate below minimum',
     &     ' permitted value - will be reset')

C     EXECUTABLE
c     ----------
      scp_proc_ped = STAFCV_OK

      do ipixel = 1, raw_h.NOK

c     If the current estimate of the pedestal average is too low or
c     high, reset it to the low or high bound. 
c     Issue warning message accordingly.

         if (ped(ipixel).avg.lt.par(1).ped_min) then
            write(6,1001) 
c            call message(msg,1,msg_id(1))
            ped(ipixel).avg = par(1).ped_min
         else if (ped(ipixel).avg .gt.par(1).ped_max) then
            write(6,1002) 
c            call message(msg,1,msg_id(2))
            ped(ipixel).avg = par(1).ped_max
         end if

c     Accumulate 0th, 1st, and 2nd moment after offseting for
c     current estimate. The accumulation is done only if the
c     current value is within range defined by "dev_max"
c     i.e. the maximum deviation.

         iadc = raw(ipixel).adc - ped(ipixel).avg
         if (abs(iadc).le.par(1).dev_max) then
            pedm(ipixel).m0 = pedm(ipixel).m0 + 1
            pedm(ipixel).m1 = pedm(ipixel).m1 + iadc
            pedm(ipixel).m2 = pedm(ipixel).m2 + iadc*iadc
         end if

      end do

      return
      end
