c:Copyright 1996, Wayne State University
c:>---------------------------------------------------------------------
c:FILE:         scp_init_ped.f
c:DESCRIPTION:  SVT Pedestal Calculation
c:AUTHOR:       Claude Pruneau
c:BUGS:         in development
c:HISTORY:      created May 2, 1996
c:<---------------------------------------------------------------------
c:<---------------------------------------------------------------------
C:ROUTINE:	INTEGER*4 scp_init_ped
C:DESCRIPTION:	Initialize SVT pedestal averages and rms calculations
C:              by resetting accumulators
C:
C:              This routine works on one wafer at a time, i.e.
C:              the pedm table carries pixel pedestals for one
C:              wafer: 2 hybrids.
C:
C:ARGUMENTS:	
C     pedm  : Pedestal Moments:  0th, 1st, and 2nd moment accumulators
C:RETURN VALUE:	TRUE or FALSE
c:<---------------------------------------------------------------------
      Integer function scp_init_ped(
     &     par_h,   par,
     &     pedm_h,  pedm)

#include "scp_init_ped.inc"

c     Local Declarations
c     ------------------
      integer ipixel

      integer   msg_id(1)
      character msg*132
      save      msg_id

c     FORMAT
c     ------
 1001 FORMAT(' SCP_INIT_PED-I1 : Pedestal Accumulators have been reset')

C     EXECUTABLE
c     ----------
      scp_init_ped = STAFCV_OK

c     reset moment accumulators to zero

      do ipixel = 1, pedm_h.maxlen
         pedm(ipixel).m0 = 0.
         pedm(ipixel).m1 = 0.
         pedm(ipixel).m2 = 0.
      end do

      write(6,1001) 
c      call message(msg,1,msg_id(1))

      return
      end
