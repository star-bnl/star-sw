c:Copyright 1996, Wayne State University
c:>---------------------------------------------------------------------
c:FILE:         scp_calc_ped.f
c:DESCRIPTION:  SVT Pedestal Calculation
c:AUTHOR:       Claude Pruneau
c:BUGS:         in development
c:HISTORY:      created May 2, 1996
c:<---------------------------------------------------------------------
c:<---------------------------------------------------------------------
C:ROUTINE:	INTEGER*4 scp_calc_ped
C:DESCRIPTION:	Routine to calculate SVT pedestal averages and rms
C:
C:    This routine uses the pedestal momemts accumulated in the
C:    table pedm to determine the average and rms pedestal of each pixel
C:    and save these into the table spr_ped.
C:
C:    
C:
C:ARGUMENTS:	
C:     spr_par       : pedestal calculation control parameters
C:     spr_pedm      : 0th, 1st, and 2nd moment accumulators
C:     spr_ped       : 0th moment, average, standard deviation of pixel pedestals
C:     spr_bad_anode : list of bad anodes
C:
C:RETURN VALUE:	TRUE or FALSE
c:<---------------------------------------------------------------------
      Integer function scp_calc_ped(
     &     par_h, par,
     &     pedm_h,  pedm,
     &     ped_h,   ped,
     &     bad_anodes_h, bad_anodes)

#include "scp_calc_ped.inc"

c     Local Declarations
c     ------------------
      integer ipixel
      real    m0,m1,m2

      integer   msg_id(1)
      character msg*132
      save      msg_id

c     FORMAT
c     ------
 1001 format('SCP_PROCESS_PED-W1 : not enough events for pixel',i8)

C     EXECUTABLE
c     ----------
      scp_calc_ped =  STAFCV_OK

      do ipixel = 1, pedm_h.nok

c     If the current number of events is too low to estimate the pedestal average, 
c     Keep the current estimate as the final value. Issue warning message accordingly.
c     The minimum number of events required is settable through "par.n_min"

         if (pedm(ipixel).m0 .lt. par(1).n_min) then
            write(6,1001) 
c            call message(msg,1,msg_id(1))
            m0 = pedm(ipixel).m0
            m1 = 0
            m2 = -1    ! not enough data
         else 
            m0 = pedm(ipixel).m0
            m1 = pedm(ipixel).m1/m0
            m2 = pedm(ipixel).m2/m0
            m2 = m2 - m1*m1
            if (m2.gt.0) then
               m2 = sqrt(m2)
            else
               m2 = -2  ! m2 is zero or negative.
            end if
         end if
         pedm(ipixel).m0   = m0
         pedm(ipixel).m1   = ped(ipixel).avg + m1
         pedm(ipixel).m2   = m2
	 ped(ipixel).avg   = pedm(ipixel).m1 
      end do

c     Calculate the number of bad pixels
c     and decide if given anodes  should
c     be decalred bad anodes.

      bad_anodes_h.nok = 0
 
      return
      end

