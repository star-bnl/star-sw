c:Copyright 1996, Wayne State University
c:>---------------------------------------------------------------------
c:FILE:         scp.f
c:DESCRIPTION:  SVT Simple Pedestal Faker
c:AUTHOR:       Claude Pruneau
c:BUGS:         in development
c:HISTORY:      created Aug 8, 97
c:<---------------------------------------------------------------------
c:<---------------------------------------------------------------------
C:ROUTINE:	INTEGER*4 scp_fake_ped
C:DESCRIPTION:	
c:     This routines fakes pedestal in the SVT
c:
C:ARGUMENTS:	
c
C     par       : pedestal determination parameters
C     ped       : current pedestal estimate
C     pedm      : accumulators for 0th, 1st, and 2nd moments
C     raw       : raw data - unzero suppressed
C     bad_anode : list of bad anodes
C
C:NOTES:
C     The version as it stands assume that the pixel number
C     is continuous i.e. no mapping is done.
C:RETURN VALUE:	TRUE or FALSE
c:<---------------------------------------------------------------------
      Integer function scp_fake_ped(
     &     par_h,       par,
     &     ped_h,       ped,
     &     raw_h,       raw,
     &     bad_anodes_h, bad_anodes)

#include "scp_fake_ped.inc"

c     Local Declarations
c     ------------------
      integer ipixel
      integer iadc
      real     rg32,dummy
      external rg32

c     FORMAT
c     ------
C     EXECUTABLE
c     ----------
      scp_fake_ped = STAFCV_OK

      do ipixel = 1, raw_h.NOK

	raw(ipixel).adc = ped(ipixel).avg + par(1).rms*rg32(dummy)

      end do

      return
      end
