* $Id: tph_get_cluster_data.F,v 1.7 1999/10/07 03:21:38 snelling Exp $
* $Log: tph_get_cluster_data.F,v $
* Revision 1.7  1999/10/07 03:21:38  snelling
* changes related to StMessage
*
* Revision 1.6  1999/10/05 00:47:14  snelling
* the tdc range saga continues
*
* Revision 1.5  1999/10/01 03:16:57  snelling
* changed range for TDC from 1-512 to 0-511 (itdc_lo, itdc_hi) also in comments
*
* Revision 1.4  1999/07/17 00:32:07  genevb
* Use StMessMgr
*
* Revision 1.3  1999/06/09 14:45:43  hardtke
* add 1 to kseqpix
*
* Revision 1.2  1998/07/12 21:21:35  sakrejda
* protection against unphysical time buckets
*
* Revision 1.1  1998/05/22 00:18:22  hardtke
* modified hit finder with new data format
*
* Revision 1.1  1998/01/27 00:49:17  fisyak
* Split sources
*
        Subroutine tph_get_cluster_data(jseq, nseq, irow, isec, npads
     +,                            tpseq_h, tpseq 
     +,                            pixel_data_h, pixel_data
     +,                            nclspix, ipad, itdc, iadc, itrk
     +,				   nclpix_max
     +,				   npix_seq
     +,				   ipad_lo, ipad_hi, itdc_lo, itdc_hi)
C-----------------------------------------------------------------------

      IMPLICIT NONE

C   Input arguments  (One to a line with definition after ! ) 
C
C     jseq:        Index for tpseq
C     nseq:        Number of sequences in cluster
C     irow:        Row number for cluster (1-45)
C     isec:        Sector number for the cluster (1-24)
C     npads:       Number of pads in pad row
C     tpseq_h:
C     tpseq:
C     pixel_data_h
C     pixel_data:
C     nclpix_max:  Max. pixels in cluster
C
C   Output arguments : 
C
C     nclspix:      Number of pixels in cluster
C     ipad:         Pads for cluster pixels
C     itdc:         Time buckets for cluster pixels
C     iadc:
C     itrk:         Track ID for MC data
C     npix_seq:	(added 23 Nov 94) number of pixels in segment on each pad
C     ipad_lo:      pad number lower limit for cluster
C     ipad_hi:      pad number upper limit for cluster
C     itdc_lo:      TDC lower limit (0-511) for cluster
C     itdc_hi:      TDC upper limit (0-511) for cluster
C
C  Returns:
C       Contained in the output arguments...
C     
C   Functional Description : 
C    This subroutine unpacks the data associated with a given cluster
C    into the caller arrays.
C
C   Moved into TAS 12-Apr-1994   Richard Morse
C   Copyright 1994    Lawrence Berkeley Laboratory
c   
c   modifications:
c   ==============
C   23nov94 - m lisa - added output argument of npix_seq=number of pixels
C			in each sequence
C
C   30mar95 now sector=1..24,row=1..45
C
c   9apr96 -- malisa 
c	account for the fact that data is stored such that tdc=0..511
c	NOT 1..512.  The rest of the hitfinder assumes minimum tdc
c	is 1.  This is the only routine where tdc information is unpacked,
c	so I just add one to it here...
c
c   03-NOV-1997 --- R.Bossingham
c       * Find/return lower and upper tdc, pad-number limits;
c         change subroutine arguments accordingly.
c       * Update max. pads -> 182, max. time buckets -> 512
c       * Add number of pads per row
c       * Explicitly save variables that must be non-volatile
c       * Remove some obsolete (commented) code
c
C   21-May-1998 --- D. Hardtke
C       * This is a hacked version of tph_get_cls_data using the new data
C         Format.      
C   
C   Error conditions : 
C   ================
C-----------------------------------------------------------------------
#include "PAM.inc"
#include "tcl_tp_seq.inc"
      RECORD      /TABLE_HEAD_ST/                 tpseq_h
      RECORD       /TCL_TP_SEQ_ST/                tpseq(*)
#include "type_shortdata.inc"
      RECORD      /TABLE_HEAD_ST/               pixel_data_h
      RECORD     /TYPE_SHORTDATA_ST/            pixel_data(*)

C-----------------------------------------------------------------------
c passed variables:
c
        integer iadc(*)               ! ADC's for cluster pixels
        integer ipad(*)               ! Pads for cluster pixels
        integer ipad_lo, ipad_hi      ! Pad number extent for cluster
        integer irow                  ! Row number for cluster (1-45)
        integer isec                  ! Sector number for cluster (1-24)
        integer itdc(*)               ! Time buckets for cluster pixels
        integer itdc_lo, itdc_hi      ! Time bucket extent for cluster
        integer itrk(*)               ! Track ID for MC data
        integer jseq                  ! Index for tpseq
        integer nclspix               ! Number of pixels in cluster
        integer npads                 ! Number of pads in cluster's pad row
	integer npix_seq(*)           ! Number of pixels in sequence on pad
        integer nseq                  ! Number of sequences in cluster
C-----------------------------------------------------------------------
c local variables:
c
        integer nbuck_max             ! Total number of time buckets
        integer npads_max             ! Largest number of pads in any row
	integer numseq_factor         ! Decoding constant for tppixel
	integer tdc_factor            ! Decoding constant for tppixel
c
        parameter (nbuck_max=511)
        parameter (npads_max=182)
	parameter (numseq_factor='100000'X)
	parameter (tdc_factor='400'X)
C
        integer iclspix               ! Pixel index within cluster
        integer indx                  ! General index
	integer iseq_num              ! Sequence index
        integer jpix                  ! Pixel index
        integer kseq                  ! Sequence index
        integer kseqpix               ! Number of time buckets in sequence
        integer ktrk                  ! Track number for MC data
        integer ldatum                ! tppixel word
        integer nclpix_max            ! Max. pixels in cluster
        integer local_tdc_low
        integer local_tdc_high

c message call related variables
        character*132 m132

c
c 24mar95 -now data is packed in a new way in tppixel
C-----------------------------------------------------------------------

        iclspix = 0
	iseq_num = 0
        ipad_lo = npads_max
        ipad_hi = 1
        itdc_lo = nbuck_max
        itdc_hi = 0
        do kseq = jseq, jseq+nseq-1
           jpix = tpseq(kseq).jpix
           local_tdc_low=tpseq(kseq).tdc_low
           local_tdc_high=tpseq(kseq).tdc_hi
           if(local_tdc_high.gt.512) local_tdc_high=512
           
           iclspix = iclspix + 1

	   if (iclspix .gt. nclpix_max) then
              write(m132,*) 'TPHAM-!-TMP too many pixels!'
              call StError(m132)
              return
	   endif

           ldatum = pixel_data(jpix).data
c
c          for MC data!
c
           itrk(iclspix) = 0
c new way to unpack data
c           kseqpix = tpseq(kseq).tdc_hi - tpseq(kseq).tdc_low
CDH           kseqpix = local_tdc_high -local_tdc_low
           kseqpix = local_tdc_high -local_tdc_low + 1
         
c           itdc(iclspix) = tpseq(kseq).tdc_low
           itdc(iclspix) = local_tdc_low
           iadc(iclspix) = ldatum
c
           itdc_lo = MIN(itdc_lo,itdc(iclspix))
           itdc_hi = MAX(itdc_hi,itdc(iclspix)+kseqpix-1)
C
           ipad(iclspix) = tpseq(kseq).secpad
           ipad_lo = MIN(ipad_lo,ipad(iclspix))
           ipad_hi = MAX(ipad_hi,ipad(iclspix))
C
	   iseq_num = iseq_num+1
	   npix_seq(iseq_num) = kseqpix
C
C Step through one sequence on one pad:
           do indx = jpix+1, jpix+kseqpix-1
              iclspix = iclspix + 1
	      if (iclspix .gt. nclpix_max) then
                 write(m132,*) 'TPHAM-!-TMP too many pixels!'
                 call StError(m132)
                 return
	      endif
              ldatum = pixel_data(indx).data
              itdc(iclspix) = itdc(iclspix-1)+1
c
c itdc(iclspix-1) = itdc(iclspix)-1
              iadc(iclspix) = ldatum
              ipad(iclspix) = ipad(iclspix-1)
c
c             for MC data!
c
              itrk(iclspix) = 0
           enddo
        enddo

        nclspix = iclspix

        return
        end
