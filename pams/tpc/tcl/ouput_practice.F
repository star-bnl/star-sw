* $Id: ouput_practice.F,v 1.1 1998/01/27 00:49:10 fisyak Exp $
* $Log: ouput_practice.F,v $
* Revision 1.1  1998/01/27 00:49:10  fisyak
* Split sources
*
        subroutine ouput_practice(ipad,itdc,iadc,nclspix,irow,nhits)
c
c just calculates some integer moments and outputs them for playing with
c
        implicit none

C-----------------------------------------------------------------------
c passed variables:
c
        integer iadc(*)               ! ADC's for cluster pixels
        integer ipad(*)               ! Pads for cluster pixels
        integer irow                  ! Row number for cluster (1-45)
        integer itdc(*)               ! Time buckets for cluster pixels
        integer nclspix               ! Number of pixels in cluster
        integer nhits                 ! Number of hits in cluster

C-----------------------------------------------------------------------
c local variables:
c
        integer ipix                  ! Pixel index within cluster
c
c see pp21-22 of STAR tcl notebook #2
c
        real a                        ! ADC sum in cluster
        real pa                       ! ADC*Pad sum in cluster
        real ppa                      ! ADC*Pad^2 sum in cluster
        real pta                      ! ADC*Pad*TDC sum in cluster
        real rms2_mix                 !
        real rms2_pad                 ! RMS of pad number in cluster
        real rms2_tdc                 ! RMS of TDC in cluster
        real ta                       ! ADC*TDC sum in cluster
        real tdc_mean                 ! TDC weighted mean
        real tta                      ! ADC*TDC^2 sum in cluster

C-----------------------------------------------------------------------

        a   = 0.
        pa  = 0.
        ppa = 0.
        pta = 0.
        ta  = 0.
        tta = 0.
        do ipix=1,nclspix
           a   = a   + iadc(ipix)
           pa  = pa  + ipad(ipix)*iadc(ipix)
           ppa = ppa + ipad(ipix)*(ipad(ipix)*iadc(ipix))
           ta  = ta  + itdc(ipix)*iadc(ipix)
           pta = pta + ipad(ipix)*(itdc(ipix)*iadc(ipix))
           tta = tta + itdc(ipix)*(itdc(ipix)*iadc(ipix))
        enddo
        rms2_pad = sqrt(ppa/a - (pa/a)**2)
        rms2_tdc = sqrt(tta/a - (ta/a)**2)
        rms2_mix = sqrt(pta/a - pa*ta/(a**2))
        tdc_mean = ta/a

c ??? Direct write is forbidden
        write(55,*)rms2_pad,rms2_tdc,rms2_mix,tdc_mean,irow,nhits

        return
        end
