* $Id: calcuaverage.F,v 1.1 2000/06/14 15:17:21 aihong Exp $
*  average.F
* This function is splited out from tde.F

      INTEGER FUNCTION calcuaverage(tdpar_h,tdpar,
     >                         track_h,track,j,
     >                         nde_orig,nde,dedx)

      IMPLICIT NONE

C  Input arguements
C  tdpar_h = header for the tphit table
C  tdpar   = rows of the tdeparm table
C  dedxlocal = rows of the dedx local in tde.F
C  nde       = # of elements of  dedxlocal
#include "PAM.inc"
#include "tdeparm.inc"
#include "tpt_track.inc"

      RECORD      /TABLE_HEAD_ST/                 tdpar_h
      RECORD         /TDEPARM_ST/                tdpar(*)
      RECORD      /TABLE_HEAD_ST/                 track_h
      RECORD       /TPT_TRACK_ST/                track(*)


      STRUCTURE /tde_local/
                REAL length
                REAL charge
                REAL adedx
                REAL sorta_dedx
                INTEGER row
      END STRUCTURE


      INTEGER nde,j,k,irow,nde_trunc,ndemax
      INTEGER hhid, hnbin, nde_orig

      real sumsq, mean, sigma, sigma_scaled, sum_weight,ds,weight
      real ascale, bscale, scale, meanscale, arms, brms, rms 
      real mean_scaled

      real hxmin,hxmax,hxamp
      REAL hparam(3),hstep(3),hpmin(3),hpmax(3),hsig(3),hchi2

      PARAMETER (ndemax=1000)
      RECORD  /tde_local/ dedx(ndemax)

      EXTERNAL landau2
C-----------------------------------------------------------------------




              track(j).dedx(1)=0.0
              track(j).dedx(2)=0.0

         if(tdpar(1).averaging_method.eq.0.and.nde.gt.0) then

            sumsq = 0
            do k=1,nde
               track(j).dedx(1)=track(j).dedx(1)+ dedx(k).length
               track(j).dedx(2)=track(j).dedx(2)+ dedx(k).charge
               sumsq = sumsq + (dedx(k).charge)**2
            end do
            mean = track(j).dedx(2)/nde
            sigma = sqrt(sumsq/nde - mean**2)
            track(j).dedx(1)=track(j).dedx(2)/track(j).dedx(1)
            track(j).dedx(2)=sigma/sqrt(real(nde))/track(j).dedx(1)

         else if((tdpar(1).averaging_method.eq.1..or.
     >          tdpar(1).averaging_method.eq.4).and.nde.gt.0) then
            sumsq = 0
            do k=1,nde
               track(j).dedx(1)= track(j).dedx(1)+ dedx(k).adedx
               sumsq = sumsq + (dedx(k).adedx)**2
            end do
            track(j).dedx(1)=track(j).dedx(1)/real(nde)
            sigma = sqrt(sumsq/real(nde) - (track(j).dedx(1))**2)
            track(j).dedx(2)=sigma/sqrt(real(nde))
c
c calc weighted as well
c 
            sum_weight = 0.
            sumsq = 0.
            mean=0.


            do k=1,nde
c
c first scale the dedx by path-length dependence
c    dedx = a(1-b/dx) 
c                     
               irow = dedx(k).row
               ds=dedx(k).length
               if (irow.lt.13) then
                     ascale= 0.636 
                     bscale = 0.34 
                     arms = 0.195
                     brms = 0.050
                else   
                     ascale = 0.632 
                     bscale = 0.26
                     arms = 0.170
                     brms = 0.053 
               endif
               meanscale = 0.632*(1-0.26/2.2)
               scale = ascale*(1-bscale/ds)/meanscale
               rms = arms + brms/sqrt(ds)
c               weight = 1./rms
               weight = sqrt(ds)
               mean=mean+weight*(dedx(k).adedx/scale)
               sumsq=sumsq+weight*(dedx(k).adedx/scale)**2
               sum_weight=sum_weight+weight
            end do
            if (sum_weight.gt.0) then
               mean_scaled=mean/sum_weight
               sigma = sqrt(sumsq/sum_weight - mean_scaled**2)
               sigma_scaled=sigma/sqrt(sum_weight)
            endif
         elseif (tdpar(1).averaging_method.eq.3.and.nde.gt.10) then
c
c likelihood method
c get standard (0.7) mean dE/dx (hxamp) 
c for starting values and range of histogram
c
            hxamp=0.
            nde_trunc=nde_orig-int(0.3*real(nde_orig))
            do k=1,nde_trunc
               hxamp= hxamp + dedx(k).adedx
            end do
            if (nde_trunc.gt.0) then
               hxamp=hxamp/real(nde_trunc)
            endif
c histogram
            hxmin=0.
            hxmax=10.*hxamp
            hhid=101
            hnbin=50         
            call hbook1(hhid,' ',hnbin,hxmin,hxmax,0)
c
c fill with dE/dx for each hit
c
            do k=1,nde
               call hfill(hhid,dedx(k).adedx,0.,1.)
            enddo
c
c set initial values
c            hparam(1) = 20.
c
            hparam(1) = -6.267+ 1.864*real(nde)
            hparam(2) = hxamp
            hparam(3) = hxamp*0.2
c
c set bounds 
c
c            hpmin(1)=hparam(1)
c            hpmax(1)=hparam(1)+0.0001
c            hpmin(2)=0.
c            hpmax(2)=hxmax
c            hstep(1)=0.001
c            hstep(2)=hxmax/100.
            call hfith(hhid,landau2,'QL',3,
     >        hparam,hstep,hpmin,hpmax,hsig,hchi2)
            track(j).dedx(1)=hparam(2)
            track(j).dedx(2)=hsig(2)
c            write(6,*) 'resoultion' , hsig(2)/hparam(2)
            call hdelet(hhid)
         endif

      calcuaverage=STAFCV_OK

      end
