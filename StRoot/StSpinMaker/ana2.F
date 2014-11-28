*  This routine analyzes sorted histograms of the SMD profile for an event.
*  Changes:
*  1) 16 August 2002
*     Included SMD strip gains.  The original program was intended to
*     be run on CWN data created by the XSYS sorting code.  Software
*     gain matching of the raw ADC values was performed in the XSYS
*     code to generate CWN data.
*     SMD gain values have been moved into this code.  The following 
*     coding changes have been made:
*     1.a) cde/fpd_smdpar.inc
*          Definitions of arrays smxgain(60) and smygain(100) have 
*          been added to COMMON/FPDSMD/
*     1.b) BLOCK DATA
*          cde/fpd_smdpar.inc has been included into BLOCK DATA
*          The output from spin:/home/bland/fpd/fpddev/smdgain/smdgain
*          has been used in DATA statements to define the SMD strip
*          gain values.
*          NOTE:  an intermediate program is required to map gain
*                 values from FERA module,subaddress to SMD strips.
*                 The XSYS sorting code applied gain corrections to
*                 the raw ADC values before mapping them to strip
*                 arrays.
*     1.c) SUBROUTINE INIT
*          CDE/FPD_SMDPAR.INC has been included in this subroutine.
*          COMMON/SMDGAIN/ is defined in that file, and contains
*          the SMD x,y software gain corrections.
*          The mapping of the CWN10 data onto the SMDX,SMDY arrays
*          used in the pi0 reconstruction analysis now includes
*          software gain corrections.  As well, the original data
*          is checked if it is positive.  Negative values for the
*          original data results in zero values for those strips in
*          the arrays SMDX,SMDY for the event.
*
*     With these changes, ana can no longer be applied to the CWN data
*     produced in 3/02.
*          
**************************************************************************

      subroutine init

#include "fpd_ntdat.inc"
#include "fpd_pi0dat.inc"

*  Initialize quantities
      esum=0
      do nx=1,ntx
         do ny=1,nty
            towe(nx,ny)=0
         enddo
      enddo
      finv2=-1
      timdif=0
      return
      end

***************************************************************
      subroutine fillpi1(adc,run)
      integer adc(256), run

*  Fills event histograms required for SMD-based pi0 reconstruction

#include "fpd_ntdat.inc"
#include "fpd_smdhst.inc"
#include "fpd_towgain.inc"
#include "fpd_smdpar.inc"

      character*50 htit
      byte btit(50)
      equivalence (btit(1),htit)
      data btit/50*0/

*      write(6,*) '**fillpi1** Fill event histos for pi0 recon ',
*     $     numevt,ihevt
      ihevt=ihevt+1
      if(ihevt.gt.ihmx0) then
         ihevt=1
      endif
      ih=ihevt+ismx0
*      write(cevt,'(i6.6)') numevt
      htit='SMX, run '//cpirun//' , event '//cevt
*      write(6,*) '**fillpi1** Reset histogram ',ih
      call hreset(ih,htit)
      do i=1,60
         call hfill(ih,float(i),0.0,float(adc(i+60))*smxgain(i))
c         write(*,*) float(i),float(adc(i+60)),smxgain(i),float(adc(i+60))*smxgain(i)
      enddo

      ih=ihevt+ismy0
      htit='SMY, run '//cpirun//' , event '//cevt
*      write(6,*) '**fillpi1** Reset histogram ',ih
      call hreset(ih,htit)
      do i=1,100
         call hfill(ih,float(i),0.0,float(adc(i+120))*smygain(i))
c   *      write(*,*) float(i),float(adc(i+120)),smygain(i),float(adc(i+120))*smygain(i)
      enddo

      ih1=ihevt+itow0
      htit='North towers, run '//cpirun//' , event '//cevt
*      write(6,*) '**fillpi1** Reset histogram ',ih1
      call hreset(ih1,htit)
      ih2=ihevt+ipre1
      htit='N preshower 1, run '//cpirun//' , event '//cevt
*      write(6,*) '**fillpi1** Reset histogram ',ih2
      call hreset(ih2,htit)
      ih3=ihevt+ipre2
      htit='N preshower 2, run '//cpirun//' , event '//cevt
*      write(6,*) '**fillpi1** Reset histogram ',ih3
      call hreset(ih3,htit)
      do j=1,12
         nr=(j-1)/4+1
         nc=j-(nr-1)*4
         if(adc(j+48).gt.0) then
            if(run.le.3014027) then
               call hfill(ih1,float(nc),float(nr),float(adc(j+48))*towgainN(j))
            else
               call hfill(ih1,float(nc),float(nr),float(adc(j+48))*towgainF(j))
            endif
         endif
         if(adc(j+220).gt.0) then
            call hfill(ih2,float(nc),float(nr),float(adc(j+220)))
         endif
         if(adc(j+236).gt.0) then
            call hfill(ih3,float(nc),float(nr),float(adc(j+236)))
         endif
      enddo
      return
      end

***************************************************************
      subroutine fillpi2

      implicit none

#include "fpd_ntdat.inc"
#include "fpd_pi0dat.inc"
#include "fpd_smd.inc"

      integer nmax
      real zgam                 ! photon energy sharing
      real SMXdif,SMYdif,SMDdif ! peak pair position differences
      real finv                 ! Invariant mass
      integer nx,ny,nt          ! loop variables
      integer icycle            ! HBOOK variable for HROUT
      integer filled


*      write(6,*) '**fillpi2** event number ',numevt

      filled=0
      npkpar=10*npeak(1)+npeak(2)
      if(npkpar.gt.11) then
         nmax=(nrmax-1)*4+ncmax
         zgam=(p1(4)-p2(4))/(p1(4)+p2(4))
         SMXdif=SMXp1cnt-SMXp2cnt
         SMYdif=SMYp1cnt-SMYp2cnt
         SMDdif=sqrt(SMXdif*SMXdif+SMYdif*SMYdif)
         if(finv2.gt.0) then
            finv=sqrt(finv2)
         else
            finv=-sqrt(-finv2)
         endif
         
         call hfill(101,SMXp1cnt,0.0,1.0)
         call hfill(102,SMXp1cnt,float(nrmax),1.0)
         call hfill(103,SMXp1sum,tmaxe,1.0)
         call hfill(104,SMXp1wid,0.0,1.0)
         
         call hfill(111,SMYp1cnt,0.0,1.0)
         call hfill(112,SMYp1cnt,float(ncmax),1.0)
         call hfill(113,SMYp1sum,tmaxe,1.0)
         call hfill(114,SMYp1wid,0.0,1.0)
         
         call hfill(121,SMXp2cnt,0.0,1.0)
         call hfill(124,SMXp2wid,0.0,1.0)
         call hfill(125,SMXqfit,0.0,1.0)
         
         call hfill(131,SMYp2cnt,0.0,1.0)
         call hfill(134,SMYp2wid,0.0,1.0)
         call hfill(135,SMYqfit,0.0,1.0)
         
         if(
     $        ((SMXp1cnt.ge.smxlo).and.(SMXp1cnt.le.smxhi)).and.
     $        ((SMXp2cnt.ge.smxlo).and.(SMXp2cnt.le.smxhi)).and.
     $        ((SMYp1cnt.ge.smylo).and.(SMYp1cnt.le.smyhi)).and.
     $        ((SMYp2cnt.ge.smylo).and.(SMYp2cnt.le.smyhi))
     $        ) then
            call hfill(141,finv,0.0,1.0)
            if(zgam.lt.0.7) then
               call hfill(142,finv,timdif,1.0)
               call hfill(143,finv,esum,1.0)
               call hfill(144,finv,float(nmax),1.0)
               call hfill(146,finv,SMXp1cnt,1.0)
               call hfill(147,finv,SMXp2cnt,1.0)
               call hfill(148,finv,SMYp1cnt,1.0)
               call hfill(149,finv,SMYp2cnt,1.0)
               call hfill(150,finv,SMXdif,1.0)
               call hfill(151,finv,SMYdif,1.0)
               call hfill(152,finv,SMDdif,1.0)
            endif
            
            call hfill(145,finv,zgam,1.0)
*     
            recon_mass=finv
            recon_ener=esum
            recon_phi=phipi
            recon_eta=etapi
            recon_zg=zgam
            smx1_sum=SMXp1sum
            smx1_cnt=SMXp1cnt
            smx1_wid=SMXp1wid
            smx2_sum=SMXp2sum
            smx2_cnt=SMXp2cnt
            smx2_wid=SMXp2wid
            smy1_sum=SMYp1sum
            smy1_cnt=SMYp1cnt
            smy1_wid=SMYp1wid
            smy2_sum=SMYp2sum
            smy2_cnt=SMYp2cnt
            smy2_wid=SMYp2wid
            smxq=SMXqfit
            smyq=SMYqfit
            do nx=1,ntowx
               do ny=1,ntowy
                  nt=4*(ny-1)+nx
                  towen(nt)=towe(nx,ny)
               enddo
            enddo
*************************
*            ntcnt=ntcnt+1
*            if(ntcnt.le.5000) then
               call hfnt(12)
               filled = 1
*              write(6,*) '**fillpi2** hfnt ',numevt,ntcnt
*            elseif(f_cwn.eq.1) then
*               call hrout(12,icycle,' ')
*               call hrend('fpd')
*               close (1)
*               call hdelet(12)
*               print *,'**fillpi2** ',
*     $              'Just wrote/close ntuple file after ntcnt ',
*     $              ntcnt
*               f_cwn=0
*               call ntfileopen
*               if(f_cwn.eq.1) ntcnt=0
*            endif
************************
         endif
      endif
      if(filled.eq.0) then
         call hfnt(12)
      endif         
*
      return
      end

***************************************************************
      subroutine anasmd(zvertex, bbcdiff, run)
      
      implicit none
      real zvertex, bbcdiff
      integer run

#include "fpd_smdhst.inc"
#include "fpd_pi0dat.inc"
#include "fpd_ntdat.inc"

      real frac
      parameter (frac=0.05)

      real sigmax
      real outpar
      real x0_1,x1_1,x2_1
      real x0_2,x1_2,x2_2
      common/fitsmd/sigmax,outpar(10),
     $     x0_1,x1_1,x2_1,
     $     x0_2,x1_2,x2_2

      integer npln
      integer ih,jh
      integer ihmin,ihmax
      integer i,n
      integer npass
      real r1,r2
*
      do npln=1,2
         if(npln.eq.1) then
            ih=ismx0+ihevt
         else
            ih=ismy0+ihevt
         endif
         ihmin=1
         ihmax=2
         do i=ihmin,ihmax
            jh=ih+i*ismdf
*      write(6,*)'**anasmd** Copy histograms ',ih,jh
            call hcopy(ih,jh,' ')
         enddo
         npass=1
         call fit_smd(ih,npass)
         call parsav(npln,npass)
         r1=x0_1/outpar(10)
         r2=x0_2/outpar(10)
         if(r1.gt.frac.or.r2.gt.frac) then ! try 2-peak fit
            npass=2
            npeak(npln)=2
            call fit_smd(ih+ihmin*ismdf,0)
            call parsav(npln,npass)
            call fit_smd2(ih+ihmax*ismdf,0,npln)
            call parsav2(npln)
         else                              ! do not try 2-peak fit
            do n=1,4
               savpar2(npln,n  )=savpar(npln,1,n)
               savpar2(npln,n+4)=savpar(npln,1,n)
            enddo
            savpar2(npln,5)=0              ! set 2-peak integral to zero
            savpar2(npln,9)=savpar(npln,1,5) ! fit quality
            npeak(npln)=1                  ! only single peak for SMD plane
            jh=ih+ihmax*ismdf
            call hdelet(jh)
*           write(6,*) '**anamsd** Copy histogram ',ih,jh
            call hcopy(ih,jh,' ')
         endif
      enddo
      if(npeak(1).eq.2.or.npeak(2).eq.2) then
         call pi0ana(zvertex, bbcdiff, run)
      endif
*     write(6,*) '**anasmd** end event ',numevt
      return
      end

***************************************************************
      subroutine pi0ana(zvertex, bbcdiff, run)
      
      implicit none
      real zvertex, bbcdiff
      integer run

#include "fpd_ntdat.inc"
#include "fpd_pi0dat.inc"
#include "fpd_smdhst.inc"
#include "fpd_smdpar.inc"
#include "fpd_towgain.inc"
*
      integer maxns
      parameter (maxns=100)
*
      real towrc(ntowx,ntowy)
      real smd_dif(2)
      real p1f(4),p2f(4)        ! 4 momenta of cluster pair (SMX based)
      real p1g(4),p2g(4)        ! 4 momenta of cluster pair (SMY based)
      real ppi(4)               ! 4 momentum of reconstructed cluster pair
*
      integer iht
      integer nx,ny
      integer i,np
      real tsum
      real d2,distance
      real f1,f2,g1,g2
      real finv2f,finv2g
      integer i1,i2,j1,j2
      real fwhm                ! function fwhm
      real zvert               ! computed z_vertex from BBC time difference
      real smdh0
      
      if(run.le.3014027) then
         smdh0=smdh0N
      else
         smdh0=smdh0F
      endif
*      write(6,*) '**pi0ana** event number',numevt

*      timdif=bbct(1)-bbct(2)
*      if(bbct(1).eq.1500 .and. bbct(2).eq.1500) then
*         timdif=25             ! set to mean value value for far position runs
*      endif
      timdif = bbcdiff

      iht=itow0+ihevt
      call hunpak(iht,towrc,'HIST',0)
      tsum=0
      tmaxe=0
      do nx=1,ntowx
         do ny=1,ntowy
            if(towrc(nx,ny).gt.0) then
               towe(nx,ny)=towrc(nx,ny)*escal
               tsum=tsum+towrc(nx,ny)
               if(towe(nx,ny).gt.tmaxe) then
                  tmaxe=towe(nx,ny)
                  ncmax=nx
                  nrmax=ny
               endif
            else
               towe(nx,ny)=0.0
            endif
         enddo
      enddo
*      write(6,*) '**pi0ana** tower summed energy ',tsum

      d2=0

      SMXp1sum=savpar2(1,1)
      SMXp1cnt=savpar2(1,2)
      SMXp1wid=fwhm(1,1)
      SMXp2sum=savpar2(1,5)
      SMXp2cnt=savpar2(1,6)
      SMXp2wid=fwhm(1,2)
      SMXqfit=savpar2(1,9)

      SMYp1sum=savpar2(2,1)
      SMYp1cnt=savpar2(2,2)
      SMYp1wid=fwhm(2,1)
      SMYp2sum=savpar2(2,5)
      SMYp2cnt=savpar2(2,6)
      SMYp2wid=fwhm(2,2)
      SMYqfit=savpar2(2,9)

      do np=1,2
         smd_dif(np)=savpar2(np,2)-savpar2(np,6)
         d2=d2+smd_dif(np)*smd_dif(np)
      enddo
      if(SMXp1sum.gt.SMXp2sum) then
         f1=SMXp1sum/(SMXp1sum+SMXp2sum)
         i1=2
         i2=6
         if(SMYp1sum.gt.SMYp2sum) then
            g1=SMYp1sum/(SMYp1sum+SMYp2sum)
            j1=2
            j2=6
         else
            g1=SMYp2sum/(SMYp1sum+SMYp2sum)
            j1=6
            j2=2
         endif
      else
         f1=SMXp2sum/(SMXp1sum+SMXp2sum)
         i1=6
         i2=2
         if(SMYp1sum.gt.SMYp2sum) then
            g1=SMYp1sum/(SMYp1sum+SMYp2sum)
            j1=2
            j2=6
         else
            g1=SMYp2sum/(SMYp1sum+SMYp2sum)
            j1=6
            j2=2
         endif
      endif
*
      esum=tsum*escal
*
*      zvert=smdz0+(timdif+atdf)*btdf ! find z_vertex from BBC times
*      zvert = 790.0
       zvert = 750.0
*      zvert = smdz0 + zvertex
*
      p1f(4)=f1*esum
      p1f(1)=f1*esum*(smyscl*savpar2(2,j1)-smdh0)/zvert
      p1f(2)=f1*esum*(smxscl*savpar2(1,i1)-smdv0)/zvert
      p1f(3)=sqrt(p1f(4)*p1f(4)-p1f(1)*p1f(1)-p1f(2)*p1f(2))
*
      f2=1-f1
*
      p2f(1)=f2*esum*(smyscl*savpar2(2,j2)-smdh0)/zvert
      p2f(2)=f2*esum*(smxscl*savpar2(1,i2)-smdv0)/zvert
      p2f(4)=f2*esum
      p2f(3)=sqrt(p2f(4)*p2f(4)-p2f(1)*p2f(1)-p2f(2)*p2f(2))
*
      finv2f=
     $      (p1f(4)+p2f(4))**2
     $     -(p1f(1)+p2f(1))**2
     $     -(p1f(2)+p2f(2))**2
     $     -(p1f(3)+p2f(3))**2

      p1g(1)=g1*esum*(smyscl*savpar2(2,j1)-smdh0)/zvert
      p1g(2)=g1*esum*(smxscl*savpar2(1,i1)-smdv0)/zvert
      p1g(4)=g1*esum
      p1g(3)=sqrt(p1g(4)*p1g(4)-p1g(1)*p1g(1)-p1g(2)*p1g(2))
*
      g2=1-g1
*
      p2g(1)=g2*esum*(smyscl*savpar2(2,j2)-smdh0)/zvert
      p2g(2)=g2*esum*(smxscl*savpar2(1,i2)-smdv0)/zvert
      p2g(4)=g2*esum
      p2g(3)=sqrt(p2g(4)*p2g(4)-p2g(1)*p2g(1)-p2g(2)*p2g(2))
*
      finv2g=
     $      (p1g(4)+p2g(4))**2
     $     -(p1g(1)+p2g(1))**2
     $     -(p1g(2)+p2g(2))**2
     $     -(p1g(3)+p2g(3))**2

      distance=sqrt(d2)

      if(abs(smd_dif(1)).gt.abs(smd_dif(2))) then
         do i=1,4
            p1(i)=p1f(i)
            p2(i)=p2f(i)
            finv2=finv2f
         enddo
      else
         do i=1,4
            p1(i)=p1g(i)
            p2(i)=p2g(i)
            finv2=finv2g
         enddo
      endif
      do i=1,4
         ppi(i)=p1(i)+p2(i)
      enddo
      pmagpi=sqrt(
     $     ppi(1)*ppi(1)+
     $     ppi(2)*ppi(2)+
     $     ppi(3)*ppi(3))
      thpi=acos(ppi(3)/pmagpi)
      etapi=-alog(tan(thpi/2))
      phipi=atan2(ppi(2),ppi(1))

*      write(6,*) '**pi0ana** mass^2 ',finv2f,finv2g,finv2
*      write(6,*) '**pi0ana** end event ',numevt
      return
      end

***************************************************************
      block data fpdpar
*----
#include "fpd_smdpar.inc"
*----Modified parameters to match those used in analysis of simulations
      data smxscl/0.373/  ! h/sqrt(3)+cladding, with h=triangle hght (cm)
      data smyscl/0.373/  ! h/sqrt(3)+cladding, with h=triangle hght (cm)
      data smdh0N/-23.6/   ! near posn dist, beam to SMX (cm)
      data smdh0F/-42.7/   ! near posn dist, beam to SMX (cm)
      data smdv0/10.82/   ! near posn dist, beam to SMY (cm)
*
*  3 July 2002  Modified to account for shift of z_vertex distribution
*               from FPD acceptance (convolution of diamond Gaussian
*               and dN/d_eta)
      data smdz0/750.0/   ! distance in cm from IP to mid-point of SMD
      data atdf/-100.0/   ! offset of BBC time difference to convert to z_vert
      data btdf/1.0/      ! slope of  BBC time difference to convert to z_vert

*  16-august-2002
*  Include SMD software gain parameters, sorted by strip numbers.
*  NOTE:  an intermediate program is required to map gain
*         values from FERA module,subaddress to SMD strips.
*         The XSYS sorting code applied gain corrections to
*         the raw ADC values before mapping them to strip
*         arrays.
*         spin:/home/bland/fpd/fpddev/smdgain/smdgain does this
*         mapping, producing strip sorted gains in the file smdgain.dat
*         The data statements below use the output from that file.
*
      data smxgain/
     $     0.81925 , 1.58648 , 1.30039 , 1.93758 , 0.68921 ,
     $     0.84525 , 0.80624 , 1.43043 , 0.72822 , 1.02731 ,
     $     0.65020 , 0.93628 , 1.14434 , 1.84655 , 0.74122 ,
     $     0.81925 , 0.89727 , 1.37841 , 0.67620 , 1.07932 ,
     $     0.78023 , 1.13134 , 0.85826 , 1.46944 , 0.94928 ,
     $     0.84525 , 0.98830 , 1.19636 , 0.70221 , 1.04031 ,
     $     1.05332 , 1.52146 , 1.13134 , 1.96359 , 1.06632 ,
     $     1.09233 , 1.07932 , 1.54746 , 0.81925 , 1.40442 ,
     $     1.07932 , 1.07932 , 1.13134 , 2.01560 , 0.93628 ,
     $     0.81925 , 1.05332 , 0.88427 , 0.94928 , 0.63719 ,
     $     0.79324 , 0.92328 , 1.27438 , 3.04291 , 0.74122 ,
     $     1.09233 , 1.01430 , 0.96229 , 0.79324 , 0.87126 /

      data smygain/
     $     1.07932 , 0.88427 , 1.11834 , 0.83225 , 0.78023 ,
     $     0.65020 , 0.80624 , 0.70221 , 0.76723 , 0.62419 ,
     $     0.67620 , 0.62419 , 0.76723 , 0.87126 , 0.74122 ,
     $     0.78023 , 0.92328 , 1.04031 , 0.78023 , 0.84525 ,
     $     0.78023 , 0.85826 , 0.42913 , 0.45514 , 1.59948 ,
     $     1.85956 , 1.62549 , 2.71782 , 0.76723 , 0.63719 ,
     $     1.26138 , 1.09233 , 0.46814 , 0.48114 , 3.27698 ,
     $     2.36671 , 2.82185 , 2.74382 , 1.37841 , 0.68921 ,
     $     1.39142 , 1.13134 , 0.55917 , 2.32770 , 2.89987 ,
     $     2.43173 , 2.65280 , 3.43303 , 1.54746 , 0.88427 ,
     $     1.11834 , 1.61248 , 0.61118 , 0.91027 , 2.28869 ,
     $     3.65410 , 2.50975 , 3.87516 , 0.92328 , 1.00130 ,
     $     1.15735 , 0.85826 , 0.46814 , 0.48114 , 2.40572 ,
     $     1.84655 , 8.98570 , 2.36671 , 0.96229 , 0.84525 ,
     $     1.20936 , 1.49545 , 0.65020 , 0.53316 , 2.89987 ,
     $     4.10923 , 3.47204 , 2.96489 , 1.01430 , 1.56047 ,
     $     1.31339 , 1.84655 , 0.63719 , 0.59818 , 2.95189 ,
     $     4.36931 , 3.15995 , 3.28999 , 1.13134 , 1.71651 ,
     $     1.37841 , 1.13134 , 0.80624 , 0.59818 , 2.54876 ,
     $     2.32770 , 3.12094 , 2.00260 , 1.00130 , 0.98830 /

*----
#include "fpd_smd.inc"
      data smxlo/2.0/     ! lower limit on valid SMX centroid
      data smxhi/58.0/    ! upper limit on valid SMX centroid
      data smylo/2.0/     ! lower limit on valid SMY centroid
      data smyhi/98.0/    ! upper limit on valid SMY centroid

*----
#include "fpd_towgain.inc"
      data escal/0.04/          ! north calorimeter energy scale (GeV/ADC cnt)
*  2-july-2002
*  Result from high-tower vs mass analysis of fill 2161 using
*  the same geometry parameters as used in reconstruction of simulations
*  This recalibration used SMDZ0=790 cm.
      data towgainN/
     $     1.029,0.738,0.673,0.522, ! north towers 1-4 rel gains (2nd iter., near posn)
     $     1.124,0.906,0.707,0.597, ! north towers 5-8 rel gains
     $     1.076,0.756,0.786,0.454/ ! north towers 9-12 rel gains
      data towgainF/
     $     0.965,0.814,0.644,0.543, ! north towers 1-4 rel gains
     $     1.124,0.912,0.674,0.579, ! north towers 5-8 rel gains
     $     1.036,0.759,0.824,0.433/ ! north towers 9-12 rel gains

      end                       ! end of block data fpdpar
***************************************************************
      subroutine fit_smd(ihist,ipass)
#include "fpd_ntdat.inc"
*
*  This routine performs a shower profile analysis
*
      parameter (pi=3.141592654) ! fixed parameter
      parameter (maxns=100)      ! fixed parameter
      parameter (fmxpk=6.0)      ! parameter specifying peak extent
      parameter (fmxr=0.01)      ! parameter specifying peak extent
      parameter (frng0=15.0)     ! extent of fitting range relative to centroid

      character*8 names(4)

      external fitfunc1

      real sigmax
      real outpar(10)
      common/fitsmd/sigmax,outpar,
     $     x0_1,x1_1,x2_1,
     $     x0_2,x1_2,x2_2

      data sigmax/3.5/
*************************************************
*
*
      real strip_de(100)        ! SMD profile vector
      real param(10)            ! fitting parameter array
      real step(10)             ! not used unless CHOPT='B'
      real pmin(10),pmax(10)    ! not used unless CHOPT='B'
      real sigpar(10)           ! std dev on fitted parameters

      common/hfpar/param
*
      common/quest/iquest(100)

* Determine summed energy deposition in strips
*
      call hunpak(ihist,strip_de,'HIST',0)
*
      detot=0
      do ns=1,maxns
         detot=detot+strip_de(ns)
      enddo
*
*      write(6,3000) numevt,ihist,detot
 3000 format('**fit_smd** Begin SMD analysis: ',i8,1x,i5,1x,f8.2)
*
      fmax=hmax(ihist)
      smd_cent=hstati(ihist,1,'HIST',0)
      smd_rms =hstati(ihist,2,'HIST',0)
*
      fmax=hmax(ihist)
*
* Initialize for fitting SMD plane
*
      do ns=1,maxns
         if(strip_de(ns).eq.fmax) then
            nsmax=ns
            goto 201
         endif
      enddo
 201  continue
      call hix(ihist,nsmax,smd_cent)
      nsmn=nsmax-5
      nsmx=nsmax+6
      nchn=0
      do ns=nsmn,nsmx
         if(strip_de(ns).ge.0.2*fmax) nchn=nchn+1
      enddo
      smd_rms=nchn/2.

      do n=1,10
         outpar(n)=0.0
      enddo
      fryld = 0.0
      cent2   = 0.0
      sigma2  = -1.0
      istat1  = -1
      xt=0
      do i=1,4
         param(i)=0.0
      enddo
      
*  Now, fit this histogram with a single peak...

      frng=frng0

      if(detot.gt.0.0) then    ! do the fits only if something's there
*
*  Initialize fitting parameters for one peak fitted to SMD profile

         param(2)=smd_cent      ! peak centroid:  initial guess, peak channel
         pmin(2)=smd_cent-15.0
         if(pmin(2).lt.0) pmin(2)=0
         pmax(2)=smd_cent+15.0
         if(pmax(2).gt.maxns) pmax(2)=maxns
         step(2)=0.5

         param(3)=smd_rms       ! peak width
         pmin(3)=0.25
         pmax(3)=sigmax
         step(3)=0.1

         param(4)=0.10          ! ratio of peak amplitudes
         pmin(4)=0.0
         pmax(4)=0.40
         step(4)=0.1

         param(5)=3.5           ! ratio of common Gaussian sigmas (fixed)

         param(1)=fmax*         ! initial guess for total yield
     $        param(3)*sqrt(2.*pi)
         pmin(1)=0.1*param(1)
         pmax(1)=1.5*param(1)
         step(1)=5.

 10      continue
         xx0=smd_cent-frng      ! set lower fit limit
         if(xx0.lt.0.5) xx0=0.5
         call hxi(ihist,xx0,ixxx)
         iquest(11)=ixxx
         xx1=smd_cent+frng      ! set upper fit limit
         if(xx1.gt.maxns) xx1=maxns
         call hxi(ihist,xx1,ixxx)
         iquest(12)=ixxx
         nb1=iquest(11)
         nb2=iquest(12)

*      write(6,4095) ihist,xx0,xx1
*      write(6,4100) param(1),param(2),param(3),param(4)
*      write(6,4101)  pmin(1), pmin(2), pmin(3), pmin(4)
*      write(6,4101)  pmax(1), pmax(2), pmax(3), pmax(4)
*      write(6,*) iquest(11),iquest(12)
         fmax=fitfunc1(smd_cent)
*      write(6,*) fmax
*
 4095    format(/,5x,'Fit histogram ',i4,' in range ',f8.1,'<x<',f8.1)
 4100    format(5x,'SMD plane, 1-peak fit initial params: ',4(f8.3,1x))
 4101    format(5x,'                                      ',4(f8.3,1x))
         call hfith(ihist,fitfunc1,'FBRWQ',
     $        4,param,step,pmin,pmax,sigpar,chi1)

         chisqt=0
         npts=0
         do nb=nb1,nb2
            data=hi(ihist,nb)
            if(data.gt.0) then
               call hix(ihist,nb,xxx)
               fit=fitfunc1(xxx)
               npts=npts+1
               chisqt=chisqt+(fit-data)**2
            endif
         enddo

            
*  Label fit parameters
         names(1)='Yield 1'
         names(2)='Cent 1'
         names(3)='[s]?0! 1'
         names(4)='Ratio 1'
         call hfinam(ihist,names,4)

         call mnstat(fmin,fedm,errdef,npari,nparx,istat1)

*      write(6,4000) istat1,chi1,chisqt/(npts-4)
 4000    format(5x,'ISTAT SMD plane, 1-peak fit: ',i3,1x,2(f10.2,1x))
*      write(6,4001) (param(i),i=1,10),(sigpar(i),i=1,10)
 4001    format('Params ',5(f12.5,1x),/
     $          '       ',5(f12.5,1x),/
     $          'Errors ',5(f12.5,1x),/
     $          '       ',5(f12.5,1x))
         if(istat1.gt.1) then
*
            if(ipass.eq.1) then
               cmx=param(2)
               call hxi(ihist,cmx,ix)
               call hix(ihist,ix,cmx)
               fmx=fitfunc1(cmx)
               dmx=strip_de(ix)
*     
               do ns=1,maxns
                  call hix(ihist,ns,x)
                  fm=fitfunc1(x)
                  dm=strip_de(ns)
                  if(abs(fm/fmx).gt.0.01) then
                     if(dm.gt.fm) then
                        strip_de(ns)=dm-fm
                     else
                        strip_de(ns)=0
                     endif
                     if(abs(x-cmx).lt.fmxpk) then
                        strip_de(ns)=0
                     endif
                  endif
               enddo
               npln=ihist/1000
               jhist=ihist + 100
               call hpak(jhist,strip_de)
            endif
*
            yield1=param(1)
            cent1=param(2)
            sigma1=param(3)
            ratio1=param(4)
*
*  Compute moments of fit residuals for 1<x<cent1-sig1 and cent1+sig1<x<86
*
            x0_1=0.0
            x1_1=0.0
            x2_1=0.0
            x0_2=0.0
            x1_2=0.0
            x2_2=0.0
            fmax=fitfunc1(cent1)
            do ns=1,maxns
               call hix(ihist,ns,xx)
               fval=fitfunc1(xx)
               if(fval.lt.0.15*fmax) then
                  weight=strip_de(ns)-fval
                  if(weight.gt.0.0) then
                     if(xx.lt.cent1) then
                        x0_1=x0_1+weight
                        x1_1=x1_1+xx*weight
                        x2_1=x2_1+xx*xx*weight
                     else
                        x0_2=x0_2+weight
                        x1_2=x1_2+xx*weight
                        x2_2=x2_2+xx*xx*weight
                     endif
                  endif
               endif
            enddo
            if(x0_2.gt.0.0 .or. x0_1.gt.0.0) then
               if(x0_2.gt.x0_1) then
                  yield2=x0_2
                  cent2=x1_2/x0_2
                  sigma2=sqrt(x2_2/yield2 - cent2*cent2)
               else
                  yield2=x0_1
                  cent2=x1_1/x0_1
                  sigma2=sqrt(x2_1/yield2 - cent2*cent2)
               endif

               fryld=yield2
               xt=fryld

*      write(6,5000) yield1,cent1,sigma1,wgh,x1l/wgh, 
*      $              yield2,cent2,sigma2
 5000          format(
     $              /,5x,'SMD, single peak: ',f8.1,1x,2(f8.3,1x),
     $              /,5x,'distribution mom:   ',f8.1,1x,f8.3,
     $              /,5x,'fit resid mom:      ',f8.1,1x,2(f8.3,1x))

            else
               yield2=0.0
            endif
         elseif(frng.gt.5.0) then
            frng=frng-1.0
            goto 10             ! try again
         endif                  ! if(istat1.eq.3) ...
      endif                     ! if(detot...
*
*  Fill outpar array with results...
*
      outpar( 1) = param(1)
      outpar( 2) = param(2)
      outpar( 3) = param(3)
      outpar( 4) = param(4)
      outpar( 5) = qfit(ihist,nb1,nb2,chi1,4)
      outpar( 6) = fryld
      if(x0_1+x0_2.gt.0.0) then
         outpar( 7) = (x1_1+x1_2)/(x0_1+x0_2)
      else
         outpar( 7) = 0.0
      endif
      outpar( 8) = sigma2
      outpar( 9) = istat1
      outpar(10) = detot
      
      return
      end


******************************************************************
      function fitfunc1(x)
      common/hfpar/param(10)
      real scale(2)
      real sigma(2)
*
      parameter (pi=3.141592654)
*
      fitfunc1=0.0
*
      yld=param(1)
      cent=param(2)
      sigma(1)=param(3)
      sigma(2)=param(3)*param(5)
      scale(1)=(1.0-param(4))/sigma(1)/sqrt(2.*pi)
      scale(2)=(    param(4))/sigma(2)/sqrt(2.*pi)
      do i=1,2
         fitfunc1=fitfunc1+yld*scale(i)*exp(-0.5*((x-cent)/sigma(i))**2)
      enddo
      return
      end


***************************************************************
      function qfit(ihist,nb1,nb2,chi1,npar)

      integer npts              ! number of points used in MINUIT fit
      real totcnts              ! integral energy deposition in fitting region
      real cnts                 ! energy deposition in strip
      real hi                   ! HBOOK functioni
      integer nb                ! strip
      real chi2tot              ! total chi^2, computed from red. chi^2
      real qfit                 ! fit quality

*  Compute quality factor for fit defined as the ratio of the
*  summed deviation magitude to the energy deposition in the fitted region.
*  This quantity can be derived from the reduced chi^2 returned from MINUIT
*  when unit weights are used for the fit.

      npts=0
      totcnts=0
      do nb=nb1,nb2
         cnts=hi(ihist,nb)
         if(cnts.gt.0) then
            npts=npts+1
            totcnts=totcnts+cnts
         endif
      enddo

      chi2tot=float(npts-npar)*chi1
      if(npts-4.gt.0) then
         qfit=sqrt(chi2tot)/totcnts
      else
         qfit=-1
      endif
*      write(6,1000) ihist,nb1,nb2,chi1,chi2tot,totcnts,qfit
 1000 format('Fit quality : ',i4,1x,2(i3,1x),3(f10.2,1x),f8.4)
      return
      end

*
***************************************************************
      subroutine fit_smd2(ihist,ipass,npln)
*
*  This routine performs a shower profile analysis
*
#include "fpd_ntdat.inc"
#include "fpd_pi0dat.inc"
      external fitfunc2
*
      parameter (maxns=100)
      real sigmax
      real outpar(10)
      common/fitsmd/sigmax,outpar,
     $     x0_1,x1_1,x2_1,
     $     x0_2,x1_2,x2_2
*************************************************
*
      real strip_de(100)        ! SMD profile vector
      real param(10)            ! fitting parameter array
      real step(10)             ! not used unless CHOPT='B'
      real pmin(10),pmax(10)    ! not used unless CHOPT='B'
      real sigpar(10)           ! std dev on fitted parameters

      character*8 names(8)

      common/hfpar/param
*
      common/quest/iquest(100)

*      write(6,*) '**fit_smd2** IHIST,IPASS,NPLN = ',ihist,ipass,npln
*
* Determine summed energy deposition in strips
*
      call hunpak(ihist,strip_de,'HIST',0)
*
      do i=1,8
         param(i)=0.0
      enddo
*      
      detot=0
      do ns=1,maxns
         detot=detot+strip_de(ns)
      enddo
*
*      write(6,3000) numevt,ihist,detot
 3000 format('**fit_smd2** Begin SMD analysis #2: ',i8,1x,i5,1x,f8.2)
*
*  Now, fit this histogram with a single peak...

      if(detot.gt.0.0) then    ! do the fits only if something's there
*
*  Initialize fitting parameters for two peaks to SMD profile

         do n=1,2
            do i=1,4
               j=(n-1)*4+i
               param(j)=savpar(npln,n,i)
            enddo
         enddo
         param( 9)=3.5
         param(10)=3.5
*
         pmin(1)=0.5*param(1)   ! set lower fit limit on peak-1 yield
         pmax(1)=1.5*param(1)   ! set upper fit limit on peak-1-yield
         step(1)=5.0
*
         pmin(2)=param(2)-5.0  ! set lower fit limit on peak-1 centroid
         pmax(2)=param(2)+5.0  ! set upper fit limit on peak-1 centroid
         step(2)=0.5
*
         if(param(2).lt.param(6)) then
            xxx=param(2)-12.0   ! set lower fit limit based on peak centroid
            call hxi(ihist,xxx,ixxx)
            iquest(11)=ixxx
            xxx=param(6)+12.0
            call hxi(ihist,xxx,ixxx)
            iquest(12)=ixxx
         else
            xxx=param(2)+12.0   ! set upper fit limit based on peak centroid
            call hxi(ihist,xxx,ixxx)
            iquest(12)=ixxx
            xxx=param(6)-12.0
            call hxi(ihist,xxx,ixxx)
            iquest(11)=ixxx
         endif
         nb1=iquest(11)
         nb2=iquest(12)
*
         pmin(3)=0.2            ! set lower limit on peak-1 width
         pmax(3)=sigmax
         step(3)=0.1
*
         pmin(4)=0.0
         pmax(4)=0.80
         step(4)=0.1
*
         pmin(5)=0.5*param(5)   ! set lower fit limit on peak-2 yield
         pmax(5)=1.5*param(5)   ! set upper fit limit on peak-2 yield
         step(5)=5.0
*
         pmin(6)=param(6)-5.0  ! set lower fit limit on peak-2 centroid
         pmax(6)=param(6)+5.0  ! set upper fit limit on peak-2 centroid
         step(6)=0.5
*
         pmin(7)=0.2            ! set lower limit on peak-1 width
         pmax(7)=sigmax
         step(7)=0.1
*
         pmin(8)=0.0
         pmax(8)=0.80
         step(8)=0.1
*
         call hfith(ihist,fitfunc2,'FBRWQ',
     $        8,param,step,pmin,pmax,sigpar,chi1)

         chisqt=0
         npts=0
         do nb=nb1,nb2
            data=hi(ihist,nb)
            if(data.gt.0) then
               call hix(ihist,nb,xxx)
               fit=fitfunc1(xxx)
               npts=npts+1
               chisqt=chisqt+(fit-data)**2
            endif
         enddo

*  Label fit parameters
         names(1)='Yield 1'
         names(2)='Cent 1'
         names(3)='[s]?0! 1'
         names(4)='Ratio 1'
         names(5)='Yield 2'
         names(6)='Cent 2'
         names(7)='[s]?0! 2'
         names(8)='Ratio 2'
         call hfinam(ihist,names,8)

         call mnstat(fmin,fedm,errdef,npari,nparx,istat1)

*      write(6,4000) istat1,chi1,chisqt/(npts-4)
 4000    format(5x,'ISTAT SMD plane, 2-peak fit: ',i3,1x,2(f10.2,1x))
*
*  Fill outpar array with results...
*
      endif
*
      do n=1,8
         outpar(n) = param(n)
      enddo
      outpar( 9) = qfit(ihist,nb1,nb2,chi1,8)
      outpar(10) = istat1
*
      return
      end

******************************************************************
      function fitfunc2(x)
      common/hfpar/param(10)
      real scale(2)
      real sigma(2)
*
      parameter (pi=3.141592654)
*
      fitfunc2=0.0
*
      yld=param(1)
      cent=param(2)
      sigma(1)=param(3)
      sigma(2)=param(3)*param(9)
      scale(1)=(1.0-param(4))/sigma(1)/sqrt(2.*pi)
      scale(2)=(    param(4))/sigma(2)/sqrt(2.*pi)
      do i=1,2
         fitfunc2=fitfunc2+yld*scale(i)*exp(-0.5*((x-cent)/sigma(i))**2)
      enddo
*
      yld=param(5)
      cent=param(6)
      sigma(1)=param(7)
      sigma(2)=param(7)*param(10)
      scale(1)=(1.0-param(8))/sigma(1)/sqrt(2.*pi)
      scale(2)=(    param(8))/sigma(2)/sqrt(2.*pi)
      do i=1,2
         fitfunc2=fitfunc2+yld*scale(i)*exp(-0.5*((x-cent)/sigma(i))**2)
      enddo
      return
      end

***************************************************************
      subroutine parsav(npln,npass)
*
*  Save single-peak fit results to savpar(npln,npass,m)
*  defined in fpd_pi0dat.inc
*     npln = 1,2 corresponds to the two SMD planes
*
*     npass = 1,2 corresponds to the pass # for single-peak fits to SMD plane
*                 The first pass fits all data.  The second pass fits
*                 residuals from the first pass.
*
*      m = 1   fitted peak integral
*          2   fitted peak centroid
*          3   fitted peak sigma (narrow Gaussian)
*          4   fitted narrow/wide Gaussian amplitude ratio
*          5   chi^2/dof
*          6   sided fit residual yield
*          7   fit residual centroid
*          8   sided fit residual RMS
*          9   ISTAT code from MINUIT
*          10  total energy deposited in SMD plane
*
#include "fpd_pi0dat.inc"

      real sigmax
      real outpar(10)
      common/fitsmd/sigmax,outpar,
     $     x0_1,x1_1,x2_1,
     $     x0_2,x1_2,x2_2
*
      do n=1,10
         savpar(npln,npass,n)=outpar(n)
      enddo
      return
      end

***************************************************************
      subroutine parsav2(np)
#include "fpd_pi0dat.inc"
      real sigmax
      real outpar(10)
      common/fitsmd/sigmax,outpar,
     $     x0_1,x1_1,x2_1,
     $     x0_2,x1_2,x2_2
*
      do n=1,10
         savpar2(np,n)=outpar(n)
      enddo
      return
      end


***************************************************************
      function fwhm(npln,npk)
#include "fpd_pi0dat.inc"
#include "fpd_ntdat.inc"

      common/hfpar/param(10)

      iter=0
      nset=(npk-1)*4
      if(savpar2(npln,nset+3).eq.0.0) then
         fwhm=0.0
         return
      endif
      do npar=1,4
         ipar=nset+npar
         param(npar)=savpar2(npln,ipar)
      enddo
*
      xmax=savpar2(npln,nset+2)
      fmax=fitfunc1(xmax)
      fhmax=0.5*fmax
      xhmax=xmax+5*savpar2(npln,nset+3)
      chmax=fitfunc1(xhmax)
*
      do while (
     $     (abs(fhmax-chmax).gt.0.01*fhmax).and.
     $     (iter.lt.itmax))
         iter=iter+1
         if(fhmax.gt.chmax) then
            xhmax=xhmax-0.5*abs(xhmax-xmax)
         else
            xhmax=xhmax+0.5*abs(xhmax-xmax)
         endif
         chmax=fitfunc1(xhmax)
      enddo
      if(iter.lt.itmax) then
         fwhm=2*(xhmax-xmax)
      else
         fwhm=0.0
      endif
*
*      write(6,*) '**fwhm** event number ',numevt,iter
*      
      return
      end
      
***************************************************************
      subroutine book
*
*  Books histograms for neutral pion event reconstruction.
*
#include "fpd_ntdat.inc"
#include "fpd_smdhst.inc"

      irun=runnum
*      write(cpirun,'(i4.4)') irun
      write(6,*) '**book**Create histograms for pi0 recon, run ',irun
      ihevt=0
      do ih=1,ihmx0
         ihist=ismx0+ih
*      write(6,*) '**book** histogram ',ihist
         call hbook1(ihist,'SMX',100,0.5,100.5,0.0)
         ihist=ismx1+ih
*      write(6,*) '**book** histogram ',ihist
         call hbook1(ihist,'SMX',100,0.5,100.5,0.0)
         ihist=ismy0+ih
*      write(6,*) '**book** histogram ',ihist
         call hbook1(ihist,'SMY',100,0.5,100.5,0.0)
         ihist=ismy1+ih
*      write(6,*) '**book** histogram ',ihist
         call hbook1(ihist,'SMY',100,0.5,100.5,0.0)
         ihist=itow0+ih
*      write(6,*) '**book** histogram ',ihist
         call hbook2(ihist,'Tower profile',4,0.5,4.5,3,0.5,3.5,0.0)
         ihist=ipre1+ih
*      write(6,*) '**book** histogram ',ihist
         call hbook2(ihist,'Preshower 1',4,0.5,4.5,3,0.5,3.5,0.0)
         ihist=ipre2+ih
*      write(6,*) '**book** histogram ',ihist
         call hbook2(ihist,'Preshower 2',4,0.5,4.5,3,0.5,3.5,0.0)
      enddo

*  histograms for transformed event information -- cumulative

      call hbook1( 101,'SMX primary cluster centroid',500,0.0,100.0,0.0)
      call hbook2( 102,'SMX primary cluster centroid v HT row',
     $     500,0.0,100.0,3,0.5,3.5,0.0)
      call hbook2( 103,'SMX primary cluster sum v HT energy',
     $     100,0.0,10000.0,100,0.0,50.0,0.0)
      call hbook1( 104,'SMX primary cluster FWHM',100,0.0,5.0,0.0)
      call hbook1( 111,'SMY primary cluster centroid',500,0.0,100.0,0.0)
      call hbook2( 112,'SMY primary cluster centroid v HT column',
     $     500,0.0,100.0,4,0.5,4.5,0.0)
      call hbook2( 113,'SMY primary cluster sum v HT energy',
     $     100,0.0,10000.0,100,0.0,50.0,0.0)
      call hbook1( 114,'SMY primary cluster FWHM',100,0.0,5.0,0.0)
      call hbook1( 121,'SMX second  cluster centroid',500,0.0,100.0,0.0)
      call hbook1( 124,'SMX second  cluster FWHM',100,0.0,5.0,0.0)
      call hbook1( 125,'SMX fit quality',100,0.0,0.5,0.0)
      call hbook1( 131,'SMY second  cluster centroid',500,0.0,100.0,0.0)
      call hbook1( 134,'SMY second  cluster FWHM',100,0.0,5.0,0.0)
      call hbook1( 135,'SMY fit quality',100,0.0,0.5,0.0)
      call hbook1( 141,'Invariant mass (GeV/c^2!)',350,0.0,0.7,0.0)
      call hbook2( 142,'Invariant mass (GeV/c^2!) v BBC time diff',
     $     350,0.0,0.70,150,-150.0,150.0,0.0)
      call hbook2( 143,'Invariant mass (GeV/c^2!) v tower-sum energy',
     $     350,0.0,0.70,100,10.0,60.0,0.0)
      call hbook2( 144,'Invariant mass (GeV/c^2!) v HT id',
     $     350,0.0,0.70,12,0.5,12.5,0.0)
      call hbook2( 145,'Invariant mass (GeV/c^2!) v z[g]',
     $     350,0.0,0.70,100,0.0,1.0,0.0)
      call hbook2( 146,'Invariant mass (GeV/c^2!) v SMX cl1 cent',
     $     140,0.0,0.70,100,0.0,100.0,0.0)
      call hbook2( 147,'Invariant mass (GeV/c^2!) v SMX cl2 cent',
     $     140,0.0,0.70,100,0.0,100.0,0.0)
      call hbook2( 148,'Invariant mass (GeV/c^2!) v SMY cl1 cent',
     $     140,0.0,0.70,100,0.0,100.0,0.0)
      call hbook2( 149,'Invariant mass (GeV/c^2!) v SMY cl2 cent',
     $     140,0.0,0.70,100,0.0,100.0,0.0)
      call hbook2( 150,'Invariant mass (GeV/c^2!) v SMX cent diff',
     $     350,0.0,0.70,60,-30.0,30.0,0.0)
      call hbook2( 151,'Invariant mass (GeV/c^2!) v SMY cent diff',
     $     350,0.0,0.70,60,-30.0,30.0,0.0)
      call hbook2( 152,'Invariant mass (GeV/c^2!) v SMD distance',
     $     350,0.0,0.70,60,0.0,60.0,0.0)
      call hbook2( 153,'Inv mass (GeV/c^2!) v tower-sum energy (up)',
     $     350,0.0,0.70,100,10.0,60.0,0.0)
      call hbook2( 154,'Inv mass (GeV/c^2!) v tower-sum energy (dn)',
     $     350,0.0,0.70,100,10.0,60.0,0.0)
      call hbook2( 155,'Inv mass (GeV/c^2!) v tower-sum energy (oth)',
     $     350,0.0,0.70,100,10.0,60.0,0.0)

      return
      end

