* $Id: tde.F,v 1.6 1999/12/28 20:10:08 ogilvie Exp $
* $Log: tde.F,v $
* Revision 1.6  1999/12/28 20:10:08  ogilvie
* adde scaled dedx, changed to error on mean
*
* Revision 1.5  1999/12/17 16:35:26  sakrejda
* Introduced additional integer to avit having mod(short,int).
* It was not harmful, but confusing and debuggers complained about it.
*
* Revision 1.4  1999/10/15 00:54:06  nevski
* pgf->g77 cleanup
*
* Revision 1.3  1999/10/01 16:35:54  margetis
* updated tid, put method instead of iflag
*
* Revision 1.2  1999/08/18 20:46:39  fisyak
* replace ran by rmdn
*
* Revision 1.1  1999/08/12 16:35:02  ogilvie
* storing results in tpc_dedx table, changing default to tde.F, will freeze tde_new.F
*
* Revision 1.17  1999/07/19 19:15:51  genevb
* Use StMessMgr
*
* Revision 1.16  1999/06/09 14:28:57  ogilvie
* *** empty log message ***
*
* Revision 1.15  1999/06/01 17:55:57  ogilvie
* fixed bug in sign of track rotation, now includes sign of bz
* comments added throughout for path-length across a pad-row for each hit
*
* Revision 1.14  1999/05/27 15:28:14  ogilvie
* removed extra write statements
*
* Revision 1.13  1999/05/27 13:54:46  ogilvie
* added new parameter to control the method used to calculate averages
* called "averaging_method"
*
* Revision 1.12  1999/04/25 04:25:08  sakrejda
* protected against accidental runs with no field
*
* Revision 1.11  1999/04/13 13:49:55  ogilvie
* Shortened several lines to less than 72 characters
*
* Revision 1.10  1999/02/19 21:14:37  ogilvie
* added new method 3 = use weights to calc de/dx
*
* Revision 1.9  1999/01/11 22:35:37  sakrejda
* increased local arrays to accomodate a Vini event
*
* Revision 1.8  1998/12/13 21:34:26  sakrejda
* tpt_const.inc replaced by math_constants.inc
*
* Revision 1.7  1998/12/09 00:53:19  sakrejda
* increased local array size to accomodate Venus events
*
* Revision 1.6  1998/10/10 00:53:18  sakrejda
* take care of tracks with decreasing radius, do not discard them
*
* Revision 1.5  1998/08/05 21:58:30  fisyak
* Correct parameters in hdelet for NT (VF)
*
* Revision 1.4  1998/07/07 17:21:48  ogilvie
* removing hardwired B-field and math const
*
* Revision 1.3  1998/06/26 13:42:42  ogilvie
* adding landau fit option
*
* Revision 1.2  1998/01/27 01:54:47  fisyak
* Split sources
*
*23456789012345678901234567890123456789012345678901234567890123456789012
      INTEGER FUNCTION tde(tdpar_h,tdpar,hit_h,hit,
     >                         track_h,track,pad_h,pad,
     >                         tpcdedx_h, tpcdedx)

      IMPLICIT NONE
C
C
C Input arguements
C      hit_h   = header for the tphit table
C      hit     = rows of the tphit table
C      track_h = header for the tptrack table (reconstructed tracks)
C      track   = rows of the tptrack table
C      pad_h   = header for the pad_plane table
C      pad     = rows of the pad_plane table
C      tdpar_h = header for the tdeparm (parameters) table
C      tdpar   = rows of the tdeparm table
C
C Output arguements:
C      ndedx = number of points used in dE/dx for each reconstructed
C              track in the tptrack table
C      dedx  = dE/dx for each reconstructed track in the tptrack table
C
C Functional description:
C      A module that calculate dE/dx from reconstructed tracking data
C
C-----------------------------------------------------------------------

#include "tde.inc"
      STRUCTURE /tde_local/
                REAL length
                REAL charge
                REAL adedx
                REAL sorta_dedx
                INTEGER row
      END STRUCTURE

C-----------------------------------------------------------------------
#include "phys_constants.inc"
#include "math_constants.inc"

      real xlocal(3), bfield(3)
      data xlocal /3*0/

      INTEGER tls_Index_Sort_i,tls_Quick_Sort_r      
      INTEGER iret
      INTEGER i,l,itrack,j,irow,isect,tgc_row_par,cros,k,last
      INTEGER imsg1, imsg2, imsg3,iret1,iret2,flag,nde,ndemax
      INTEGER nlocmax,nloctrc,nde_orig,nde_trunc
      REAL    x1,y1,xc,yc,radius,psic,cosl
      REAL    xa,xb,ya,yb,a,b,bi,bo,ai,ao,ds,half_pad
      REAL    rowtmp
      real weight,sum_weight,sumsq,mean
      REAL fndedx,fract,frndm
      real z(10),erf(10),frachalf(10),zcut,nontrunc,slope
      real average,sigma,dedx_upper,rndm,mean_scaled,sigma_scaled
      INTEGER iseed, nde_old, ipos,nzero,kk

      real ascale, bscale, scale, meanscale, arms, brms, rms 

      INTEGER longrow

C
c     variables for maximumum likeihood
c
      REAL hxmin,hxmax,hxamp
      INTEGER hhid, hnbin
      REAL hparam(3),hstep(3),hpmin(3),hpmax(3),hsig(3),hchi2

      CHARACTER*132 m132
c
      PARAMETER (ndemax=1000)
      PARAMETER (nlocmax=500000)
      PARAMETER  (nloctrc=30000)
c
      INTEGER loc_hit(nlocmax),loc_track(nloctrc)

      RECORD  /tde_local/ dedx(ndemax)
c      DATA    bconv /0.00299792458/
c      DATA    dtorad /0.0174532925/
c      DATA    cpi /3.14159265/
      DATA    imsg1 /0/, imsg2 /0/, imsg3 /0/
      DATA iseed /1000001/
      data z / 0.15,0.2,0.25,0.3,0.35,0.4,0.45,0.5,0.55,0.6/
c            /0.75,0.8,0.85,0.9,0.95,1.0,1.05,1.15,1.20,1.25/
      data erf /0.168,0.223,0.276,0.329,0.379,0.428,0.475,0.521,0.563,0.604/
c            /0.7116,0.7420,0.7707,0.7969,0.8209,0.8427,0.8624,0.8802,0.8961,.9103/

      EXTERNAL landau2
C-----------------------------------------------------------------------


*      call message('TDE_DEDX-I: Execution started',1,imsg1)


C Abort if there are no TPC hits to work with

      if (hit_h.nok.eq.0) then
        call message('TDE_DEDX-W: No TPC hitss.'// 
     >                 ' TDE aborted.',1,imsg2)
         tde=STAFCV_BAD
         return
      endif
C Abort if there are too many TPC hits to work with

      if (hit_h.nok.gt.nlocmax) then
        call message('TDE_DEDX-W: Too many TPC hits.'// 
     >                 'TDE aborted.',1,imsg2)
         tde=STAFCV_BAD
         return
      endif

C Abort if there are no TPC tracks to work with

      if (track_h.nok.eq.0) then
        call message('TDE_DEDX-W: No TPC tracks.'//
     >                 'TDE aborted.',1,imsg3)
         tde=STAFCV_BAD
         return
      endif
C Abort if there are too many TPC tracks to work with

      if (track_h.nok.gt.nloctrc) then
        call message('TDE_DEDX-W: too many TPC tracks.'// 
     >                 'TDE aborted.',1,imsg3)
         tde=STAFCV_BAD
         return
      endif

      call gufld(xlocal,bfield)
      if(abs(bfield(3)).lt.0.00001) return 
      
c
c  this only once on initialisation-
c field is uniform enough in the TPC)

c
c
c as a test for the likelihood method
c book a histigram, fill it, fit, then delete
c if this works, find a more efficient way later
c C. Ogilvie June 1998
c
c to run this option use
c      tdpar(1).truncopt=4
c      tdpar(1).truncfact=0.3
c       tdpar(1).truncopt = 5

c
c setup for option 5 likelihood fit
c
       if (tdpar(1).truncopt.eq.5) then
        nontrunc = 2.*( (1 - tdpar(1).truncfact) - 0.5)
        do i=1, 10
          frachalf(i)=1-erf(i)
          write(m132,*) 'TDE - ',erf(i),frachalf(i),z(i)
          call StInfo(m132)
        enddo
        ipos = 1
        do while ((erf(ipos).lt.nontrunc).and.ipos.lt.10)
           ipos=ipos+1
        enddo
        if (ipos.lt.10.and.ipos.gt.1) then
           slope = (z(ipos)-z(ipos-1))/(erf(ipos)-erf(ipos-1))
           zcut  = z(ipos) + (nontrunc-erf(ipos))*slope
        else
           zcut = z(ipos)
        endif
       endif
c
c end of setup
c     
      write(m132,*)  'TDE - averaging, truncation methods: ',
     >     tdpar(1).averaging_method, tdpar(1).truncopt   
      call StInfo(m132)        
      if (tdpar(1).usemerge.ne.0) then
           call StInfo('TDE - using merged hits!!!')
      endif
         
C Sort the hit array according to the track number
      
      iret = tls_Index_Sort_i(hit_h.nok,hit(1).track,
     >       hit(2).track,loc_hit,
     >       hit_h.maxlen)

C Establish pointers to tracks

      do i=1,track_h.nok
         loc_track(track(i).id)=i
      end do
C       and do some more initialisation for
       do i = 1,track_h.nok
           track(i).dedx(1) = 0.0
           track(i).dedx(2) = 0.0
           track(i).ndedx   = 0
      end do

C Loop over tracks and calculate average de/dx for each point
      nzero = 0
      l=hit_h.nok
      last = 0
c
c last is a flag introduced by CO, it is set to 1, when
c the loop exhausts the number of hits, i.e. l goes to 0.
c It is needed because a simple test of l=0 in the
c wile statement would cause a crash due to loc_hit(0) in
c the same statement. Ugly solution
c
      itrack=hit(loc_hit(l)).track/1000
      i=0
      do while(hit(loc_hit(l)).track.gt.0.
     >   and.i.lt.track_h.nok.and.last.eq.0)
         i=i+1
         itrack=hit(loc_hit(l)).track/1000
         j=loc_track(itrack)
C        calculate center and radius from the track parameters
         x1 = track(j).r0 * cos(track(j).phi0*C_RAD_PER_DEG)
         y1 = track(j).r0 * sin(track(j).phi0*C_RAD_PER_DEG)
c
c        x1,y1 are the cordinates of the track's start
c        raidus is the radius of the transverse circle
c        psic is the azimuthal angle of the start of the track
c        wrt to the center of the transverse circle
c        xc, yc are the coordinates of center of the circle
c
         radius=1.0 / (track(j).invp*bfield(3)*C_D_CURVATURE)
         psic=track(j).psi*C_RAD_PER_DEG+
     >      sign(1.,track(j).q*bfield(3))*C_PI*0.5
         xc = x1 - radius*cos(psic)
         yc = y1 - radius*sin(psic)
         cosl= sqrt(1.0+track(j).tanl**2)
C        loop over all the hits on this track to calculate the track length
C        and average charge
************************************************************************
*   Calculate dE/dx for each track & output to tptrack table  (TdEdx)  *
************************************************************************
C
C        tdpar.truncopt=1   use all dedx points
C        tdpar.truncopt=2   tries to smooth out integerization
c                       e.g if nhit = 11, truncfac = 0.3, 
c                              fndedx = 11 - 3.3 = 7.7
c                              mod(7.7,1)=0.7
c                              nde = 7 + 1 (if rndm.lt.0.7)
c        tdpar.trucnopt=3   truncate nint(truncfrac%) of points with larger dE/dx
C        tdpar.truncopt=4   truncate int(truncfrac%) of points with larger dE/dx
C
C        tdpar.averaging_method=0   calculate dE/dx=(sum of q)/(sum of s)
C
C        tdpar.averaging_method=1   calculate dE/dx=(sum of q/s)/(# of pnts)
c     
c        tdpar.averaging_method=3   calculate dE/dx as maximum likelihood
C
c        tdpar.averaging_method=4   dE/dx=(sum of weight*q/s)/(# of weight)
c                           weight = sqrt(s)
c
C        tdpar.usemerge=0   skipp point flagged as merged 
c        tdpar.usemerge.ne.0 use merged hit
C
C        if row less than tdpar.minrow skipp the point
C        options based on Jeff Mitchell's  

         nde=0
         do while (int(hit(loc_hit(l)).track/1000).
     >      eq.itrack.and.last.eq.0)
            longrow=hit(loc_hit(l)).row
            irow=mod(longrow,100)
            if(irow.gt.tdpar(1).minrow) then
               isect=hit(loc_hit(l)).row/100
c              get the parametrisation for the center of a row
c              (ax + by = 1)
               rowtmp=irow
               iret  = tgc_row_par(isect,rowtmp,a,b,pad_h,pad) 
c              calculate the parametrisation for the inner and the outer
c              boundary of a row
               half_pad = pad(1).pad_len_in*0.5
               if(irow.gt.13)half_pad = pad(1).pad_len_out*0.5
c              check whether both a and b are >0
               if(abs(a*b).gt. 0.00000001)then
c                 so first the inner part
                  ai=a/(1.0+half_pad*sqrt(a**2+b**2))
                  bi=b/(1.0+half_pad*sqrt(a**2+b**2))
c                 and the outer part
                  ao=a/(1.0-half_pad*sqrt(a**2+b**2))
                  bo=b/(1.0-half_pad*sqrt(a**2+b**2))
               else if (abs(a).lt.0.0000001) then
                  ai=0.0
                  bi=1.0/(1.0/b-half_pad)
                  ao=0.0
                  bo=1.0/(1.0/b+half_pad)
               else if (abs(b).lt.0.0000001) then
                  ai=1.0/(1.0/a-half_pad)
                  bi=0.0
                  ao=1.0/(1.0/a+half_pad)
                  bo=0.0
               endif
c
               flag  = 0
               iret1 = cros(flag,xc,yc,radius,ai,bi,hit
     >                (loc_hit(l)).x,
     >                 hit(loc_hit(l)).y,xa,ya)
               iret2 = cros(flag,xc,yc,radius,ao,bo,hit
     >                (loc_hit(l)).x,
     >                 hit(loc_hit(l)).y,xb,yb)
c
c  (xa,ya) (xb,yb) are the coordinates of the pad-row where the transverse
c  circle enters and leaves the pad-raow
c
               if(iret1.lt.0.and.iret2.eq.0) then
                  flag = 1
                  iret1= cros(flag,xc,yc,radius,ao,bo,hit
     >                  (loc_hit(l)).x,
     >                   hit(loc_hit(l)).y,xa,ya)
               else if(iret2.lt.0.and.iret1.eq.0) then
                  flag = 1
                  iret2= cros(flag,xc,yc,radius,ai,bi,hit
     >                  (loc_hit(l)).x,
     >                   hit(loc_hit(l)).y,xb,yb)
               endif
c
c To calculate the arc-length across the pad-row first
c the angle subtended by the entrance (xa,ya) and exit points (xb,yb)
c from the center of the circle is calculated. 
c The arc-length across the pad is is calculated from half of this
c angle and the radius of the transverse circle. The path-length of the track
c then includes the dip angle factor, cosl
c               
               if(iret1.eq.0.and.iret2.eq.0) then
                  ds=radius*2.0*
     >            asin(0.5*sqrt((xa-xb)**2+(ya-yb)**2)
     >            /radius)*cosl
                  hit(loc_hit(l)).dq=ds
c
c check on quality of hit before including it into dedx
c
c             if (hit(loc_hit(l)).flag.ne.0) then
c                write(m132,*)'flag',hit(loc_hit(l)).flag ,ds
c                call StInfo(m132)
c              endif   
            if (((hit(loc_hit(l)).flag.eq.0).or.(tdpar(1).usemerge.ne.0))
     >       .and.nde.lt.ndemax) then
               if (hit(loc_hit(l)).flag.ne.0) then
                 write(m132,*)'flag',hit(loc_hit(l)).flag ,ds
                 call StInfo(m132)
               endif  
                    nde=nde+1
                    dedx(nde).length=ds
                    dedx(nde).charge=hit(loc_hit(l)).q
                    dedx(nde).adedx=dedx(nde).charge/ds
                    dedx(nde).row=irow
                    if (tdpar(1).averaging_method.eq.4) then
c
c first scale the dedx by path-length dependence
c    dedx = a(1-b/dx) 
c     
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
                       weight = 1./rms
c                      weight = sqrt(ds)                       
                       dedx(nde).sorta_dedx = dedx(nde).adedx*weight/scale
                    else
                      dedx(nde).sorta_dedx = dedx(nde).adedx
                    endif  
                  endif
c               else if((xc-hit(loc_hit(l)).x)**2+
c     >            (yc-hit(loc_hit(l)).y)**2
c     >            .gt.radius**2) then
c  include the flag cuts for the merged tracks. 
c  Because after merged, some hits from other recid can't 
c  fit this circle parameter, so cause the track.flag=-1 or -999  
c  liqun 10/08/98
               else if((xc-hit(loc_hit(l)).x)**2+
     >            (yc-hit(loc_hit(l)).y)**2
     >            .gt.radius**2.and.track(j).flag.ne.100) then

                  hit(loc_hit(l)).dq=-1.0
                  track(j).flag=-1
               else if(track(j).flag.ne.100) then
                  hit(loc_hit(l)).dq=-999.0
                  track(j).flag=-999
               endif
            endif
            l=l-1
c
c  CO flag to aviod l=0 crash in top while statement
c
	    if (l.eq.0) then
		l=1
	        last=1
            else
		last=0
            endif     
         end do
         iret=tls_Quick_Sort_r(nde,dedx(1).sorta_dedx,
     >        dedx(2).sorta_dedx,dedx)
c
c truncating options
c
         nde_orig=nde
         if (tdpar(1).truncopt.eq.5) then
            average = 0.
            do k=1,nde_orig
               average = average + dedx(k).adedx
            enddo
            average = average /real(nde_orig)
            sigma = 0.6*average /sqrt(real(nde_orig))
            dedx_upper = average + zcut*sigma
            nde = 0
            do k=1,nde_orig
               if (dedx(k).adedx.lt.dedx_upper) then
                  nde=nde+1
               endif
            enddo
         elseif (tdpar(1).truncopt.eq.4) then
            nde=nde_orig-int(tdpar(1).truncfact*real(nde_orig))
c
c removes fewest number of points
c
         elseif (tdpar(1).truncopt.eq.3) then
c 
c may be the logical choice
c
            nde=nde_orig-nint(tdpar(1).truncfact*real(nde_orig))

         elseif (tdpar(1).truncopt.eq.2) then 
c tries to smooth out nde
            fndedx = real(nde_orig) - tdpar(1).truncfact*real(nde_orig)
            fract = mod(fndedx,1.)
            frndm = rndm(iseed)
            if (frndm.lt.fract) then
              nde = int(fndedx)+1
            else
              nde = int(fndedx)
            endif
         else
c no truncation
            nde=nde_orig
         endif   
         if(nde.lt.0) then
            nde=0
            nzero=nzero+1
         endif
 
         track(j).ndedx=nde
         track(j).dedx(1)=0.
         track(j).dedx(2)=0.
c         if (track(j).flag .lt. 0) then
c            write(6,*) track(j).flag, nde
c            pause
c         endif   
c
c options for dE/dx calc
c
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
c
c
c fill tpcdedx table
         if (tdpar(1).averaging_method.eq.1.or.
     >           tdpar(1).averaging_method.eq.4) then
            tpcdedx_h.nok = tpcdedx_h.nok+1
            kk=tpcdedx_h.nok
            tpcdedx(kk).ndedx = nde
            tpcdedx(kk).dedx(1) = track(j).dedx(1)
            tpcdedx(kk).dedx(2) = track(j).dedx(2)
            tpcdedx(kk).id_track = track(j).id
            tpcdedx(kk).method = 1  
c
c write out scaled as well
c
             tpcdedx_h.nok = tpcdedx_h.nok+1
             kk=tpcdedx_h.nok
             tpcdedx(kk).ndedx = nde
             tpcdedx(kk).dedx(1) = mean_scaled
             tpcdedx(kk).dedx(2) = sigma_scaled
             tpcdedx(kk).id_track = track(j).id
             tpcdedx(kk).method = 4
          else
            tpcdedx_h.nok = tpcdedx_h.nok+1
            kk=tpcdedx_h.nok
            tpcdedx(kk).ndedx = nde
            tpcdedx(kk).dedx(1) = track(j).dedx(1)
            tpcdedx(kk).dedx(2) = track(j).dedx(2)
            tpcdedx(kk).id_track = track(j).id
            tpcdedx(kk).method = tdpar(1).averaging_method
          endif
            
  
       end do 
c      if (tdpar(1).averaging_method.eq.3) then
c         call hdelet(hhid)
c      endif
       write(m132,*)  'TDE - num_tptracks, num_tpcdedx, nzeroes: ',
     >       track_h.nok,tpcdedx_h.nok,nzero
       call StInfo(m132)        
  

      tde=STAFCV_OK
      end











