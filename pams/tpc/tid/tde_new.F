* $Id: tde_new.F,v 1.5 1998/08/05 21:58:30 fisyak Exp $
* $Log: tde_new.F,v $
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
      INTEGER FUNCTION tde_new(tdpar_h,tdpar,hit_h,hit,
     >                         track_h,track,pad_h,pad)

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

#include "tde_new.inc"
      STRUCTURE /tde_local/
                REAL length
                REAL charge
                REAL adedx
      END STRUCTURE

C-----------------------------------------------------------------------
#include "tpt_const.inc"
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
C
c     variables for maximumum likeihood
c
      REAL hxmin,hxmax,hxamp
      INTEGER hhid, hnbin
      REAL hparam(3),hstep(3),hpmin(3),hpmax(3),hsig(3),hchi2

c
      PARAMETER (ndemax=1000)
      PARAMETER (nlocmax=400000)
      PARAMETER  (nloctrc=10000)
c
      INTEGER loc_hit(nlocmax),loc_track(nloctrc)

      RECORD  /tde_local/ dedx(ndemax)
c      DATA    bconv /0.00299792458/
c      DATA    dtorad /0.0174532925/
c      DATA    cpi /3.14159265/
      DATA    imsg1 /0/, imsg2 /0/, imsg3 /0/

      EXTERNAL landau2
C-----------------------------------------------------------------------


*      call message('TDE_DEDX-I: Execution started',1,imsg1)

C Abort if there are no TPC hits to work with

      if (hit_h.nok.eq.0) then
*        call message('TDE_DEDX-W: No TPC hitss. 
*    >                TDE aborted.',1,imsg2)
         tde_new=STAFCV_BAD
         return
      endif
C Abort if there are too many TPC hits to work with

      if (hit_h.nok.gt.nlocmax) then
*        call message('TDE_DEDX-W: Too many TPC hits. 
*    >                 TDE aborted.',1,imsg2)
         tde_new=STAFCV_BAD
         return
      endif

C Abort if there are no TPC tracks to work with

      if (track_h.nok.eq.0) then
*        call message('TDE_DEDX-W: No TPC tracks. 
*    >                 TDE aborted.',1,imsg3)
         tde_new=STAFCV_BAD
         return
      endif
C Abort if there are too many TPC tracks to work with

      if (track_h.nok.gt.nloctrc) then
*        call message('TDE_DEDX-W: too many TPC tracks. 
*    >                 TDE aborted.',1,imsg3)
         tde_new=STAFCV_BAD
         return
      endif

      call gufld(xlocal,bfield) 
      
c
c  this only once on initialisation-
c field is uniform enough in the TPC)

c
c as a test for the likelihood method
c book a histigram, fill it, fit, then delete
c if this works, find a more efficient way later
c C. Ogilvie June 1998
c
c to run this option use
c      tdpar(1).debug(2)=2
c      tdpar(1).truncopt=4
c      tdpar(1).truncfact=0.0
      

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
c old hardwired B=0.5
c         radius = 1. / (track(j).invp * 0.5 * bconv)
c
         radius=1.0 / (track(j).invp*bfield(3)*C_D_CURVATURE)
         psic = track(j).psi*dtorad + sign(1,track(j).q)*C_PI*0.5

         xc = x1 - radius*cos(psic)
         yc = y1 - radius*sin(psic)
         cosl= sqrt(1.0+track(j).tanl**2)
C        loop over all the hits on this track to calculate the track length
C        and average charge
************************************************************************
*   Calculate dE/dx for each track & output to tptrack table  (TdEdx)  *
************************************************************************
C
C        tdpar.truncopt=1   truncate tdpar.debug(1) points 
C                            with larger dE/dx
C        tdpar.truncopt=2   use all dedx points
C        tdpar.truncopt=4   truncate P% of points with larger dE/dx
C
C        tdpar.debug(2)=0   calculate dE/dx=(sum of q)/(sum of s)
C
C        tdpar.debug(2)=1   calculate dE/dx=(sum of q/s)/(# of pnts)
c     
c        tdpar.debug(2)=2   calculate dE/dx as maximum likelihood
C
C        tdpar.usemerge=1   skipp  point flagged as merged 
c                           implemented as default, with no
c                           option yet to include merged
C
C        if row less than tdpar.minrow skipp the point
C        options based on Jeff Mitchell's 


         nde=0
         do while (int(hit(loc_hit(l)).track/1000).
     >      eq.itrack.and.last.eq.0)
            irow=mod(hit(loc_hit(l)).row,100)
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
               
               if(iret1.eq.0.and.iret2.eq.0) then
                  ds=radius*2.0*
     >            asin(0.5*sqrt((xa-xb)**2+(ya-yb)**2)
     >            /radius)*cosl
                  hit(loc_hit(l)).dq=ds
c
c check on quality of hit before including it into dedx
c but currently (June 98) merged hits are filtered out before
c they reach tde_new
c
                  if ((hit(loc_hit(l)).flag.eq.0).and.(nde.lt.ndemax)) then
                    nde=nde+1
                    dedx(nde).length=ds
                    dedx(nde).charge=hit(loc_hit(l)).q
                    dedx(nde).adedx=dedx(nde).charge/ds
                  endif
               else if((xc-hit(loc_hit(l)).x)**2+
     >            (yc-hit(loc_hit(l)).y)**2
     >            .gt.radius**2) then
                  hit(loc_hit(l)).dq=-1.0
                  track(j).flag=-1
               else
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
         iret=tls_Quick_Sort_r(nde,dedx(1).adedx,
     >        dedx(2).adedx,dedx)
c
c truncating options
c
         nde_orig=nde
         if(tdpar(1).truncopt.eq.1) then
            nde=nde-tdpar(1).debug(1)
            if(nde.lt.0) nde=0
         else if(tdpar(1).truncopt.eq.4) then
            nde=nde-int(tdpar(1).truncfact*real(nde))
            if(nde.lt.0) nde=0
         elseif (tdpar(1).truncopt.eq.2) then
c
c            no truncation nde=nde
c
         endif
         track(j).ndedx=nde
         track(j).dedx(1)=0.
         track(j).dedx(2)=0.
c
c options for dE/dx calc
c
         if(tdpar(1).debug(2).eq.0.and.nde.gt.0) then
            do k=1,nde
               track(j).dedx(1)=track(j).dedx(1)+ dedx(k).length
               track(j).dedx(2)=track(j).dedx(2)+ dedx(k).charge
            end do
            track(j).dedx(1)=track(j).dedx(2)/track(j).dedx(1)
         else if(tdpar(1).debug(2).eq.1.and.nde.gt.0) then
            do k=1,nde
               track(j).dedx(1)= track(j).dedx(1)+ dedx(k).adedx
               track(j).dedx(2)=track(j).dedx(2)+ dedx(k).charge
            end do
            track(j).dedx(1)=track(j).dedx(1)/real(nde)
         elseif (tdpar(1).debug(2).eq.2.and.nde.gt.10) then
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

           call hfith(hhid,landau2,'QL',3,hparam,hstep,hpmin,hpmax,hsig,hchi2)
            track(j).dedx(1)=hparam(2)
            track(j).dedx(2)=hxamp
            call hdelet(hhid)
         endif
c
c
  
       end do 
c      if (tdpar(1).debug(2).eq.2) then
c         call hdelet(hhid)
c      endif
      tde_new=STAFCV_OK
      end





