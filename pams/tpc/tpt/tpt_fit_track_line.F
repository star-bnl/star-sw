* $Id: tpt_fit_track_line.F,v 1.1 2000/03/01 22:21:33 sakrejda Exp $
*
      INTEGER FUNCTION TPT_FIT_TRACK_LINE(ipt,trk,hit_h,hit,
     >                               tpar_h,tpar,ipass)
C>---------------------------------------------------------------------
C
C TPT_FIT_TRACK - straigth track fitting routine
C DESCRIPTION:
C INPUT ARGUMENTS:
C OUTPUT ARGUMENTS:
C<---------------------------------------------------------------------

      IMPLICIT NONE
C
C     Input arguments
C     tpar_h    - header for the tptpar (tracking parameters) table
C     tpar     - tracking parameters table
C     hit_h     - header for the tphit (tpc hits) table
C     hit      - rows of the tphit table
C
C     Output arguments :
C     Values in track
C     iok = user status code
C
C     Functional description:
C     Fits a line to the TPC track ipt

#include "PAM.inc"

#include "tpt_pars.inc"
#include "tcl_tphit.inc"
#include "tpt_mxdim.inc"
#include "tpt_track_pointers.inc"
#include "math_constants.inc"
C______________________________________________________________________

      RECORD /table_head_st/    tpar_h
      RECORD /tpt_pars_st/    tpar(*)
      RECORD /table_head_st/    hit_h
      RECORD /tcl_tphit_st/     hit(*)
C_____________________________________________________________________
C     Local variables
C#include "tpt_pointers.inc"

      RECORD /tpt_track_pointers_st/ trk(*)

      INTEGER i, ipt, ipass, ntry, nfit, ient, ient1, nfit, ier, lswlf
      INTEGER iret, tpt_cross_fact
      REAL xfit(mxpnt), yfit(mxpnt), zfit(mxpnt), wxy(mxpnt), wz(mxpnt)
      REAL sfit(mxpnt), pnt(mxpnt)
      REAL cf, theta, ap, bp, dap, dbp, chi1, cp, dp, dcp, ddp, chi2
      REAL xold, yold, xt, yt
      LOGICAL good_hit(mxpnt), enough
C
      write(6,*)"I am here, fitting track ",ipt
C     at the beginning mark all the points that belong to a track as good
      do i=1,trk(ipt).nhit
	 good_hit(i) = .true.
      end do
C
      ntry   = 1            ! count number of iterations
      enough = .false.      ! enough= .true. means fit was successful
C     try to fit till successful( enough=.true.) or maximum number of 
C     iterations exceeded 
C
      do while (ntry.le.tpar(ipass).mxtry.and..not.enough)
	nfit = 0            ! counts number of points used in the fit
        cf = -1.0
        do i=trk(ipt).nhit,1,-1
C        load the working arrays for the fitting subroutines
C        omitt points that were marked as outliers
         if(good_hit(i)) then
	   nfit = nfit+1
           ient = trk(ipt).ipnt(i)
	   xfit(nfit) = hit(ient).x
	   yfit(nfit) = hit(ient).y
	   zfit(nfit) = hit(ient).z
           wxy(nfit)  = 1.0/(hit(ient).dx**2+hit(ient).dy**2)
C          calculate correction to the error, coming from the 
C          crossing angle
           iret = tpt_cross_fact(hit(ient).row,xfit(nfit),yfit(nfit),
     >                           xold,yold,cf,theta)
           if(nfit.eq.1) then
                ient1 = ient
           else if(nfit.eq.2) then
                wxy(1) = wxy(1)/cf
           endif
           xold = xfit(nfit)
           yold = yfit(nfit)
           wxy(nfit) = wxy(nfit)/cf
           wz(nfit) = 1.0/hit(ient).dz**2
           if(hit(ient).flag.ne.0) then 
              wxy(nfit)=0.001*wxy(nfit)
              wz(nfit)=0.001*wz(nfit)
           endif
           pnt(nfit)  = i ! pointer that allows to identify outliers
         endif
        end do
C
C       Now fit provided there are at least MINFIT hits left
        if(nfit.lt.tpar(ipass).minfit) then
            trk(ipt).flag=-1 ! track destroyed by the outliers removal
            ntry =  tpar(ipass).mxtry+1
        else
c           check whether to fit y=ax+b  or x=ay+b
            if( abs(xfit(1)-xfit(nfit)).gt.abs(yfit(1)-yfit(nfit))) then
c             it's y=ax+b
              ier =lswlf(xfit,yfit,wxy,nfit,ap,bp,chi1,dap,dbp) 
c             calculate point where the parameters will be given
              xold=(xfit(1)+ap*yfit(1)-ap*bp)/(1+ap**2)
              yold=ap*xold+bp
              sfit(1)=0
              do i=2,nfit
                  xt=(xfit(i)+ap*yfit(i)-ap*bp)/(1+ap**2)
                  yt=ap*xt+bp
                  sfit(i)=sqrt((xt-xold)**2+(yt-yold)**2)
              end do
            else
c             it's x=ay+b              
              ier =lswlf(yfit,xfit,wxy,nfit,ap,bp,chi1,dap,dbp) 
c             calculate point where the parameters will be given
              yold=(yfit(1)+ap*xfit(1)-ap*bp)/(1+ap**2)
              xold=ap*yold+bp
              sfit(1)=0
              do i=2,nfit
                  yt=(yfit(i)+ap*xfit(i)-ap*bp)/(1+ap**2)
                  xt=ap*yt+bp
                  sfit(i)=sqrt((xt-xold)**2+(yt-yold)**2)
              end do
            endif
c           now fit s-z
            ier =lswlf(sfit,zfit,wz,nfit,cp,dp,chi2,dcp,ddp) 
        endif
        enough=.true.
      end do
      trk(ipt).flag=1
      trk(ipt).p(1)=0
      trk(ipt).p(2)=sqrt(yold**2+xold**2)
      trk(ipt).p(3)=sfit(nfit)
      trk(ipt).p(4)=atan2(yold,xold)
      trk(ipt).p(5)=atan2(yt-yold,xt-xold)

*     tanl
      trk(ipt).p(6)=dp
      trk(ipt).p(7)=cp
      trk(ipt).p(8)=chi1
      trk(ipt).p(9)=chi2
      TPT_FIT_TRACK_LINE = STAFCV_OK
      return
      end
