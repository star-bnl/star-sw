* $Id: tpt_fit_track.F,v 1.4 1998/07/07 14:30:51 sakrejda Exp $
* $Log: tpt_fit_track.F,v $
* Revision 1.4  1998/07/07 14:30:51  sakrejda
* pnt defined as integer
*
* Revision 1.3  1998/02/06 22:03:15  fisyak
* Remove tgc_cross_and_dip.F
*
* Revision 1.2  1998/01/27 00:35:46  fisyak
* Split sources
*
      INTEGER FUNCTION TPT_FIT_TRACK(ipt,trk,hit_h,hit,
     >                               tpar_h,tpar,tte_skip,ipass)
C>---------------------------------------------------------------------
C
C TPT_FIT_TRACK - track fitting routine
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
C     Fits a helix to the TPC track ipt

#include "PAM.inc"

#include "tpt_pars.inc"
#include "tcl_tphit.inc"
C______________________________________________________________________

      RECORD /table_head_st/    tpar_h
      RECORD /tpt_pars_st/    tpar(*)
      RECORD /table_head_st/    hit_h
      RECORD /tcl_tphit_st/     hit(*)
C_____________________________________________________________________
C     Local variables
#include "tpt_pointers.inc"

      RECORD /track_pointers/ trk(*)

      INTEGER ipt, ncir,i,ntry,j,iret,ibad,ient ,ient1
      INTEGER tpt_helix_fit, tpt_find_outlier
      INTEGER tpt_cross_fact, tte_skip,ipass
      INTEGER n_good, ipnt_temp(mxpnt)
      LOGICAL enough, good_hit(mxpnt)
      REAL    xcir(mxpnt),ycir(mxpnt),zcir(mxpnt)
      REAL    wcir(mxpnt),wlin(mxpnt)
      INTEGER pnt(mxpnt)
      REAL    as,bs,cf,xold,yold,theta
      REAL    r1,tanphi0

C
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
	ncir = 0            ! counts number of points used in the fit
        cf = -1.0
        do i=trk(ipt).nhit,1,-1
C        load the working arrays for the fitting subroutines
C        omitt points that were marked as outliers
         if(good_hit(i)) then
	   ncir = ncir+1
           ient = trk(ipt).ipnt(i)
	   xcir(ncir) = hit(ient).x
	   ycir(ncir) = hit(ient).y
	   zcir(ncir) = hit(ient).z
           wcir(ncir) = 1.0/(hit(ient).dx**2+hit(ient).dy**2)
C          calculate correction to the error, coming from the 
C          crossing angle
           iret = tpt_cross_fact(hit(ient).row,xcir(ncir),ycir(ncir),
     >                           xold,yold,cf,theta)
           if(ncir.eq.1) then
                ient1 = ient
           else if(ncir.eq.2) then
                wcir(1) = wcir(1)/cf
           endif
           xold = xcir(ncir)
           yold = ycir(ncir)
           wcir(ncir) = wcir(ncir)/cf
           wlin(ncir) = 1.0/hit(ient).dz**2
           pnt(ncir)  = i ! pointer that allows to identify outliers
         endif
        end do
C
C       Now fit provided there are at least MINFIT hits left
        if(ncir.lt.tpar(ipass).minfit) then
            trk(ipt).flag=-1 ! track destroyed by the outliers removal
            ntry =  tpar(ipass).mxtry+1
        else
C           try the  first full half of the circle
            bs=0
            j=2
	    as=(xcir(1)-xcir(2))**2+(ycir(1)-ycir(2))**2
	    do while (bs.lt.as.and.j.lt.ncir)
              j=j+1
	      bs = as
              as = (xcir(1)-xcir(j))**2+(ycir(1)-ycir(j))**2
            end do
            if(bs.gt.as) then
                  ncir = j-1
            endif
C
C           Check whether enough hits survived
            if(ncir.lt.tpar(ipass).minfit) then
              trk(ipt).flag=-2    ! not enough points on the first half
              ntry =  tpar(ipass).mxtry+1
            else
C             If yes, call the helix fit
              iret=tpt_helix_fit(ipt,trk,xcir,ycir,zcir,
     >                           wcir,wlin,ncir,tpar_h,tpar,ipass)
C             calculate the error squared on phi0
              r1 = (xcir(1)-trk(ipt).p(3))**2 + 
     >             (ycir(1)-trk(ipt).p(4))**2
              tanphi0 = (ycir(1)-trk(ipt).p(4))/
     >                  (xcir(1)-trk(ipt).p(3))
              trk(ipt).dpar(7) = ((xcir(1)-trk(ipt).p(3))/r1)**2 *
     >        (hit(ient1).dy**2 + abs(trk(ipt).dpar(4)) +
     >        tanphi0**2*(hit(ient1).dx**2 + abs(trk(ipt).dpar(1))))
              if(iret.eq.STAFCV_OK) then
                   if((trk(ipt).p(8)/real(ncir-3).lt.
     >                 tpar(ipass).prob(1)
     >             .or.tpar(ipass).prob(1).lt.0.0).and.
     >             (trk(ipt).p(9)/real(ncir-2).lt.
     >                 tpar(ipass).prob(2)
     >             .or.tpar(ipass).prob(2).lt.0.0)) then
                     trk(ipt).nfit = ncir
                     enough=.true.        ! if fit succesful quit
                     trk(ipt).flag=ipass
                   else
                     if(tpar(ipass).debug(2).eq.1) then
                         write(6,*) 'Calling outliers removal'
                     endif
                     iret=tpt_find_outlier(ipt,trk
     >                  ,xcir,ycir,zcir,wcir,wlin,ncir,tpar_h,tpar,
     >                   ibad,ipass)
                     if(tpar(ipass).debug(2).eq.1) then
                        write(6,*) 'Out from the outliers removal'
                     endif
                     if(iret.eq.STAFCV_OK) then
                        good_hit(pnt(ibad))= .false.
                        trk(ipt).flag=-3      ! too many iterations
                        ntry = ntry+1    ! increase number of iterations
                     else
C                       could not identify the outlier
                        trk(ipt).flag = -6
                        ntry=tpar(ipass).mxtry+1
                     endif
                   endif
              else
                   trk(ipt).flag=-4      ! too many iterations
                   ntry = ntry+1        ! increase number of iterations
              endif
            end if 
        end if
      end do
C
C     clean up list of hits and remove outliers
      if(.not.enough) then
C         fit wasn't succesful, reset all the points
          TPT_FIT_TRACK = STAFCV_BAD 
      else
          n_good=0    ! number of kept hits
          do i=1,trk(ipt).nhit
             if(good_hit(i)) then
                n_good = n_good+1                  ! count good hits
                ipnt_temp(n_good)=trk(ipt).ipnt(i) ! and memorize them
             else
*               forget about this hit- 
*               The "tte_skip" variable keeps this routine from chainging
*               the hit table when called by tte.
                if (tte_skip.ne.-999) then 
                 if(hit(trk(ipt).ipnt(i)).track.gt.0)
     >           hit(trk(ipt).ipnt(i)).track=0.0
                 endif
                endif
          end do 
*         reload good points to the trk.ipnt
          if(n_good.lt.trk(ipt).nhit) then
            trk(ipt).nhit = n_good
            do i=1,n_good
              trk(ipt).ipnt(i)=ipnt_temp(i)
*             and change their position on the track
            end do
          endif
          TPT_FIT_TRACK = STAFCV_OK
      endif
      return
      end
