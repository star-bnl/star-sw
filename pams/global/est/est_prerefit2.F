      integer function est_prerefit2(
     >     tphit_h, tphit,
     >     tptrack_h, tptrack, 
     >     est_ctrl_h, est_ctrl,
     >     est_match_h, est_match,
     >     egrpar_h,egrpar)
      implicit none

#include "est_prerefit2.inc"
#include "egr_track_pointers.inc"
#include "math_constants.inc"
#include "phys_constants.inc"
C_____________________________________________________________________
C version suitable for sorting of the hits from the outermost to the 
C innermost (ie from tpc to main vertex)

C     Local variables

      RECORD /track_pointers/gtrk(1)
      
      INTEGER sgn,itrk
      INTEGER ncir,ntry,iret,ibad,ient
      INTEGER i,j,jj,jjj
      INTEGER egr_helix_fit, est_find_outlier
      INTEGER egr_cross_fact
      INTEGER n_bad,ptgt
      INTEGER imsg,imsg2,imsg3
      INTEGER nf1,nf0,nfm1,nfm2,nfm3,nfm4,nfm5,nfm6,nfm7,curlo
      INTEGER modtrack
      INTEGER malloc
      INTEGER maxtrack
      INTEGER est2gtr(1)
      INTEGER pnt(mxpnt)
      REAL    xcir(mxpnt),ycir(mxpnt),zcir(mxpnt)
      REAL    wcir(mxpnt),wlin(mxpnt)
      REAL    as,bs,cf,xold,yold,theta
      REAL dx,dy,dz,ptsup
      REAL x0,y0,z0,rh
      REAL q_ch,b_magn,h
      REAL covar(3,3)
      REAL phi0,r0,tanl,psi
      CHARACTER*100 errmsg
      LOGICAL enough, good_hit(mxpnt)
      DATA imsg,imsg2,imsg3 /3*0/

      pointer(est2gtr_p,est2gtr)
      pointer(gtrk_p,gtrk)

      if (est_ctrl(1).ktrl1.eq.1) write(6,*)'Refitter started.'
      curlo = est_ctrl(1).current_loop
      if (curlo.eq.1) then
         ptsup=1.e+6
      else
         ptsup=est_ctrl(1).apmin(curlo-1)
      endif
C CALCULATE FIRST modtrack
      modtrack=0
      do i=1,est_match_h.nok
         if (((1./est_match(i).invp_tpc).gt.est_ctrl(1).apmin(curlo))
     >        .and.((1./est_match(i).invp_tpc).le.ptsup)
     >        .and.(est_match(i).flag.eq.-1)) then
            modtrack=modtrack+1
         endif
      enddo
      if (modtrack.eq.0) goto 789
C INITIALIZING THE GTRK TABLE
C FIRST WE SHOULD SET THE SIZE OF THE ARRAYS
C maxtrack is the size of est_match table
C modtrack is the number of tracks satisfying the current loop criteria
C these criteria are locally replaced by est2gtr<>0   
      maxtrack=est_match_h.nok
      est2gtr_p=malloc(maxtrack*4)
      call vzero(est2gtr(1),maxtrack)
      modtrack=0
      do i=1,est_match_h.nok
         if (((1./est_match(i).invp_tpc).gt.est_ctrl(1).apmin(curlo))
     >        .and.((1./est_match(i).invp_tpc).le.ptsup)
     >        .and.(est_match(i).flag.eq.-1)) then
            modtrack=modtrack+1
            est2gtr(i)=modtrack
         endif
      enddo
      i=abs(loc(gtrk(2))-loc(gtrk(1)))
      gtrk_p=malloc(modtrack*i)
      call vzero(gtrk(1),(modtrack*i)/4)

      if (est_ctrl(1).ktrl1.eq.1) write(6,*)
     *     'INITIALIZING EST2GTR and GTRK : rows=',maxtrack,modtrack
C     FILLING OF THE GTRK TABLE WITH THE TPC HITS
      if (est_ctrl(1).ktrl1.eq.1) write(6,*)
     *     'FILLING OF THE GTRK TABLE WITH THE TPC HITS'
      do j=1,tphit_h.nok
C     FIRST WE GET THE ORIGINAL TPC TRACK ID
         itrk = tphit(j).track/1000
         if (itrk.ge.1.and.itrk.le.est_match_h.nok) then 
            if (est2gtr(itrk).ne.0) then
C     POSITION OF THE HIT IN THE TPC TRACK
               ient=tptrack(itrk).nrec-(tphit(j).track-itrk*1000)+1
C     NO MORE THAN REF_MXPNT_TPC FROM THE TPC
               if (ient.ge.1.and.ient.le.est_ctrl(1).prerefit(curlo)) then
                  gtrk(est2gtr(itrk)).ipnt(ient) = j
                  gtrk(est2gtr(itrk)).pos(ient) = ient
                  gtrk(est2gtr(itrk)).det(ient) = 1
                  gtrk(est2gtr(itrk)).nhit = gtrk(est2gtr(itrk)).nhit + 1
               endif
            endif
         else
            if (itrk.ne.0) then
               write (errmsg,'(A60,I8)')
     >              ' EST_PREREFIT: track id out of range',tphit(j).track
               call message(errmsg,1,imsg3)
            endif
         endif
      enddo                     ! END OF LOOP ON TPC HITS
  
C     LOOP TO AVOID HOLE IN POSITION IN THE TPC_TRACK
      if (est_ctrl(1).ktrl1.eq.1) write(6,*)
     *     'FILLING THE HOLES (TPC HITS) IN THE GTRK TABLE'
      do i=1,est_match_h.nok
         if (est2gtr(i).ne.0) then
            if (gtrk(est2gtr(i)).nhit.ne.tptrack(i).nrec) then
               jj=2
               j=1
               do while(j.le.gtrk(est2gtr(i)).nhit)
                  if (gtrk(est2gtr(i)).ipnt(j).eq.0) then
                     do while((gtrk(est2gtr(i)).ipnt(jj).eq.0).and.(jj.le.tptrack(i).nrec))
                        jj=jj+1
                     enddo
                     gtrk(est2gtr(i)).ipnt(j) = gtrk(est2gtr(i)).ipnt(jj)
                     gtrk(est2gtr(i)).pos(j) = gtrk(est2gtr(i)).pos(jj)
                     gtrk(est2gtr(i)).det(j) = gtrk(est2gtr(i)).det(jj)
                     gtrk(est2gtr(i)).ipnt(jj)=0
                  else
                     j=j+1
                     jj=j+1
                  endif
               enddo
            endif
         endif
      enddo

      do i=1,est_match_h.nok
         if (est2gtr(i).ne.0) then
            gtrk(est2gtr(i)).ntpc = gtrk(est2gtr(i)).nhit
         endif
      enddo
C.....NOW WE LOOP OVER ALL TPC MATCHED TRACKS TO DO THE REFITTING 
      if (est_ctrl(1).ktrl1.eq.1) write(6,*)
     *     'LOOPING OVER ALL TPC TRACKS TO DO THE REFITTING'
      do i=1,est_match_h.nok 
C........TRACKS WITH GOOD PT AND GOOD FLAG
         if (est2gtr(i).ne.0) then

            est_match(i).flag = 0
            do j=1,gtrk(est2gtr(i)).nhit
               good_hit(j)=.true.
            enddo
            ntry=1
            enough=.false.
               
C..............TRY TO FIT UNTIL SUCCESSFUL( ENOUGH=.TRUE.) OR MAXIMUM NUMBER OF
C..............ITERATIONS EXCEEDED
            do while (ntry.le.egrpar(1).mxtry.and..not.enough)
               ncir = 0         ! counts number of points used in the fit
               cf = -1.0
               do j=1,gtrk(est2gtr(i)).nhit
                  if (j .le. est_ctrl(1).prerefit(curlo))then
                     ptgt=j
                     if(good_hit(ptgt)) then
                        ncir = ncir+1
                        ient = gtrk(est2gtr(i)).ipnt(j)
                        xcir(ncir) = tphit(ient).x
                        ycir(ncir) = tphit(ient).y
                        zcir(ncir) = tphit(ient).z
                        wcir(ncir) = 1.0/((tphit(ient).dx)**2+(tphit(ient).dy)**2)
C.......................CALCULATE CORRECTION TO THE ERROR, COMING FROM THE CROSSING ANGLE
                        iret = egr_cross_fact(tphit(ient).row,xcir(ncir),
     +                       ycir(ncir),xold,yold,cf,theta)
                        if(ncir.eq.2) then
                           wcir(1) = wcir(1)/cf
                        endif
                        xold = xcir(ncir)
                        yold = ycir(ncir)
                        wcir(ncir) = wcir(ncir)/cf
                        wlin(ncir) = 1.0/tphit(ient).dz**2
                        pnt(ncir)  = j ! pointer that identifies outliers
                     endif
                  endif
               enddo            ! END OF LOOP OVER TPC HITS

C     Now fit provided there are at least MINFIT hits left
               if(ncir.le.egrpar(1).minfit) then
                  gtrk(est2gtr(i)).flag=-1 ! track destroyed by the outliers removal
                  ntry =  egrpar(1).mxtry+1
               else
C     try the first full half of the circle
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
                  
C     Check whether enough hits survived
                  if(ncir.le.egrpar(1).minfit) then
                     gtrk(est2gtr(i)).flag=-2 ! not enough points on the first half
                     ntry =  egrpar(1).mxtry+1
                  else
C     If yes, call the helix fit
                     iret=egr_helix_fit(est2gtr(i),gtrk,xcir,ycir,zcir,
     >                    wcir,wlin,ncir,egrpar_h,egrpar,covar)

                     if(iret.eq.STAFCV_OK) then
                        if((gtrk(est2gtr(i)).p(8)/real(2*ncir-3).lt.
     >                       egrpar(1).prob(1).or.egrpar(1).prob(1)
     >                       .lt.0.0).and.
     >                       (gtrk(est2gtr(i)).p(9)/real(2*ncir-2).lt.
     >                       egrpar(1).prob(2)
     >                       .or.egrpar(1).prob(2).lt.0.0)) then
                           gtrk(est2gtr(i)).nfit = ncir
                           enough=.true. ! if fit succesful quit
                           gtrk(est2gtr(i)).flag=1
                        else
                           if(egrpar(1).debug(2).eq.1) then
                              call message('EGR_REFIT_TRACK-I: Calling 
     >                             outlier removal',1,imsg)
                           endif
                           iret=est_find_outlier(est2gtr(i),gtrk,xcir,ycir,zcir
     >                          ,wcir,wlin,pnt,ncir,ibad,egrpar_h,egrpar)
                           
                           if(egrpar(1).debug(2).eq.1) then
                              call message('EGR_REFIT_TRACK-I: Outlier 
     >                             removal completed',1,imsg)
                           endif
                           if(iret.eq.STAFCV_OK) then
                              good_hit(pnt(ibad))= .false.
                              gtrk(est2gtr(i)).flag=-3 ! Too many iterations
                              ntry = ntry+1 ! increase number of iterations
                           else
C     could not identify the outlier
                              gtrk(est2gtr(i)).flag = -6
                              ntry=egrpar(1).mxtry+1 ! Leave fit have a bad trk
                           endif
                        endif
                     else
                        gtrk(est2gtr(i)).flag=-4 ! Error from helix fit
                        ntry = egrpar(1).mxtry+1 ! Leave fit have a bad track
                     endif
                  endif
               endif
            enddo



C     clean up list of hits and remove outliers
            if(.not.enough) then
               EST_PREREFIT2 = STAFCV_BAD
            else
               goto 333
               n_bad=0          ! number of bad hits
               if( ntry .gt. 1) then 
                  do j=1,gtrk(est2gtr(i)).nhit
                     if( good_hit(j)) then
                        gtrk(est2gtr(i)).ipnt(j-n_bad) = gtrk(est2gtr(i)).ipnt(j)
                        gtrk(est2gtr(i)).det(j-n_bad) = gtrk(est2gtr(i)).det(j)
                        gtrk(est2gtr(i)).pos(j-n_bad) = j-n_bad
                     else
                        n_bad = n_bad + 1
                     endif
                  enddo
               endif
 333           continue
               EST_PREREFIT2 = STAFCV_OK
            endif
            if (gtrk(est2gtr(i)).flag.eq.1) then   
               b_magn = est_ctrl(1).bmag
               q_ch = est_match(i).q
               h = 0
               if (q_ch*b_magn.lt.0.) h = 1
               if (q_ch*b_magn.gt.0.) h = -1
               x0=abs(gtrk(est2gtr(i)).p(2))*cos(gtrk(est2gtr(i)).p(5))+
     >              gtrk(est2gtr(i)).p(3)
               y0 = abs(gtrk(est2gtr(i)).p(2))*sin(gtrk(est2gtr(i)).p(5))+
     >              gtrk(est2gtr(i)).p(4)
               phi0=atan2(y0,x0)
               phi0=phi0*C_DEG_PER_RAD
               if (phi0.gt.180.0) then
                  phi0 = phi0 - 360.0
               elseif(phi0 .lt. -180.0) then
                  phi0 = phi0 + 360.0
               endif
               r0 = sqrt(x0**2+y0**2)
               tanl = gtrk(est2gtr(i)).p(7)
               rh = abs(gtrk(est2gtr(i)).p(2))
               sgn = sign(1.0,gtrk(est2gtr(i)).p(2))
               psi = gtrk(est2gtr(i)).p(5)-sgn*C_PI_2
               psi = psi*C_DEG_PER_RAD    ! to degrees
               if(psi.gt.180.0) then
                  psi= psi-360.0
               elseif( psi .lt. -180.0) then
                  psi = psi + 360.0
               endif
               z0 = gtrk(est2gtr(i)).p(6)
               
               est_match(i).r0 = r0
               est_match(i).phi0 = phi0
               est_match(i).z0 = z0
               est_match(i).psi = psi
               est_match(i).invp =1./(rh*abs(10.*b_magn*q_ch*C_D_CURVATURE))
               est_match(i).tanl = tanl
            endif
            
C..............SAVING THE REFITTER FLAG     
            jjj=1
            do while (jjj.lt.10.and.est_match(i).rflag(jjj).ne.0)
               jjj = jjj+1
            enddo
            est_match(i).rflag(jjj)=gtrk(est2gtr(i)).flag
         endif
      enddo

 789  continue
      if (est_ctrl(1).ktrl1.eq.1) then
         write(6,*)' printing out the statistics on flags'
         nf1=0
         nf0=0
         nfm1=0
         nfm2=0
         nfm3=0
         nfm4=0
         nfm5=0
         nfm6=0
         nfm7=0
         
         do i=1,modtrack
            if (gtrk(i).flag.eq. 1) nf1=nf1+1
            if (gtrk(i).flag.eq. 0) nf0=nf0+1
            if (gtrk(i).flag.eq.-1) nfm1=nfm1+1
            if (gtrk(i).flag.eq.-2) nfm2=nfm2+1
            if (gtrk(i).flag.eq.-3) nfm3=nfm3+1
            if (gtrk(i).flag.eq.-4) nfm4=nfm4+1
            if (gtrk(i).flag.eq.-5) nfm5=nfm5+1
            if (gtrk(i).flag.eq.-6) nfm6=nfm6+1
            if (gtrk(i).flag.eq.-7) nfm7=nfm7+1
         enddo
         write(6,*)'prerefit :curlo,concerned_track : ',curlo,modtrack
         write(6, *)'flag        1      0     -1     -2     -3     -4     -5     -6     -7'
         write(6,75)'number ',nf1,nf0,nfm1,nfm2,nfm3,nfm4,nfm5,nfm6,nfm7
 75      format(a7,9I7)
         write(6,*)'Refitter finished.'
      endif
C FREE THE ALLOCATED MEMORY
      if (modtrack.ne.0) then
         call free(gtrk_p)
         call free(est2gtr_p)
      endif
      return
      end


C>--------------------------------------------------------------------
C
C EST_FIND_OUTLIER - identifies position of the outlier on a track
C
C DESCRIPTION:
C For every point used in the fit weighted residua in both projections
C are calculated and the one with the worst chisq is identified.
C
C INPUT PARAMETERS:
C  ipt  - track id
C  trk  - auxiliary tracking table
C  xcir - x coordinates of the hits
C  ycir - y coordinates of the hits
C  zcir - z coordinates of the hits
C  wcir - weights for the circle fit
C  wlin - weights for the linear regresion fit
C  ncir - number of points 	INCLUDEd in fit
C
C OUTPUT PARAMETERS:
C  ibad - id of a point with the worst chisq contribution
C
C AUTHOR:
C Iwona Sakrejda
C<---------------------------------------------------------------------
      INTEGER*4 FUNCTION EST_FIND_OUTLIER(ipt,trk,xcir,ycir,zcir
     >                  ,wcir,wlin,pnt,ncir,ibad,egrpar_h,egrpar)
 
      IMPLICIT NONE
#include "PAM.inc"
#include "tpt_track.inc"
#include "egr_egrpar.inc"
#include "egr_track_pointers.inc"
C__________________________________________________________________
      RECORD /track_pointers/ trk(*)
      RECORD /table_head_st/    egrpar_h
      RECORD /egr_egrpar_st/    egrpar(*)
C_________________________________________________________________
      INTEGER ibad, ipt, ncir, i, ibig
      REAL    xcir(*),ycir(*),zcir(*),wcir(*),wlin(*),pnt(*)
      REAL    zn1,resbig,restot,phi0,dz,ri,rr
      REAL    stot,dsxy,dsz,acent,bcent,xn1,yn1
      CHARACTER*100 errmsg
      INTEGER imsg,imsg1
      DATA imsg,imsg1 /2*0/
C_________________________________________________________________

      if(egrpar(1).debug(6).eq.1) then
         write (errmsg,'(A60),I5')
     >        ' EST_FIND_OUTLIER-I: track id ',ipt
         call message(errmsg,1,imsg)
      endif
      ibig = 0
      resbig  = 0.0
      stot = 0.0
      phi0 = trk(ipt).p(5)      ! angle in the helix coor. system
      rr = abs(trk(ipt).p(2))
      acent = trk(ipt).p(3)
      bcent = trk(ipt).p(4)
      zn1 = trk(ipt).p(6)
      yn1 = ycir(1)
      xn1 = xcir(1)
      do i=1,ncir
         ri = abs(sqrt((xcir(i)-acent)**2+(ycir(i)-bcent)**2)- rr)
         if(abs(trk(ipt).p(7)).lt.0.0000001) then
            est_find_outlier = STAFCV_BAD
            return
         endif
         dsz = (zcir(i) - zn1)/trk(ipt).p(7)
         if(dsz.ge.2.0*3.141592653589*rr) then
            est_find_outlier = STAFCV_BAD
C     it's a very special case
         else
            dsxy = 0.5*sqrt((ycir(i)-yn1)**2+(xcir(i)-xn1)**2)/rr
            if( abs(dsxy) .lt. 1.0) then
               stot = stot + asin(dsxy)*rr*2.0
               dz = trk(ipt).p(6) + stot*trk(ipt).p(7)-zcir(i)
               restot = dz**2*wlin(i)+ri*wcir(i)
                if(restot.gt.resbig) then
                  ibig=i
                  resbig = restot
               endif
               zn1 = zcir(i)
               yn1 = ycir(i)
               xn1 = xcir(i)
            else
               est_find_outlier = STAFCV_BAD
               return
            endif
         endif
      end do
      ibad = ibig
      if( ibad .eq. 0) then
         est_find_outlier = STAFCV_BAD
      else
         est_find_outlier = STAFCV_OK
      endif
      return
      end
