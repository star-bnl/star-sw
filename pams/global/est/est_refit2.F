      integer function est_refit2(
     >     tphit_h, tphit,
     >     tptrack_h, tptrack,  
     >     est_ctrl_h, est_ctrl,
     >     est_match_h, est_match,
     >     egrpar_h,egrpar)
      implicit none

#include "est_refit2.inc"
#include "egr_track_pointers.inc"
#include "phys_constants.inc"
#include "math_constants.inc"
C_____________________________________________________________________
C version suitable for sorting of the hits from the outermost to the 
C innermost (ie from tpc to main vertex)

C     Local variables

      integer mxtrack
      PARAMETER (mxtrack=30000)
      RECORD /track_pointers/ gtrk(mxtrack)
      
      INTEGER itrk,sgn
      INTEGER ncir,ntry,iret,ibad,ient,ient_total
      INTEGER i,j,jj,jjj,k,n
      INTEGER egr_helix_fit, est_find_outlier
      INTEGER egr_cross_fact
      INTEGER n_bad
      INTEGER imsg,imsg2,imsg3
      INTEGER ntot,nf1,nf0,nfm1,nfm2,nfm3,nfm4,nfm5,nfm6,nfm7,curlo
      integer ptgt, pnt(mxpnt)
      LOGICAL enough, good_hit(mxpnt)
      REAL    xcir(mxpnt),ycir(mxpnt),zcir(mxpnt)
      REAL    wcir(mxpnt),wlin(mxpnt)
      REAL    as,bs,cf,xold,yold,theta
      REAL dx,dy,dz,angle,ptsup
      REAL x0,y0,z0,rh
      REAL q_ch,b_magn,h
      REAL phi0,r0,tanl,psi
      CHARACTER*100 errmsg
      REAL    covar(3,3)
      DATA imsg,imsg2,imsg3 /3*0/



      if (est_ctrl(1).ktrl1.eq.1) write(6,*)'Refitter started.'


      curlo = est_ctrl(1).current_loop
      if (curlo.eq.1) then
         ptsup=1.e+6
      else
         ptsup=est_ctrl(1).apmin(curlo-1)
      endif

C.....INITIALIZING THE GTRK TABLE
C      write(6,*)'INITIALIZING THE GTRK TABLE'
      do i=1,est_match_h.nok
         if (est_match(i).flag.eq.0) then
            gtrk(i).nhit=0
            gtrk(i).ntpc=0
            gtrk(i).nfit=0
            gtrk(i).flag=0
            do j=1,mxpnt
               gtrk(i).ipnt(j)=0
               gtrk(i).pos(j)=0
               gtrk(i).det(j)=0
            enddo
            do j=1,9
               gtrk(i).p(j) = 0.
            enddo
         endif
      enddo


C.....FILLING OF THE GTRK TABLE WITH THE SVT HITS
C      write(6,*)'FILLING OF THE GTRK TABLE WITH THE SVT HITS',est_match_h.nok
      do i=1,est_match_h.nok
         if ((est_match(i).flag.eq.0).and.((1./est_match(i).invp_tpc).gt.
     >        est_ctrl(1).apmin(curlo)).and.(est_match(i).n_svt_hits.ne.0)) then
            ient=0
            do j=est_match(i).n_svt_hits,1,-1
               gtrk(i).nhit=gtrk(i).nhit+1
               ient=ient+1
               gtrk(i).ipnt(ient)=j
               gtrk(i).pos(ient)=ient
               gtrk(i).det(ient)=2
            end do
         endif
      enddo
C.....FILLING OF THE GTRK TABLE WITH THE TPC HITS
      do j=1,tphit_h.nok
C........FIRST WE GET THE ORIGINAL TPC TRACK ID
         itrk = tphit(j).track/1000
         if (itrk.ge.1.and.itrk.le.est_match_h.nok) then
C...........POSITION OF THE HIT IN THE TPC TRACK
            ient=tptrack(itrk).nrec-(tphit(j).track-itrk*1000)+1
            if ((est_match(itrk).flag.eq.0).and.((1./est_match(itrk).invp_tpc).gt.
     >           est_ctrl(1).apmin(curlo)).and.(est_match(itrk).n_svt_hits.ne.0)) then
C.................NO MORE THAN REF_MXPNT_TPC FROM THE TPC
               if (ient.ge.1.and.ient.le.est_ctrl(1).prerefit(curlo)) then
C....................NO MORE THAN REF_MXPNT_TPC+N_SVT_HITS FOR THE TOTAL TRACK
                  if (gtrk(itrk).nhit .le.
     >                 (est_ctrl(1).prerefit(curlo)+est_match(itrk).n_svt_hits)) then
                     ient_total=ient+est_match(itrk).n_svt_hits
                     gtrk(itrk).ipnt(ient_total) = j
                     gtrk(itrk).pos(ient_total) = ient_total 
                     gtrk(itrk).det(ient_total) = 1
                     gtrk(itrk).nhit = gtrk(itrk).nhit + 1
                  endif
               endif
            endif
         else
            if (itrk.ne.0) then
               write (errmsg,'(A60,I8)')
     >              ' EGR_REFITTER-W: track id out of range',
     >              tphit(j).track
               call message(errmsg,1,imsg3)
            endif
         endif
      enddo                     ! END OF LOOP ON TPC HITS

      

      do i=1,est_match_h.nok
         gtrk(i).ntpc = gtrk(i).nhit-est_match(i).n_svt_hits
      enddo

      do i=1,est_match_h.nok
         if ((est_match(i).flag.eq.0).and.((1./est_match(i).invp_tpc).gt.
     >        est_ctrl(1).apmin(curlo)).and.(est_match(i).n_svt_hits.ne.0)) then
            if (gtrk(i).ntpc.ne.tptrack(i).nrec) then
               jj=est_match(i).n_svt_hits+2
               j=est_match(i).n_svt_hits+1
               do while(j.le.gtrk(i).nhit)
                  if (gtrk(i).ipnt(j).eq.0) then
                     do while((gtrk(i).ipnt(jj).eq.0).and.
     >                    (jj.le.(tptrack(i).nrec+est_match(i).n_svt_hits)))
                        jj=jj+1
                     enddo
                     gtrk(i).ipnt(j) = gtrk(i).ipnt(jj)
                     gtrk(i).pos(j) = gtrk(i).pos(jj)
                     gtrk(i).det(j) = gtrk(i).det(jj)
                     gtrk(i).ipnt(jj)=0
                  else
                     j=j+1
                     jj=j+1
                  endif
               enddo
            endif
         endif
      enddo

     

C.....NOW WE LOOP OVER ALL TPC MATCHED TRACKS TO DO THE REFITTING 
      do i=1,est_match_h.nok 
         
C........TRACKS WITH GOOD PT AND GOOD FLAG
         if (((1./est_match(i).invp_tpc).gt.est_ctrl(1).apmin(curlo))
     >        .and.(est_match(i).flag.eq.0).and.(est_match(i).n_svt_hits.ne.0)) then
C...........POSSIBLE PRE-REFITTING BEFORE TRACKING
            do j=1,gtrk(i).nhit
               good_hit(j)=.true.
            enddo
            
            ntry=1
            enough=.false.
            
C..............TRY TO FIT UNTIL SUCCESSFUL( ENOUGH=.TRUE.) OR MAXIMUM NUMBER OF
C..............ITERATIONS EXCEEDED
            do while (ntry.le.egrpar(1).mxtry.and..not.enough)
               ncir = 0         ! counts number of points used in the fit
               cf = -1.0
               
C.................LOAD THE FITTING ARRAYS WITH THE SVT SPACE POINTS
               do j=est_match(i).n_svt_hits,1,-1
                  ient=j
                  ptgt=est_match(i).n_svt_hits-j+1
                  if (good_hit(ptgt) .ne. .false. ) then
                     ncir = ncir + 1
                     xcir(ncir) = est_match(i).r_4th(ient)
     &                    *cos(est_match(i).phi_4th(ient))
                     ycir(ncir) = est_match(i).r_4th(ient)
     &                    *sin(est_match(i).phi_4th(ient))
                     zcir(ncir) =est_match(i).z_4th(ient)
                     angle = est_match(i).ang_det(ient)
                     dx = est_ctrl(1).svt_er*abs(sin(angle)) + 0.0005
                     dy = est_ctrl(1).svt_er*abs(cos(angle)) + 0.0005
                     dz = est_ctrl(1).svt_er + 0.0005
                     wcir(ncir)=1.0/(dx**2+dy**2)
                     wlin(ncir) = 1.0/dz**2
                     pnt(ncir) = ptgt ! Used to identify outliers
                  endif         ! END OF GOOD HITS (NOT OUTLIERS)
               enddo            ! END OF HITS
               


C.................LOAD THE FITTING ARRAYS WITH THE TPC POINTS
               do j=est_match(i).n_svt_hits+1,gtrk(i).nhit
                  ptgt=j
                  
                  if(good_hit(ptgt)) then
                     ncir = ncir+1
                     ient = gtrk(i).ipnt(j)
                     xcir(ncir) = tphit(ient).x
                     ycir(ncir) = tphit(ient).y
                     zcir(ncir) = tphit(ient).z
                     dx=tphit(ient).dx
                     dy=tphit(ient).dy
                     dz=tphit(ient).dz
                     wcir(ncir) = 1.0/(dx**2+dy**2)
                     
C.......................CALCULATE CORRECTION TO THE ERROR, COMING FROM THE CROSSING ANGLE
                     iret = egr_cross_fact(tphit(ient).row,xcir(ncir),
     +                    ycir(ncir),xold,yold,cf,theta)
                     
                     if(ncir.eq.2) then
                        wcir(1) = wcir(1)/cf
                     endif
                     xold = xcir(ncir)
                     yold = ycir(ncir)
                     wcir(ncir) = wcir(ncir)/cf
                     wlin(ncir) = 1.0/dz**2
                     pnt(ncir)  = j ! pointer that identifies outliers
                  endif
               enddo            ! END OF LOOP OVER TPC HITS
               
C     Now fit provided there are at least MINFIT hits left
               if(ncir.le.egrpar(1).minfit) then
                  gtrk(i).flag=-1 ! track destroyed by the outliers removal
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
C     
C     Check whether enough hits survived
                  if(ncir.le.egrpar(1).minfit) then
                     gtrk(i).flag=-2 ! not enough points on the first half
                     ntry =  egrpar(1).mxtry+1
                  else
C     If yes, call the helix fit
                     iret=egr_helix_fit(i,gtrk,xcir,ycir,zcir,
     >                    wcir,wlin,ncir,egrpar_h,egrpar,covar)
                     
                     if(iret.eq.STAFCV_OK) then
                        if((gtrk(i).p(8)/real(ncir-3).lt.
     >                       egrpar(1).prob(1).or.egrpar(1).prob(1)
     >                       .lt.0.0).and.
     >                       (gtrk(i).p(9)/real(ncir-2).lt.
     >                       egrpar(1).prob(2)
     >                       .or.egrpar(1).prob(2).lt.0.0)) then
                           gtrk(i).nfit = ncir
                           enough=.true. ! if fit succesful quit
                           gtrk(i).flag=1
                        else
                           if(egrpar(1).debug(2).eq.1) then
                              call message('EGR_REFIT_TRACK-I: Calling 
     >                             outlier removal',1,imsg)
                           endif
                           iret=est_find_outlier(i,gtrk,xcir,ycir,zcir
     >                          ,wcir,wlin,pnt,ncir,ibad,egrpar_h,egrpar)
                           if(egrpar(1).debug(2).eq.1) then
                              call message('EGR_REFIT_TRACK-I: Outlier 
     >                             removal completed',1,imsg)
                           endif
                           if(iret.eq.STAFCV_OK) then
                              good_hit(pnt(ibad))= .false.
                              gtrk(i).flag=-3 ! Too many iterations
                              ntry = ntry+1 ! increase number of iterations
                           else
C     could not identify the outlier
                              gtrk(i).flag = -6
                              ntry=egrpar(1).mxtry+1 ! Leave fit have a bad trk
                           endif
                        endif
                     else
                        gtrk(i).flag=-4 ! Error from helix fit
                        ntry = egrpar(1).mxtry+1 ! Leave fit have a bad track
                     endif
                  endif
               endif
            enddo



C     clean up list of hits and remove outliers
            if(.not.enough) then
               EST_REFIT2 = STAFCV_BAD
            else
               goto 333
               n_bad=0          ! number of bad hits
               if( ntry .gt. 1) then 
                  do j=1,gtrk(i).nhit
                     if( good_hit(j)) then
                        gtrk(i).ipnt(j-n_bad) = gtrk(i).ipnt(j)
                        gtrk(i).det(j-n_bad) = gtrk(i).det(j)
                        gtrk(i).pos(j-n_bad) = j-n_bad
                     else
                        n_bad = n_bad + 1
                     endif
                  enddo
               endif
 333           continue
               EST_REFIT2 = STAFCV_OK
            endif
            if (gtrk(i).flag.eq.1) then
               b_magn = est_ctrl(1).bmag
               q_ch = est_match(i).q
               h = 0
               if (q_ch*b_magn.lt.0.) h = 1
               if (q_ch*b_magn.gt.0.) h = -1
               x0=abs(gtrk(i).p(2))*cos(gtrk(i).p(5))+
     +              gtrk(i).p(3)
               y0 = abs(gtrk(i).p(2))*sin(gtrk(i).p(5))+
     +              gtrk(i).p(4)
               phi0=atan2(y0,x0)
               phi0=phi0*C_DEG_PER_RAD
               if (phi0.gt.180.0) then
                  phi0 = phi0 - 360.0
               elseif(phi0 .lt. -180.0) then
                  phi0 = phi0 + 360.0
               endif
               r0 = sqrt(x0**2+y0**2)
               tanl = gtrk(i).p(7)
               rh = abs(gtrk(i).p(2))
               sgn = sign(1.0,gtrk(i).p(2))
               psi = gtrk(i).p(5)-sgn*C_PI_2
               psi = psi*C_DEG_PER_RAD    ! to degrees
               if(psi.gt.180.0) then
                  psi= psi-360.0
               elseif( psi .lt. -180.0) then
                  psi = psi + 360.0
               endif
               z0 = gtrk(i).p(6)
               est_match(i).r0 = r0
               est_match(i).phi0 = phi0
               est_match(i).z0 = z0
               est_match(i).psi = psi
               est_match(i).invp =1./(rh*abs(10.*b_magn*q_ch*C_D_CURVATURE))
               est_match(i).tanl = tanl
            endif              
            
             

C..............SAVING THE REFITTER FLAG     
            jjj=1
            do while (jjj.lt.8.and.est_match(i).rflag(jjj).ne.0)
               jjj = jjj+1
            enddo
            est_match(i).rflag(jjj)=gtrk(i).flag
         endif
      enddo

      if (est_ctrl(1).ktrl1.eq.1) then
         ntot=0
         nf1=0
         nf0=0
         nfm1=0
         nfm2=0
         nfm3=0
         nfm4=0
         nfm5=0
         nfm6=0
         nfm7=0
         do i=1,est_match_h.nok 
            ntot=ntot+1
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
         write(6,*)'total number of tracks =',ntot
         write(6, *)'flag        1      0     -1     -2     -3     -4     -5     -6     -7'
         write(6,75)'number ',nf1,nf0,nfm1,nfm2,nfm3,nfm4,nfm5,nfm6,nfm7
 75      format(a7,9I7)
         write(6,*)'Refitter finished.'
      endif
      return
      end


