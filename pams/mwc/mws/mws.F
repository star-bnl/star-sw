CC:>--------------------------------------------------------------------
CC: FILE:       mws.F
CC: HISTORY:
CC:               feb97-v000a-jpw- First version for STAF
CC:               jan98-v001a-ppy- Call to geometry module eliminated
CC:               jan99-v002a-der- ROOT ready/change params
CC:
CC                jun00-      Vladimir Morozov - slow simulation 
CC:<--------------------------------------------------------------------

      INTEGER*4 FUNCTION MWS(
     1              mhit_h,              mhit ,
     3              geom_h,              geom ,
     5              mpar_h,              mpar ,
     6            mevent_h,            mevent ,
     7            sector_h,            sector ,
     8               raw_h,               raw , 
     9              pars_h,              pars ) 
      IMPLICIT NONE
#include "mws.inc"
CC:>--------------------------------------------------------------------
CC: ROUTINE:    MWS
CC: DESCRIPTION: Simulation package for MWC 
CC:              where both hit counting
CC:              and charge integration can be performed.             
CC:  
CC:             Please edit comments and code.
CC: AUTHOR:     jpw - James Whitfield whitfield@cmchem.chem.cmu.edu
CC:             der - Dan Russ dan.russ@cmu.edu
CC:                   Vladimir Morozov vlmrz@sseos.lbl.gov
CC: ARGUMENTS:
CC:          IN:
CC:       INOUT:
CC:             mhit     - geant hit table for mwc
CC:             geom     - geometry definition table for MWC
CC:            mpar     - input parameters for simulation
CC:           mevent     - event table for simulation
CC:           sector     - sector results
CC:              raw     - count output output table for MWC sim
CC:             pars     - new parameters table.
CC:         OUT:
CC:>------------------------------------------------------------------
      INTEGER MWG
      INTEGER Mwc_raw_len
      PARAMETER (Mwc_raw_len=8000) 
      INTEGER Ihit, Isec, Id, ieta        
      INTEGER Ir, Iphi, Index, Num_hits
      INTEGER Nsignal, Nhit(Mwc_raw_len)
      INTEGER num_wires_sect, num_wires_count, num_counts_sect, num_counts_out
      INTEGER icell, cell_num, cell_hit(0:10,7680)
      INTEGER se_hit(0:10,96),overload_slat, num_below_thresh
      INTEGER hit_index, ee_correct, numevs
      INTEGER isect, iosect, i, j, jj, ipie,tot_cell_hit,hit_tot
      REAL    Tolerance, Pi, se_de(96),vshift(0:595)
      REAL    Rhit, Phihit, Phi_step
      REAL    Scale, RG32, Dummy, Ped
      REAL    Total_de(Mwc_raw_len), gain
      REAL    outsect_ir, insect_ir, wire_step
      REAL    phicent, phicos, sector_ir
      REAL    local_hit, local_step, cell_de(7680)
      LOGICAL First
      SAVE    Tolerance
      SAVE    First      
      SAVE    Pi
*--
      DATA    Tolerance / -0.1 /
      DATA    First     / .true. /
      DATA    Pi        / 3.141592654 /

      INTEGER trg(0:10)
      REAL    y_pos, fraction
      REAL xlocal, plocal
      REAL drift1, drift2, mip_deds, vgain, threshold
      INTEGER igain, ogain, tr,delay,save_cell,tof,counter
      COMMON /deposition/ drift1,drift2,igain,ogain,
     * vgain,tr,vshift
      INTEGER rhic_delay,sign
      REAL r1min,r1max,r2min,r2max,noise_width



      COMMON /mwc_ss/ insect_ir, r1max,r2min,wire_step
*--
*--    Default this works
*--
      MWS = STAFCV_OK
*--   
     


*--- Time is split into 11 buckets, each has a variable trg(1),2,3,4 etc
*--- for starters, trg = 0, then they are incremented by 1 in the 
*--- subroutine deposition, if voltage > thereshold in that time bucket
*--- this is repeated for every hit in the event
*--- trg - is a "local" variable for each of the many hits in event, trg = 0 or 1

      if ( First ) then
         First   = .false.
      numevs = 0
*-- 
*--    Set the gain for output from MWC
*--
         if (mpar(1).gain .eq. 0.0)  then
            write (6,*) 'MWS:WARNING Gains not set.Setting gain to 1.0!'
            mpar(1).gain = 1.0
         endif
*--
*--      Let's set some constants !!!
*--      outsect_ir     :  outer sector radius
*--      insect_ir      :  inner sector radius
*--      wire_step      : distance between wires in cm
*--      num_wires_sect : Number of wires per sector (inner or outer)
*--      num_counts_out : Number of count values for MWC
*--      num_wires_count: Number of wires used per count
*--      num_counts_sect: Number of count values per sector (inner or outer)
*--
CC:      let us define some constants here - in real life they must come from the database, 
CC:      but in the absence thereof, we should provide these values in the code


         ogain = pars(1).ogain
         igain =  pars(1).igain
         vgain =  pars(1).vgain
         drift1 =  pars(1).drift1
         drift2 =  pars(1).drift2
         tr =  pars(1).tr
         noise_width = mpar(1).el_noise_width
         mip_deds =  pars(1).mip_deds
         threshold =  pars(1).threshold
         delay =  pars(1).delay

         r1max = geom(1).r1max
         r1min = geom(1).r1min
         r2max = geom(1).r2max
         r2min = geom(1).r2min
         outsect_ir       = r2min
         insect_ir        = r1min
         wire_step        = 0.4
         num_wires_sect   = 160
         rhic_delay = delay/109 ! RHIC strobes 


CC:      end database constants

         num_counts_out   = mpar(1).num_counts_out
         num_wires_count  = mpar(1).num_wires_count
         num_counts_sect  = num_wires_sect/num_wires_count
*--
*--    Check that the endcap wire segments have been set correctly
*--

        ee_correct = abs(7680 - num_counts_out * num_wires_count)
        if (ee_correct .gt. 0) then
           write(6,*)'MWS: ERROR GSTAR geometry there must be 7680 wires'
           MWS = STAFCV_BAD
           return
        endif
  
*--
*--    Check that geometry set correctly
*--
         if ( Geom(1).neta .eq. 0 ) then
            write (6,*) 'MWS: Geometry NOT DEFINED '
            MWS = STAFCV_BAD
            return
         endif
*--
*--   Get sector length in phi
*--
         if ( Geom(1).nphi .gt. 0 ) then
            Phi_step  = 2 * Pi / Geom(1).nphi
         else
            write (6,*) 'MWS: Geometry not initialized '
            MWS = STAFCV_BAD
            return
         endif
      endif 
*--
      call VZERO ( Total_de, Mwc_raw_len )
      call VZERO ( Nhit    , Mwc_raw_len )
*--
*--    Zero event hit counter
*--
      Mevent(1).no_hits = 0
      overload_slat = 0
      num_below_thresh = 0

      do j = 1,num_counts_out*num_wires_count

         do jj = 0,10            
          cell_hit(jj,j) = 0 ! 1 cell = 1 wire
         enddo
         cell_de(j)  = 0

      enddo

*--    Check if any data
      if ( mhit_h.nok .gt. 0 ) then
      tot_cell_hit = 0
      hit_tot = 0
 
      ihit = 1
 29   continue
      do j= 0,595
       vshift(j) = 0
      enddo
      do j=0,10
       trg(j) = 0
      enddo
 19   continue

*--    Loop over hits
            cell_num = mhit(ihit).volume_id
            tof = int(mhit(ihit).tof*1E+09) ! delay due to tof in ns units
            delay = rhic_delay*109 + tof
*--
            Rhit   = sqrt(Mhit(Ihit).x(1)**2+Mhit(Ihit).x(2)**2)
            if (cell_num.gt.0)then

               Mevent(1).no_hits = Mevent(1).no_hits + 1
               if (Ihit .eq. 1 .or. 
     &          Mhit(Ihit).track_p .ne. Mhit(Ihit-1).track_p) then
                  Mevent(1).no_tracks = Mevent(1).no_tracks + 1
               endif 

*--
*--            Determine whether hit was in inner or outer sector
*--
               if (rhit .lt. 120.0) then
                iosect = 1
               else 
                iosect = 2
               endif

C     VM: now let's do voltage simulation for this hit (hit is one occurence of
C  energy deposition in one cell), there may be more than one hit in one cell, due to 
C  a passage of several particles thru one cell, and there may be more than one 
C  hit from one particle - i.e. a particale goes thru more than 1 cell

*-- VM: now we want to know how much more energy was deposited in this hit, compared to a MIP
               
               fraction = (mhit(ihit).de/mhit(ihit).ds)/(mip_deds)  
               if(iosect.eq.1)then
                y_pos=10*(208.6-abs(mhit(ihit).x(3)))+5
               endif
               if(iosect.eq.2)then
                y_pos=10*(208.6-abs(mhit(ihit).x(3)))+7
               endif

               sign = 1
               if (mhit(ihit).x(3).gt.0) then
                sign = -1
               endif

               call xploc(xlocal,mhit(ihit).x(1),mhit(ihit).x(2),
     * plocal,mhit(ihit).p(1),mhit(ihit).p(2))       

               call deposit (10*xlocal,y_pos,
     * plocal,sign*mhit(ihit).p(3),fraction,delay,noise_width)


CC: VM - insert this for testing purposes

C      do jj=0,10
C      if (trg(jj).gt.0) then
C       write(99,*)cell_num,mhit(ihit).volume_id
C      write(*,93)mhit(ihit).de,
C     * sqrt(Mhit(Ihit).x(1)**2+Mhit(Ihit).x(2)**2),mhit(ihit).tof
C     * ,mhit(ihit).id,mhit(ihit).track_p,mhit(ihit).volume_id,
C     * cell_num,jj
C      endif
C      enddo
C 93   FORMAT(E10.3,1X,E10.3,1X,E10.3,1X,I2,1X,I2,1X,I4,1X,I4,1X,I2)


            endif ! end if this hit is OK (if mhit(ihit).cell_num.ne.0 )

            if (ihit.lt.mhit_h.nok) then
             ihit = ihit + 1
             if (mhit(ihit).volume_id.eq.cell_num) then 
              goto 19
             else
C   VM: this is the main change I'm intruducing - a cell is hit if voltage generated in this cell exceeds threshold voltage.
C   cell_hit(cell_num) is incremented at most by 1 in one particle hit but in principle, we may have cell_hit > 1
              call trigger(trg,threshold,vshift)
              do jj =0,10                  
               cell_hit(jj,cell_num)=cell_hit(jj,cell_num)+trg(jj)
              enddo
              cell_de(cell_num)  = cell_de(cell_num) + mhit(ihit-1).de 
              goto 29
             endif
            else
             call trigger(trg,threshold,vshift)
             do jj =0,10                  
              cell_hit(jj,cell_num)=cell_hit(jj,cell_num)+trg(jj)
             enddo
             cell_de(cell_num)  = cell_de(cell_num) + mhit(ihit).de 
            endif
           
          

*--    end loop over hits
*--

       Mevent_h.nok = 1
       do i = 1,96  
          do jj =0 , 10 
           se_hit(jj,i) = 0
          enddo
          se_de(i)  = 0
       enddo
*--
*--    Loop over sectors now 
*--
         counter = 0
         Nsignal = 0
         do Isec = 1, num_counts_out
           do j = 1, num_wires_count
             hit_index = (isec-1)*num_wires_count + j
             do jj = 0,10
             se_hit(jj,isec)  = se_hit(jj,isec) + cell_hit(jj,hit_index)
             enddo
             se_de(isec)   = se_de(isec)  + cell_de(hit_index)
            enddo

*--
*--   Changed the number of eta segments to 2*num_counts_sect (# of sets of
*--   wires in an outer AND inner sector) from 16.  16 is the value only if 
*--   we look at the wires in sets of 20.  Also, subtracting 1 from the 
*--   sector number so that (1 through 4) become (0 through 3) and goto phi=1.
*--   I think it's neater than subtracting 0.01 so that (3.99/4)=0 
*--                                              -d.e. russ 1/27/99
*--
*--             iphi = int((float(isec)/16.0)-0.01) +  1
*--             ieta = isec - ((iphi-1) * 16)
*--
*--             if (iphi .gt. 12) iphi = iphi - 12
*--
           
           iphi = int ( (float(isec-1)) / (2*num_counts_sect) ) + 1
           ieta = isec - ((iphi - 1) * ( 2*num_counts_sect ) )
           if (iphi .gt. 12) iphi = iphi - 12

CC: we will write mwc_raw table with 96x11 pairs of values, even if some of hit counts will be zeroz
CC: to this end, I delete the following condition:

CC:           if (se_de(Isec) .gt. 0.) then
               nsignal = nsignal + 1 ! this way nsignal just indexes the sector number
*--
               sector(nsignal).iphi    = iphi
               sector(nsignal).ieta    = ieta
               sector(nsignal).sector  = isec
               tot_cell_hit = 0
               sector(nsignal).nhit    = se_hit(7,isec)
               sector(nsignal).de      = se_de(isec)
*-- 

               do jj = 0,10
                raw(nsignal + 96*jj).sector = isec
                raw(nsignal + 96*jj).count = se_hit(jj,isec) 
               enddo
CC:           endif
         enddo
     

*--    Define the number of slats with raw data
*--
         Raw_h.nok    = Nsignal*11
         Sector_h.nok = Nsignal
*--
*--    end if any data
*--
   
      endif 
*--
*--    That's It
*--
  990 continue
      end


*---------------------------------------------------------------
       subroutine deposit(x0,y0,v_x,v_y,
     * fraction,delay,noise)
       real q_x(-70:70), q_y(-70:70), delt, factor, 
     * v_x,v_y, x0, y0,x_wall1, x_wall2,
     * y_wall1, y_wall2, x,y,
     * y_ground, y_anode,
     * t(-70:70),drift1,drift2,vgain,
     * q1(0:200),gain,deltat
       integer n, helper, j, num,num_tot,
     * jj, delay,igain,ogain,tr
C  This program calculates the voltage output from the MWPC wires,
C  we want to get 24mV per 6750 electrons - V. Morozov      
      real sum1(1000),total,noise,elnoise
      DIMENSION VOUT(800)
      real vshift(0:595) ! voltage output shifted by signal delay
      
      COMMON /deposition/ drift1,drift2,igain,
     * ogain,vgain,tr,vshift


       factor = 0
       helper = iosect     
       
       if (helper.eq.1) then 
       goto 200
       else
       goto 100
       end if

  100  continue
       x_wall1 = 2.0
       x_wall2 = -2.0
       y_wall1 = 7.0
       y_wall2 = -7.0
       y_anode = -3.0
       y_ground = 1.0
       gain =ogain
       goto 300
  200  continue
       x_wall1 = 2.0
       x_wall2 = -2.0
       y_wall1 = 5.0
       y_wall2 = -5.0
       y_anode = -3.0
       y_ground = -1.0
       gain = igain
       goto 300

 300   continue
CC     put everything equal to zero

       do 298 nk = -70,70
       q_x(nk) = 0
       q_y(nk) = 0
       t(nk) = 2000
 298   continue

       do 299 nk = 0, 800
       vout(nk)=0
       q1(nk)=0
 299   continue

       n = 0
       x = x0
       y = y0
       delt = .17/(sqrt(v_x*v_x +v_y*v_y))! we assume a MIP deposits ionization once every .17mm along it's track
C      now we propagate the particle thru the cell
       
       if(v_y.ge.0) then
        v_y = -1*v_y
        v_x = -1*v_x
       endif

       if (v_x.gt.0) then     
 11     if ((x.le.x_wall1).and.(y.ge.y_wall2)) then
         q_x(N) = x
         q_y(N) = y
         x = x + delt*v_x
         y = y + delt*v_y
         N = N +1
         if (v_x*v_y.eq.0)then
          print*,' !!!!!!!! MWC ERROR !!!!!!!!'
         endif
         goto 11
        endif
      
        n=0   
        x = x0
        y = y0

 21     if ((x.ge.x_wall2).and.(y.le.y_wall1)) then 
         x = x - delt*v_x
         y = y - delt*v_y
         N = N-1
         q_x(N) = x
         q_y(N) = y
         if (v_x*v_y.eq.0)then
         print*,' !!!!!!!! MWC ERROR !!!!!!!!'
         endif
     
         goto 21
        endif

       else ! if v_x < 0

 14     if ((x.le.x_wall1).and.(y.le.y_wall1)) then
         q_x(N) = x
         q_y(N) = y
         x = x - delt*v_x
         y = y - delt*v_y
         N = N +1
         if (v_x*v_y.eq.0)then
          print*,' !!!!!!!! MWC ERROR !!!!!!!!'
         endif

        goto 14
        endif
      
        n=0
        x = x0
        y = y0

 24     if ((x.ge.x_wall2).and.(y.ge.y_wall2)) then 
         x = x + delt*v_x
         y = y + delt*v_y
         N = N-1
         q_x(N) = x
         q_y(N) = y
         if (v_x*v_y.eq.0)then
          print*,' !!!!!!!! MWC ERROR !!!!!!!!'
         endif
         goto 24
        endif
       
       endif
  

C      now we will calculate drift times for each electron
C      from it's original position (q_x(n),q_y(n)) to anode
C      we will assume velosity_drift1 = .04969 mm/ns from the gating gr. to ground
C      and velocity drift2 = .025 mm/ns from the ground to anode 
C      (neglect sharp increase of v close to anodes)
C      for electron with x > 1mm use "factor" to account for curving track
       
       num_tot = 0
       do 500 n = -70,70
          if ((q_x(n).ne.0).or.(q_y(n).ne.0)) then
C            num_tot = num_tot +1 
            if (abs(q_x(n)).ge.1) then
                factor = .2
             endif
             if (q_y(n).ge.y_ground) then
                t(n) = ((q_y(n)-y_ground)/drift1) + (sqrt(q_x(n)*q_x(n) 
     *               + (y_ground - y_anode)*(y_ground-y_anode))/drift2) 
     *               + factor*(abs(q_x(n))/drift2)
             else
             t(n)=(sqrt(q_x(n)*q_x(n)+(q_y(n)-y_anode)*(q_y(n)-y_anode))
     *               /drift2)+factor*(abs(q_x(n))/drift2)
             endif
             
          endif
 500   continue
C       print*,'MWC: obtained # pairs: ',num_tot*fraction


C      now we want to add all these arrival times t(n) into q1(j)
C      q1(j)- how much charge arrives on anode within 2 ns at time j

       do 600 n = -70,70
       if (q_x(n).ne.0.or.q_y(n).ne.0) then
       num = t(n)/2
       q1(num) = q1(num) + 1
       endif
 600   continue

C this increases the electron count by one every time there is 
C anything at all reaching the wire in that time bucket
C if more than one e's reach the wire, we miss them.
C it doesn't happen often, though
       
       do j = 0,200
       q1(j) = gain*q1(j)*fraction
       enddo

      NTIME=200

      DO 40 J=8,4*NTIME,4
         DO 50 K=4,J,4
            DT=FLOAT(J-K)
            VOUT(J)=VOUT(J)+DELTAT(DT,vgain,tr)*Q1(nint(K/4.0))
 50      CONTINUE
 40   CONTINUE
       


C      VM: now let's digitize this V(t) curve, one timebucket = 109 ns
C      but first we have to shift time by the amount of signal delay


       do 565 jj = 1,594  ! jj - time in 2ns, spans 11 time buckets
        if(jj*2.ge.delay) then
         if (((jj-delay/2)*4 +4).lt.800) then 
          vshift(jj) = vshift(jj)+vout((jj-delay/2)*4 + 4)
         endif
        endif
 565   continue    

       return
       stop
       end
  

CC: ------------------------------------------------------
 
      subroutine trigger(trg, threshold,vshift)
      integer trg(0:10),jj,j
      real vshift(0:595),threshold,v_peak(0:10)
    
       do jj= 0,10              ! jj - timebucket number
          v_peak(jj) = vshift(1+54*jj)
          do J=1+54*jj,54*(jj+1)
             if (vshift(j+1).gt.vshift(j)) then
                v_peak(jj) = vshift(j+1)
             endif
          enddo

CC add electronic noise, if needed
          if (noise.gt.0) then
           v_peak(jj)= elnoise(v_peak(jj),noise)
          endif

          if (jj.gt.0) then
           if(v_peak(jj).ge.threshold.and.v_peak(jj-1).lt.threshold)then
                trg(jj) = 1
             endif
          elseif(jj.eq.0) then
             if (v_peak(0).gt.threshold) then
                trg(0) = 1
             endif
          endif
       enddo

C      VM: flip-flop mechanism is on - if the voltage EVER goes above threshold, 
C      trigger will be in that time bin and that time bin only

               
       return
       stop
       end

       
         FUNCTION DELTAT(DT,vgain,tr)
         integer tr
         TPZ=500.
         DELTAT=((DT/2*TR)**2*EXP(-2*DT/TR))*vgain
C VM:have put the factor 2.75e-13 to make V_out(1electron) = (24/6750)mV
         TO=TR
         RAT=DT/TO
         RETURN
         END


      real function elnoise(mean,sigma)

*--   
*--  Return a value selected from a gaussian distribution
*--  Distribution clipped at mean +/- 3 sigma
*--

       integer seed/11787/
       real mean,sigma,rval1,rval2,prob,ret_val,pi,t,signal
       logical next

       pi=3.141592654
       next=.true.
       do while (next)
          rval1=3.0*(2.0*ran(seed)-1.0)   ! random number -3 -> +3
          rval2=ran(seed)                 ! probablity
          ret_val=mean+rval1*sigma
          t=(ret_val-mean)/sigma
          prob=( 1./(sigma*sqrt(2.0*pi)) )*exp(-0.5*t*t) ! normalized gaussian
          if (rval2.lt.prob) next=.false.
       enddo
       elnoise=ret_val
       return
       end

CC------------------------------------------------------------
CC here we write a subroutine to calculate the position of the hit with respect to the cell,
CC if we know the lab frame coordinates of the hit.
      SUBROUTINE XPLOC (xlocal, x,y,plocal,px,py)
      REAL xlocal,x,y,plocal, px,py
      INTEGER i,imax
      REAL max,angle	
      COMMON /mwc_ss/ r1min, r1max,r2min,step
      PARAMETER (angle =.523598775598)! pi/6 = 30 degrees
 
      imax = 1
      max = -200
    
      do 10 i = 1, 12
      if((x*sin(angle*i)+y*cos(angle*i)).ge.max) then
      imax = i
      max = x*sin(angle*i)+y*cos(angle*i)
      endif
 10   continue
CC now we need to convert max into the coordinate with respect to a cell

      if (max.lt.r1max) then
      xlocal = max - r1min
      i = 1
      else
      xlocal = max - r2min
      i = 161
      endif
CC    this part will check if xlocal is within (-2,+2)mm

      xlocal = xlocal - int(xlocal/step)*step- step/2
      plocal =  px*sin(angle*imax)+py*cos(angle*imax)
      return


      end




















