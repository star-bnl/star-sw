cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C
C  John F. Novak
C    Sunday, October 29, 2012, 7:27 pm 
C    Huricane Sandy should be getting ready to pound NYC and BNL right
C    about now. Lets hope the power doesn't cut out and that there is
C    a lab that still wants this in the morning. I'm in Michigan, btw.
C
C  This file is a modified copy of urqmd.f (renamed urqmd.F)
C    Our frame work requires the function calls for event generation
C    to be done from within the StarUrQMD library. "Vanilla" UrQMD is
C    run as a standalone program which accepts and returns text files
C    defined by enviormental variables. Instead of using these files,
C    we are intercepting the values in the common blocks. Also, 
C    instead of attempting to recreate the functionality of urqmd.F 
C    in the StarUrQMD library, I decided to rewrap urqmd.F into a few
C    independent functions that can be called from StarUrQMD.
C
C  8:24pm- after much deliberation I have decided to use the input/
C    output files convention. It would require too much reworking of 
C    the code to remove it, and this code needs to be relatively easy
C    to modify if a new version of UrQMD comes out. What we are going
C    to do is have StarUrQMD generate a temp input file for the code
C    to read in, then pull the event data off the comment blocks and 
C    let it spam the directory with unused output files. They may be
C    usefull for debugging, and if they are a problem we can put in a
C    few lines to erase them all.
C
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc


cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C
C    iurqmd() : InitUrQMD
C
C    Initialization function
C
C    This function initalizes the thigns UrQMD needs. Mostly it calls
C    uinit() and sets a few values
C
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

      integer function iurqmd()

      implicit none
#include "coms.inc"
#include "comres.inc"
#include "options.inc"
#include "colltab.inc"
#include "inputs.inc"
#include "newpart.inc"
#include "boxinc.inc"
c
      integer i,j,k,steps,ii,ocharge,ncharge, mc, mp, noc, it1,it2
      real*8 sqrts,otime,xdummy,st
      logical isstable
      integer stidx,CTOsave
      real*8 Ekinbar, Ekinmes, ESky2, ESky3, EYuk, ECb, EPau
      common /energies/ Ekinbar, Ekinmes, ESky2, ESky3, EYuk, ECb, EPau
      integer cti1sav,cti2sav
chp hydro variables
      real*8 thydro_start,thydro,nucrad
      logical lhydro

C JFN 10/29/12 8:31pm- Custom common block for things I broke by 
C   functionizing the code
      common /custom/ mc, mp, noc
c
c     numerical/technical initialisation
c
      call uinit(0)
      call osc_header
      call osc99_header
c
c  Main program
c

      mc=0
      mp=0
      noc=0

      return
      End


cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C
C    genevt() : GenerateEvent
C
C    This fucntion generates each UrQMD event
C
C    In the vanilla UrQMD code, everything in this function is
C    is contained in a loop which loops as many times as events are 
C    asked for
C
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

      integer function genevt()

      implicit none
#include "coms.inc"
#include "comres.inc"
#include "options.inc"
#include "colltab.inc"
#include "inputs.inc"
#include "newpart.inc"
#include "boxinc.inc"
c
      integer i,j,k,steps,ii,ocharge,ncharge, mc, mp, noc, it1,it2
      real*8 sqrts,otime,xdummy,st
      logical isstable
      integer stidx,CTOsave
      real*8 Ekinbar, Ekinmes, ESky2, ESky3, EYuk, ECb, EPau
      common /energies/ Ekinbar, Ekinmes, ESky2, ESky3, EYuk, ECb, EPau
      integer cti1sav,cti2sav
chp hydro variables
      real*8 thydro_start,thydro,nucrad
      logical lhydro
      common /custom/ mc, mp, noc

c
c     start event here
c    

c     time is the system time at the BEGINNING of every timestep
      time = 0.0
chp hydro flag, hydro should be called only once
      lhydro=.true.

C JFN 10/29/12 8:33pm- any references to setting up the random number generator
C or the rand seeds can be blocked out because we are using StarRandom
Cc     initialize random number generator
Cc     call auto-seed generator only for first event and if no seed was fixed
C      if(.not.firstseed.and.(.not.fixedseed)) then
C         ranseed=-(1*abs(ranseed))
C         call sseed(ranseed)
C      else
C         firstseed=.false.
C      endif
C
C      write(6,*)'event# ',event,ranseed

c
c     initialisation of physics quantities 
c
      call init

chp hydro switch      
      if (CToption(45).eq.1)then
chp hydro start time (nuclei have passed each other)
chp ebeam is only the kinetic energy 
chp CTParam(65) is useful for the variation of the start time 
chp default value is one
       thydro_start=CTParam(65)*2.d0*nucrad(Ap)*sqrt(2.d0*emnuc/ebeam)
       write(*,*) 'thydro_start is:', thydro_start
chp lower limit for hydro start time
       if(thydro_start.lt.CTParam(63)) then
        thydro_start=CTParam(63)
        write(6,*) 'thydro_start is set to:',CTParam(63)
       end if
      end if

c old time if an old fort.14 is used 
      if(CTOption(40).eq.1)time=acttime

c output preparation

c write headers to file
      call output(13)
      call output(14)
      call output(15)
      call output(16)
      if(event.eq.1)call output(17)
      call osc99_event(-1)


c for CTOption(4)=1 : output of initialization configuration
      if(CTOption(4).eq.1)call file14out(0)

c     participant/spectator model:
      if(CTOption(28).ne.0) call rmspec(0.5d0*bimp,-(0.5d0*bimp))

c     compute time of output
      otime = outsteps*dtimestep
      
c reset time step counter
      steps = 0

c  loop over all timesteps

      do 20  steps=1,nsteps

c store coordinates in arrays with *_t
c this is needed for MD type propagation
         if (eos.ne.0) then
            do 23 j=1,npart
               r0_t(j) = r0(j)
               rx_t(j) = rx(j)
               ry_t(j) = ry(j)
               rz_t(j) = rz(j)
 23         continue
         end if

c we are at the beginning of the timestep, set current time (acttime) 
         acttime = time
c  option for MD without collision term
         if(CTOption(16).ne.0) goto 103

c  Load collision table with next collisions in current timestep
         call colload
c     check for collisions in time-step, nct = # of collisions in table
         if (nct.gt.0) then

c     entry-point for collision loop in case of full colload after every coll. 
 101        continue
            k = 0
c     normal entry-point for collision loop 
 100        continue
c     get next collision
            call getnext(k)

c     exit collision loop if no collisions are left
            if (k.eq.0) goto 102
chp call hydro if start time is reached
            if(CTOption(45).eq.1)then
           
             if(cttime(k).gt.thydro_start.and.lhydro)then
              st=thydro_start-acttime
              call cascstep(acttime,st)
chp all particle arrays will be modified by hydro
              call hydro(thydro_start,thydro)
              acttime=thydro_start
              lhydro=.false.
              if(thydro.gt.1.d-8.or.CTOption(48).eq.1)then
chp full update of collision table
              call colload
                      
              go to 101
              end if
             end if    
            end if 

c  propagate all particles to next collision time
c  store actual time in acttime, propagation time st=cttime(k)-acttime
	    st=cttime(k)-acttime
            call cascstep(acttime,st)
c  new actual time (for upcoming collision)
            acttime = cttime(k)

c  perform collision 

            if(cti2(k).gt.0.and.
     .           abs(sqrts(cti1(k),cti2(k))-ctsqrts(k)).gt.1d-3)then
               write(6,*)' ***(E) wrong collision update (col) ***'
               write(6,*)cti1(k),cti2(k),
     .              ctsqrts(k),sqrts(cti1(k),cti2(k))
            else if(cti2(k).eq.0.and.
     .              abs(fmass(cti1(k))-ctsqrts(k)).gt.1d-3) then
               write(6,*)' *** main(W) wrong collision update (decay)'
               write(6,*)ctag,cti1(k),ityp(cti1(k)),dectime(cti1(k)),
     .              fmass(cti1(k)),ctsqrts(k)
            endif

            ocharge=charge(cti1(k))
            if(cti2(k).gt.0) ocharge=ocharge+charge(cti2(k))

c     store quantities in local variables for charge conservation check
            it1= ityp(cti1(k))
            if(cti2(k).gt.0)it2= ityp(cti2(k))

c increment "dirty" collision counter
            if(cti2(k).gt.0)then !scatter
               mc=mc+1
            endif
c     perform scattering/decay
            cti1sav = cti1(k)               
            cti2sav = cti2(k)    
            call scatter(cti1(k),cti2(k),ctsigtot(k),ctsqrts(k),
     &                   ctcolfluc(k))

c
c  update collision table 
c
c     normal update mode
            if(CTOption(17).eq.0) then
               if(nexit.eq.0) then
c     new collision partners for pauli-blocked states (nexit=0)
                  if (cti1(k).ne.cti1sav.or.cti2(k).ne.cti2sav) then
                   cti1(k) = cti1sav 
                   cti2(k) = cti2sav 
                  endif
                  call collupd(cti1(k),1)
                  if(cti2(k).gt.0) call collupd(cti2(k),1)
               else
                  ncharge=0
c     new collision partners for scattered/produced particles (nexit><0)
                  do 30 i=1,nexit
c     ncharge is used for charge conservation check
                     ncharge=ncharge+charge(inew(i))
                     call collupd(inew(i),1)
 30               continue

c     charge conservation check
                  if(ocharge.ne.ncharge) then
                     write(6,*)'ch-conservation error coll/dec ',ctag
                     write(6,*)'   it1:',it1,'   it2:',it2
                     write(6,*)'   ch:',ocharge,ncharge
                     write(6,*)'cti1(k),cti2(k),ctsigtot(k),ctsqrts(k)'
                     write(6,*)cti1(k),cti2(k),ctsigtot(k),ctsqrts(k)
                  endif
               endif

c     update collisions for partners of annihilated particles
               do 55 ii=1,nsav
                  call collupd(ctsav(ii),1)
 55            continue
               nsav=0

            else ! (CTOption(17).ne.0)
c     full collision load
               call colload
            endif

            if (CTOption(17).eq.0) goto 100
            goto 101

c     this is the point to jump to after all collisions in the timestep
c     have been taken care of
 102        continue

         endif ! (nct.gt.0)

c  After all collisions in the timestep are done, propagate to end of 
c  the timestep.

c     point to jump to in case of MD without collision term
 103     continue

c     increment timestep
         time = time+dtimestep

c  After all collisions in the timestep are done, propagate to end of 
c  the timestep.
         call cascstep(acttime,time-acttime)

c     in case of potential interaction, do MD propagation step
         if (eos.ne.0) then

c set initial conditions for MD propagation-step
            do 24 j=1,npart
               r0(j) = r0_t(j)
               rx(j) = rx_t(j)
               ry(j) = ry_t(j)
               rz(j) = rz_t(j)
 24         continue

c now molecular dynamics trajectories
            call proprk(time,dtimestep)

         end if ! (eos.ne.0)

c     perform output if desired
         if(mod(steps,outsteps).eq.0.and.steps.lt.nsteps)then 
            if(CTOption(28).eq.2)call spectrans(otime)
            call file14out(steps)
         endif ! output handling

 20   continue ! time step loop


ce
	acttime=time
c optional decay of all unstable particles before final output
c DANGER: pauli-blocked decays are not performed !!!
         if(CTOption(18).eq.0) then
c no do-loop is used because npart changes in loop-structure
            i=0
            nct=0
            actcol=0
c disable Pauli-Blocker for final decays
	    CTOsave=CTOption(10)
            CTOption(10)=1
c decay loop structure starts here
 40         continue
            i=i+1

c is particle unstable
            if(dectime(i).lt.1.d30) then
 41            continue
               isstable = .false.
               do 44 stidx=1,nstable
                  if (ityp(i).eq.stabvec(stidx)) then
c                     write (6,*) 'no decay of particle ',ityp(i)
                     isstable = .true.
                  endif
 44            enddo
               if (.not.isstable) then
c     perform decay
                  call scatter(i,0,0.d0,fmass(i),xdummy)
c     backtracing if decay-product is unstable itself
                  if(dectime(i).lt.1.d30) goto 41
               endif
            endif
c     check next particle
            if(i.lt.npart) goto 40
         endif ! final decay
         CTOption(10)=CTOsave
c final output

	   if(CTOption(28).eq.2)call spectrans(otime)

         call file13out(nsteps)
         if(CTOption(50).eq.0)then
          call file14out(nsteps)
         end if
         call file16out
         if(CTOption(50).eq.0)then
          call osc_event
         end if
         call osc99_event(1)
         call osc99_eoe
      
         mp=mp+npart
         if(ctag.eq.0)then
           write(*,*)'(W) No collision in event ',event
           noc=noc+1
         endif

c     end of event loop
      return
      end

 

