CC:>--------------------------------------------------------------------
CC: FILE:       particle_dst_filler.F
CC: HISTORY:
CC:             25Aug1998- Created by Lanny Ray 
CC:<--------------------------------------------------------------------

      INTEGER*4 FUNCTION PARTICLE_DST_FILLER(
     1               par_h,               par ,
     2          particle_h,          particle ,
     3         dst_track_h,         dst_track ) 
      IMPLICIT NONE
#include "particle_dst_filler.inc"
CC:>--------------------------------------------------------------------
CC: ROUTINE:    PARTICLE_DST_FILLER
CC: DESCRIPTION:
CC:             DST level filler module that copies particles from
CC:             the 'particle' table into the dst_track table.
CC:             Momentum space acceptance cuts in {pt,eta,phi}
CC:             can be included; all particles or just the charged
CC:             particles can be transfered.
CC: 
CC: AUTHOR:    Lanny Ray, ray@physics.utexas.edu 
CC: ARGUMENTS:
CC:          IN:
CC:              par     - Parameters and controls
CC:            par_h     - Header Structure for par
CC:         particle     - Event generator output 'particle' table 
CC:       particle_h     - Header Structure for 'particle'
CC:       INOUT:
CC:         OUT:
CC:        dst_track     - DST level global track table
CC:      dst_track_h     - Header Structure for dst_track
CC: RETURNS:    STAF Condition Value
CC:>--------------------------------------------------------------------

CC:  Local Variable Type Declarations:

      integer*4  i
      integer*2  charge_short

      real*4  pt,phi,eta,pi,rad,charge,icharge
      real*4  px,py,pz,E,theta,tanl,invpt,pcut

      parameter(pi = 3.141592654)
      parameter(pcut = 0.000001)

CC:  Set constants:

      dst_track_h.nok = 0
      rad = 180.0/pi

CC:  Start loop thru all rows in table 'particle':

      IF(particle_h.nok .gt. 0 .and. dst_track_h.maxlen .gt. 0) THEN
         do i = 1,particle_h.nok
            if(particle(i).isthep .eq. 1 .and. particle(i).idhep
     1         .lt. 100000) then
               
               px = particle(i).phep(1)
               py = particle(i).phep(2)
               pz = particle(i).phep(3)
               E  = particle(i).phep(4)
               pt = sqrt(px*px + py*py)

CC:  Compute azimuthal angle>>

               if(pt .le. pcut) then
                  phi = 0.0
               else if(pt.gt.pcut .and. abs(py).le.pcut .and.
     1                 px.lt.0.0) then
                  phi = pi
               else
                  phi = atan2(py,px)
               end if
               if(phi .lt. 0.0) phi = phi + 2.0*pi
               phi = phi*rad

CC:  Compute pseudorapidity>>

               if(pt.le.pcut .and. abs(pz).le.pcut) then
                  eta = 0.0
               else if(pt.le.pcut .and. abs(pz).gt.pcut) then
                  eta = 0.5*log((E+pz)/(E-pz))  ! Use beam rapidity
               else
                  theta = atan2(pt,pz)
                  eta = -log(tan(theta/2.0))
               end if

CC:  Determine if particle is within the kinematic acceptance>>

               if(pt.ge.par(1).pt_min  .and. pt.le.par(1).pt_max
     1     .and. phi.ge.par(1).phi_min .and. phi.le.par(1).phi_max
     2     .and. eta.ge.par(1).eta_min .and. eta.le.par(1).eta_max)
     3         then

CC:  Determine the charge of the particle>>

               charge = icharge(particle(i).idhep)
               charge_short = int(charge)

CC:  And include it if charge.ne.0 OR if par(1).switch_neutrals = 1

               if(abs(charge).ge.0.001 .or. (abs(charge).lt.0.001
     1            .and. par(1).switch_neutrals .eq.1)) then

CC:  Compute tanl = pz/pt>>

                  if(pt .le. pcut) then
                     if(abs(pz) .le. pcut) then
                        tanl = 0.0
                     else if(pz .gt. pcut) then
                         tanl = 9999.
                     else if(pz .lt. (-pcut)) then
                         tanl = -9999.
                     end if
                  else
                     tanl = pz/pt
                  end if

CC:  Compute inverse pt:>>

                  if(pt .le. pcut) then
                     invpt = 9999.
                  else
                     invpt = 1.0/pt
                  end if

CC:  Load/Fill dst_track and increment nok value:

               if(dst_track_h.nok .lt. dst_track_h.maxlen) then
                dst_track_h.nok = dst_track_h.nok + 1
                dst_track(dst_track_h.nok).id = dst_track_h.nok
                dst_track(dst_track_h.nok).iflag       = 0
                dst_track(dst_track_h.nok).det_id      = 0
                dst_track(dst_track_h.nok).n_point     = 0
                dst_track(dst_track_h.nok).n_max_point = 0
                dst_track(dst_track_h.nok).n_fit_point = 0    
                dst_track(dst_track_h.nok).icharge = charge_short
                dst_track(dst_track_h.nok).x0 = 0.1*particle(i).vhep(1)
                dst_track(dst_track_h.nok).y0 = 0.1*particle(i).vhep(2)
                dst_track(dst_track_h.nok).z0 = 0.1*particle(i).vhep(3)
                dst_track(dst_track_h.nok).psi           = phi
                dst_track(dst_track_h.nok).tanl          = tanl
                dst_track(dst_track_h.nok).invpt         = invpt
                dst_track(dst_track_h.nok).covar_diag(1) = 0.0
                dst_track(dst_track_h.nok).covar_diag(2) = 0.0
                dst_track(dst_track_h.nok).covar_diag(3) = 0.0
                dst_track(dst_track_h.nok).covar_diag(4) = 0.0
                dst_track(dst_track_h.nok).covar_diag(5) = 0.0
                dst_track(dst_track_h.nok).chisq(1)      = 0.0
                dst_track(dst_track_h.nok).chisq(2)      = 0.0
                dst_track(dst_track_h.nok).x_first(1)    = 0.0
                dst_track(dst_track_h.nok).x_first(2)    = 0.0
                dst_track(dst_track_h.nok).x_first(3)    = 0.0
                dst_track(dst_track_h.nok).x_last(1)     = 0.0
                dst_track(dst_track_h.nok).x_last(2)     = 0.0
                dst_track(dst_track_h.nok).x_last(3)     = 0.0
                dst_track(dst_track_h.nok).length        = 0.0
                dst_track(dst_track_h.nok).impact        = 0.0
                dst_track(dst_track_h.nok).ndegf         = 0
                dst_track(dst_track_h.nok).id_global_pid = 0
                dst_track(dst_track_h.nok).id_hypo_pid   = 0
                dst_track(dst_track_h.nok).id_start_vertex = 0
                dst_track(dst_track_h.nok).id_stop_vertex = 0
                dst_track(dst_track_h.nok).id_emc         = 0
                dst_track(dst_track_h.nok).id_smd         = 0
               else
                  write(6,20) dst_track_h.nok
20                Format(2x,'No. DST TRACK entries .ge. maximum')
                  PARTICLE_DST_FILLER = STAFCV_BAD
                  Return
               end if

               end if      !  Charge particle option
               end if      !  Momentum acceptance cut option
            end if         !  Propagating particles selection option
         end do            !  End particle loop thru table 'particle'
      ELSE
         write(6,10) particle_h.nok, dst_track_h.maxlen
10       Format(2x,'particle_h.nok,dst_track_h.maxlen=',2I10,
     1          ' -BAD Values')
         PARTICLE_DST_FILLER = STAFCV_BAD
         Return 
      END IF               !  End # table rows adequacy check 

CC:  Successful completion of module

      PARTICLE_DST_FILLER = STAFCV_OK
      RETURN
      END

CC-------------------------------------------------------------------------

      REAL*4 FUNCTION icharge(idhep)
      implicit none

CC:  This Function returns the particle charge in units of |e| for the
CC:  Particle Data Group Standard particle ID numbering scheme as
CC:  described in:
CC:         T. G. Trippe and G. R. Lynch, "Particle ID Numbers, Decay
CC:         Tables, and Other Possible Contributions of the Particle
CC:         Data Group to Monte Carlo Standards,"  LBL-24287, in Proc.
CC:         of the Workshop on Detector Simulation for the SSC (August
CC:         1987).
CC:
CC:  INPUT:   idhep - PDG particle ID number
CC:  OUTPUT:  icharge = particle charge in units of |e|
CC:         Note:  Valid input ID #s result in correct values for
CC:                the returned charge.  Invalid input ID #s result
CC:                in unpredictable results.

CC:  Local Variable Type Declarations:

      integer*4 idhep,ievod,absid,sign
      integer*4 digit1,digit2,digit3,digit4,digit5

      real*4  quark_chrg(6), q1,q2,q3

CC:  Set and Determine constants and individual digits in the value
CC:  of 'idhep'

      absid = abs(idhep)
      if(idhep .ge. 0) sign = 1
      if(idhep .lt. 0) sign = -1

      quark_chrg(1) = -0.333333
      quark_chrg(2) =  0.666667
      quark_chrg(3) = -0.333333
      quark_chrg(4) =  0.666667
      quark_chrg(5) = -0.333333
      quark_chrg(6) =  0.666667

      digit5 =  absid/10000
      digit4 = (absid-10000*digit5)/1000
      digit3 = (absid-10000*digit5-1000*digit4)/100
      digit2 = (absid-10000*digit5-1000*digit4-100*digit3)/10
      digit1 = (absid-10000*digit5-1000*digit4-100*digit3-10*digit2)

CC:  Quark charges:

      If(absid .ge. 1 .and. absid .le. 6) Then
         icharge = sign*quark_chrg(absid)
         Return

CC:  Lepton charges:

      Else If(absid .ge. 11 .and. absid .le. 16) Then
         ievod = absid - 2*(absid/2)
         if(ievod .eq. 1) then     !  odd idhep value
            icharge = float(sign)
         else if(ievod .eq. 0) then  ! even idhep value
            icharge = 0.0
         end if
         Return

CC:  Charged Gauge bosons and charged Higgs:

      Else If(absid .eq. 24 .or. absid .eq. 37) Then
         icharge = float(sign)
         Return

CC:  Di-Quarks:

      Else If(digit4 .ne. 0 .and. digit2 .eq. 0) Then
         if(digit4 .ge. 1 .and. digit4 .le. 6) then
            q1 = quark_chrg(digit4)
         else
            icharge = 0.0
            Return
         end if
         if(digit3 .ge. 1 .and. digit3 .le. 6) then
            q2 = quark_chrg(digit3)
         else
            icharge = 0.0
            Return
         end if
         icharge = sign*(q1+q2)

CC:  Mesons:

      Else If(digit4.eq.0 .and. digit3.ne.0 .and. digit2.ne.0) Then
         if(digit3 .ge. 1 .and. digit3 .le. 6) then
            q1 = quark_chrg(digit3)
         else
            icharge = 0.0
            Return
         end if
         if(digit2 .ge. 1 .and. digit2 .le. 6) then
            q2 = quark_chrg(digit2)
         else
            icharge = 0.0
            Return
         end if
         icharge = sign*abs(q1-q2)

CC:  Baryons:

      Else If(digit2.ne.0 .and. digit3.ne.0 .and. digit4.ne.0) Then
         if(digit2 .ge. 1 .and. digit2 .le. 6) then
            q1 = quark_chrg(digit2)
         else
            icharge = 0.0
            Return
         end if
         if(digit3 .ge. 1 .and. digit3 .le. 6) then
            q2 = quark_chrg(digit3)
         else
            icharge = 0.0
            Return
         end if
         if(digit4 .ge. 1 .and. digit4 .le. 6) then
            q3 = quark_chrg(digit4)
         else
            icharge = 0.0
            Return
         end if
         icharge = sign*(q1+q2+q3)

CC:  Neutrals skipped so far (e.g. photons) and Problematic values:

      Else

         icharge = 0.0
         Return

      END IF

CC:  Round-Off charges to nearest whole integer:

      if(abs(icharge - 2.0) .lt. 0.001) then
         icharge = 2.000
         Return
      else if(abs(icharge - 1.0) .lt. 0.001) then
         icharge = 1.000
         Return
      else if(abs(icharge) .lt. 0.001) then
         icharge = 0.000
         Return
      else if(abs(icharge + 1.0) .lt. 0.001) then
         icharge = -1.000
         Return
      else if(abs(icharge + 2.0) .lt. 0.001) then
         icharge = -2.000
         Return
      end if

      Return
      END



