#ifdef MAIN
      program edisto

      parameter (MXIRHO=30)
      parameter (MXIZ=42)
      parameter (DZ=5)
      parameter (DRHO=5)
      parameter (DRIFT_LENGTH=210.)

      integer nwpaw
      real    h
      parameter (nwpaw=60000)
      common /pawc/ h(nwpaw)

      integer ntags
      parameter (ntags=6)
 
      character*(80) file
      character*(8)  chtags(ntags)
 
      data chtags /'R','Z','DR_UN','DR_SH','DR_PP','DR_HV'/

      real xnt(ntags)
 
c*** initialize HBOOK
 
      call hlimit(nwpaw)

c*** open output file
 
      write(6,'(a)') 'Enter output filename:'
      read(5,'(a)') file
      write(6,'(a)') 'Enter z coord. of shorted stripe [cm]:'
      read(5,*) z_short
      write(6,'(a)') 'Enter min. radius [cm], and max. distortion [cm]:'
      read(5,*) rho_min, rdi_max
      write(6,'(a)') 'Enter fraction potential mismatch:'
      read(5,*) dV
  
      call hropen(1,'NTUPLE',file,'N',8092,istat)
      call hbookn(1,'TEST',ntags,'NTUPLE',10000,chtags)
 
c*** In HWs calculations z=0 is at pad plane

      z_short = DRIFT_LENGTH - z_short

      do ir = 1, MXIRHO
        rho = 50+(ir-1)*DRHO
        do iz = 1, MXIZ
          z = (iz-1)*DZ
          xnt(1) = rho
          xnt(2) = z

c*** In HWs calculations z=0 is at pad plane

          z = DRIFT_LENGTH - z 

          xnt(3) = TDI_RDI_UNIFORM(rho,rho_min,rdi_max)
          xnt(4) = TDI_RDI_SHORT(z,rho,z_short)
          xnt(5) = TDI_RDI_PISTON_PAD(z,rho,dV)
          xnt(6) = TDI_RDI_PISTON_HV(z,rho,dV)
          call hfn(1,xnt)
        enddo
      enddo

      call hrout(0,icycle,' ')
      call hrend('NTUPLE')

      end
#else
C>----------------------------------------------------------------------
C TDI_RDI - Radial Distortion Integral for the STAR TPC
C
C DESCRIPTION:
C Generates the distortion due to shorted stripes on the outer field
C cage. Uses formalism and nomenclature of STAR note 253 by H. Wieman.
C The modules modifies the x-y position of TPC hits in the g2t_tpc_hit
C table, selecting just those hits in the required sector specified in
C the tdi_rdi_pars parameter table. Note both pseudo and physical
C padrows are distorted.
C
C INPUT ARGUMENTS:
C tdi_rdi_pars - table of RDI parameters
C g2t_tpc_hit  - table of MC TPC hits
C g2t_track    - table of MC tracks
C
C OUTPUT ARGUMENTS:
C g2t_tpc_hit  - table of MC TPC hits
C
C AUTHOR:
C 09-Dec-1996: Peter G. Jones: University of Birmingham
C
C MODIFICATIONS:
C 27-May-1997: PGJ - Changed Rin=50. cm to Rin=47.476 cm (from GSTAR geom) 
C 24-Apr-1997: PGJ - Accept and distort pseudo as well as physical
C                    padrows for the slow simulator.
C<----------------------------------------------------------------------
      REAL FUNCTION TDI_RDI( rdi_pars_h, rdi_pars,
     >                       g2t_tpc_hit_h, g2t_tpc_hit,
     >                       g2t_track_h, g2t_track )

      IMPLICIT NONE

#include "tdi_rdi.inc"
c-----------------------------------------------------------------------
      integer MAX_INDEX_HIT
      parameter(MAX_INDEX_HIT=5000)
      integer index_hit(MAX_INDEX_HIT)

      real DRIFT_LENGTH
      parameter(DRIFT_LENGTH=210.)

      integer i, i_sector, n_tpc_hit, nok, i_pseudo
      integer i_track, i_hit, i_last, i_next

      real TDI_RDI_UNIFORM, TDI_RDI_SHORT
      real TDI_RDI_PISTON_PAD, TDI_RDI_PISTON_HV
      real z_short, z, rho, drho, rho_min, rdi_max, dV
c-----------------------------------------------------------------------
      if( rdi_pars_h.nok .ne. 1 ) then
        write(6,'('' TDI_RDI: Error - no parameters'')')
        TDI_RDI = STAFCV_BAD
        return
      endif

c*** set up index table for g2t_tpc_hit

      call vzero(index_hit,MAX_INDEX_HIT)

      do i = 1, g2t_tpc_hit_h.nok
        if( g2t_tpc_hit(i).id .gt. MAX_INDEX_HIT ) then
          write(6,'('' TDI_RDI: Error - g2t_tpc_hit index list too small'')')
          TDI_RDI = STAFCV_BAD
          return
        endif
        index_hit(g2t_tpc_hit(i).id) = i
      enddo

c*** check mode and then loop over hits

      if( rdi_pars(1).mode .eq. 0 ) then                      !* NO DISTORTION
        do i_track = 1, g2t_track_h.nok
          n_tpc_hit = 0
          i_hit = index_hit(g2t_track(i_track).hit_tpc_p)
          g2t_track(i_track).hit_tpc_p = 0
          do while( i_hit .ne. 0 ) 
            i_next = index_hit(g2t_tpc_hit(i_hit).next_tr_hit_p)
            g2t_tpc_hit(i_hit).next_tr_hit_p = 0
            i_pseudo = int( g2t_tpc_hit(i_hit).volume_id/100000 ) * 1000
            i_sector = int( g2t_tpc_hit(i_hit).volume_id/100 ) - i_pseudo
c            if( rdi_pars(1).sector(i_sector) .ne. 0 ) then
            if( rdi_pars(1).sector .eq. i_sector ) then
              n_tpc_hit = n_tpc_hit + 1
              if( n_tpc_hit .eq. 1 ) then
                g2t_track(i_track).hit_tpc_p = g2t_tpc_hit(i_hit).id
              else
                g2t_tpc_hit(i_last).next_tr_hit_p = g2t_tpc_hit(i_hit).id
              endif
              i_last = i_hit
            else
              g2t_tpc_hit(i_hit).track_p = 0
            endif
            i_hit = i_next
          enddo
          g2t_track(i_track).n_tpc_hit = n_tpc_hit
        enddo
      elseif( rdi_pars(1).mode .eq. 1 ) then                  !* UNIFORM
        rho_min = rdi_pars(1).rho_min
        rdi_max = rdi_pars(1).rdi_max
        do i_track = 1, g2t_track_h.nok
          n_tpc_hit = 0
          i_hit = index_hit(g2t_track(i_track).hit_tpc_p)
          g2t_track(i_track).hit_tpc_p = 0
          do while( i_hit .ne. 0 ) 
            i_next = index_hit(g2t_tpc_hit(i_hit).next_tr_hit_p)
            g2t_tpc_hit(i_hit).next_tr_hit_p = 0
            i_pseudo = int( g2t_tpc_hit(i_hit).volume_id/100000 ) * 1000
            i_sector = int( g2t_tpc_hit(i_hit).volume_id/100 ) - i_pseudo
c            if( rdi_pars(1).sector(i_sector) .ne. 0 ) then
            if( rdi_pars(1).sector .eq. i_sector ) then
              rho = sqrt( g2t_tpc_hit(i_hit).x(1)**2 + g2t_tpc_hit(i_hit).x(2)**2 )
              drho = TDI_RDI_UNIFORM(rho,rho_min,rdi_max)
              call TDI_RDI_DISTORT(i_sector,drho,g2t_tpc_hit(i_hit).x,g2t_tpc_hit(i_hit).p)
              n_tpc_hit = n_tpc_hit + 1
              if( n_tpc_hit .eq. 1 ) then
                g2t_track(i_track).hit_tpc_p = g2t_tpc_hit(i_hit).id
              else
                g2t_tpc_hit(i_last).next_tr_hit_p = g2t_tpc_hit(i_hit).id
              endif
              i_last = i_hit
            else
              g2t_tpc_hit(i_hit).track_p = 0
            endif
            i_hit = i_next
          enddo
          g2t_track(i_track).n_tpc_hit = n_tpc_hit
        enddo
      elseif( rdi_pars(1).mode .eq. 2 ) then                  !* SHORT
        z_short = DRIFT_LENGTH - abs(rdi_pars(1).z_short)
        do i_track = 1, g2t_track_h.nok
          n_tpc_hit = 0
          i_hit = index_hit(g2t_track(i_track).hit_tpc_p)
          do while( i_hit .ne. 0 ) 
            i_next = index_hit(g2t_tpc_hit(i_hit).next_tr_hit_p)
            g2t_tpc_hit(i_hit).next_tr_hit_p = 0
            i_pseudo = int( g2t_tpc_hit(i_hit).volume_id/100000 ) * 1000
            i_sector = int( g2t_tpc_hit(i_hit).volume_id/100 ) - i_pseudo
c            if( rdi_pars(1).sector(i_sector) .ne. 0              .and.
            if( rdi_pars(1).sector .eq. i_sector                 .and.
     >          g2t_tpc_hit(i_hit).x(3)*rdi_pars(1).z_short .gt. 0 ) then
              z = DRIFT_LENGTH - abs( g2t_tpc_hit(i_hit).x(3) )
              rho = sqrt( g2t_tpc_hit(i_hit).x(1)**2 + g2t_tpc_hit(i_hit).x(2)**2 )
              drho = TDI_RDI_SHORT(z,rho,z_short)
              call TDI_RDI_DISTORT(i_sector,drho,g2t_tpc_hit(i_hit).x,g2t_tpc_hit(i_hit).p)
              n_tpc_hit = n_tpc_hit + 1
              if( n_tpc_hit .eq. 1 ) then
                g2t_track(i_track).hit_tpc_p = g2t_tpc_hit(i_hit).id
              else
                g2t_tpc_hit(i_last).next_tr_hit_p = g2t_tpc_hit(i_hit).id
              endif
              i_last = i_hit
            else
              g2t_tpc_hit(i_hit).track_p = 0
            endif
            i_hit = i_next
          enddo
          g2t_track(i_track).n_tpc_hit = n_tpc_hit
        enddo
      elseif( rdi_pars(1).mode .eq. 3 ) then                  !* PISTON PAD
        dV = abs(rdi_pars(1).dV)
        do i_track = 1, g2t_track_h.nok
          n_tpc_hit = 0
          i_hit = index_hit(g2t_track(i_track).hit_tpc_p)
          g2t_track(i_track).hit_tpc_p = 0
          do while( i_hit .ne. 0 ) 
            i_next = index_hit(g2t_tpc_hit(i_hit).next_tr_hit_p)
            g2t_tpc_hit(i_hit).next_tr_hit_p = 0
            i_pseudo = int( g2t_tpc_hit(i_hit).volume_id/100000 ) * 1000
            i_sector = int( g2t_tpc_hit(i_hit).volume_id/100 ) - i_pseudo
c            if( rdi_pars(1).sector(i_sector) .ne. 0              .and.
            if( rdi_pars(1).sector .eq. i_sector                 .and.
     >          g2t_tpc_hit(i_hit).x(3)*rdi_pars(1).z_short .gt. 0 ) then
              z = DRIFT_LENGTH - abs( g2t_tpc_hit(i_hit).x(3) )
              rho = sqrt( g2t_tpc_hit(i_hit).x(1)**2 + g2t_tpc_hit(i_hit).x(2)**2 )
              drho = TDI_RDI_PISTON_PAD(z,rho,dV)
              call TDI_RDI_DISTORT(i_sector,drho,g2t_tpc_hit(i_hit).x,g2t_tpc_hit(i_hit).p)
              n_tpc_hit = n_tpc_hit + 1
              if( n_tpc_hit .eq. 1 ) then
                g2t_track(i_track).hit_tpc_p = g2t_tpc_hit(i_hit).id
              else
                g2t_tpc_hit(i_last).next_tr_hit_p = g2t_tpc_hit(i_hit).id
              endif
              i_last = i_hit
            else
              g2t_tpc_hit(i_hit).track_p = 0
            endif
            i_hit = i_next
          enddo
          g2t_track(i_track).n_tpc_hit = n_tpc_hit
        enddo
      elseif( rdi_pars(1).mode .eq. 4 ) then                  !* PISTON HV
        dV = abs(rdi_pars(1).dV)
        do i_track = 1, g2t_track_h.nok
          n_tpc_hit = 0
          i_hit = index_hit(g2t_track(i_track).hit_tpc_p)
          g2t_track(i_track).hit_tpc_p = 0
          do while( i_hit .ne. 0 ) 
            i_next = index_hit(g2t_tpc_hit(i_hit).next_tr_hit_p)
            g2t_tpc_hit(i_hit).next_tr_hit_p = 0
            i_pseudo = int( g2t_tpc_hit(i_hit).volume_id/100000 ) * 1000
            i_sector = int( g2t_tpc_hit(i_hit).volume_id/100 ) - i_pseudo
c            if( rdi_pars(1).sector(i_sector) .ne. 0              .and.
            if( rdi_pars(1).sector .eq. i_sector                 .and.
     >          g2t_tpc_hit(i_hit).x(3)*rdi_pars(1).z_short .gt. 0 ) then
              z = DRIFT_LENGTH - abs( g2t_tpc_hit(i_hit).x(3) )
              rho = sqrt( g2t_tpc_hit(i_hit).x(1)**2 + g2t_tpc_hit(i_hit).x(2)**2 )
              drho = TDI_RDI_PISTON_HV(z,rho,dV)
              call TDI_RDI_DISTORT(i_sector,drho,g2t_tpc_hit(i_hit).x,g2t_tpc_hit(i_hit).p)
              n_tpc_hit = n_tpc_hit + 1
              if( n_tpc_hit .eq. 1 ) then
                g2t_track(i_track).hit_tpc_p = g2t_tpc_hit(i_hit).id
              else
                g2t_tpc_hit(i_last).next_tr_hit_p = g2t_tpc_hit(i_hit).id
              endif
              i_last = i_hit
            else
              g2t_tpc_hit(i_hit).track_p = 0
            endif
            i_hit = i_next
          enddo
          g2t_track(i_track).n_tpc_hit = n_tpc_hit
        enddo
      else
        TDI_RDI = STAFCV_BAD
        return
      endif

c*** hit garbage collection

      nok = 0

      do i = 1, g2t_tpc_hit_h.nok
        if( g2t_tpc_hit(i).track_p .ne. 0 ) then
          nok = nok + 1
          g2t_tpc_hit(nok) = g2t_tpc_hit(i)
        endif
      enddo

      g2t_tpc_hit_h.nok = nok

      TDI_RDI = STAFCV_OK

      end
C>----------------------------------------------------------------------
C TDI_RDI_DISTORT - Distorts the MC hit
C
C DESCRIPTION:
C Distorts the MC hit by projecting the radial distortion on to the
C pad plane. Theta is the angle between the cosmic ray track and the
C radial line defined by the undistorted MC point. In general theta
C is not equal to the track crossing angle alpha, which is defined
C with respect to the line perpendicular to the pad row. Only when
C radial line is perpendicular to the pad row are theta and alpha the
C same value. 
C
C INPUT ARGUMENTS:
C i_sector - sector number
C drho     - radial distortion
C x_g(3)   - undistorted MC hit (global coordinates)
C p_g(3)   - MC momentum (or direction cosines in global coordinates)
C
C OUTPUT ARGUMENTS:
C x_g(3)   - distorted MC hit (global coordinates)
C
C AUTHOR:
C 18-Mar-1997: Peter G. Jones: University of Birmingham
C<----------------------------------------------------------------------
      SUBROUTINE TDI_RDI_DISTORT(i_sector,drho,x_g,p_g)

      IMPLICIT NONE
c-----------------------------------------------------------------------
      integer i_sector
      real drho, x_g(3), p_g(3)

      real x_l(3), p_l(3), theta, tan_theta, tan_alpha, dy
c-----------------------------------------------------------------------
      call tgc_global_to_local(i_sector,x_g,x_l)
      tan_theta = x_l(1)/x_l(2)
      theta = atan(tan_theta)
      dy = drho*cos(theta)
    
      call tgc_global_to_local_p(i_sector,p_g,p_l)
      tan_alpha = p_l(1)/p_l(2)

      x_l(1) = x_l(1) - dy*(tan_alpha-tan_theta)
      call tgc_local_to_global(i_sector,x_l,x_g)

      return
      end
#endif
C>----------------------------------------------------------------------
C TDI_RDI_UNIFORM - Calculates the Radial Distortion Integral
C
C DESCRIPTION:
C This simplified distortion scenario has no physical basis. A uniform
C (z-independent) radial distortion is introduced, which has a maximum
C value of rdi_max at the radius of the OFC, and decreases linearly to
C zero distortion at a radius of rho_min.
C
C INPUT ARGUMENTS:
C rho     - radial position of space point, rho = sqrt(x**2 + y**2)
C rho_min - minimum radius of distortion
C rdi_max - maximum amplitiude of the distortion (at OFC)
C
C OUTPUT ARGUMENTS:
C Function returns the radial distortion [cm]
C
C AUTHOR:
C 29-Nov-1996: Peter G. Jones: University of Birmingham
C<----------------------------------------------------------------------
      REAL FUNCTION TDI_RDI_UNIFORM(rho,rho_min,rdi_max)

      IMPLICIT NONE
c-----------------------------------------------------------------------
      real rho, rho_min, rdi_max

      real Rout
      parameter (Rout=200.)

      real rdi
c-----------------------------------------------------------------------
c*** Initialize

      rdi = 0

c*** Calculate distortion

      if( rho.gt.rho_min .and. rho.lt.Rout ) then
        rdi = rdi_max*(rho-rho_min)/(Rout-rho_min)
      endif

      TDI_RDI_UNIFORM = rdi

      end
C>----------------------------------------------------------------------
C TDI_RDI_SHORT - Calculates the Radial Distortion Integral
C
C DESCRIPTION:
C Calculates the distortion due to shorted stripes on the outer field
C cage. Uses formalism and nomenclature of STAR note 253 by H. Wieman.
C
C INPUT ARGUMENTS:
C z      - z coordinate of space point
C rho    - radial position of space point, rho = sqrt(x**2 + y**2)
C zshort - z coordinate of shorted stripe
C
C OUTPUT ARGUMENTS:
C Function returns the radial distortion [cm]
C
C AUTHOR:
C 29-Nov-1996: Peter G. Jones: University of Birmingham
C<----------------------------------------------------------------------
      REAL FUNCTION TDI_RDI_SHORT(z,rho,zshort)

      IMPLICIT NONE
c-----------------------------------------------------------------------
      real z, rho, zshort

      integer NResistors, Nn
      real    DriftLength, Pi, Rin, Rout

      parameter (NResistors=182)
      parameter (DriftLength=210.)
      parameter (Pi=3.141592654)
      parameter (Nn=80)
      parameter (Rin=47.476)
      parameter (Rout=200.)

      logical converged

      integer n

      double precision sor_n, rdi, rdi_last
      double precision ai, ao, ar, az
      double precision a_n, b_n, besfun
      double precision I0_ai, K0_ao, I0_ao, K0_ai
      double precision DBESI0, DBESI1, DBESK0, DBESK1
c-----------------------------------------------------------------------
c*** Initialize

      converged = .false.
      rdi = 0
      n = 0

c*** Perform sum, coverge test prevents NaN from Inexact/Underflow/Overflow

      do while( n.lt.Nn .and. .not.converged )

        n = n + 1

        sor_n = -2*cos(n*Pi*zshort/Driftlength)/(n*Pi*(NResistors-1))

        ai  = n*Pi*Rin/DriftLength
        ao  = n*Pi*Rout/DriftLength
        ar  = n*Pi*rho/DriftLength
        az  = n*Pi*z/Driftlength

        I0_ai = DBESI0(ai)
        K0_ao = DBESK0(ao)
        I0_ao = DBESI0(ao)
        K0_ai = DBESK0(ai)

        besfun = I0_ai*K0_ao - I0_ao*K0_ai

        a_n = -sor_n*K0_ai/besfun
        b_n =  sor_n*I0_ai/besfun

        rdi_last = rdi
        rdi = rdi + (a_n*DBESI1(ar) - b_n*DBESK1(ar))*(cos(az) - 1.)
c        if( abs(rdi-rdi_last).lt.1e-30 ) converged = .true.

      enddo

      TDI_RDI_SHORT = real(rdi)*DriftLength

      end
C>----------------------------------------------------------------------
C TDI_RDI_PISTON_PAD - Calculates the Radial Distortion Integral
C
C DESCRIPTION:
C Calculates the distortion due to a pad plane positioning error.
C Uses formalism and nomenclature of STAR note 253 by H. Wieman.
C
C INPUT ARGUMENTS:
C z      - z coordinate of space point
C rho    - radial position of space point, rho = sqrt(x**2 + y**2)
C dV     - potential mismatch as a fraction of total potential
C
C OUTPUT ARGUMENTS:
C Function returns the radial distortion [cm]
C
C AUTHOR:
C 29-Nov-1996: Peter G. Jones: University of Birmingham
C<----------------------------------------------------------------------
      REAL FUNCTION TDI_RDI_PISTON_PAD(z,rho,dV)

      IMPLICIT NONE
c-----------------------------------------------------------------------
      real z, rho, dV

      integer Nn
      real    DriftLength, Pi, Rin, Rout

      parameter (DriftLength=210.)
      parameter (Pi=3.141592654)
      parameter (Nn=80)
      parameter (Rin=47.476)
      parameter (Rout=200.)

      logical converged

      integer n

      double precision sir_n, sor_n, rdi, rdi_last
      double precision ai, ao, ar, az
      double precision a_n, b_n, besfun
      double precision I0_ai, K0_ao, I0_ao, K0_ai
      double precision DBESI0, DBESI1, DBESK0, DBESK1
c-----------------------------------------------------------------------
c*** Initialize

      converged = .false.
      rdi = 0
      n = 0

c*** Perform sum, coverge test prevents NaN from Inexact/Underflow/Overflow

      do while( n.lt.Nn .and. .not.converged )

        n = n + 1

        sor_n = 2*dV / (n*Pi)
        sir_n = 2*dV / (n*Pi)

        ai  = n*Pi*Rin/DriftLength
        ao  = n*Pi*Rout/DriftLength
        ar  = n*Pi*rho/DriftLength
        az  = n*Pi*z/Driftlength

        I0_ai = DBESI0(ai)
        K0_ao = DBESK0(ao)
        I0_ao = DBESI0(ao)
        K0_ai = DBESK0(ai)

        besfun = I0_ai*K0_ao - I0_ao*K0_ai

        a_n = (sir_n*K0_ao - sor_n*K0_ai) / besfun
        b_n = (sor_n*I0_ai - sir_n*I0_ao) / besfun

        rdi_last = rdi
        rdi = rdi + (a_n*DBESI1(ar) - b_n*DBESK1(ar))*(cos(az) - 1.)
c        if( abs(rdi-rdi_last).lt.1.e-6 ) converged = .true.

      enddo

      TDI_RDI_PISTON_PAD = real(rdi)*DriftLength

      end
C>----------------------------------------------------------------------
C TDI_RDI_PISTON_HV - Calculates the Radial Distortion Integral
C
C DESCRIPTION:
C Calculates the distortion due to a hv plane positioning error.
C Uses formalism and nomenclature of STAR note 253 by H. Wieman.
C
C INPUT ARGUMENTS:
C z      - z coordinate of space point
C rho    - radial position of space point, rho = sqrt(x**2 + y**2)
C dV     - potential mismatch as a fraction of total potential
C
C OUTPUT ARGUMENTS:
C Function returns the radial distortion [cm]
C
C AUTHOR:
C 29-Nov-1996: Peter G. Jones: University of Birmingham
C<----------------------------------------------------------------------
      REAL FUNCTION TDI_RDI_PISTON_HV(z,rho,dV)

      IMPLICIT NONE
c-----------------------------------------------------------------------
      real z, rho, dV

      integer Nn
      real    DriftLength, Pi, Rin, Rout

      parameter (DriftLength=210.)
      parameter (Pi=3.141592654)
      parameter (Nn=80)
      parameter (Rin=47.476)
      parameter (Rout=200.)

      logical converged

      integer n

      double precision sir_n, sor_n, rdi, rdi_last
      double precision ai, ao, ar, az
      double precision a_n, b_n, besfun
      double precision I0_ai, K0_ao, I0_ao, K0_ai
      double precision DBESI0, DBESI1, DBESK0, DBESK1
c-----------------------------------------------------------------------
c*** Initialize

      converged = .false.
      rdi = 0
      n = 0

c*** Perform sum, coverge test prevents NaN from Inexact/Underflow/Overflow

      do while( n.lt.Nn .and. .not.converged )

        n = n + 1

        sor_n = 2*(-1)**n*dV / (n*Pi)
        sir_n = 2*(-1)**n*dV / (n*Pi)

        ai  = n*Pi*Rin/DriftLength
        ao  = n*Pi*Rout/DriftLength
        ar  = n*Pi*rho/DriftLength
        az  = n*Pi*z/Driftlength

        I0_ai = DBESI0(ai)
        K0_ao = DBESK0(ao)
        I0_ao = DBESI0(ao)
        K0_ai = DBESK0(ai)

        besfun = I0_ai*K0_ao - I0_ao*K0_ai

        a_n = (sir_n*K0_ao - sor_n*K0_ai) / besfun
        b_n = (sor_n*I0_ai - sir_n*I0_ao) / besfun

        rdi_last = rdi
        rdi = rdi + (a_n*DBESI1(ar) - b_n*DBESK1(ar))*(cos(az) - 1.)
c        if( abs(rdi-rdi_last).lt.1.e-6 ) converged = .true.

      enddo

      TDI_RDI_PISTON_HV = real(rdi)*DriftLength

      end
