* $Id: tdi_rdi.F,v 1.2 1998/01/27 00:59:18 fisyak Exp $
* $Log: tdi_rdi.F,v $
* Revision 1.2  1998/01/27 00:59:18  fisyak
* Split sources
*
      REAL FUNCTION TDI_RDI( rdi_pars_h, rdi_pars,
     >                       g2t_tpc_hit_h, g2t_tpc_hit,
     >                       g2t_track_h, g2t_track )
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
