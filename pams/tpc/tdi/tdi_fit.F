* $Id: tdi_fit.F,v 1.2 1998/01/27 00:59:18 fisyak Exp $
* $Log: tdi_fit.F,v $
* Revision 1.2  1998/01/27 00:59:18  fisyak
* Split sources
*
      REAL FUNCTION TDI_FIT( fit_pars_h, fit_pars,
     >                       tphit_h, tphit,
     >                       strack_h, strack,
     >                       residual_h, residual)
C>----------------------------------------------------------------------
C TDI_FIT - Residual Calculator for Straight (Cosmic/Laser) Tracks
C
C DESCRIPTION:
C Performs straight line fit on space points in x-y and r-z planes.
C The routine is designed for use with cosmic ray tracks and laser
C tracks. Control parameters can be used to select hits from a single
C sector and to omit rows from the fit.
C
C INPUT ARGUMENTS:
C tdi_fit_pars - table of selection parameters
C tcl_tphit    - table of TPC hits
C
C OUTPUT ARGUMENTS:
C tdi_strack   - table of (straight) track parameters
C tdi_residual - space point residuals
C
C AUTHOR:
C 19-Dec-1996: Peter G. Jones: University of Birmingham
C
C MODIFICATION HISTORY:
C 16-Jun-1997: Skip tracks which exceed mxhit. PGJ
C 02-Jun-1997: Fit in rz plane changed to yz plane, since radius is a 
C              non-liner function of distance. PGJ
C 13-May-1997: Fit in xy plane now uses y as the independent variable
C              since tracks are nearly vertical in sector 5. The proper
C              way to do this would be to fit in local coordinates,
C              since the measurement error is along the padrow. PGJ
C<----------------------------------------------------------------------

      IMPLICIT NONE

#include "tdi_fit.inc"
c-----------------------------------------------------------------------
      integer mxhit
      parameter (mxhit=45)

      integer mxtrk
      parameter (mxtrk=50)

      integer tls_Quick_Sort_i, lswlf

      integer i, iret, ntrack, itrack, itrack_last
      integer ihit, nhit, isec, irow, nfit
      integer lhit, lrow

      integer nhfirst(mxtrk), nhtot(mxtrk), ih(mxhit), ir(mxhit)

      real x(mxhit), y(mxhit), z(mxhit)
      real wcir(mxhit), wlin(mxhit)

      real slope_xy, dslope_xy, slope_yz, dslope_yz
      real x0, dx0, z0, dz0, chisq_xy, chisq_yz
      real p_g(3), p_l(3), sum_res_xy, sum_res_yz

      integer isector
      real sec_ang(12), alpha, lambda, cos_alpha
      data sec_ang /60.,30.,0.,-30.,-60.,-90.,
     >              -120.,-150.,180.,150.,120.,90./

      integer ievent
      save ievent
      data ievent /0/

      integer ntags1, ntags2
      parameter (ntags1=17, ntags2=11)
      character*(8)  chtags1(ntags1), chtags2(ntags2)
      real xnt1(ntags1), xnt2(ntags2) 
 
      data chtags1 /'track_id','nhit','nfit',
     >              'slope_xy','y0','chisq_xy',
     >              'slope_yz','z0','chisq_yz',
     >              'last_row','z_last',
     >              'res_xy','res_yz',
     >              'alpha','lambda',
     >              'sres_xy','sres_yz'/
 
      data chtags2 /'track_id','nhit','nfit','row','res_xy','res_yz',
     >              'last_row','alpha','lambda','z','z_last'/

      integer istat, icycle

      logical NTUPLE, FIT_OK
      save NTUPLE
      data NTUPLE /.false./
c-----------------------------------------------------------------------
c*** initialize

      if( fit_pars_h.nok .ne. 1 ) then
        write(6,'('' TDI_FIT: Error - No parameters'')')
        TDI_FIT = STAFCV_BAD
        return
      elseif( .not.NTUPLE .and. fit_pars(1).ntuple.eq.1 ) then
        call hropen(1,'NTUPLE','cr_fit.ntp','N',1024,istat)
        call hbookn(1,'Tracks',ntags1,'NTUPLE',200000,chtags1)
        call hbookn(2,'Residuals',ntags2,'NTUPLE',200000,chtags2)
        if( istat .eq. 0 ) NTUPLE = .true.
      endif

      ievent = ievent + 1

      strack_h.nok = 0
      residual_h.nok = 0

c*** sort hits by track number

      iret = tls_Quick_Sort_i(tphit_h.nok,tphit(1).track,
     >                        tphit(2).track,tphit)

      ntrack = 0
      itrack_last = 0

      do i = 1, tphit_h.nok
        itrack = int(tphit(i).track/1000)
        if( itrack .ne. itrack_last ) then
          itrack_last = itrack
          ntrack = ntrack + 1
          if( ntrack .le. mxtrk ) then
            nhfirst(ntrack) = i
            nhtot(ntrack) = 1
          endif
        else
          if( ntrack.gt.0 .and. ntrack.le.mxtrk ) 
     >      nhtot(ntrack) = nhtot(ntrack) + 1
        endif
      enddo

      if( ntrack .gt. mxtrk ) then
        write(6,'(''TDI_FIT: Event: '',i6)') ievent
        write(6,'(''TDI_FIT: Warning - mxtrk exceeded'')')
        ntrack = mxtrk
      endif

c*** loop over tracks and pass hits to fitting routine

      do itrack = 1, ntrack

        nhit = 0	! number of all points
        nfit = 0	! number of fit points

        lrow = 0	! last row seen
        lhit = 0	! last hit seen

c*** loop over hits and make selection

        do ihit = nhfirst(itrack), nhfirst(itrack)+nhtot(itrack)-1
          isec = int(tphit(ihit).row/100)
          if( isec .eq. fit_pars(1).sector ) then
            irow = mod(tphit(ihit).row,100)
            nhit = nhit + 1
            if( nhit .le. mxhit ) then
              ih(nhit) = ihit 
              ir(nhit) = irow
              if( irow .gt. lrow ) then
                lrow = irow
                lhit = nhit
              endif
            endif
            if( irow .ge. fit_pars(1).first_row .and.
     >          irow .le. fit_pars(1).last_row ) then 
              nfit = nfit + 1
              if( nfit .le. mxhit ) then
                x(nfit) = tphit(ihit).x
                y(nfit) = tphit(ihit).y
                z(nfit) = tphit(ihit).z
                wcir(nfit) = 1./(tphit(ihit).dx**2 + tphit(ihit).dy**2)
                wlin(nfit) = 1./tphit(ihit).dz**2
              endif
            endif
          endif
        enddo

        if( nhit .gt. mxhit ) then
          write(6,'(''TDI_FIT: Event: '',i6)') ievent
          write(6,'(''TDI_FIT: Warning - mxhit exceeded !'')')
        endif

c*** fit straight lines and fill track and residual tables

        if( nfit .ge. fit_pars(1).min_fit .and.
     >      nhit .le. mxhit               ) then

          FIT_OK = .true.

          iret = lswlf(y,x,wcir,nfit,slope_xy,x0,chisq_xy,dslope_xy,dx0)
          if( iret .ne. 0 ) then
            write(6,'(''TDI_FIT: Warning xy fit failed'')')
            FIT_OK = .false.
          endif

          iret = lswlf(y,z,wlin,nfit,slope_yz,z0,chisq_yz,dslope_yz,dz0)
          if( iret .ne. 0 ) then
            write(6,'(''TDI_FIT: Warning yz fit failed'')')
            FIT_OK = .false.
          endif

          if( FIT_OK ) then
            isector = fit_pars(1).sector

c            cos_alpha = cosd(sec_ang(isector))*sin(atan(slope_xy)) +
c     >                  sind(sec_ang(isector))*cos(atan(slope_xy))
c            alpha = acosd(cos_alpha)
c            if( alpha .gt. 90. ) alpha = 180. - alpha

            p_g(1) = sin(atan(slope_xy))
            p_g(2) = cos(atan(slope_xy))
            p_g(3) = 0.
            call tgc_global_to_local_p(isector,p_g,p_l)
            alpha = atand(p_l(1)/p_l(2))

            lambda = atand(slope_yz)

            strack_h.nok = strack_h.nok + 1
            strack(strack_h.nok).id = itrack
            strack(strack_h.nok).nhit = nhit
            strack(strack_h.nok).nfit = nfit  
            strack(strack_h.nok).hitid = ih(1)
            strack(strack_h.nok).slope_xy = slope_xy
            strack(strack_h.nok).x0 = x0
            strack(strack_h.nok).chisq_xy = chisq_xy
            strack(strack_h.nok).slope_yz = slope_yz
            strack(strack_h.nok).z0 = z0
            strack(strack_h.nok).chisq_yz = chisq_yz
            strack(strack_h.nok).last_row = mod(tphit(ih(lhit)).row,100)
            strack(strack_h.nok).z_last = tphit(ih(lhit)).z
            strack(strack_h.nok).res_xy = ( tphit(ih(lhit)).x -
     >          slope_xy*tphit(ih(lhit)).y - x0 ) * cosd(alpha)
            strack(strack_h.nok).res_yz = ( tphit(ih(lhit)).z -
     >          slope_yz*tphit(ih(lhit)).y - z0 ) / sqrt( 1 + slope_yz**2 )
            strack(strack_h.nok).alpha = alpha
            strack(strack_h.nok).lambda = lambda

            sum_res_xy = 0
            sum_res_yz = 0

            do ihit = 1, nhit
              residual_h.nok = residual_h.nok + 1
              residual(residual_h.nok).trackid = itrack
              residual(residual_h.nok).hitid = ih(ihit)
              residual(residual_h.nok).row = ir(ihit)
              residual(residual_h.nok).res_xy = ( tphit(ih(ihit)).x -
     >          slope_xy*tphit(ih(ihit)).y - x0 ) * cosd(alpha)
              residual(residual_h.nok).res_yz = ( tphit(ih(ihit)).z -
     >          slope_yz*tphit(ih(ihit)).y - z0 ) / sqrt( 1 + slope_yz**2 )

              if( NTUPLE ) then
                xnt2(1) = strack(strack_h.nok).id
                xnt2(2) = strack(strack_h.nok).nhit
                xnt2(3) = strack(strack_h.nok).nfit
                xnt2(4) = residual(residual_h.nok).row
                xnt2(5) = residual(residual_h.nok).res_xy
                xnt2(6) = residual(residual_h.nok).res_yz
                xnt2(7) = strack(strack_h.nok).last_row
                xnt2(8) = strack(strack_h.nok).alpha
                xnt2(9) = strack(strack_h.nok).lambda
                xnt2(10) = tphit(ih(ihit)).z
                xnt2(11) = strack(strack_h.nok).z_last
                call hfn(2,xnt2)
              endif

              if( ir(ihit) .gt. fit_pars(1).last_row ) then
                sum_res_xy = sum_res_xy + residual(residual_h.nok).res_xy
                sum_res_yz = sum_res_yz + residual(residual_h.nok).res_yz
              endif
            enddo ! loop over hits

            strack(strack_h.nok).sres_xy = sum_res_xy
            strack(strack_h.nok).sres_yz = sum_res_yz

            if( NTUPLE ) then
              xnt1(1) = strack(strack_h.nok).id
              xnt1(2) = strack(strack_h.nok).nhit
              xnt1(3) = strack(strack_h.nok).nfit
              xnt1(4) = strack(strack_h.nok).slope_xy
              xnt1(5) = strack(strack_h.nok).x0
              xnt1(6) = strack(strack_h.nok).chisq_xy
              xnt1(7) = strack(strack_h.nok).slope_yz
              xnt1(8) = strack(strack_h.nok).z0
              xnt1(9) = strack(strack_h.nok).chisq_yz
              xnt1(10) = strack(strack_h.nok).last_row
              xnt1(11) = strack(strack_h.nok).z_last
              xnt1(12) = strack(strack_h.nok).res_xy
              xnt1(13) = strack(strack_h.nok).res_yz
              xnt1(14) = strack(strack_h.nok).alpha
              xnt1(15) = strack(strack_h.nok).lambda
              xnt1(16) = strack(strack_h.nok).sres_xy
              xnt1(17) = strack(strack_h.nok).sres_yz
              call hfn(1,xnt1)
            endif

          endif ! ( FIT_OK ) 

        endif ! ( nfit .ge. fit_pars(1).min_fit )

      enddo ! loop over tracks

c*** close ntuple

      if( NTUPLE .and. fit_pars(1).ntuple.eq.0 ) then
        call hrout(0,icycle,' ')
        call hrend('NTUPLE')
        NTUPLE = .false.
      endif

      TDI_FIT = STAFCV_OK

      end
