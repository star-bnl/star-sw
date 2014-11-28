* $Id: cr_star.F,v 1.2 1998/02/14 23:02:39 fisyak Exp $
* $Log: cr_star.F,v $
* Revision 1.2  1998/02/14 23:02:39  fisyak
* Split Fortran sources
*
      INTEGER*4 FUNCTION CR_STAR()
      IMPLICIT NONE
#include "cr_star.inc"
C>----------------------------------------------------------------------
C CR_STAR - Cosmic Ray generator in STAR
C
C DESCRIPTION:
C Generates events in the star tpc from Cosmic Rays using the 
C BaBar HEMISCOM generation program.
C
C AUTHOR:
C Main by Douglas E. Greiner: Lawrence Berkeley National Laboratory
C
C MODIFIED:
C 09-Jun-1997: Modified to allow any hit and write ntuple output for
C              Peripheral Collisions Background Rate study. PGJ
C 13-Mar-1997: Trigger condition specified on command line. PGJ
C 20-Feb-1997: Modified to allow e.g. CTB slats {56,5}. PGJ
C 11-Feb-1997: Modified to select exit CTB slat. PGJ
C 29-Oct-1996: Peter G. Jones: University of Birmingham
C              Now takes command line options.
C<----------------------------------------------------------------------
      Integer i
C      Integer j
      Integer in_acceptance
*..Input :
      Integer D_Mode           ! Program execution mode
      Integer MSpec            ! Momentum Spectrum Mode
      Real*4 Len               ! Mother Cylinder Length (cm)
      Real*4 Rad               ! Mother Cylinder Rad (cm)
      Real*4 Off               ! Mother Cylinder Offset from ground (cm)
      Real*4 P_Low,P_High      ! Momentum Spectrum Cutoffs (GeV/c)
*..Output:
      Integer Charge           ! Muon Charge (+1 or -1)
      Integer End_Hits         ! Number of particles thrown at the cylinder
      Integer End_Nsph         ! Number of hemi-sphere events
      Real*4 Theta,Phi         ! Momentum direction(radian)
      Real*4 P                 ! Momentum (GeV/c)
      Real*4 X,Y,Z             ! Particle location (cm)
      Real*4 Rate_Sph          ! Predicted Rate (Hz) on the hemisphere
      Real*4 Rate_PD           ! Actual Rate (Hz) of particles hitting
                               !   the detector (See Shadow)
      Real*4  Version          ! Program version (output)
*..data output
      real*4 t                 ! time of generation
      real*4 px,py,pz          ! momentum in TPC coordinates
      real*4 ang               ! angle between +y and hit point on CTB
      real*4 del_ang           ! the angle between CTB slats in radians
      integer ctb_in           ! phi index of ctb slat hit
      integer ctb_out          ! phi index of ctb slat hit
      integer ctb_max          ! max phi index of ctb selected
      integer ctb_min          ! min phi index of ctb selected
      integer ctb_trg          ! phi index of trigger ctb slat
      integer ctb_z            ! z indez of ctb slat hit
      integer ctb_z_in         ! z index of first ctb hit
      integer ctb_z_max        ! max z indez of ctb slat selected
      integer ctb_z_min        ! min z indez of ctb slat selected
      integer ctb_hits(6,60)   ! ctb slat hit, 1,6 are ends,2-5 are modules
      integer pspect(300)      ! momentum spectrum of accepted particles
      integer ipbin            ! bin index for momentum spectrum
      integer ge_pid
      integer vertex_id
      integer parent_track
      integer n_daughters

      data Len/480.0/
      data Rad/219.5/
      data Off/0.0/
      data MSpec/3/
      data P_high/1000.0/
      data P_low/1.0/
      data ctb_z_min,ctb_z_max/3,4/
      data ctb_min,ctb_max/47,51/
      data ctb_trg/27/

      data del_ang/0.104719755/
      data t/0.0/
      data vertex_id/1/
      data parent_track/0/
      data n_daughters/0/

      logical accept, ntuple
c-----------------------------------------------------------------------
#if defined(NEVER)
      integer nwpaw
      parameter (nwpaw=100000)
      real h(nwpaw)
      common /pawc/ h
#endif
      data  D_Mode / 0 /
      integer ntags, istat, icycle
      parameter (ntags=7)
      real xnt(ntags)
      character*(8)  chtags(ntags)
      data chtags /'PX','PY','PZ',
     >             'CTB_IN','CTB_OUT','CTB_ZIN','CTB_ZOUT'/
c-----------------------------------------------------------------------
      character*(80) arg, ofile
      integer narg, iargc, iarg, ierr, nevents
      logical usage

      real slope_yx, slope_zy, intercept_yx, intercept_zy
      real ayx, byx, cyx, xcyl1, xcyl2, ycyl1, ycyl2
      real d1, d2, xout, yout, zout
c-----------------------------------------------------------------------
#if defined(NEVER)
c*** Get command line arguments
      narg = iargc()
      iarg = 1
      do while( iarg .le. narg )
        call getarg(iarg,arg)
        iarg = iarg + 1
        if( arg .eq. '-o' ) then
          if( iarg .le. narg ) then
            call getarg(iarg,ofile)
            iarg = iarg + 1
          else
            usage = .true.
          endif
        elseif( arg .eq. '-n' ) then
          if( iarg .le. narg ) then
            call getarg(iarg,arg)
            iarg = iarg + 1
            read(arg,*,iostat=ierr) nevents
            if( ierr .ne. 0 ) usage=.true.
          else
            usage = .true.
          endif
        elseif( arg .eq. '-z' ) then
          if( iarg .le. narg ) then
            call getarg(iarg,arg)
            iarg = iarg + 1
            read(arg,*,iostat=ierr) ctb_z_min, ctb_z_max
            if( ierr .ne. 0 ) usage=.true.
          else
            usage = .true.
          endif
        elseif( arg .eq. '-t' ) then
          if( iarg .le. narg ) then
            call getarg(iarg,arg)
            iarg = iarg + 1
            read(arg,*,iostat=ierr) ctb_min, ctb_max, ctb_trg
            if( ierr .ne. 0 ) usage=.true.
          else
            usage = .true.
          endif
        elseif( arg .eq. '-help' ) then
          usage = .true.
        else
          usage = .true.
        endif
      enddo

      if( ctb_z_min .gt. ctb_z_max ) usage = .true.

      if( nevents.eq.0 .or. ofile.eq.' ' .or. usage ) then
        write(6,'('' Usage: cr_star [-n <events>] '')')
        write(6,'(''                [-o <file>]'')')
        write(6,'(''                [-t <low CTB id>,<high CTB id>,<trig CTB id>]'')')
        write(6,'(''                [-z <low CTB z index>,<high CTB z index>]'')')
        write(6,'(''                [-help]'')')
        call exit(1)
      endif
#endif
c*** Initialization
      if (D_mode .eq. 0) then
      D_mode = 1
      call HemiCosm(Len,Rad,Off,MSpec,P_High,P_Low,D_Mode,
     >              Theta,Phi,P,X,Y,Z,Charge,
     >              End_Hits,Rate_PD,End_Nsph,Rate_Sph,
     >              Version)

      endif
c*** Open output file
#if defined(NEVER)
      ntuple = index(ofile,'ntp').ne.0
      if( ntuple ) then
        call hlimit(nwpaw)
        call hropen(1,'NTUPLE',ofile,'N',8092,istat)
        call hbookn(1,'TEST',ntags,'NTUPLE',10000,chtags)
      else
        open(unit=20,file=ofile,form='formatted',status='unknown')
      endif

c*** Event loop

      D_mode = 2
      in_acceptance = 0
      do while( in_acceptance .lt. nevents )
#endif
        call HemiCosm(Len,Rad,Off,MSpec,P_High,P_Low,D_Mode,
     >                Theta,Phi,P,X,Y,Z,Charge,
     >                End_Hits,Rate_PD,End_Nsph,Rate_Sph,
     >                Version) 

c*** Determine the CTB module hit

        ang = atan2(X,Y)
        ctb_in = (ang + del_ang/2.0)/del_ang+58
        if( ctb_in.gt.60 ) ctb_in = ctb_in - 60

        accept = .false.

        if( ctb_min .le. ctb_max ) then 
          if( ctb_in.ge.ctb_min .and. ctb_in.le.ctb_max )
     >      accept = .true.
        else
          if( ctb_in.ge.ctb_min .or. ctb_in.le.ctb_max )
     >      accept = .true.
        endif

c*** Set ctb_z to 0 or 5 if particle hits the end caps

        ctb_z = 5
        if( Z.le.-240.0 ) then
          ctb_z = 0
        elseif( Z.lt.-120.0 ) then
          ctb_z = 1
        elseif( Z.lt.0.0    ) then
          ctb_z = 2
        elseif( Z.lt.120.0  ) then
          ctb_z = 3
        elseif( Z.lt.240.0  ) then
          ctb_z = 4
        endif

        ctb_hits(ctb_z+1,ctb_in) = ctb_hits(ctb_z+1,ctb_in) + 1

        ctb_z_in = ctb_z

c*** Write out the event if it is in selected region of ctb      

        if( accept .and. ctb_z.ge.ctb_z_min .and. ctb_z.le.ctb_z_max ) then

c          in_acceptance = in_acceptance + 1

c*** Store the momentum spectrum

          ipbin=P*10+1
          if( ipbin.gt.0 .and. ipbin.lt.301 ) then
            pspect(ipbin)=pspect(ipbin)+1.
          endif

c*** Calculate the cosmic exit point

          pz = P*cos(theta)
          px = P*sin(theta)*cos(phi)
          py = P*sin(theta)*sin(phi)      

          slope_yx = py/px
          slope_zy = pz/py

          intercept_yx = Y - slope_yx * X
          intercept_zy = Z - slope_zy * Y

          ayx = (1 + slope_yx**2)
          byx = 2*slope_yx*intercept_yx
          cyx = intercept_yx**2 - Rad**2

          xcyl1 = ( -byx + sqrt(byx**2 - 4*ayx*cyx) ) / (2*ayx)
          xcyl2 = ( -byx - sqrt(byx**2 - 4*ayx*cyx) ) / (2*ayx)

          ycyl1 = slope_yx*xcyl1 + intercept_yx
          ycyl2 = slope_yx*xcyl2 + intercept_yx

          d1 = sqrt( (xcyl1-X)**2 + (ycyl1-Y)**2 ) 
          d2 = sqrt( (xcyl2-X)**2 + (ycyl2-Y)**2 ) 

          if( d1 .gt. d2 ) then
            xout = xcyl1
            yout = ycyl1
          else
            xout = xcyl2
            yout = ycyl2
          endif

          zout = slope_zy*yout + intercept_zy

c*** Check exit point is in acceptance

          ang = atan2(xout,yout)
          ctb_out = (ang + del_ang/2.0)/del_ang+58
          if( ctb_out.gt.60 ) ctb_out = ctb_out - 60

          ctb_z = 5
          if( zout.le.-240.0 ) then
            ctb_z = 0
          elseif( zout.lt.-120.0 ) then
            ctb_z = 1
          elseif( zout.lt.0.0    ) then
            ctb_z = 2
          elseif( zout.lt.120.0  ) then
            ctb_z = 3
          elseif( zout.lt.240.0  ) then
            ctb_z = 4
          endif

c*** check whether trigger slat was specified

          if( (ctb_trg .eq. 0          .and.
     >         ctb_z   .ge. ctb_z_min  .and. 
     >         ctb_z   .le. ctb_z_max) .or.
     >        (ctb_z   .eq. ctb_z_in   .and.
     >         ctb_out .eq. ctb_trg)   ) then

            in_acceptance = in_acceptance + 1

            if( ntuple ) then
              xnt(1) = P*sin(theta)*cos(phi)
              xnt(2) = P*sin(theta)*sin(phi)
              xnt(3) = P*cos(theta)
              xnt(4) = ctb_in
              xnt(5) = ctb_out
              xnt(6) = ctb_z_in
              xnt(7) = ctb_z
              call hfn(1,xnt)
            else
              write(20,100) in_acceptance
100           format('EVENT:',3x,1x,i6,'      1      1')
              write(20,101),X,Y,Z,t,vertex_id,parent_track,n_daughters
101           format('VERTEX:',4(1x,g12.5),4(1x,i6))
              ge_pid = 5
              if( charge.lt.0.0 ) ge_pid = 6
              pz = P*cos(theta)
              px = P*sin(theta)*cos(phi)
              py = P*sin(theta)*sin(phi)      
              write (20,102) ge_pid,px,py,pz
102           format ('TRACK:',1x,i6,3(1x,g12.5))      
            endif
          endif   ! exit hit accepted
        endif     ! entrance hit accepted
#if defined(NEVER)
      enddo
c*** End job

      D_mode = 3
      call HemiCosm(Len,Rad,Off,MSpec,P_High,P_Low,D_Mode,
     >                    Theta,Phi,P,X,Y,Z,Charge,
     >                    End_Hits,Rate_PD,End_Nsph,Rate_Sph,
     >                    Version)

c*** Close output file

      if( ntuple ) then
        call hrout(0,icycle,' ')
        call hrend('NTUPLE')
      else
       close (20)
      endif

c*** Output the ctb hit information

      write(6,103) in_acceptance
103   format (' tracks output in acceptance=',i10)
      write (6,104) ctb_z_min,ctb_z_max,ctb_min,ctb_max
104   format (' acceptance in i_sub z   =',i3,' to ',i3/
     >        ' acceptance in i_sub_phi =',i5,' to ' i5)

C      write (6,105) ((i,(ctb_hits(j,i),j=1,6)),i=1,60)

      do i = 1, 60
        write (6,105) i,ctb_hits(1,i),ctb_hits(2,i),ctb_hits(3,i),
     >                  ctb_hits(4,i),ctb_hits(5,i),ctb_hits(6,i)
      enddo
105   format (7i10)

C      write (6,106) (pspect(i),i=1,300)
C106   format(i20)

*yf   stop
#endif
      CR_STAR=STAFCV_OK
      end
