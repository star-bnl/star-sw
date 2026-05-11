c 17.11.2011 Link routines between UrQMD and EPOS.
c author T. Pierog based on CORSIKA link to UrQMD


c-----------------------------------------------------------------------
      subroutine IniUrQMD
c-----------------------------------------------------------------------
c Primary initialization for UrQMD 1.31
c-----------------------------------------------------------------------
      implicit none
      include 'epos.inc'
      include 'boxinc.f'
      include 'inputs.f'
      include 'options.f'

c commons from coms.f
      integer Ap, At, Zp, Zt, npart, nbar, nmes, ctag
      integer nsteps,ranseed,event,eos,dectag,uid_cnt
      integer NHardRes,NSoftRes,NDecRes,NElColl,NBlColl
      common /sys/ npart, nbar, nmes, ctag,nsteps,uid_cnt,
     +             ranseed,event,Ap,At,Zp,Zt,eos,dectag,
     +             NHardRes,NSoftRes,NDecRes,NElColl,NBlColl

c local
      INTEGER          i,io,ia,ie,id
      CHARACTER        CTPStrg(numctp)*60, CTOStrng(numcto)*60
      integer mxie,mxid,mxia
      parameter (mxie=41,mxid=10,mxia=3)
      character adum
      double precision sig_u1,ekdummy
      integer iamax,idmax,iemax
      common /cxs_u1/ sig_u1(mxie,mxid,mxia),iamax,idmax,iemax
      double precision xs(3),bim(3)
      common /cxs_u2/ xs
      integer ishini,iudebug
      data bim/6.d0,6.d0,7.d0/
      SAVE


      call utpri('iniurq',ish,ishini,6)
      write(ifmt,'(a,i6)')'initialize URQMD ...'

C-----------------------------------------------------------------------

      IF ( ish.ge.2 ) THEN
        IUDEBUG = ish-1
      ELSE
        IUDEBUG = 0
      ENDIF

      WRITE (IFMT,*)
     $   '############################################################'
      WRITE (IFMT,*)
     $   '##                                                        ##'
      WRITE (IFMT,*)
     $   '##     UrQMD 1.3.1  University of Frankfurt               ##'
      WRITE (IFMT,*)
     $   '##                  urqmd@th.physik.uni-frankfurt.de      ##'
      WRITE (IFMT,*)
     $   '##                                                        ##'
      WRITE (IFMT,*)
     $   '############################################################'
      WRITE (IFMT,*)
     $   '##                                                        ##'
      WRITE (IFMT,*)
     $   '##     please cite when using this model:                 ##'
      WRITE (IFMT,*)
     $   '##     S.A.Bass et al. Prog.Part.Nucl.Phys. 41 (1998) 225 ##'
      WRITE (IFMT,*)
     $   '##     M.Bleicher et al. J.Phys. G25  (1999) 1859         ##'
      WRITE (IFMT,*)
     $   '##                                                        ##'
      WRITE (IFMT,*)
     $   '############################################################'

C  SET THE 'LARGE' CROSS-SECTIONS FOR ALL 3 TARGET ELEMENTS
      DO  I = 1, 3
        XS(I) = 10.D0 * PI * BIM(I)**2
      ENDDO

C  SET NMAX TO DEFAULT VALUE
      call set0
      call params

C  THIS IS THE SUBSTITUE FOR THE URQMD INPUT ROUTINE
C  INITIALIZE COUNTERS
      boxflag = 0
      mbflag  = 0
      edens   = 0.d0
      para    = 0
      solid   = 0
      mbox    = 0
      io      = 0

C  THE FOLLOWING FLAGS CHECK, WHETHER ALL NECESSARY INPUT IS GIVEN
C  PROJECTILE
      prspflg = 0
C  TARGET
      trspflg = 0
C
      srtflag = 0
      firstev = 0
C  EXCITATION FUNCTION
      nsrt    = 1
      npb     = 1
      efuncflag = 0
C  DEFAULT NUMBER OF EVENTS
      nevents = 1
C  DEFAULT NUMBER OF TIMESTEPS
      nsteps  = 1000

C  SKIP CONDITIONS ON UNIT 13, 14, 15, 16 & 18
C  SUPPRESS ALL OUTPUT
      bf13 = .true.
      bf14 = .true.
      bf15 = .true.
      bf16 = .true.
      bf18 = .true.
      bf19 = .true.
      bf20 = .true.
C  SET DEBUG OUTPUT DEPENDING ON CHOSEN DEBUG LEVEL
C  SET THE OUTPUT OF UNITS 13, 14, 15 TO THE DEBUG OUTPUT UNIT
      IF     ( IUDEBUG .EQ. 1 ) THEN
        bf13 = .true.
        bf14 = .false.
        call uounit(14,IFCH)
        bf15 = .true.
      ELSEIF ( IUDEBUG .EQ. 2 ) THEN
        bf13 = .false.
        call uounit(13,IFCH)
        bf14 = .true.
        bf15 = .true.
      ELSEIF ( IUDEBUG .GT. 2 ) THEN
        bf13 = .true.
        bf14 = .true.
        bf15 = .false.
        call uounit(15,IFCH)
      ENDIF
      do  i = 1, numcto
         CTOdc(i) = '  '
      enddo
      do  i = 1, numctp
         CTPdc(i) = '  '
      enddo
      do  i = 1, maxstables
         stabvec(i) = 0
      enddo
      nstable = 0

C  DEFAULT SETTINGS FOR CTParam AND CTOption
C  DEFAULT SETTINGS FOR CTParam
      CTParam(1)=1.d0
      CTPStrg(1)='scaling factor for decay-width'
      CTParam(2)=0.52d0
      CTPStrg(2)='used for minimal stringmass & el/inel cut in makestr'
      CTParam(3)=2.d0
      CTPStrg(3)='velocity exponent for modified AQM'
      CTParam(4)=0.3d0
      CTPStrg(4)='transverse pion mass, used in make22 & strexct'
      CTParam(5)=0.d0
      CTPStrg(5)='probabil. for quark rearrangement in cluster'
      CTParam(6)=0.37d0
      CTPstrg(6)='strangeness probability'
      CTParam(7)=0.d0
      CTPStrg(7)='charm probability (not yet implemented in UQMD)'
      CTParam(8)=0.093d0
      CTPStrg(8)='probability to create a diquark'
      CTParam(9)=0.35d0
      CTPStrg(9)='kinetic energy cut off for last string break'
      CTParam(10)=0.25d0
      CTPStrg(10)='min. kinetic energy for hadron in string'
      CTParam(11)=0.d0
      CTPStrg(11)='fraction of non groundstate resonances'
      CTParam(12)=.5d0
      CTPStrg(12)='probability for rho 770 in String'
      CTParam(13)=.27d0
      CTPStrg(13)='probability for rho 1450 (rest->rho1700)'
      CTParam(14)=.49d0
      CTPStrg(14)='probability for omega 782'
      CTParam(15)=.27d0
      CTPStrg(15)='probability for omega 1420(rest->om1600)'
      CTParam(16)=1.0d0
      CTPStrg(16)='mass cut betw. rho770 and rho 1450'
      CTParam(17)=1.6d0
      CTPSTRG(17)='mass cut betw. rho1450 and rho1700'
      CTParam(18)=.85d0
      CTPStrg(18)='mass cut betw. om 782 and om1420'
      CTParam(19)=1.55d0
      CTPStrg(19)='mass cut betw. om1420 and om1600'
      CTParam(20)=0.0d0
      CTPStrg(20)=' distance for second projectile'
      CTParam(21)=0.0d0
      CTPStrg(21)=' deformation parameter'

      CTParam(25)=.9d0
      CTPStrg(25)=' probability for diquark not to break'
      CTParam(26)=50.d0
      CTPStrg(26)=' maximum trials to get string masses'
      CTParam(27)=1.d0
      CTPStrg(27)=' scaling factor for xmin in string excitation'
      CTParam(28)=1.d0
      CTPStrg(28)=' scaling factor for transverse fermi motion'
      CTParam(29)=0.4d0
      CTPStrg(29)=' single strange di-quark suppression factor '
      CTParam(30)=1.5d0
      CTPStrg(30)=' radius offset for initialization  '
      CTParam(31)=1.6d0
      CTPStrg(31)=' sigma of gaussian for tranverse momentum tranfer '
      CTParam(32)=0.d0
      CTPStrg(32)=' alpha-1 for valence quark distribution  '
      CTParam(33)=2.5d0
      CTPStrg(33)=' betav for valence quark distribution  (DPM)'
      CTParam(34)=0.1d0
      CTPStrg(34)=' minimal x multiplied with ecm  '
      CTParam(35)=3.0d0
      CTPStrg(35)=' offset for cut for the FSM '
      CTParam(36)=0.275d0
      CTPStrg(36)=' fragmentation function parameter a  '
      CTParam(37)=0.42d0
      CTPStrg(37)=' fragmentation function parameter b  '
      CTParam(38)=1.08d0
      CTPStrg(38)=' diquark pt scaling factor '
      CTParam(39)=0.8d0
      CTPStrg(39)=' strange quark pt scaling factor '
      CTParam(40)=0.5d0
      CTPStrg(40)=' betas-1 for valence quark distribution (LEM)'
      CTParam(41)=0.d0
      CTPStrg(41)=' distance of initialization'
      CTParam(42)=0.55d0
      CTPStrg(42)=' width of gaussian -> pt in string-fragmentation '
      CTParam(43)=5.d0
      CTPStrg(43)=' maximum kinetic energy in mesonic clustr '
      CTParam(44)=0.8d0
      CTPStrg(44)=' prob. of double vs. single excitation for AQM inel.'
      CTParam(45)=0.5d0
      CTPStrg(45)=' offset for minimal mass generation of strings'
      CTParam(46)=800000.d0
      CTPStrg(46)=' maximal number of rejections for initialization'
      CTParam(47)=1.0d0
      CTPStrg(47)=' field feynman fragmentation funct. param. a'
      CTParam(48)=2.0d0
      CTPStrg(48)=' field feynman fragmentation funct. param. b'

      CTParam(50)=1.d0
      CTPStrg(50)=' enhancement factor for 0- mesons'
      CTParam(51)=1.d0
      CTPStrg(51)=' enhancement factor for 1- mesons'
      CTParam(52)=1.d0
      CTPStrg(52)=' enhancement factor for 0+ mesons'
      CTParam(53)=1.d0
      CTPStrg(53)=' enhancement factor for 1+ mesons'
      CTParam(54)=1.d0
      CTPStrg(54)=' enhancement factor for 2+ mesons'
      CTParam(55)=1.d0
      CTPStrg(55)=' enhancement factor for 1+-mesons'
      CTParam(56)=1.d0
      CTPStrg(56)=' enhancement factor for 1-*mesons'
      CTParam(57)=1.d0
      CTPStrg(57)=' enhancement factor for 1-*mesons'
      CTParam(58)=1.d0
      CTPStrg(58)=' scaling factor for DP time-delay'

C  DEFAULT SETTINGS FOR CTOption
      CTOption(1)=1                  ! hjd1
      CTOStrng(1)=' resonance widths are mass dependent '
      CTOption(2)=0
      CTOStrng(2)=' conservation of scattering plane'
      CTOption(3)=0
      CTOStrng(3)=' use modified detailed balance'
      CTOption(4)=0
      CTOStrng(4)=' no initial conf. output '
      CTOption(5)=2
      CTOStrng(5)=' random impact parameter between limits'
      CTOption(6)=0
      CTOStrng(6)=' no first collisions inside proj/target'
      CTOption(7)=0
      CTOStrng(7)=' elastic cross-section enabled (<>0:total=inelast)'
      CTOption(8)=0
      CTOStrng(8)=' extrapolate branching ratios '
      CTOption(9)=0
      CTOStrng(9)=' use tabulated pp cross-sections '
      CTOption(10)=0
      CTOStrng(10)=' enable Pauli Blocker'
      CTOption(11)=0
      CTOStrng(11)=' mass reduction for cascade initialization'
      CTOption(12)=0
      CTOStrng(12)=' string condition =0 (.ne.0 no strings)'
      CTOption(13)=0
      CTOStrng(13)=' enhanced file16 output '
      CTOption(14)=0
      CTOStrng(14)=' cos(the) is distributet between -1..1 '
      CTOption(15)=0
      CTOStrng(15)=' allow mm&mb-scattering'
      CTOption(16)=0
      CTOStrng(16)=' propagate without collisions'
      CTOption(17)=0
      CTOStrng(17)=' colload after every timestep '
      CTOption(18)=0
      CTOStrng(18)=' final decay of unstable particles'
      CTOption(19)=0
      CTOStrng(19)=' allow bbar annihilaion'
      CTOption(20)=0
      CTOStrng(20)=' dont generate e+e- instead of bbar'
      CTOption(21)=0
      CTOStrng(21)=' use field feynman frgm. function'
      CTOption(22)=1
      CTOStrng(22)=' use lund excitation function'
      CTOption(23)=0
      CTOStrng(23)=' lorentz contraction of projectile & targed'
      CTOption(24)=2      ! 1 is default    2 means fast method
      CTOStrng(24)=' Wood-Saxon initialization'
      CTOption(25)=0
      CTOStrng(25)=' phase space corrections for resonance mass'
      CTOption(26)=0
      CTOStrng(26)=' use z -> 1-z for diquark-pairs'
      CTOption(27)=1             ! hjd1
      CTOStrng(27)=' reference frame (1=target, 2=projectile, else=cms)'
      CTOption(28)=0
      CTOStrng(28)=' propagate spectators also '
      CTOption(29)=2
      CTOStrng(29)=' no transverse momentum in clustr '
      CTOption(30)=1
      CTOStrng(30)=' frozen fermi motion '
      CTOption(31)=0
      CTOStrng(31)='  reduced mass spectrum in string'
      CTOption(32)=0
      CTOStrng(32)=' masses are distributed acc. to m-dep. widths'
      CTOption(33)=0
      CTOStrng(33)=' use tables & m-dep. for pmean in fprwdt & fwidth'
      CTOption(34)=1
      CTOStrng(34)=' lifetme according to m-dep. width'
      CTOption(35)=1
      CTOStrng(35)=' generate high precision tables'
      CTOption(36)=0
      CTOStrng(36)=' normalize Breit-Wigners with m.dep. widths '
      CTOption(37)=0
      CTOStrng(37)=' heavy quarks form di-quark clusters'
      CTOption(38)=0
      CTOStrng(38)=' scale p-pbar to b-bbar with equal p_lab '
      CTOption(39)=0
      CTOStrng(39)=' dont call pauliblocker'
      CTOption(40)=0
      CTOStrng(40)=' read old fort.14 file '
      CTOption(41)=0
      CTOStrng(41)=' generate extended output for cto40'
      CTOption(42)=0
      CTOStrng(42)=' hadrons now have color fluctuations'
      CTOption(43)=0
      CTOStrng(43)=' dont generate dimuon intead of dielectron output'
      CTOption(44)=0
      CTOStrng(44)=' not used at the moment'
      CTOption(45)=0
      CTOStrng(45)=' not used at the moment'

C  INITIALIZE ARRAYS FOR SPECIAL PRO/TAR COMBINATIONS
      do  i = 1, 2
         spityp(i) = 0
         spiso3(i) = 0
      enddo

C  INITIALIZE ARRAYS FOR SPECIAL PARTICLES
      EoS = 0

C  READ CROSS-SECTION FILES
Cdh   CALL URQREC()

C  INITIALIZES SOME ARRAYS
      call strini      ! initialize mixing angles for meson-multipletts
      call loginit

      IF ( CTOption(33) .EQ. 0  .OR.  CTOption(9) .EQ. 0 ) THEN
        call loadwtab(io)
        IF ( IUDEBUG .GT. 0 ) WRITE(IFCH,*) 'URQINI: AFTER LOADWTAB'
      ENDIF

C READ URQMD TOTAL CROSS SECTION TABLE
c
c   ie=1..41   E=10.0**(float(ie)/10-1.0-0.05)  (bin-middle)
c   id=1..9    p,ap,n,an,pi+,pi-,K+,K-,KS
c   ia=1..3    N,O,Ar
c
      OPEN(UNIT=76,FILE=fnnx(1:nfnnx)//
     *     'urqmd13/UrQMD-1.3.1-xs.dat',STATUS='OLD')
      read(76,*) adum,iamax,idmax,iemax
      do ia=1,iamax
        do id=1,idmax
          do ie=1,iemax
            read(76,*) ekdummy,sig_u1(ie,id,ia)
          enddo
          read(76,*)
          read(76,*)
        enddo
      enddo
      close(76)

C  IN CASE OF CASCADE MODE, THE POTENTIALS NEED NOT BE CALCULATED

C  CALCULATE NORMALIZATION OF RESONANCES DISTRIBUTION...
      call norm_init

      egymin=1.31   !min cms energy using pion as proj (min ekin=0.3)
      egymax=1.e4
      irescl=0
      

      call utprix('iniurq',ish,ishini,6)
      end

c-----------------------------------------------------------------------
      subroutine IniEvtUrq
c-----------------------------------------------------------------------
c Initialization for each type of event (for given proj, targ and egy)
c-----------------------------------------------------------------------
      implicit none
      include 'epos.inc'
      real rmproj,rmtarg,bmax,bkmx
      common/geom/rmproj,rmtarg,bmax,bkmx

      include 'boxinc.f'
      include 'inputs.f'
      include 'options.f'

c commons from coms.f
      integer Ap, At, Zp, Zt, npart, nbar, nmes, ctag
      integer nsteps,ranseed,event,eos,dectag,uid_cnt
      integer NHardRes,NSoftRes,NDecRes,NElColl,NBlColl
      common /sys/ npart, nbar, nmes, ctag,nsteps,uid_cnt,
     +             ranseed,event,Ap,At,Zp,Zt,eos,dectag,
     +             NHardRes,NSoftRes,NDecRes,NElColl,NBlColl

      real*8  time,  acttime, bdist, ebeam, bimp,bmin,ecm
      common /rsys/ time,acttime,bdist,bimp,bmin,ebeam,ecm

      real*8
     +     gw, sgw, delr, fdel, dt,
     +     da, db,
     +     Cb0, Yuk0, Pau0, Sky20, Sky30, gamSky, gamYuk, drPau, dpPau,
     +     dtimestep
c 19 real*8
      common /pots/ Cb0, Yuk0, Pau0, Sky20, Sky30, gamSky,
     +              gamYuk, drPau, dpPau, gw, sgw, delr, fdel,
     +              dt,da, db,dtimestep

c local

      integer ICUTBL(200),ICU2I3(200)
      common /urqmdproj/ICUTBL,ICU2I3
C  CONVERSION TABLE CORSIKA TO URQMD
      DATA ICUTBL/
     &   0,   0,   0,   0,   0,   0, 101, 101, 101, 106,    ! 10
     & 106,-106,   1,   1,  -1,-106, 102,  27,  40,  40,    ! 20
     &  40,  49,  49,  55,  -1, -27, -40, -40, -40, -49,    ! 30
     & -49, -55,   0,   0,   0,   0,   0,   0,   0,   0,    ! 40
     & 160 * 0 /
C  TABLE FOR ISOSPIN COMPONENT 2*I3  (CONVERSION CORSIKA TO URQMD)
      DATA ICU2I3/
     &   0,   0,   0,   0,   0,   0,   0,   2,  -2,  -1,
     &   1,  -1,  -1,   1,  -1,   1,   0,   0,   2,   0,
     &  -2,   1,  -1,   0,   1,   0,  -2,   0,   2,  -1,
     &   1,   0,   0,   0,   0,   0,   0,   0,   0,   0,
     & 160 * 0 /
      
      double precision caltim,outtim,nucrad
      real furqcrse
      integer idtmp,idtrafo

      if(maproj.gt.210)
     &  call utstop('Mass too big for UrQMD (Mprj<210) !&')
      if(abs(idtarg).ne.1120.and.abs(idtarg).ne.0)
     &  call utstop('target no allowed in UrQMD !&')
      call iclass(idproj,iclpro)
      bmin=bminim
      bdist=dble(bmaxim)
C  SELECT AND INITIALIZE TARGET
      trspflg   =  0
      At  = matarg
      Zt  = latarg

C  SET THE PROJECTILE
      if(laproj.ge.0)then
C  PROJECTILE IS A NUCLEUS
        prspflg = 0
        Ap  = maproj
        Zp  = laproj
      else
C  PROJECTILE IS A SPECIAL PARTICLE
        Ap  = 1
        prspflg = 1
        idtmp=idtrafo('nxs','cor',idproj)
        spityp(1) = ICUTBL(idtmp)
        spiso3(1) = ICU2I3(idtmp)
      endif

C  ENERGY OF COLLISION (LAB-SYSTEM)
      ebeam   = dble(ekin)
C  eos: impact parameter
      eos     = 0
C  nev: number of events
      nevents = 1
C  tim: time of propagation
      caltim  = 200.d0
      outtim  = 200.d0
C  fast CASCADE mode
      if ( eos .eq. 0 ) dtimestep = outtim
C
      nsteps   = int(0.01d0+caltim/dtimestep)
      outsteps = int(0.01d0+outtim/dtimestep)

      if ( boxflag .eq. 0 ) then

C  initialize nuclei (projectile and target) and store them

C  initialize normal projectile
        if ( prspflg .eq. 0 ) then
          call cascinit(Zp,Ap,1)
        endif
C  initialize normal target
        if ( At .ne. 0 ) then
          if ( trspflg .eq. 0 ) then
            call cascinit(Zt,At,2)
          endif
        endif
      endif


      bmax=sngl(nucrad(maproj)+nucrad(matarg)+2*CTParam(30))


      urqincs=furqcrse()                    !total cross section !!!
      if(engy.lt.egymin)urqincs=0.          !below egymin, no interaction
c initialize cross-sections
      call xsigma

      if(ish.ge.2)write(ifch,*)
     &  'UrQMD used with (Ek,idproj,idtarg,xs) ',ekin,idproj,idtarg
     &                                          ,urqincs


      return
      end

c-----------------------------------------------------------------------
      subroutine emsurq(iret)
c-----------------------------------------------------------------------
c  call UrQMD to simulate interaction
c-----------------------------------------------------------------------
      implicit none
      include 'epos.inc'
      real rmproj,rmtarg,bmax,bkmx
      common/geom/rmproj,rmtarg,bmax,bkmx
      integer ICUTBL(200),ICU2I3(200)
      common /urqmdproj/ICUTBL,ICU2I3
      include 'boxinc.f'
      include 'inputs.f'
      include 'options.f'
c commons from coms.f
      integer Ap, At, Zp, Zt, npart, nbar, nmes, ctag
      integer nsteps,ranseed,event,eos,dectag,uid_cnt
      integer NHardRes,NSoftRes,NDecRes,NElColl,NBlColl
      common /sys/ npart, nbar, nmes, ctag,nsteps,uid_cnt,
     +             ranseed,event,Ap,At,Zp,Zt,eos,dectag,
     +             NHardRes,NSoftRes,NDecRes,NElColl,NBlColl

      real*8  time,  acttime, bdist, ebeam, bimp,bmin,ecm
      common /rsys/ time,acttime,bdist,bimp,bmin,ebeam,ecm

      integer nmax
      parameter (nmax = 500) ! maximum number of particles
      integer spin(nmax),ncoll(nmax),charge(nmax),strid(nmax),
     +        ityp(nmax),lstcoll(nmax),iso3(nmax),origin(nmax),uid(nmax)
      real*8
     +     r0(nmax), rx(nmax), ry(nmax), rz(nmax),
     +     p0(nmax), px(nmax), py(nmax), pz(nmax),
     +     fmass(nmax), rww(nmax),
     +     dectime(nmax)
      common/isys/spin,ncoll,charge,ityp,lstcoll,iso3,origin,strid,
     +            uid
      common /coor/ r0, rx, ry, rz, p0, px, py, pz, fmass, rww, dectime

c local
      real a,rangen,b1,b2,rrr,pt2
      integer iret,i,idepos,idpdg,j,ncol,nn,idtrafo,pdgid,idtmp
     +,ntps,ntns,ntppar,ntnpar


      iret=0
      ncol=0
      b1=bminim
      b2=bmax
      a=pi*(b2**2-b1**2)
c      print *,b1,b2,a
      rrr=rangen()
      if(urqincs.le.1.e-3)a=-1.    !elastic event !
      if(a.lt.0.)then
        goto 1002               !no interaction
      elseif(a.gt.0.)then
        if(rangen().gt.urqincs/10./a)goto 1001 !no interaction
      endif
      if(ish.ge.3)call alist('Determine UrQMD Production&',0,0)



      if(irdmpr.ne.0.and.laproj.eq.-1)then
C  UPDATE PROJECTILE
        idtmp=idtrafo('nxs','cor',idproj)
        spityp(1) = ICUTBL(idtmp)
        spiso3(1) = ICU2I3(idtmp)
      endif
      if(idtargin.eq.0)then
C  UPDATE TARGET
        trspflg   =  0
        At  = matarg
        Zt  = latarg
C  initialize normal target
        if ( boxflag .eq. 0 ) then
          if ( At .ne. 0 ) then
            if ( trspflg .eq. 0 ) then
              call cascinit(Zt,At,2)
            endif
          endif
        endif
      endif

C  NOW THE REAL WORK IS DONE
      CALL URQMD( 0 )

      if(ish.ge.3)write(ifch,*)'UrQMD: ctag,NElColl=', ctag,NElColl

      if(ctag.le.NElColl)then
c only elastic interactions
        if(ionudi.eq.1)then   !elastic event part of the cross section (CR)
          anintine=anintine+1.
          goto 1002             !accept elastic
        else
          goto 1001             !redo
        endif
      endif

      nptl=0
      
      call conre
      call conwr

      ncol=1
      nevt=1
      kolevt=-1
      koievt=-1
      kohevt=-1
      npjevt=maproj
      ntgevt=matarg
      pmxevt=pnll
      egyevt=engy
      bimevt=bimp
      phievt=0.

      anintine=anintine+1.

c set projectile and target as non final particles
      do i=1,maproj
        istptl(i)=1
      enddo
      do j=1,matarg
        istptl(j)=1
      enddo

      ntps=0
      ntns=0

      do nn=1,npart

        idpdg=pdgid(ityp(nn),iso3(nn))
        idepos=idtrafo('pdg','nxs',idpdg)
        
        nptl=nptl+1
               
        iorptl(nptl)=0
        jorptl(nptl)=0
        istptl(nptl)=0
        xorptl(4,nptl)= sngl(r0(nn))
        xorptl(1,nptl)=sngl(rx(nn))
        xorptl(2,nptl)=sngl(ry(nn))
        xorptl(3,nptl)=sngl(rz(nn))
        pptl(4,nptl)= sngl(p0(nn))
        pptl(1,nptl)=sngl(px(nn))
        pptl(2,nptl)=sngl(py(nn))
        pptl(3,nptl)=sngl(pz(nn))
        pptl(5,nptl)=sngl(fmass(nn))
        idptl(nptl)=idepos
        ityptl(nptl)=0
        rinptl(nptl)=-9999
c Put particle in cms frame.
        call utlob5(yhaha, pptl(1,nptl), pptl(2,nptl)
     *     , pptl(3,nptl), pptl(4,nptl), pptl(5,nptl))
        pt2=pptl(1,nptl)**2+pptl(2,nptl)**2
        if(pt2.lt.1e-6)then    !spectators
          if(idepos.eq.1120)then
            ntps=ntps+1
          elseif(idepos.eq.1220)then
            ntns=ntns+1
          endif
        endif
            
        if(ish.ge.5)write(ifch,'(a,i5,a,i5,a,4(e10.4,1x),f6.3)')
     $         ' particle from UrQMD ',nptl,' id :',idptl(nptl)
     $         , ' momentum :',(pptl(i,nptl),i=1,5)

                                  
      enddo

      if(latarg.gt.1)then
        ntgevt=matarg-ntps-ntns
        ntppar=latarg-ntps
        ntnpar=ntgevt-ntppar
        do j=maproj+1,matarg+maproj
          if(idptl(j).eq.1120)then
            if(ntppar.gt.0)then
              ntppar=ntppar-1
            else                !restore spectators
              iorptl(j)=0
            endif
          endif
          if(idptl(j).eq.1220)then
            if(ntnpar.gt.0)then
              ntnpar=ntnpar-1
            else                !restore spectators
              iorptl(j)=0
            endif
          endif
        enddo
      endif


1000  if(ish.ge.5)write(ifch,'(a,i4,i3)')
     $         ' End of UrQMD event :',npart,iret
      return

1001  iret=-1 
      goto 1000 

 1002 iret=-2      !elastic
      goto 1000 

      end

c------------------------------------------------------------------------------
      real function furqcrse()
c------------------------------------------------------------------------------
c hadron/nucleus-nucleus particle production cross section with UrQMD.
c------------------------------------------------------------------------------
      include 'epos.inc'
      real rmproj,rmtarg,bmax,bkmx
      common/geom/rmproj,rmtarg,bmax,bkmx
      furqcrse=urqcrse(ekin,idproj,maproj,matarg,idtargin,bmax)
      return
      end
c------------------------------------------------------------------------------
      real function urqcrse(ek,idpro,mapr,matg,idtar,bmax)
c------------------------------------------------------------------------------
c hadron/nucleus-nucleus particle production cross section with UrQMD.
c------------------------------------------------------------------------------
      include 'epos.inc'
      integer mxie,mxid,mxia
      parameter (mxie=41,mxid=10,mxia=3)
      double precision sig_u1,xsu
      integer iamax,idmax,iemax
      common /cxs_u1/ sig_u1(mxie,mxid,mxia),iamax,idmax,iemax
      double precision xs(3)
      common /cxs_u2/ xs

      dimension we(3)
      double precision ye,we
      integer je,i,id,ia


      urqcrse = 0.
      if(ek.gt.900.)return
      iemax   = mxie
      ye = log10(dble(ek))*10.d0+10.5d0
      if ( ye. lt. 1d0 ) ye = 1.d0
      je = min(iemax-2,int(ye))
      we(2) = ye-je
      we(3) = we(2)*(we(2)-1.d0)*.5d0
      we(1) = 1.d0-we(2)+we(3)
      we(2) = we(2)-2.d0*we(3)
      
      id=0
      if    ( idpro .eq. 1120 ) then
        id = 1
        xstyp=0.88
      elseif( idpro .eq. -1120) then
        id = 2
        xstyp=0.88
      elseif( idpro .eq. 1220 ) then
        id = 3
        xstyp=0.88
      elseif( idpro .eq. -1220) then
        id = 4
        xstyp=0.88
      elseif( idpro .eq.  120 ) then
        id = 5
        xstyp=0.77
      elseif( idpro .eq.  -120) then
        id = 6
        xstyp=0.77
      elseif( idpro .eq. 130 ) then
        id = 7
        xstyp=0.73
      elseif( idpro .eq. -130) then
        id = 8
        xstyp=0.73
      elseif( idpro .eq. 20  .or.  idpro .eq. -20 ) then
        id = 9
        xstyp=0.73
      endif
      
      if(mapr.eq.1.and.id.ne.0)then

c     ia=1..3
c  N,O,Ar
        if(idtargin.eq.0)then
          iamin=1
          iamax=3
        elseif(matg.le.airanxs(1))then
          iamin=1
          iamax=1
        elseif(matg.ge.0.5*(airanxs(2)+airanxs(3)))then
          iamin=3
          iamax=3
        else
          iamin=2
          iamax=2
        endif
        do  ia = iamin,iamax
          xsu=0d0
          do i = 1,3
            xsu  = xsu + sig_u1(je+i-1,id,ia)*we(i)
          enddo
          xsnorm=airwnxs(ia)
c if not air target, use the 3 components to get an average with Ztarg/Zair as scaling factor
        if(idtargin.ne.0)xsnorm=xstyp/airznxs(ia)*float(matg)**0.88
          urqcrse  = urqcrse + sngl(xsu)*xsnorm
        enddo

      elseif(mapr.eq.1.and.idtar.eq.0)then

        do ia = 1,3
          urqcrse = urqcrse + airwnxs(ia)*XS(ia)
        enddo

      else     !geometric black disc xs

        urqcrse = 10.D0 * PI * bmax**2

      endif


      return
      end



C=======================================================================

      DOUBLE PRECISION FUNCTION RANF(idum)

C-----------------------------------------------------------------------
C  FL(UKA) R(A)ND(O)M (GENERATOR)
C  THIS FUNCTON IS CALLED FROM FLUKA ROUTINES.
C-----------------------------------------------------------------------
      double precision dranf
      include 'epos.inc'
      ranf=dranf(dble(idum))
      if(irandm.eq.1)write(ifch,*)'ranf()= ',ranf

      RETURN
      END
 
