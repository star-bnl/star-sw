
c 31.04.2021 Introduce option to call CRMC from outside
c 15.01.2009 Simplified Main program and random number generator for epos

      subroutine crmc_init_f(degymx,iSeed,iModel,itab
     +                      ,itypout,iParam,output,lout)
***************************************************************
*
*  init models with values set in crmc_set_f
*
*          degymx     - maximum center-of-mass energy which will be call in the run
*          iseed      - random seed
*          iModel     - HE model switch
*          itab       - force tables production or stop if missing
*          itypout    - output type
*                       <0 no nuclei as beam (ROOT)
*                        0 standard
*                        1 LHE
*                        2 FLUKA
*          iParam     - param file name
*          output     - output file name for LHE output
*          lout       - lenght of the output string (useful only for LHE output) 
*
*  should be called once only.
***************************************************************
      implicit none
      include "epos.inc"
      integer iSeed,itypout,iout,lout,itab,init,iModel,iadd,ipath
      double precision degymx
      character*1000 output,iParam,path
      character*4 lhct
      common/typoutput/iout
      common/producetab/ producetables              !used to link CRMC
      logical producetables                         !with EPOS and QII
      data init/0/
      save init


      if(init.eq.0)then     !security just to be sure it is called once only
        
c     Set parameters to default value
      call aaset(0)

c     Stop program if missing tables (after aaset)
      producetables=.false.
      if(itab.eq.1)producetables=.true.

c     Set common for crmc_init
      iout=itypout

      iframe=11                 !11 puts it always in nucleon nucleon reference
                                !frame. This is ok because we use ecms
                                !which is calculated in crmc_f.

      model=max(1,iModel)              ! epos = 0,1 / qgsjet01 = 2 / gheisha = 3
                                ! / pythia = 4 / hijing = 5 / sibyll 2.3d
                                ! = 6 / qgsjetII.04 = 7 / phojet = 8
                                ! qgsjetII.03 = 11 / dpmjetIII = 12 / qgsjetIII = 13
      ionudi=1
      lhct=""
      iadd=0
      if(iModel.eq.0)then
        ihacas=-1                !use hadronic rescattering
      else
        ihacas=0                !Do not use hadronic rescattering (faster)
      endif

      ipath=index(iParam,' ')-1
      if(iout.eq.2.and.ipath.gt.0)then  !define path for tables using "output" (FLUKA)
        path=iParam(1:ipath)
      else
        ipath=0
        path=""
      endif

      nfnnx=ipath
      fnnx=path(1:ipath)
      nfnhpf=13
      fnhpf="./tables.dat "
      nfnii=ipath+15            ! epos tab file name length
      fnii=path(1:ipath)//"tabs/epos.initl"    ! epos tab file name
      nfnid=ipath+15
      fnid=path(1:ipath)//"tabs/epos.inidi"
      nfnie=ipath+15
      fnie=path(1:ipath)//"tabs/epos.iniev"
      nfnrj=ipath+15+iadd
      fnrj=path(1:ipath)//"tabs/epos.inirj"//lhct
      nfncs=ipath+15+iadd
      fncs=path(1:ipath)//"tabs/epos.inics"//lhct


      seedi=1.d0                !seed for random number generator: at start program
      seedj=iSeed               !seed for random number generator: for first event
      jwseed=0                  !print out seed in see file (1) or not


c     Initialize decay of particles
      nrnody=0                  !number of particle types without decay
                                !(if 0 (default) : all unstable
                                !particles decay (at the end only
                                !(anti)nucleons, (anti)electrons and
                                !muons)
c Particle code is given as
c     id=+/-ijkl
c
c          mesons--
c          i=0, j<=k, +/- is sign for j
c          id=110 for pi0, id=220 for eta, etc.
c
c          baryons--
c          i<=j<=k in general
c          j<i<k for second state antisymmetric in (i,j), eg. l = 2130
c
c          other--
c          id=1,...,6 for quarks
c          id=9 for gluon
c          id=10 for photon
c          id=11,...,16 for leptons
c          i=17 for deuteron
c          i=18 for triton
c          i=19 for alpha
c          id=20 for ks, id=-20 for kl
c
c          i=21...26 for scalar quarks
c          i=29 for gluino
c
c          i=30 for h-dibaryon
c
c          i=31...36 for scalar leptons
c          i=39 for wino
c          i=40 for zino
c
c          id=80 for w+
c          id=81,...,83 for higgs mesons (h0, H0, A0, H+)
c          id=84,...,87 for excited bosons (Z'0, Z''0, W'+)
c          id=90 for z0
c
c          diquarks--
c          id=+/-ij00, i<j for diquark composed of i,j.
c
c Examples : 2130 = lambda, 1330=xi0, 2330=xi-, 3331=omega
c
c Conversion from epos to pdg code can be done using
c      id_pdg=idtrafo('nxs','pdg',id_epos)


      if(iout.eq.2)then     !stable particles before FLUKA
c$$$  nrnody=nrnody+1
c$$$      nody(nrnody)=120     !pi+
c$$$      nrnody=nrnody+1
c$$$      nody(nrnody)=-120    !pi-
c$$$      nrnody=nrnody+1
c$$$      nody(nrnody)=130     !K+
c$$$      nrnody=nrnody+1
c$$$      nody(nrnody)=-130    !K-
c$$$      nrnody=nrnody+1
c$$$      nody(nrnody)=-20     !Kl
c$$$      nrnody=nrnody+1
c$$$      nody(nrnody)=-14     !mu+
c$$$      nrnody=nrnody+1
c$$$      nody(nrnody)=14      !mu-

c      nrnody=nrnody+1
c      nody(nrnody)=idtrafo('pdg','nxs',3122)    !lambda using pdg code
      endif

      iecho=0                   !"silent" reading mode
      modsho = 100000000        !printout every modsho events (not active)


c     Debug
      ish=0                     !debug level
c     ifch=6                    !debug output (screen)
c     ifch=31    !debug output (file)
c     fnch="epos.debug"
c     nfnch=index(fnch,' ')-1
c     open(ifcx,file=fnch(1:nfnch),status='unknown')
      istmax = 0                !final and mother particles (istmax=0 includes
                                !real mother particles)
      infragm=2                 !nuclear fragmentation (realistic)
      if(model.eq.12)infragm=0 !no EPOS fragmentation by default but can be switch on !(evaporation in DPMJET switch off ???)


c     The parameters can be changed optionnaly by reading a file
c     (example.param) using the following subroutine call
      if(iout.ne.2)call EposInput(iParam)     !(it can be commented)

      if ( model.ne.1 )istmax=0 !for most models virtual mothers are not defined

c     if you put what is in input.optns in example.param, you can even run
c     exactly the same way (coded parameters are overwritten). Don't forget
c     the command : "EndEposInput" at the end of example.param, otherwise it
c     will not run.
c     initialization for the maximum energy and mass
      engy=degymx
      idprojin=1120
      idtargin=1120
      if(model.eq.1)then
        call atitle
        call hnbcreate
        laproj=100
        maproj=210              !to set difnuc up to the maximum mass
      else if (model.eq.6.or.model.eq.12) then
        laproj=9
        maproj=18               !limited by Sibyll and DPMJET06 here (not important)
      else
         laproj=1
         maproj=1
      endif
      latarg=laproj
      matarg=maproj
      iwseed=0                !to switch off random number print out here
      call ainit
      iwseed=1

      else

        init=1

      endif

c     Here the cross section sigineaa is defined
c     LHE type output done by EPOS
      if(iout.eq.1)call EposOutput(output(1:lout)//' ')

      end


      subroutine crmc_set_f(iEvent,pproj,ptarg,ipart,itarg)

***************************************************************
*
*  interface to epos subroutine
*
*   input: iEvent     - number of events to generate
*          pproj      - beam momentum in GeV/c (detector frame)
*          ptarg      - target momentum in GeV/c of nucleon! (detector frame)
*          ipart      - primary particle type
*          itarg      - target particle type
*
***************************************************************
      implicit none
      include "epos.inc"

c     Input values
      integer ipart,itarg,iEvent
      double precision pproj, ptarg

      real   m1,m2
      double precision decms,e1,e2

      double precision ycm2det
      logical doBoost
      common/boostvars/ycm2det,doBoost

c     Calculations of energy of the center-of-mass in the detector frame
      call idmass(1120,m2)      !target mass = proton
      m1=m2                     !projectile mass
      if(abs(ipart).eq.211)call idmass(120,m1)
      if(abs(itarg).eq.211)call idmass(120,m2) !if pion as targ or proj
      if(abs(ipart).eq.321)call idmass(130,m1)
      if(abs(itarg).eq.321)call idmass(130,m2) !if kaon as targ or proj

      e1=dsqrt(dble(m1)**2+pproj**2)
      e2=dsqrt(dble(m2)**2+ptarg**2)
      decms=dsqrt(((e1+e2)+(pproj+ptarg))*((e1+e2)-(pproj+ptarg)))
c     Later a rapidity boost back into the detector system will be performed
      doBoost = .true.
c     ycm2det defines this rapidity
      if (((e1+e2)-(pproj+ptarg)) .le. 0d0) then
         ycm2det=1d99
      elseif (((e1+e2)+(pproj+ptarg)) .le. 0d0) then
         ycm2det=-1d99
      else
         ycm2det=0.5d0*dlog(((e1+e2)+(pproj+ptarg))/
     +        ((e1+e2)-(pproj+ptarg)))
      endif

      if (pproj .le. 0d0) then
         ycm2det=-ycm2det
      endif
c     Update some parameters value to run correctly
      call IniEpos(iEvent,ipart,itarg,decms)

      end

      subroutine crmc_f(iout,ievent,noutpart,impactpar,outpart,outpx
     +                  ,outpy,outpz,oute,outm,outstat)

***************************************************************
*   output: iout      - output type
*           ievent    - event number
*           noutpart  - number of stable particles produced
*           impactpar - impact parameter in fm
*           outpart   - particle ID (pdg code)
*           outpx     - particle momentum px
*           outpy     - particle momentum py
*           outpz     - particle momentum pz
*           oute      - particle energy
*           outm      - particle mass
*           outstat   - particle status code
*
***************************************************************
      implicit none
      include "epos.inc"
c     Output quantities
      integer noutpart,iout,ievent
      double precision impactpar
      integer outpart(*)
      double precision outpx(*), outpy(*), outpz(*)
      double precision oute(*), outm(*)
      integer outstat(*)

      double precision boostvec1,boostvec2,boostvec3,boostvec4,boostvec5
      double precision mass,ppp,xcount
      double precision ycm2det
      logical doBoost
      common/boostvars/ycm2det,doBoost
      data xcount / 0d0 /
      save

      integer i!,k

c     Calculate an inelastic event
      call aepos(-1)

c     Fix final particles and some event parameters
      call afinal

c     Fill HEP common
      call hepmcstore(iout)  !use hepmcstore for all models to be sure to get same vertex structure
c      call xInvMass(ievent)          !invariant mass distribution

c     optional Statistic information (only with debug level ish=1)
      call astati
      if(ish.ge.1) call bfinal

c     Print out (same as the one defined in input.optns)
      if(nhep.gt.nmxhep)then
        print *,'Warning : produced number of particles is too high'
        print *,'          increase nmxhep : ',nhep,' > ',nmxhep
c        stop
      endif
      noutpart=nhep
      impactpar=dble(bimevt)
c     define vec to boost from cm. to cms frame
      boostvec1=0d0
      boostvec2=0d0
      boostvec3=dsinh(ycm2det)
      boostvec4=dcosh(ycm2det)
      boostvec5=1d0
c      write(*,*)nevhep,nhep,boostvec3,boostvec4,ycm2det
      do i=1,nhep
c Put particle is on-shell (needed because of single/double precision pb)
          ppp  = sqrt( phep(1,i)**2 + phep(2,i)**2 + phep(3,i)**2)
          mass = (phep(4,i)+ppp)*(phep(4,i)-ppp)
          if(abs(mass-phep(5,i)**2)/max(1d2,ppp**2).gt.1d-3)   !not to count precision problems
     +         xcount=xcount+1d0
          mass = phep(5,i)      !use the true mass to avoid precision problem
c          if(abs(mass-phep(5,i)**2)/max(1d2,ppp**2).gt.1d-4)then
c            print *,mass,phep(5,i)**2,ppp,idhep(i)
c            stop
c          endif
          phep(4,i)=sqrt(phep(3,i)**2+phep(2,i)**2+phep(1,i)**2
     +                      +mass**2)     !force particles to be on-shell
c     boost output to cms frame
          if(doBoost.eqv..true..and.ycm2det.ne.0d0) then
            call utlob2(-1,boostvec1,boostvec2,boostvec3
     +           ,boostvec4,boostvec5
     +           ,vhep(1,i),vhep(2,i),vhep(3,i),vhep(4,i),-99)
            call utlob5dbl(-ycm2det
     +           ,phep(1,i), phep(2,i), phep(3,i), phep(4,i), mass)
            if( phep(4,i).ne. phep(4,i))print *,mass,idhep(i)
          endif
          outpart(i)=idhep(i)
          outpx(i)=phep(1,i)
          outpy(i)=phep(2,i)
          outpz(i)=phep(3,i)
          oute(i)=phep(4,i)
          outm(i)=phep(5,i)
          outstat(i)=isthep(i)
c      write(*,'(4x,i6,1x,4(e12.6,1x))')idhep(i),(vhep(k,i),k=1,4)
c         write(*,'(i5,3x,i2,2x,2i5,2x,2i5)')i,isthep(i)
c     *        ,jmohep(1,i),jmohep(2,i),jdahep(1,i),jdahep(2,i)
c         write(*,'(i10,1x,4(e12.6,1x))')idhep(i),(phep(k,i),k=1,4)
      enddo
      if(ievent.eq.nevent)then
        if(xcount.gt.0d0)print *,
     +       'Warning : negative mass for ',xcount,' particles !'
        if(model.le.1)call hnbdestroy
      endif

c     Write lhe file
      if(iout.eq.1)call lhesave(ievent)

      end


c-----------------------------------------------------------------------
      subroutine IniEpos(iEvent,ipart,itarg,decms)
c-----------------------------------------------------------------------
c     Update some parameters and define path to tab files here can be set
c     what particle is stable or not transfer number of event and debug
c     level to main program (to avoid epos.inc in main)
c-----------------------------------------------------------------------
      implicit none
      include "epos.inc"

      double precision decms
      integer ipart,itarg,idtrafo,iEvent

      nrevt=0
      nevent=iEvent             !number of events

      engy=sngl(decms)                 !reinitialize energy definitions
      elab=-1
      pnll=-1
      ekin=-1
      ecms=-1          !center of mass energy in GeV/c2
c     pnll=pproj                !beam momentum GeV/c


c     Projecticle definitions
      if (abs(ipart) .eq. 1 .or. abs(ipart) .eq. 1120) then
c     proton
         idprojin = sign(1120,ipart) !proton
         laproj = -1                !<0 means not a nucleus
         maproj =  1                !single particle
      elseif ( abs(ipart) .eq. 11 ) then
c     electron
         idprojin=1120
         laproj = -1             !targ Z
         maproj = 1            !targ A
      elseif (ipart .eq. 12) then
c     carbon
         idprojin=1120
         laproj = 6             !proj Z
         maproj = 12            !proj A
      elseif (ipart .eq. 208) then
c     lead
         idprojin=1120
         laproj = 82            !proj Z
         maproj = 208           !proj A
      elseif (abs(ipart) .eq. 120) then
c     pi+/-
         idprojin = ipart       !pi+/-
         laproj = -1            !<0 means not a nucleus
         maproj = 1             !single particle
      elseif (ipart.gt.10000)then
c nuclei
         idprojin=1120
         maproj=mod(ipart,10000)/10           !proj A
         laproj=mod(ipart,10000000)/10000     !proj Z
c PDG
      else
        idprojin=idtrafo('pdg','nxs',ipart)
        laproj = -1             !<0 means not a nucleus
        maproj = 1              !single particle
      endif

      if(idprojin.eq.99)then
         print *,'Warning : projectile particle not known : ',ipart
         print *,'          id particle must be +/-120(pi+/-)'
         print *,'          1(proton) 12(carbon) 208(lead) or PDG'
         stop
      endif

c     Target definitions : for nucleons, idtarg does not exist
c     Mass number matarg as well as charge, latarg, must be defined

c     idtarg = 1120             !proton
      if ( abs(itarg) .eq. 1  .or. abs(itarg) .eq. 1120) then
c     proton
         idtargin = sign(1120,itarg)
         latarg = -1             !<0 means not a nucleus
         matarg =  1             !single particle
      elseif ( abs(itarg) .eq. 11 ) then
c     electron
         idtargin=1120
         latarg = -1             !targ Z
         matarg = 1            !targ A
      elseif ( itarg .eq. 12 ) then
c     carbon
         idtargin=1120
         latarg = 6             !targ Z
         matarg = 12            !targ A
      elseif ( itarg .eq. 208 ) then
c     lead
         idtargin=1120
         latarg = 82            !targ Z
         matarg = 208           !targ A
c nuclei
      elseif (itarg.gt.10000)then
         idtargin=1120
         matarg=mod(itarg,10000)/10          !targ A
         latarg=mod(itarg,10000000)/10000    !targ Z
c PDG
      elseif (abs(itarg).eq.2112.or.abs(itarg).eq.2212)then
        idtargin=idtrafo('pdg','nxs',itarg)
        latarg = -1              !<0 means not a nucleus
        matarg =  1              !single particle
      else
         print *,'Warning : target particle not known : ',itarg
         print *,'          id particle must be +/-120(pi+/-)'
         print *,'          1(proton) 12(carbon) 208(lead) or PDG'
         stop

       endif

       if(abs(ipart).eq.11.and.abs(itarg).eq.11)iappl=6   !electron-positron

      call ainit

      end

c-----------------------------------------------------------------------
      subroutine lhesave(n)
c-----------------------------------------------------------------------
c     writes the results of a simulation into the file with unit ifdt
c     contains a description of the stored variables.
c     use Les Houches Event File as defined in hep-ph/0109068 for the
c     common block and hep-ph/0609017 for the XML output.
c     some code taken from example from Torbjrn Sjstrand
c     in http://www.thep.lu.se/~torbjorn/lhef
c-----------------------------------------------------------------------
      include 'epos.inc'

      integer id
      real taugm
C...User process event common block.
      INTEGER MAXNUP
      PARAMETER (MAXNUP=nmxhep)  !extend array for file production
c      PARAMETER (MAXNUP=500)
      INTEGER NUP,IDPRUP,IDUP,ISTUP,MOTHUP,ICOLUP
      DOUBLE PRECISION XWGTUP,SCALUP,AQEDUP,AQCDUP,PUP,VTIMUP,SPINUP
      COMMON/HEPEUP/NUP,IDPRUP,XWGTUP,SCALUP,AQEDUP,AQCDUP,IDUP(MAXNUP),
     &ISTUP(MAXNUP),MOTHUP(2,MAXNUP),ICOLUP(2,MAXNUP),PUP(5,MAXNUP),
     &VTIMUP(MAXNUP),SPINUP(MAXNUP)
      SAVE /HEPEUP/


C...set event info and get number of particles.
      NUP=nhep             !number of particles
      IDPRUP=nint(typevt)  !type of event (ND,DD,SD)
      XWGTUP=1d0           !weight of event
      SCALUP=-1d0          !scale for PDF (not used)
      AQEDUP=-1d0          !alpha QED (not relevant)
      AQCDUP=-1d0          !alpha QCD (not relevant)

C...Copy event lines, omitting trailing blanks.
C...Embed in <event> ... </event> block.
      write(ifdt,'(A)') '<event>'
      write(ifdt,*)NUP,IDPRUP,XWGTUP,SCALUP,AQEDUP,AQCDUP
      DO 220 i=1,nhep

c  store particle variables:
          IDUP(i)=idhep(i)
          if(isthep(i).eq.4)then
          ISTUP(i)=-9      !incoming particle
          else
          ISTUP(i)=isthep(i) !in LHEF:1=final, 2=decayed
          endif
          MOTHUP(1,i)=jmohep(1,i)
          MOTHUP(2,i)=jmohep(2,i)
          ICOLUP(1,i)=0        !color flow
          ICOLUP(2,i)=0        !color flow
          do J=1,5                !particle momentum (GeV/c)
            PUP(J,i)=phep(J,i)
          enddo
          id=idtrafo('pdg','nxs',idhep(i))
          call idtau(id,sngl(phep(4,i)),sngl(phep(5,i)),taugm)
          VTIMUP(i)=dble(taugm*(-alog(rangen())))*1d-12 !life time c*tau in mm
          if(VTIMUP(i).gt.dble(ainfin)
     &   .or.VTIMUP(i).ne.VTIMUP(i))VTIMUP(i)=ainfin
          SPINUP(i)=9           !polarization (not known)
          write(ifdt,*)IDUP(i),ISTUP(i),
     &      MOTHUP(1,i),MOTHUP(2,i),ICOLUP(1,i),ICOLUP(2,i),
     &      (PUP(J,i),J=1,5),VTIMUP(i),SPINUP(i)
  220 CONTINUE

c optional informations
      write(ifdt,*)'#geometry',bimevt,phievt

      write(ifdt,'(A)') '</event>'

      if(n.eq.nevent)then
C...Successfully reached end of event loop: write closing tag
        write(ifdt,'(A)') '</LesHouchesEvents>'
        write(ifdt,'(A)') ' '
        close(ifdt)
      endif

      return
      end


c-----------------------------------------------------------------------
      subroutine EposOutput(iFile)
c-----------------------------------------------------------------------
c Use EPOS to create lhe file output
c-----------------------------------------------------------------------
      include "epos.inc"
      character*(*) iFile

      istore=4
      nfndt=index(iFile,' ')  !final space important in bstora
      fndt(1:nfndt)=iFile(1:nfndt)
      kdtopen=0

      call bstora

c      nopen=0
c      ifop=52
c      open(unit=ifop,file=TRIM(iParam),status='old')
c      call aread
c      close(ifop)
      end

c-----------------------------------------------------------------------
      subroutine EposInput(iParam)
c-----------------------------------------------------------------------
c     Read informations (new options or parameter change) in the file
c     "epos.param". The unit "ifop" is used in aread. If not used, it will
c     use the default value of all parameters.
c-----------------------------------------------------------------------
      include "epos.inc"
      character*1000 iParam

      nopen=0
      ifop=52
      imax=index(iParam,' ')
      if(imax.gt.1)then
      open(unit=ifop,file=iParam(1:imax),status='old')
      call aread
      close(ifop)
      endif
      end


c-----------------------------------------------------------------------
      subroutine utLob5dbl(yboost,x1,x2,x3,x4,x5)
c-----------------------------------------------------------------------
c     Same as utlob5 but in double precision
c-----------------------------------------------------------------------
      implicit none
      double precision yboost,y,amt,x1,x2,x3,x4,x5
      amt=dsqrt(x5**2+x1**2+x2**2)
      y=dsign(1D0,x3)*dlog((x4+dabs(x3))/amt)
      y=y-yboost
      x4=amt*dcosh(y)
      x3=amt*dsinh(y)
      return
      end

c-----------------------------------------------------------------------
      subroutine crmc_xsection_f(xsigtot,xsigine,xsigela,xsigdd,xsigsd
     &                          ,xsloela,xsigtotaa,xsigineaa,xsigelaaa)
c-----------------------------------------------------------------------
c     Provide all components of cross-section (slow for EPOS with nuclei)
c-----------------------------------------------------------------------
      implicit none
      include 'epos.inc'
      double precision xsigtot,xsigine,xsigela,xsigdd,xsigsd
     &                ,xsloela,xsigtotaa,xsigineaa,xsigelaaa

      xsigtot   = dble( sigtot   )
      xsigine   = dble( sigine   )
      xsigela   = dble( sigela   )
      xsigdd    = dble( sigdd    )
      xsigsd    = dble( sigsd    )
      xsloela   = dble( sloela   )
c Nuclear cross section only if needed
      xsigtotaa = 0d0
      xsigineaa = 0d0
      xsigelaaa = 0d0
      if(maproj.gt.1.or.matarg.gt.1)then
        if(model.eq.1)then
          call crseaaEpos(sigtotaa,sigineaa,sigcutaa,sigelaaa)
        else
          call crseaaModel(sigtotaa,sigineaa,sigcutaa,sigelaaa)
        endif
        xsigtotaa = dble( sigtotaa )
        xsigineaa = dble( sigineaa )
        xsigelaaa = dble( sigelaaa )
      endif

      return
      end

c-----------------------------------------------------------------------
      double precision function crmc_inelastic_xs()
c-----------------------------------------------------------------------
c     return current inelastic cross-section (fast)
c-----------------------------------------------------------------------
      implicit none
      include 'epos.inc'

      crmc_inelastic_xs   = dble( sigine )
c Nuclear cross section only if needed
      if(maproj.gt.1.or.matarg.gt.1)crmc_inelastic_xs=dble( sigineaa )

      return
      end

c-----------------------------------------------------------------------
      function rangen()
c-----------------------------------------------------------------------
c     generates a random number
c-----------------------------------------------------------------------
      include 'epos.inc'
      double precision dranf
 1    rangen=sngl(dranf(dble(irandm)))
      if(rangen.le.0.)goto 1
      if(rangen.ge.1.)goto 1
      if(irandm.eq.1)write(ifch,*)'rangen()= ',rangen

      return
      end

c-----------------------------------------------------------------------
      double precision function drangen(dummy)
c-----------------------------------------------------------------------
c     generates a random number
c-----------------------------------------------------------------------
      include 'epos.inc'
      double precision dummy,dranf
      drangen=dranf(dummy)
      if(irandm.eq.1)write(ifch,*)'drangen()= ',drangen

      return
      end
c-----------------------------------------------------------------------
      function cxrangen(dummy)
c-----------------------------------------------------------------------
c     generates a random number
c-----------------------------------------------------------------------
      include 'epos.inc'
      double precision dummy,dranf
      cxrangen=sngl(dranf(dummy))
      if(irandm.eq.1)write(ifch,*)'cxrangen()= ',cxrangen

      return
      end



c Random number generator from CORSIKA *********************************




