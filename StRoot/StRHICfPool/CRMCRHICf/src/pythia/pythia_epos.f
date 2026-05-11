c 30.06.2003 Link routines between PYTHIA and epos.
c author T. Pierog

c-----------------------------------------------------------------------
      subroutine IniPythia
c-----------------------------------------------------------------------
c Primary initialization for Pythia
c-----------------------------------------------------------------------
      include 'epos.inc'

      double precision PARU,PARJ,PARP,PARI,CKIN
C...Parameters.
      COMMON/PYDAT1/MSTU(200),PARU(200),MSTJ(200),PARJ(200)
C...Parameters.
      COMMON/PYPARS/MSTP(200),PARP(200),MSTI(200),PARI(200)
C...Selection of hard scattering subprocesses.
      COMMON/PYSUBS/MSEL,MSELPD,MSUB(500),KFIN(2,-40:40),CKIN(200)

      call utpri('inipyt',ish,ishini,6)
      write(ifmt,'(a,i6)')'initialize Pythia ...'

c unit of the output (and here should be placed all this kind of setting)
       MSTU(11)=ifch
       MSTU(4)=9990      !Number of possible particle in one event (should be change in common pyjets)
       MSEL=0             !Full hadronic interactions (p176) (MSEL=2 without elastic)
       MSUB(11)=1         !qq->qq (QFD)
       MSUB(12)=1         !qq->qq (QCD)
       MSUB(13)=1         !qqb->gg
       MSUB(28)=1         !qg->qg
       MSUB(53)=1         !gg->qqb
       MSUB(68)=1         !gg->gg
       MSUB(92)=1         !single diff AB->XB
       MSUB(93)=1         !single diff AB->AX
       MSUB(94)=1         !double diffraction
       MSUB(95)=1         !low pt (soft)
       MSUB(96)=1         !semi-hard QCD 2->2
c      MSUB(...)=...      !To be changed to use specific subprocesses with MSEL=0 (p177)
       MSTP(82)=4         !Multiple interaction with varying impact parameter (p313)
       PARP(2)=8.         !Minimum energy (change calculation time!)

      call PYTUNE(ipytune)   !default Perugia Tune (2011) (#350)
c      MSTP(52)=1          !internal pdf
c      MSTP(51) = 7        !CTEQ5L
c Information for epos for minimum and maximum energy
      egymin=sngl(PARP(2))
      egymax=egymax

c Do not rescale energy for energy conservation
      irescl=0
      

      call utprix('inipyt',ish,ishini,6)
      end

c-----------------------------------------------------------------------
      subroutine IniEvtPyt
c-----------------------------------------------------------------------
c Initialization for each type of event (for given proj, targ and egy)
c-----------------------------------------------------------------------
      include 'epos.inc'
      common/geom/rmproj,rmtarg,bmax,bkmx
      character*160 NameProj,NameTarg,NameDecay
      double precision egypyt
c Integer function to translate normal id from pythia to compact one.
      integer PYCOMP
C...Parameters from Pythia
      double precision PARI,PARP,PARU,PARJ
      COMMON/PYPARS/MSTP(200),PARP(200),MSTI(200),PARI(200)
      COMMON/PYDAT1/MSTU(200),PARU(200),MSTJ(200),PARJ(200)



      NameProj='--- '
      NameTarg='--- '
        if(maproj.gt.1.or.matarg.gt.1)
     &  call utstop('Nucleus not allowed in Pythia !&')
        if(bminim.gt.0.or.bmaxim.lt.1000)
     &  write(ifmt,*)'Only min bias event in Pythia ... no b !'
        bminim=0.
        bmaxim=10000.
        bmax=5.

        if(idproj.eq.1120)NameProj='p+ '
        if(idproj.eq.-1120)NameProj='pbar- '
        if(idproj.eq.1220)NameProj='n0 '
        if(idproj.eq.-1220)NameProj='nbar0 '
        if(idproj.eq.110)NameProj='p0 '
        if(idproj.eq.120)NameProj='pi+ '
        if(idproj.eq.-120)NameProj='pi- '
        if(idproj.eq.130)NameProj='k+ '
        if(idproj.eq.-130)NameProj='k- '
        if(idproj.eq.20.or.idproj.eq.230)NameProj='ks0 '
        if(idproj.eq.-20.or.idproj.eq.-230)NameProj='kl0 '

        if(idtarg.eq.1120)NameTarg='p+ '
        if(idtarg.eq.-1120)NameTarg='pbar- '
        if(idtarg.eq.1220)NameTarg='n0 '
        if(idtarg.eq.-1220)NameTarg='nbar0 '
        if(idtarg.eq.110)NameTarg='p0 '
        if(idtarg.eq.120)NameTarg='pi+ '
        if(idtarg.eq.-120)NameTarg='pi- '
        if(idtarg.eq.130)NameTarg='k+ '
        if(idtarg.eq.-130)NameTarg='k- '
        if(idtarg.eq.20.or.idtarg.eq.230)NameTarg='ks0 '
        if(idtarg.eq.-20.or.idtarg.eq.-230)NameTarg='kl0 '

        i=1
        j=0
        k=1
        l=0
        if(iappl.eq.7)then
          NameProj='p+ '
          NameTarg='p+ '
        endif
        call utword(NameProj,i,j,0)
        if(NameProj(i:j).eq.'---')
     &       call utstop('Proj not allowed in Pythia !&')
        call utword(NameTarg,k,l,0)
        if(NameTarg(k:l).eq.'---')
     &       call utstop('Targ not allowed in Pythia !&')
        
        egypyt=dble(engy)
        if(ish.ge.2)write(ifch,*)
     &       'Pythia used with (E,proj,targ) in CMS (',egypyt
     &       ,', ',NameProj(i:j),', ',NameTarg(k:l),')'
        call PYINIT('CMS',NameProj(i:j),NameTarg(k:l),egypyt)

c Inelastic cross section

      pytincs=pytcrse(idum)
      if(engy.lt.egymin)pytincs=0.          !below egymin, no interaction
      call xsigma
      
c Set decay according to EPOS settings

      call IniDkyJetset

c do not use EPOS decay
      if(iappl.ne.7)ifrade=0

      return
      end

c-----------------------------------------------------------------------
      subroutine emspyt(iret,ip)
c-----------------------------------------------------------------------
c  call pythia to simulate interaction
c-----------------------------------------------------------------------
      include 'epos.inc'
      common/geom/rmproj,rmtarg,bmax,bkmx
C...HEPEVT commonblock by default in epos.inc
C...Parameters from Pythia
      double precision PARI,PARP
      COMMON/PYPARS/MSTP(200),PARP(200),MSTI(200),PARI(200)
      double precision PE,THE,PHI
      dimension ipy2ep(mxptl)


      iret=0

      if(ip.le.0)then        !normal interaction
      ncol=0
      b1=bminim
      b2=min(bmax,bmaxim)
      a=pi*(b2**2-b1**2)

      if(a.gt.0..and.rangen().gt.pytincs/10./a)goto 1001   !no interaction
      if(ish.ge.3)call alist('Determine Pythia Production&',0,0)

 10   nptl=0
      call PYEVNT               !generate event
c      call PYEDIT(2)            !To remove unknown and decayed particles
      call PYHEPC(1)            !convert tu HEPEVT standart

      if(MSTI(1).eq.91)goto 10   !elastic scattering not counted
      typevt=1
      if(MSTI(1).eq.94)typevt=2   !DD
      if(MSTI(1).eq.93)typevt=-4   !SD
      if(MSTI(1).eq.92)typevt=4   !SD

      ncol=1
      nevt=1
      kolevt=nint(PARI(61))
      npjevt=maproj
      ntgevt=matarg
      pmxevt=pnll
      egyevt=sngl(PARI(11))
      bimevt=0.
      bimp=0.
      phievt=0.
      anintine=anintine+1.

      else            !decay of one particle

C  FILL PYTHIA COMMONS WITH ONE PARTICLE TO TREAT IT''S DECAY
        KF    = idtrafo('nxs','pdg',idptl(ip))
        PE    = pptl(4,ip)
        pp    = sqrt((pptl(4,ip)+pptl(5,ip))
     &        *max(0.,pptl(4,ip)-pptl(5,ip)))
        if(pp.gt.0.)then
          THE   = acos(max(-1.,min(1.,pptl(3,ip)/pp)))
          PHI   = 0.d0
          if(abs(THE).gt.0.d0)PHI=acos(max(-1.,
     &                                 min(1.,pptl(1,ip)/pp/sin(THE))))
        else
          PE=pptl(5,ip)
          THE=0.d0
          PHI=0.d0
        endif
C  WITH ARGUMENT II = 0 AUTOMATICALLY PYEXEC IS CALLED
        II    = 0
        CALL PY1ENT( II, KF, PE, THE, PHI )
        call PYHEPC(1)          !convert tu HEPEVT standart
      endif

c      call conre
c      call conwr

      n=nptl
      do 99 is=1,NHEP            !number of particle in event
        if(ISTHEP(is).le.0.or.ISTHEP(is).gt.3)goto 99
        if(ip.gt.0.and.ISTHEP(is).ne.1)goto 99
        id = idtrafostatus('pdg','nxs',ic,istatus)
        if(istatus.ne.0.and.ISTHEP(is).ne.1)goto 99
        n=n+1
        ipy2ep(is)=n
 99   continue
      do 100 is=1,NHEP            !number of particle in event

        ic=IDHEP(is)
        if(ish.ge.7)write(ifch,'(a,i5,a,i5,2a,i3,1x,4(e10.4,1x))')
     $       ' Pythia particle ',is,' id :',ic,' before conversion'
     $     , ' momentum :',ISTHEP(is),(PHEP(k,is),k=1,4)

        if(ISTHEP(is).le.0.or.ISTHEP(is).gt.3)goto 100
        if(ip.gt.0.and.ISTHEP(is).ne.1)goto 100
        id = idtrafostatus('pdg','nxs',ic,istatus)
        if(istatus.ne.0)then
          if(ISTHEP(is).eq.1)then
            print  *,ic,id,istatus
            stop "pdg id from Pythia unknown !"
          else
            goto 100
          endif
        endif

            nptl=nptl+1
            if(nptl.gt.mxptl)call utstop('PYTHIA: mxptl too small&')
            if(ish.ge.7)write(ifch,'(a,i5,a,i10,a)')
     $       ' epos particle ',nptl,' id :',id,' after conversion'

            am=sngl(phep(5,is))
            pptl(1,nptl)=sngl(phep(1,is)) !P_x
            pptl(2,nptl)=sngl(phep(2,is)) !P_y
            pptl(3,nptl)=sngl(phep(3,is)) !P_z
            pptl(4,nptl)=sngl(phep(4,is)) !E
            pptl(5,nptl)=am              !mass
            iorptl(nptl)=0
            if(JMOHEP(1,is).gt.0.and.JMOHEP(1,is).le.mxptl)then
              iorptl(nptl)=ipy2ep(JMOHEP(1,is))
            endif
            ida=iabs(id)
            if(nptl.le.2)then
              iorptl(nptl)=-1
            elseif(iorptl(nptl).le.2.and.ida.gt.100.and.ida.lt.10000   !remnant
     &             .and.mod(ida,100).ne.0.and.ISTHEP(is).ne.1)then
              id=sign(ida*100+99,id)
              ida=iabs(id)
            endif
            ityptl(nptl)=0
            if(ida.gt.10000.and.mod(ida-99,10).eq.0)then  !remnant
              if(iorptl(nptl).eq.1)then     !projectile
                ityptl(nptl)=40
              else                          !target
                ityptl(nptl)=10
              endif
            elseif(iorptl(nptl).gt.2)then
              ityptl(nptl)=ityptl(iorptl(nptl))+1
              if(mod(ityptl(nptl),10).eq.0)ityptl(nptl)=ityptl(nptl)-1
            elseif(iorptl(nptl).ge.0)then
              ityptl(nptl)=21
            endif
            if(ISTHEP(is).eq.1)then
              istptl(nptl)=0
            else
              if(ida.le.9)then           !parton
                istptl(nptl)=21
              elseif(ida.gt.1000.and.mod(ida,100).eq.0)then     !diquark
                istptl(nptl)=21
              elseif(ida.gt.10000.and.mod(ida-99,10).eq.0)then
                istptl(nptl)=41                !remnant
              elseif(id.gt.800000000)then
                istptl(nptl)=31                !hard Pomeron
                ityptl(nptl)=31
              else
                istptl(nptl)=1
              endif
            endif
            jorptl(nptl)=0
            if(JMOHEP(2,is).gt.0.and.JMOHEP(2,is).le.mxptl)then
              jorptl(nptl)=ipy2ep(JMOHEP(2,is))
            endif
            ifrptl(1,nptl)=0
            if(JDAHEP(1,is).gt.0.and.JDAHEP(1,is).le.mxptl)then
              ifrptl(1,nptl)=ipy2ep(JDAHEP(1,is))
            endif
            ifrptl(2,nptl)=0
            if(JDAHEP(2,is).gt.0.and.JDAHEP(2,is).le.mxptl)then
              ifrptl(2,nptl)=ipy2ep(JDAHEP(2,is))
            endif
            xorptl(1,nptl)=sngl(vhep(1,is))*1e12 !x vertex position
            xorptl(2,nptl)=sngl(vhep(2,is))*1e12 !y vertex position
            xorptl(3,nptl)=sngl(vhep(3,is))*1e12 !z vertex position
            xorptl(4,nptl)=sngl(vhep(4,is))*1e12 !production time
            tivptl(1,nptl)=sngl(vhep(4,is))*1e12 !production time
            if(ifrptl(1,nptl).gt.0)then
              tivptl(2,nptl)=sngl(vhep(4,JDAHEP(1,is)))*1e12 !decay time (daughter's production time)
            endif
            idptl(nptl)=id

            
            if(ish.ge.5)write(ifch,'(a,i5,a,i5,a,4(e10.4,1x),f6.3)')
     $       ' particle from pythia ',nptl,' id :',idptl(nptl)
     $  , ' momentum :',(pptl(k,nptl),k=1,5)


 100  continue


      nhpevt=MSTI(31)

1000  return

1001  iret=-1 
      goto 1000 

      end

c------------------------------------------------------------------------------
      function pytcrse(idum)
c------------------------------------------------------------------------------
c inelastic cross section of pythia
c initialization done in PYINIT
c------------------------------------------------------------------------------
C...Total cross sections.
      double precision SIGT
      COMMON/PYINT7/SIGT(0:6,0:6,0:5)

      pytcrse=sngl(SIGT(0,0,0)-SIGT(0,0,1))        !total-elastic

      return
      end


