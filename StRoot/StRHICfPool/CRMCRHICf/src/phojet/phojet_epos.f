c 13.04.2010 Link routines between PHOJET and EPOS.
c author T. Pierog


c-----------------------------------------------------------------------
      subroutine IniPhojet
c-----------------------------------------------------------------------
c Primary initialization for PHOJET
c-----------------------------------------------------------------------
      include 'epos.inc'
C  event debugging information
      INTEGER NMAXD
      PARAMETER (NMAXD=100)
      INTEGER IDEB,KSPOM,KHPOM,KSREG,KHDIR,KACCEP,KSTRG,KHTRG,KSLOO,
     &        KHLOO,KSDPO,KHDPO,KEVENT,KSOFT,KHARD
      COMMON /PODEBG/ IDEB(NMAXD),KSPOM,KHPOM,KSREG,KHDIR,KACCEP,KSTRG,
     &                KHTRG,KSLOO,KHLOO,KSDPO,KHDPO,KEVENT,KSOFT,KHARD
C  particle decay data
      INTEGER MDCY,MDME,KFDP
      DOUBLE PRECISION  BRAT
      COMMON/PYDAT3/MDCY(500,3),MDME(8000,2),BRAT(8000),KFDP(8000,5)
C  general process information
      INTEGER IPROCE,IDNODF,IDIFR1,IDIFR2,IDDPOM,IPRON
      COMMON /POPRCS/ IPROCE,IDNODF,IDIFR1,IDIFR2,IDDPOM,IPRON(15,4)
C  model switches and parameters
      CHARACTER*8 MDLNA
      INTEGER ISWMDL,IPAMDL
      DOUBLE PRECISION PARMDL
      COMMON /POMDLS/ MDLNA(50),ISWMDL(50),PARMDL(400),IPAMDL(400)
C...Parameters from Pythia
      double precision PARI,PARP,PARU,PARJ
      COMMON/PYPARS/MSTP(200),PARP(200),MSTI(200),PARI(200)
      COMMON/PYDAT1/MSTU(200),PARU(200),MSTJ(200),PARJ(200)

      call utpri('inipho',ish,ishini,6)
      write(ifmt,'(a,i6)')'initialize Phojet ...'

      do i=1,nmaxd
        IDEB(i)=0
      enddo

C... PHOJET initialization
      CALL PHO_INIT(-1,IREJ)
      if(irej.eq.1)stop"problem with PHOJET ini"

      if(ish.ge.3)then
        do i=1,nmaxd
          IDEB(i)=ish-2
        enddo
      endif

c load tuned parameters for JETSET 7.3 (3) and prevent strange/charm hadrons from decaying (<0)
      ISWMDL(6)=-3

c epos syntax to allow (or not) particle decay in Pythia  (used by phojet)
c (part taken from epos-dky: hdecas)
c This should be after PHO_INIT
c used only if ISWMDL(6)>0 but better not to decay in Phojet because history of mpother/daughter is lost between phojet and jetset so only final particles are given.
c first set all decays to 0 (no decay) in Jetset (Pythia)
      do i=100,500
        MDCY(i,1)=0
      enddo
      if(ISWMDL(6).ge.0)call IniDkyJetset

C  change/set some PHOJET parameters (optional)

C  non-diffractive elastic scattering (1 - on, 0 - off)
      IPRON(1,1) = 1
C  elastic scattering
      IPRON(2,1) = 0
C  quasi-elastic scattering (for incoming photons only)
      IPRON(3,1) = 0
C  central diffration (double-pomeron scattering)
      IPRON(4,1) = 1
C  particle 1 single diff. dissociation
      IPRON(5,1) = 1
C  particle 2 single diff. dissociation
      IPRON(6,1) = 1
C  double diff. dissociation
      IPRON(7,1) = 1
C  direct photon interaction (for incoming photons only)
      IPRON(8,1) = 0


      egymin=10.
      egymax=1e7
      irescl=0
      

      call utprix('inipho',ish,ishini,6)
      end


c-----------------------------------------------------------------------
      subroutine IniEvtPho
c-----------------------------------------------------------------------
c Initialization for each type of event (for given proj, targ and egy)
c-----------------------------------------------------------------------
      include 'epos.inc'
      common/geom/rmproj,rmtarg,bmax,bkmx
      double precision p1(4),p2(4),sigmax


      if(matarg.gt.1.or.maproj.gt.1)
     &  call utstop('Mass too big for PHOJET (Mtrg=1, Mprj=1) !&')
      idprj=idtrafo('nxs','pdg',idproj)
      idtrg=idtrafo('nxs','pdg',idtarg)
      if(abs(idproj).ne.1120)
     &  call utstop('projectile no allowed in PHOJET !&')
      if(abs(idtarg).ne.1120)
     &  call utstop('target no allowed in PHOJET !&')
      if(bminim.gt.0.or.bmaxim.lt.1000)
     &  write(ifmt,*)'Only min bias event in PHOJET ... no b !'
      call iclass(idproj,iclpro)
      bminim=0.
      bmaxim=10000.
      bmax=10.+maproj+matarg


c set target
      call pho_setpar(1,idtrg,0,0d0)
      p1(1)=0d0
      p1(2)=0d0
      p1(3)=dble(-pnullx)
      p1(4)=sqrt(dble(pnullx)**2+dble(amproj)**2)
c set projectile
      call pho_setpar(2,idprj,0,0d0)
      p2(1)=0d0
      p2(2)=0d0
      p2(3)=dble(pnullx)
      p2(4)=sqrt(dble(pnullx)**2+dble(amproj)**2)
c initialize event
      call pho_event(-1,p1,p2,sigmax,IREJ)


      phoincs=sngl(sigmax)
      if(engy.lt.egymin)phoincs=0.          !below egymin, no interaction

c initialize cross-sections
      call xsigma

      if(ish.ge.2)write(ifch,*)
     &  'PHOJET used with (E,idproj,idtarg,xs) ',ecms,idproj,idtarg
     &                                          ,phoincs


      return
      end

c-----------------------------------------------------------------------
      subroutine emspho(iret)
c-----------------------------------------------------------------------
c  call PHOJET to simulate interaction
c-----------------------------------------------------------------------
      include 'epos.inc'
      common/geom/rmproj,rmtarg,bmax,bkmx
      double precision p1(4),p2(4)
C  PHOJET
C...Note that HEPEVT is in double precision according to LEP 2 standard.
      PARAMETER (NMXHEPP=4000)
      COMMON/POEVT1/NEVHEPP,NHEPP,ISTHEPP(NMXHEPP),IDHEPP(NMXHEPP),
     &JMOHEPP(2,NMXHEPP),JDAHEPP(2,NMXHEPP),PPHEP(5,NMXHEPP)
     &,PVHEP(4,NMXHEPP)
      DOUBLE PRECISION PPHEP,PVHEP

      double precision sigcur
      integer nptlhep(NMXHEPP)

      iret=0
      ncol=0
      b1=bminim
      b2=min(bmax,bmaxim)
      a=pi*(b2**2-b1**2)

      if(a.gt.0..and.rangen().gt.phoincs/10./a)goto 1001   !no interaction
      if(ish.ge.3)call alist('Determine PHOJET Production&',0,0)

      nptl=0
      
      call conre
      call conwr

      p1(1)=dble(pptl(1,2))
      p1(2)=dble(pptl(2,2))
      p1(3)=dble(pptl(3,2))
      p1(4)=dble(pptl(4,2))
      p2(1)=dble(pptl(1,1))
      p2(2)=dble(pptl(2,1))
      p2(3)=dble(pptl(3,1))
      p2(4)=dble(pptl(4,1))
      call PHO_EVENT(1,p1,p2,sigcur,IREJ)
      if(IREJ.ne.0.or.nhepp.le.0)goto 1001    !no interaction

      nptl=0          !reset beam particles to get it from phojet
      ncol=1
      nevt=1
      kolevt=ncol
      npjevt=maproj
      ntgevt=matarg
      pmxevt=pnll
      egyevt=engy
      bimevt=0.
      bimp=0.
      phievt=0.
      phi=0.

      anintine=anintine+1.

c set projectile and target as non final particles        
      istptl(1)=1
      istptl(2)=1

        if(ish.ge.5)write(ifch,'(a,i5)')
     $         ' number of particles from PHOJET :',NHEPP
        do 500 k=1,NHEPP
          nptlhep(k)=0

c LLIST is the code of final particle, P - its 4-momentum and mass.
          ic=IDHEPP(k)
            
          if(ish.ge.7)write(ifch,'(a,i5,a,i5,2a,i3,1x,4(e10.4,1x))')
     $       ' PHOJET particle ',k,' id :',ic,' before conversion'
     $     , ' momentum :',ISTHEPP(k),(sngl(PPHEP(i,k)),i=1,5)

          id = idtrafostatus('pdg','nxs',ic,istatus)
          if(istatus.ne.0)then
            if(ISTHEPP(k).eq.1)then
              print  *,ic,id,istatus
              stop "pdg id from Phojet unknown !"
            else
              if(ish.ge.7)write(ifch,'(a)')
     &      'Not final particle with unknown id: skipped ...'
              goto 500
            endif
          endif

c          IF(ISTHEPP(k).GE.1.AND.ISTHEPP(k).LE.2)THEN
          IF(ISTHEPP(k).EQ.1.OR.k.LE.2)THEN
c final particle
            nptl=nptl+1
            nptlhep(k)=nptl
            istptl(nptl)=ISTHEPP(k)-1
          else
            if(ish.ge.7)write(ifch,'(a)')
     &      'Not final particle: skipped ...'
            goto 500 
          endif
          if(nptl.gt.mxptl)call utstop('PHOJET: mxptl too small&')

          if(ish.ge.7)write(ifch,'(a,i5,a,i5,a)')
     $         ' epos particle ',nptl,' id :',id,' after conversion'
            

          pptl(1,nptl)=sngl(PPHEP(1,k))   !P_x
          pptl(2,nptl)=sngl(PPHEP(2,k))   !P_y
          pptl(3,nptl)=sngl(PPHEP(3,k))   !P_z
          pptl(4,nptl)=sngl(PPHEP(4,k))   !E
          pptl(5,nptl)=sngl(PPHEP(5,k))   !mass
          iorptl(nptl)=0
          if(JMOHEPP(1,k).gt.0.and.JMOHEPP(1,k).le.mxptl)then
            iorptl(nptl)=nptlhep(JMOHEPP(1,k))
          endif
          ida=iabs(id)
          if(nptl.le.2)then
            iorptl(nptl)=-1
          elseif(iorptl(nptl).gt.0.and.iorptl(nptl).le.2
     &           .and.ida.gt.100.and.ida.lt.10000 !remnant
     &           .and.mod(ida,100).ne.0)then
            id=sign(ida*100+99,id)
            ida=iabs(id)
          endif
          ityptl(nptl)=0
          if(ida.gt.10000.and.mod(ida-99,10).eq.0)then !remnant
            if(iorptl(nptl).eq.1)then !projectile
              ityptl(nptl)=40
            else                !target
              ityptl(nptl)=10
            endif
          elseif(iorptl(nptl).gt.2)then
            ityptl(nptl)=ityptl(iorptl(nptl))+1
            if(mod(ityptl(nptl),10).eq.0)ityptl(nptl)=ityptl(nptl)-1
          elseif(iorptl(nptl).ge.0)then
            ityptl(nptl)=21
          endif
          if(ida.le.9)then      !parton
            istptl(nptl)=20+istptl(nptl)
          elseif(ida.gt.1000.and.mod(ida,100).eq.0)then !diquark
            istptl(nptl)=20+istptl(nptl)
          elseif(ida.gt.10000.and.mod(ida-99,10).eq.0)then
            istptl(nptl)=40+istptl(nptl)    !remnant
          elseif(id.gt.800000000)then
            istptl(nptl)=30+istptl(nptl)    !hard Pomeron
            ityptl(nptl)=31
          endif
          jorptl(nptl)=0
          if(JMOHEPP(2,k).gt.0.and.JMOHEPP(2,k).le.mxptl)then
            jorptl(nptl)=nptlhep(JMOHEPP(2,k))
          endif
          ifrptl(1,nptl)=-JDAHEPP(1,k)      !updated later
          ifrptl(2,nptl)=-JDAHEPP(2,k)      !updated later
          xorptl(1,nptl)=sngl(PVHEP(1,k))*1e12
          xorptl(2,nptl)=sngl(PVHEP(2,k))*1e12
          xorptl(3,nptl)=sngl(PVHEP(3,k))*1e12
          xorptl(4,nptl)=sngl(PVHEP(4,k))*1e12
          tivptl(1,nptl)=0.
          tivptl(2,nptl)=0.
          idptl(nptl)=id
          
            
          if(ish.ge.5)write(ifch,'(a,i5,a,i5,a,4(e10.4,1x),f6.3)')
     $         ' particle from PHOJET ',nptl,' id :',idptl(nptl)
     $         , ' momentum :',(pptl(i,nptl),i=1,5)


 500      continue

c update daughter index
      do i=1,nptl
        if(ifrptl(1,i).lt.0)then
          ifrptl(1,i)=nptlhep(-ifrptl(1,i))
        endif
        if(ifrptl(2,i).lt.0)then
          ifrptl(2,i)=nptlhep(-ifrptl(2,i))
        endif
      enddo


1000  return

1001  iret=-1 
      goto 1000 

      end



c------------------------------------------------------------------------------
      function phocrse(ek,mapro,matar,id)
c------------------------------------------------------------------------------
c inelastic cross section of PHOJET
c if id=0, target = air
c ek - kinetic energy in GeV
c maproj - projec mass number     (1<maproj<64)
c matarg - projec mass number
c id - target id (0=air)
c------------------------------------------------------------------------------
      double precision EE
C  cross sections
      INTEGER IPFIL,IFAFIL,IFBFIL
      DOUBLE PRECISION SIGTOT,SIGELA,SIGVM,SIGINE,SIGNDF,SIGDIR,
     &                 SIGLSD,SIGHSD,SIGLDD,SIGHDD,SIGCDF,
     &                 SIGPOM,SIGREG,SIGHAR,SIGTR1,SIGTR2,SIGLOO,
     &                 SIGDPO,SIG1SO,SIG1HA,SLOEL,SLOVM,SIGCOR,
     &                 FSUP,FSUD,FSUH,ECMFIL,P2AFIL,P2BFIL
      COMMON /POCSEC/ SIGTOT,SIGELA,SIGVM(0:4,0:4),SIGINE,SIGNDF,SIGDIR,
     &                SIGLSD(2),SIGHSD(2),SIGLDD,SIGHDD,SIGCDF(0:4),
     &                SIGPOM,SIGREG,SIGHAR,SIGTR1(2),SIGTR2(2),SIGLOO,
     &                SIGDPO(4),SIG1SO,SIG1HA,SLOEL,SLOVM(4,4),SIGCOR,
     &                FSUP(2),FSUD(2),FSUH(2),ECMFIL,P2AFIL,P2BFIL,
     &                IPFIL,IFAFIL,IFBFIL

      phocrse=0d0
      if(mapro.ne.1.and.matar.ne.1)return
      if(id.eq.0)return
      call idmass(1120,am)
      elab=ek+am
      EE=dble(sqrt( 2.*elab*am+2.*am**2 ))
      call PHO_XSECT(1,1,EE)
      phocrse=sngl(SIGTOT-SIGELA)
      return
      end

c--------------------------------------------------------------------
      double precision function PHO_RNDM(dum)
c--------------------------------------------------------------------
c random number generator
c--------------------------------------------------------------------
      include 'epos.inc'
      double precision dum,drangen

      PHO_RNDM=drangen(dum)
      if(irandm.eq.1)write(ifch,*)'PHO_RNDM()= ',PHO_RNDM,dum

      return
      end
 
c--------------------------------------------------------------------
      subroutine PHO_PHIST(idum,dum)
c--------------------------------------------------------------------
c random number generator
c--------------------------------------------------------------------
      double precision dum,dum2

      dum2=dum
      idum2=idum

      return
      end
 
c--------------------------------------------------------------------
      subroutine PHO_LHIST(idum,dum)
c--------------------------------------------------------------------
c random number generator
c--------------------------------------------------------------------
      double precision dum,dum2

      dum2=dum
      idum2=idum

      return
      end
 


