c 06.01.2004 Link routines between HIJING and epos.
c author T. Pierog

c-----------------------------------------------------------------------
      subroutine IniHijing
c-----------------------------------------------------------------------
c Primary initialization for Hijing
c-----------------------------------------------------------------------
      include 'epos.inc'
      common/geom/rmproj,rmtarg,bmax,bkmx

C...Parameters.
      COMMON/HIPARNT/HIPR1(100), IHPR2(50), HINT1(100), IHNT2(50) 
      COMMON/LUDAT1/MSTU(200),PARU(200),MSTJ(200),PARJ(200)
      COMMON/PYHIPARS/MSTP(200),PARP(200),MSTI(200),PARI(200)

      call utpri('inihij',ish,ishini,6)
      write(ifmt,'(a,i6)')'initialize Hijing ...'

c unit of the output (and here should be placed all this kind of setting)
       MSTU(11)=ifch
       MSTU(4)=9000      !Number of possible particle in one event (should be change in common pyjets)
       IHPR2(10)=0       !switch for warning messages
       if(ish.ge.2)IHPR2(10)=ish
       IHPR2(14)=0        !Full hadronic interactions (p21) (no elastic interactions)
       IHPR2(4)=1         !switch for Jet Quenching (default=1) 
       IHPR2(5)=1         !switch for the pT kink due to soft interactions (default=1) 
       IHPR2(6)=1         !switch for the nuclear effect on the parton distribution such as shadowing  (default=1)
       IHPR2(8)=10        !maximum number of hard scattering per pair interaction
       if(ishpom.eq.0)IHPR2(8)=0         !no hard
       IHPR2(19)=1        !switch for initial state soft interaction
       if(isopom.eq.0)IHPR2(19)=0        !no soft
       IHPR2(11)=1        !(D=1) choice of baryon production model.
                                 !=0: no baryon-antibaryon pair production, initial diquark treated as a unit.
                                 !=1: diquark-antidiquark pair production allowed, initial diquark treated as a unit.
                                 !=2: diquark-antidiquark pair production allowed, with the possibility for diquark to split according to the 'popcorn' scheme (see the documentation of JETSET 7.2).
       IHPR2(12)=0        !decay defined by epos options (see IniEvtHij)
       IHPR2(13)=1        !switch for single diffractive event
       IHPR2(18)=0        !switch for B-quark production (if on, C-charm off)
       IHPR2(21)=1        !option to keep information about all particles
       HIPR1(11)=2.0      ! (D=2.0 GeV/c) minimum P_T of a jet which will interact with excited nuclear matter. When the P_T of a jet is smaller than HIPR1(11) it will stop interacting further.
       HIPR1(12)=1.0      !(D=1.0 fm) transverse distance between a traversing jet and an excited nucleon (string system) below which they will interact and the jet will lose energy and momentum to that string system.
       HIPR1(13)=1.0      !(D=1.0 fm) the mean free path of a jet when it goes through the excited nuclear matter.
       HIPR1(14)=2.0      !(D=2.0 GeV/fm) the energy loss $dE/dz$ of a gluon jet inside the excited nuclear matter. The energy loss for a quark jet is half of the energy loss of a gluon.

c other parameter changes in Hijing can be inserted there ...

c Information for epos for minimum and maximum energy
      egymin=5.
      egymax=16000.        !can not do more because of simple precision (crash)
      bmax=8

c Do not rescale energy for energy conservation
      irescl=0

c Do not use epos decay with Hijing
      ifrade=0
      

      call utprix('inihij',ish,ishini,6)
      end

c-----------------------------------------------------------------------
      subroutine IniEvtHij
c-----------------------------------------------------------------------
c Initialization for each type of event (for given proj, targ and egy)
c-----------------------------------------------------------------------
      include 'epos.inc'
      character*4 NameProj,NameTarg
      character*160 NameDecay
      COMMON/LUDAT1/MSTU(200),PARU(200),MSTJ(200),PARJ(200)
      SAVE /LUDAT1/
      COMMON/LUDAT3/MDCY(500,3),MDME(2000,2),BRAT(2000),KFDP(2000,5)
      SAVE /LUDAT3/
      COMMON/HIPARNT/HIPR1(100),IHPR2(50),HINT1(100),IHNT2(50)
      SAVE  /HIPARNT/

      NameProj='--- '
      NameTarg='--- '
      
      if(maproj.eq.1.and.abs(laproj).eq.1)then
        if(idproj.eq.1120)NameProj='P   '
        if(idproj.eq.-1120)NameProj='PBAR'
        if(idproj.eq.1220)NameProj='N   '
        if(idproj.eq.-1220)NameProj='NBAR'
        if(idproj.eq.110)NameProj='PI+ '
        if(idproj.eq.120)NameProj='PI+ '
        if(idproj.eq.-120)NameProj='PI- '
        if(idproj.eq.130)NameProj='K+  '
        if(idproj.eq.-130)NameProj='K-  '
        if(idproj.eq.20.or.idproj.eq.230)NameProj='K+  '
        if(idproj.eq.-20.or.idproj.eq.-230)NameProj='K-  '
      else
        NameProj='A   '
      endif
        
      if(matarg.eq.1.and.abs(latarg).eq.1)then
        if(idtarg.eq.1120)NameTarg='P  '
        if(idtarg.eq.-1120)NameTarg='PBAR'
        if(idtarg.eq.1220)NameTarg='N  '
        if(idtarg.eq.-1220)NameTarg='NBAR'
        if(idtarg.eq.110)NameTarg='PI+ '
        if(idtarg.eq.120)NameTarg='PI+ '
        if(idtarg.eq.-120)NameTarg='PI- '
        if(idtarg.eq.130)NameTarg='K+  '
        if(idtarg.eq.-130)NameTarg='K-  '
        if(idtarg.eq.20.or.idtarg.eq.230)NameTarg='K+  '
        if(idtarg.eq.-20.or.idtarg.eq.-230)NameTarg='K-  '
      else
        NameTarg='A   '
      endif
        
      if(NameProj(1:3).eq.'---')
     &     call utstop('Proj not allowed in Hijing !&')
      if(NameTarg(1:3).eq.'---')
     &     call utstop('Targ not allowed in Hijing !&')
      

c epos syntax to allow (or not) particle decay in Hijing 
c (part taken from epos-dky: hdecas)
c This should be before HIJSET

      MDCY(13,1)=1     !muon do not decay by default in Jetset
      do i=100,500
        MDCY(i,1)=1
      enddo
      if(idecay.eq.1)then

      MSTJ(21) = 1            !switch on decays

      if(mod(ndecay/10,10).eq.1)then          !Kshort, Klong
        ic1=130
        ic2=310
        write(NameDecay,'(a,i3,a,i3,a)')
     &       'MDCY(C',ic1,',1)=0;MDCY(C',ic2,',1)=0'
        call LUGIVE(NameDecay(1:30))
        write(NameDecay,'(a,i4,a,i4,a)')
     &       'MDCY(C',-ic1,',1)=0;MDCY(C',-ic2,',1)=0'
        call LUGIVE(NameDecay(1:32))
      endif
      if(mod(ndecay/100,10).eq.1)then         !Lambda
        ic=3122
        write(NameDecay,'(a,i4,a)')
     &       'MDCY(C',ic,',1)=0'
        call LUGIVE(NameDecay(1:15))
      endif
      if(mod(ndecay/1000,10).eq.1)then        !sigma+
        ic=3222
        write(NameDecay,'(a,i4,a,i5,a)')
     &       'MDCY(C',ic,',1)=0;MDCY(C',-ic,',1)=0'
        call LUGIVE(NameDecay(1:33))
      endif
      if(mod(ndecay/1000,10).eq.1)then        !sigma-
        ic=3112
        write(NameDecay,'(a,i4,a,i5,a)')
     &       'MDCY(C',ic,',1)=0;MDCY(C',-ic,',1)=0'
        call LUGIVE(NameDecay(1:33))
      endif
      if(mod(ndecay/10000,10).eq.1)then       !Xi+/-
        ic=3312
        write(NameDecay,'(a,i4,a,i5,a)')
     &       'MDCY(C',ic,',1)=0;MDCY(C',-ic,',1)=0'
        call LUGIVE(NameDecay(1:33))
      endif
      if(mod(ndecay/10000,10).eq.1)then       !Xi0
        ic=3322
        write(NameDecay,'(a,i4,a,i5,a)')
     &       'MDCY(C',ic,',1)=0;MDCY(C',-ic,',1)=0'
        call LUGIVE(NameDecay(1:33))
      endif
      if(mod(ndecay/100000 ,10).eq.1)then     !omega
        ic=3334
        write(NameDecay,'(a,i4,a,i5,a)')
     &       'MDCY(C',ic,',1)=0;MDCY(C',-ic,',1)=0'
        call LUGIVE(NameDecay(1:33))
      endif
      if(mod(ndecay/1000000,10).eq.1)then     !pi0
        ic=111
        write(NameDecay,'(a,i3,a)')
     &       'MDCY(C',ic,',1)=0'
        call LUGIVE(NameDecay(1:13))
      endif

      if(nrnody.gt.0)then                      !all other particle
        do nod=1,nrnody
          ic=idtrafo('nxs','pdg',nody(nod))
          if(ic.lt.100.and.ic.gt.0)then
            write(NameDecay,'(a,i2,a)')'MDCY(C',ic,',1)=0'
            call LUGIVE(NameDecay(1:13))
          elseif(ic.lt.1000.and.ic.gt.-100)then
            write(NameDecay,'(a,i3,a)')'MDCY(C',ic,',1)=0'
            call LUGIVE(NameDecay(1:14))
          elseif(ic.lt.-1000.and.ic.gt.-10000)then
            write(NameDecay,'(a,i5,a)')'MDCY(C',ic,',1)=0'
            call LUGIVE(NameDecay(1:16))
          elseif(abs(ic).lt.10000)then
            write(NameDecay,'(a,i4,a)')'MDCY(C',ic,',1)=0'
            call LUGIVE(NameDecay(1:15))
          endif
        enddo 
      endif

      if(ctaumin.gt.0.)then

        MSTJ(22) = 2   !switch on decay according to life time
        PARJ(71) = ctaumin*10.  !cm->mm

      endif

      else

        MSTJ(21) = 0   !switch off all decays
        IHPR2(12) = 1

      endif

      if(ish.ge.2)write(ifch,*)
     &  'Hijing used with (E,proj,targ) in CMS (',engy
     &  ,', ',NameProj,', ',NameTarg,')'
      call HIJSET(engy,'CMS ',NameProj,NameTarg
     &              ,maproj,laproj,matarg,latarg)

c Inelastic cross section

      hijincs=hijcrse(idtargin)
      if(engy.lt.egymin)hijincs=0.          !below egymin, no interaction
      call xsigma

      return
      end

c-----------------------------------------------------------------------
      subroutine emshij(iret)
c-----------------------------------------------------------------------
c  call hijing to simulate interaction
c-----------------------------------------------------------------------
      include 'epos.inc'
      common/geom/rmproj,rmtarg,bmax,bkmx
C...Parameters from Hijing
C....information of produced particles:    
      COMMON/HIMAIN1/NATT, EATT, JATT, NT, NP, N0, N01, N10, N11 
      COMMON/HIMAIN2/KATT(130000,4), PATT(130000,4) 
C....information of produced partons:     
      COMMON/HIJJET1/NPJ(300), KFPJ(300,500), PJPX(300,500), 
     &    PJPY(300,500), PJPZ(300,500), PJPE(300,500), PJPM(300,500),
     &    NTJ(300), KFTJ(300,500), PJTX(300,500), PJTY(300,500),
     &    PJTZ(300,500), PJTE(300,500), PJTM(300,500)
      COMMON/HIJJET2/NSG, NJSG(900), IASG(900,3), K1SG(900,100),
     & K2SG(900,100), PXSG(900,100), PYSG(900,100), PZSG(900,100), 
     & PESG(900,100), PMSG(900,100) 
      COMMON/HISTRNG/NFP(300,15), PP(300,15), NFT(300,15), PT(300,15) 
C....information about event
      COMMON/HIPARNT/HIPR1(100), IHPR2(50), HINT1(100), IHNT2(50) 
      COMMON/HIJCRDN/YP(3,300),YT(3,300)


      iret=0
      ncol=0
      b1=bminim
      if(maproj+matarg.eq.2)then
        b2=min(bmax,bmaxim)
      else
        b2=min(HIPR1(34)+HIPR1(35),bmaxim)
      endif
      a=max(pi*(b2**2-b1**2),1e-30)
      if(a.gt.0..and.rangen().gt.hijincs/10./a)goto 1001   !no interaction
      if(ish.ge.3)call alist('Determine Hijing Production&',0,0)

      nptl=0
      nptl2=maproj+matarg
      IHNT2(1)=maproj                 !can be changed in case of air target
      IHNT2(3)=matarg                 !can be changed in case of air target
      call HIJING('CMS     ',b1,b2)               !generate event

      ncol=N0+N10+N01+N11
      nevt=1
      kolevt=ncol
      npjevt=NP
      ntgevt=NT
      pmxevt=pnll
      egyevt=engy
      bimp=HINT1(19)
      bimevt=bimp
      phievt=HINT1(20)
      anintine=anintine+1.


c Projectiles and their remnants

      do i=1,maproj
        nptl=nptl+1
        ic=NFP(i,4)
        if(ish.ge.7)write(ifch,'(a,i5,a,i5,a)')
     $       ' Hijing particle ',i,' id :',ic,' in projectile'
        id=idtrafo('pdg','nxs',ic)
        if(ish.ge.7)write(ifch,'(a,i5,a,i10,a)')
     $       ' epos particle ',nptl,' id :',id,' after conversion'
        if(id.lt.10000)then
          call idmass(id,am)
        else
          am=PP(5,i)
        endif
        pptl(1,nptl)=0.                !P_x
        pptl(2,nptl)=0.                !P_y
        pptl(3,nptl)=sqrt(HINT1(6)*HINT1(6)-am*am) !P_z
        pptl(4,nptl)=HINT1(6)          !E
        pptl(5,nptl)=am                !mass
        idptl(nptl)=id
        iorptl(nptl)=-1
        ityptl(nptl)=0
        xorptl(1,nptl)=YP(1,i)
        xorptl(2,nptl)=YP(2,i)
        xorptl(3,nptl)=YP(3,i)
        xorptl(4,nptl)=0.
        tivptl(1,nptl)=0.
        tivptl(2,nptl)=0.
        istptl(nptl)=1
        if(NFP(i,5).eq.3)then!if the projectile suffer an inelastic interaction
          nptl2=nptl2+1
          ifrptl(1,nptl)=nptl2
          ifrptl(2,nptl)=nptl2
          istptl(nptl2)=41
          pptl(1,nptl2)=PP(i,1) !P_x
          pptl(2,nptl2)=PP(i,2) !P_y
          pptl(3,nptl2)=PP(i,3) !P_z
          pptl(4,nptl2)=PP(i,4) !E
          pptl(5,nptl2)=PP(i,5) !mass
          idptl(nptl2)=id*100+99
          iorptl(nptl2)=nptl
          ityptl(nptl2)=40
          xorptl(1,nptl2)=xorptl(1,nptl)
          xorptl(2,nptl2)=xorptl(2,nptl)
          xorptl(3,nptl2)=xorptl(3,nptl)
          xorptl(4,nptl2)=xorptl(4,nptl)
          tivptl(1,nptl2)=tivptl(1,nptl)
          tivptl(2,nptl2)=tivptl(2,nptl)

c Partons from projectiles
          nptli=nptl2
          nptl2=nptl2+1
          ic=NFP(i,1)
          if(ish.ge.7)write(ifch,'(a,i5,a,i5,a)')
     $       ' Hijing particle ',i,' id :',ic,' in projectile quark'
          id=idtrafo('pdg','nxs',ic)
          if(ish.ge.7)write(ifch,'(a,i5,a,i10,a)')
     $       ' epos particle ',nptl2,' id :',id,' after conversion'
          pptl(1,nptl2)=PP(i,6) !P_x
          pptl(2,nptl2)=PP(i,7) !P_y
          pptl(3,nptl2)=0.      !P_z
          pptl(4,nptl2)=0.      !E
          pptl(5,nptl2)=PP(i,14)!mass
          idptl(nptl2)=id
          iorptl(nptl2)=nptli
          ityptl(nptl2)=42
          xorptl(1,nptl2)=xorptl(1,nptl)
          xorptl(2,nptl2)=xorptl(2,nptl)
          xorptl(3,nptl2)=xorptl(3,nptl)
          xorptl(4,nptl2)=xorptl(4,nptl)
          tivptl(1,nptl2)=tivptl(1,nptl)
          tivptl(2,nptl2)=tivptl(2,nptl)
          istptl(nptl2)=21
          if(NPJ(i).gt.0)then
            do j=1,NPJ(i)
              ic=KFPJ(i,j)
              if(ic.ne.0)then
                nptl2=nptl2+1
                if(ish.ge.7)write(ifch,'(a,i5,a,i5,a)')
     $       ' Hijing particle ',j,' id :',ic,' projectile string'
                id=idtrafo('pdg','nxs',ic)
                if(ish.ge.7)write(ifch,'(a,i5,a,i10,a)')
     $       ' epos particle ',nptl2,' id :',id,' after conversion'
                pptl(1,nptl2)=PJPX(i,j) !P_x
                pptl(2,nptl2)=PJPY(i,j) !P_y
                pptl(3,nptl2)=PJPZ(i,j) !P_z
                pptl(4,nptl2)=PJPE(i,j) !E
                pptl(5,nptl2)=PJPM(i,j) !mass
                idptl(nptl2)=id
                iorptl(nptl2)=nptli
                ityptl(nptl2)=42
                xorptl(1,nptl2)=xorptl(1,nptl)
                xorptl(2,nptl2)=xorptl(2,nptl)
                xorptl(3,nptl2)=xorptl(3,nptl)
                xorptl(4,nptl2)=xorptl(4,nptl)
                tivptl(1,nptl2)=tivptl(1,nptl)
                tivptl(2,nptl2)=tivptl(2,nptl)
                istptl(nptl2)=21
              endif
            enddo
          endif
          nptl2=nptl2+1
          ic=NFP(i,2)
          if(ish.ge.7)write(ifch,'(a,i5,a,i5,a)')
     $       ' Hijing particle ',i,' id :',ic,' projectile diquark'
          id=idtrafo('pdg','nxs',ic)
          if(ish.ge.7)write(ifch,'(a,i5,a,i10,a)')
     $       ' epos particle ',nptl2,' id :',id,' after conversion'
          pptl(1,nptl2)=PP(i,8) !P_x
          pptl(2,nptl2)=PP(i,9) !P_y
          pptl(3,nptl2)=0.      !P_z
          pptl(4,nptl2)=0.      !E
          pptl(5,nptl2)=PP(i,15)!mass
          idptl(nptl2)=id
          iorptl(nptl2)=nptli
          ityptl(nptl2)=42
          xorptl(1,nptl2)=xorptl(1,nptl)
          xorptl(2,nptl2)=xorptl(2,nptl)
          xorptl(3,nptl2)=xorptl(3,nptl)
          xorptl(4,nptl2)=xorptl(4,nptl)
          tivptl(1,nptl2)=tivptl(1,nptl)
          tivptl(2,nptl2)=tivptl(2,nptl)
          istptl(nptl2)=21
        endif
      enddo

c Targets and their remnants

      do i=1,matarg
        nptl=nptl+1
        ic=NFT(i,4)
        if(ish.ge.7)write(ifch,'(a,i5,a,i5,a)')
     $       ' Hijing particle ',i,' id :',ic,' in target'
        id=idtrafo('pdg','nxs',ic)
        if(ish.ge.7)write(ifch,'(a,i5,a,i10,a)')
     $       ' epos particle ',nptl,' id :',id,' after conversion'
        if(id.lt.10000)then
          call idmass(id,am)
        else
          am=PT(5,i)
        endif
        pptl(1,nptl)=0.                !P_x
        pptl(2,nptl)=0.                !P_y
        pptl(3,nptl)=-sqrt(HINT1(7)*HINT1(7)-am*am) !P_z
        pptl(4,nptl)=HINT1(7)          !E
        pptl(5,nptl)=am                !mass
        idptl(nptl)=id
        iorptl(nptl)=-1
        ityptl(nptl)=0
        xorptl(1,nptl)=YT(1,i)
        xorptl(2,nptl)=YT(2,i)
        xorptl(3,nptl)=YT(3,i)
        xorptl(4,nptl)=0.
        tivptl(1,nptl)=0.
        tivptl(2,nptl)=0.
        istptl(nptl)=1
        if(NFT(i,5).eq.3)then!if the projectile suffer an inelastic interaction
          nptl2=nptl2+1
          ifrptl(1,nptl)=nptl2
          ifrptl(2,nptl)=nptl2
          istptl(nptl2)=41
          pptl(1,nptl2)=PT(i,1) !P_x
          pptl(2,nptl2)=PT(i,2) !P_y
          pptl(3,nptl2)=PT(i,3) !P_z
          pptl(4,nptl2)=PT(i,4) !E
          pptl(5,nptl2)=PT(i,5) !mass
          idptl(nptl2)=id*100+99
          iorptl(nptl2)=nptl
          ityptl(nptl2)=10
          xorptl(1,nptl2)=xorptl(1,nptl)
          xorptl(2,nptl2)=xorptl(2,nptl)
          xorptl(3,nptl2)=xorptl(3,nptl)
          xorptl(4,nptl2)=xorptl(4,nptl)
          tivptl(1,nptl2)=tivptl(1,nptl)
          tivptl(2,nptl2)=tivptl(2,nptl)

c Partons from target
          nptli=nptl2
          nptl2=nptl2+1
          ic=NFT(i,1)
          if(ish.ge.7)write(ifch,'(a,i5,a,i5,a)')
     $       ' Hijing particle ',i,' id :',ic,' in target quark'
          id=idtrafo('pdg','nxs',ic)
          if(ish.ge.7)write(ifch,'(a,i5,a,i10,a)')
     $       ' epos particle ',nptl,' id :',id,' after conversion'
          pptl(1,nptl2)=PT(i,6) !P_x
          pptl(2,nptl2)=PT(i,7) !P_y
          pptl(3,nptl2)=0.      !P_z
          pptl(4,nptl2)=0.      !E
          pptl(5,nptl2)=PT(i,14)!mass
          idptl(nptl2)=id
          iorptl(nptl2)=nptli
          ityptl(nptl2)=12
          xorptl(1,nptl2)=xorptl(1,nptl)
          xorptl(2,nptl2)=xorptl(2,nptl)
          xorptl(3,nptl2)=xorptl(3,nptl)
          xorptl(4,nptl2)=xorptl(4,nptl)
          tivptl(1,nptl2)=tivptl(1,nptl)
          tivptl(2,nptl2)=tivptl(2,nptl)
          istptl(nptl2)=21
          if(NTJ(i).gt.0)then
            do j=1,NTJ(i)
              ic=KFPJ(i,j)
              if(ic.ne.0)then
                nptl2=nptl2+1
                if(ish.ge.7)write(ifch,'(a,i5,a,i5,a)')
     $       ' Hijing particle ',j,' id :',ic,' in target string'
                id=idtrafo('pdg','nxs',ic)
                if(ish.ge.7)write(ifch,'(a,i5,a,i10,a)')
     $       ' epos particle ',nptl2,' id :',id,' after conversion'
                pptl(1,nptl2)=PJTX(i,j) !P_x
                pptl(2,nptl2)=PJTY(i,j) !P_y
                pptl(3,nptl2)=PJTZ(i,j) !P_z
                pptl(4,nptl2)=PJTE(i,j) !E
                pptl(5,nptl2)=PJTM(i,j) !mass
                idptl(nptl2)=id
                iorptl(nptl2)=nptli
                ityptl(nptl2)=12
                xorptl(1,nptl2)=xorptl(1,nptl)
                xorptl(2,nptl2)=xorptl(2,nptl)
                xorptl(3,nptl2)=xorptl(3,nptl)
                xorptl(4,nptl2)=xorptl(4,nptl)
                tivptl(1,nptl2)=tivptl(1,nptl)
                tivptl(2,nptl2)=tivptl(2,nptl)
                istptl(nptl2)=21
              endif
            enddo
          endif
          nptl2=nptl2+1
          ic=NFT(i,2)
          if(ish.ge.7)write(ifch,'(a,i5,a,i5,a)')
     $       ' Hijing particle ',i,' id :',ic,' in target diquark'
          id=idtrafo('pdg','nxs',ic)
          if(ish.ge.7)write(ifch,'(a,i5,a,i10,a)')
     $       ' epos particle ',nptl2,' id :',id,' after conversion'
          pptl(1,nptl2)=PT(i,8) !P_x
          pptl(2,nptl2)=PT(i,9) !P_y
          pptl(3,nptl2)=0.      !P_z
          pptl(4,nptl2)=0.      !E
          pptl(5,nptl2)=PT(i,15)!mass
          idptl(nptl2)=id
          iorptl(nptl2)=nptli
          ityptl(nptl2)=12
          xorptl(1,nptl2)=xorptl(1,nptl)
          xorptl(2,nptl2)=xorptl(2,nptl)
          xorptl(3,nptl2)=xorptl(3,nptl)
          xorptl(4,nptl2)=xorptl(4,nptl)
          tivptl(1,nptl2)=tivptl(1,nptl)
          tivptl(2,nptl2)=tivptl(2,nptl)
          istptl(nptl2)=21
        endif
      enddo

      nptl=nptl2

c Partons from strings

      do i=1,NSG
        do j=1,NJSG(i)
          ic=K2SG(i,j)
          if(ish.ge.7)write(ifch,'(a,i5,a,i5,a)')
     $       ' Hijing particle ',i,' id :',ic,' in strings'
          if(ic.ne.0)then
            nptl=nptl+1
            id=idtrafo('pdg','nxs',ic)
            if(ish.ge.7)write(ifch,'(a,i5,a,i10,a)')
     $       ' epos particle ',nptl,' id :',id,' after conversion'
            pptl(1,nptl)=PXSG(i,j) !P_x
            pptl(2,nptl)=PYSG(i,j) !P_y
            pptl(3,nptl)=PZSG(i,j) !P_z
            pptl(4,nptl)=PESG(i,j) !E
            pptl(5,nptl)=PMSG(i,j) !mass
            idptl(nptl)=id
            iorptl(nptl)=IASG(i,1)
            jorptl(nptl)=IASG(i,2)+maproj
            ityptl(nptl)=21
            xorptl(1,nptl)=0.5*(YT(1,IASG(i,1))+YT(1,IASG(i,2)))
            xorptl(2,nptl)=0.5*(YT(2,IASG(i,1))+YT(3,IASG(i,2)))
            xorptl(3,nptl)=0.5*(YT(2,IASG(i,1))+YT(3,IASG(i,2)))
            xorptl(4,nptl)=0.
            tivptl(1,nptl)=0.
            tivptl(2,nptl)=0.
            istptl(nptl)=21
          endif
        enddo
      enddo

      do 100 is=1,NATT            !number of particle in event

        ic=KATT(is,1)
        if(ish.ge.7)write(ifch,'(a,i5,a,i5,2a,4(e10.4,1x))')
     $       ' Hijing particle ',is,' id :',ic,' before conversion'
     $     , ' momentum :',(PATT(is,k),k=1,4)


        nptl=nptl+1
        id=idtrafo('pdg','nxs',ic)
        if(ish.ge.7)write(ifch,'(a,i5,a,i10,a)')
     $       ' epos particle ',nptl,' id :',id,' after conversion'

        call idmass(id,am)
            
        pptl(1,nptl)=PATT(is,1)    !P_x
        pptl(2,nptl)=PATT(is,2)    !P_y
        pptl(3,nptl)=PATT(is,3)    !P_z
        pptl(4,nptl)=PATT(is,4)    !E
        pptl(5,nptl)=am            !mass
        if(KATT(is,2).le.2)then
          ityptl(nptl)=40
        elseif(KATT(is,2).eq.3)then
          ityptl(nptl)=41
        elseif(KATT(is,2).le.12)then
          ityptl(nptl)=50
        elseif(KATT(is,2).eq.13)then
          ityptl(nptl)=51
        elseif(KATT(is,2).eq.20)then
          ityptl(nptl)=21
        elseif(KATT(is,2).eq.40)then
          ityptl(nptl)=31
        else
          ityptl(nptl)=0
        endif
        iorptl(nptl)=0  !nptl2+KATT(is,3)
        jorptl(nptl)=0
        ifrptl(1,nptl)=0
        ifrptl(2,nptl)=0
        xorptl(1,nptl)=0. !x vertex position
        xorptl(2,nptl)=0. !y vertex position
        xorptl(3,nptl)=0. !z vertex position
        xorptl(4,nptl)=0. !production time
        tivptl(1,nptl)=0. !production time
        tivptl(2,nptl)=0. !decay time
        idptl(nptl)=id
        if(KATT(is,4).eq.1.or.KATT(is,4).eq.4)then
          istptl(nptl)=0
        else
          istptl(nptl)=1
        endif

            
        if(ish.ge.5)write(ifch,'(a,i5,a,i5,a,4(e10.4,1x),f6.3)')
     $       ' particle from hijing ',nptl,' id :',idptl(nptl)
     $  , ' momentum :',(pptl(k,nptl),k=1,5)


 100  continue

      
      nhpevt=JATT

1000  return

1001  iret=-1 
      goto 1000 

      end

c------------------------------------------------------------------------------
      function hijcrse(idtrg)
c------------------------------------------------------------------------------
c inelastic cross section of hijing
c------------------------------------------------------------------------------
      include 'epos.inc'
      common/geom/rmproj,rmtarg,bmax,bkmx
      COMMON/HIPARNT/HIPR1(100), IHPR2(50), HINT1(100), IHNT2(50) 

      hijcrse=HINT1(12)
      if(maproj.gt.1.or.matarg.gt.1)then
C....calculate the Glauber probability and its integrated value:
        DIP=BMAX**2/100.0 
        GBTOT=0.0
        if(idtrg.eq.0)then
          hijcrse=0
          do k=1,3
            IHNT2(3)=int(airanxs(k))
            DO 10 I=1,101 
              XBI=sqrt((I-1)*DIP) 
              OV=HIPROFILE(XBI) 
              GBI=DIP*(1.0-EXP(-HINT1(12)*OV)) 
              GBTOT=GBTOT+GBI
 10         CONTINUE 
            gbtot=pi*10.*gbtot
            hijcrse=hijcrse+airwnxs(k)*gbtot
          enddo
          IHNT2(3)=matarg
        else
          DO 100 I=1,101 
            XBI=sqrt((I-1)*DIP) 
            OV=HIPROFILE(XBI) 
            GBI=DIP*(1.0-EXP(-HINT1(12)*OV)) 
            GBTOT=GBTOT+GBI
 100      CONTINUE 
          hijcrse=pi*10.*gbtot
        endif
      endif
      return
      end

c------------------------------------------------------------------------------
      subroutine hjcrossc(sigi,sigt)
c------------------------------------------------------------------------------
c inelastic cross section of hijing
c------------------------------------------------------------------------------
      include 'epos.inc'
      common/geom/rmproj,rmtarg,bmax,bkmx
      COMMON/HIPARNT/HIPR1(100), IHPR2(50), HINT1(100), IHNT2(50) 

      sigi=HINT1(12)
      sigt=HINT1(13)
      if(maproj.gt.1.or.matarg.gt.1)then
C....calculate the Glauber probability and its integrated value:
        IHNT2(1)=maproj
        IHNT2(3)=matarg
        DIP=BMAX**2/100.0 
        GBINE=0.0
        GBTOT=0.0
        DO 100 I=1,101 
          XBI=sqrt((I-1)*DIP) 
          OV=HIPROFILE(XBI) 
          GBI=DIP*(1.0-EXP(-HINT1(12)*OV)) 
          GBT=DIP*(1.0-EXP(-HINT1(13)*OV)) 
          GBINE=GBINE+GBI
          GBTOT=GBTOT+GBT
 100    CONTINUE 
        sigi=pi*10.*gbine
        sigt=pi*10.*gbtot
      endif
      return
      end

c--------------------------------------------------------------------
      function RLU(idum)
c--------------------------------------------------------------------
c random number generator
c--------------------------------------------------------------------
      include 'epos.inc'
      double precision drangen

      RLU=sngl(drangen(dble(idum)))
      if(irandm.eq.1)write(ifch,*)'RLU(0)= ',RLU,idum

      return
      end
