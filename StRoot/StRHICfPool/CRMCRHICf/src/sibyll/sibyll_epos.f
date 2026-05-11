c 15.02.2004 Link routines between SIBYLL2.1 and EPOS.
c author T. Pierog


c-----------------------------------------------------------------------
      subroutine IniSibyll
c-----------------------------------------------------------------------
c Primary initialization for Sibyll
c-----------------------------------------------------------------------
      include 'epos.inc'
      INTEGER NCALL, NDEBUG, LUN
      COMMON /S_DEBUG/ NCALL, NDEBUG, LUN
      DOUBLE PRECISION CBR
      INTEGER KDEC,LBARP,IDB
      COMMON /S_CSYDEC/ CBR(223+16+12+8), KDEC(1338+6*(16+12+8)),
     &     LBARP(99), IDB(99)

      call utpri('inisib',ish,ishini,6)
      write(ifmt,'(a,i6)')'initialize Sibyll ...'
      
      Ndebug=0
      if(ish.ge.3)Ndebug=ish-2
      lun = 7
C... SIBYLL initialization
      CALL SIBYLL_INI

C...Cross sections for nucleus-nucleus and hadron nucleus
      CALL NUC_NUC_INI

C...define all particles as unstable
      do i=1,99
        IDB(i) = abs(IDB(i))   ! >0 means unstable
      enddo


      egymin=10.1
      egymax=1e7
      irescl=0
      

      call utprix('inisib',ish,ishini,6)
      
      end

c-----------------------------------------------------------------------
      subroutine IniEvtSib
c-----------------------------------------------------------------------
c Initialization for each type of event (for given proj, targ and egy)
c-----------------------------------------------------------------------
      include 'epos.inc'
      DOUBLE PRECISION CBR,sibcrse
      INTEGER KDEC,LBARP,IDB
      COMMON /S_CSYDEC/ CBR(223+16+12+8), KDEC(1338+6*(16+12+8)),
     &     LBARP(99), IDB(99)
      common/geom/rmproj,rmtarg,bmax,bkmx

      if(matarg.gt.18.or.maproj.gt.64)
     &  call utstop('Mass too big for Sibyll (Mtrg<18, Mprj<64) !&')
      id=idtrafo('nxs','sib',idproj)
      ida=abs(id)
      if(ida.lt.6.or.ida.gt.14)
     &  call utstop('projectile no allowed in Sibyll !&')
      if(idtarg.ne.0.and.idtarg.ne.1120)
     &  call utstop('target no allowed in Sibyll !&')
      if(bminim.gt.0.or.bmaxim.lt.1000)
     &  write(ifmt,*)'Only min bias event in Sibyll ... no b !'
      call iclass(idproj,iclpro)
      bminim=0.
      bmaxim=10000.
      bmax=10.+maproj+matarg
      sibincs=real(sibcrse(ekin,maproj,matarg,idtargin))
      if(engy.lt.egymin)sibincs=0.          !below egymin, no interaction
      call xsigma

c Epos syntax to allow (or not) particle decay in Sibyll
c (part taken from epos-dky: hdecas)

      if(idecay.eq.1.and.ctaumin.le.0.)then

      if(ndecay.eq.1.or.mod(ndecay/10,10).eq.1)then          !Kshort, Klong
        IDB(11) = -abs(IDB(11))   ! <0 means stable
        IDB(12) = -abs(IDB(12))
      endif
      if(ndecay.eq.1.or.mod(ndecay/100,10).eq.1)then         !Lambda
        IDB(39) = -abs(IDB(39))
      endif
      if(ndecay.eq.1.or.mod(ndecay/1000,10).eq.1)then        !sigma+
        IDB(34) = -abs(IDB(34))
      endif
      if(ndecay.eq.1.or.mod(ndecay/1000,10).eq.1)then        !sigma-
        IDB(36) = -abs(IDB(36))
      endif
      if(ndecay.eq.1.or.mod(ndecay/10000,10).eq.1)then       !Xi+/-
        IDB(38) = -abs(IDB(38))
      endif
      if(ndecay.eq.1.or.mod(ndecay/10000,10).eq.1)then       !Xi0
        IDB(37) = -abs(IDB(37))
      endif
      if(ndecay.eq.1.or.mod(ndecay/100000 ,10).eq.1)then     !omega
        IDB(49) = -abs(IDB(49))
      endif
      if(ndecay.eq.1.or.mod(ndecay/1000000,10).eq.1)then     !pi0 and eta
        IDB(6)  = -abs(IDB(6))
        IDB(23) = -abs(IDB(23))
      endif

      if(nrnody.gt.0)then                      !all other particle
        do nod=1,nrnody
          idd=abs(idtrafo('nxs','sib',nody(nod)))
          if(idd.lt.50)IDB(idd) = -abs(IDB(idd))
        enddo
      endif

      else

C...define all particles as stable
      do i=1,99
        IDB(i) = -abs(IDB(i))   ! <0 means stable
      enddo

      endif

      if(ish.ge.2)write(ifch,*)
     &  'Sibyll used with (E,proj,maproj,matarg)',engy,id,maproj
     &  ,matarg

      return
      end

c-----------------------------------------------------------------------
      subroutine emssib(iret)
c-----------------------------------------------------------------------
c  call Sibyll to simulate interaction
c-----------------------------------------------------------------------
      include 'epos.inc'
      common/geom/rmproj,rmtarg,bmax,bkmx
      double precision esum!,  dummy, drangen
C  SIBYLL
      DOUBLE PRECISION P
      INTEGER NP,LLIST,NP_max
      PARAMETER (NP_max=8000)
      COMMON /S_PLIST/ P(NP_max,5), LLIST(NP_max), NP

      INTEGER NCALL, NDEBUG, LUN
      COMMON /S_DEBUG/ NCALL, NDEBUG, LUN

      INTEGER NW_max
      PARAMETER (NW_max = 20)

      INTEGER NNSOF,NNJET,JDIF,NWD,NJET,NSOF
      COMMON /S_CHIST/ NNSOF(NW_max),NNJET(NW_max),
     &     JDIF(NW_max),NWD,NJET,NSOF

      COMMON /S_PLNUC/ PA(5,40000), LLA(40000), NPA
      double precision engy_dbl,pa!,ptm
      COMMON /S_CLDIF/ LDIFF
      INTEGER          LDIFF

      iret=0
      b1=bminim
      b2=min(bmax,bmaxim)
      a=pi*(b2**2-b1**2)
      if(a.gt.0..and.rangen().gt.sibincs/10./a)goto 1001   !no interaction
      if(ish.ge.3)call alist('Determine Sibyll Production&',0,0)

      nptl=0
      NP=0
      NPA=0
      LDIFF=0        !all types of events

      nevt=1
      kolevt=-1
      koievt=-1
      kohevt=-1
      npjevt=maproj
      ntgevt=matarg
      pmxevt=pnll
      egyevt=engy
      bimevt=0.
      bimp=0.
      phievt=0.
      phi=0.
      anintine=anintine+1.
      ns=0                      !number of projectile spectators
      npps=0
      npns=0
      nbar=0
      if(laproj.lt.0)then
        call idchrg(90,idproj,chrg)
        nch=nint(chrg)
      else
        nch=laproj
      endif
      if(latarg.lt.0)then
        call idchrg(91,idtarg,chrg)
        nch=nch+nint(chrg)
      else
        nch=nch+latarg
      endif

      call conre
      call conwr

      engy_dbl = dble(engy)
      esum=0.5d0*dble(maproj+matarg)*engy_dbl
      itrg=matarg
      if(idtargin.eq.0)itrg=0
      if(maproj.eq.1)then             !hadronic projectile
        L0=idtrafo('nxs','sib',idproj)
        CALL SIBYLL (L0, itrg, engy_dbl)
        CALL DECSIB
        if(ish.ge.5)write(ifch,'(a,i5)')
     $         ' number of particles from Sibyll :',NP
c save interaction type
        if(nwd.eq.1)then
c     single nucleon interaction
           if(JDIF(1).eq.0)then
              typevt=1          !ND
           elseif(JDIF(1).eq.3)then
              typevt=2          !DD
           elseif(JDIF(1).eq.2)then
              typevt=-4         !SD tar
           else
              typevt=4          !SD pro
           endif
        else
c     multiple nucleon interaction
           indif=0
           idif_b=0
           idif_t=0
           idif_d=0
           do jj=1,nwd
              if(jdif(jj).eq.0) indif=1
              if(jdif(jj).eq.1) idif_b=1
              if(jdif(jj).eq.2) idif_t=1
              if(jdif(jj).eq.3) idif_d=1
           enddo
           if(indif.eq.1)then
              typevt=1
           else
              if(idif_d.eq.1.or.(idif_t.eq.1.and.idif_b.eq.1))then
                 typevt=1                 
              elseif(idif_t.eq.1)then
                 typevt=-4
              else
                 typevt=4
              endif
           endif
        endif
        do k=1,NP

c LLIST is the code of final particle, P - its 4-momentum and mass.
          ic=LLIST(k)
            
          if(ish.ge.7)write(ifch,'(a,i5,a,i5,2a,4(e10.4,1x))')
     $       ' Sibyll particle ',k,' id :',ic,' before conversion'
     $     , ' momentum :',(P(k,i),i=1,5)

          nptl=nptl+1
          if(nptl.gt.mxptl)call utstop('Sibyll: mxptl too small&')

          if(abs(ic).ge.10000)then
            ic=ic-sign(10000,ic)
            istptl(nptl)=1
          else
            istptl(nptl)=0
          endif

          id=idtrafo('sib','nxs',ic)
          if(ish.ge.7)write(ifch,'(a,i5,a,i5,a)')
     $         ' epos particle ',nptl,' id :',id,' after conversion'
            

          if(istptl(nptl).eq.0)then
            if(abs(id).gt.1000)nbar=nbar+sign(1,id)
            call idchrg(92,id,chrg)
            nch=nch-nint(chrg)
            esum=esum-P(k,4)
          endif
          pptl(1,nptl)=sngl(P(k,1))   !P_x
          pptl(2,nptl)=sngl(P(k,2))  !P_y
          pptl(3,nptl)=sngl(P(k,3))   !P_z
          pptl(4,nptl)=abs(sngl(P(k,4)))   !E
          pptl(5,nptl)=sngl(P(k,5))   !mass
          ityptl(nptl)=0
          iorptl(nptl)=1
          jorptl(nptl)=maproj+matarg
          ifrptl(1,nptl)=0
          ifrptl(2,nptl)=0
          xorptl(1,nptl)=0.
          xorptl(2,nptl)=0.
          xorptl(3,nptl)=0.
          xorptl(4,nptl)=0.
          tivptl(1,nptl)=0.
          tivptl(2,nptl)=0.
          idptl(nptl)=id
          
            
          if(ish.ge.5)write(ifch,'(a,i5,a,i5,a,4(e10.4,1x),f6.3)')
     $         ' particle from Sibyll ',nptl,' id :',idptl(nptl)
     $         , ' momentum :',(pptl(i,nptl),i=1,5)


        enddo
        if(abs(idproj).gt.1000)ns=nbar-sign(1,idproj)
        nse=matarg-nint(2d0*esum/engy_dbl)
c        if(ns.ne.NWD.or.ns.ne.nse)
c     $  write(ifmt,*)"Warning in Sibyll: wrong # of spect !"
c     $       ,ns,nse,NWD,esum
        ns=nse !priority given to energy conservation (over baryon conservation)
        if(ns.le.matarg)then
          ntgevt=NWD
          neut=matarg-ns
          npro=min(nch,neut)
          do is=maproj+1,maproj+matarg    !first try to conserve charge
            if(idptl(is).eq.1120.and.npro.gt.0)then
              npro=npro-1
              neut=neut-1
              iorptl(is)=0
              istptl(is)=0
            endif
          enddo
          do is=maproj+1,maproj+matarg    !then complete with neutrons
            if(idptl(is).eq.1220.and.neut.gt.0)then
              neut=neut-1
              iorptl(is)=0
              istptl(is)=0
            endif
          enddo
          do is=maproj+1,maproj+matarg    !if not enought finish with protons
            if(istptl(is).ne.0.and.neut.gt.0)then
              neut=neut-1
              iorptl(is)=0
              istptl(is)=0
            endif
          enddo
          if(ish.ge.5.and.(neut.ne.0.or.npro.ne.0))
     &    write(ifch,*)"Warning in Sibyll: wrong charge conservation !"
     &                 ,nch,ns,neut,npro
        endif
      else                          !for nucleus projectile
        IAP = maproj
        CALL SIBNUC (IAP, itrg, engy_dbl)
        if(ish.ge.5)write(ifch,'(a,i5)')
     $         ' number of particles from Sibyll :',NPA
        mfragmaxp=1
        do 100 k=1,NPA

c LLIST is the code of final particle, P - its 4-momentum and mass.
          ic=LLA(k)
            
          if(ish.ge.7)write(ifch,'(a,i5,a,i5,2a,4(e10.4,1x))')
     $       ' Sibyll particle ',k,' id :',ic,' before conversion'
     $     , ' momentum :',(PA(i,k),i=1,5)


c In Sibyll, because of the semi-superposition model, the exact composition of the projectile nucleus is not know. By construction neither the charge not the baryon number (because of the loop on the target nuclei with has always the same number of nucleon available) can be conserved. Even the energy conservation can be violated if too many target remnants are produced. So the stable state of the projectile nuclei is given artificially to respect model predictions.
          nNuc=0
          if(ic.ge.1001) then                !count spectators
            nNuc=ic-1000
            if(infragm.le.1)then   !nuclear interaction only above min energy, otherwise : fragmentation
              ns=ns+nNuc
              nbar=nbar+nNuc
              esum=esum-PA(4,k)
              goto 100
            elseif(ic.eq.1001)then
              id=1001
c              if(drangen(dummy).lt.0.45d0) then
c                id = 1120
c                npps=npps+1
c                nch=nch-1
c              else
c                id = 1220          !start with neutron to properly count missing charge
c                npns=npns+1
c               endif
            else
              mfragmaxp=max(mfragmaxp,nNuc)
c              ptm=sqrt(PA(1,k)*PA(1,k)+PA(2,k)*PA(2,k)+PA(5,k)*PA(5,k))
c              PA(4,k)=PA(4,k)*float(nNuc)            !energy by nucleon
c              PA(3,k)=sign(sqrt((PA(4,k)+ptm)*(PA(4,k)-ptm)),PA(3,k))
              if(nNuc.le.4)then
                nbar=nbar+nNuc
                nNuc=0
                id=idtrafo('sib','nxs',ic)
                if(id.eq.17)then
                  npps=npps+1
                  npns=npns+1
                elseif(id.eq.18)then
                  npps=npps+1
                  npns=npns+2
                elseif(id.eq.19)then
                  npps=npps+2
                  npns=npns+2
                endif
              else
                id=1000000000+nNuc/2*10000+nNuc*10 !code for nuclei
                nch=nch-nNuc/2
                npps=npps+nNuc/2
                npns=npns+nNuc-nNuc/2
              endif
            endif
          else
            id=idtrafo('sib','nxs',ic)
          endif
          nptl=nptl+1
          if(nptl.gt.mxptl)call utstop('Sibyll: mxptl too small&')
          if(ish.ge.7)write(ifch,'(a,i5,a,i10,a)')
     $         ' epos particle ',nptl,' id :',id,' after conversion'
            

          nbar=nbar+nNuc
          if(nNuc.eq.0)then
            if(abs(id).gt.1000)nbar=nbar+sign(1,id)
            call idchrg(93,id,chrg)
            nch=nch-nint(chrg)
          endif
          esum=esum-PA(4,k)
          pptl(1,nptl)=sngl(PA(1,k))  !P_x
          pptl(2,nptl)=sngl(PA(2,k))  !P_y
          pptl(3,nptl)=sngl(PA(3,k))  !P_z
          pptl(4,nptl)=sngl(PA(4,k))  !E
          pptl(5,nptl)=sngl(PA(5,k))  !mass
          istptl(nptl)=0
          ityptl(nptl)=0
          iorptl(nptl)=1
          jorptl(nptl)=maproj+matarg
          ifrptl(1,nptl)=0
          ifrptl(2,nptl)=0
          xorptl(1,nptl)=0.
          xorptl(2,nptl)=0.
          xorptl(3,nptl)=0.
          xorptl(4,nptl)=0.
          tivptl(1,nptl)=0.
          tivptl(2,nptl)=0.
          idptl(nptl)=id
          
            
          if(ish.ge.5)write(ifch,'(a,i5,a,i10,a,4(e10.4,1x),f6.3)')
     $         ' particle from Sibyll ',nptl,' id :',idptl(nptl)
     $         , ' momentum :',(pptl(i,nptl),i=1,5)

 100    continue

c adjust charge if possible in particular for quasi elastic dissociation
        If(infragm.eq.2)then
          do is=maproj+matarg+1,nptl
            if(idptl(is).eq.1001)then
              if(npps.lt.laproj)then
                idptl(is)=1120
                npps=npps+1
                nch=nch-1
              else
                idptl(is)=1220
                npns=npns+1
              endif
              if(ish.ge.5)write(ifch,'(a,i5,a,i10)')
     $         ' adjust particle from Sibyll ',is,' id :',idptl(is)
            endif
          enddo
        elseif(ns.gt.0)then
          nch=nch-min(abs(laproj),ns/2)
        endif

        ntw=max(0,maproj+matarg-nbar)   !baryons from target
        nse=nint(2d0*esum/engy_dbl)
        if(ish.ge.5.and.nse.ne.ntw)
     &    write(ifch,*)"Warning in Sibyll: wrong # of spect. !"
     &                 ,nse,ntw,esum
        ntw=nse !priority given to energy conservation (over baryon conservation)
        
        nsf=npps+npns

        if(ntw.gt.0)then
         if(ntw.le.matarg)then
          ntgevt=matarg-ntw
          neut=ntw   !in case there is not enough proton available, make sure the total number of spectators is reached
          npro=min(nch,neut)
          if(matarg.eq.1.and.idtarg.eq.1120)npro=1
          do is=maproj+1,maproj+matarg    !first try to conserve charge
            if(idptl(is).eq.1120.and.npro.gt.0)then
              npro=npro-1
              neut=neut-1
              iorptl(is)=0
              istptl(is)=0
            endif
          enddo
          do is=maproj+1,maproj+matarg    !then complete with neutrons
            if(idptl(is).eq.1220.and.neut.gt.0)then
              neut=neut-1
              iorptl(is)=0
              istptl(is)=0
            endif
          enddo
          do is=maproj+1,maproj+matarg    !if not enought finish with protons
            if(istptl(is).ne.0.and.neut.gt.0)then
              neut=neut-1
              iorptl(is)=0
              istptl(is)=0
            endif
          enddo
          if(ish.ge.5.and.(neut.ne.0.or.npro.ne.0))
     &    write(ifch,*)"Warning in Sibyll: wrong charge conservation !"
     &                 ,nch,ntw,neut,npro,nbar
        endif
       endif
          if(ish.ge.5)write(ifch,'(a,i3,a,i3,a,i2,a)')
     $         ' target spectators :',ntw
     $       ,' projectile spectators (ns) :',nsf,' (',ns,')'
        if(infragm.le.1.or.ns.gt.0)then
          if(infragm.eq.1.and.ns.gt.0)then
c  remaining nucleus is one fragment
            nptl=nptl+1
            istptl(nptl)=0
            pptl(1,nptl)=0.d0
            pptl(2,nptl)=0.d0
            pptl(4,nptl)=0.d0
            inucl=0
            do is=1,ns
              inucl=inucl+1
              pptl(4,nptl)=pptl(4,nptl)+pptl(4,is)
            enddo
            mfragmaxp=max(mfragmaxp,inucl)
            iprot= int(dble(inucl) / 2.15d0 + 0.7d0)
            idnucl=1000000000+iprot*10000+inucl*10 !code for nuclei
            npps=npps+iprot
            npns=npns+inucl-iprot
            call idmass(idnucl,am)
            pptl(5,nptl)=am  !mass
            ptot=(pptl(4,nptl)+am)*(pptl(4,nptl)-am)
            pptl(3,nptl)=sqrt(ptot)
            ityptl(nptl)=0
            istptl(nptl)=0
            iorptl(nptl)=1
            jorptl(nptl)=maproj
            ifrptl(1,nptl)=0
            ifrptl(2,nptl)=0
            xorptl(1,nptl)=xorptl(1,1)
            xorptl(2,nptl)=xorptl(2,1)
            xorptl(3,nptl)=xorptl(3,1)
            xorptl(4,nptl)=xorptl(4,1)
            tivptl(1,nptl)=tivptl(1,1)
            tivptl(2,nptl)=tivptl(2,1)
            idptl(nptl)=idnucl
          else
            do is=maproj-ns+1,maproj         !make the maproj-ns first projectile nucleon final (not wounded)
              if(infragm.eq.2)istptl(is)=0
              if(idptl(is).eq.1120)npps=npps+1
              if(idptl(is).eq.1220)npns=npns+1
            enddo
          endif
        endif
c number of participants
        if(laproj.gt.1)then
        npjevt=maproj-npps-npns
        npppar=max(0,laproj-npps)
        npnpar=npjevt-npppar
c set participant projectile as non spectators
        do i=1,maproj
          if(idptl(i).eq.1120)then
            if(npppar.gt.0)then
              npppar=npppar-1
            else                !restore spectators
              iorptl(i)=0
              if(infragm.eq.0)istptl(i)=0
            endif
          endif
          if(idptl(i).eq.1220)then
            if(npnpar.gt.0)then
              npnpar=npnpar-1
            else                !restore spectators
              iorptl(i)=0
              if(infragm.eq.0)istptl(i)=0
            endif
          endif
        enddo
      endif

      endif


1000  return

1001  iret=-1 
      goto 1000 

      end


c------------------------------------------------------------------------------
      function fsibcrse(egy,mapro,matar)
c------------------------------------------------------------------------------
c hadron-proton particle production cross section with Sibyll.
c egy - center of mass energy
c------------------------------------------------------------------------------
      include 'epos.inc'
      dimension SDIF(3)
      double precision egy_dbl,ST,SEL,SINEL,SDIF,SL,RHO,
     &     SIGprod,dum

      egy_dbl = dble(egy)

      if(iclpro.eq.1)then
        L=2
      elseif(iclpro.eq.2)then
        L=1
      else
        L=3
      endif
      call SIB_SIGMA_HP(L,egy_dbl,ST,SEL,SINEL,SDIF,SL,RHO)
      if(matar.gt.1)then
C  calculate hadron-A(matar) cross section
      CALL SIB_SIGMA_HNUC(L,matar,egy_dbl,SIGprod,dum,dum)
C     particle production cross section        
         fsibcrse = SIGprod
      else
        fsibcrse=SINEL
      endif

      if(mapro.gt.1)fsibcrse=ainfin !???????? temporary

      return
      end

c------------------------------------------------------------------------------
      double precision function sibcrse(ek,mapro,matar,id)
c------------------------------------------------------------------------------
c inelastic cross section of Sibyll 
c if id=0, target = air
c ek - kinetic energy in GeV
c maproj - projec mass number     (1<maproj<64)
c matarg - projec mass number
c id - proj id (sibyll code)
c------------------------------------------------------------------------------
      include 'epos.inc'
      double precision E0,egy, sigbmdif, sibcr,SSIGNUC,alnuc,sqs_dbl
      COMMON /CLENNN/ SSIGNUC(60), ALNUC(60)

      sibcrse=0.d0
      call idmass(1120,amt1)
      call idmass(1220,amt2)
      amtar=0.5*(amt1+amt2)
      if(mapro.eq.1)call idmass(idproj,ampro)
      if(matar.eq.1)call idmass(idtarg,amtar)
      egy=dble(ek+ampro)
      if(id.eq.0)then
        if(maproj.eq.1)then
       sqs_dbl=sqrt(2d0*egy*dble(amtar)+dble(amtar)**2+dble(ampro)**2)
          if(iclpro.eq.1)then
            L=2
          elseif(iclpro.eq.2)then
            L=1
          else
            L=3
          endif
          call SIB_SIGMA_HAIR (L,sqs_dbl,sibcr, sigbmdif)
          sibcrse=sibcr
        else
          E0=dble(egy*1.d-3)         !e0 in TeV
          CALL  SIGNUC_INI(mapro,E0) !  fills SSIGNUC and ALNUC
          sibcrse  = SSIGNUC(mapro)
        endif
      else
        sqs=sqrt( 2*sngl(egy)*amtar+amtar**2+ampro**2 )
        sibcrse=dble(fsibcrse(sqs,mapro,matar))
      endif

      return
      end

c--------------------------------------------------------------------
      double precision function S_RNDM(idum)
c--------------------------------------------------------------------
c random number generator
c--------------------------------------------------------------------
      include 'epos.inc'
      double precision drangen

      S_RNDM= drangen(dble(idum))
      if(irandm.eq.1)write(ifch,*)'S_RNDM()= ',S_RNDM,idum

      return
      end
 


C=======================================================================

      DOUBLE PRECISION FUNCTION GASDEV(IDUM)

C-----------------------------------------------------------------------
C   Gaussian deviation
c   linked to corsikas gaussian random number generator to keep
c   random number sequence intact.
C-----------------------------------------------------------------------
      IMPLICIT NONE

      DOUBLE PRECISION RANNORM,XMEAN,XDEV
      INTEGER          IDUM
      SAVE
      EXTERNAL         RANNORM
C-----------------------------------------------------------------------
      GASDEV = IDUM
      XMEAN  = 0.D0
      XDEV   = 1.D0
      GASDEV = RANNORM(XMEAN,XDEV)

      RETURN
      END


