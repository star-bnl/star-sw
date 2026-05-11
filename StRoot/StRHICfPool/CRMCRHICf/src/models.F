#ifdef __QGSJETII03__
#define __QGSJETII__ 1
#elif __QGSJETII04__
#define __QGSJETII__ 1
#else
#define __QGSJETII__ 0
#endif

      subroutine NumberModel(cmodel,model)
      character cmodel*21
      n=index(cmodel,' ')-1
      if(cmodel(1:n).eq.'epos' )     model=1
      if(cmodel(1:n).eq.'qgsjet' )then
#ifndef __QGSJET01__
         stop'please compile with requested model'
#endif
         model=2
      endif
      if(cmodel(1:n).eq.'gheisha' )then
#ifndef __GHEISHA__
         stop'please compile with requested model'
#endif
         model=3
      endif
      if(cmodel(1:n).eq.'pythia' )then
#ifndef __PYTHIA__
         stop'please compile with requested model'
#endif
         model=4
      endif
      if(cmodel(1:n).eq.'hijing' )then
#ifndef __HIJING__
         stop'please compile with requested model'
#endif
         model=5
      endif
      if(cmodel(1:n).eq.'sibyll' )then
#ifndef __SIBYLL__
         stop'please compile with requested model'
#endif
         model=6
      endif
      if(cmodel(1:n).eq.'IIqgsjet' )then
#ifndef __QGSJETII__
         stop'please compile with requested model.'
#endif
         model=7
      endif
      if(cmodel(1:n).eq.'phojet' )then
#ifndef __PHOJET__
         stop'please compile with requested model'
#endif
         model=8
      endif
      if(cmodel(1:n).eq.'fluka' )then
#ifndef __FLUKA__
         stop'please compile with requested model'
#endif
         model=9
      endif
      if(cmodel(1:n).eq.'urqmd' )then
#ifndef __URQMD__
         stop'please compile with requested model'
#endif
         model=10
      endif
      if(cmodel(1:n).eq.'dpmjet' )then
#ifndef __DPMJET__
         stop'please compile with requested model'
#endif
         model=12
      endif
      if(cmodel(1:n).eq.'IIIqgsjet' )then
#ifndef __QGSJETIII__
         stop'please compile with requested model.'
#endif
         model=13
      endif
      end

      subroutine IniModel(model)
      if(model.eq.2) then
#ifndef __QGSJET01__
         stop'please compile with requested model'
#else
         call IniQGSjet
#endif
      endif
      if(model.eq.3) then
#ifndef __GHEISHA__
         stop'please compile with requested model'
#else
         call IniGheisha
#endif
      endif
      if(model.eq.4) then
#ifndef __PYTHIA__
         stop'please compile with requested model'
#else
         call IniPythia
#endif
      endif
      if(model.eq.5) then
#ifndef __HIJING__
         stop'please compile with requested model'
#else
         call IniHijing
#endif
      endif
      if(model.eq.6) then
#ifndef __SIBYLL__
         stop'please compile with requested model'
#else
         call IniSibyll
#endif
      endif
      if(model.eq.7) then
#ifndef __QGSJETII04__
         stop'please compile with requested model.'
#else
         call IniQGSJetII
#endif
      endif
      if(model.eq.11) then
#ifndef __QGSJETII03__
         stop'please compile with requested model .'
#else
         call IniQGSJetII
#endif
      endif
      if(model.eq.13) then
#ifndef __QGSJETIII__
         stop'please compile with requested model .'
#else
         call IniQGSJetIII
#endif
      endif
      if(model.eq.8) then
#ifndef __PHOJET__
         stop'please compile with requested model'
#else
         call IniPHOJET
#endif
      endif
      if(model.eq.9) then
#ifndef __FLUKA__
         stop'please compile with requested model'
#else
         call IniFluka
#endif
      endif
      if(model.eq.10) then
#ifndef __URQMD__
         stop'please compile with requested model'
#else
         call IniUrQMD
#endif
      endif
      if(model.eq.12) then
#ifndef __DPMJET__
         stop'please compile with requested model'
#else
         call IniDPMJET
#endif
      endif
      end

      subroutine IniEvtModel
      include 'epos.inc'
      iclegy=1                  !to avoid crash in plots
      if(model.eq.2)then
#ifndef __QGSJET01__
         stop'please compile with requested model'
#else
         call IniEvtQGS
#endif
      endif
      if(model.eq.3)then
#ifndef __GHEISHA__
         stop'please compile with requested model'
#else
         call IniEvtGhe
#endif
      endif
      if(model.eq.4)then
#ifndef __PYTHIA__
         stop'please compile with requested model'
#else
         engysave=engy
         if(engy.lt.egymin)engy=egymin
         call IniEvtPyt
         engy=engysave
#endif
      endif
      if(model.eq.5)then
#ifndef __HIJING__
         stop'please compile with requested model'
#else
         engysave=engy
         if(engy.lt.egymin)engy=egymin
         call IniEvtHij
         engy=engysave
#endif
      endif
      if(model.eq.6)then
#ifndef __SIBYLL__
         stop'please compile with requested model'
#else
         call IniEvtSib
#endif
      endif
      if(model.eq.7)then
#ifndef __QGSJETII04__
         stop'please compile with requested model'
#else
         call IniEvtQGSII
#endif
      endif
      if(model.eq.11)then
#ifndef __QGSJETII03__
         stop'please compile with requested model'
#else
         call IniEvtQGSII
#endif
      endif
      if(model.eq.13)then
#ifndef __QGSJETIII__
         stop'please compile with requested model'
#else
         call IniEvtQGSIII
#endif
      endif
      if(model.eq.8)then
#ifndef __PHOJET__
         stop'please compile with requested model'
#else
         call IniEvtPho
#endif
      endif
      if(model.eq.9)then
#ifndef __FLUKA__
         stop'please compile with requested model'
#else
         call IniEvtFlu
#endif
      endif
      if(model.eq.10)then
#ifndef __URQMD__
         stop'please compile with requested model'
#else
         call IniEvtUrq
#endif
      endif
      if(model.eq.12)then
#ifndef __DPMJET__
         stop'please compile with requested model'
#else
         call IniEvtDpm
#endif
      endif
      end

      subroutine emsaaaModel(model,id,iret)
      if (0.eq.1) print *, id, iret !get rid of unused warning
      if(model.eq.2) then
#ifndef __QGSJET01__
         stop'please compile with requested model'
#else
         if(id.eq.0)call IniEvtQGS
         call emsqgs(iret)
#endif
      endif
      if(model.eq.3) then
#ifndef __GHEISHA__
         stop'please compile with requested model'
#else
         call emsghe(iret)
#endif
      endif
      if(model.eq.4) then
#ifndef __PYTHIA__
         stop'please compile with requested model'
#else
         call emspyt(iret,0)
#endif
      endif
      if(model.eq.5) then
#ifndef __HIJING__
         stop'please compile with requested model'
#else
         call emshij(iret)
#endif
      endif
      if(model.eq.6) then
#ifndef __SIBYLL__
         stop'please compile with requested model'
#else
         call emssib(iret)
#endif
      endif
      if(model.eq.7) then
#ifndef __QGSJETII04__
         stop'please compile with requested model'
#else
         if(id.eq.0)call IniEvtQGSII
         call emsqgsII(iret)
#endif
      endif
      if(model.eq.11) then
#ifndef __QGSJETII03__
         stop'please compile with requested model'
#else
         if(id.eq.0)call IniEvtQGSII
         call emsqgsII(iret)
#endif
      endif
      if(model.eq.13) then
#ifndef __QGSJETIII__
         stop'please compile with requested model'
#else
         if(id.eq.0)call IniEvtQGSIII
         call emsqgsIII(iret)
#endif
      endif
      if(model.eq.8) then
#ifndef __PHOJET__
         stop'please compile with requested model'
#else
         call emspho(iret)
#endif
      endif
      if(model.eq.9) then
#ifndef __FLUKA__
         stop'please compile with requested model'
#else
         call emsflu(iret)
#endif
      endif
      if(model.eq.10) then
#ifndef __URQMD__
         stop'please compile with requested model'
#else
         call emsurq(iret)
#endif
      endif
      if(model.eq.12) then
#ifndef __DPMJET__
         stop'please compile with requested model'
#else
         call emsdpmjet(iret)
#endif
      endif
      end


      subroutine crseaaModel(sigt,sigi,sigc,sige)
      include 'epos.inc'
      sigt=0.
      sigc=0.
      sigi=0.
      sige=0.
      if(model.eq.3.or.model.eq.4.or.model.eq.8)return !no AA with Gheisha, Pythia, Phojet
      if(idtarg.eq.0.and.model.ne.9)then
         kmax=3
         if(model.eq.6)kmax=2   !no Argon with SIBYLL
         do k=1,kmax
            matarg=int(airanxs(k))
            call crseaaModel0(xsigt,xsigi,xsigc,xsige)
            sigt=sigt+airwnxs(k)*xsigt
            sigi=sigi+airwnxs(k)*xsigi
            sigc=sigc+airwnxs(k)*xsigc
            sige=sige+airwnxs(k)*xsige
         enddo
      else
         call crseaaModel0(sigt,sigi,sigc,sige)
*          write (*,*) 'in models aft crs',
*     & sigt,sigi,sigc,sige
      endif
      end

      subroutine crseaaModel0(sigt,sigi,sigc,sige)
      include 'epos.inc'
      common/geom/rmproj,rmtarg,bmax,bkmx

      double precision GTOT,GPROD,GABS,GDD,GQEL,GCOH
      double precision e0
#ifdef __SIBYLL__
      double precision engy_d,ssig,slope,rho,sigt_d,sige_d,sigqel_d,
     &        sigsd_d,sigqsd_d
*** P.S. added 28 march 2022
      double precision dum0,dum1,dumdif,alam
      double precision SSIGNUC,alnuc
      COMMON /CLENNN/ SSIGNUC(60), ALNUC(60)
#endif
      dimension dumdif(3)
      if(model.eq.2)then
#ifndef __QGSJET01__
         stop'please compile with requested model'
         print *, GTOT,GPROD,GABS,GDD,GQEL,GCOH,sigt,sigi,sigc,sige,e0 !get rid of unused warning
#else
         NITER=20000
         if(idtarg.eq.0)then
            e0=dble(elab)
            icp=idtrafo('nxs','qgs',idproj)
            call xxaini(e0,icp,maproj,matarg)
         endif
         CALL CROSSC(NITER,GTOT,GPROD,GABS,GDD,GQEL,GCOH)
         sigt=sngl(GTOT)
         sigi=sngl(GPROD)
         sigc=sngl(GABS)
         sige=sigt-sigi
#endif
      elseif(model.eq.3)then
#ifndef __GHEISHA__
         stop'please compile with requested model'
#else
         idtar=idtarg
         if(idtarg.eq.0)idtar=1120
         call ghecrse(ekin,idproj,idtar,latarg,matarg,sigi,sige)
         sigt=sigi+sige
         sigc=sigi
#endif
      elseif(model.eq.5)then
#ifndef __HIJING__
         stop'please compile with requested model'
#else
         call hjcrossc(sigi,sigt)
#endif
      elseif(model.eq.6)then
#ifndef __SIBYLL__
         stop'please compile with requested model'
         print *, dumdif(3)!get rid of unused warning
#else
        if(maproj.eq.1)then
         K=1
         if(iclpro.eq.1)then
            K=2
         elseif(iclpro.eq.3)then
            K=3
         endif
         engy_d = dble(engy)
*         write (*,*)  'in models bef sigma ', K, engy_d, engy
         CALL SIB_SIGMA_HP(K,engy_d,SSIG,dum0,dum1,dumdif,SLOPE,RHO)
*         write (*,*) 'in models creaao0 bef glauber2',k,
*     &        engy_d,dum0,dum1,dumdif(1),dumdif(2),dumdif(3),SSIG,
*     &   SLOPE,RHO,matarg
*         CALL GLAUBER2
*     &        (matarg,SSIG,SLOPE,RHO,sigt_d,sige_d,sigqel_d,
*     &        sigsd_d,sigqsd_d)
***   P.S. added 28 march 2022
***   patch looking at how glauber2 is called within sibyll 
         ALAM = SQRT (dumdif(1)/dum0)
         CALL GLAUBER2
     &        (matarg,SSIG,SLOPE,RHO, ALAM, sigt_d,sige_d,sigqel_d,
     &        sigsd_d,sigqsd_d)
*          write (*,*) 'in models creaa',matarg,SSIG,SLOPE,RHO,
*     &   sigt_d,sige_d,sigqel_d,sigsd_d,sigqsd_d
*           call fflush
         sigt=sngl(sigt_d)
         sige=sngl(sige_d)
         sigqel=sngl(sigqel_d)
         sigi=sngl(sigt_d-sigqel_d)
*          write (*,*) 'in models creaa 2',
*     &        sigt,sige,sigqel,sigi
*          call fflush
        elseif(idtarg.eq.0)then
         E0=dble(elab)*1.d-3          !e0 in TeV
         CALL  SIGNUC_INI(maproj,E0) !  fills SSIGNUC and ALNUC
         sigi=sngl(SSIGNUC(maproj))
         sigt=0.
         sige=0.
         sigqel=0.
        else
         sigi=0.
         sigt=0.
         sige=0.
         sigqel=0.
        endif
         sigc=sigi
#endif
      elseif(model.eq.7.or.model.eq.11.or.model.eq.13)then
#if !__QGSJETII__ && !__QGSJETIII__
         stop'please compile with requested model'
#else
         NITER=20000
         if(idtarg.eq.0)then
            e0=dble(elab)
            icp=idtrafo('nxs','qgs',idproj)
            call qgini(e0,icp,maproj,matarg)
         endif
         CALL qgcrossc(NITER,GTOT,GPROD,GABS,GDD,GQEL,GCOH)
         sigt=sngl(GTOT)
         sigi=sngl(GPROD)
         sigc=sngl(GABS)
         sige=sigt-sigi
#endif
      elseif(model.eq.9)then
#ifndef __FLUKA__
         stop'please compile with requested model'
#else
         sigt=0.
         sige=0.
         sigc=0.
         sigi=flucrse(ekin,maproj,matarg,idtargin)
#endif
      elseif(model.eq.10)then
#ifndef __URQMD__
         stop'please compile with requested model'
#else
         sigi=0.
         sige=0.
         sigc=0.
         sigt=urqcrse(ekin,idproj,maproj,matarg,idtarg,bmax)
#endif
      elseif(model.eq.12)then
#ifndef __DPMJET__
         stop'please compile with requested model'
#else
         NITER=20000
         call GetDPMJETSigmaAA(niter,sigt,sigi,sigc,sige)
#endif
      else
         sigt=0.
         sigi=0.
         sige=0.
         sigc=0.
      endif
      end


      subroutine m2XXFZ( a,b)
      double precision a,b
#ifndef __QGSJET01__
      stop'please compile with requested model'
      print *, a, b             !get rid of unused warning
#else
      CALL XXFZ(a,b)
#endif
      end

      subroutine m3SIGMA(ek,idpro,idtar,latar,matar,sigi,sige)
#ifndef __GHEISHA__
      stop'please compile with requested model'
      print *, ek,idpro,idtar,latar,matar,sigi,sige !get rid of unused warning
#else
      call ghecrse(ek,idpro,idtar,latar,matar,sigi,sige)
#endif
      end

      subroutine m6SIGMA(icl,engy,stot,sela,sine,sdifr,slela,Rho)
#ifdef __SIBYLL__
      double precision engy_d,stot_d,sela_d,sine_d,slela_d,rho_d
      double precision sdifr0_d(3)
#endif

#ifndef __SIBYLL__
      stop'please compile with requested model'
      print *, icl,engy,stot,sela,sine,sdifr,slela,Rho,sdifr0 !get rid of unused warning
#else
      if(icl.eq.1)then
         L=2
      elseif(icl.eq.2)then
         L=1
      else
         L=3
      endif
      engy_d = dble(engy)
      CALL SIB_SIGMA_HP
     &     (L,engy_d,stot_d,sela_d,sine_d,sdifr0_d,slela_d,Rho_d)
      stot   =  sngl(  stot_d  )
      sela   =  sngl(  sela_d  )
      sine   =  sngl(  sine_d  )
      slela  =  sngl(  slela_d )
      Rho    =  sngl(  Rho_d   )
      sdifr=sngl(sdifr0_d(1)+sdifr0_d(2)+sdifr0_d(3))
#endif
      end


      subroutine m7SIGMA(stot,scut,sine,slela)
      double precision bmsave
      double precision GzZ0(5),pi,bm,amws
      common /qgarr1/  ia(2),icz,icp
      common /qgarr6/  pi,bm,amws
#if !__QGSJETII__ && !__QGSJETIII__
      stop'please compile with requested model QII'
      print *, stot,scut,sine,slela,bmsave,GzZ0(5),pi,bm,amws !get rid of unused warning
#else
      ia2save=ia(2)
      bmsave=bm
      ia(2)=1
      call qgfz(0.d0,gzz0,0,0)
      scut=sngl(gzz0(2))/2.     !cut pomerons cross-section
      stot=sngl(gzz0(1))        !tot cross-section
      sine=sngl(gzz0(2)+gzz0(3)+gzz0(4))/2. !inelastic cross section
      slela=sngl(gzz0(5))
      ia(2)=ia2save
      bm=bmsave
#endif
      end

      subroutine m8SIGMA(stot,scut,sine,sela,slela,ssd)
C     cross sections
      INTEGER IPFIL,IFAFIL,IFBFIL
      DOUBLE PRECISION SIGTOT,SIGELA,SIGVM,SIGINE,SIGNDF,SIGDIR,
     &     SIGLSD,SIGHSD,SIGLDD,SIGHDD,SIGCDF,
     &     SIGPOM,SIGREG,SIGHAR,SIGTR1,SIGTR2,SIGLOO,
     &     SIGDPO,SIG1SO,SIG1HA,SLOEL,SLOVM,SIGCOR,
     &     FSUP,FSUD,FSUH,ECMFIL,P2AFIL,P2BFIL
      COMMON /POCSEC/ SIGTOT,SIGELA,SIGVM(0:4,0:4),SIGINE,SIGNDF,SIGDIR,
     &     SIGLSD(2),SIGHSD(2),SIGLDD,SIGHDD,SIGCDF(0:4),
     &     SIGPOM,SIGREG,SIGHAR,SIGTR1(2),SIGTR2(2),SIGLOO,
     &     SIGDPO(4),SIG1SO,SIG1HA,SLOEL,SLOVM(4,4),SIGCOR,
     &     FSUP(2),FSUD(2),FSUH(2),ECMFIL,P2AFIL,P2BFIL,
     &     IPFIL,IFAFIL,IFBFIL
#ifndef __PHOJET__
         stop'please compile with requested model'
         print *, stot,scut,sine,sela,slela,ssd !get rid of unused warning
#else
      stot=sngl(SIGTOT)
      sine=sngl(SIGTOT-SIGELA)
      sela=sngl(SIGELA)
      slela=sngl(SLOEL)
      ssd=sngl(SIGLSD(1)+SIGHSD(1)+SIGLSD(2)+SIGHSD(2))
      sdd=sngl(SIGLDD+SIGHDD)
      scut=sine-ssd-sdd-sngl(SIGCDF(0))
#endif
      end

      subroutine m9SIGMA(stot,sine,sela)
      include "epos.inc"
      double precision PLA,EKIN1,SHPTOT,SHPINE,ZZ,AA,Sel,Zl
#ifndef __FLUKA__
         stop'please compile with requested model'
         print *, stot,sine,sela
     *          ,PLA,EKIN1,SHPTOT,SHPINE,ZZ,AA,Sel,Zl !get rid of unused warning
#else
      if(iclpro.eq.1)then
      IP=13
      elseif(iclpro.eq.2)then
      IP=1
      else
      IP=15
      endif
      PLA=dble(pnll)
      EKIN1=dble(ekin)
      ZZ=1
      AA=1
      stot=SHPTOT(IP,PLA)
      sine=SHPINE(IP,PLA)
      call SIGELH(IP,ZZ,AA,EKIN1,PLA,Sel,Zl)
      sela=sngl(Sel)
#endif
      end

      subroutine dpmjetSIGMA(stot,sine,sela)

#ifndef __DPMJET__
         stop'please compile with requested model'
         print *, stot,sine,sela !get rid of unused warning
#else
      call GetDPMJETSigma(stot,sine,sela)
#endif
      end

      subroutine decaymod(ip,iret)
      include 'epos.inc'
      if(model.eq.4)then
#ifndef __PYTHIA__
         stop'please compile with requested model'
         print *, ip,iret !get rid of unused warning
#else
        call emspyt(iret,ip)
#endif
      endif
      end

#if  __PYTHIA__ || __PHOJET__ || __DPMJET__

c-----------------------------------------------------------------------
      subroutine IniDkyJetset
c-----------------------------------------------------------------------
c Primary initialization for decay in Jetset (Pythia)
c-----------------------------------------------------------------------
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      include 'epos.inc'

      COMMON/PYDAT1/MSTU(200),PARU(200),MSTJ(200),PARJ(200)
      COMMON/PYDAT3/MDCY(500,3),MDME(8000,2),BRAT(8000),KFDP(8000,5)

      character*100 NameDecay
c     Integer function to translate normal id from pythia to compact one.
      integer PYCOMP

c first set all decays to 1 in Jetset (Pythia)
      MDCY(13,1)=1     !muon do not decay by default in Jetset
      do i=100,500
        MDCY(i,1)=1
      enddo

c set values according to EPOS settings

      if(idecay.eq.1)then

      MSTJ(21) = 1  !decay active

      if(mod(ndecay/10,10).eq.1)then          !Kshort, Klong
        ic1=PYCOMP(130)
        ic2=PYCOMP(310)
        write(NameDecay,'(a,i3,a,i3,a)')
     &       'MDCY(',ic1,',1)=0;MDCY(',ic2,',1)=0'
        call PYGIVE(NameDecay(1:27))
      endif
      if(mod(ndecay/100,10).eq.1)then         !Lambda
        ic=PYCOMP(3122)
        write(NameDecay,'(a,i3,a)')
     &       'MDCY(',ic,',1)=0'
        call PYGIVE(NameDecay(1:13))
      endif
      if(mod(ndecay/1000,10).eq.1)then        !sigma+
        ic=PYCOMP(3222)
        write(NameDecay,'(a,i3,a)')
     &       'MDCY(',ic,',1)=0'
        call PYGIVE(NameDecay(1:13))
      endif
      if(mod(ndecay/1000,10).eq.1)then        !sigma-
        ic=PYCOMP(3112)
        write(NameDecay,'(a,i3,a)')
     &       'MDCY(',ic,',1)=0'
        call PYGIVE(NameDecay(1:13))
      endif
      if(mod(ndecay/10000,10).eq.1)then       !Xi+/-
        ic=PYCOMP(3312)
        write(NameDecay,'(a,i3,a)')
     &       'MDCY(',ic,',1)=0'
        call PYGIVE(NameDecay(1:13))
      endif
      if(mod(ndecay/10000,10).eq.1)then       !Xi0
        ic=PYCOMP(3322)
        write(NameDecay,'(a,i3,a)')
     &       'MDCY(',ic,',1)=0'
        call PYGIVE(NameDecay(1:13))
      endif
      if(mod(ndecay/100000 ,10).eq.1)then     !omega
        ic=PYCOMP(3334)
        write(NameDecay,'(a,i3,a)')
     &       'MDCY(',ic,',1)=0'
        call PYGIVE(NameDecay(1:13))
      endif
      if(mod(ndecay/1000000,10).eq.1)then     !pi0
        ic=PYCOMP(111)
        write(NameDecay,'(a,i3,a)')
     &       'MDCY(',ic,',1)=0'
        call PYGIVE(NameDecay(1:13))
      endif

      if(nrnody.gt.0)then                      !all other particle
        do nod=1,nrnody
          ic=idtrafo('nxs','pdg',nody(nod))
          if(ic.lt.100.and.ic.gt.0)then
            write(NameDecay,'(a,i2,a)')'MDCY(C',ic,',1)=0'
            call PYGIVE(NameDecay(1:13))
          elseif(ic.lt.1000.and.ic.gt.-100)then
            write(NameDecay,'(a,i3,a)')'MDCY(C',ic,',1)=0'
            call PYGIVE(NameDecay(1:14))
          elseif(ic.lt.-1000.and.ic.gt.-10000)then
            write(NameDecay,'(a,i5,a)')'MDCY(C',ic,',1)=0'
            call PYGIVE(NameDecay(1:16))
          elseif(abs(ic).lt.10000)then
            write(NameDecay,'(a,i4,a)')'MDCY(C',ic,',1)=0'
            call PYGIVE(NameDecay(1:15))
          endif
        enddo
      endif

      if(ctaumin.gt.0.)then

        MSTJ(22) = 2   !switch on decay according to life time
        PARJ(71) = ctaumin*10.  !cm->mm

      endif

      else

        MSTJ(21) = 0   !switch off all decays

      endif

      END
      
c--------------------------------------------------------------------
      double precision function pyr(idummy)
c--------------------------------------------------------------------
c  returns random number between 0 and 1 excluding the limits
c--------------------------------------------------------------------
      double precision drangen
      pyr=drangen(dble(idummy))

      return
      end


C*********************************************************************

C...PDFSET
C...Dummy routine, to be removed when PDFLIB is to be linked.

      SUBROUTINE PDFSET(PARM,VALUE)

C...Double precision and integer declarations.
      IMPLICIT DOUBLE PRECISION(A-H, O-Z)
      IMPLICIT INTEGER(I-N)
C...Commonblocks.
      COMMON/PYDAT1/MSTU(200),PARU(200),MSTJ(200),PARJ(200)
      SAVE /PYDAT1/
C...Local arrays and character variables.
      CHARACTER*20 PARM(20)
      DOUBLE PRECISION VALUE(20)

C...Stop program if this routine is ever called.
      WRITE(MSTU(11),5000)
      IF(PYR(0).LT.10D0) STOP
      PARM(20)=PARM(1)
      VALUE(20)=VALUE(1)

C...Format for error printout.
 5000 FORMAT(1X,'Error: you did not link PDFLIB correctly.'/
     &1X,'Dummy routine PDFSET in PYTHIA file called instead.'/
     &1X,'Execution stopped!')

      RETURN
      END

C*********************************************************************

C...STRUCTM
C...Dummy routine, to be removed when PDFLIB is to be linked.

      SUBROUTINE STRUCTM(XX,QQ,UPV,DNV,USEA,DSEA,STR,CHM,BOT,TOP,GLU)

C...Double precision and integer declarations.
      IMPLICIT DOUBLE PRECISION(A-H, O-Z)
      IMPLICIT INTEGER(I-N)
C...Commonblocks.
      COMMON/PYDAT1/MSTU(200),PARU(200),MSTJ(200),PARJ(200)
      SAVE /PYDAT1/
C...Local variables
      DOUBLE PRECISION XX,QQ,UPV,DNV,USEA,DSEA,STR,CHM,BOT,TOP,GLU

C...Stop program if this routine is ever called.
      WRITE(MSTU(11),5000)
      IF(PYR(0).LT.10D0) STOP
      UPV=XX+QQ
      DNV=XX+2D0*QQ
      USEA=XX+3D0*QQ
      DSEA=XX+4D0*QQ
      STR=XX+5D0*QQ
      CHM=XX+6D0*QQ
      BOT=XX+7D0*QQ
      TOP=XX+8D0*QQ
      GLU=XX+9D0*QQ

C...Format for error printout.
 5000 FORMAT(1X,'Error: you did not link PDFLIB correctly.'/
     &1X,'Dummy routine STRUCTM in PYTHIA file called instead.'/
     &1X,'Execution stopped!')

      RETURN
      END

C*********************************************************************

C...STRUCTP
C...Dummy routine, to be removed when PDFLIB is to be linked.

      SUBROUTINE STRUCTP(XX,QQ2,P2,IP2,UPV,DNV,USEA,DSEA,STR,CHM,
     &BOT,TOP,GLU)

C...Double precision and integer declarations.
      IMPLICIT DOUBLE PRECISION(A-H, O-Z)
      IMPLICIT INTEGER(I-N)
C...Commonblocks.
      COMMON/PYDAT1/MSTU(200),PARU(200),MSTJ(200),PARJ(200)
      SAVE /PYDAT1/
C...Local variables
      DOUBLE PRECISION XX,QQ2,P2,UPV,DNV,USEA,DSEA,STR,CHM,BOT,
     &TOP,GLU

C...Stop program if this routine is ever called.
      WRITE(MSTU(11),5000)
      IF(PYR(0).LT.10D0) STOP
      UPV=XX+QQ2
      DNV=XX+2D0*QQ2
      USEA=XX+3D0*QQ2
      DSEA=XX+4D0*QQ2
      STR=XX+5D0*QQ2
      CHM=XX+6D0*QQ2
      BOT=XX+7D0*QQ2
      TOP=XX+8D0*QQ2
      GLU=XX+9D0*QQ2

C...Format for error printout.
 5000 FORMAT(1X,'Error: you did not link PDFLIB correctly.'/
     &1X,'Dummy routine STRUCTP in PYTHIA file called instead.'/
     &1X,'Execution stopped!')

      RETURN
      END
#endif

#if  (__PYTHIA__ ||  __DPMJET__) && !__PHOJET__

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

#endif
