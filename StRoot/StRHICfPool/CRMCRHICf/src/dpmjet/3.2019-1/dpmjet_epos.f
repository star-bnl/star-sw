c 13.04.2010 Link routines between DPMJET and CRMC.
c authors C. Baus, A. Feydnitch
c 12.04.2017 update for DPMJET III (2017.1) by T. Pierog
c-----------------------------------------------------------------------
      subroutine IniDPMJET
c-----------------------------------------------------------------------
c Primary initialization for DPMJET
c-----------------------------------------------------------------------
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      include 'epos.inc'
c event number
      COMMON /DTEVNO/ NDPMEVENT,ICASCA

c     common block var iframe renamed to idframe since it is used by epos
      LOGICAL          LEMCCK,LHADRO,LSEADI,LEVAPO
      COMMON /DTFLG1/  IFRAG(2),IRESCO,IMSHL,IRESRJ,IOULEV(6),
     &                 LEMCCK,LHADRO(0:9),LSEADI,LEVAPO,IDFRAME,ITRSPT,
     &                 IFUSION,IFLOW
      COMMON /DTFLKA/ LINP,LOUT,LDAT, LPRI
* central particle production, impact parameter biasing
      COMMON /DTIMPA/ BIMIN,BIMAX,XSFRAC,ICENTR
C  model switches and parameters
      CHARACTER*8 MDLNA
      INTEGER ISWMDL,IPAMDL
      DOUBLE PRECISION PARMDL
      COMMON /POMDLS/ MDLNA(50),ISWMDL(50),PARMDL(400),IPAMDL(400)
c      CHARACTER*132    DATADIR
* Glauber formalism: flags and parameters for statistics
      LOGICAL LPROD
      CHARACTER*8 CGLB
      COMMON /DTGLGP/ JSTATB,JBINSB,CGLB,IOGLB,LPROD
c  emulsion treatment
      PARAMETER        (NCOMPX=100,NEB=8,NQB= 5,KSITEB=50)
      COMMON /DTCOMP/  EMUFRA(NCOMPX),IEMUMA(NCOMPX),IEMUCH(NCOMPX),
     &                 NCOMPO,IEMUL
c  nucleon-nucleon event-generator
      PARAMETER ( MXFFBK =     6 )
      PARAMETER ( MXZFBK =    10 )
      PARAMETER ( MXNFBK =    12 )
      PARAMETER ( MXAFBK =    16 )
      PARAMETER ( MXPSST =   700 )
      PARAMETER ( MXPSFB = 43000 )
      PARAMETER ( MXASST =    25)
      PARAMETER ( NXAFBK = MXAFBK + 1 )
      PARAMETER ( NXZFBK = INT(MXZFBK + MXFFBK / 3 
     &                     + MXASST - NXAFBK) )
      PARAMETER ( NXNFBK = INT(MXNFBK + MXFFBK / 3
     &                     + MXASST - NXAFBK) )
      LOGICAL          LFRMBK, LNCMSS
      COMMON /FRBKCM/  AMUFBK, EEXFBK(MXPSST), AMFRBK(MXPSST),
     &                 WEIFBK(MXPSST) ,GAMFBK(MXPSST), EXFRBK(MXPSFB),
     &                 SDMFBK(MXPSFB), COUFBK(MXPSFB), CENFBK(MXPSFB),
     &                 EXMXFB, R0FRBK, R0CFBK, C1CFBK, C2CFBK, FRBKLS,
     &                 IFRBKN(MXPSST), IFRBKZ(MXPSST), IFBKSP(MXPSST),
     &                 IFBKPR(MXPSST), IFBKST(MXPSST), IFBKLV(MXPSST),
     &                 IPSIND(0:NXNFBK,0:NXZFBK,2), JPSIND(0:MXASST),
     &                 IFBIND(0:NXNFBK,0:NXZFBK,2), JFBIND(0:NXAFBK),
     &                 IFBCHA(9,MXPSFB), IPOSST,IPOSFB,IFBSTF, IFBPSF,
     &                 IFBPSI, IFBFRB, IFBCHN, IFBNC1, IFBNC2, NBUFBK,
     &                 LFRMBK, LNCMSS
c  properties of interacting particles
      LOGICAL          LDIFFR,LINCTV,LEVPRT,LHEAVY,LDEEXG,LGDHPR,LPREEX,
     &                 LHLFIX,LPRFIX,LPARWV,LPOWER,LSNGCH,LSCHDF,LHADRI,
     &                 LNUCRI,LPEANU,LEVBME,LPHDRC,LATMSS,LISMRS,LCHDCY,
     &                 LCHDCR,LMLCCR,LRVKIN,LVP2XX,LV2XNW,LNWV2X,LEVFIN
      PARAMETER       (NALLWP = 64)
      COMMON /PAREVT/  DPOWER, FSPRD0, FSHPFN, RN1GSC, RN2GSC, RNSWTC,
     &                 LDIFFR(NALLWP), LPOWER, LINCTV, LEVPRT, LHEAVY,
     &                 LDEEXG, LGDHPR, LPREEX, LHLFIX, LPRFIX, LPARWV,
     &                 LSNGCH, LSCHDF, LHADRI, LNUCRI, LPEANU, LEVBME,
     &                 LPHDRC, LATMSS, LISMRS, LCHDCY, LCHDCR, LMLCCR,
     &                 LRVKIN, LVP2XX, LV2XNW, LNWV2X, LEVFIN

      LPRI = 1+ish
      LOUT = ifch

      call utpri('inidpm',ish,ishini,6)
      write(ifmt,'(a,i6)')'initialize DPMjet ...'

      IDFRAME = 2               !dpmjet iframe variable. nucleon-nucleon frame
      NDPMEVENT = 0             !event number needed in dpmjet
      IFUSION = 1     ! ENABLE FUSION IN NUCLEUS-NUCLEUS COLLISIONS
      ICASCA = 1      !  NO STATISTICAL INFO NEEDED IN SHOWER SIMULATION
C  EVAPORATION MODULES NOT AVAILABLE WITH THIS VERSION
      LEVPRT = .FALSE.
      LDEEXG = .FALSE.
      LHEAVY = .FALSE.
      LFRMBK = .FALSE.
      CGLB   = 'conextar'
c DO NOT USE EMULSION : CREATE SOME PROBLEM WITH ENERGY CONSERVATION (BUG IN INTERFACE ?)
C BUT TABULATION CAN BE USED ANY WAY ....
c      if(idtargin.eq.0)then      !Air
c        CGLB   = 'conextar'
cC       GLAUBER DATA SET CONTAINS TARGET EMULSION WITH 14N, 16O, 40AR
c        IEMUL  = 1
c        NCOMPO = 3
c        NB     = 40
c        IEMUMA(1) = 14          ! NITROGEWN ARGET
c        IEMUCH(1) = 7
c        EMUFRA(1) = airwnxs(1)
c        IEMUMA(2) = 16          ! OXYGEN TARGET
c        IEMUCH(2) = 8
c        EMUFRA(2) = airwnxs(2)
c        IEMUMA(3) = 40          ! ARGON TARGET
c        IEMUCH(3) = 18
c        EMUFRA(3) = airwnxs(3)
c      endif

      egymin = 4.              !min energy for model
      egymax = 2.e6             !max energy for model

      BIMIN = bminim            !impact parameter range
      BIMAX = bmaxim

      irescl = 0                !don't rescale/skip events with wrong energy
      infragm = 0            !keep nuclear fragment as they are
      istmax=min(istmax,1)   !to avoid unnecessary particle in final analysis
      isigma = 2             !to get proper cross-section

      call utprix('inidpm',ish,ishini,6)
      END

c-----------------------------------------------------------------------
      subroutine IniEvtDpm
c-----------------------------------------------------------------------
c Setting energy, primaries,... for each event class. Useful for e.g. conex
c-----------------------------------------------------------------------
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      include 'epos.inc'
      real rmproj,rmtarg,bmax,bkmx
      common/geom/rmproj,rmtarg,bmax,bkmx
      integer ifirst
      data ifirst/0/
      save ifirst

*     particle properties (BAMJET index convention)
      CHARACTER*8  ANAME
      COMMON /DTPART/ ANAME(210),AAM(210),GA(210),TAU(210),
     &     IICH(210),IIBAR(210),K1(210),K2(210)

C  model switches and parameters
      CHARACTER*8 MDLNA
      INTEGER ISWMDL,IPAMDL
      DOUBLE PRECISION PARMDL
      COMMON /POMDLS/ MDLNA(50),ISWMDL(50),PARMDL(400),IPAMDL(400)
c     fresh common block
      INTEGER NPMASS,NPCHAR,NTMASS,NTCHAR,IDPDG
      DOUBLE PRECISION EPROJ,EINID
      COMMON /DPMEVTINI/ EPROJ,NPMASS,NPCHAR,NTMASS,NTCHAR,IDPDG

      COMMON/PYDAT2/KCHG(500,4),PMAS(500,4),PARF(2000),VCKM(4,4)
      COMMON/PYDAT3/MDCY(500,3),MDME(8000,2),BRAT(8000),KFDP(8000,5)
      COMMON/PYPARS/MSTP(200),PARP(200),MSTI(200),PARI(200)
* properties of interacting particles
      COMMON /DTPRTA/ IT,ITZ,IP,IPZ,IJPROJ,IBPROJ,IJTARG,IBTARG

c      dimension WHAT(6)

      integer istatus
      real taugm

      call utpri('idpmev',ish,ishini,6)

      EPROJ  = dble(elab)
      NPMASS = maproj
      NPCHAR = laproj
      NTMASS = matarg
      NTCHAR = latarg

      IF(laproj.eq.-1) THEN     !not nucleus
         IDPDG = idtrafo('nxs','pdg',idproj)
         IDP=IDT_ICIHAD(IDPDG)
         NPCHAR = IICH(IDP)
      ELSE
c     will be treated as nucleus in DT_INIT and NPMASS,... will be used
         IDPDG = 0
         IDP = 0
      ENDIF
      if(abs(idtarg).ne.1120.and.idtarg.ne.0)then
        call utstop("Only (anti)proton or nucleus as target !&")
      else
        NTCHAR = sign(abs(latarg),idtarg)
      endif
      bmax=3.+(maproj+matarg)**0.4

      if(ifirst.eq.0)then
        
      ifirst=1


C     general initialization
      if(noebin.gt.1)then       !if more than one energy bin, initialize for
        if(iologl.ge.0)then
          EINID = dble(engmax)+1d0   !highest kin energy
        else
          EINID = 0.5d0*dble(engmax**2)   !highest cms energy
        endif
      else
        EINID = EPROJ
      endif
      NCASES = -2               !skip reading steering cards and fixed energy
      if(idtargin.eq.0)NCASES = -1
c      NCASES = -100             !make cross-section table (using iron as projectile and idtarg=0 for air)

      IGLAU = 0
c      write(ifmt,*)'dt_init',NCASES,EPROJ,NPMASS,NPCHAR,NTMASS,NTCHAR
c     +,IDP,IGLAU
c      CALL DT_INIT(NCASES,EINID,NPMASS,NPCHAR,NTMASS,NTCHAR,IDP,IGLAU)
      CALL DT_DTUINI(NCASES,EINID,NPMASS,NPCHAR,NTMASS,NTCHAR,IDP,IEMU)

      IPAMDL(179)=1000000000    !prevent Phojet to stop because of inconsistencies

* pre-initialization of profile function for nuclear collisions*
c      ECM=dble(ecms)
c      CALL DT_XSGLAU(NPMASS,NTMASS,IJPROJ,0D0,0D0,ECM,1,1,-1)

c set decay flag in Pythia for DPMJET (after DT_INIT otherwise default is used)
      call IniDkyJetset

c particle without breit-wigner decay will be decayed in EPOS to get full history

      do i=100,500
         idpdgi = KCHG(I,4)

c     don't touch decays with unknown code
         idepos = idtrafostatus('pdg','nxs',idpdgi,istatus)
         if(istatus.ne.0) then
            go to 100
         endif

c     don't touch decays with unknown width
         call idtaustatus(idepos,0,0,taugm,istatus)
c        print *,i,(taugm.eq.indexOutOfRange.or.taugm.eq.iijjOutOfRange)
c    +        ,(PMAS(i,2).LT.PARP(41))
         if(istatus.ne.0) then
            go to 100
         endif

c     don't touch decays with too small width
         if(PMAS(i,2).LT.PARP(41)) then
            go to 101
         endif

c     because epos var ifrade is not 0 it will automatically use epos to decay when switched off in pythia
         if(ish.ge.2) write(ifch,*)
     +        "CF Id:",i,PMAS(i,1),PMAS(i,2),
     +        "PDG ID:",KCHG(I,4),
     +        "(decay in EPOS)"
         MDCY(i,1)=0
         goto 101
c if unkown particle in EPOS, decay it in DPMJET
 100     if(MDCY(i,1).ne.1.and.ish.ge.2) write(ifch,*)
     +        "CF Id:",i,PMAS(i,1),PMAS(i,2),
     +        "PDG ID:",KCHG(I,4),
     +        "(decay in DPMJET)"
         MDCY(i,1)=1
c 101     write(ifch,*)
c     +        "CF Id:",i,PMAS(i,1),PMAS(i,2),
c     +        "PDG ID:",KCHG(I,4),MDCY(i,1),istatus
c       enddo
 101  enddo                     !loop over 100-500 particle ids

c     initialize cross-sections by calling epos-bas.f function -> models.f -> dpmjet-crmc.f
      endif

      call xsigma

      dpmincs=sigineaa

      if(ish.ge.2)write(ifch,*)
     &  'DPMJET used with (Elab,idproj,idtarg,xs) ',EPROJ,idproj,idtarg
     &                                     ,dpmincs,elab,ecms,engy,ekin

      call utprix('idpmev',ish,ishini,6)

      END
c-----------------------------------------------------------------------
      subroutine emsdpmjet(iret)
c-----------------------------------------------------------------------
c  call DPMJET to simulate interaction
c-----------------------------------------------------------------------
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      include 'epos.inc'
      real rmproj,rmtarg,bmax,bkmx
      common/geom/rmproj,rmtarg,bmax,bkmx
      real yprj,ytrg,b1,b2,a,rangen
*     event history
      PARAMETER (NMXHKK=250000)
      COMMON /DTEVT1/ NHKK,NEVHKK,ISTHKK(NMXHKK),IDHKK(NMXHKK),
     &     JMOHKK(2,NMXHKK),JDAHKK(2,NMXHKK),
     &     PHKK(5,NMXHKK),VHKK(4,NMXHKK),WHKK(4,NMXHKK)
*     extended event history
      COMMON /DTEVT2/ IDRES(NMXHKK),IDXRES(NMXHKK),NOBAM(NMXHKK),
     &     IDBAM(NMXHKK),IDCH(NMXHKK),NPOINT(10),
     &     IHIST(2,NMXHKK)
*     particle properties (BAMJET index convention)
      CHARACTER*8  ANAME
      COMMON /DTPART/ ANAME(210),AAM(210),GA(210),TAU(210),
     &     IICH(210),IIBAR(210),K1(210),K2(210)
* central particle production, impact parameter biasing
      COMMON /DTIMPA/ BIMIN,BIMAX,XSFRAC,ICENTR
* Glauber formalism: collision properties
      COMMON /DTGLCP/ RPROJ,RTARG,BIMPAC,
     &                NWTSAM,NWASAM,NWBSAM,NWTACC,NWAACC,NWBACC
* properties of interacting particles
      COMMON /DTPRTA/ IT,ITZ,IP,IPZ,IJPROJ,IBPROJ,IJTARG,IBTARG
c event number
      COMMON /DTEVNO/ NDPMEVENT,ICASCA

c  emulsion treatment
      PARAMETER (NCOMPX=100,NEB=8,NQB= 5,KSITEB=50)
      COMMON /DTCOMP/  EMUFRA(NCOMPX),IEMUMA(NCOMPX),IEMUCH(NCOMPX),
     &                 NCOMPO,IEMUL

      INTEGER NPMASS,NPCHAR,NTMASS,NTCHAR,IDPDG
      INTEGER IREJ, KKMAT
      DOUBLE PRECISION EPROJ
      COMMON /DPMEVTINI/ EPROJ,NPMASS,NPCHAR,NTMASS,NTCHAR,IDPDG
      integer nptlhep(NMXHKK)

      iret=0
      ncol=0
      b1=bminim
      b2=min(bmax,bmaxim)
      a=pi*(b2**2-b1**2)

      if(a.gt.0..and.rangen().gt.dpmincs/10./a)goto 1001   !no interaction
      if(ish.ge.3)call alist('Determine DPMJET Production&',0,0)

      call conre !! adds beam particles that are known from intial call of crmc
      call conwr

      IREJ = 0
      KKMAT = -1
      if (IEMUL.gt.0)then
        CALL DT_GETEMU(NTMASS,NTCHAR,KKMAT,0)
        KKMAT = -KKMAT       !tables already in memory
      endif

C     call dpmjet event simulation
      NDPMEVENT = NDPMEVENT+1   !needs to be updated for internal init. sets NEVHKK
c projecticle and target can be changed without reinitialization of the event
      IF (IDPDG.EQ.0) THEN
         IDP = 1
      ELSE
         if(irdmpr.ne.0.and.laproj.eq.-1)
     .   IDPDG = idtrafo('nxs','pdg',idproj)
         IDP = IDT_ICIHAD(IDPDG)
      ENDIF
      NPMASS = maproj
      IF(laproj.eq.-1) THEN     !not nucleus
        NPCHAR = IICH(IDP)
      else
        NPCHAR = laproj
      endif
      NTMASS = matarg
      NTCHAR = sign(abs(latarg),idtarg)
      CALL dt_kkinc(NPMASS,NPCHAR,NTMASS,NTCHAR,IDP,EPROJ, KKMAT,IREJ)
      IF(IREJ.NE.0) THEN
      WRITE(ifch,'(a)')
     &'Bad return from DPMJET. Skipping event'
      GOTO 1001
      ENDIF

c      call dt_evtout(1)

c     nevt .......... error code. 1=valid event, 0=invalid event
c     bimevt ........ absolute value of impact parameter
c     phievt ........ angle of impact parameter
c     kolevt ........ number of collisions
c     koievt ........ number of inelastic collisions
c     kohevt ........ number of hard collisions
c     pmxevt ........ reference momentum
c     egyevt ........ pp cm energy (hadron) or string energy (lepton)
c     npjevt ........ number of primary projectile participants
c     ntgevt ........ number of primary target participants
c     npnevt ........ number of primary projectile neutron spectators
c     nppevt ........ number of primary projectile proton spectators
c     ntnevt ........ number of primary target neutron spectators
c     ntpevt ........ number of primary target proton spectators
c     jpnevt ........ number of absolute projectile neutron spectators
c     jppevt ........ number of absolute projectile proton spectators
c     jtnevt ........ number of absolute target neutron spectators
c     jtpevt ........ number of absolute target proton spectators
c     xbjevt ........ bjorken x for dis
c     qsqevt ........ q**2 for dis
c     sigtot ........ total cross section
c     nglevt ........ number of collisions acc to  Glauber
c     zppevt ........ average Z-parton-proj
c     zptevt ........ average Z-parton-targ
c     ng1evt ........ number of Glauber participants with at least one IAs
c     ng2evt ........ number of Glauber participants with at least two IAs
c     ikoevt ........ number of elementary parton-parton scatterings
c     typevt ........ type of event (1=Non Diff, 2=Double Diff, 3=Central Diff, 4=AB->XB, -4=AB->AX)

c     PLEASE FILL!!
      nevt=1
      kolevt=-1
      kohevt=-1 !hepmc HI block checks for .ne. 0
      kohevt=-1
      npjevt=0 !filled later
      ntgevt=0 !filled later
      pmxevt=pnll
      egyevt=ecms
      bimevt=BIMPAC
      bimp=-1.
      phievt=-1.
      phi=-1.

      IF(NHKK.GE.NMXHKK) THEN
      if(ish.ge.2)WRITE(ifch,'(a)')
     &'NMAXHKK too small for event. Skipping'
      GOTO 1001
      ENDIF
      anintine=anintine+1.
c      esum=0.d0

      do 100 k=1,NHKK
         nptlhep(k)=0

c     LIST is the code of final particle, P - its 4-momentum and mass.
         ic=IDHKK(k)           !! copies IDs

         if(ish.ge.7)write(ifch,'(a,i5,a,i5,2a,i6,1x,5(e10.4,1x),4i4)')
     $        ' DPMJET particle ',k,' id :',ic,' before conversion'
     $        , ' momentum :',ISTHKK(k),(sngl(PHKK(i,k)),i=1,5)
     $        ,JMOHKK(1,k),JMOHKK(2,k),JDAHKK(1,k),JDAHKK(2,k)



         IF(ISTHKK(k).GE.1 .AND. ISTHKK(k).LE.2 .AND. ic.NE.99999 )THEN! if final particle
            ist=ISTHKK(k)-1 !!0 means last generation other codes are e.g. for pomerons, remnants...
         elseif(ic.eq.99999)then
            ist=31 !convert to status for epos string
         elseif(ic.eq.66666)then
            ist=11 !convert to status for epos cluster
         else
            ist=abs(ISTHKK(k)) !!0 means last generation other codes are e.g
         endif

         am=PHKK(5,k)
         if(ic.eq.80000)then        !nucleus
           id=1000000000+IDXRES(k)*10000+IDRES(k)*10
           if(ist.eq.1000)then
             ist=0
c             esum=esum+PHKK(4,k)
             if(abs(am-IDXRES(k)*0.938d0).gt.1d0)am=IDRES(k)*0.938d0
           else
             ist=19
           endif
         elseif(ic.eq.66666)then        !cascade
           id=80000000
         else
           id = idtrafostatus('pdg','nxs',ic,istatus)
           if(istatus.ne.0)then
             if(ist.eq.0)then
               print  *,ic,id,istatus
               stop "pdg id from DPMJet unknown !"
             else
               goto 100
             endif
           endif
c           if(ist.eq.0)esum=esum+PHKK(4,k)
         endif
         nptl=nptl+1            !! add 1 particle to stack

         IF(nptl.GE.mxptl) THEN
           if(ish.ge.2)WRITE(ifch,'(a)')
     &          'mxptl too small for event. Skipping'
           GOTO 1001
         ENDIF
         nptlhep(k)=nptl
         if(ish.ge.7)write(ifch,'(a,i5,a,i12,a)')
     $        ' epos particle ',nptl,' id :',id,' after conversion'


         pptl(1,nptl)=sngl(PHKK(1,k)) !P_x
         pptl(2,nptl)=sngl(PHKK(2,k)) !P_y
         pptl(3,nptl)=sngl(PHKK(3,k)) !P_z
c         pptl(4,nptl)=sngl(PHKK(4,k)) !E
         pptl(5,nptl)=sngl(am) !mass
c put particle on-shell in single precision
         pptl(4,nptl)=sqrt(pptl(1,nptl)**2+pptl(2,nptl)**2
     .                    +pptl(3,nptl)**2+pptl(5,nptl)**2)
         ifrptl(1,nptl)=-JDAHKK(1,k)   !updated later
         ifrptl(2,nptl)=-JDAHKK(2,k)   !updated later
         xorptl(1,nptl)=sngl(VHKK(1,k))*1e12
         xorptl(2,nptl)=sngl(VHKK(2,k))*1e12
         xorptl(3,nptl)=sngl(VHKK(3,k))*1e12
         xorptl(4,nptl)=sngl(VHKK(4,k))*1e12
         tivptl(1,nptl)=0.
         tivptl(2,nptl)=0.
         ityptl(nptl)=0
         idptl(nptl)=id

c     treatment of mothers and daughters

         if(ist.ge.11.and.ist.le.18)then
c fix beam particles which are registered at rest in DPMJET
           if(mod(ist,2).eq.0)then  !target remnant (ist=12,14,16,18)
              ytrg=log((sqrt(pnullx**2+pptl(5,nptl)**2)+pnullx)
     .                   /pptl(5,nptl))
              call utlob5(ytrg,pptl(1,nptl)
     .        ,pptl(2,nptl),pptl(3,nptl),pptl(4,nptl),pptl(5,nptl)) !boost in cms frame
              if(ist.eq.12)then  !inelastic nucleon
                istptl(nptl)=41
                iorptl(nptl)=maproj+1
                jorptl(nptl)=maproj+matarg
                idptl(nptl)=sign(abs(idptl(nptl))*100+99,idptl(nptl))
                ntgevt=ntgevt+1
              elseif(ist.eq.14)then  !spectator nucleon
                istptl(nptl)=1
                iorptl(nptl)=maproj+1
                jorptl(nptl)=maproj+matarg
              elseif(ist.eq.18)then  !wounded nucleon
                istptl(nptl)=41
                iorptl(nptl)=maproj+1
                jorptl(nptl)=maproj+matarg
              else  !ist=16 grey final particles
                istptl(nptl)=0
                iorptl(nptl)=0  !defined later
                jorptl(nptl)=0  !defined later
              endif
           else                     !projectile remnant (ist=11,13,15,17)
              yprj=log((sqrt(pnullx**2+pptl(5,nptl)**2)+pnullx)
     .                   /pptl(5,nptl))
              call utlob5(-yprj,pptl(1,nptl)
     .        ,pptl(2,nptl),pptl(3,nptl),pptl(4,nptl),pptl(5,nptl)) !boost in cms frame
              if(ist.eq.11)then  !inelastic nucleon
                istptl(nptl)=41
                iorptl(nptl)=1
                jorptl(nptl)=maproj
                idptl(nptl)=sign(abs(idptl(nptl))*100+99,idptl(nptl))
                npjevt=npjevt+1
              elseif(ist.eq.13)then  !spectator nucleon
                istptl(nptl)=1
                iorptl(nptl)=1
                jorptl(nptl)=maproj
              elseif(ist.eq.17)then  !wounded nucleon
                istptl(nptl)=41
                iorptl(nptl)=1
                jorptl(nptl)=maproj
              else  !ist=15 grey final particles
                istptl(nptl)=0
                iorptl(nptl)=0  !defined later
                jorptl(nptl)=0  !defined later
              endif
           endif
         else
           istptl(nptl)=ist
           iorptl(nptl)=0
           jorptl(nptl)=0
         endif

         if(JMOHKK(1,k).ne.0)then
           if(nptlhep(JMOHKK(1,k)).NE.0) then !mother
             iorptl(nptl)=nptlhep(JMOHKK(1,k))
           endif
         endif
         if(JMOHKK(2,k).ne.0)then
           if(nptlhep(JMOHKK(2,k)).NE.0) then !father
             jorptl(nptl)=nptlhep(JMOHKK(2,k))
           endif
         endif
         if(jorptl(nptl).gt.0.and.iorptl(nptl).gt.jorptl(nptl))then   !reorder mother/father to avoid problem with hepmc
           io=iorptl(nptl)
           iorptl(nptl)=jorptl(nptl)
           jorptl(nptl)=io
           if(io.ne.0)istptl(io)=istptl(io)+2
         endif

C         print*,istmax,k,JMOHKK(1,k),nptlhep(k),iorptl(nptl),
C     $        nptlhep(JMOHKK(1,k))

         if(ish.ge.5)write(ifch,'(a,i5,a,i12,a,i5,a,4(e10.4,1x),f6.3)')
     $        ' particle from DPMJET ',nptl,' id :',idptl(nptl)
     $        ,' st :',istptl(nptl)
     $        , ' momentum :',(pptl(i,nptl),i=1,5)


 100     continue     !! end of loop over particles for copying to epos block
c      write(ifch,*)'esum',esum,0.5*engy*(maproj+matarg)

c update daughter index
      do i=1,nptl
        if(ifrptl(1,i).lt.0)then
          ifrptl(1,i)=nptlhep(-ifrptl(1,i))
        endif
        if(ifrptl(2,i).lt.0)then
          ifrptl(2,i)=nptlhep(-ifrptl(2,i))
        endif
      enddo

      iret=0
1000  return

1001  iret=-1
      goto 1000

      END




c------------------------------------------------------------------------------
      subroutine GetDPMJETSigma(xstot,xsine,xsela)
c------------------------------------------------------------------------------
c return hp cross sections of DPMJET
c but also fill other cross section variables
c some dpmjet functions must be called for this

c in dpmjet the control card xs-table starts to calculate the table and then quits
c it calls subroutine dt_xstabl with the 6 parameters for binning and energy and binning and q
c in this subroutine the actual table COMMON/DTPART/ is filled
c this happens with dt_xsglau. emulsion is off? at least ncompo is 0 => no loops over table
c instead always (1,1,1) contains the value
c xprod seems to be prod + quasi-ela. this is why we subtract it again

      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      include 'epos.inc'
      real xstot,xsine,xsela !subroutine return values

c     fresh common block
      INTEGER NPMASS,NPCHAR,NTMASS,NTCHAR,IDPDG
      DOUBLE PRECISION EPROJ
      COMMON /DPMEVTINI/ EPROJ,NPMASS,NPCHAR,NTMASS,NTCHAR,IDPDG

c      PARAMETER (TINY10=1.0D-10,TINY2=1.0D-2,ZERO=0.0D0,DLARGE=1.0D10,
c     &           OHALF=0.5D0,ONE=1.0D0,TWO=2.0D0)

* properties of interacting particles
      COMMON /DTPRTA/ IT,ITZ,IP,IPZ,IJPROJ,IBPROJ,IJTARG,IBTARG


* Glauber formalism: flags and parameters for statistics
c      LOGICAL LPROD
c      CHARACTER*8 CGLB
c      COMMON /DTGLGP/ JSTATB,JBINSB,CGLB,IOGLB,LPROD
c      JSTATB = 1 !loops for glauber calculation !limited by ksiteb = 50

c      LPROD = .FALSE. !if set to false it enables the calculation of tot, el... otherwise only prod

c #######from dt_xstabl##########
c      CALL DT_XSGLAU(1,1,sign(1,idproj),ZERO,ZERO,dble(ECMS),1,1,-1)
c #######from dt_xstabl##########
      PL=0d0
      ECM=dble(engy)
      IPi=max(1,IDT_ICIHAD(IDPDG))
      ITi=1
      call DT_XSHN(IPi,ITi,PL,ECM,STOT,SELA)


      xstot = sngl(STOT)
      xsine = sngl(STOT-SELA)
      xsela = sngl(SELA)

      end

c------------------------------------------------------------------------------
      subroutine GetDPMJETSigmaAA(niter,stotaa,sineaa,scutaa,selaaa)
c------------------------------------------------------------------------------
c return AA cross sections of DPMJET
c but also fill other cross section variables
c some dpmjet functions must be called for this

c in dpmjet the control card xs-table starts to calculate the table and then quits
c it calls subroutine dt_xstabl with the 6 parameters for binning and energy and binning and q
c in this subroutine the actual table COMMON/DTPART/ is filled
c this happens with dt_xsglau. emulsion is off? at least ncompo is 0 => no loops over table
c instead always (1,1,1) contains the value
c xprod seems to be prod + quasi-ela. this is why we subtract it again

      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      include 'epos.inc'
      integer niter !input
      real stotaa,sineaa,scutaa,selaaa !subroutine return values

      PARAMETER (TINY10=1.0D-10,TINY2=1.0D-2,ZERO=0.0D0,DLARGE=1.0D10,
     &           OHALF=0.5D0,ONE=1.0D0,TWO=2.0D0)

* Glauber formalism: cross sections
      PARAMETER (NCOMPX=100,NEB=8,NQB= 5,KSITEB=50)
      COMMON /DTGLXS/ ECMNN(NEB),Q2G(NQB),ECMNOW,Q2,
     &                XSTOT(NEB,NQB,NCOMPX),XSELA(NEB,NQB,NCOMPX),
     &                XSQEP(NEB,NQB,NCOMPX),XSQET(NEB,NQB,NCOMPX),
     &                XSQE2(NEB,NQB,NCOMPX),XSPRO(NEB,NQB,NCOMPX),
     &                XSDEL(NEB,NQB,NCOMPX),XSDQE(NEB,NQB,NCOMPX),
     &                XETOT(NEB,NQB,NCOMPX),XEELA(NEB,NQB,NCOMPX),
     &                XEQEP(NEB,NQB,NCOMPX),XEQET(NEB,NQB,NCOMPX),
     &                XEQE2(NEB,NQB,NCOMPX),XEPRO(NEB,NQB,NCOMPX),
     &                XEDEL(NEB,NQB,NCOMPX),XEDQE(NEB,NQB,NCOMPX),
     &                BSLOPE,NEBINI,NQBINI

* Glauber formalism: flags and parameters for statistics
      LOGICAL LPROD
      CHARACTER*8 CGLB
      COMMON /DTGLGP/ JSTATB,JBINSB,CGLB,IOGLB,LPROD
c  emulsion treatment
      COMMON /DTCOMP/  EMUFRA(NCOMPX),IEMUMA(NCOMPX),IEMUCH(NCOMPX),
     &                 NCOMPO,IEMUL


      idum=niter
c      IF(NCOMPO.EQ.0)JSTATB = niter !loops for glauber calculation !limited by ksiteb = 50

      LPROD = .FALSE. !if set to false it enables the calculation of tot, el... otherwise only prod

c #######from dt_xstabl##########
c         CALL DT_GLBSET(IJPROJ,maproj,matarg,dble(elab),2)
c      call DT_SIGEMU           !to print cross-section in dpmjet

      IF(laproj.eq.-1) THEN     !not nucleus
         IDPDG = idtrafo('nxs','pdg',idproj)
         IDP=IDT_ICIHAD(IDPDG)
      ELSE
         IDP = 1
      ENDIF
c        EMUFRA(1) = airwnxs(1)
      
c      IF (NCOMPO.GT.0) THEN
c
c        IE = 1
c        IQ = 1
c        XTOT = 0D0
c        XELA = 0D0
c        XINE = 0D0
c        SIGQEP = 0D0
c        SIGQET = 0D0
c        SIGQE2 = 0D0
c        SIGDEL = 0D0
c        SIGDQE = 0D0
c        IC = 1
c        DO 3 ICI=1,NCOMPO
c          CALL DT_GLBSET(IDP,maproj,IEMUMA(ICI),dble(elab),1)
c          XTOT = XTOT+EMUFRA(ICI)*XSTOT(IE,IQ,IC)
c          XELA = XELA+EMUFRA(ICI)*XSELA(IE,IQ,IC)
c          XINE = XINE+EMUFRA(ICI)*XSPRO(IE,IQ,IC)
c          SIGQEP = SIGQEP+EMUFRA(ICI)*XSQEP(IE,IQ,IC)
c          SIGQET = SIGQET+EMUFRA(ICI)*XSQET(IE,IQ,IC)
c          SIGQE2 = SIGQE2+EMUFRA(ICI)*XSQE2(IE,IQ,IC)
cc          SIGDEL = SIGDEL+EMUFRA(IC)*XSDEL(IE,IQ,IC)
cc          SIGDQE = SIGDQE+EMUFRA(IC)*XSDQE(IE,IQ,IC)
c
c 3      CONTINUE
      IF (idtargin.eq.0) THEN

        IE = 1
        IQ = 1
        XTOT = 0D0
        XELA = 0D0
        XINE = 0D0
        SIGQEP = 0D0
        SIGQET = 0D0
        SIGQE2 = 0D0
        SIGDEL = 0D0
        SIGDQE = 0D0
        IC = 1
        DO 3 ICI=1,3
          CALL DT_GLBSET(IDP,maproj,nint(airanxs(ici)),dble(elab),1)
c          CALL DT_XSGLAU(maproj,nint(airanxs(ici)),IDP,ZERO,ZERO,
c     .                   dble(ECMS),1,1,1)
          XTOT = XTOT+airwnxs(ici)*XSTOT(IE,IQ,IC)
          XELA = XELA+airwnxs(ici)*XSELA(IE,IQ,IC)
          XINE = XINE+airwnxs(ici)*XSPRO(IE,IQ,IC)
          SIGQEP = SIGQEP+airwnxs(ici)*XSQEP(IE,IQ,IC)
          SIGQET = SIGQET+airwnxs(ici)*XSQET(IE,IQ,IC)
          SIGQE2 = SIGQE2+airwnxs(ici)*XSQE2(IE,IQ,IC)
c          SIGDEL = SIGDEL+airwnxs(ici)*XSDEL(IE,IQ,IC)
c          SIGDQE = SIGDQE+airwnxs(ici)*XSDQE(IE,IQ,IC)

 3      CONTINUE

      else

        CALL DT_XSGLAU(maproj,matarg,IDP,ZERO,ZERO,dble(ECMS),1,1,1)
        XTOT = XSTOT(1,1,1)
        XELA = XSELA(1,1,1)
        XINE = XSPRO(1,1,1)
        SIGQEP = XSQEP(1,1,1)
        SIGQET = XSQET(1,1,1)
        SIGQE2 = XSQE2(1,1,1)

      endif

      XPRO = XTOT - XELA - SIGQEP - SIGQET - SIGQE2

      stotaa = sngl(XTOT)
      scutaa = sngl(XPRO)
      sineaa = sngl(XINE)
      selaaa = sngl(XELA)

      end


c--------------------------------------------------------------------
      double precision function DT_RNDM(dum)
c--------------------------------------------------------------------
c random number generator
c--------------------------------------------------------------------
      include 'epos.inc'
      double precision dum,drangen

      DT_RNDM=drangen(dum)
      if(irandm.eq.1)write(ifch,*)'DT_RNDM()= ',DT_RNDM,dum

      return
      end
