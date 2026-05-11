c 13.04.2010 Link routines between DPMJET and CRMC.
c authors C. Baus, A. Feydnitch
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
      LOGICAL LEMCCK,LHADRO,LSEADI,LEVAPO
      COMMON /DTFLG1/ IFRAG(2),IRESCO,IMSHL,IRESRJ,IOULEV(6),
     &                LEMCCK,LHADRO(0:9),LSEADI,LEVAPO,IDFRAME,ITRSPT


      IDFRAME = 2               !dpmjet iframe variable. nucleon-nucleon frame
      NDPMEVENT = 0             !event number needed in dpmjet

      egymin = 10.              !min energy for model
      egymax = 5e4              !max energy for model

      irescl = 0                !don't rescale/skip events with wrong energy

      END

c-----------------------------------------------------------------------
      subroutine IniEvtDpm
c-----------------------------------------------------------------------
c Setting energy, primaries,... for each event class. Useful for e.g. conex
c-----------------------------------------------------------------------
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      include 'epos.inc'

*     particle properties (BAMJET index convention)
      CHARACTER*8  ANAME
      COMMON /DTPART/ ANAME(210),AAM(210),GA(210),TAU(210),
     &     IICH(210),IIBAR(210),K1(210),K2(210)

c     fresh common block
      INTEGER NPMASS,NPCHAR,NTMASS,NTCHAR,IDPDG
      DOUBLE PRECISION EPROJ
      COMMON /DPMEVTINI/ EPROJ,NPMASS,NPCHAR,NTMASS,NTCHAR,IDPDG

      COMMON/PYDAT2/KCHG(500,4),PMAS(500,4),PARF(2000),VCKM(4,4)
      COMMON/PYDAT3/MDCY(500,3),MDME(4000,2),BRAT(4000),KFDP(4000,5)
      COMMON/PYPARS/MSTP(200),PARP(200),MSTI(200),PARI(200)
* properties of interacting particles
      COMMON /DTPRTA/ IT,ITZ,IP,IPZ,IJPROJ,IBPROJ,IJTARG,IBTARG

c      dimension WHAT(6)

      integer istatus
      real taugm

C     general initialization
      NCASES = -1               !skip reading steering cards
      EPROJ  = dble(elab)
      NPMASS = maproj
      NPCHAR = laproj
      NTMASS = matarg
      NTCHAR = latarg

      IF(laproj.eq.-1) THEN     !not nucleus
         IDPDG = idtrafo('nxs','pdg',idproj)
         IDP=IDT_ICIHAD(IDPDG)
         NPCHAR = IICH(IDP)
      elseif(maproj.gt.20)then
        call utstop("Only nucleus with A<20 as projectile !&")
      ELSE
c     will be treated as nucleus in DT_INIT and NPMASS,... will be used
         IDPDG = 0
         IDP = 0
      ENDIF
      if(abs(idtarg).ne.1120.and.idtarg.ne.0)then
        call utstop("Only (anti)proton or nucleus as target !&")
      elseif(matarg.gt.20)then
        call utstop("Only nucleus with A<20 as target !&")
      else
        NTCHAR = sign(abs(latarg),idtarg)
      endif

      IGLAU = 0
      CALL DT_INIT(NCASES,EPROJ,NPMASS,NPCHAR,NTMASS,NTCHAR,IDPDG
     +,IGLAU)


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
            go to 100
         endif

c     because epos var ifrade is not 0 it will automatically use epos to decay when switched off in pythia
         if(ish.ge.2) write(ifch,*)
     +        "CF Id:",i,PMAS(i,1),PMAS(i,2),
     +        "PDG ID:",KCHG(I,4),
     +        "(decay in epos)"
         MDCY(i,1)=0
 100  enddo !loop over 100-500 particle ids

c     initialize cross-sections by calling epos-bas.f function -> models.F -> dpmjet-crmc.f
      call xsigma

c      istmax=0 !uncomment only if you want to be sure to have only final particles

      END
c-----------------------------------------------------------------------
      subroutine emsdpmjet(iret)
c-----------------------------------------------------------------------
c  call DPMJET to simulate interaction
c-----------------------------------------------------------------------
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      include 'epos.inc'
      real yprj,ytrg
*     event history
      PARAMETER (NMXHKK=200000)
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

      INTEGER NPMASS,NPCHAR,NTMASS,NTCHAR,IDPDG
      INTEGER IREJ, KKMAT
      DOUBLE PRECISION EPROJ
      COMMON /DPMEVTINI/ EPROJ,NPMASS,NPCHAR,NTMASS,NTCHAR,IDPDG
      integer nptlhep(NMXHKK)

      anintine=anintine+1.

c      call conre !! adds beam particles that are known from intial call of crmc
c      call conwr

      IREJ = 0
      KKMAT = -1


C     call dpmjet event simulation
      NDPMEVENT = NDPMEVENT+1   !needs to be updated for internal init. sets NEVHKK
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

      do 100 k=1,NHKK
         nptlhep(k)=0

c     LIST is the code of final particle, P - its 4-momentum and mass.
         ic=IDHKK(k)           !! copies IDs

         if(ish.ge.7)write(ifch,'(a,i5,a,i5,2a,i3,1x,5(e10.4,1x),4i4)')
     $        ' DPMJET particle ',k,' id :',ic,' before conversion'
     $        , ' momentum :',ISTHKK(k),(sngl(PHKK(i,k)),i=1,5)
     $        ,JMOHKK(1,k),JMOHKK(2,k),JDAHKK(1,k),JDAHKK(2,k)


         IF(ISTHKK(k).GE.1 .AND. ISTHKK(k).LE.2 .AND. ic.NE.99999 )THEN !! if final particle
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


 100     continue    !! end of loop over particles for copying to epos block

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



*===usrhis=============================================================*
*
      SUBROUTINE DT_USRHIS(MODE)

      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      SAVE
*
* COMMON /DTEVT1/ :
*                   NHKK         number of entries in common block
*                   NEVHKK       number of the event
*                   ISTHKK(i)    status code for entry i
*                   IDHKK(i)     identifier for the entry
*                                (for particles: identifier according
*                                 to the PDG numbering scheme)
*                   JMOHKK(1,i)  pointer to the entry of the first mother
*                                of entry i
*                   JMOHKK(2,i)  pointer to the entry of the second mother
*                                of entry i
*                   JDAHKK(1,i)  pointer to the entry of the first daughter
*                                of entry i
*                   JDAHKK(2,i)  pointer to the entry of the second daughter
*                                of entry i
*                   PHKK(1..3,i) 3-momentum
*                   PHKK(4,i)    energy
*                   PHKK(5,i)    mass
*
* event history
      PARAMETER (NMXHKK=200000)
      COMMON /DTEVT1/ NHKK,NEVHKK,ISTHKK(NMXHKK),IDHKK(NMXHKK),
     &                JMOHKK(2,NMXHKK),JDAHKK(2,NMXHKK),
     &                PHKK(5,NMXHKK),VHKK(4,NMXHKK),WHKK(4,NMXHKK)
* extended event history
      COMMON /DTEVT2/ IDRES(NMXHKK),IDXRES(NMXHKK),NOBAM(NMXHKK),
     &                IDBAM(NMXHKK),IDCH(NMXHKK),NPOINT(10),
     &                IHIST(2,NMXHKK)

      GOTO (1,2,3) MODE

*------------------------------------------------------------------
*
    1 CONTINUE
*
* initializations
*
*  Called with MODE=1 once at the beginning of the run.
*
      RETURN
*
*------------------------------------------------------------------
*
    2 CONTINUE
*
* scoring of the present event
*
*  Called with MODE=2 every time one event has been finished.
*
*  The final state particles from the actual event (number NEVHKK)
*  can be found in DTEVT1 and identified by their status:
*
*     ISTHKK(i) = 1    final state particle produced in
*                      photon-/hadron-/nucleon-nucleon collisions or
*                      in intranuclear cascade processes
*                -1    nucleons, deuterons, H-3, He-3, He-4 evaporated
*                      from excited nucleus and
*                      photons produced in nuclear deexcitation processes
*                1001  residual nucleus (ground state)
*
*  The types of these particles/nuclei are given in IDHKK as follows
*
*     all final state part. except nuclei :
*       IDHKK(i)=particle identifier according to PDG numbering scheme
*     nuclei (evaporation products, and residual nucleus) :
*       IDHKK(i)=80000, IDRES(i)=mass number, IDXRES(i)=charge number
*
*  The 4-momenta and masses can be found in PHKK (target nucleus rest frame):
*                   PHKK(1..3,i) 3-momentum (p_x,p_y,p_z)
*                   PHKK(4,i)    energy
*                   PHKK(5,i)    mass
*
*
*
*  Pick out the final state particles from DTEVT1 in each event for
*  instance by the following loop (NHKK=number of entries in the present
*  event) and fill your histograms
C     DO 20 I=1,NHKK
C        IF (ABS(ISTHKK(I)).EQ.1) THEN
C        ELSEIF (ABS(ISTHKK(I)).EQ.1001) THEN
C        ENDIF
C  20 CONTINUE

*  At any time during the run a list of the actual entries in DTEVT1 and
*  DTEVT2 can be obtained (output unit 6) by the following statement:
C     CALL DT_EVTOUT(4)

      RETURN
*
*------------------------------------------------------------------
*
    3 CONTINUE
*
* output/statistics/histograms etc.
*
*  Called with MODE=3 once after all events have been sampled.
*
      RETURN

      END
c------------------------------------------------------------------------------
      subroutine GetDPMJETSigma(stot,sine,sela)
c------------------------------------------------------------------------------
c dummy until returned from phojet
      stot   = 0.
      sela   = 0.
      sine   = 0.

      end

c------------------------------------------------------------------------------
      subroutine GetDPMJETSigmaAA(niter,stotaa,sineaa,selaaa)
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
      real stotaa,sineaa,selaaa !subroutine return values

      PARAMETER (TINY10=1.0D-10,TINY2=1.0D-2,ZERO=0.0D0,DLARGE=1.0D10,
     &           OHALF=0.5D0,ONE=1.0D0,TWO=2.0D0)

* properties of interacting particles
      COMMON /DTPRTA/ IT,ITZ,IP,IPZ,IJPROJ,IBPROJ,IJTARG,IBTARG

* Glauber formalism: cross sections
      PARAMETER (NCOMPX=20,NEB=8,NQB= 5,KSITEB=50)
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
      JSTATB = niter !loops for glauber calculation !limited by ksiteb = 50

      LPROD = .FALSE. !if set to false it enables the calculation of tot, el... otherwise only prod

c #######from dt_xstabl##########
      CALL DT_XSGLAU(IP,IT,IJPROJ,ZERO,ZERO,dble(ECMS),1,1,-1)
c #######from dt_xstabl##########

      XTOT = XSTOT(1,1,1)
      XELA = XSELA(1,1,1)
      XINE = XSPRO(1,1,1)
      XPRO = XTOT - XELA - XSQEP(1,1,1) - XSQET(1,1,1) - XSQE2(1,1,1)

      stotaa = sngl(XTOT)
      sineaa = sngl(XPRO)
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
