c 12.03.2003 Link routines between Gheisha and epos.
c author T. Pierog

c-----------------------------------------------------------------------
      subroutine IniGheisha
c-----------------------------------------------------------------------
c Primary initialization for Gheisha
c-----------------------------------------------------------------------
      include 'epos.inc'
      COMMON /CRELABCT/ELCUT
      DOUBLE PRECISION ELCUT(4)

      call utpri('inighe',ish,ishini,3)
      write(ifmt,'(a,i6)')'initialize Gheisha ...'

c common model parameters setting
      call nghini
      egymin=0.1
      elcut(1)=1.
      egymax=egymax
      irescl=0

      call utprix('inighe',ish,ishini,3)
      end

c-----------------------------------------------------------------------
      subroutine IniEvtGhe
c-----------------------------------------------------------------------
c Initialization for each type of event (for given proj, targ and egy)
c-----------------------------------------------------------------------
      include 'epos.inc'
      common/geom/rmproj,rmtarg,bmax,bkmx
c GHEISHA Common
      common/ghecsquel/anquasiel,iquasiel

        anquasiel=0.
        if(maproj.gt.1)
     &  call utstop('Nucleus too big for Gheisha (Mmax=1) !&')
        if(bminim.gt.0.or.bmaxim.lt.1000)
     &  write(ifmt,*)'Only min bias event in Gheisha ... no b !'
        bminim=0.
        bmaxim=10000.
        icp=idtrafo('nxs','ghe',idproj)
        ict=idtrafo('nxs','ghe',idtargin)
        gheincs=GheSig(pnll,ekin,icp,ict,matarg,latarg)
        if(engy.lt.egymin)gheincs=0.           !below egymin, no interaction
        bmax=20.
        if(ish.ge.2)write(ifch,*)
     &  'Gheisha used with (E,pz,proj,targ(A,Z))',elab,pnll,icp,ict
     &  ,'(',matarg,',',latarg,')'
        call xsigma

      return
      end

c------------------------------------------------------------------------------
      subroutine ghecrse(ek,idpro,idtar,latar,matar,sigi,sige)
c------------------------------------------------------------------------------
c inelastic and elastic cross section from gheisha
c ek - kinetic lab energy
c idpro - id of projectile
c idtar - id of target(idtar=0 corresponds to air)
c latar - charge of target
c matar - mass of target
c output :
c   sigi - inelastic cross section
c   sige - elastic cross section
c------------------------------------------------------------------------------
      include 'epos.inc'
c Gheisha Common
      COMMON/GSECTI/ AIEL(20),AIIN(20),AIFI(20),AICA(20),ALAM,K0FLAG
      DOUBLE PRECISION AIEL,AIIN,AIFI,AICA,ALAM
      INTEGER K0FLAG
      PARAMETER (KKK=3)
      COMMON /CGCOMP/ ACOMP,ZCOMP,WCOMP,KK
      REAL           ACOMP(KKK),ZCOMP(KKK),WCOMP(KKK)

      call idmass(idpro,amp)
      p=sqrt(max(0.,(ek+amp)**2-amp**2))
      icp=idtrafo('nxs','ghe',idpro)
      ict=idtrafo('nxs','ghe',idtar)
      sigi=GheSig(p,ek,icp,ict,matar,latar)
      sige=0.
      do k=1,kk
        sige=sige+AIEL(k)
      enddo

      return
      end

c-----------------------------------------------------------------------
      subroutine emsghe(iret)
c-----------------------------------------------------------------------
c  call gheisha to simulate interaction
C  itypr  = -1 REACTION CROSS SECTIONS NOT YET TABULATED/PROGRAMMED
C         =  0 NO INTERACTION
C         =  1 ELASTIC SCATTERING
C         =  2 INELASTIC SCATTERING
C         =  3 NUCLEAR FISSION WITH INELEASTIC SCATTERING
C         =  4 NEUTRON CAPTURE
c-----------------------------------------------------------------------
      include 'epos.inc'
      common/nucl3/phi,bimp
      common/col3/ncol,kolpt
      common/geom/rmproj,rmtarg,bmax,bkmx
      common/ghecsquel/anquasiel,iquasiel


      iret=0
      itypr=0
      b1=bminim
      b2=min(bmax,bmaxim)
      a=pi*(b2**2-b1**2)
      if(idtargin.eq.0.and.gheincs.gt.0.)then
        icp=idtrafo('nxs','ghe',idproj)
        ict=idtrafo('nxs','ghe',idtarg)
        gheincs=GheSig(pnll,ekin,icp,ict,matarg,latarg)
      endif

      if(a.gt.0..and.rangen().gt.gheincs/10./a)goto 1001   !no interaction

      if(ish.ge.3)call alist('Determine Gheisha Production&',0,0)
      ntry=0
 10   continue
      ntry=ntry+1

      nptl=0
      ncol=1
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

      call conre
      call conwr

      call NGHEI(itypr)

      if(itypr.le.1)go to 10

      mlim=0
      nmes=0
      if(iabs(idproj).le.1000)mlim=1
      do i=1,nptl
        if(istptl(i).eq.0.and.iabs(idptl(i)).lt.1000)nmes=nmes+1
      enddo
      if(nmes.eq.mlim)itypr=5

      if(itypr.eq.5)then
        if(ntry.eq.1)anquasiel=anquasiel+1.
        if(ish.ge.2)write(ifch,*)'Quasi-elastic event'
        if(iquasiel.eq.0)then
          if(ntry.le.100)then
            goto 10
          else
            nptl=0
          endif
        endif
      endif

      if(itypr.eq.-1)call utstop('Problem in Gheisha&')

1000  return

1001  iret=1 
      goto 1000 

      end

c--------------------------------------------------------------------------
      subroutine nex2ghe(idghe,pnex)
c--------------------------------------------------------------------------
      include 'epos.inc'
      real pnex(5)
      idnex=idptl(1)
      idghe=idtrafo('nxs','ghe',idnex)
      do ii=1,2
        pnex(ii)=0.
      enddo
      pnex(3)=pnll
      pnex(4)=elab
      pnex(5)=amproj
      if(ish.ge.3)write(ifch,'(a,i5,a,5(e10.4,1x))')
     $       ' Initial Gheisha particle, id :',idghe
     $     , ' momentum :',(pnex(k),k=1,5)
      end

c--------------------------------------------------------------------------
      subroutine ghe2nex(idghe,pnex)
c--------------------------------------------------------------------------
c  Put gheisha particle with momentum pnex into epos stack
c  imod=1 put new particle
c--------------------------------------------------------------------------
      include 'epos.inc'
      real pnex(5)

      idnex=idtrafo('ghe','nxs',idghe)
      nptl=nptl+1
      if(nptl.gt.mxptl)call utstop('gheisha: mxptl too small&')
      istptl(nptl)=0
      ifrptl(1,nptl)=0
      ifrptl(2,nptl)=0
      idptl(nptl)=idnex
      do ii=1,5
        pptl(ii,nptl)=pnex(ii)
      enddo
      ityptl(nptl)=0
      iorptl(nptl)=1
      jorptl(nptl)=maproj+matarg
      xorptl(1,nptl)=0.
      xorptl(2,nptl)=0.
      xorptl(3,nptl)=0.
      xorptl(4,nptl)=0.
      tivptl(1,nptl)=0.
      tivptl(2,nptl)=0.

c Put particle in cms frame.
      call utlob5(yhaha, pptl(1,nptl), pptl(2,nptl)
     *, pptl(3,nptl), pptl(4,nptl), pptl(5,nptl))

      if(ish.ge.5)write(ifch,'(a,i5,a,i5,a,4(e10.4,1x),f6.3)')
     $       ' particle from Gheisha ',nptl,' id :',idptl(nptl)
     $  , ' momentum :',(pptl(k,nptl),k=1,5)


      end

c-----------------------------------------------------------------------
*CMZ :          24/04/2003  by  T. PIEROG IK FZK KARLSRUHE
*-- Author :    The CONEX development group   24/04/2003
C=======================================================================

      SUBROUTINE NGHEI(INTE)

C-----------------------------------------------------------------------
C  N(EXUS) GHE(ISHA) I(NTERFACE)
C
C  MAIN STEERING SUBROUT. FOR HADRON PACKAGE GHEISHA ***
C  THIS SUBROUTINE IS CALLED FROM EMSGHE.
C
C  RETURN INTE (SEE DEFINITION BELOW)
C
C  ORIGIN  : F.CARMINATI, H.FESEFELDT (SUBROUT. GHESIG)
C  REDESIGN FOR CORSIKA : P. GABRIEL IK1  FZK KARLSRUHE
C  REDESIGN FOR epos : T. PIEROG IK3  FZK KARLSRUHE
C-----------------------------------------------------------------------

      IMPLICIT DOUBLE PRECISION(A-H, O-Z)
      real pnex(5),atnxs,ztnxs,airznxs,airanxs,airwnxs
     &             ,airavznxs,airavanxs
      common/ghe_nxs_targ/atnxs,ztnxs
      common/nxsair/airznxs(3),airanxs(3),airwnxs(3)
     &             ,airavznxs,airavanxs
*KEEP,CGCOMP.
      PARAMETER (KKK=3)
      COMMON /CGCOMP/ ACOMP,ZCOMP,WCOMP,KK
      REAL           ACOMP(KKK),ZCOMP(KKK),WCOMP(KKK)
*KEEP,ELABCT.
      COMMON /CRELABCT/ELCUT
      DOUBLE PRECISION ELCUT(4)
      COMMON /CRRUNPAR/DEBUG,MDEBUG
      LOGICAL          DEBUG
      INTEGER          MDEBUG

      COMMON/GSECTI/   AIEL(20),AIIN(20),AIFI(20),AICA(20),ALAM,K0FLAG
      DOUBLE PRECISION AIEL,AIIN,AIFI,AICA,ALAM
      INTEGER          K0FLAG

C --- GHEISHA COMMONS ---
      PARAMETER (MXGKGH=100)
      PARAMETER (MXGKPV=MXGKGH)
      COMMON /VECUTY/ PV(10,MXGKPV)

      COMMON /CONSTS/PI,TWPI,PIBTW,MP,MPI,MMU,MEL,MKCH,MK0,SMP,SMPI,
     $               SMU,CT,CTKCH,CTK0,
     $               ML0,MSP,MS0,MSM,MX0,MXM,CTL0,CTSP,CTSM,CTX0,CTXM,
     $               RMASS(35),RCHARG(35)

      DOUBLE PRECISION MP,MPI,MMU,MEL,MKCH,MK0,
     *                 ML0,MSP,MS0,MSM,MX0,MXM

      PARAMETER (MXEVEN=12*MXGKGH)
C --- COMMON "EVENT" CHANGED TO "GEVENT"  DUE TO CLASH ---
C --- WITH  "EVENT" IN epos COMMON ---
      COMMON /GEVENT / NSIZE,NCUR,NEXT,NTOT,EVE(MXEVEN)

      COMMON /PRNTFL/INBCD,NEWBCD,INBIN,NEWBIN,NPEVT,NEVTP,LPRT
     *,NPRT(10)
                    LOGICAL LPRT,NPRT


C --- "NEVENT" CHANGED TO "KEVENT" IN COMMON /GHECURPAR/ DUE TO CLASH ---
C --- WITH VARIABLE "NEVENT" IN GEANT COMMON ---

      PARAMETER (MXGKCU=MXGKGH)
      COMMON /CURPAR/WEIGHT(10),DDELTN,IFILE,IRUN,NEVT,KEVENT,SHFLAG,
     $                ITHST,ITTOT,ITLST,IFRND,TOFCUT,CMOM(5),CENG(5),
     $                RS,S,ENP(10),NP,NM,NN,NR,NO,NZ,IPA(MXGKCU),
     $                ATNO2,ZNO2

C --- "IPART" CHANGED TO "KPART" IN COMMON /GHERESULT/ DUE TO CLASH ---
C --- WITH VARIABLE "IPART" IN GEANT COMMON ---

      COMMON /RESULT/  XEND,YEND,ZEND,RCA,RCE,AMAS,NCH,TOF,PX,PY,PZ,
     $                 USERW,INTCT,P,EN,EK,AMASQ,DELTN,ITK,NTK,KPART,
     $                 IND,LCALO,ICEL,SINL,COSL,SINP,COSP,
     $                 XOLD,YOLD,ZOLD,POLD,PXOLD,PYOLD,PZOLD,
     $                 XSCAT,YSCAT,ZSCAT,PSCAT,PXSCAT,PYSCAT,PZSCAT
      DOUBLE PRECISION NCH,INTCT

C --- "ABSL(21)" CHANGED TO "ABSLTH(21)" IN COMMON /GHEMAT/ DUE TO CLASH ---
C --- WITH VARIABLE "ABSL" IN GEANT COMMON ---

      COMMON /MAT/  DEN(21),RADLTH(21),ATNO(21),ZNO(21),ABSLTH(21),
     *                 CDEN(21),X0DEN(21),X1DEN(21),RION(21),
     *                 FRAC1(21,10),DEN1(21,10),ATNO1(21,10),
     *                 ZNO1(21,10),
     *                 PARMAT(21,10),MATID(21),MATID1(21,24),MDEN(21),
     *                 IFRAT,IFRAC(21),LMAT

*     DIMENSION IPELOS(35)
c$$$      REAL EMAX,EEESQ

      DIMENSION RNDM(1)

c$$$      DIMENSION KIPART(48),IKPART(35)
c$$$C --- ANGLES FOR NEW COUPLING WITH CORSIKA D. HECK DEC. 2000
c$$$      DOUBLE PRECISION PHIRAN,PHIG,THETG

      SAVE

C --- DATA STMTS. FOR GEANT/GHEISHA PARTICLE CODE CONVERSIONS ---
C --- KIPART(I)=GHEISHA CODE CORRESPONDING TO GEANT   CODE I ---
C --- IKPART(I)=GEANT   CODE CORRESPONDING TO GHEISHA CODE I ---

c$$$      DATA KIPART/
c$$$     $               1,   3,   4,   2,   5,   6,   8,   7,
c$$$     $               9,  12,  10,  13,  16,  14,  15,  11,
c$$$     $              35,  18,  20,  21,  22,  26,  27,  33,
c$$$     $              17,  19,  23,  24,  25,  28,  29,  34,
c$$$     $              35,  35,  35,  35,  35,  35,  35,  35,
c$$$     $              35,  35,  35,  35,  30,  31,  32,  35/
c$$$
c$$$      DATA IKPART/
c$$$     $               1,   4,   2,   3,   5,   6,   8,   7,
c$$$     $               9,  11,  16,  10,  12,  14,  15,  13,
c$$$     $              25,  18,  26,  19,  20,  21,  27,  28,
c$$$     $              29,  22,  23,  30,  31,  45,  46,  47,
c$$$     $              24,  32,  48/


C --- DENOTE STABLE PARTICLES ACCORDING TO GHEISHA CODE ---
C --- STABLE : GAMMA, NEUTRINO, ELECTRON, PROTON AND HEAVY FRAGMENTS ---
C --- WHEN STOPPING THESE PARTICLES ONLY LOOSE THEIR KINETIC ENERGY ---
*     DATA IPELOS/
*    $             1,   1,   0,   1,   0,   0,   0,   0,
*    $             0,   0,   0,   0,   0,   1,   0,   0,
*    $             0,   0,   0,   0,   0,   0,   0,   0,
*    $             0,   0,   0,   0,   0,   1,   1,   1,
*    $             0,   0,   1/

C --- LOWERBOUND OF KINETIC ENERGY BIN IN N CROSS-SECTION TABLES ---
      DATA TEKLOW /0.0001D0/

C --- KINETIC ENERGY TO SWITCH FROM "CASN" TO "GNSLWD" FOR N CASCADE ---
      DATA SWTEKN /0.05D0/
C-----------------------------------------------------------------------
c$$$      IF ( DEBUG ) WRITE(MDEBUG,445) (CURPAR(I),I=1,9)
c$$$ 445  FORMAT(' NGHEI : CURPAR=',1P,9E10.3)
c$$$
c$$$      IF ( DEBUG ) WRITE(MDEBUG,*)
c$$$     *                    'NGHEI : E = ',CURPAR(2)*PAMA(NINT(CURPAR(1)))
c$$$
c$$$C --- DEFINE PARTICLE TYPE
c$$$      IF     ( ITYPE .LE.  48 ) THEN
c$$$         IPART = ITYPE
c$$$      ELSEIF ( ITYPE .EQ. 201 ) THEN
c$$$         IPART = 45
c$$$      ELSEIF ( ITYPE .EQ. 301 ) THEN
c$$$         IPART = 46
c$$$      ELSEIF ( ITYPE .EQ. 402 ) THEN
c$$$         IPART = 47
c$$$      ELSE
c$$$        WRITE(MONIOU,444) (CURPAR(I),I=1,9)
c$$$ 444    FORMAT(' NGHEI : CURPAR=',1P,9E10.3)
c$$$         WRITE(MONIOU,7795) ITYPE
c$$$ 7795    FORMAT (//,' *NGHEI*  ILLEGAL PARTICLE TYPE OCCURS =',I5)
c$$$         IPART = 48
c$$$      ENDIF
c$$$
c$$$      NETEST=IKPART(KPART)
c$$$      IF ( NETEST .EQ. IPART ) GOTO 9004
c$$$
c$$$      WRITE(MONIOU,8881) IPART,KPART
c$$$ 8881 FORMAT(' *NGHEI* IPART,KPART = ',2(I3,1X)/
c$$$     $ ' *NGHEI* ======> PARTICLE TYPES DO NOT MATCH <=======')
c$$$      STOP
c$$$
c$$$ 9004 CONTINUE

C --- INITIALIZE RELEVANT GHEISHA VARIABLES ---

      call nex2ghe(KPART,pnex)



C  MIXING OF NEUTRAL KAONS
      IF ( KPART .EQ. 11  .OR.  KPART .EQ. 12 ) THEN
        CALL GRNDM( RNDM,1 )
        IF ( RNDM(1) .LT. 0.5 ) THEN
          KPART = 11
        ELSE
          KPART = 12
        ENDIF
      ENDIF
      KKPART = KPART

C --- TRANSPORT THE TRACK NUMBER TO GHEISHA AND INITIALIZE SOME NUMBERS
C --- NTK=ITRA   ITRA = CURRENT TRACK NUMBER IN GEANT (GCKINE)
      NTK   = 0
      INTCT = 0.0D0
      NEXT  = 1
      NTOT  = 0
      INTE  = 0
      TOF   = 0.0D0

c$$$C --- RESET ITYPE
c$$$      SECPAR(1) = 0.D0

C --- FILL RESULT COMMON FOR THIS TRACK WITH CORSIKA VALUES ---

c      iptl=1
      AMAS  = RMASS(KPART)
      NCH   = RCHARG(KPART)
      XEND  = 0.D0  !xorptl(1,iptl)
      YEND  = 0.D0  !xorptl(2,iptl)
      ZEND  = 0.D0  !xorptl(3,iptl)
      USERW = 0.0D0

      P=sqrt(dble(pnex(1)**2+pnex(2)**2+pnex(3)**2)) 
      PX = pnex(1)/P           
      PY = pnex(2)/P          
      PZ = pnex(3)/P         

      AMASQ=AMAS*AMAS
      EN = SQRT(AMASQ+P*P)
      EK = ABS ( EN - ABS(AMAS) )
c$$$      ENOLD = EN
c$$$      EMAX  = 0.
c$$$      ETOT  = 0.D0
c$$$      ELABOR = EN

      if(abs(P-SQRT((EN-AMAS)*(EN+AMAS))).gt.1.d-3)
     $ write(MDEBUG,'("On-Shell problem in Gheisha:  ",2(g12.6,1x),i3)')
     $ P,SQRT((EN-AMAS)*(EN+AMAS)),kpart


      if(nprt(9))
     $     write(MDEBUG,'("in:  ",5(g12.6,1x),i3)') P*PX,P*PY,P*PZ,EN
     $     ,sqrt(amasq),kpart

      SINL=0.0D0
      COSL=1.0D0
      SINP=0.0D0
      COSP=1.0D0
C
      IF (ABS(P) .LE. 1.0E-10) GO TO 1
      SINL=PZ
      COSL=SQRT(ABS(1.0-SINL**2))
C
 1    CONTINUE
      CALL GRNDM(RNDM,1)
      PHI=RNDM(1)*TWPI
      IF ((PX .EQ. 0.0D0) .AND. (PY .EQ. 0.0D0)) GOTO 3
      IF (ABS(PX) .LT. 1.D-10) GOTO 2
      PHI=ATAN2(PY,PX)
      GOTO 3
C
 2    CONTINUE
      IF (PY .GT. 0.0D0) PHI=PI/2.0D0
      IF (PY .LE. 0.0D0) PHI=3.0*PI/2.0D0
C
 3    CONTINUE
      SINP=SIN(PHI)
      COSP=COS(PHI)

C --- SET GHEISHA INDEX FOR THE CURRENT MEDIUM ALWAYS TO 1 ---
      IND = 1

C --- TRANSFER GLOBAL MATERIAL CONSTANTS FOR CURRENT MEDIUM ---
C --- DETAILED DATA FOR COMPOUNDS IS OBTAINED VIA SUBROUT. COMPO ---

      ATNO(IND+1)  = dble(atnxs) !14.56        !atnxs, ztnxs : see ghesig
      ZNO(IND+1)   = dble(ztnxs) ! 7.265
      DEN(IND+1)   = 0.0d0
      RADLTH(IND+1)= 0.0d0
      ABSLTH(IND+1)= 0.0d0

C --- SETUP PARMAT FOR PHYSICS STEERING ---
      PARMAT(IND+1,10)=0.0D0

c  5   CONTINUE

C --- INDICATE LIGHT (<= PI) AND HEAVY PARTICLES (HISTORICALLY) ---
C --- CALIM CODE ---
      J = 2
      TEST = RMASS(7)-0.001D0
      IF (ABS(AMAS) .LT. TEST) J=1

C *** DIVISION INTO VARIOUS INTERACTION CHANNELS DENOTED BY "INTE" ***
C THE CONVENTION FOR "INT" IS THE FOLLOWING

C INTE  = -1 REACTION CROSS-SECTIONS NOT YET TABULATED/PROGRAMMED
C       =  0 NO INTERACTION
C       =  1 ELASTIC SCATTERING
C       =  2 INELASTIC SCATTERING
C       =  3 NUCLEAR FISSION WITH INELASTIC SCATTERING
C       =  4 NEUTRON CAPTURE

C --- INTACT CODE ---
      ALAM1 = 0.0D0
      CALL GRNDM( RNDM,1 )
      RAT   = RNDM(1)*ALAM

C ---  DEFAULT VALUES FOR AIR
      ATNO2 = dble(airavanxs) !14.56
      ZNO2  = dble(airavznxe) !7.265

      DO  K = 1, KK
        ATNO2 = ACOMP(K)
        ZNO2  = ZCOMP(K)

C --- TRY FOR ELASTIC SCATTERING ---            !elastic scattering not use
ctp        INTE = 1
ctp        ALAM1 = ALAM1+AIEL(K)
ctp        IF (RAT .LT. ALAM1) GOTO 8

C --- TRY FOR INELASTIC SCATTERING ---
        INTE = 2
        ALAM1 = ALAM1+AIIN(K)
        IF (RAT .LT. ALAM1) GOTO 8

C --- TRY FOR NEUTRON CAPTURE ---               !neutron capture scattering not use
ctp        INTE = 4
ctp        ALAM1 = ALAM1+AICA(K)
ctp        IF (RAT .LT. ALAM1) GOTO 8

      ENDDO

C --- NO REACTION SELECTED ==> ELASTIC SCATTERING ---
      INTE = 1

C *** TAKE ACTION ACCORDING TO SELECTED REACTION CHANNEL ***
C --- FOLLOWING CODE IS A TRANSLATION OF "CALIM" INTO GEANT JARGON ---

 8    CONTINUE
      IF (NPRT(9)) WRITE(MDEBUG,1001) INTE
 1001 FORMAT(' *NGHEI* INTERACTION TYPE CHOSEN INTE = ',I3)

      IF (INTE .NE. 4) GOTO 10

C --- NEUTRON CAPTURE ---
      IF (NPRT(9)) WRITE(MDEBUG,2000)
 2000 FORMAT(' *NGHEI* SUBROUT. CAPTUR WILL BE CALLED')
      CALL CAPTUR( NOPT )
      GOTO 40

 10   CONTINUE
      IF (INTE .NE. 3) GO TO 11
C --- NUCLEAR FISSION ---
      IF (NPRT(9)) PRINT 2001
 2001 FORMAT(' *GHEISH* ROUTINE FISSIO WILL BE CALLED')
c      ISTOP=1
      TKIN=FISSIO(EK)
      GO TO 40

 11   CONTINUE

C --- ELASTIC AND INELASTIC SCATTERING ---
      PV(1,MXGKPV) = P*PX
      PV(2,MXGKPV) = P*PY
      PV(3,MXGKPV) = P*PZ
      PV(4,MXGKPV) = EN
      PV(5,MXGKPV) = AMAS
      PV(6,MXGKPV) = NCH
      PV(7,MXGKPV) = TOF
      PV(8,MXGKPV) = KPART
      PV(9,MXGKPV) = 0.D0
      PV(10,MXGKPV)= USERW

C --- ADDITIONAL PARAMETERS TO SIMULATE FERMI MOTION AND EVAPORATION ---
      DO  JENP = 1, 10
        ENP(JENP) = 0.D0
      ENDDO
      ENP(5) = EK
      ENP(6) = EN
      ENP(7) = P

      IF (INTE .NE. 1) GOTO 12

C *** ELASTIC SCATTERING PROCESSES ***

C --- ONLY NUCLEAR INTERACTIONS FOR HEAVY FRAGMENTS ---
      IF ((KPART .GE. 30) .AND. (KPART .LE. 32)) GOTO 35

C --- NORMAL ELASTIC SCATTERING FOR LIGHT MEDIA ---
      IF (ATNO2 .LT. 1.5D0) GOTO 35

C --- COHERENT ELASTIC SCATTERING FOR HEAVY MEDIA ---
      IF (NPRT(9)) WRITE(MDEBUG,2002)
 2002 FORMAT(' *NGHEI* SUBROUT. COSCAT WILL BE CALLED')
      CALL COSCAT
      GOTO 40

C *** NON-ELASTIC SCATTERING PROCESSES ***
 12   CONTINUE

C --- ONLY NUCLEAR INTERACTIONS FOR HEAVY FRAGMENTS ---
      IF ((KPART .GE. 30) .AND. (KPART .LE. 32)) GOTO 35

C *** USE SOMETIMES NUCLEAR REACTION SUBROUT. "NUCREC" FOR LOW ENERGY
C *** PROTON AND NEUTRON SCATTERING ***
      CALL GRNDM( RNDM,1 )
      TEST1 = RNDM(1)
      TEST2 = 4.5D0*(EK-0.01D0)
      IF ((KPART .EQ. 14) .AND. (TEST1 .GT. TEST2)) GOTO 85
      IF ((KPART .EQ. 16) .AND. (TEST1 .GT. TEST2)) GOTO 86

C *** FERMI MOTION AND EVAPORATION ***
      TKIN = CINEMA(EK)
      PV(9,MXGKPV) = TKIN
      ENP(5) = EK+TKIN
C --- CHECK FOR LOWERBOUND OF EKIN IN CROSS-SECTION TABLES ---
      IF (ENP(5) .LE. TEKLOW) ENP(5)=TEKLOW
      ENP(6) = ENP(5)+ABS(AMAS)
      ENP(7) = (ENP(6)-AMAS)*(ENP(6)+AMAS)
      ENP(7) = SQRT(ABS(ENP(7)))
      TKIN   = FERMIG(ENP(5))
      ENP(5) = ENP(5)+TKIN
C --- CHECK FOR LOWERBOUND OF EKIN IN CROSS-SECTION TABLES ---
      IF (ENP(5) .LE. TEKLOW) ENP(5)=TEKLOW
      ENP(6) = ENP(5)+ABS(AMAS)
      ENP(7) = (ENP(6)-AMAS)*(ENP(6)+AMAS)
      ENP(7) = SQRT(ABS(ENP(7)))
      TKIN   = EXNU(ENP(5))
      ENP(5) = ENP(5)-TKIN
C --- CHECK FOR LOWERBOUND OF EKIN IN CROSS-SECTION TABLES ---
      IF (ENP(5) .LE. TEKLOW) ENP(5)=TEKLOW
      ENP(6) = ENP(5)+ABS(AMAS)
      ENP(7) = (ENP(6)-AMAS)*(ENP(6)+AMAS)
      ENP(7) = SQRT(ABS(ENP(7)))

C *** IN CASE OF ENERGY ABOVE CUT-OFF LET THE PARTICLE CASCADE ***
      IF ( ENP(5) .GT. ELCUT(1)) GOTO 35

C --- SECOND CHANCE FOR ANTI-BARYONS DUE TO POSSIBLE ANNIHILATION ---
      IF ((AMAS .GE. 0.0D0) .OR. (KPART .LE. 14)) GOTO 13
      ANNI = 1.3D0*P
      IF (ANNI .GT. 0.4D0) ANNI=0.4D0
      CALL GRNDM( RNDM,1 )
      TEST = RNDM(1)
      IF (TEST .GT. ANNI) GOTO 35

C *** PARTICLE WITH ENERGY BELOW CUT-OFF ***
C --- ==> ONLY NUCLEAR EVAPORATION AND QUASI-ELASTIC SCATTERING ---
 13   CONTINUE

      IF (NPRT(9)) WRITE(MDEBUG,1002) KPART,EK,EN,P,ENP(5),ENP(6),ENP(7)
 1002 FORMAT(' *NGHEI* ENERGY BELOW CUT-OFF FOR GHEISHA PARTICLE ',I3/
     $ ' EK,EN,P,ENP(5),ENP(6),ENP(7) = ',6(G12.5,1X))

ctp We don't wan't nuclear evaporation or quasi-elastic scattering -> no interaction
ctp      GOTO 40


      IF ((KPART .NE. 14) .AND. (KPART .NE. 16)) GOTO 14
      IF (KPART .EQ. 16) GOTO 86
 
C --- SLOW PROTON ---
 85   CONTINUE
      IF (NPRT(9)) WRITE(MDEBUG,2003) EK,KPART
 2003 FORMAT(' *NGHEI* SUBROUT. NUCREC WILL BE CALLED',
     $ ' EK = ',G12.5,' GEV  KPART = ',I3)
      CALL NUCREC( NOPT,2 )

      IF (NOPT .NE. 0) GOTO 50

      IF (NPRT(9)) WRITE(MDEBUG,2004)EK,KPART
 2004 FORMAT(' *NGHEI* SUBROUT. COSCAT WILL BE CALLED',
     $ ' EK = ',G12.5,' GEV  KPART = ',I3)
      CALL COSCAT
      GOTO 40

C --- SLOW NEUTRON ---
 86   CONTINUE
      IF (NPRT(9)) WRITE(MDEBUG,2015)
      NUCFLG = 0
      CALL GNSLWD( NUCFLG,INTE,NFL,TEKLOW )
      IF (NUCFLG .NE. 0) GOTO 50
      GOTO 40

C --- OTHER SLOW PARTICLES ---
 14   CONTINUE
      IPA(1) = KPART
C --- DECIDE FOR PROTON OR NEUTRON TARGET ---
      IPA(2) = 16
      CALL GRNDM( RNDM,1 )
      TEST1 = RNDM(1)
      TEST2 = ZNO2/ATNO2
      IF (TEST1 .LT. TEST2) IPA(2)=14
      AVERN = 0.0D0
      NFL = 1
      IF (IPA(2) .EQ. 16) NFL=2
      IPPP = KPART
      IF (NPRT(9)) WRITE(MDEBUG,2005)
 2005 FORMAT(' *NGHEI* SUBROUT. TWOB WILL BE CALLED')
      CALL TWOB( IPPP,NFL,AVERN )
      GOTO 40

C --- INITIALIZATION OF CASCADE QUANTITIES ---
 35   CONTINUE

C *** CASCADE GENERATION ***
C --- CALCULATE FINAL STATE MULTIPLICITY AND LONGITUDINAL AND ---
C --- TRANSVERSE MOMENTUM DISTRIBUTIONS ---

C --- FIXED PARTICLE TYPE TO STEER THE CASCADE ---
      KKPART = KPART

C --- NO CASCADE FOR LEPTONS ---
      IF (KKPART .LE. 6) GOTO 9999

C *** WHAT TO DO WITH "NEW PARTICLES" FOR GHEISHA ?????? ***
C --- RETURN FOR THE TIME BEING ---
      IF (KKPART .GE. 35) GOTO 9999

C --- CASCADE OF HEAVY FRAGMENTS
      IF ((KKPART .GE. 30) .AND. (KKPART .LE. 32)) GOTO 390

C --- INITIALIZE THE IPA ARRAY ---
*     CALL VZERO( IPA(1),MXGKCU )
CDH
      DO  III = 1, MXGKCU
        IPA(III) = 0
      ENDDO


C --- CASCADE OF OMEGA - AND OMEGA - BAR ---
      IF (KKPART .EQ. 33) GOTO 330
      IF (KKPART .EQ. 34) GOTO 331

      NVEPAR = KKPART-17
      IF (NVEPAR .LE. 0) GOTO 15
      GOTO (318,319,320,321,322,323,324,325,326,327,328,329),NVEPAR

 15   CONTINUE
      NVEPAR = KKPART-6
      GOTO (307,308,309,310,311,312,313,314,315,316,317,318),NVEPAR

C --- PI+ CASCADE ---
 307  CONTINUE
      IF (NPRT(9)) WRITE(MDEBUG,2006)
 2006 FORMAT(' *NGHEI* SUBROUT. CASPIP WILL BE CALLED')
      CALL CASPIP( J,INTE,NFL )
      GOTO 40

C --- PI0 ==> NO CASCADE ---
 308  CONTINUE
      GOTO 40

C --- PI- CASCADE ---
 309  CONTINUE
      IF (NPRT(9)) WRITE(MDEBUG,2007)
 2007 FORMAT(' *NGHEI* SUBROUT. CASPIM WILL BE CALLED')
      CALL CASPIM( J,INTE,NFL )
      GOTO 40

C --- K+ CASCADE ---
 310  CONTINUE
      IF (NPRT(9)) WRITE(MDEBUG,2008)
 2008 FORMAT(' *NGHEI* SUBROUT. CASKP WILL BE CALLED')
      CALL CASKP( J,INTE,NFL )
      GOTO 40

C --- K0 CASCADE ---
 311  CONTINUE
      IF (NPRT(9)) WRITE(MDEBUG,2009)
 2009 FORMAT(' *NGHEI* SUBROUT. CASK0 WILL BE CALLED')
      CALL CASK0( J,INTE,NFL )
      GOTO 40

C --- K0 BAR CASCADE ---
 312  CONTINUE
      IF (NPRT(9)) WRITE(MDEBUG,2010)
 2010 FORMAT(' *NGHEI* SUBROUT. CASK0B WILL BE CALLED')
      CALL CASK0B( J,INTE,NFL )
      GOTO 40

C --- K- CASCADE ---
 313  CONTINUE
      IF (NPRT(9)) WRITE(MDEBUG,2011)
 2011 FORMAT(' *NGHEI* SUBROUT. CASKM WILL BE CALLED')
      CALL CASKM( J,INTE,NFL )
      GOTO 40

C --- PROTON CASCADE ---
 314  CONTINUE
      IF (NPRT(9)) WRITE(MDEBUG,2012)
 2012 FORMAT(' *NGHEI* SUBROUT. CASP WILL BE CALLED')
      CALL CASP( J,INTE,NFL )
      GOTO 40

C --- PROTON BAR CASCADE ---
 315  CONTINUE
      IF (NPRT(9)) WRITE(MDEBUG,2013)
 2013 FORMAT(' *NGHEI* SUBROUT. CASPB WILL BE CALLED')
      CALL CASPB( J,INTE,NFL )
      GOTO 40

C --- NEUTRON CASCADE ---
 316  CONTINUE
      NUCFLG = 0
      IF (EK .GT. SWTEKN) THEN
         CALL CASN( J,INTE,NFL )
         IF (NPRT(9)) WRITE(MDEBUG,2014)
 2014 FORMAT(' *NGHEI* SUBROUT. CASN WILL BE CALLED')
      ELSE
         CALL GNSLWD( NUCFLG,INTE,NFL,TEKLOW )
         IF (NPRT(9)) WRITE(MDEBUG,2015)
 2015 FORMAT(' *NGHEI* SUBROUT. GNSLWD WILL BE CALLED')
      ENDIF
      IF (NUCFLG .NE. 0) GOTO 50
      GOTO 40

C --- NEUTRON BAR CASCADE ---
 317  CONTINUE
      IF (NPRT(9)) WRITE(MDEBUG,2016)
 2016 FORMAT(' *NGHEI* SUBROUT. CASNB WILL BE CALLED')
      CALL CASNB( J,INTE,NFL )
      GOTO 40

C --- LAMBDA CASCADE ---
 318  CONTINUE
      IF (NPRT(9)) WRITE(MDEBUG,2017)
 2017 FORMAT(' *NGHEI* SUBROUT. CASL0 WILL BE CALLED')
      CALL CASL0( J,INTE,NFL )
      GOTO 40

C --- LAMBDA BAR CASCADE ---
 319  CONTINUE
      IF (NPRT(9)) WRITE(MDEBUG,2018)
 2018 FORMAT(' *NGHEI* SUBROUT. CASAL0 WILL BE CALLED')
      CALL CASAL0( J,INTE,NFL )
      GOTO 40

C --- SIGMA + CASCADE ---
 320  CONTINUE
      IF (NPRT(9)) WRITE(MDEBUG,2019)
 2019 FORMAT(' *NGHEI* SUBROUT. CASSP WILL BE CALLED')
      CALL CASSP( J,INTE,NFL )
      GOTO 40

C --- SIGMA 0 ==> NO CASCADE ---
 321  CONTINUE
      GOTO 40

C --- SIGMA - CASCADE ---
 322  CONTINUE
      IF (NPRT(9)) WRITE(MDEBUG,2020)
 2020 FORMAT(' *NGHEI* SUBROUT. CASSM WILL BE CALLED')
      CALL CASSM( J,INTE,NFL )
      GOTO 40

C --- SIGMA + BAR CASCADE ---
 323  CONTINUE
      IF (NPRT(9)) WRITE(MDEBUG,2021)
 2021 FORMAT(' *NGHEI* SUBROUT. CASASP WILL BE CALLED')
      CALL CASASP( J,INTE,NFL )
      GOTO 40

C --- SIGMA 0 BAR ==> NO CASCADE ---
 324  CONTINUE
      GOTO 40

C --- SIGMA - BAR CASCADE ---
 325  CONTINUE
      IF (NPRT(9)) WRITE(MDEBUG,2022)
 2022 FORMAT(' *NGHEI* SUBROUT. CASASM WILL BE CALLED')
      CALL CASASM( J,INTE,NFL )
      GOTO 40

C --- XI 0 CASCADE ---
 326  CONTINUE
      IF (NPRT(9)) PRINT 2023
 2023 FORMAT(' *NGHEI* SUBROUT. CASX0 WILL BE CALLED')
      CALL CASX0( J,INTE,NFL )
      GOTO 40

C --- XI - CASCADE ---
 327  CONTINUE
      IF (NPRT(9)) PRINT 2024
 2024 FORMAT(' *NGHEI* SUBROUT. CASXM WILL BE CALLED')
      CALL CASXM( J,INTE,NFL )
      GOTO 40

C --- XI 0 BAR CASCADE ---
 328  CONTINUE
      IF (NPRT(9)) PRINT 2025
 2025 FORMAT(' *NGHEI* SUBROUT. CASAX0 WILL BE CALLED')
      CALL CASAX0( J,INTE,NFL )
      GOTO 40

C --- XI - BAR CASCADE ---
 329  CONTINUE
      IF (NPRT(9)) PRINT 2026
 2026 FORMAT(' *NGHEI* SUBROUT. CASAXM WILL BE CALLED')
      CALL CASAXM( J,INTE,NFL )
      GOTO 40

C --- OMEGA - CASCADE ---
 330  CONTINUE
      IF (NPRT(9)) PRINT 2027
 2027 FORMAT(' *NGHEI* SUBROUT. CASOM WILL BE CALLED')
      CALL CASOM( J,INTE,NFL )
      GOTO 40

C --- OMEGA - BAR CASCADE ---
 331  CONTINUE
      IF (NPRT(9)) PRINT 2028
 2028 FORMAT(' *NGHEI* SUBROUT. CASAOM WILL BE CALLED')
      CALL CASAOM( J,INTE,NFL )
      GOTO 40

C --- HEAVY FRAGMENT CASCADE ---
 390  CONTINUE
      IF (NPRT(9)) WRITE(MDEBUG,2090)
 2090 FORMAT(' *NGHEI* SUBROUT. CASFRG WILL BE CALLED')
      NUCFLG = 0
      CALL CASFRG( NUCFLG,INTE,NFL )
      IF (NUCFLG .NE. 0) GOTO 50

C *** CHECK WHETHER THERE ARE NEW PARTICLES GENERATED ***
 40   CONTINUE
      IF ((NTOT .NE. 0) .OR. (KKPART .NE. KPART)) GOTO 50

C
C --- NO SECONDARIES GENERATED AND PARTICLE IS STILL THE SAME ---
C --- ==> COPY PROJECTILE BACK IN epos ---
C --- In case of crazy momentum value ==> no change to epos stack ---
      IF (P .LT. 0.) GO TO 41
      call ghe2nex(KPART,pnex)
      
  41   CONTINUE

       IF (NPRT(9)) WRITE(MDEBUG,1003)NTOT,KPART,KKPART
 1003 FORMAT(' *GHEISH* NO SEC. GEN. NTOT,KPART,KKPART = ',
     $ 3(I3,1X)/
     $ ' CURRENT PARTICLE ON THE STACK AGAIN')
      GO TO 9999
C
C *** CURRENT PARTICLE IS NOT THE SAME AS IN THE BEGINNING OR/AND ***
C *** ONE OR MORE SECONDARIES HAVE BEEN GENERATED ***
 50   CONTINUE

      IF (NPRT(9)) WRITE(MDEBUG,1004)NTOT,KPART,KKPART
 1004 FORMAT(' *NGHEI* SEC. GEN. NTOT,KPART,KKPART = ',
     $ 3(I3,1X))

C --- INITIAL PARTICLE TYPE HAS BEEN CHANGED ==> PUT NEW TYPE ON ---
C --- THE TEMPORARY STACK ---

C --- MAKE CHOICE BETWEEN K0 LONG / K0 SHORT ---
      IF ((KPART .NE. 11) .AND. (KPART .NE. 12)) GOTO 52
      CALL GRNDM( RNDM,1 )
      KPART = int(11.5D0+RNDM(1))

 52   CONTINUE

C --- IN CASE THE NEW PARTICLE IS A NEUTRINO ==> FORGET IT ---
      IF (KPART .EQ. 2) GOTO 60
c$$$
c$$$C --- PUT CURRENT GHEISHA PARTICLE ON THE CORSIKA STACK
c$$$C --- ( IF SURVIVING ANGLE CUT ! )
c$$$      NGKINE = 1
c$$$
c$$$C --- CALCULATE ELASTICITY
c$$$      IF ( EN .GT. EMAX ) THEN
c$$$         EMAX = EN
c$$$      ENDIF
c$$$
c$$$      ITY=IKPART(KPART)
c$$$C  OLD COUPLING
c$$$C     SECPAR(3) = -PZ
c$$$C     IF ( SECPAR(3) .GT. C(29) ) THEN
c$$$
c$$$      IF     ( ITY .LT. 45 ) THEN
c$$$        SECPAR(1) = DBLE(ITY)
c$$$      ELSEIF ( ITY .EQ. 45 ) THEN
c$$$        SECPAR(1) = 201.D0
c$$$      ELSEIF ( ITY .EQ. 46 ) THEN
c$$$        SECPAR(1) = 301.D0
c$$$      ELSEIF ( ITY .EQ. 47 ) THEN
c$$$        SECPAR(1) = 402.D0
c$$$      ENDIF
c$$$      IF ( ABS(AMAS) .LT. 1.E-9 ) THEN
c$$$        SECPAR(2) = EN
c$$$      ELSE
c$$$        SECPAR(2) = DBLE(EN) / DBLE(ABS(AMAS))
c$$$      ENDIF
c$$$      IF     ( ITY .EQ. 13  .OR. ITY .EQ. 14 ) THEN
c$$$        ETOT = ETOT + (SECPAR(2) - 1.D0) * PAMA(ITY)
c$$$      ELSEIF ( ITY .EQ. 15  .OR. ITY .EQ. 25 ) THEN
c$$$        ETOT = ETOT + (SECPAR(2) + 1.D0) * PAMA(ITY)
c$$$      ELSE
c$$$        ETOT = ETOT + EN
c$$$      ENDIF
c$$$C  NEW COUPLING WITH CORSIKA  D. HECK  DEC. 2000
c$$$      THETG = -PZ
c$$$      IF ( PX .NE. 0.  .OR.  PY .NE. 0. ) THEN
c$$$        PHIG = ATAN2( DBLE(PY), DBLE(PX) ) + PHIRAN
c$$$      ELSE
c$$$        PHIG = 0.D0 + PHIRAN
c$$$      ENDIF
c$$$      CALL ADDANG( CURPAR(3),CURPAR(4), THETG,PHIG, SECPAR(3),SECPAR(4))
c$$$C  CHECK WETHER PARTICLE SURVIVES ANGULAR CUT
c$$$      IF ( SECPAR(3) .GT. C(29) ) THEN
c$$$        CALL TSTACK
c$$$      ELSE
c$$$        IF ( LLONGI ) THEN
c$$$C  ADD ENERGY TO LONGITUDINAL ENERGY DEPOSIT
c$$$          IF     ( ITY .EQ. 1                   ) THEN
c$$$            DLONG(LHEIGH,11) = DLONG(LHEIGH,11) + EN
c$$$          ELSEIF ( ITY .EQ. 2                   ) THEN
c$$$            DLONG(LHEIGH,13) = DLONG(LHEIGH,13) + EN + PAMA(2)
c$$$          ELSEIF ( ITY .EQ. 3                   ) THEN
c$$$            DLONG(LHEIGH,13) = DLONG(LHEIGH,13) + EN - PAMA(2)
c$$$          ELSEIF ( ITY .EQ. 5  .OR.  ITY .EQ. 6 ) THEN
c$$$            DLONG(LHEIGH,15) = DLONG(LHEIGH,15) + EN
c$$$          ELSEIF ( ITY .GE. 7                   ) THEN
c$$$            DLONG(LHEIGH,17) = DLONG(LHEIGH,17) + EN - RESTMS(ITY)
c$$$          ENDIF
c$$$        ENDIF
c$$$      ENDIF
c$$$
      pnex(1)=sngl(PX*P)
      pnex(2)=sngl(PY*P)
      pnex(3)=sngl(PZ*P)
      pnex(4)=sngl(SQRT(P*P+RMASS(KPART)**2))
      pnex(5)=sngl(abs(RMASS(KPART)))
      call ghe2nex(KPART,pnex)

C *** CHECK WHETHER SECONDARIES HAVE BEEN GENERATED AND COPY THEM ***
C *** ALSO ON THE GEANT STACK ***
 60   CONTINUE

C --- ALL QUANTITIES ARE TAKEN FROM THE GHEISHA STACK WHERE THE ---
C --- CONVENTION IS THE FOLLOWING ---
C
C EVE(INDEX+ 1)= X
C EVE(INDEX+ 2)= Y
C EVE(INDEX+ 3)= Z
C EVE(INDEX+ 4)= NCAL
C EVE(INDEX+ 5)= NCELL
C EVE(INDEX+ 6)= MASS
C EVE(INDEX+ 7)= CHARGE
C EVE(INDEX+ 8)= TOF
C EVE(INDEX+ 9)= PX
C EVE(INDEX+10)= PY
C EVE(INDEX+11)= PZ
C EVE(INDEX+12)= TYPE

      IF ( NTOT .LE. 0 ) GOTO 9999

C --- ONE OR MORE SECONDARIES HAVE BEEN GENERATED ---
      DO 61  L = 1, NTOT
        INDEX = (L-1)*12
        JND = int(EVE(INDEX+12))

C --- MAKE CHOICE BETWEEN K0 LONG / K0 SHORT ---
        IF ((JND .NE. 11) .AND. (JND .NE. 12)) GOTO 63
        CALL GRNDM( RNDM,1 )
        JND = int(11.5D0+RNDM(1))

C --- FORGET ABOUT NEUTRINOS ---
 63     CONTINUE
        IF ( JND .EQ. 2 ) GOTO 61

c$$$C --- SWITCH TO CORSIKA QUANTITIES ---
c$$$        ITY = IKPART(JND)
c$$$        IF (NPRT(9)) WRITE(MDEBUG,1006)
c$$$     $                            ITY,NGKINE,L,(EVE(INDEX+J),J=1,12)
c$$$ 1006   FORMAT(' *NGHEI* GEANT PART. ',I3,' ALSO PUT ONTO STACK AT',
c$$$     $   ' POS. ',I3/
c$$$     $   ' EVE(',I2,') = ',(' ',10G12.5))

        PLX = EVE(INDEX+9)
        PLY = EVE(INDEX+10)
        PLZ = EVE(INDEX+11)
c$$$        PLSQ   = PLX**2 + PLY**2 + PLZ**2
c$$$        PLTOT  = SQRT(PLSQ)
c$$$        RMASSK = ABS(RMASS(JND))

c$$$C  FIND HIGHEST ENERGY PARTICLE FOR ELASTICITY
c$$$        EEESQ = PLSQ + RMASSK**2
c$$$        IF ( EEESQ .GT. EMAX**2 ) THEN
c$$$          EMAX = SQRT(EEESQ)
c$$$        ENDIF
c$$$
c$$$C --- APPLY ANGLE CUT AND
c$$$C --- ADD PARTICLE TO THE CORSIKA STACK (RESTRICTED TO 100) ---
c$$$        IF ( PLTOT .LE. 1.D-10 ) GOTO 61
c$$$C     SECPAR(3) = (-PLZ) / PLTOT
c$$$        THETG     = (-PLZ) / PLTOT
c$$$        IF ( RMASSK .LT. 1.D-9 ) THEN
c$$$          SECPAR(2) = PLTOT
c$$$        ELSE
c$$$          SECPAR(2) = SQRT (PLSQ+RMASSK**2) / RMASSK
c$$$        ENDIF
c$$$        IF     ( ITY .LT. 45 ) THEN
c$$$          SECPAR(1) = DBLE(ITY)
c$$$        ELSEIF ( ITY .EQ. 45 ) THEN
c$$$          SECPAR(1) = 201.D0
c$$$        ELSEIF ( ITY .EQ. 46 ) THEN
c$$$          SECPAR(1) = 301.D0
c$$$        ELSEIF ( ITY .EQ. 47 ) THEN
c$$$          SECPAR(1) = 402.D0
c$$$        ELSE
c$$$          SECPAR(1) = 0.D0
c$$$          WRITE(MONIOU,*) '*NGHEI*  ILLEGAL PARTICLE TYPE',ITY
c$$$        ENDIF
c$$$C --- COUNTERS FOR FIRST INTERACTION
c$$$        ITY = SECPAR(1)
c$$$        IF     ( ITY .EQ. 1                    ) THEN
c$$$          ETOT = ETOT + SQRT(EEESQ)
c$$$        ELSEIF ( ITY .EQ. 13  .OR. ITY .EQ. 14 ) THEN
c$$$          ETOT = ETOT + (SECPAR(2) - 1.D0) * PAMA(ITY)
c$$$        ELSEIF ( ITY .EQ. 15  .OR. ITY .EQ. 25 ) THEN
c$$$          ETOT = ETOT + (SECPAR(2) + 1.D0) * PAMA(ITY)
c$$$        ELSE
c$$$          ETOT = ETOT + SECPAR(2) * PAMA(ITY)
c$$$        ENDIF
c$$$        IF ( FIRSTI ) THEN
c$$$          IF     ( ITY .EQ.  7  .OR.  ITY .EQ.  8
c$$$     *          .OR.  ITY .EQ.  9                    ) THEN
c$$$            IFINPI = IFINPI + 1
c$$$          ELSEIF ( ITY .EQ. 13  .OR.  ITY .EQ. 14
c$$$     *          .OR.  ITY .EQ. 15  .OR.  ITY .EQ. 25 ) THEN
c$$$            IFINNU = IFINNU + 1
c$$$          ELSEIF ( ITY .EQ. 10  .OR.  ITY .EQ. 11
c$$$     *          .OR.  ITY .EQ. 12  .OR.  ITY .EQ. 16 ) THEN
c$$$            IFINKA = IFINKA + 1
c$$$          ELSEIF ( ITY .EQ. 17 ) THEN
c$$$            IFINET = IFINET + 1
c$$$          ELSEIF ((ITY .GE. 18  .AND. ITY .LE. 24)
c$$$     *          .OR. (ITY .GE. 26  .AND. ITY .LE. 32)) THEN
c$$$            IFINHY = IFINHY + 1
c$$$          ENDIF
c$$$        ENDIF
c$$$
c$$$C  NEW COUPLING WITH CORSIKA  D. HECK  DEC. 2000
c$$$        IF (NGKINE .GE. MXGKGH) GOTO 9999
c$$$        NGKINE = NGKINE+1
c$$$        IF ( PLX .NE. 0.D0  .OR.  PLY .NE. 0.D0 ) THEN
c$$$          PHIG = ATAN2( PLY, PLX ) + PHIRAN
c$$$        ELSE
c$$$          PHIG = 0.D0 + PHIRAN
c$$$        ENDIF
c$$$        CALL ADDANG( CURPAR(3),CURPAR(4), THETG,PHIG,
c$$$     *                                          SECPAR(3),SECPAR(4) )
c$$$C  CHECK WETHER PARTICLE SURVIVES ANGULAR CUT
c$$$        IF ( SECPAR(3) .GT. C(29) ) THEN
c$$$          CALL TSTACK
c$$$        ELSE
c$$$          IF ( LLONGI ) THEN
c$$$C  ADD ENERGY TO LONGITUDINAL ENERGY DEPOSIT
c$$$            IF     ( ITY .EQ. 1                   ) THEN
c$$$              DLONG(LHEIGH,11) = DLONG(LHEIGH,11) + SECPAR(2)
c$$$            ELSEIF ( ITY .EQ. 2                   ) THEN
c$$$              DLONG(LHEIGH,13) = DLONG(LHEIGH,13)
c$$$     *                                      + (SECPAR(2)+1.D0)*PAMA(2)
c$$$            ELSEIF ( ITY .EQ. 3                   ) THEN
c$$$              DLONG(LHEIGH,13) = DLONG(LHEIGH,13)
c$$$     *                                      + (SECPAR(2)-1.D0)*PAMA(2)
c$$$            ELSEIF ( ITY .EQ. 5  .OR.  ITY .EQ. 6 ) THEN
c$$$              DLONG(LHEIGH,15) = DLONG(LHEIGH,15) + SECPAR(2)*PAMA(5)
c$$$            ELSEIF ( ITY .GE. 7                   ) THEN
c$$$              DLONG(LHEIGH,17) = DLONG(LHEIGH,17) + SECPAR(2)*PAMA(ITY)
c$$$     *                                            - RESTMS(ITY)
c$$$            ENDIF
c$$$          ENDIF
c$$$        ENDIF
      pnex(1)=sngl(PLX)
      pnex(2)=sngl(PLY)
      pnex(3)=sngl(PLZ)
      pnex(4)=sngl(SQRT(PLX*PLX+PLY*PLY+PLZ*PLZ
     $     +EVE(INDEX+6)*EVE(INDEX+6)))
      pnex(5)=sngl(abs(EVE(INDEX+6))) ! RMASS()
      call ghe2nex(JND,pnex)

   61 CONTINUE

c$$$C --- COUNTER FOR ENERGY-MULTIPLICITY MATRIX
c$$$      MSMM = MSMM + NTOT
c$$$
c$$$C --- FILL ELASTICITY IN MATRICES
c$$$      ELASTI = EMAX/ENOLD
c$$$      MELL = MIN ( 1.D0+10.D0*      MAX( 0.D0, ELASTI ) , 11.D0 )
c$$$      MEN  = MIN ( 4.D0+ 3.D0*LOG10(MAX( .1D0, EKINL )), 40.D0 )
c$$$      IELDPM(MEN,MELL) = IELDPM(MEN,MELL) + 1
c$$$      IELDPA(MEN,MELL) = IELDPA(MEN,MELL) + 1
c$$$      IF ( ELASTI .LT. 1. ) THEN
c$$$        ELMEAN(MEN) = ELMEAN(MEN) + ELASTI
c$$$        ELMEAA(MEN) = ELMEAA(MEN) + ELASTI
c$$$      ENDIF

c$$$      IF ( FIRSTI ) THEN
c$$$        TARG1I = ATNO2
c$$$        SIG1I  = SIGAIR
c$$$        ELAST  = ELASTI
c$$$        FIRSTI = .FALSE.
c$$$      ENDIF
c$$$      IF ( DEBUG ) WRITE(MDEBUG,*)'NGHEI : EXIT WITH ETOT=',SNGL(ETOT)

 9999 CONTINUE
c$$$C --- LIMIT THE VALUE OF NGKINE IN CASE OF OVERFLOW ---
c$$$      NGKINE = MIN(NGKINE,MXGKGH)

      RETURN
      END
*CMZ :          05/02/2003  09.12.43  by  D. HECK IK FZK KARLSRUHE
*-- Author :    The CORSIKA development group   21/04/1994
C=======================================================================

      SUBROUTINE NGHINI

C-----------------------------------------------------------------------
C  N(EXUS) GH(EISHA) INI(TIALIZATION)
C  INITIALIZATION OF RELEVANT GHEISHA VARIABLES.
C  THIS SUBROUTINE IS CALLED FROM IniGheisha.
C
C  ORIGIN  : GHEISHA SUBROUT. "GHEINI", F.CARMINATI
C  REDESIGN FOR CORSIKA : P. GABRIEL IK1  FZK KARLSRUHE
C  REDESIGN FOR epos : P. T. PIEROG IK3  FZK KARLSRUHE
C-----------------------------------------------------------------------

      IMPLICIT DOUBLE PRECISION(A-H, O-Z)
C epos COMMON
      common /ighnx/ ighenex(35)
      common/prnt1/iprmpt,ish,ishsub,irandm,irewch,iecho,modsho,idensi
      common/files/ifop,ifmt,ifch,ifcx,ifhi,ifdt,ifcp,ifdr,ifio
      real pi,pii,hquer,prom,piom,ainfin,am
      common/cnsta/pi,pii,hquer,prom,piom,ainfin 

c$$$*KEEP,AIR.
c$$$      COMMON /GHECRAIR/   COMPOS,PROBTA,AVERAW,AVOGDR
c$$$      DOUBLE PRECISION COMPOS(3),PROBTA(3),AVERAW,AVOGDR
c$$$*KEEP,CGCOMP.
c$$$      PARAMETER (KKK=3)
c$$$      COMMON /GHECGCOMP/ ACOMP,ZCOMP,WCOMP,KK
c$$$      REAL           ACOMP(KKK),ZCOMP(KKK),WCOMP(KKK)
c$$$*KEEP,PAM.
c$$$      COMMON /GHECRPAM/   PAMA,SIGNUM,RESTMS,DECTIM
c$$$      DOUBLE PRECISION PAMA(6000),SIGNUM(6000),RESTMS(6000),
c$$$     *                 DECTIM(200)
c$$$
c$$$*KEEP,RUNPAR.
      COMMON /CRRUNPAR/DEBUG,MDEBUG
      LOGICAL          DEBUG
      INTEGER          MDEBUG
c$$$      COMMON /GHECRRUNPAR/FIXHEI,THICK0,HILOECM,HILOELB,SIG1I,TARG1I,
c$$$     *                 STEPFC,NRRUN,NSHOW,MPATAP,MONIIN,
c$$$     *                 MONIOU,MDEBUG,NUCNUC,MTABOUT,MLONGOUT,
c$$$     *                 ISHOWNO,ISHW,NOPART,NRECS,NBLKS,MAXPRT,NDEBDL,
c$$$     *                 N1STTR,MDBASE,
c$$$     *                 DEBDEL,DEBUG,FDECAY,FEGS,FIRSTI,FIXINC,FIXTAR,
c$$$     *                 FIX1I,FMUADD,FNKG,FPRINT,FDBASE,FPAROUT,FTABOUT,
c$$$     *                 FLONGOUT,GHEISH,GHESIG,GHEISDB,USELOW,TMARGIN
c$$$      COMMON /GHECRRUNPAC/DSN,DSNTAB,DSNLONG,HOST,USER
c$$$      DOUBLE PRECISION FIXHEI,THICK0,HILOECM,HILOELB,SIG1I,TARG1I,STEPFC
c$$$      INTEGER          NRRUN,NSHOW,MPATAP,MONIIN,MONIOU,MDEBUG,NUCNUC,
c$$$     *                 ISHOWNO,ISHW,NOPART,NRECS,NBLKS,MAXPRT,NDEBDL,
c$$$     *                 N1STTR,MDBASE,MTABOUT,MLONGOUT
c$$$      CHARACTER*79     DSN,DSNTAB,DSNLONG
c$$$      CHARACTER*20     HOST,USER
c$$$      LOGICAL          DEBDEL,DEBUG,FDECAY,FEGS,FIRSTI,FIXINC,FIXTAR,
c$$$     *                 FIX1I,FMUADD,FNKG,FPRINT,FDBASE,FPAROUT,FTABOUT,
c$$$     *                 FLONGOUT,GHEISH,GHESIG,GHEISDB,USELOW,TMARGIN
*KEND.

      COMMON/GSECTI/ AIEL(20),AIIN(20),AIFI(20),AICA(20),ALAM,K0FLAG
      DOUBLE PRECISION AIEL,AIIN,AIFI,AICA,ALAM
      INTEGER K0FLAG

C --- GHEISHA COMMONS ---
C --- INITIALIZATION FLAGS FOR VARIOUS GHEISHA ROUTINES ---
      COMMON /KGINIT/ KGINIT(50)

      COMMON /CONSTS/gPI,TWPI,PIBTW,MP,MPI,MMU,MEL,MKCH,MK0,SMP,SMPI,
     $               SMU,CT,CTKCH,CTK0,
     $               ML0,MSP,MS0,MSM,MX0,MXM,CTL0,CTSP,CTSM,CTX0,CTXM,
     $               RMASS(35),RCHARG(35)

      DOUBLE PRECISION MP,MPI,MMU,MEL,MKCH,MK0,
     *                 ML0,MSP,MS0,MSM,MX0,MXM

      PARAMETER (MXGKGH=100)
      PARAMETER (MXEVEN=12*MXGKGH)
C --- COMMON "EVENT" CHANGED TO "GEVENT"  DUE TO CLASH ---
C --- WITH  "EVENT" IN epos COMMON ---
      COMMON /GEVENT / NSIZE,NCUR,NEXT,NTOT,EVE(MXEVEN)

      COMMON /PRNTFL/INBCD,NEWBCD,INBIN,NEWBIN,NPEVT,NEVTP,LPRT
     *,NPRT(10)
                    LOGICAL LPRT,NPRT

      PARAMETER (MXGKPV=MXGKGH)
      COMMON /VECUTY/ PV(10,MXGKPV)

C --- BOUNDARY LIMITS FOR ARGUMENTS OF INTRINSIC FUNCTIONS ---
C --- XL DENOTES LOWER BOUND WHEREAS XU DENOTES UPPER BOUND ---
      COMMON /LIMITS/ EXPXL,EXPXU


C --- "NEVENT" CHANGED TO "KEVENT" IN COMMON /GHECURPAR/ DUE TO CLASH ---
C --- WITH VARIABLE "NEVENT" IN GEANT COMMON ---

      PARAMETER (MXGKCU=MXGKGH)
      COMMON /CURPAR/WEIGHT(10),DDELTN,IFILE,IRUN,NEVT,KEVENT,SHFLAG,
     $                ITHST,ITTOT,ITLST,IFRND,TOFCUT,CMOM(5),CENG(5),
     $                RS,S,ENP(10),NP,NM,NN,NR,NO,NZ,IPA(MXGKCU),
     $                ATNO2,ZNO2

      SAVE

C-----------------------------------------------------------------------

c$$$C --- INITIALIZE COMPOSITION OF AIR
c$$$      KK       = 3
c$$$      WCOMP(1) = COMPOS(1)
c$$$      WCOMP(2) = COMPOS(2)
c$$$      WCOMP(3) = COMPOS(3)
c$$$      ACOMP(1) = 14.
c$$$      ACOMP(2) = 16.
c$$$      ACOMP(3) = 40.
c$$$      ZCOMP(1) =  7.
c$$$      ZCOMP(2) =  8.
c$$$      ZCOMP(3) = 18.

C --- SET GHEISHA I/O UNITS TO THE SAME AS FOR epos --
      INBCD=ifmt
      NEWBCD=ifmt
      IF (ish.ge.3) NEWBCD=ifch
      MDEBUG=ifch

C --- INITIALIZE ALL GHEISHA PRINT FLAGS AS FALSE ---
C --- ACTIVATION IS DONE BY "DEBUG" STEERING CARD ---
      DO  J = 1, 10
        NPRT(J)=.FALSE.
      ENDDO
c$$$      IF (ish.ge.3) THEN
c$$$        NPRT(4)=.TRUE.
c$$$        NPRT(9)=.TRUE.
c$$$      ELSE
c$$$        NPRT(4)=.FALSE.
c$$$        NPRT(9)=.FALSE.
c$$$      ENDIF
      if(ish.ge.2)jmax=min(ish-1,10)
      do j=1,jmax
        NPRT(J)=.TRUE.
      enddo
      if(ish.ge.3)NPRT(9)=.TRUE.
      LPRT=.FALSE.
      DO  I = 1, MXGKPV
        DO  J = 1, 10
          PV(J,I)=0.D0
        ENDDO
      ENDDO

C --- INITIALIZE KGINIT ARRAY ---
      DO  J = 1, 50
        KGINIT(J)=0
      ENDDO

C --- INITIALIZE SOME CUT-OFF PARAMETERS WITH epos VALUES ---
      TOFCUT=dble(ainfin) !1.0E+20
      NSIZE=MXEVEN
      K0FLAG=0
      CENG(3)=0.D0
      CENG(4)=0.D0

C --- INITIALIZE PI, 2*PI, PI/2 AND PARTICLE PARAMETERS ---
      gPI=dble(pi)
      TWPI=2.0*gPI
      PIBTW=gPI/2.0
C *** GAMMA ***
      call idmass(ighenex(1),am)
      RMASS(1)=dble(am)
      RCHARG(1)=0.0D0
C *** NEUTRINO ***
      call idmass(ighenex(2),am)
      RMASS(2)=dble(am)
      RCHARG(2)=0.0D0
C *** E+ ***
      call idmass(ighenex(3),am)
      RMASS(3)=dble(am)
      RCHARG(3)=1.0D0
C *** E- ***
      call idmass(ighenex(4),am)
      RMASS(4)=dble(am)
      RCHARG(4)=-1.0D0
C *** MU+ ***
      call idmass(ighenex(5),am)
      RMASS(5)=dble(am)
      RCHARG(5)=1.0D0
C *** MU- ***
      call idmass(ighenex(6),am)
      RMASS(6)=dble(am)
      RCHARG(6)=-1.0D0
C *** PI+ ***
      call idmass(ighenex(7),am)
      RMASS(7)=dble(am)
      RCHARG(7)=1.0D0
      CT=780.4
C *** PI0 ***
      call idmass(ighenex(8),am)
      RMASS(8)=dble(am)
      RCHARG(8)=0.0D0
C *** PI- ***
      call idmass(ighenex(9),am)
      RMASS(9)=dble(am)
      RCHARG(9)=-1.0D0
C *** K+ ***
      call idmass(ighenex(10),am)
      RMASS(10)=dble(am)
      RCHARG(10)=1.0D0
      CTKCH=370.9
C *** K0 SHORT (==> K0) ***
      call idmass(ighenex(11),am)
      RMASS(11)=dble(am)
      RCHARG(11)=0.0D0
      CTK0=2.675
C *** K0 LONG (==> K0 BAR) ***
      call idmass(ighenex(12),am)
      RMASS(12)=-dble(am)
      RCHARG(12)=0.0D0
C *** K- ***
      call idmass(ighenex(13),am)
      RMASS(13)=dble(am)
      RCHARG(13)=-1.0D0
C *** P ***
      call idmass(ighenex(14),am)
      RMASS(14)=dble(am)
      RCHARG(14)=1.0D0
C *** P BAR ***
      call idmass(ighenex(15),am)
      RMASS(15)=-dble(am)
      RCHARG(15)=-1.0D0
C *** N ***
      call idmass(ighenex(16),am)
      RMASS(16)=dble(am)
      RCHARG(16)=0.0D0
C *** N BAR ***
      call idmass(ighenex(17),am)
      RMASS(17)=-dble(am)
      RCHARG(17)=0.0D0
C *** L0 ***
      call idmass(ighenex(18),am)
      RMASS(18)=dble(am)
      RCHARG(18)=0.0D0
      CTL0=7.89
C *** L0 BAR ***
      call idmass(ighenex(19),am)
      RMASS(19)=-dble(am)
      RCHARG(19)=0.0D0
C *** S+ ***
      call idmass(ighenex(20),am)
      RMASS(20)=dble(am)
      RCHARG(20)=1.0D0
      CTSP=2.40
C *** S0 ***
      call idmass(ighenex(21),am)
      RMASS(21)=dble(am)
      RCHARG(21)=0.0D0
C *** S- ***
      call idmass(ighenex(22),am)
      RMASS(22)=dble(am)
      RCHARG(22)=-1.0D0
      CTSM=4.44
C *** S+ BAR ***
      call idmass(ighenex(23),am)
      RMASS(23)=-dble(am)
      RCHARG(23)=-1.0D0
C *** S0 BAR ***
      call idmass(ighenex(24),am)
      RMASS(24)=-dble(am)
      RCHARG(24)=0.0D0
C *** S- BAR ***
      call idmass(ighenex(25),am)
      RMASS(25)=-dble(am)
      RCHARG(25)=1.0D0
C *** XI0 ***
      call idmass(ighenex(26),am)
      RMASS(26)=dble(am)
      RCHARG(26)=0.0D0
      CTX0=8.69
C *** XI- ***
      call idmass(ighenex(27),am)
      RMASS(27)=dble(am)
      RCHARG(27)=-1.0D0
      CTXM=4.92
C *** XI0 BAR ***
      call idmass(ighenex(28),am)
      RMASS(28)=-dble(am)
      RCHARG(28)=0.0D0
      CTX0=8.69
C *** XI- BAR ***
      call idmass(ighenex(29),am)
      RMASS(29)=-dble(am)
      RCHARG(29)=1.0D0
C *** DEUTERON ***
      call idmass(ighenex(30),am)
      RMASS(30)=dble(am)
      RCHARG(30)=1.0D0
C *** TRITON ***
      call idmass(ighenex(31),am)
      RMASS(31)=dble(am)
      RCHARG(31)=1.0D0
C *** ALPHA ***
      call idmass(ighenex(32),am)
      RMASS(32)=dble(am)
      RCHARG(32)=2.0D0
C *** OMEGA- ***
      call idmass(ighenex(33),am)
      RMASS(33)=dble(am)
      RCHARG(33)=-1.0D0
C *** OMEGA- BAR ***
      call idmass(ighenex(34),am)
      RMASS(34)=-dble(am)
      RCHARG(34)=1.0D0
C *** NEW PARTICLE (GEANTINO) ***
      RMASS(35)=0.0D0
      RCHARG(35)=0.0D0

      IF (NPRT(4))
     $ WRITE(MDEBUG,1000) (I,RMASS(I),RCHARG(I),I=1,33),
     $            CT,CTKCH,CTK0,CTL0,CTSP,CTSM,CTX0,CTXM
 1000 FORMAT(' *CGHINI* === GHEISHA PARTICLE PROPERTIES ==='/
     $ '0INDEX',5X,'MASS (GEV)',5X,'CHARGE'/1H /
     $ 33(1H ,1X,I3,5X,F11.6,6X,F5.2/),
     $ '0PI +-  CT = ',G12.5,' K  +-  CT = ',G12.5/
     $ ' K0     CT = ',G12.5,' L0     CT = ',G12.5/
     $ ' S+     CT = ',G12.5,' S-     CT = ',G12.5/
     $ ' X0     CT = ',G12.5,' X-     CT = ',G12.5)

      MP=RMASS(14)
      MPI=RMASS(7)
      MMU=RMASS(5)
      MEL=RMASS(3)
      MKCH=RMASS(10)
      MK0=RMASS(11)
      SMP=MP**2
      SMPI=MPI**2
      SMU=MMU**2
      ML0=RMASS(18)
      MSP=RMASS(20)
      MS0=RMASS(21)
      MSM=RMASS(22)
      MX0=RMASS(26)
      MXM=RMASS(27)

C --- LOAD LIMITS FOR INTRINSIC FUNCTION ARGUMENTS ---
      EXPXL = -82.0D0
      EXPXU =  82.0D0

      IF (NPRT(10)) WRITE(MDEBUG,1001) EXPXL,EXPXU
 1001 FORMAT(' *GHEINI* === INTRINSIC FUNCTION BOUNDARIES ==='/
     $ ' EXPXL,EXPXU = ',2(G12.5,1X))

      RETURN
      END

*CMZ :          25/04/2003 by  T. Pierog IK FZK KARLSRUHE
*-- Author :    The CONEX development group   25/04/2003
C=======================================================================

      FUNCTION GHESIG( PPART,EKIN,KPART,ITARG,IATARG,IZTARG )

C-----------------------------------------------------------------------
C  GH(EISHA) SIG(MA)
C
C  CALCULATION OF THE PROBABILITIES FOR (IN)ELASTIC INTERACTIONS ***
C  THIS FUNCTION IS CALLED FROM BOX2
C  ARGUMENTS:
C   PPART  = R*4 PARTICLE MOMENTUM (GEV)
C   EKIN   = R*4 KINETIC ENERGY (GEV)
C   KPART  = INCIDENT PARTICLE TYPE
C   IATAR  = TARGET PARTICLE MASS (A)
C   IZTAR  = TARGET PARTICLE CHARGE (Z)
C
C  ORIGIN  : F.CARMINATI, H.FESEFELDT (SUBROUT. GHESIG)
C  REDESIGN FOR CORSIKA : P. GABRIEL IK1  FZK KARLSRUHE
C  REDESIGN FOR epos : T. PIEROG IK3  FZK KARLSRUHE
C-----------------------------------------------------------------------
C  *** KPART DENOTES THE GHEISHA PARTICLE INDEX ***
C
C  CONVENTION :
C
C   PARTICLE                 IPART
C   ------------------------------
C   GAMMA                    1
C   NEUTRINO                 2
C   POSITRON                 3
C   ELECTRON                 4
C   MUON +                   5
C   MUON -                   6
C   PION +                   7
C   PION 0                   8
C   PION -                   9
C   KAON +                  10
C   KAON 0 S  (= K(0))      11
C   KAON 0 L  (= K(0) BAR)  12
C   KAON -                  13
C   PROTON                  14
C   PROTON BAR              15
C   NEUTRON                 16
C   NEUTRON BAR             17
C   LAMBDA                  18
C   LAMBDA BAR              19
C   SIGMA +                 20
C   SIGMA 0                 21
C   SIGMA -                 22
C   SIGMA + BAR             23
C   SIGMA 0 BAR             24
C   SIGMA - BAR             25
C   XSI 0                   26
C   XSI -                   27
C   XSI 0 BAR               28
C   XSI - BAR               29
C   DEUTERON                30
C   TRITON                  31
C   ALPHA                   32
C   OMEGA -                 33
C   OMEGA - BAR             34
C   NEW PARTICLES           35
C
C-----------------------------------------------------------------------

      IMPLICIT DOUBLE PRECISION(A-H, O-Z)
      real atnxs,ztnxs,airznxs,airanxs,airwnxs,ghesig
     &             ,airavznxs,airavanxs,ppart,ekin
      common/ghe_nxs_targ/atnxs,ztnxs
      common/nxsair/airznxs(3),airanxs(3),airwnxs(3)
     &             ,airavznxs,airavanxs
c$$$*KEEP,CGCOMP.
      PARAMETER (KKK=3)
      COMMON /CGCOMP/ ACOMP,ZCOMP,WCOMP,KK
      REAL           ACOMP(KKK),ZCOMP(KKK),WCOMP(KKK)
*KEEP,RUNPAR.
      COMMON /CRRUNPAR/DEBUG,MDEBUG
      LOGICAL          DEBUG
      INTEGER          MDEBUG
c$$$      COMMON /GHECRRUNPAR/FIXHEI,THICK0,HILOECM,HILOELB,SIG1I,TARG1I,
c$$$     *                 STEPFC,NRRUN,NSHOW,MPATAP,MONIIN,
c$$$     *                 MONIOU,MDEBUG,NUCNUC,MTABOUT,MLONGOUT,
c$$$     *                 ISHOWNO,ISHW,NOPART,NRECS,NBLKS,MAXPRT,NDEBDL,
c$$$     *                 N1STTR,MDBASE,
c$$$     *                 DEBDEL,DEBUG,FDECAY,FEGS,FIRSTI,FIXINC,FIXTAR,
c$$$     *                 FIX1I,FMUADD,FNKG,FPRINT,FDBASE,FPAROUT,FTABOUT,
c$$$     *                 FLONGOUT,GHEISH,GHESIG,GHEISDB,USELOW,TMARGIN
c$$$      COMMON /GHECRRUNPAC/DSN,DSNTAB,DSNLONG,HOST,USER
c$$$      DOUBLE PRECISION FIXHEI,THICK0,HILOECM,HILOELB,SIG1I,TARG1I,STEPFC
c$$$      INTEGER          NRRUN,NSHOW,MPATAP,MONIIN,MONIOU,MDEBUG,NUCNUC,
c$$$     *                 ISHOWNO,ISHW,NOPART,NRECS,NBLKS,MAXPRT,NDEBDL,
c$$$     *                 N1STTR,MDBASE,MTABOUT,MLONGOUT
c$$$      CHARACTER*79     DSN,DSNTAB,DSNLONG
c$$$      CHARACTER*20     HOST,USER
c$$$      LOGICAL          DEBDEL,DEBUG,FDECAY,FEGS,FIRSTI,FIXINC,FIXTAR,
c$$$     *                 FIX1I,FMUADD,FNKG,FPRINT,FDBASE,FPAROUT,FTABOUT,
c$$$     *                 FLONGOUT,GHEISH,GHESIG,GHEISDB,USELOW,TMARGIN
*KEND.

      COMMON/GSECTI/ AIEL(20),AIIN(20),AIFI(20),AICA(20),ALAM,K0FLAG
      DOUBLE PRECISION AIEL,AIIN,AIFI,AICA,ALAM
      INTEGER K0FLAG

C --- GHEISHA COMMONS ---
      COMMON /RESULT/XEND,YEND,ZEND,RCA,RCE,AMAS,NCH,TOF,PX,PY,PZ,
     *              USERW,INTCT,P,EN,EK,AMASQ,DELTN,ITK,NTK,IPART,IND,
     *              LCALO,ICEL,SINL,COSL,SINP,COSP,
     *              XOLD,YOLD,ZOLD,POLD,PXOLD,PYOLD,PZOLD,
     *              XSCAT,YSCAT,ZSCAT,PSCAT,PXSCAT,PYSCAT,PZSCAT
      DOUBLE PRECISION NCH,INTCT

      COMMON /PRNTFL/INBCD,NEWBCD,INBIN,NEWBIN,NPEVT,NEVTP,LPRT
     *,NPRT(10)
                    LOGICAL LPRT,NPRT


      DIMENSION ALPHA(35),ALPHAC(41),IPART2(7),CSA(4)
      DIMENSION PARTEL(35),PARTIN(35),INTRC(35)
*     DIMENSION ICORR(35)

C --- DIMENSION STATEMENTS FOR CROSS-SECTION DATA ---
      DIMENSION PLAB(41),CSEL(35,41),CSIN(35,41),CSPIEL(3,41),
     $          CSPIIN(3,41),CSPNEL(3,41),CSPNIN(3,41),
     $          ELAB(17),CNLWAT(15),CNLWEL(15,17),CNLWIN(15,17),
     $          CSCAP(100)

C --- DIMENSION STMTS. FOR GEANT/GHEISHA PARTICLE CODE CONVERSIONS ---
C --- KIPART(I)=GHEISHA CODE CORRESPONDING TO GEANT   CODE I ---
C --- IKPART(I)=GEANT   CODE CORRESPONDING TO GHEISHA CODE I ---

c$$$      DIMENSION KIPART(48)
*     DIMENSION IKPART(35)

      SAVE

C --- CROSS-SECTION DATA BY "PCSDAT" 01-FEB-1989 ---
      DATA PLAB /
     $  0.00000D0  , 0.10000D0  , 0.15000D0  , 0.20000D0  , 0.25000D0  ,
     $  0.30000D0  , 0.35000D0  , 0.40000D0  , 0.45000D0  , 0.50000D0  ,
     $  0.55000D0  , 0.60000D0  , 0.65000D0  , 0.70000D0  , 0.75000D0  ,
     $  0.80000D0  , 0.85000D0  , 0.90000D0  , 0.95000D0  ,  1.0000D0  ,
     $   1.1000D0  ,  1.2000D0  ,  1.3000D0  ,  1.4000D0  ,  1.5000D0  ,
     $   1.6000D0  ,  1.8000D0  ,  2.0000D0  ,  2.2000D0  ,  2.4000D0  ,
     $   2.6000D0  ,  2.8000D0  ,  3.0000D0  ,  4.0000D0  ,  5.0000D0  ,
     $   6.0000D0  ,  8.0000D0  ,  10.000D0  ,  20.000D0  ,  100.00D0  ,
     $   1000.0D0  /

C  ELASTIC SCATTERING CROSS-SECTIONS ON FREE PROTONS
C  GAMMA, NEUTRINO, POSITRON, ELECTRON, MU(+), MU(-)
      DATA ((CSEL(I,J),I=1,6),J=1,41) / 246 * 0.D0 /
C  PI(0)
      DATA (CSEL( 8,J),J=1,41) / 41 * 0.D0 /
C  SIGMA(0)
      DATA (CSEL(21,J),J=1,41) / 41 * 0.D0 /
C  SIGMA(0)_BAR
      DATA (CSEL(24,J),J=1,41) / 41 * 0.D0 /
C  DEUTERIUM, TRITIUM, ALPHA
      DATA ((CSEL(I,J),I=30,32),J=1,41) / 123 * 0.D0 /
C  NEW PARTICLES
      DATA (CSEL(35,J),J=1,41) / 41 * 0.D0 /
C  PI(+)
      DATA (CSEL( 7,J),J=1,41) /
     $  0.00000D0  ,  6.0000D0  ,  20.000D0  ,  71.000D0  ,  155.00D0  ,
     $   195.00D0  ,  130.00D0  ,  78.000D0  ,  60.000D0  ,  32.000D0  ,
     $   23.500D0  ,  18.500D0  ,  15.000D0  ,  12.500D0  ,  10.000D0  ,
     $   9.1000D0  ,  8.6000D0  ,  8.8000D0  ,  9.5000D0  ,  10.600D0  ,
     $   13.000D0  ,  15.500D0  ,  17.100D0  ,  17.200D0  ,  16.200D0  ,
     $   15.000D0  ,  12.300D0  ,  10.200D0  ,  9.0000D0  ,  8.0000D0  ,
     $   7.3000D0  ,  6.8000D0  ,  6.5000D0  ,  5.8000D0  ,  5.4000D0  ,
     $   5.2000D0  ,  5.0000D0  ,  4.9000D0  ,  3.8000D0  ,  3.2000D0  ,
     $   3.5000D0  /
C  PI(-)
      DATA (CSEL( 9,J),J=1,41) /
     $  0.00000D0  ,  1.0000D0  ,  3.0000D0  ,  8.0000D0  ,  18.000D0  ,
     $   25.000D0  ,  27.500D0  ,  12.300D0  ,  10.600D0  ,  11.000D0  ,
     $   12.500D0  ,  14.500D0  ,  17.000D0  ,  19.400D0  ,  19.800D0  ,
     $   16.800D0  ,  14.000D0  ,  14.800D0  ,  20.000D0  ,  26.100D0  ,
     $   19.500D0  ,  15.000D0  ,  12.800D0  ,  11.500D0  ,  10.500D0  ,
     $   9.8000D0  ,  8.8000D0  ,  8.2000D0  ,  7.8000D0  ,  7.5000D0  ,
     $   7.2000D0  ,  7.0000D0  ,  6.8000D0  ,  6.1000D0  ,  5.7000D0  ,
     $   5.4000D0  ,  4.9000D0  ,  4.6000D0  ,  4.0000D0  ,  3.3000D0  ,
     $   3.5000D0  /
C  K(+)
      DATA (CSEL(10,J),J=1,41) /
     $   10.000D0  ,  11.200D0  ,  11.300D0  ,  11.400D0  ,  11.500D0  ,
     $   11.600D0  ,  11.800D0  ,  12.000D0  ,  12.100D0  ,  12.200D0  ,
     $   12.300D0  ,  12.400D0  ,  12.500D0  ,  12.500D0  ,  12.500D0  ,
     $   12.400D0  ,  12.300D0  ,  12.200D0  ,  12.000D0  ,  11.800D0  ,
     $   11.200D0  ,  11.500D0  ,  9.9000D0  ,  9.4000D0  ,  8.8000D0  ,
     $   8.4000D0  ,  7.5000D0  ,  6.9000D0  ,  6.3000D0  ,  5.9000D0  ,
     $   5.5000D0  ,  5.2000D0  ,  5.0000D0  ,  4.0000D0  ,  3.5000D0  ,
     $   3.3000D0  ,  3.1000D0  ,  3.1000D0  ,  3.0000D0  ,  2.5000D0  ,
     $   3.0000D0  /
C  K(0) SHORT  (= K(0))
      DATA (CSEL(11,J),J=1,41) /
     $   10.000D0  ,  11.200D0  ,  11.300D0  ,  11.400D0  ,  11.500D0  ,
     $   11.600D0  ,  11.800D0  ,  12.000D0  ,  12.100D0  ,  12.200D0  ,
     $   12.300D0  ,  12.400D0  ,  12.500D0  ,  12.500D0  ,  12.500D0  ,
     $   12.400D0  ,  12.300D0  ,  12.200D0  ,  12.000D0  ,  11.800D0  ,
     $   11.200D0  ,  11.500D0  ,  9.9000D0  ,  9.4000D0  ,  8.8000D0  ,
     $   8.4000D0  ,  7.5000D0  ,  6.9000D0  ,  6.3000D0  ,  5.9000D0  ,
     $   5.5000D0  ,  5.2000D0  ,  5.0000D0  ,  4.0000D0  ,  3.5000D0  ,
     $   3.3000D0  ,  3.1000D0  ,  3.1000D0  ,  3.0000D0  ,  2.5000D0  ,
     $   3.0000D0  /
C  K(0) LONG (= K(0)_BAR)
      DATA (CSEL(12,J),J=1,41) /
     $   160.83D0  ,  82.800D0  ,  58.575D0  ,  43.683D0  ,  34.792D0  ,
     $   28.650D0  ,  24.367D0  ,  20.917D0  ,  18.192D0  ,  16.300D0  ,
     $   14.608D0  ,  13.017D0  ,  12.250D0  ,  11.700D0  ,  12.017D0  ,
     $   14.075D0  ,  15.842D0  ,  16.433D0  ,  16.042D0  ,  15.008D0  ,
     $   12.575D0  ,  10.708D0  ,  9.2000D0  ,  8.0167D0  ,  7.2833D0  ,
     $   7.0750D0  ,  6.6333D0  ,  6.1250D0  ,  5.6583D0  ,  5.2750D0  ,
     $   4.9333D0  ,  4.6250D0  ,  4.4583D0  ,  3.7333D0  ,  3.3833D0  ,
     $   3.1833D0  ,  2.9833D0  ,  2.7500D0  ,  2.3667D0  ,  2.2000D0  ,
     $   2.6000D0  /
C  K(-)
      DATA (CSEL(13,J),J=1,41) /
     $   300.00D0  ,  140.00D0  ,  97.000D0  ,  70.000D0  ,  55.000D0  ,
     $   45.000D0  ,  37.000D0  ,  31.000D0  ,  26.000D0  ,  23.000D0  ,
     $   20.000D0  ,  17.000D0  ,  15.500D0  ,  14.500D0  ,  14.700D0  ,
     $   18.500D0  ,  22.000D0  ,  23.000D0  ,  22.500D0  ,  20.700D0  ,
     $   16.500D0  ,  14.000D0  ,  11.500D0  ,  9.6000D0  ,  8.6000D0  ,
     $   8.5000D0  ,  8.3000D0  ,  7.6000D0  ,  7.0000D0  ,  6.4000D0  ,
     $   5.9000D0  ,  5.5000D0  ,  5.3000D0  ,  4.4000D0  ,  4.1000D0  ,
     $   3.9000D0  ,  3.7000D0  ,  3.3000D0  ,  2.6000D0  ,  2.5000D0  ,
     $   3.0000D0  /
C  PROTON
      DATA (CSEL(14,J),J=1,41) /
     $   1100.0D0  ,  115.00D0  ,  105.00D0  ,  100.00D0  ,  56.000D0  ,
     $   40.000D0  ,  27.000D0  ,  22.000D0  ,  21.000D0  ,  20.000D0  ,
     $   20.000D0  ,  20.000D0  ,  20.500D0  ,  21.000D0  ,  22.000D0  ,
     $   23.000D0  ,  24.000D0  ,  24.000D0  ,  24.400D0  ,  24.500D0  ,
     $   25.000D0  ,  25.500D0  ,  26.000D0  ,  26.500D0  ,  27.000D0  ,
     $   27.000D0  ,  26.000D0  ,  23.000D0  ,  21.500D0  ,  20.000D0  ,
     $   19.000D0  ,  18.000D0  ,  17.000D0  ,  13.000D0  ,  11.500D0  ,
     $   10.300D0  ,  9.4000D0  ,  9.0000D0  ,  8.8000D0  ,  7.0000D0  ,
     $   7.5000D0  /
C  PROTON_BAR
      DATA (CSEL(15,J),J=1,41) /
     $   200.00D0  ,  163.00D0  ,  141.00D0  ,  120.00D0  ,  111.00D0  ,
     $   99.500D0  ,  92.500D0  ,  86.500D0  ,  82.000D0  ,  78.000D0  ,
     $   74.000D0  ,  71.000D0  ,  67.500D0  ,  65.000D0  ,  62.500D0  ,
     $   59.700D0  ,  58.100D0  ,  56.300D0  ,  54.700D0  ,  52.700D0  ,
     $   50.000D0  ,  48.400D0  ,  47.000D0  ,  46.000D0  ,  45.200D0  ,
     $   42.800D0  ,  39.200D0  ,  36.300D0  ,  32.800D0  ,  30.400D0  ,
     $   28.100D0  ,  26.300D0  ,  24.500D0  ,  19.250D0  ,  16.840D0  ,
     $   14.600D0  ,  12.340D0  ,  11.210D0  ,  8.8500D0  ,  7.5000D0  ,
     $   7.5000D0  /
C  NEUTRON
      DATA (CSEL(16,J),J=1,41) /
     $   4200.0D0  ,  440.00D0  ,  420.00D0  ,  400.00D0  ,  230.00D0  ,
     $   160.00D0  ,  105.00D0  ,  80.000D0  ,  62.000D0  ,  50.000D0  ,
     $   45.000D0  ,  41.000D0  ,  38.000D0  ,  36.000D0  ,  35.000D0  ,
     $   34.000D0  ,  33.000D0  ,  32.000D0  ,  31.500D0  ,  31.000D0  ,
     $   30.500D0  ,  30.000D0  ,  29.500D0  ,  29.000D0  ,  28.500D0  ,
     $   28.000D0  ,  26.000D0  ,  23.000D0  ,  21.500D0  ,  20.000D0  ,
     $   19.000D0  ,  18.000D0  ,  17.000D0  ,  13.000D0  ,  11.500D0  ,
     $   10.300D0  ,  9.4000D0  ,  9.0000D0  ,  8.8000D0  ,  7.0000D0  ,
     $   7.5000D0  /
C  NEUTRON_BAR
      DATA (CSEL(17,J),J=1,41) /
     $   185.88D0  ,  133.23D0  ,  119.37D0  ,  102.86D0  ,  93.102D0  ,
     $   82.752D0  ,  76.205D0  ,  71.008D0  ,  67.366D0  ,  64.096D0  ,
     $   60.891D0  ,  58.501D0  ,  55.735D0  ,  53.773D0  ,  51.839D0  ,
     $   49.671D0  ,  48.485D0  ,  47.045D0  ,  45.803D0  ,  44.306D0  ,
     $   42.623D0  ,  41.786D0  ,  41.115D0  ,  40.630D0  ,  40.129D0  ,
     $   38.242D0  ,  35.233D0  ,  32.662D0  ,  29.639D0  ,  27.573D0  ,
     $   25.536D0  ,  23.948D0  ,  22.356D0  ,  17.723D0  ,  15.614D0  ,
     $   13.653D0  ,  11.675D0  ,  10.653D0  ,  8.6198D0  ,  7.4464D0  ,
     $   7.4821D0  /
C  LAMBDA
      DATA (CSEL(18,J),J=1,41) /
     $   1100.0D0  ,  115.00D0  ,  105.00D0  ,  100.00D0  ,  56.000D0  ,
     $   40.000D0  ,  27.000D0  ,  22.000D0  ,  21.000D0  ,  20.000D0  ,
     $   20.000D0  ,  19.067D0  ,  19.333D0  ,  19.500D0  ,  19.833D0  ,
     $   20.567D0  ,  21.800D0  ,  22.900D0  ,  23.869D0  ,  23.809D0  ,
     $   22.161D0  ,  21.488D0  ,  19.732D0  ,  19.433D0  ,  19.345D0  ,
     $   19.029D0  ,  18.121D0  ,  16.280D0  ,  15.258D0  ,  14.280D0  ,
     $   13.644D0  ,  12.963D0  ,  12.316D0  ,  9.5333D0  ,  8.4333D0  ,
     $   7.5728D0  ,  6.9696D0  ,  6.7518D0  ,  6.6175D0  ,  5.6000D0  ,
     $   6.1145D0  /
C  LAMBDA_BAR
      DATA (CSEL(19,J),J=1,41) /
     $   157.65D0  ,  73.701D0  ,  76.096D0  ,  68.571D0  ,  57.305D0  ,
     $   49.257D0  ,  43.616D0  ,  40.024D0  ,  38.098D0  ,  36.287D0  ,
     $   34.674D0  ,  33.105D0  ,  31.712D0  ,  30.685D0  ,  29.613D0  ,
     $   28.602D0  ,  28.336D0  ,  28.075D0  ,  27.786D0  ,  27.215D0  ,
     $   26.380D0  ,  26.146D0  ,  25.108D0  ,  24.783D0  ,  24.360D0  ,
     $   23.219D0  ,  21.431D0  ,  20.095D0  ,  18.382D0  ,  17.267D0  ,
     $   16.100D0  ,  15.175D0  ,  14.271D0  ,  11.573D0  ,  10.305D0  ,
     $   9.1471D0  ,  8.0149D0  ,  7.4349D0  ,  6.2499D0  ,  5.8928D0  ,
     $   6.0774D0  /
C  SIGMA(+)
      DATA (CSEL(20,J),J=1,41) /
     $   1100.0D0  ,  115.00D0  ,  105.00D0  ,  100.00D0  ,  56.000D0  ,
     $   40.000D0  ,  27.000D0  ,  22.000D0  ,  21.000D0  ,  20.000D0  ,
     $   20.000D0  ,  19.067D0  ,  19.333D0  ,  19.500D0  ,  19.833D0  ,
     $   20.567D0  ,  21.800D0  ,  22.900D0  ,  23.869D0  ,  23.809D0  ,
     $   22.161D0  ,  21.488D0  ,  19.732D0  ,  19.433D0  ,  19.345D0  ,
     $   19.029D0  ,  18.121D0  ,  16.280D0  ,  15.258D0  ,  14.280D0  ,
     $   13.644D0  ,  12.963D0  ,  12.316D0  ,  9.5333D0  ,  8.4333D0  ,
     $   7.5728D0  ,  6.9696D0  ,  6.7518D0  ,  6.6175D0  ,  5.6000D0  ,
     $   6.1145D0  /
C  SIGMA(-)
      DATA (CSEL(22,J),J=1,41) /
     $   1100.0D0  ,  115.00D0  ,  105.00D0  ,  100.00D0  ,  56.000D0  ,
     $   40.000D0  ,  27.000D0  ,  22.000D0  ,  21.000D0  ,  20.000D0  ,
     $   20.000D0  ,  19.067D0  ,  19.333D0  ,  19.500D0  ,  19.833D0  ,
     $   20.567D0  ,  21.800D0  ,  22.900D0  ,  23.869D0  ,  23.809D0  ,
     $   22.161D0  ,  21.488D0  ,  19.732D0  ,  19.433D0  ,  19.345D0  ,
     $   19.029D0  ,  18.121D0  ,  16.280D0  ,  15.258D0  ,  14.280D0  ,
     $   13.644D0  ,  12.963D0  ,  12.316D0  ,  9.5333D0  ,  8.4333D0  ,
     $   7.5728D0  ,  6.9696D0  ,  6.7518D0  ,  6.6175D0  ,  5.6000D0  ,
     $   6.1145D0  /
C  SIGMA(+)_BAR
      DATA (CSEL(23,J),J=1,41) /
     $   185.88D0  ,  133.23D0  ,  119.37D0  ,  102.86D0  ,  93.102D0  ,
     $   82.752D0  ,  76.205D0  ,  71.008D0  ,  67.366D0  ,  64.096D0  ,
     $   60.891D0  ,  58.104D0  ,  55.241D0  ,  53.140D0  ,  50.934D0  ,
     $   48.660D0  ,  47.566D0  ,  46.585D0  ,  45.581D0  ,  44.003D0  ,
     $   41.134D0  ,  39.374D0  ,  36.878D0  ,  35.523D0  ,  34.503D0  ,
     $   32.334D0  ,  29.365D0  ,  27.370D0  ,  24.705D0  ,  22.921D0  ,
     $   21.229D0  ,  19.879D0  ,  18.559D0  ,  14.625D0  ,  12.758D0  ,
     $   11.041D0  ,  9.3440D0  ,  8.5484D0  ,  6.7104D0  ,  6.0000D0  ,
     $   6.1131D0  /
C  SIGMA(-)_BAR
      DATA (CSEL(25,J),J=1,41) /
     $   157.65D0  ,  73.701D0  ,  76.096D0  ,  68.571D0  ,  57.305D0  ,
     $   49.257D0  ,  43.616D0  ,  40.024D0  ,  38.098D0  ,  36.287D0  ,
     $   34.674D0  ,  33.105D0  ,  31.712D0  ,  30.685D0  ,  29.613D0  ,
     $   28.602D0  ,  28.336D0  ,  28.075D0  ,  27.786D0  ,  27.215D0  ,
     $   26.380D0  ,  26.146D0  ,  25.108D0  ,  24.783D0  ,  24.360D0  ,
     $   23.219D0  ,  21.431D0  ,  20.095D0  ,  18.382D0  ,  17.267D0  ,
     $   16.100D0  ,  15.175D0  ,  14.271D0  ,  11.573D0  ,  10.305D0  ,
     $   9.1471D0  ,  8.0149D0  ,  7.4349D0  ,  6.2499D0  ,  5.8928D0  ,
     $   6.0774D0  /
C  XI(0)
      DATA (CSEL(26,J),J=1,41) /
     $   1100.0D0  ,  115.00D0  ,  105.00D0  ,  100.00D0  ,  56.000D0  ,
     $   40.000D0  ,  27.000D0  ,  22.000D0  ,  21.000D0  ,  20.000D0  ,
     $   20.000D0  ,  18.133D0  ,  18.167D0  ,  18.000D0  ,  17.667D0  ,
     $   18.133D0  ,  19.600D0  ,  21.800D0  ,  23.338D0  ,  23.118D0  ,
     $   19.323D0  ,  17.476D0  ,  13.464D0  ,  12.367D0  ,  11.691D0  ,
     $   11.057D0  ,  10.242D0  ,  9.5593D0  ,  9.0151D0  ,  8.5591D0  ,
     $   8.2884D0  ,  7.9253D0  ,  7.6311D0  ,  6.0667D0  ,  5.3667D0  ,
     $   4.8456D0  ,  4.5392D0  ,  4.5036D0  ,  4.4351D0  ,  4.2000D0  ,
     $   4.7289D0  /
C  XI(-)
      DATA (CSEL(27,J),J=1,41) /
     $   1100.0D0  ,  115.00D0  ,  105.00D0  ,  100.00D0  ,  56.000D0  ,
     $   40.000D0  ,  27.000D0  ,  22.000D0  ,  21.000D0  ,  20.000D0  ,
     $   20.000D0  ,  18.133D0  ,  18.167D0  ,  18.000D0  ,  17.667D0  ,
     $   18.133D0  ,  19.600D0  ,  21.800D0  ,  23.338D0  ,  23.118D0  ,
     $   19.323D0  ,  17.476D0  ,  13.464D0  ,  12.367D0  ,  11.691D0  ,
     $   11.057D0  ,  10.242D0  ,  9.5593D0  ,  9.0151D0  ,  8.5591D0  ,
     $   8.2884D0  ,  7.9253D0  ,  7.6311D0  ,  6.0667D0  ,  5.3667D0  ,
     $   4.8456D0  ,  4.5392D0  ,  4.5036D0  ,  4.4351D0  ,  4.2000D0  ,
     $   4.7289D0  /
C  XI(0)_BAR
      DATA (CSEL(28,J),J=1,41) /
     $   157.65D0  ,  73.701D0  ,  76.096D0  ,  68.571D0  ,  57.305D0  ,
     $   49.257D0  ,  43.616D0  ,  40.024D0  ,  38.098D0  ,  36.287D0  ,
     $   34.674D0  ,  32.708D0  ,  31.218D0  ,  30.052D0  ,  28.707D0  ,
     $   27.591D0  ,  27.417D0  ,  27.615D0  ,  27.564D0  ,  26.913D0  ,
     $   24.891D0  ,  23.734D0  ,  20.871D0  ,  19.677D0  ,  18.734D0  ,
     $   17.311D0  ,  15.563D0  ,  14.803D0  ,  13.448D0  ,  12.615D0  ,
     $   11.794D0  ,  11.106D0  ,  10.474D0  ,  8.4745D0  ,  7.4498D0  ,
     $   6.5350D0  ,  5.6835D0  ,  5.3300D0  ,  4.3406D0  ,  4.4464D0  ,
     $   4.7083D0  /
C  XI(-)_BAR
      DATA (CSEL(29,J),J=1,41) /
     $   143.53D0  ,  43.935D0  ,  54.462D0  ,  51.429D0  ,  39.407D0  ,
     $   32.510D0  ,  27.321D0  ,  24.532D0  ,  23.465D0  ,  22.383D0  ,
     $   21.566D0  ,  20.209D0  ,  19.453D0  ,  18.825D0  ,  18.046D0  ,
     $   17.562D0  ,  17.802D0  ,  18.360D0  ,  18.667D0  ,  18.519D0  ,
     $   17.514D0  ,  17.120D0  ,  14.985D0  ,  14.306D0  ,  13.663D0  ,
     $   12.753D0  ,  11.596D0  ,  11.165D0  ,  10.287D0  ,  9.7882D0  ,
     $   9.2294D0  ,  8.7539D0  ,  8.3300D0  ,  6.9480D0  ,  6.2234D0  ,
     $   5.5881D0  ,  5.0189D0  ,  4.7733D0  ,  4.1104D0  ,  4.3929D0  ,
     $   4.6905D0  /
C  OMEGA(-)
      DATA (CSEL(33,J),J=1,41) /
     $   1100.0D0  ,  115.00D0  ,  105.00D0  ,  100.00D0  ,  56.000D0  ,
     $   40.000D0  ,  27.000D0  ,  22.000D0  ,  21.000D0  ,  20.000D0  ,
     $   20.000D0  ,  18.133D0  ,  18.167D0  ,  18.000D0  ,  17.667D0  ,
     $   18.133D0  ,  19.600D0  ,  21.800D0  ,  23.338D0  ,  23.118D0  ,
     $   19.323D0  ,  17.476D0  ,  13.464D0  ,  12.367D0  ,  11.691D0  ,
     $   11.057D0  ,  10.242D0  ,  9.5593D0  ,  9.0151D0  ,  8.5591D0  ,
     $   8.2884D0  ,  7.9253D0  ,  7.6311D0  ,  6.0667D0  ,  5.3667D0  ,
     $   4.8456D0  ,  4.5392D0  ,  4.5036D0  ,  4.4351D0  ,  4.2000D0  ,
     $   4.7289D0  /
C  OMEGA(-)_BAR
      DATA (CSEL(34,J),J=1,41) /
     $   143.53D0  ,  43.935D0  ,  54.462D0  ,  51.429D0  ,  39.407D0  ,
     $   32.510D0  ,  27.321D0  ,  24.532D0  ,  23.465D0  ,  22.383D0  ,
     $   21.566D0  ,  20.209D0  ,  19.453D0  ,  18.825D0  ,  18.046D0  ,
     $   17.562D0  ,  17.802D0  ,  18.360D0  ,  18.667D0  ,  18.519D0  ,
     $   17.514D0  ,  17.120D0  ,  14.985D0  ,  14.306D0  ,  13.663D0  ,
     $   12.753D0  ,  11.596D0  ,  11.165D0  ,  10.287D0  ,  9.7882D0  ,
     $   9.2294D0  ,  8.7539D0  ,  8.3300D0  ,  6.9480D0  ,  6.2234D0  ,
     $   5.5881D0  ,  5.0189D0  ,  4.7733D0  ,  4.1104D0  ,  4.3929D0  ,
     $   4.6905D0  /

C  INELASTIC CROSS-SECTIONS ON FREE PROTONS
C  GAMMA, NEUTRINO, POSITRON, ELECTRON, MU(+), MU(-)
      DATA ((CSIN(I,J),I=1,6),J=1,41) / 246 * 0.D0 /
C  PI(0)
      DATA (CSIN( 8,J),J=1,41) / 41 * 0.D0 /
C  SIGMA(0)
      DATA (CSIN(21,J),J=1,41) / 41 * 0.D0 /
C  SIGMA(0)_BAR
      DATA (CSIN(24,J),J=1,41) / 41 * 0.D0 /
C  DEUTERIUM, TRITIUM, ALPHA
      DATA ((CSIN(I,J),I=30,32),J=1,41) / 123 * 0.D0 /
C  NEW PARTICLES
      DATA (CSIN(35,J),J=1,41) / 41 * 0.D0 /
C  PI(+)
      DATA (CSIN( 7,J),J=1,41) /
     $  0.00000D0  , 0.00000D0  , 0.00000D0  , 0.00000D0  , 0.00000D0  ,
     $  0.00000D0  , 0.00000D0  , 0.50000D0  ,  1.2000D0  ,  1.7000D0  ,
     $   2.2500D0  ,  3.0000D0  ,  3.6000D0  ,  4.5000D0  ,  5.4000D0  ,
     $   6.3000D0  ,  8.6000D0  ,  9.0000D0  ,  10.000D0  ,  11.500D0  ,
     $   14.000D0  ,  17.000D0  ,  19.500D0  ,  22.000D0  ,  24.000D0  ,
     $   21.500D0  ,  18.500D0  ,  19.000D0  ,  20.500D0  ,  22.200D0  ,
     $   23.000D0  ,  23.300D0  ,  23.000D0  ,  21.000D0  ,  20.500D0  ,
     $   20.200D0  ,  20.100D0  ,  20.000D0  ,  20.000D0  ,  20.000D0  ,
     $   21.000D0  /
C  PI(-)
      DATA (CSIN( 9,J),J=1,41) /
     $  0.00000D0  ,  3.0000D0  ,  9.2000D0  ,  20.500D0  ,  36.500D0  ,
     $   45.000D0  ,  28.000D0  ,  19.500D0  ,  15.500D0  ,  14.200D0  ,
     $   15.500D0  ,  17.500D0  ,  20.000D0  ,  23.000D0  ,  26.000D0  ,
     $   20.000D0  ,  23.000D0  ,  26.500D0  ,  32.000D0  ,  35.000D0  ,
     $   28.500D0  ,  22.000D0  ,  22.500D0  ,  23.500D0  ,  24.000D0  ,
     $   24.500D0  ,  26.000D0  ,  27.500D0  ,  27.500D0  ,  27.000D0  ,
     $   26.500D0  ,  25.500D0  ,  25.000D0  ,  23.000D0  ,  22.500D0  ,
     $   22.200D0  ,  22.000D0  ,  22.000D0  ,  21.200D0  ,  20.700D0  ,
     $   21.000D0  /
C  K(+)
      DATA (CSIN(10,J),J=1,41) /
     $  0.00000D0  , 0.00000D0  , 0.00000D0  , 0.00000D0  , 0.00000D0  ,
     $  0.00000D0  , 0.00000D0  , 0.00000D0  , 0.00000D0  , 0.00000D0  ,
     $  0.00000D0  , 0.00000D0  , 0.00000D0  , 0.00000D0  , 0.00000D0  ,
     $  0.50000D0  ,  1.5000D0  ,  2.7000D0  ,  3.8000D0  ,  4.8000D0  ,
     $   6.5000D0  ,  7.6000D0  ,  8.4000D0  ,  9.0000D0  ,  9.4000D0  ,
     $   9.8000D0  ,  10.500D0  ,  11.000D0  ,  11.500D0  ,  11.800D0  ,
     $   12.200D0  ,  12.400D0  ,  12.600D0  ,  13.200D0  ,  13.500D0  ,
     $   13.700D0  ,  14.000D0  ,  14.200D0  ,  14.500D0  ,  16.400D0  ,
     $   17.000D0  /
C  K(0) SHORT  (= K(0))
      DATA (CSIN(11,J),J=1,41) /
     $  0.00000D0  , 0.00000D0  , 0.00000D0  , 0.00000D0  , 0.00000D0  ,
     $  0.00000D0  , 0.00000D0  , 0.00000D0  , 0.00000D0  , 0.00000D0  ,
     $  0.00000D0  , 0.00000D0  , 0.00000D0  , 0.00000D0  , 0.00000D0  ,
     $  0.50000D0  ,  1.5000D0  ,  2.7000D0  ,  3.8000D0  ,  4.8000D0  ,
     $   6.5000D0  ,  7.6000D0  ,  8.4000D0  ,  9.0000D0  ,  9.4000D0  ,
     $   9.8000D0  ,  10.500D0  ,  11.000D0  ,  11.500D0  ,  11.800D0  ,
     $   12.200D0  ,  12.400D0  ,  12.600D0  ,  13.200D0  ,  13.500D0  ,
     $   13.700D0  ,  14.000D0  ,  14.200D0  ,  14.500D0  ,  16.400D0  ,
     $   17.000D0  /
C  K(0) LONG  (=  K(0)_BAR)
      DATA (CSIN(12,J),J=1,41) /
     $   266.67D0  ,  133.33D0  ,  83.333D0  ,  57.083D0  ,  44.500D0  ,
     $   33.250D0  ,  24.583D0  ,  20.833D0  ,  18.333D0  ,  16.083D0  ,
     $   15.625D0  ,  15.083D0  ,  14.833D0  ,  15.083D0  ,  15.833D0  ,
     $   17.042D0  ,  18.958D0  ,  20.758D0  ,  22.533D0  ,  22.825D0  ,
     $   21.250D0  ,  18.567D0  ,  17.767D0  ,  18.100D0  ,  19.933D0  ,
     $   20.783D0  ,  21.225D0  ,  21.000D0  ,  20.558D0  ,  20.258D0  ,
     $   20.017D0  ,  19.767D0  ,  19.600D0  ,  19.183D0  ,  18.850D0  ,
     $   18.575D0  ,  18.350D0  ,  18.175D0  ,  17.808D0  ,  17.558D0  ,
     $   19.250D0  /
C  K(-)
      DATA (CSIN(13,J),J=1,41) /
     $   400.00D0  ,  200.00D0  ,  120.00D0  ,  81.000D0  ,  62.000D0  ,
     $   47.000D0  ,  35.000D0  ,  28.000D0  ,  24.000D0  ,  21.000D0  ,
     $   19.500D0  ,  19.000D0  ,  18.800D0  ,  19.000D0  ,  20.000D0  ,
     $   21.000D0  ,  23.000D0  ,  25.000D0  ,  27.000D0  ,  27.500D0  ,
     $   25.500D0  ,  22.000D0  ,  20.800D0  ,  21.000D0  ,  23.000D0  ,
     $   24.000D0  ,  24.000D0  ,  23.800D0  ,  23.000D0  ,  22.500D0  ,
     $   22.000D0  ,  21.600D0  ,  21.400D0  ,  21.000D0  ,  20.500D0  ,
     $   20.200D0  ,  19.800D0  ,  19.500D0  ,  18.600D0  ,  17.500D0  ,
     $   20.000D0  /
C  PROTON
      DATA (CSIN(14,J),J=1,41) /
     $  0.00000D0  , 0.00000D0  , 0.00000D0  , 0.00000D0  , 0.00000D0  ,
     $  0.00000D0  , 0.00000D0  , 0.00000D0  , 0.00000D0  , 0.00000D0  ,
     $  0.00000D0  , 0.00000D0  , 0.00000D0  , 0.00000D0  , 0.00000D0  ,
     $  0.00000D0  , 0.00000D0  , 0.00000D0  , 0.10000D0  ,  1.5000D0  ,
     $   7.0000D0  ,  12.000D0  ,  17.000D0  ,  19.500D0  ,  20.500D0  ,
     $   22.000D0  ,  23.500D0  ,  24.800D0  ,  25.800D0  ,  26.500D0  ,
     $   27.000D0  ,  27.500D0  ,  28.000D0  ,  30.000D0  ,  31.000D0  ,
     $   32.000D0  ,  32.500D0  ,  32.500D0  ,  33.000D0  ,  33.500D0  ,
     $   34.000D0  /
C  PROTON_BAR
      DATA (CSIN(15,J),J=1,41) /
     $   1500.0D0  ,  1160.0D0  ,  310.00D0  ,  230.00D0  ,  178.00D0  ,
     $   153.00D0  ,  134.00D0  ,  124.00D0  ,  113.00D0  ,  106.00D0  ,
     $   101.00D0  ,  96.000D0  ,  92.000D0  ,  89.000D0  ,  87.000D0  ,
     $   84.000D0  ,  81.000D0  ,  78.500D0  ,  76.500D0  ,  75.000D0  ,
     $   72.000D0  ,  70.000D0  ,  68.000D0  ,  64.500D0  ,  63.000D0  ,
     $   62.000D0  ,  61.000D0  ,  59.500D0  ,  58.500D0  ,  56.500D0  ,
     $   56.500D0  ,  56.000D0  ,  55.500D0  ,  52.000D0  ,  50.000D0  ,
     $   48.000D0  ,  45.000D0  ,  44.000D0  ,  39.200D0  ,  34.500D0  ,
     $   34.500D0  /
C  NEUTRON
      DATA (CSIN(16,J),J=1,41) /
     $  0.00000D0  , 0.00000D0  , 0.00000D0  , 0.00000D0  , 0.00000D0  ,
     $  0.00000D0  , 0.00000D0  , 0.00000D0  , 0.00000D0  , 0.00000D0  ,
     $  0.00000D0  , 0.00000D0  , 0.00000D0  , 0.00000D0  , 0.00000D0  ,
     $  0.00000D0  , 0.00000D0  , 0.00000D0  , 0.10000D0  ,  1.5000D0  ,
     $   7.0000D0  ,  12.000D0  ,  17.000D0  ,  19.500D0  ,  20.500D0  ,
     $   22.000D0  ,  23.500D0  ,  24.800D0  ,  25.800D0  ,  26.500D0  ,
     $   27.000D0  ,  27.500D0  ,  28.000D0  ,  30.000D0  ,  31.000D0  ,
     $   32.000D0  ,  32.500D0  ,  32.500D0  ,  33.000D0  ,  33.500D0  ,
     $   34.000D0  /
C  NEUTRON_BAR
      DATA (CSIN(17,J),J=1,41) /
     $   1394.1D0  ,  948.17D0  ,  262.43D0  ,  197.14D0  ,  149.30D0  ,
     $   127.25D0  ,  110.39D0  ,  101.79D0  ,  92.834D0  ,  87.104D0  ,
     $   83.109D0  ,  79.099D0  ,  75.965D0  ,  73.627D0  ,  72.161D0  ,
     $   69.889D0  ,  67.595D0  ,  65.595D0  ,  64.057D0  ,  63.054D0  ,
     $   61.377D0  ,  60.434D0  ,  59.485D0  ,  56.970D0  ,  55.931D0  ,
     $   55.398D0  ,  54.827D0  ,  53.538D0  ,  52.861D0  ,  51.247D0  ,
     $   51.344D0  ,  50.992D0  ,  50.644D0  ,  47.876D0  ,  46.358D0  ,
     $   44.887D0  ,  42.577D0  ,  41.815D0  ,  38.180D0  ,  34.254D0  ,
     $   34.418D0  /
C  LAMBDA
      DATA (CSIN(18,J),J=1,41) /
     $  0.00000D0  , 0.00000D0  , 0.00000D0  , 0.00000D0  , 0.00000D0  ,
     $  0.00000D0  , 0.00000D0  , 0.00000D0  , 0.00000D0  , 0.00000D0  ,
     $  0.00000D0  , 0.00000D0  , 0.00000D0  , 0.00000D0  , 0.00000D0  ,
     $  0.00000D0  , 0.00000D0  , 0.00000D0  , 0.97815D-01,  1.4577D0  ,
     $   6.2052D0  ,  10.112D0  ,  12.902D0  ,  14.300D0  ,  14.688D0  ,
     $   15.505D0  ,  16.379D0  ,  17.554D0  ,  18.309D0  ,  18.920D0  ,
     $   19.389D0  ,  19.804D0  ,  20.284D0  ,  22.000D0  ,  22.733D0  ,
     $   23.527D0  ,  24.097D0  ,  24.382D0  ,  24.816D0  ,  26.800D0  ,
     $   27.719D0  /
C  LAMBDA_BAR
      DATA (CSIN(19,J),J=1,41) /
     $   1182.4D0  ,  524.50D0  ,  167.30D0  ,  131.43D0  ,  91.895D0  ,
     $   75.743D0  ,  63.184D0  ,  57.376D0  ,  52.502D0  ,  49.313D0  ,
     $   47.326D0  ,  44.762D0  ,  43.222D0  ,  42.015D0  ,  41.221D0  ,
     $   40.244D0  ,  39.504D0  ,  39.145D0  ,  38.860D0  ,  38.731D0  ,
     $   37.987D0  ,  37.814D0  ,  36.326D0  ,  34.750D0  ,  33.953D0  ,
     $   33.635D0  ,  33.349D0  ,  32.938D0  ,  32.785D0  ,  32.092D0  ,
     $   32.373D0  ,  32.312D0  ,  32.329D0  ,  31.261D0  ,  30.597D0  ,
     $   30.073D0  ,  29.228D0  ,  29.182D0  ,  27.683D0  ,  27.107D0  ,
     $   27.956D0  /
C  SIGMA(+)
      DATA (CSIN(20,J),J=1,41) /
     $  0.00000D0  , 0.00000D0  , 0.00000D0  , 0.00000D0  , 0.00000D0  ,
     $  0.00000D0  , 0.00000D0  , 0.00000D0  , 0.00000D0  , 0.00000D0  ,
     $  0.00000D0  , 0.00000D0  , 0.00000D0  , 0.00000D0  , 0.00000D0  ,
     $  0.00000D0  , 0.00000D0  , 0.00000D0  , 0.97815D-01,  1.4577D0  ,
     $   6.2052D0  ,  10.112D0  ,  12.902D0  ,  14.300D0  ,  14.688D0  ,
     $   15.505D0  ,  16.379D0  ,  17.554D0  ,  18.309D0  ,  18.920D0  ,
     $   19.389D0  ,  19.804D0  ,  20.284D0  ,  22.000D0  ,  22.733D0  ,
     $   23.527D0  ,  24.097D0  ,  24.382D0  ,  24.816D0  ,  26.800D0  ,
     $   27.719D0  /
C  SIGMA(-)
      DATA (CSIN(22,J),J=1,41) /
     $  0.00000D0  , 0.00000D0  , 0.00000D0  , 0.00000D0  , 0.00000D0  ,
     $  0.00000D0  , 0.00000D0  , 0.00000D0  , 0.00000D0  , 0.00000D0  ,
     $  0.00000D0  , 0.00000D0  , 0.00000D0  , 0.00000D0  , 0.00000D0  ,
     $  0.00000D0  , 0.00000D0  , 0.00000D0  , 0.97815D-01,  1.4577D0  ,
     $   6.2052D0  ,  10.112D0  ,  12.902D0  ,  14.300D0  ,  14.688D0  ,
     $   15.505D0  ,  16.379D0  ,  17.554D0  ,  18.309D0  ,  18.920D0  ,
     $   19.389D0  ,  19.804D0  ,  20.284D0  ,  22.000D0  ,  22.733D0  ,
     $   23.527D0  ,  24.097D0  ,  24.382D0  ,  24.816D0  ,  26.800D0  ,
     $   27.719D0  /
C  SIGMA(+)_BAR
      DATA (CSIN(23,J),J=1,41) /
     $   1394.1D0  ,  948.17D0  ,  262.43D0  ,  197.14D0  ,  149.30D0  ,
     $   127.25D0  ,  110.39D0  ,  101.79D0  ,  92.834D0  ,  87.104D0  ,
     $   83.109D0  ,  78.563D0  ,  75.292D0  ,  72.760D0  ,  70.900D0  ,
     $   68.467D0  ,  66.314D0  ,  64.955D0  ,  63.746D0  ,  62.623D0  ,
     $   59.233D0  ,  56.946D0  ,  53.355D0  ,  49.810D0  ,  48.090D0  ,
     $   46.839D0  ,  45.695D0  ,  44.863D0  ,  44.062D0  ,  42.599D0  ,
     $   42.684D0  ,  42.328D0  ,  42.041D0  ,  39.508D0  ,  37.880D0  ,
     $   36.299D0  ,  34.075D0  ,  33.553D0  ,  29.723D0  ,  27.600D0  ,
     $   28.120D0  /
C  SIGMA(-)_BAR
      DATA (CSIN(25,J),J=1,41) /
     $   1182.4D0  ,  524.50D0  ,  167.30D0  ,  131.43D0  ,  91.895D0  ,
     $   75.743D0  ,  63.184D0  ,  57.376D0  ,  52.502D0  ,  49.313D0  ,
     $   47.326D0  ,  44.762D0  ,  43.222D0  ,  42.015D0  ,  41.221D0  ,
     $   40.244D0  ,  39.504D0  ,  39.145D0  ,  38.860D0  ,  38.731D0  ,
     $   37.987D0  ,  37.814D0  ,  36.326D0  ,  34.750D0  ,  33.953D0  ,
     $   33.635D0  ,  33.349D0  ,  32.938D0  ,  32.785D0  ,  32.092D0  ,
     $   32.373D0  ,  32.312D0  ,  32.329D0  ,  31.261D0  ,  30.597D0  ,
     $   30.073D0  ,  29.228D0  ,  29.182D0  ,  27.683D0  ,  27.107D0  ,
     $   27.956D0  /
C  XI(0)
      DATA (CSIN(26,J),J=1,41) /
     $  0.00000D0  , 0.00000D0  , 0.00000D0  , 0.00000D0  , 0.00000D0  ,
     $  0.00000D0  , 0.00000D0  , 0.00000D0  , 0.00000D0  , 0.00000D0  ,
     $  0.00000D0  , 0.00000D0  , 0.00000D0  , 0.00000D0  , 0.00000D0  ,
     $  0.00000D0  , 0.00000D0  , 0.00000D0  , 0.95639D-01,  1.4154D0  ,
     $   5.4104D0  ,  8.2240D0  ,  8.8031D0  ,  9.1000D0  ,  8.8761D0  ,
     $   9.0095D0  ,  9.2576D0  ,  10.307D0  ,  10.818D0  ,  11.341D0  ,
     $   11.778D0  ,  12.108D0  ,  12.569D0  ,  14.000D0  ,  14.467D0  ,
     $   15.054D0  ,  15.694D0  ,  16.263D0  ,  16.632D0  ,  20.100D0  ,
     $   21.438D0  /
C  XI(-)
      DATA (CSIN(27,J),J=1,41) /
     $  0.00000D0  , 0.00000D0  , 0.00000D0  , 0.00000D0  , 0.00000D0  ,
     $  0.00000D0  , 0.00000D0  , 0.00000D0  , 0.00000D0  , 0.00000D0  ,
     $  0.00000D0  , 0.00000D0  , 0.00000D0  , 0.00000D0  , 0.00000D0  ,
     $  0.00000D0  , 0.00000D0  , 0.00000D0  , 0.95639D-01,  1.4154D0  ,
     $   5.4104D0  ,  8.2240D0  ,  8.8031D0  ,  9.1000D0  ,  8.8761D0  ,
     $   9.0095D0  ,  9.2576D0  ,  10.307D0  ,  10.818D0  ,  11.341D0  ,
     $   11.778D0  ,  12.108D0  ,  12.569D0  ,  14.000D0  ,  14.467D0  ,
     $   15.054D0  ,  15.694D0  ,  16.263D0  ,  16.632D0  ,  20.100D0  ,
     $   21.438D0  /
C  XI(0)_BAR
      DATA (CSIN(28,J),J=1,41) /
     $   1182.4D0  ,  524.50D0  ,  167.30D0  ,  131.43D0  ,  91.895D0  ,
     $   75.743D0  ,  63.184D0  ,  57.376D0  ,  52.502D0  ,  49.313D0  ,
     $   47.326D0  ,  44.225D0  ,  42.549D0  ,  41.148D0  ,  39.960D0  ,
     $   38.822D0  ,  38.223D0  ,  38.505D0  ,  38.549D0  ,  38.301D0  ,
     $   35.843D0  ,  34.326D0  ,  30.196D0  ,  27.590D0  ,  26.112D0  ,
     $   25.076D0  ,  24.217D0  ,  24.264D0  ,  23.985D0  ,  23.445D0  ,
     $   23.713D0  ,  23.647D0  ,  23.726D0  ,  22.892D0  ,  22.119D0  ,
     $   21.485D0  ,  20.726D0  ,  20.921D0  ,  19.226D0  ,  20.454D0  ,
     $   21.658D0  /
C  XI(-)_BAR
      DATA (CSIN(29,J),J=1,41) /
     $   1076.5D0  ,  312.66D0  ,  119.74D0  ,  98.571D0  ,  63.193D0  ,
     $   49.990D0  ,  39.579D0  ,  35.168D0  ,  32.335D0  ,  30.417D0  ,
     $   29.434D0  ,  27.325D0  ,  26.514D0  ,  25.775D0  ,  25.120D0  ,
     $   24.711D0  ,  24.818D0  ,  25.600D0  ,  26.106D0  ,  26.355D0  ,
     $   25.220D0  ,  24.760D0  ,  21.681D0  ,  20.060D0  ,  19.044D0  ,
     $   18.474D0  ,  18.044D0  ,  18.301D0  ,  18.347D0  ,  18.192D0  ,
     $   18.557D0  ,  18.639D0  ,  18.870D0  ,  18.769D0  ,  18.478D0  ,
     $   18.372D0  ,  18.302D0  ,  18.735D0  ,  18.206D0  ,  20.207D0  ,
     $   21.576D0  /
C  OMEGA(-)
      DATA (CSIN(33,J),J=1,41) /
     $  0.00000D0  , 0.00000D0  , 0.00000D0  , 0.00000D0  , 0.00000D0  ,
     $  0.00000D0  , 0.00000D0  , 0.00000D0  , 0.00000D0  , 0.00000D0  ,
     $  0.00000D0  , 0.00000D0  , 0.00000D0  , 0.00000D0  , 0.00000D0  ,
     $  0.00000D0  , 0.00000D0  , 0.00000D0  , 0.95639D-01,  1.4154D0  ,
     $   5.4104D0  ,  8.2240D0  ,  8.8031D0  ,  9.1000D0  ,  8.8761D0  ,
     $   9.0095D0  ,  9.2576D0  ,  10.307D0  ,  10.818D0  ,  11.341D0  ,
     $   11.778D0  ,  12.108D0  ,  12.569D0  ,  14.000D0  ,  14.467D0  ,
     $   15.054D0  ,  15.694D0  ,  16.263D0  ,  16.632D0  ,  20.100D0  ,
     $   21.438D0  /
C  OMEGA(-)_BAR
      DATA (CSIN(34,J),J=1,41) /
     $   1076.5D0  ,  312.66D0  ,  119.74D0  ,  98.571D0  ,  63.193D0  ,
     $   49.990D0  ,  39.579D0  ,  35.168D0  ,  32.335D0  ,  30.417D0  ,
     $   29.434D0  ,  27.325D0  ,  26.514D0  ,  25.775D0  ,  25.120D0  ,
     $   24.711D0  ,  24.818D0  ,  25.600D0  ,  26.106D0  ,  26.355D0  ,
     $   25.220D0  ,  24.760D0  ,  21.681D0  ,  20.060D0  ,  19.044D0  ,
     $   18.474D0  ,  18.044D0  ,  18.301D0  ,  18.347D0  ,  18.192D0  ,
     $   18.557D0  ,  18.639D0  ,  18.870D0  ,  18.769D0  ,  18.478D0  ,
     $   18.372D0  ,  18.302D0  ,  18.735D0  ,  18.206D0  ,  20.207D0  ,
     $   21.576D0  /

C  ELASTIC CROSS-SECTION FOR MEDI WITH PIONS
C  ALUMINIUM
      DATA (CSPIEL( 1,J),J=1,41) /
     $  0.00000D0  ,  350.00D0  ,  580.00D0  ,  600.00D0  ,  550.00D0  ,
     $   450.00D0  ,  410.00D0  ,  370.00D0  ,  340.00D0  ,  230.00D0  ,
     $   220.00D0  ,  205.00D0  ,  180.00D0  ,  155.00D0  ,  145.00D0  ,
     $   140.00D0  ,  160.00D0  ,  195.00D0  ,  235.00D0  ,  250.00D0  ,
     $   270.00D0  ,  280.00D0  ,  300.00D0  ,  300.00D0  ,  290.00D0  ,
     $   285.00D0  ,  265.00D0  ,  240.00D0  ,  230.00D0  ,  222.00D0  ,
     $   204.00D0  ,  196.00D0  ,  190.00D0  ,  170.00D0  ,  170.00D0  ,
     $   160.00D0  ,  150.00D0  ,  140.00D0  ,  120.00D0  ,  80.000D0  ,
     $   80.000D0  /
C  COPPER
      DATA (CSPIEL( 2,J),J=1,41) /
     $  0.00000D0  ,  700.00D0  ,  1000.0D0  ,  1200.0D0  ,  1300.0D0  ,
     $   1300.0D0  ,  1250.0D0  ,  1250.0D0  ,  1100.0D0  ,  1000.0D0  ,
     $   940.00D0  ,  740.00D0  ,  700.00D0  ,  670.00D0  ,  660.00D0  ,
     $   670.00D0  ,  680.00D0  ,  700.00D0  ,  735.00D0  ,  800.00D0  ,
     $   810.00D0  ,  820.00D0  ,  820.00D0  ,  810.00D0  ,  800.00D0  ,
     $   800.00D0  ,  700.00D0  ,  600.00D0  ,  500.00D0  ,  470.00D0  ,
     $   440.00D0  ,  410.00D0  ,  380.00D0  ,  330.00D0  ,  330.00D0  ,
     $   330.00D0  ,  330.00D0  ,  330.00D0  ,  285.00D0  ,  240.00D0  ,
     $   240.00D0  /
C  LEAD
      DATA (CSPIEL( 3,J),J=1,41) /
     $  0.00000D0  ,  1700.0D0  ,  2200.0D0  ,  2200.0D0  ,  1800.0D0  ,
     $   1300.0D0  ,  1200.0D0  ,  900.00D0  ,  900.00D0  ,  1000.0D0  ,
     $   1100.0D0  ,  1300.0D0  ,  1400.0D0  ,  1420.0D0  ,  1490.0D0  ,
     $   1560.0D0  ,  1580.0D0  ,  1690.0D0  ,  1795.0D0  ,  2000.0D0  ,
     $   2070.0D0  ,  2140.0D0  ,  2050.0D0  ,  2010.0D0  ,  1970.0D0  ,
     $   1880.0D0  ,  1690.0D0  ,  1500.0D0  ,  1420.0D0  ,  1390.0D0  ,
     $   1350.0D0  ,  1360.0D0  ,  1370.0D0  ,  1280.0D0  ,  1290.0D0  ,
     $   1295.0D0  ,  1250.0D0  ,  1200.0D0  ,  1050.0D0  ,  900.00D0  ,
     $   900.00D0  /
C  INELASTIC CROSS-SECTION FOR MEDIA WITH PIONS
C  ALIMINUIM
      DATA (CSPIIN( 1,J),J=1,41) /
     $  0.00000D0  ,  200.00D0  ,  320.00D0  ,  500.00D0  ,  600.00D0  ,
     $   600.00D0  ,  590.00D0  ,  530.00D0  ,  510.00D0  ,  470.00D0  ,
     $   430.00D0  ,  425.00D0  ,  420.00D0  ,  425.00D0  ,  425.00D0  ,
     $   430.00D0  ,  430.00D0  ,  435.00D0  ,  435.00D0  ,  440.00D0  ,
     $   430.00D0  ,  430.00D0  ,  420.00D0  ,  420.00D0  ,  420.00D0  ,
     $   415.00D0  ,  415.00D0  ,  410.00D0  ,  410.00D0  ,  408.00D0  ,
     $   406.00D0  ,  404.00D0  ,  400.00D0  ,  380.00D0  ,  340.00D0  ,
     $   340.00D0  ,  340.00D0  ,  340.00D0  ,  340.00D0  ,  340.00D0  ,
     $   340.00D0  /
C  COPPER
      DATA (CSPIIN( 2,J),J=1,41) /
     $  0.00000D0  ,  400.00D0  ,  800.00D0  ,  1000.0D0  ,  1100.0D0  ,
     $   1200.0D0  ,  1150.0D0  ,  1050.0D0  ,  1000.0D0  ,  900.00D0  ,
     $   860.00D0  ,  860.00D0  ,  850.00D0  ,  850.00D0  ,  840.00D0  ,
     $   830.00D0  ,  820.00D0  ,  810.00D0  ,  805.00D0  ,  800.00D0  ,
     $   800.00D0  ,  800.00D0  ,  800.00D0  ,  800.00D0  ,  800.00D0  ,
     $   800.00D0  ,  800.00D0  ,  800.00D0  ,  800.00D0  ,  780.00D0  ,
     $   760.00D0  ,  740.00D0  ,  720.00D0  ,  720.00D0  ,  700.00D0  ,
     $   690.00D0  ,  680.00D0  ,  670.00D0  ,  665.00D0  ,  660.00D0  ,
     $   660.00D0  /
C  LEAD
      DATA (CSPIIN( 3,J),J=1,41) /
     $  0.00000D0  ,  1000.0D0  ,  1900.0D0  ,  2600.0D0  ,  2900.0D0  ,
     $   3000.0D0  ,  2800.0D0  ,  2600.0D0  ,  2500.0D0  ,  2300.0D0  ,
     $   2200.0D0  ,  2000.0D0  ,  1900.0D0  ,  1880.0D0  ,  1860.0D0  ,
     $   1840.0D0  ,  1820.0D0  ,  1810.0D0  ,  1805.0D0  ,  1800.0D0  ,
     $   1780.0D0  ,  1760.0D0  ,  1750.0D0  ,  1740.0D0  ,  1730.0D0  ,
     $   1720.0D0  ,  1710.0D0  ,  1700.0D0  ,  1680.0D0  ,  1660.0D0  ,
     $   1650.0D0  ,  1640.0D0  ,  1630.0D0  ,  1620.0D0  ,  1610.0D0  ,
     $   1605.0D0  ,  1600.0D0  ,  1600.0D0  ,  1550.0D0  ,  1500.0D0  ,
     $   1500.0D0  /
C  ELASTIC CROSS-SECTION FOR MEDI WITH NUCLEONS
C  ALUMINIUM
      DATA (CSPNEL( 1,J),J=1,41) /
     $   2100.0D0  ,  1800.0D0  ,  1500.0D0  ,  1050.0D0  ,  900.00D0  ,
     $   950.00D0  ,  800.00D0  ,  650.00D0  ,  570.00D0  ,  390.00D0  ,
     $   300.00D0  ,  240.00D0  ,  230.00D0  ,  230.00D0  ,  220.00D0  ,
     $   220.00D0  ,  225.00D0  ,  225.00D0  ,  240.00D0  ,  240.00D0  ,
     $   290.00D0  ,  330.00D0  ,  335.00D0  ,  350.00D0  ,  355.00D0  ,
     $   370.00D0  ,  350.00D0  ,  330.00D0  ,  310.00D0  ,  290.00D0  ,
     $   270.00D0  ,  265.00D0  ,  260.00D0  ,  230.00D0  ,  210.00D0  ,
     $   210.00D0  ,  200.00D0  ,  200.00D0  ,  190.00D0  ,  180.00D0  ,
     $   180.00D0  /
C  COPPER
      DATA (CSPNEL( 2,J),J=1,41) /
     $   3800.0D0  ,  2900.0D0  ,  1850.0D0  ,  1550.0D0  ,  1450.0D0  ,
     $   1520.0D0  ,  1460.0D0  ,  1300.0D0  ,  1140.0D0  ,  880.00D0  ,
     $   700.00D0  ,  620.00D0  ,  540.00D0  ,  560.00D0  ,  460.00D0  ,
     $   460.00D0  ,  470.00D0  ,  470.00D0  ,  480.00D0  ,  480.00D0  ,
     $   580.00D0  ,  600.00D0  ,  610.00D0  ,  620.00D0  ,  620.00D0  ,
     $   620.00D0  ,  590.00D0  ,  580.00D0  ,  460.00D0  ,  440.00D0  ,
     $   420.00D0  ,  400.00D0  ,  480.00D0  ,  430.00D0  ,  380.00D0  ,
     $   380.00D0  ,  380.00D0  ,  380.00D0  ,  380.00D0  ,  380.00D0  ,
     $   380.00D0  /
C  LEAD
      DATA (CSPNEL( 3,J),J=1,41) /
     $   7000.0D0  ,  6000.0D0  ,  4500.0D0  ,  3350.0D0  ,  2700.0D0  ,
     $   3000.0D0  ,  3550.0D0  ,  3970.0D0  ,  3280.0D0  ,  2490.0D0  ,
     $   2100.0D0  ,  1510.0D0  ,  1440.0D0  ,  1370.0D0  ,  1370.0D0  ,
     $   1370.0D0  ,  1400.0D0  ,  1400.0D0  ,  1420.0D0  ,  1420.0D0  ,
     $   1440.0D0  ,  1460.0D0  ,  1460.0D0  ,  1450.0D0  ,  1450.0D0  ,
     $   1470.0D0  ,  1400.0D0  ,  1400.0D0  ,  1380.0D0  ,  1370.0D0  ,
     $   1360.0D0  ,  1350.0D0  ,  1340.0D0  ,  1330.0D0  ,  1320.0D0  ,
     $   1310.0D0  ,  1305.0D0  ,  1300.0D0  ,  1300.0D0  ,  1300.0D0  ,
     $   1300.0D0  /
C  INELASTIC CROSS-SECTION FOR MEDI WITH NUCLEONS
C  ALUMINIUM
      DATA (CSPNIN( 1,J),J=1,41) /
     $  0.00000D0  ,  200.00D0  ,  400.00D0  ,  800.00D0  ,  800.00D0  ,
     $   550.00D0  ,  500.00D0  ,  450.00D0  ,  430.00D0  ,  410.00D0  ,
     $   400.00D0  ,  390.00D0  ,  380.00D0  ,  370.00D0  ,  370.00D0  ,
     $   370.00D0  ,  365.00D0  ,  365.00D0  ,  360.00D0  ,  360.00D0  ,
     $   360.00D0  ,  360.00D0  ,  365.00D0  ,  370.00D0  ,  375.00D0  ,
     $   380.00D0  ,  400.00D0  ,  410.00D0  ,  420.00D0  ,  430.00D0  ,
     $   440.00D0  ,  440.00D0  ,  440.00D0  ,  440.00D0  ,  440.00D0  ,
     $   440.00D0  ,  440.00D0  ,  440.00D0  ,  440.00D0  ,  440.00D0  ,
     $   440.00D0  /
C  COPPER
      DATA (CSPNIN( 2,J),J=1,41) /
     $  0.00000D0  ,  400.00D0  ,  950.00D0  ,  1050.0D0  ,  1050.0D0  ,
     $   980.00D0  ,  940.00D0  ,  900.00D0  ,  860.00D0  ,  820.00D0  ,
     $   800.00D0  ,  780.00D0  ,  760.00D0  ,  740.00D0  ,  740.00D0  ,
     $   740.00D0  ,  730.00D0  ,  730.00D0  ,  720.00D0  ,  720.00D0  ,
     $   720.00D0  ,  720.00D0  ,  730.00D0  ,  740.00D0  ,  750.00D0  ,
     $   760.00D0  ,  800.00D0  ,  820.00D0  ,  820.00D0  ,  820.00D0  ,
     $   820.00D0  ,  820.00D0  ,  820.00D0  ,  820.00D0  ,  820.00D0  ,
     $   820.00D0  ,  820.00D0  ,  820.00D0  ,  820.00D0  ,  820.00D0  ,
     $   820.00D0  /
C  LEAD
      DATA (CSPNIN( 3,J),J=1,41) /
     $  0.00000D0  , 0.00000D0  ,  500.00D0  ,  1450.0D0  ,  1700.0D0  ,
     $   1800.0D0  ,  1750.0D0  ,  1730.0D0  ,  1720.0D0  ,  1710.0D0  ,
     $   1700.0D0  ,  1690.0D0  ,  1660.0D0  ,  1630.0D0  ,  1630.0D0  ,
     $   1630.0D0  ,  1600.0D0  ,  1600.0D0  ,  1580.0D0  ,  1580.0D0  ,
     $   1580.0D0  ,  1580.0D0  ,  1600.0D0  ,  1630.0D0  ,  1650.0D0  ,
     $   1670.0D0  ,  1760.0D0  ,  1800.0D0  ,  1800.0D0  ,  1800.0D0  ,
     $   1800.0D0  ,  1800.0D0  ,  1800.0D0  ,  1800.0D0  ,  1800.0D0  ,
     $   1800.0D0  ,  1800.0D0  ,  1800.0D0  ,  1800.0D0  ,  1800.0D0  ,
     $   1800.0D0  /
      DATA ELAB /
     $  0.10000D-03, 0.20000D-03, 0.30000D-03, 0.40000D-03, 0.50000D-03,
     $  0.70000D-03, 0.10000D-02, 0.20000D-02, 0.30000D-02, 0.40000D-02,
     $  0.50000D-02, 0.70000D-02, 0.10000D-01, 0.15000D-01, 0.20000D-01,
     $  0.25000D-01, 0.32700D-01/
C  TABLES FOR VARIOUS ATOMIC WEIGHTS
      DATA CNLWAT /
     $   1.0000D0  ,  16.000D0  ,  27.000D0  ,  56.000D0  ,  59.000D0  ,
     $   64.000D0  ,  91.000D0  ,  112.00D0  ,  119.00D0  ,  127.00D0  ,
     $   137.00D0  ,  181.00D0  ,  207.00D0  ,  209.00D0  ,  238.00D0  /
      DATA (CNLWEL( 1,J),J=1,17) /
     $   6000.0D0  ,  5500.0D0  ,  5200.0D0  ,  4900.0D0  ,  4800.0D0  ,
     $   4400.0D0  ,  4000.0D0  ,  2900.0D0  ,  2200.0D0  ,  1800.0D0  ,
     $   1400.0D0  ,  1100.0D0  ,  900.00D0  ,  700.00D0  ,  600.00D0  ,
     $   560.00D0  ,  520.00D0  /
      DATA (CNLWEL( 2,J),J=1,17) /
     $   5400.0D0  ,  5050.0D0  ,  4800.0D0  ,  4600.0D0  ,  4399.0D0  ,
     $   4090.0D0  ,  3700.0D0  ,  2600.0D0  ,  1950.0D0  ,  1600.0D0  ,
     $   1300.0D0  ,  900.00D0  ,  700.00D0  ,  800.00D0  ,  1050.0D0  ,
     $   1250.0D0  ,  1320.0D0  /
      DATA (CNLWEL( 3,J),J=1,17) /
     $   5500.0D0  ,  5150.0D0  ,  4900.0D0  ,  4699.0D0  ,  4490.0D0  ,
     $   4150.0D0  ,  3750.0D0  ,  2790.0D0  ,  2100.0D0  ,  1650.0D0  ,
     $   1300.0D0  ,  950.00D0  ,  800.00D0  ,  860.00D0  ,  1000.0D0  ,
     $   1090.0D0  ,  1080.0D0  /
      DATA (CNLWEL( 4,J),J=1,17) /
     $   5499.0D0  ,  4970.0D0  ,  4450.0D0  ,  4080.0D0  ,  3750.0D0  ,
     $   3380.0D0  ,  2900.0D0  ,  2400.0D0  ,  2380.0D0  ,  2350.0D0  ,
     $   2300.0D0  ,  2100.0D0  ,  1720.0D0  ,  1370.0D0  ,  1200.0D0  ,
     $   1060.0D0  ,  870.00D0  /
      DATA (CNLWEL( 5,J),J=1,17) /
     $   5399.0D0  ,  4710.0D0  ,  4180.0D0  ,  3760.0D0  ,  3460.0D0  ,
     $   3150.0D0  ,  2730.0D0  ,  2270.0D0  ,  1850.0D0  ,  1850.0D0  ,
     $   2130.0D0  ,  2330.0D0  ,  2120.0D0  ,  1640.0D0  ,  1310.0D0  ,
     $   1100.0D0  ,  1050.0D0  /
      DATA (CNLWEL( 6,J),J=1,17) /
     $   5099.0D0  ,  4405.0D0  ,  3825.0D0  ,  3455.0D0  ,  3125.0D0  ,
     $   2695.0D0  ,  2350.0D0  ,  1850.0D0  ,  1580.0D0  ,  1820.0D0  ,
     $   2050.0D0  ,  2210.0D0  ,  2000.0D0  ,  1590.0D0  ,  1310.0D0  ,
     $   1120.0D0  ,  1040.0D0  /
      DATA (CNLWEL( 7,J),J=1,17) /
     $   6290.0D0  ,  5960.0D0  ,  5640.0D0  ,  5370.0D0  ,  5150.0D0  ,
     $   4800.0D0  ,  4250.0D0  ,  3150.0D0  ,  2470.0D0  ,  2100.0D0  ,
     $   2230.0D0  ,  2420.0D0  ,  2450.0D0  ,  2050.0D0  ,  1760.0D0  ,
     $   1550.0D0  ,  1330.0D0  /
      DATA (CNLWEL( 8,J),J=1,17) /
     $   6885.0D0  ,  6650.0D0  ,  6350.0D0  ,  6150.0D0  ,  6000.0D0  ,
     $   5700.0D0  ,  5360.0D0  ,  4250.0D0  ,  2800.0D0  ,  1870.0D0  ,
     $   1810.0D0  ,  1820.0D0  ,  2170.0D0  ,  2450.0D0  ,  2150.0D0  ,
     $   1700.0D0  ,  1390.0D0  /
      DATA (CNLWEL( 9,J),J=1,17) /
     $   6600.0D0  ,  6500.0D0  ,  6400.0D0  ,  6249.0D0  ,  6190.0D0  ,
     $   5950.0D0  ,  5520.0D0  ,  4250.0D0  ,  2750.0D0  ,  1900.0D0  ,
     $   1850.0D0  ,  1950.0D0  ,  2340.0D0  ,  2800.0D0  ,  2540.0D0  ,
     $   2100.0D0  ,  1760.0D0  /
      DATA (CNLWEL(10,J),J=1,17) /
     $   7400.0D0  ,  7200.0D0  ,  6999.0D0  ,  6840.0D0  ,  6655.0D0  ,
     $   6320.0D0  ,  5820.0D0  ,  4400.0D0  ,  2850.0D0  ,  2000.0D0  ,
     $   1800.0D0  ,  1800.0D0  ,  2150.0D0  ,  2600.0D0  ,  2350.0D0  ,
     $   1950.0D0  ,  2100.0D0  /
      DATA (CNLWEL(11,J),J=1,17) /
     $   7900.0D0  ,  7700.0D0  ,  7499.0D0  ,  7390.0D0  ,  7202.0D0  ,
     $   6810.0D0  ,  6360.0D0  ,  4920.0D0  ,  3450.0D0  ,  2600.0D0  ,
     $   2200.0D0  ,  1950.0D0  ,  2300.0D0  ,  2800.0D0  ,  2650.0D0  ,
     $   2250.0D0  ,  2050.0D0  /
      DATA (CNLWEL(12,J),J=1,17) /
     $   7900.0D0  ,  7750.0D0  ,  7699.0D0  ,  7590.0D0  ,  7450.0D0  ,
     $   7200.0D0  ,  6850.0D0  ,  5650.0D0  ,  4400.0D0  ,  3700.0D0  ,
     $   3400.0D0  ,  2800.0D0  ,  2700.0D0  ,  3100.0D0  ,  3250.0D0  ,
     $   3100.0D0  ,  2750.0D0  /
      DATA (CNLWEL(13,J),J=1,17) /
     $   6100.0D0  ,  5950.0D0  ,  5750.0D0  ,  5599.0D0  ,  5440.0D0  ,
     $   5200.0D0  ,  4800.0D0  ,  4300.0D0  ,  5800.0D0  ,  5750.0D0  ,
     $   4800.0D0  ,  3420.0D0  ,  2650.0D0  ,  3200.0D0  ,  3650.0D0  ,
     $   3500.0D0  ,  2980.0D0  /
      DATA (CNLWEL(14,J),J=1,17) /
     $   6100.0D0  ,  5950.0D0  ,  5750.0D0  ,  5599.0D0  ,  5440.0D0  ,
     $   5200.0D0  ,  4800.0D0  ,  4300.0D0  ,  5800.0D0  ,  5750.0D0  ,
     $   4800.0D0  ,  3420.0D0  ,  2650.0D0  ,  3200.0D0  ,  3650.0D0  ,
     $   3500.0D0  ,  2980.0D0  /
      DATA (CNLWEL(15,J),J=1,17) /
     $   6600.0D0  ,  6350.0D0  ,  6100.0D0  ,  5899.0D0  ,  5690.0D0  ,
     $   5300.0D0  ,  4850.0D0  ,  4450.0D0  ,  5650.0D0  ,  5700.0D0  ,
     $   4950.0D0  ,  3850.0D0  ,  3050.0D0  ,  3050.0D0  ,  3460.0D0  ,
     $   3650.0D0  ,  3340.0D0  /
      DATA (CNLWIN( 1,J),J=1,17) / 17*0.0D0   /
      DATA (CNLWIN( 2,J),J=1,17) /
     $  0.00000D0  , 0.00000D0  , 0.00000D0  , 0.00000D0  ,  1.0000D0  ,
     $   10.000D0  ,  50.000D0  ,  100.00D0  ,  200.00D0  ,  300.00D0  ,
     $   400.00D0  ,  600.00D0  ,  700.00D0  ,  750.00D0  ,  700.00D0  ,
     $   700.00D0  ,  680.00D0  /
      DATA (CNLWIN( 3,J),J=1,17) /
     $  0.00000D0  , 0.00000D0  , 0.00000D0  ,  1.0000D0  ,  10.000D0  ,
     $   50.000D0  ,  100.00D0  ,  260.00D0  ,  450.00D0  ,  600.00D0  ,
     $   700.00D0  ,  800.00D0  ,  900.00D0  ,  940.00D0  ,  900.00D0  ,
     $   860.00D0  ,  820.00D0  /
      DATA (CNLWIN( 4,J),J=1,17) /
     $   1.0000D0  ,  80.000D0  ,  200.00D0  ,  320.00D0  ,  400.00D0  ,
     $   520.00D0  ,  700.00D0  ,  1000.0D0  ,  1120.0D0  ,  1200.0D0  ,
     $   1200.0D0  ,  1200.0D0  ,  1180.0D0  ,  1130.0D0  ,  1100.0D0  ,
     $   1090.0D0  ,  1080.0D0  /
      DATA (CNLWIN( 5,J),J=1,17) /
     $   1.0000D0  ,  90.000D0  ,  220.00D0  ,  340.00D0  ,  420.00D0  ,
     $   550.00D0  ,  720.00D0  ,  1080.0D0  ,  1300.0D0  ,  1400.0D0  ,
     $   1420.0D0  ,  1420.0D0  ,  1380.0D0  ,  1260.0D0  ,  1190.0D0  ,
     $   1150.0D0  ,  1100.0D0  /
      DATA (CNLWIN( 6,J),J=1,17) /
     $   1.0000D0  ,  95.000D0  ,  225.00D0  ,  345.00D0  ,  425.00D0  ,
     $   555.00D0  ,  750.00D0  ,  1150.0D0  ,  1500.0D0  ,  1680.0D0  ,
     $   1700.0D0  ,  1690.0D0  ,  1550.0D0  ,  1360.0D0  ,  1240.0D0  ,
     $   1180.0D0  ,  1120.0D0  /
      DATA (CNLWIN( 7,J),J=1,17) /
     $   10.000D0  ,  140.00D0  ,  260.00D0  ,  380.00D0  ,  450.00D0  ,
     $   600.00D0  ,  750.00D0  ,  1200.0D0  ,  1580.0D0  ,  1800.0D0  ,
     $   1820.0D0  ,  1830.0D0  ,  1800.0D0  ,  1750.0D0  ,  1690.0D0  ,
     $   1650.0D0  ,  1620.0D0  /
      DATA (CNLWIN( 8,J),J=1,17) /
     $   15.000D0  ,  150.00D0  ,  300.00D0  ,  400.00D0  ,  500.00D0  ,
     $   650.00D0  ,  840.00D0  ,  1500.0D0  ,  2100.0D0  ,  2130.0D0  ,
     $   2140.0D0  ,  2130.0D0  ,  2080.0D0  ,  2000.0D0  ,  1950.0D0  ,
     $   1900.0D0  ,  1860.0D0  /
      DATA (CNLWIN( 9,J),J=1,17) /
     $  0.00000D0  , 0.00000D0  , 0.00000D0  ,  1.0000D0  ,  10.000D0  ,
     $   150.00D0  ,  380.00D0  ,  1000.0D0  ,  1650.0D0  ,  2100.0D0  ,
     $   2100.0D0  ,  2100.0D0  ,  2060.0D0  ,  1950.0D0  ,  1860.0D0  ,
     $   1800.0D0  ,  1740.0D0  /
      DATA (CNLWIN(10,J),J=1,17) /
     $  0.00000D0  , 0.00000D0  ,  1.0000D0  ,  10.000D0  ,  45.000D0  ,
     $   180.00D0  ,  380.00D0  ,  1050.0D0  ,  1900.0D0  ,  2300.0D0  ,
     $   2300.0D0  ,  2200.0D0  ,  2150.0D0  ,  2000.0D0  ,  1900.0D0  ,
     $   1800.0D0  ,  1750.0D0  /
      DATA (CNLWIN(11,J),J=1,17) /
     $  0.00000D0  , 0.00000D0  ,  1.0000D0  ,  10.000D0  ,  48.000D0  ,
     $   190.00D0  ,  390.00D0  ,  1080.0D0  ,  2000.0D0  ,  2400.0D0  ,
     $   2400.0D0  ,  2300.0D0  ,  2200.0D0  ,  2100.0D0  ,  1950.0D0  ,
     $   1850.0D0  ,  1800.0D0  /
      DATA (CNLWIN(12,J),J=1,17) /
     $  0.00000D0  , 0.00000D0  ,  1.0000D0  ,  10.000D0  ,  50.000D0  ,
     $   200.00D0  ,  400.00D0  ,  1100.0D0  ,  2100.0D0  ,  2500.0D0  ,
     $   2500.0D0  ,  2450.0D0  ,  2300.0D0  ,  2100.0D0  ,  2000.0D0  ,
     $   1900.0D0  ,  1850.0D0  /
      DATA (CNLWIN(13,J),J=1,17) /
     $  0.00000D0  , 0.00000D0  , 0.00000D0  ,  1.0000D0  ,  10.000D0  ,
     $   100.00D0  ,  350.00D0  ,  900.00D0  ,  1400.0D0  ,  2000.0D0  ,
     $   2300.0D0  ,  2380.0D0  ,  2400.0D0  ,  2300.0D0  ,  2250.0D0  ,
     $   2200.0D0  ,  2120.0D0  /
      DATA (CNLWIN(14,J),J=1,17) /
     $  0.00000D0  , 0.00000D0  , 0.00000D0  ,  1.0000D0  ,  10.000D0  ,
     $   100.00D0  ,  350.00D0  ,  900.00D0  ,  1400.0D0  ,  2000.0D0  ,
     $   2300.0D0  ,  2380.0D0  ,  2400.0D0  ,  2300.0D0  ,  2250.0D0  ,
     $   2200.0D0  ,  2120.0D0  /
      DATA (CNLWIN(15,J),J=1,17) /
     $  0.00000D0  , 0.00000D0  , 0.00000D0  ,  1.0000D0  ,  10.000D0  ,
     $   100.00D0  ,  400.00D0  ,  950.00D0  ,  1600.0D0  ,  2200.0D0  ,
     $   2550.0D0  ,  2750.0D0  ,  2700.0D0  ,  2600.0D0  ,  2540.0D0  ,
     $   2450.0D0  ,  2360.0D0  /
      DATA (CSCAP(J),J=1,50) /
     $   6.0000D0  ,  5.7000D0  ,  5.5000D0  ,  5.3000D0  ,  5.2000D0  ,
     $   5.1000D0  ,  5.0000D0  ,  4.9000D0  ,  4.8000D0  ,  4.8000D0  ,
     $   4.8000D0  ,  4.8000D0  ,  4.8000D0  ,  4.8000D0  ,  4.8000D0  ,
     $   4.8000D0  ,  4.9000D0  ,  5.0000D0  ,  5.2000D0  ,  5.5000D0  ,
     $   6.0000D0  ,  6.7000D0  ,  7.5000D0  ,  8.5000D0  ,  10.000D0  ,
     $   12.000D0  ,  14.500D0  ,  19.000D0  ,  26.500D0  ,  40.000D0  ,
     $   75.000D0  ,  120.00D0  ,  180.00D0  ,  260.00D0  ,  360.00D0  ,
     $   330.00D0  ,  60.000D0  ,  7.0000D0  ,  9.5000D0  ,  20.000D0  ,
     $   75.000D0  ,  140.00D0  ,  250.00D0  ,  360.00D0  ,  480.00D0  ,
     $   580.00D0  ,  590.00D0  ,  500.00D0  ,  300.00D0  ,  100.00D0  /
      DATA (CSCAP(J),J=51,100) /
     $   200.00D0  ,  300.00D0  ,  400.00D0  ,  470.00D0  ,  500.00D0  ,
     $   430.00D0  ,  100.00D0  ,  20.000D0  ,  22.000D0  ,  40.000D0  ,
     $   560.00D0  ,  950.00D0  ,  1000.0D0  ,  1000.0D0  ,  1000.0D0  ,
     $   990.00D0  ,  920.00D0  ,  860.00D0  ,  790.00D0  ,  740.00D0  ,
     $   650.00D0  ,  600.00D0  ,  540.00D0  ,  470.00D0  ,  440.00D0  ,
     $   390.00D0  ,  360.00D0  ,  340.00D0  ,  320.00D0  ,  310.00D0  ,
     $   280.00D0  ,  2.0000D0  ,  2.5000D0  ,  6.0000D0  ,  13.000D0  ,
     $   38.000D0  ,  65.000D0  ,  140.00D0  ,  280.00D0  ,  300.00D0  ,
     $   430.00D0  ,  580.00D0  ,  650.00D0  ,  800.00D0  ,  920.00D0  ,
     $   1100.0D0  ,  1250.0D0  ,  1400.0D0  ,  1550.0D0  ,  1700.0D0  /
C --- END OF CROSS-SECTION DATA STATEMENTS ---

C --- DATA STMTS. FOR GEANT/GHEISHA PARTICLE CODE CONVERSIONS ---
C --- KIPART(I)=GHEISHA CODE CORRESPONDING TO GEANT   CODE I ---
C --- IKPART(I)=GEANT   CODE CORRESPONDING TO GHEISHA CODE I ---

c$$$      DATA KIPART/
c$$$     $               1,   3,   4,   2,   5,   6,   8,   7,
c$$$     $               9,  12,  10,  13,  16,  14,  15,  11,
c$$$     $              35,  18,  20,  21,  22,  26,  27,  33,
c$$$     $              17,  19,  23,  24,  25,  28,  29,  34,
c$$$     $              35,  35,  35,  35,  35,  35,  35,  35,
c$$$     $              35,  35,  35,  35,  30,  31,  32,  35/

*     DATA IKPART/
*    $               1,   4,   2,   3,   5,   6,   8,   7,
*    $               9,  11,  16,  10,  12,  14,  15,  13,
*    $              25,  18,  26,  19,  20,  21,  27,  28,
*    $              29,  22,  23,  30,  31,  45,  46,  47,
*    $              24,  32,  48/


C     PARAMETER (ONETHR=1./3.)
      DATA ONETHR   / .33333333D0/
      DATA ALPHA    / 6*0.7D0,
     +                0.75D0 ,0.75D0 ,0.75D0 ,
     +                0.76D0,0.76D0 ,0.76D0 ,0.76D0 ,
     +                0.685D0,0.63D0 ,0.685D0,0.63D0,0.685D0,0.63D0,
     +                3*0.685D0,3*0.63D0,2*0.685D0,2*0.63D0,
     +                3*0.7D0,0.685D0,0.63D0,0.7D0/
      DATA ALPHAC    /1.2D0,1.2D0,1.2D0,1.15D0,0.90D0,0.91D0,0.98D0,
     +                1.06D0,1.10D0,1.11D0,1.10D0,1.08D0,1.05D0,1.01D0,
     +                0.985D0,0.962D0,0.945D0,0.932D0,0.925D0,0.920D0,
     +                0.920D0,0.921D0,0.922D0,0.923D0,0.928D0,0.931D0,
     +                0.940D0,0.945D0,0.950D0,0.955D0,0.958D0,0.962D0,
     +                0.965D0,0.976D0,0.982D0,0.988D0,0.992D0,1.010D0,
     +                1.020D0,1.030D0,1.040D0/
      DATA PARTEL/6*0.D0,29*1.D0/
      DATA PARTIN/6*0.D0,1.00D0,0.00D0,1.05D0,1.20D0,1.35D0,1.30D0,
     +            1.20D0,1.00D0,1.30D0,1.00D0,1.30D0,1.00D0,1.30D0,
     +            1.00D0,1.00D0,1.00D0,1.30D0,1.30D0,1.30D0,1.00D0,
     +            1.00D0,1.30D0,1.30D0,1.00D0,1.D0,1.D0,1.D0,1.3D0,
     +            1.D0/
*     DATA ICORR /14*1, 0, 1, 0, 1, 0, 3*1, 3*0, 2*1, 2*0, 4*1, 2*0/
C--  SET INTRC TO 0 FOR IPART = 26-29, 33, 34  ( XI'S AND OMEGA'S )
C-DH- DATA INTRC /6*0, 1, 0, 12*1, 0, 2*2, 0, 1, 4*0, 3*1, 3*0 /
C--  RESET INTRC FOR IPART = 26-29, 33, 34  ( XI'S AND OMEGA'S )
      DATA INTRC /6*0, 1, 0, 12*1, 0, 2*2, 0, 10*1, 0/

C  CROSS-SECTIONS ON NUCLEUS ARE KNOWN ONLY FOR PIONS AND PROTONS.
C  THE GENERAL LAW SIGMA(A)=1.25*SIGMA(TOT,PROTON)*A**ALPHA IS VALID
C  ONLY FOR MOMENTA > 2 GEV.THE PARAMETRIZATION DONE HERE GIVES ONLY
C  A BEHAVIOUR AVERAGED OVER MOMENTA AND PARTICLE TYPES.
C  FOR A DETECTOR WITH ONLY A FEW MATERIALS IT'S OF COURSE MUCHBETTER
C  TO USE TABLES OF THE MEASURED CROSS-SECTIONS .
C  FOR ELEMENTS WITH THE FOLLOWING ATOMIC NUMBERS MEASURED CROSS-
C  SECTIONS ARE AVAILABLE (SEE "PCSDATA").

C                 H   AL     CU     PB
      DATA  CSA  /1.D0 ,27.00D0 ,63.54D0 ,207.19D0 /
      DATA IPART2/9,8,7,11,10,13,12/
      SAVE ALPHA,ALPHAC,PARTEL,PARTIN,CSA,IPART2,INTRC

C-----------------------------------------------------------------------
c$$$
c$$$      IF ( DEBUG .AND. GHEISDB ) THEN
c$$$        WRITE(MDEBUG,*) 'GHESIG:'
c$$$        NPRT(4)=.TRUE.
c$$$        NPRT(9)=.TRUE.
c$$$      ELSE
c$$$        NPRT(4)=.FALSE.
c$$$        NPRT(9)=.FALSE.
c$$$      ENDIF

C --- INITIALIZE GHESIG AND SWITCH TO GHEISHA PARTICLE CODE ---
      GHESIG=0.0
      IPART=KPART
c$$$      IF     ( LPART .LE.  48 ) THEN
c$$$        IPART = KIPART(LPART)
c$$$      ELSEIF ( LPART .EQ. 201 ) THEN
c$$$        IPART = 30
c$$$      ELSEIF ( LPART .EQ. 301 ) THEN
c$$$        IPART = 31
c$$$      ELSEIF ( LPART .EQ. 402 ) THEN
c$$$        IPART = 32
c$$$      ELSE
c$$$        GOTO 160
c$$$      ENDIF

C --- NO INTERACTION FOR GAMMAS, NEUTRINOS, ELECTRONS, POSITRONS, MUONS,
C --- NEUTRAL PIONS, NEUTRAL SIGMAS AND ANTISIGMAS AND NEW PARTICLES.
      IF ( INTRC(IPART) .EQ. 0 ) GOTO 160
      P=dble(PPART)
      EK=dble(EKIN)
      if(itarg.eq.0)then
        KK       = 3
        WCOMP(1) = dble(airwnxs(1))
        WCOMP(2) = dble(airwnxs(2))
        WCOMP(3) = dble(airwnxs(3))
        ACOMP(1) = dble(airanxs(1))
        ACOMP(2) = dble(airanxs(2))
        ACOMP(3) = dble(airanxs(3))
        ZCOMP(1) = dble(airznxs(1))
        ZCOMP(2) = dble(airznxs(2))
        ZCOMP(3) = dble(airznxs(3))
        atnxs    = dble(airavanxs)
        ztnxs    = dble(airavznxs)
      else
        KK       = 1
        WCOMP(KK)= 1.
        ACOMP(KK)= dble(IATARG)
        ZCOMP(KK)= dble(IZTARG)
        atnxs    = dble(IATARG)
        ztnxs    = dble(IZTARG)
      endif


C --- INITIALIZE THE CROSS-SECTIONS WITH 0.0 ---
      DO  K = 1, KK
        AIEL(K)=0.0D0
        AIIN(K)=0.0D0
        AICA(K)=0.0D0
      ENDDO
C
      IF     ((IPART .GE. 30) .AND. (IPART .LE. 32)) THEN

C --- TAKE GEOMETRICAL CROSS-SECTIONS FOR INELASTIC SCATTERING ---
C --- OF DEUTERONS, TRITONS AND ALPHAS ---
        IF     ( IPART .EQ. 30 ) THEN
          APART=2.0D0**ONETHR
        ELSEIF ( IPART .EQ. 31 ) THEN
          APART=3.0D0**ONETHR
        ELSEIF ( IPART .EQ. 32 ) THEN
          APART=4.0D0**ONETHR
        ENDIF
        DO  K = 1, KK
          AIIN(K)=49.0D0*(APART+ACOMP(K)**ONETHR)**2
        ENDDO
        IF (NPRT(9)) WRITE(MDEBUG,10000)

      ELSEIF ((IPART .EQ. 16) .AND. (EK .LE. 0.0327D0)) THEN

C --- USE TABLES FOR LOW ENERGY NEUTRONS ---
C --- GET ENERGY BIN ---
        JE2=17
        DO  J = 2, 17
          IF (EK .LT. ELAB(J)) THEN
            JE2=J
            GOTO 40
          ENDIF
        ENDDO

   40   JE1=JE2-1
        EKX=MAX(EK,1.0D-9)
        DELAB=ELAB(JE2)-ELAB(JE1)
        DO 70  K = 1, KK
C --- GET A BIN ---
          JA2=15
          DO  J = 2, 15
            IF (ACOMP(K) .LT. CNLWAT(J)) THEN
              JA2=J
              GOTO 60
            ENDIF
          ENDDO
   60     JA1=JA2-1
          DNLWAT=CNLWAT(JA2)-CNLWAT(JA1)

C --- USE LINEAR INTERPOLATION OR EXTRAPOLATION BY Y=RCE*X+RCA*X+B ---

C --- ELASTIC CROSS-SECTION ---
C --- E INTERPOLATION OR EXTRAPOLATION AT JA1 ---
          DY  = CNLWEL(JA1,JE2)-CNLWEL(JA1,JE1)
          RCE = DY/DELAB
C --- A INTERPOLATION OR EXTRAPOLATION AT JE1 ---
          DY  = CNLWEL(JA2,JE1)-CNLWEL(JA1,JE1)
          RCA = DY/DNLWAT
          B   = CNLWEL(JA1,JE1)-RCE*ELAB(JE1)-RCA*CNLWAT(JA1)
          AIEL(K) = RCE*EK+RCA*ACOMP(K)+B

C --- INELASTIC CROSS-SECTION ---
C --- E INTERPOLATION OR EXTRAPOLATION AT JA1 ---
          DY  = CNLWIN(JA1,JE2)-CNLWIN(JA1,JE1)
          RCE = DY/DELAB
C --- A INTERPOLATION OR EXTRAPOLATION AT JE1 ---
          DY  = CNLWIN(JA2,JE1)-CNLWIN(JA1,JE1)
          RCA = DY/DNLWAT
          B   = CNLWIN(JA1,JE1)-RCE*ELAB(JE1)-RCA*CNLWAT(JA1)
          AIIN(K) = RCE*EK+RCA*ACOMP(K)+B
          IZNO    = int(ZCOMP(K)+0.01D0)
          AICA(K) = 11.12D0*CSCAP(IZNO)/(EKX*1.0E6)**0.577D0
   70   CONTINUE
        IF (NPRT(9)) WRITE(MDEBUG,10100)
      ELSE

C --- USE PARAMETRIZATION OF CROSS-SECTION DATA FOR ALL OTHER CASES ---

        IF (NPRT(9)) WRITE(MDEBUG,10200)

C --- GET MOMENTUM BIN ---
        J = 40
        DO  I = 2, 41
          IF (P .LT. PLAB(I)) THEN
            J=I-1
            GOTO 90
          ENDIF
        ENDDO

C --- START WITH  CROSS-SECTIONS FOR SCATTERING ON FREE PROTONS ---
C --- USE LINEAR INTERPOLATION OR EXTRAPOLATION BY Y=RC*X+B     ---
   90   DX  = PLAB(J+1)-PLAB(J)
C --- ELASTIC CROSS-SECTION ---
        DY  = CSEL(IPART,J+1)-CSEL(IPART,J)
        RC  = DY/DX
        B   = CSEL(IPART,J)-RC*PLAB(J)
        AIELIN = RC*P+B
C --- INELASTIC CROSS-SECTION ---
        DY  = CSIN(IPART,J+1)-CSIN(IPART,J)
        RC  = DY/DX
        B   = CSIN(IPART,J)-RC*PLAB(J)
        AIININ = RC*P+B
        ALPH   = ALPHA(IPART)
        IF ( IPART .LT. 14 ) THEN
          DY  = ALPHAC(J+1)-ALPHAC(J)
          RC  = DY/DX
          B   = ALPHAC(J)-RC*PLAB(J)
          CORFAC = RC*P+B
          ALPH   = ALPH*CORFAC

          IPART3 = IPART2(IPART-6)

C --- ELASTIC CROSS-SECTION ---
          DY  = CSEL(IPART3,J+1)-CSEL(IPART3,J)
          RC  = DY/DX
          B   = CSEL(IPART3,J)-RC*PLAB(J)
          XSECEL = RC*P+B
C --- INELASTIC CROSS-SECTION ---
          DY  = CSIN(IPART3,J+1)-CSIN(IPART3,J)
          RC  = DY/DX
          B   = CSIN(IPART3,J)-RC*PLAB(J)
          XSECIN = RC*P+B

        ENDIF

C --- NOW MAKE CROSS-SECTIONS FOR COMPONENT K OF COMPOSITION
        DO 100  K = 1, KK
          AIEL(K) = AIELIN
          AIIN(K) = AIININ

          IF ( ACOMP(K) .GE. 1.5D0 ) THEN

C --- A-DEPENDENCE FROM PARAMETRIZATION ---
            CREL = 1.0D0
            CRIN = 1.0D0
C --- GET MEDIUM BIN  1=HYDR.  2=AL  3=CU  4=PB ---
            I = 3
            IF ( ACOMP(K) .LT. 50.0D0 )  I = 2
            IF ( ACOMP(K) .GT. 100.0D0 ) I = 4
            IF     ((IPART .EQ. 14) .OR. (IPART .EQ. 16)) THEN

C --- PROTONS AND NEUTRONS ---

C --- ELASTIC CROSS-SECTION ---
              DY=CSPNEL(I-1,J+1)-CSPNEL(I-1,J)
              RC=DY/DX
              B=CSPNEL(I-1,J)-RC*PLAB(J)
              XSECEL=RC*P+B
C --- INELASTIC CROSS-SECTION ---
              DY=CSPNIN(I-1,J+1)-CSPNIN(I-1,J)
              RC=DY/DX
              B=CSPNIN(I-1,J)-RC*PLAB(J)
              XSECIN=RC*P+B
              IF (AIEL(K) .GE. 0.001D0) CREL=XSECEL/(0.36D0*AIEL(K)*
     +        CSA(I)**1.17D0)
              AITOT=AIEL(K)+AIIN(K)
              IF (AITOT .GE. 0.001D0) CRIN=XSECIN/(AITOT*CSA(I)**
     +          ALPH)

            ELSEIF (IPART .LT. 15) THEN

C --- CALCULATE CORRECTION FACTORS FROM VALUES ON AL,CU,PB FOR ALL ---
C --- MESONS USE LINEAR INTERPOLATION OR EXTRAPOLATION BY Y=RC*X+B ---
C --- NOTE THAT DATA IS ONLY AVAILABLE FOR PIONS AND PROTONS
              WGCH=0.5D0
              IF (ACOMP(K) .LT. 20.0D0)
     +                         WGCH=0.5D0+0.5D0*EXP(-(ACOMP(K)-1.0D0))
              AIEL(K)=WGCH*AIEL(K)+(1.0D0-WGCH)*XSECEL
              AIIN(K)=WGCH*AIIN(K)+(1.0D0-WGCH)*XSECIN

C --- THIS SECTION NOT FOR KAONS ---
              IF (IPART .LT. 10) THEN

C --- ELASTIC CROSS-SECTION ---
                DY=CSPIEL(I-1,J+1)-CSPIEL(I-1,J)
                RC=DY/DX
                B=CSPIEL(I-1,J)-RC*PLAB(J)
                XSPIEL=RC*P+B
C --- INELASTIC CROSS-SECTION ---
                DY=CSPIIN(I-1,J+1)-CSPIIN(I-1,J)
                RC=DY/DX
                B=CSPIIN(I-1,J)-RC*PLAB(J)
                XSPIIN=RC*P+B

                IF (AIEL(K) .GE. 0.001D0) CREL=XSPIEL/(0.36D0* AIEL(K)
     +               *CSA(I)**1.17D0)
                AITOT=AIEL(K)+AIIN(K)
                IF (AITOT .GE. 0.001D0) CRIN=XSPIIN/(AITOT*CSA(I)
     +               **ALPH)
              ENDIF
            ENDIF
            AIIN(K)=CRIN*(AIIN(K)+AIEL(K))*ACOMP(K)**ALPH
            AIEL(K)=CREL*0.36D0*AIEL(K)*ACOMP(K)**1.17D0
            AIEL(K)=AIEL(K)*PARTEL(IPART)
            AIIN(K)=AIIN(K)*PARTIN(IPART)
          ENDIF
  100   CONTINUE

      ENDIF

C --- CALCULATE INTERACTION PROBABILITY ---

      ALAM=0.0D0
      DO  K = 1, KK
        AIEL(K) = AIEL(K)*WCOMP(K)
        AIIN(K) = AIIN(K)*WCOMP(K)
        AICA(K) = AICA(K)*WCOMP(K)
        ALAM = ALAM  + AIIN(K)!+ AIEL(K) + AICA(K)    !tttt only inelastic cs for MC
      ENDDO

C --- PASS THE CROSS-SECTION (MBARN) TO CORSIKA ---
      GHESIG=ALAM

      GOTO 999

C --- PRINTOUT OF SKIPPED PARTICLES IN CASE OF INTERFACE DEBUG ---
  160 IF (NPRT(9)) WRITE(MDEBUG,10300) IPART
10000 FORMAT(' *GHESIG* GEOM X-SECT. FOR INEL. SCAT. OF D,T AND ALPHA')
10100 FORMAT(' *GHESIG* X-SECT. FROM LOW ENERGY NEUTRON TABLES')
10200 FORMAT(' *GHESIG* X-SECT. FROM PARAMETRIZATION OF DATA')
10300 FORMAT(' *GHESIG* GHEISHA PARTICLE ',I3,' SKIPPED')
  999 RETURN
      END
*CMZ :          28/02/2002  10.19.04  by  D. HECK IK FZK KARLSRUHE
*-- Author :    CERN PROGLIB# M103
C=======================================================================

      SUBROUTINE FLPSOR(A,N)

C-----------------------------------------------------------------------
C CERN PROGLIB# M103    FLPSOR          .VERSION KERNFOR  3.15  820113
C ORIG. 29/04/78
C-----------------------------------------------------------------------
C   SORT THE ONE-DIMENSIONAL FLOATING POINT ARRAY A(1),...,A(N) BY
C   INCREASING VALUES
C
C     PROGRAM  M103  TAKEN FROM CERN PROGRAM LIBRARY,  29-APR-78
C-----------------------------------------------------------------------

      IMPLICIT DOUBLE PRECISION(A-H, O-Z)
      DIMENSION A(*)
      COMMON /SLATE/ LT(20),RT(20)
      INTEGER R,RT
      SAVE
C-----------------------------------------------------------------------

      LEVEL=1
      LT(1)=1
      RT(1)=N
   10 L=LT(LEVEL)
      R=RT(LEVEL)
      LEVEL=LEVEL-1
   20 IF ( R .GT. L ) GOTO 200
      IF ( LEVEL ) 50,50,10
C
C   SUBDIVIDE THE INTERVAL L,R
C     L : LOWER LIMIT OF THE INTERVAL (INPUT)
C     R : UPPER LIMIT OF THE INTERVAL (INPUT)
C     J : UPPER LIMIT OF LOWER SUB-INTERVAL (OUTPUT)
C     I : LOWER LIMIT OF UPPER SUB-INTERVAL (OUTPUT)
C
  200 I=L
      J=R
      M=(L+R)/2
      X=A(M)
  220 IF ( A(I) .GE. X ) GOTO 230
      I=I+1
      GOTO 220
  230 IF ( A(J) .LE. X ) GOTO 231
      J=J-1
      GOTO 230
C
  231 IF ( I .GT. J ) GOTO 232
      W=A(I)
      A(I)=A(J)
      A(J)=W
      I=I+1
      J=J-1
      IF (I .LE. J ) GOTO 220
C
  232 LEVEL=LEVEL+1
      IF ( (R-I) .GE. (J-L) ) GOTO 30
      LT(LEVEL)=L
      RT(LEVEL)=J
      L=I
      GOTO 20
   30 LT(LEVEL)=I
      RT(LEVEL)=R
      R=J
      GOTO 20
   50 RETURN
      END


C=======================================================================

      SUBROUTINE GRNDM(RVEC,LENV)

C-----------------------------------------------------------------------
C  G(HEISHA) R(A)ND(O)M (NUMBER GENERATOR)
C
C  THIS SUBROUTINE IS CALLED FROM GHEISHA ROUTINES.
C  ARGUMENTS:
C   RVEC   = VECTOR FIELD TO BE FILLED WITH RANDOM NUMBERS(REAL)
C   LENV   = LENGTH OF VECTOR (# OF RANDNUMBERS TO BE GENERATED)
C-- Author :    T. PIEROG IK FZK KARLSRUHE   25/04/2003
C-----------------------------------------------------------------------
      common/files/ifop,ifmt,ifch,ifcx,ifhi,ifdt,ifcp,ifdr,ifio
      common/prnt1/iprmpt,ish,ishsub,irandm,irewch,iecho,modsho,idensi

      DOUBLE PRECISION RVEC(*),drangen
      INTEGER          IVEC,LENV
C-----------------------------------------------------------------------

      DO   IVEC = 1, LENV
        RVEC(IVEC) = drangen(dble(IVEC))
      if(irandm.eq.1)write(ifch,*)'RVEC(',IVEC,')= ',RVEC(IVEC)
      ENDDO

      RETURN
      END
