c 25.08.2011 Link routines between FLUKA2011 and EPOS.
c author T. Pierog based on CORSIKA link to FLUKA


c-----------------------------------------------------------------------
      subroutine IniFluka
c-----------------------------------------------------------------------
c Primary initialization for FLUKA
c-----------------------------------------------------------------------
      INCLUDE '(DBLPRC)'  !
      INCLUDE '(DIMPAR)'  !
      INCLUDE '(IOUNIT)'  !
      INCLUDE '(FLKCMP)'  !
C      INCLUDE '(FHEAVY)'
C      INCLUDE '(GENSTK)'
      INCLUDE '(FLKMAT)'  !
C      INCLUDE '(NUCDAT)'
C      INCLUDE '(NUCGEO)'
C      INCLUDE '(PAPROP)'
      INCLUDE '(PAREVT)'  !
C      INCLUDE '(PART2)'
      INCLUDE '(PHNCCM)'  !
C      INCLUDE '(RESNUC)'
      INCLUDE '(CTITLE)'  !
C  FLUKA 2011.2 BLOCK DATA PROGRAMS
      EXTERNAL BDINPT,BDTRNS,BDHDR1,BDHDR2,BDHDR3,BDPART,BDPRDC,
     &         BDNOPT,BDEVAP,BDPREE

      DIMENSION        WHAT (6)
      CHARACTER        SDUM*10
      DATA             WHAT / 6 * ZERZER /

      include 'epos.inc'
      common/eporansto/diu0(100),iiseed(3)
      double precision    diu0
      integer iiseed
cc EPOS common (epos.inc not possible because of conflict with def of pi)
c      integer      ifop,ifmt,ifch,ifcx,ifhi,ifdt,ifcp,ifdr
c      common/files/ifop,ifmt,ifch,ifcx,ifhi,ifdt,ifcp,ifdr
c      integer      iprmpt,ish,ishsub,irandm,irewch,iecho,modsho,idensi
c      common/prnt1/iprmpt,ish,ishsub,irandm,irewch,iecho,modsho,idensi
c      real          rexdifi,rexndii
c      integer       idprojin,idtargin,irdmpr,isoproj,isotarg
c      common/hadr25/idprojin,idtargin,rexdifi(4),rexndii(4),irdmpr,
c     *              isoproj,isotarg
c      real         core,fctrmx
c      integer       laproj,maproj,latarg,matarg
c      common/nucl1/laproj,maproj,latarg,matarg,core,fctrmx
c      real         egymin,egymax,elab,ecms,ekin
c      common/enrgy/egymin,egymax,elab,ecms,ekin
c      real         gaumx
c      integer      istore,istmax,irescl,ntrymx,nclean,iopdg,ioidch
c      common/othe1/istore,istmax,gaumx,irescl,ntrymx,nclean,iopdg,ioidch


      call utpri('iniflu',ish,ishini,6)
      write(ifmt,'(a,i6)')'initialize FLUKA ...'

C open fluka output/error files
      OPEN(UNIT=LUNOUT,FORM='FORMATTED',STATUS='UNKNOWN',
     *     FILE=fnch(1:nfnch-5)//'fkout')
      OPEN(UNIT=LUNERR,FORM='FORMATTED',STATUS='UNKNOWN',
     *     FILE=fnch(1:nfnch-5)//'fkerr')

      CALL CMSPPR
C  CLEAR FLUKA STORAGE AREAS
      ITEXMX = 10000000
      CALL ZEROIN
      LEVPRT = .TRUE.
      LDEEXG = .TRUE.
      LHEAVY = .TRUE.
      IFISS  = 1
      LGDHPR = .TRUE.
C  CHARMED PARTICLES SHOULD NOT DECAY WHEN ORIGINATING
      LCHDCY = .FALSE.

c random numbers initialization
c      INSEED = 1
c      IS1    = iiseed(1)
c      NTOT   = iiseed(2)
c      NTOT2  = iiseed(3)
c      IJKLIN = -1
c      CALL FL64IN ( IS1, 0, NTOT, NTOT2 )
c      CALL FL64WR ( INSEED )

C  SET UP VARIABLES IN COMMON FLKMAT:

      if(idtargin.ne.0)then
        NMAT         = 3        !number of material (not less than 3 !)
        NREGS        = 1         !number of regions
        JMAT         = 3         !index of material (not less than 3 !)
        MEDFLK(1,1)  = JMAT      !define refion material
        ZTAR  (JMAT) = latarg
        AMSS  (JMAT) = matarg
        RHO   (JMAT) = ONEONE
        RHPHNC(JMAT) = ONEONE
        IFPHNC(JMAT) = 1111
        MSSNUM(JMAT) = 0
        AOCMBM(JMAT) = RHO(JMAT) / AMSS(JMAT) * AVOGAD * 1.D-24
        ICOMP (JMAT) = 0
        MATNAM(JMAT) = 'NUCLEUS'
      else
        NREGS        = 4
C  FLUKA MATERIAL NUMBER 1 IS NITROGEN
        MMAT         = 3
        MEDFLK(2,1)  = MMAT
        ZTAR  (MMAT) = 7
        AMSS  (MMAT) = 14.007D+00
        RHO   (MMAT) = ONEONE
        RHPHNC(MMAT) = ONEONE
        IFPHNC(MMAT) = 1111
        MSSNUM(MMAT) = 0
        AOCMBM(MMAT) = RHO(MMAT) / AMSS(MMAT) * AVOGAD * 1.D-24
        ICOMP (MMAT) = 0
        MATNAM(MMAT) = 'NITROGEN'

C  FLUKA MATERIAL NUMBER 2 IS OXYGEN
        IMAT         = 4
        MEDFLK(3,1)  = IMAT
        ZTAR  (IMAT) = 8
        AMSS  (IMAT) = 15.9994D+00
        RHO   (IMAT) = ONEONE
        RHPHNC(IMAT) = ONEONE
        IFPHNC(IMAT) = 1111
        MSSNUM(IMAT) = 0
        AOCMBM(IMAT) = RHO(IMAT) / AMSS(IMAT) * AVOGAD * 1.D-24
        ICOMP (IMAT) = 0
        MATNAM(IMAT) = 'OXYGEN'

C  FLUKA MATERIAL NUMBER 3 IS ARGON
        JMAT         = 5
        MEDFLK(4,1)  = JMAT
        ZTAR  (JMAT) = 18
        AMSS  (JMAT) = 39.948D+000
        RHO   (JMAT) = ONEONE
        RHPHNC(JMAT) = ONEONE
        IFPHNC(JMAT) = 1111
        MSSNUM(JMAT) = 0
        AOCMBM(JMAT) = RHO(JMAT) / AMSS(JMAT) * AVOGAD * 1.D-24
        ICOMP (JMAT) = 0
        MATNAM(JMAT) = 'ARGON'

C  FLUKA MATERIAL NUMBER 4 IS AIR
        NMAT         = 6
        MEDFLK(1,1)  = NMAT
        ICOMP (NMAT) = 1
        ICOMPL(NMAT) = 3
        MATNUM(1)    = 3
        MATNUM(2)    = 4
        MATNUM(3)    = 5
        RHO   (NMAT) = ONEONE
        RHPHNC(NMAT) = ONEONE
        IFPHNC(NMAT) = 1111
        CONTNT(1)    = 0.92561D-03
        CONTNT(2)    = 0.28361D-03
        CONTNT(3)    = 0.15776D-04
        RENORM       = RHO(NMAT) / ( CONTNT(1) + CONTNT(2) + CONTNT(3) )
        CONTNT(1)    = CONTNT (1) * RENORM
        CONTNT(2)    = CONTNT (2) * RENORM
        CONTNT(3)    = CONTNT (3) * RENORM
        MATNAM(NMAT) = 'AIR'
      endif
C
      CALL EVVINI( WHAT,SDUM )
      CALL SETITB



      egymin=1.076 !min cms energy using pion as proj (min ekin=0.01)
      egymax=1e4
      irescl=0
      

      call utprix('iniflu',ish,ishini,6)
      end

c-----------------------------------------------------------------------
      subroutine IniEvtFlu
c-----------------------------------------------------------------------
c Initialization for each type of event (for given proj, targ and egy)
c-----------------------------------------------------------------------
      include 'epos.inc'
      real rmproj,rmtarg,bmax,bkmx
      common/geom/rmproj,rmtarg,bmax,bkmx


      if(maproj.gt.1)
     &  call utstop('Mass too big for Fluka (Mprj=1) !&')
      if(abs(idtarg).ne.1120.and.abs(idtarg).ne.0)
     &  call utstop('target no allowed in Fluka !&')
      if(bminim.gt.0.or.bmaxim.lt.1000)
     &  write(ifmt,*)'Only min bias event in Fluka ... no b !'
      call iclass(idproj,iclpro)
      bminim=0.
      bmaxim=10000.
      bmax=10.+maproj+matarg


      fluincs=fflucrse()
      if(engy.lt.egymin)fluincs=0.          !below egymin, no interaction
c      if(iclpro.eq.3.and.ekin.lt.0.1d0)fluincs=0.     !elastic event for kaons below 100MeV (problem) !
c initialize cross-sections
      call xsigma

      if(ish.ge.2)write(ifch,*)
     &  'Fluka used with (Ek,idproj,idtarg,xs) ',ekin,idproj,idtarg
     &                                          ,fluincs


      return
      end

c-----------------------------------------------------------------------
      subroutine emsflu(iret)
c-----------------------------------------------------------------------
c  call Fluka to simulate interaction
c-----------------------------------------------------------------------
      INCLUDE '(DBLPRC)'   !
      INCLUDE '(DIMPAR)'   !
C      INCLUDE '(IOUNIT)'
C      INCLUDE '(FLKCMP)'
      INCLUDE '(FHEAVY)'
      INCLUDE '(GENSTK)'   !
C      INCLUDE '(FLKMAT)'
C      INCLUDE '(NUCDAT)'
C      INCLUDE '(PAPROP)'
      INCLUDE '(PAREVT)'   !
      INCLUDE '(PART2)'    !
C      INCLUDE '(PHNCCM)'
      INCLUDE '(RESNUC)'    !
      include 'epos.inc'
c      integer   mmry,mxptl
c      parameter (mmry=1)   !memory saving factor
c
c      parameter (mxptl=200000/mmry) !max nr of particles in epos ptl list
c      real         engy,elepti,elepto,angmue
c      integer      icinpu
c      common/lept1/engy,elepti,elepto,angmue,icinpu
c      real         egymin,egymax,elab,ecms,ekin
c      common/enrgy/egymin,egymax,elab,ecms,ekin
c      real         pnll,ptq,exmass,cutmss,wproj,wtarg
c      common/hadr1/pnll,ptq,exmass,cutmss,wproj,wtarg
c      real         core,fctrmx
c      integer       laproj,maproj,latarg,matarg
c      common/nucl1/laproj,maproj,latarg,matarg,core,fctrmx
c      real         anintdiff,anintsdif,anintine
c     *,sigineex,sigdifex,sigsdex
c      common/cdiff/anintdiff,anintsdif,anintine
c     *,sigineex,sigdifex,sigsdex
c
c      integer      ifop,ifmt,ifch,ifcx,ifhi,ifdt,ifcp,ifdr
c      common/files/ifop,ifmt,ifch,ifcx,ifhi,ifdt,ifcp,ifdr
c      integer      iprmpt,ish,ishsub,irandm,irewch,iecho,modsho,idensi
c      common/prnt1/iprmpt,ish,ishsub,irandm,irewch,iecho,modsho,idensi
c      real         pi,pii,hquer,prom,piom,ainfin 
c      common/cnsta/pi,pii,hquer,prom,piom,ainfin 
c      real         bmaxim,bminim,phimax,phimin
c      common/nucl2/bmaxim,bminim,phimax,phimin
c      real            fluincs
c      common/mod9incs/fluincs
c      integer      iomodl,idproj,idtarg
c      real         wexcit
c      common/hadr2/iomodl,idproj,idtarg,wexcit
c      real        phievt,bimevt,pmxevt,egyevt
c     *,xbjevt,qsqevt,zppevt,zptevt
c      integer     nevt,kolevt,koievt,npjevt
c     *,ntgevt,npnevt,nppevt,ntnevt,ntpevt,jpnevt,jppevt,jtnevt,jtpevt
c     *,nglevt,minfra,maxfra
c      common/cevt/phievt,nevt,bimevt,kolevt,koievt,pmxevt,egyevt,npjevt
c     *,ntgevt,npnevt,nppevt,ntnevt,ntpevt,jpnevt,jppevt,jtnevt,jtpevt
c     *,xbjevt,qsqevt,nglevt,zppevt,zptevt,minfra,maxfra
      real rmproj,rmtarg,bmax,bkmx
      common/geom/rmproj,rmtarg,bmax,bkmx
      real a,rangen,b1,b2,rrr
      double precision P0(5),P1,EKIN1,PPROJ,TXX,TYY,TZZ,WEE,POO,PRES(4)

      iret=0
      np=0
      b1=bminim
      b2=min(bmax,bmaxim)
      a=pi*(b2**2-b1**2)
      rrr=rangen()
      if(fluincs.le.1.e-3)a=-1.    !elastic event !
      if(a.le.0.)goto 1002  !no interaction
      if(rangen().gt.fluincs/10./a)goto 1001   !no interaction
      if(ish.ge.3)call alist('Determine Fluka Production&',0,0)


C  CONVERT PARTICLE TYPE TO FLUKA
      KPROJ  = idtrafo('nxs','flk',idproj)
      EKIN1  = dble(ekin)
C  CALCULATE MOMENTUM PPROJ
      PPROJ = SQRT( EKIN1 * (EKIN1 + 2.D00 * AAM(IPTOKP(KPROJ)) ) )
      LEVFIN = .FALSE.
C  TARGET
      IF ( idtargin .ne.  0 )  THEN
        MMMAT = 3
      elseif(latarg.eq.7)then
        MMMAT = 3
      elseif(latarg.eq.8)then
        MMMAT = 4
      elseif(latarg.eq.18)then
        MMMAT = 5
      else
        call utstop('Wrong mass in air component (FLUKA) !&')
      endif
C  USE THE FLUKA-INTERNALLY USED DIRECTION COSINES:
      TXX = 0.D0
      TYY = 0.D0
      TZZ = 1.D0
      WEE = ONEONE
      IJ  = KPROJ
C  CALCULATE THE MOMENTUM WITH FLUKA MASSES
      POO = PPROJ
C  RESET STACK INDEX
      NP=0
      NP0=0
C  NOW INTERACTION IS PERFORMED
      CALL EVENTV( IJ,POO,EKIN1,TXX,TYY,TZZ,WEE,MMMAT )
c      write(ifch,*)'target used',ibtar,ichtar,mmmat,ibres
      
      nptl=0
      matarg=IBTAR    !update mass in case of isotope
      
      call conre
      call conwr

      if(NP.eq.0.and.NPHEAV.eq.0)goto 1001         !no interaction

      nevt=1
      pmxevt=pnll
      egyevt=engy
      bimevt=0.
      bimp=0.
      phievt=0.
      phi=0.
      kolevt=-1    !information not defined
      koievt=-1
      kohevt=-1
      npjevt=maproj
      ntgevt=matarg
      npns=0               !counter to be defined later
      npps=0
      ntns=0
      ntps=0

      anintine=anintine+1.

c set projectile and target as non final particles
      do i=1,maproj
        istptl(i)=1
      enddo
      do j=1,matarg
        istptl(j)=1
      enddo

C  NOW TREAT THE REMAINING TARGET NUCLEUS

      if(ish.ge.5)write(ifch,550)IBRES,ICRES,EKRES 
 550  format('Fluka: remaining nucleus',/,
     *                        '  mass=',I3,' charge=',I3,
     *                             ' ekin=',g9.2)
      if(IBRES.gt.1)then

        if(IBRES.gt.matarg)then
          write(ifmt,*)'emsflu:more nucleons in target remnant than '
     *                ,'in target ! ... skip event.'
          goto 1001
        endif
        
        PRES(4)=sqrt(PXRES*PXRES+PYRES*PYRES+PZRES*PZRES)
        if(PRES(4).gt.0d0)then
          PRES(3)=PZRES/PRES(4) !Pz
          PRES(2)=PYRES/PRES(4) !Py
          PRES(1)=PXRES/PRES(4) !Px
        elseif(abs(ekin1).lt.1d-6)then
          PRES(3)=0.d0 !Pz
          PRES(2)=0.d0 !Py
          PRES(1)=0.d0 !Px
        else         !ekin and momentum not consistent
          write(ifmt,*)'emsflu: ekin and momentum not consistent !'
     *                ,' ... skip event.'
          goto 1001
        endif
        ntps=ICRES
        ntns=IBRES-ICRES
        
        if(infragm.gt.0)then

          ekin1=EKRES
c final particle is a nucleus
          nptl=nptl+1           
          istptl(nptl)=0
          if(nptl.gt.mxptl)call utstop('Fluka: mxptl too small&')

          id=1000000000+ICRES*10000+IBRES*10 !code for nuclei

          P0(5)=AAM(1)*ICRES+AAM(8)*(IBRES-ICRES) !Mass
            
          P0(4)=ekin1+P0(5)                   !Energy
          P1=sqrt((P0(4)+P0(5))*(P0(4)-P0(5))) !Momentum
          P0(3)=PRES(3)*P1                      !Pz
          P0(2)=PRES(2)*P1                      !Py
          P0(1)=PRES(1)*P1                      !Px

          call flk2epo(id,P0,1)


        else

          ekin1=EKRES/dble(IBRES)
          do k=1,IBRES
          
c final particles are free nucleons
          nptl=nptl+1           
          istptl(nptl)=0
          if(nptl.gt.mxptl)call utstop('Fluka: mxptl too small&')

          if(k.le.ICRES)then   !protons
            id=1120
            P0(5)=AAM(1)        !Mass
          else                  !neutrons
            id=1220
            P0(5)=AAM(8)        !Mass
          endif
            
          P0(4)=ekin1+P0(5)                   !Energy
          P1=sqrt((P0(4)+P0(5))*(P0(4)-P0(5))) !Momentum
          P0(3)=PRES(3)*P1                      !Pz
          P0(2)=PRES(2)*P1                      !Py
          P0(1)=PRES(1)*P1                      !Px

          call flk2epo(id,P0,1)

          enddo

        endif

      endif

        if(ish.ge.5)write(ifch,'(a,i5)')
     $         ' number of particles from Fluka :',NP
        do 500 k=1,NP

c LLIST is the code of final particle, P - its 4-momentum and mass.
          ic=KPART(k)
          icm=IPTOKP(ic)          !convert to internal FLUKA ID to use AAM
            
          if(ish.ge.7)write(ifch,'(a,i5,a,i5,2a,5(e10.4,1x))')
     $    ' FLUKA particle ',k,' id :',ic,' before conversion'
     $  , ' Ekin, cos and mass :',TKI(k),CXR(k),CYR(k),CZR(k),AAM(icm)

          if(ic.eq.-5)then      !send He-3 in fragments
            NPHEAV=NPHEAV+1
            if(NPHEAV.gt.MXHEAV)goto 1001   !no room left, skip event
            KHEAVY(NPHEAV)=5
            TKHEAV(NPHEAV)=TKI(k)
            CXHEAV(NPHEAV)=CXR(k)
            CYHEAV(NPHEAV)=CYR(k)
            CZHEAV(NPHEAV)=CZR(k)
            goto 500
          endif

c final particle
          nptl=nptl+1
          istptl(nptl)=0
          if(nptl.gt.mxptl)call utstop('Fluka: mxptl too small&')


          id=idtrafo('flk','nxs',ic)
          if(ish.ge.7)write(ifch,'(a,i5,a,i5,a)')
     $         ' epos particle ',nptl,' id :',id,' after conversion'
            
          P0(5)=AAM(icm)                         !Mass
          P0(4)=TKI(k)+P0(5)                   !Energy
          P1=sqrt((P0(4)+P0(5))*(P0(4)-P0(5))) !Momentum
          P0(3)=CZR(k)*P1                      !Pz
          P0(2)=CYR(k)*P1                      !Py
          P0(1)=CXR(k)*P1                      !Px

          call flk2epo(id,P0,0)

 500      continue


C  NOW TREAT THE HEAVY FRAGMENTS

        if(ish.ge.5)write(ifch,'(a,i5)')
     $         ' number of fragments from Fluka :',NPHEAV

        do j=1,NPHEAV

c LLIST is the code of final particle, P - its 4-momentum and mass.
         ic=KHEAVY(J)
            
         if(ish.ge.7)write(ifch,'(a,i5,a,i5,2a,4(e10.4,1x))')
     $       ' FLUKA fragment ',j,' id :',ic,' before conversion'
     $     , ' Ekin and cos :',TKHEAV(J),CXHEAV(J),CYHEAV(J),CZHEAV(J)
         IF ( ic .LE. 0 .OR. ic .gt.12 ) THEN
              if (ish .ge.1)WRITE(ifmt,*)
     *          'emsflu: WRONG FRAGMENT: KHEAVY=',KHEAVY(J)
           GOTO 1001
         ENDIF
         iz  = ICHEAV(ic)
         ia  = IBHEAV(ic)
         
         ekin1=TKHEAV(J)/dble(ia)
         PRES(3)=CZHEAV(J)      !Pz
         PRES(2)=CYHEAV(J)      !Py
         PRES(1)=CXHEAV(J)      !Px

c count spectators
         if(PRES(3).gt.sqrt(pnll))then   !projectile
           npns=npns+ia-iz
           npps=npps+iz
         else                      !target
           ntns=ntns+ia-iz
           ntps=ntps+iz
         endif

         if(ia.eq.2.and.iz.eq.1)then       !deuterium
           id=17
           P0(5)=AAM(-3)        !Mass
           ia=1
           iz=-1                !no to update the id and the mass in the loop
         elseif(ia.eq.3.and.iz.eq.1)then   !tritium
           id=18
           P0(5)=AAM(-4)        !Mass
           ia=1
           iz=-1                !no to update the id and the mass in the loop
         elseif(ia.eq.4.and.iz.eq.2)then   !helium
           id=19
           P0(5)=AAM(-6)        !Mass
           ia=1
           iz=-1                !no to update the id and the mass in the loop
         endif                  !other nucleus

         do k=1,ia
c final particle
          nptl=nptl+1
          if(k.le.iz)then       !protons
            id=1120
            P0(5)=AAM(1)        !Mass
          elseif(iz.ge.0)then   !neutrons
            id=1220
            P0(5)=AAM(8)        !Mass
          endif
            
          istptl(nptl)=0
          if(nptl.gt.mxptl)call utstop('Fluka: mxptl too small&')


          if(ish.ge.7)write(ifch,'(a,i5,a,i5,a)')
     $         ' epos particle ',nptl,' id :',id,' after conversion'
            
          P0(4)=ekin1+P0(5)                    !Energy
          P1=sqrt((P0(4)+P0(5))*(P0(4)-P0(5))) !Momentum
          P0(3)=PRES(3)*P1                      !Pz
          P0(2)=PRES(2)*P1                      !Py
          P0(1)=PRES(1)*P1                      !Px

          call flk2epo(id,P0,0)
          
         enddo

        enddo

c number of participants
      if(laproj.gt.1)then
        npjevt=maproj-npps-npns
        npppar=laproj-npps
        npnpar=npjevt-npppar
c set participant projectile and target as non spectators
        do i=1,maproj
          if(idptl(i).eq.1120)then
            if(npppar.gt.0)then
              npppar=npppar-1
            else                !restore spectators
              iorptl(i)=0
            endif
          endif
          if(idptl(i).eq.1220)then
            if(npnpar.gt.0)then
              npnpar=npnpar-1
            else                !restore spectators
              iorptl(i)=0
            endif
          endif
        enddo
      endif
      if(latarg.gt.1)then
        ntgevt=matarg-ntps-ntns
        ntppar=latarg-ntps
        ntnpar=ntgevt-ntppar
        do j=maproj+1,maproj+matarg
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
     $         ' End of Fluka event :',NP,iret
      return

1001  iret=-1 
      goto 1000 

 1002 iret=-2      !elastic
      goto 1000 

      end

c------------------------------------------------------------------------------
      subroutine flk2epo(id,P0,ir)
c------------------------------------------------------------------------------
c fill EPOS stack with particle from FLUKA
c P0 particle momentum
c id of particle
c ir =1 for remnant
c------------------------------------------------------------------------------
      include 'epos.inc'
      double precision P0(5)

      pptl(1,nptl)=sngl(P0(1))  !P_x
      pptl(2,nptl)=sngl(P0(2))  !P_y
      pptl(3,nptl)=sngl(P0(3))  !P_z
      pptl(4,nptl)=sngl(P0(4))  !E
      pptl(5,nptl)=sngl(P0(5))  !mass
      ityptl(nptl)=0
      if(ir.eq.0)then
        iorptl(nptl)=1
        jorptl(nptl)=maproj
      else
        iorptl(nptl)=maproj+1
        jorptl(nptl)=maproj+matarg
      endif
      ifrptl(1,nptl)=0
      ifrptl(2,nptl)=0
      xorptl(1,nptl)=0.d0
      xorptl(2,nptl)=0.d0
      xorptl(3,nptl)=0.d0
      xorptl(4,nptl)=0.d0
      tivptl(1,nptl)=0.
      tivptl(2,nptl)=0.
      idptl(nptl)=id
          
c Put particle in cms frame.
      call utlob5(yhaha, pptl(1,nptl), pptl(2,nptl)
     *     , pptl(3,nptl), pptl(4,nptl), pptl(5,nptl))
            
      if(ish.ge.5)write(ifch,'(a,i5,a,i5,a,4(e10.4,1x),f6.3)')
     $         ' particle from Fluka ',nptl,' id :',idptl(nptl)
     $         , ' momentum :',(pptl(i,nptl),i=1,5)

      return
      end

c------------------------------------------------------------------------------
      real function fflucrse()
c------------------------------------------------------------------------------
c hadron-proton particle production cross section with Fluka.
c ekin0 - kinetic energy
c plab0 - particle momentum
c------------------------------------------------------------------------------
      include 'epos.inc'
      double precision EKIN1,PPROJ,SIGREA,ZLDUM
C-----------------------------------------------------------------------

C  CONVERT PARTICLE TYPE TO FLUKA
      KPROJ  = idtrafo('nxs','flk',idproj)
      EKIN1  = dble(ekin)
      PPROJ = dble(pnll)
C  MATERIAL NUMBER 1 IS CURRENT MATERIAL
      MMAT  = 3
      if(idtargin.eq.0)MMAT=6     !for air
C  GET THE CROSS SECTION: SIGREA IS MICROSCOPIC CROSS SECTION (MB)
C  ZLDUM IS MACROSCOPIC CROSS SECTION (CM^-1)
      CALL SIGINM( KPROJ,MMAT,EKIN1,PPROJ,SIGREA,ZLDUM )
      fflucrse= sngl(SIGREA)

      return
      end



c------------------------------------------------------------------------------
      function flucrse(ek,mapro,matar,id)
c------------------------------------------------------------------------------
c inelastic cross section of FLUKA
c if id=0, target = air
c ek - kinetic energy in GeV
c maproj - projec mass number     (1<maproj<64)
c matarg - projec mass number
c id - target id (0=air)
c------------------------------------------------------------------------------
      include 'epos.inc'
      double precision SINE,AMPRO,AMTAR
      flucrse=0d0
      lapro=max(1,mapro/2)
      latar=max(1,matar/2)
      if(id.eq.0)then
        do k=1,3
          mt=int(airanxs(k))
          lt=max(1,mt/2)
          call SIGIAA(lapro,mapro,AMPRO,lt,mt,AMTAR,dble(ek),SINE)
          flucrse=flucrse+airwnxs(k)*sngl(SINE)
        enddo
      else
        call SIGIAA(lapro,mapro,AMPRO,latar,matar,AMTAR,dble(ek),SINE)
        flucrse=sngl(SINE)
      endif
      return
      end

C=======================================================================

      DOUBLE PRECISION FUNCTION FLRNDM()

C-----------------------------------------------------------------------
C  FL(UKA) R(A)ND(O)M (GENERATOR)
C  THIS FUNCTON IS CALLED FROM FLUKA ROUTINES.
C-----------------------------------------------------------------------
      double precision dum,drangen
      include 'epos.inc'
      flrndm=drangen(dum)
      if(irandm.eq.1)write(ifch,*)'flrndm()= ',flrndm

      RETURN
      END
 
C=======================================================================

      SUBROUTINE ZEREMF

C-----------------------------------------------------------------------
C  THIS DUMMY SUBROUTINE IS NECESSARY TO OVERRIDE A FLUKA SUBROUTINE
C  WITH IDENTICAL NAME WHICH OTHERWISE WOULD ERASE SOME EPOS COMMONS.
C  THIS SUBROUTINE IS CALLED FROM ZEROIN.
C-----------------------------------------------------------------------

      INCLUDE '(DBLPRC)'
      INCLUDE '(DIMPAR)'
      INCLUDE '(IOUNIT)'
C-----------------------------------------------------------------------

      RETURN
      END

