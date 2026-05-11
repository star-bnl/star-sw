
      SUBROUTINE PHO_EVENT(Nev,P1,P2,Fac,Irej)
C********************************************************************
C
C     main subroutine to manage simulation processes
C
C     input: NEV       -1   initialization
C                       1   generation of events
C                       2   generation of events without rejection
C                           due to energy dependent cross section
C                       3   generation of events without rejection
C                           using initialization energy
C                      -2   output of event generation statistics
C            P1(4)     momentum of particle 1 (internal TARGET)
C            P2(4)     momentum of particle 2 (internal PROJECTILE)
C            FAC       used for initialization:
C                      contains cross section the events corresponds to
C                      during generation: current cross section
C
C     output: IREJ     0: event accepted
C                      1: event rejected
C
C********************************************************************
      IMPLICIT NONE
      DOUBLE PRECISION DEPS , DT_RNDM , ecmsum , Fac , fac1 , fac2 , 
     &                 P1 , P2 , sqs , TINY , wg, initsqs
      INTEGER i , idia , idis , idna , idns , idpa , idps , ienacc , 
     &        ihpa , ihps , ipracc , iprsam , Irej , isla , isls , 
     &        ispa , isps , isra , isrs , ista
      INTEGER ists , itry1 , itry2 , jm1 , jm2 , k , kevgen , m , Nev , 
     &        nevhep
      SAVE 
 
      PARAMETER (TINY=1.D-10)
 
      DIMENSION P1(4) , P2(4)
 
C  input/output channels
      INCLUDE 'inc/poinou'
C  event debugging information
      INCLUDE 'inc/podebg'
C  model switches and parameters
      INCLUDE 'inc/pomdls'
C  current beam selection
      INCLUDE 'inc/pobeam'
C  general process information
      INCLUDE 'inc/poprcs'
C  internal rejection counters
      INCLUDE 'inc/poloop'
C  gamma-lepton or gamma-hadron vertex information
      INCLUDE 'inc/pofsrc'
C  global event kinematics and particle IDs
      INCLUDE 'inc/pogcms'
C  cross sections
      INCLUDE 'inc/pocsec'
C  event weights and generated cross section
      INCLUDE 'inc/powght'
C  names of hard scattering processes
      INCLUDE 'inc/pohpro'
C  hard cross sections and MC selection weights
      INCLUDE 'inc/pohrcs'
C  table of particle indices for recursive PHOJET calls
      INCLUDE 'inc/porecu'
Cf2py intent(out) fac, irej
 
      DIMENSION iprsam(10) , ipracc(10) , ienacc(10) , idns(4) , idna(4)
 
      PARAMETER (DEPS=1.D-14)
      LOGICAL init
      DATA init/.FALSE./
 
      Irej = 0
      Fac = 0.D0
C  initializations
      IF ( Nev.EQ.-1 ) THEN
         IF ( init ) THEN
            WRITE (LO,'(/1X,2A)') 'PHO_EVENT: Error, initialzation' , 
     &             ' (-1) called twice.'
            CALL PHO_ABORT
         END IF
 
         IF ( LPRi.GT.4 ) WRITE (LO,'(/3(/1X,A))')
     &         '======================================================='
     &        , '  ------- initialization of event generation --------'
     &        , 
     &        '======================================================='
 
C**anfe Safety margin for initialization energy is 20%
         
         initsqs = 1.2D0*SQRT((P1(4)+P2(4))**2-(P1(1)+P2(1))
     &                **2-(P1(2)+P2(2))**2-(P1(3)+P2(3))**2)
         IF (SQSGlobMax.LE.initsqs) SQSGlobMax = initsqs
         
         CALL PHO_SETMDL(0,0,-2)
 
         CALL PHO_REJSTA(-1)
C  cross section
         Fac = SIGgen(4)
         DO i = 1 , 10
            iprsam(i) = 0
            ipracc(i) = 0
            ienacc(i) = 0
         END DO
         isps = 0
         ispa = 0
         isrs = 0
         isra = 0
         ihps = 0
         ihpa = 0
         ists = 0
         ista = 0
         isls = 0
         isla = 0
         idis = 0
         idia = 0
         idps = 0
         idpa = 0
         idns(1) = 0
         idns(2) = 0
         idns(3) = 0
         idns(4) = 0
         idna(1) = 0
         idna(2) = 0
         idna(3) = 0
         idna(4) = 0
         KACcep = 0
         KEVent = 0
         kevgen = 0
         ecmsum = 0.D0
 
         init = .TRUE.
 
      ELSE IF ( Nev.GT.0 ) THEN
         nevhep = KACcep
         sqs = SQRT((P1(4)+P2(4))**2-(P1(1)+P2(1))**2-(P1(2)+P2(2))
     &         **2-(P1(3)+P2(3))**2)
         IF ( (sqs.GT.DEPS) .AND. (sqs.GE.SQSglobmax) ) THEN
            WRITE (LO,*) 
     &      'PHO_EVENT: Error: Higher energy requested then sqsglobmax.'
     &      , sqs , '>' , SQSglobmax
            CALL PHO_ABORT
         END IF
C
C  -------------- begin event generation ---------------
C
         IPAmdl(13) = 0
         IF ( Nev.EQ.3 ) IPAmdl(13) = 1
         KEVent = KEVent + 1
C  enable debugging
         CALL PHO_TRACE(0,0,0)
         IF ( IDEb(68).GE.2 ) THEN
            IF ( (MOD(KEVent,50).EQ.0) .OR. (IDEb(68).GE.3) .AND. 
     &           LPRi.GT.4 ) WRITE (LO,'(1X,A,2I12)')
     &            'call to PHO_EVENT no' , KEVent , KACcep
         END IF
C  initialize MC package
         CALL PHO_SETPCOMB
         CALL PHO_EVEINI(0,P1,P2,jm1,jm2)
 
C  cross section calculation
         Fac = SIGgen(3)
         IF ( Nev.EQ.1 ) THEN
            IF ( IVWght(1).EQ.1 ) THEN
               wg = EVWght(1)*SIGgen(3)/SIGgen(4)
            ELSE
               wg = SIGgen(3)/SIGgen(4)
            END IF
            IF ( DT_RNDM(Fac).GT.wg ) THEN
               Irej = 1
               IF ( IDEb(68).GE.6 ) THEN
                  IF ( LPRi.GT.4 )
     &                  WRITE (LO,'(1X,2A,/5X,2I10,6X,1P3E10.3)')
     &                  'PHO_EVENT: rejection due to cross section' , 
     &                 ' (CALL/ACC/EVWGHT(1)/SIG/SIGMAX)' , KEVent , 
     &                 KACcep , EVWght(1) , SIGgen(3) , SIGgen(4)
                  CALL PHO_PREVNT(-1)
               END IF
               RETURN
            END IF
         END IF
         kevgen = kevgen + 1
         SIGgen(1) = SIGgen(4)*DBLE(kevgen)/DBLE(KEVent)
         HSWght(0) = MAX(1.D0,wg)
 
         itry1 = 0
 50      itry1 = itry1 + 1
         IF ( itry1.GT.1 ) CALL PHO_EVEINI(2,P1,P2,jm1,jm2)
 
C  sample process
         IPRoce = 0
         CALL PHO_SAMPRO(1,IFPap(1),IFPap(2),ECM,PVIrt(1),PVIrt(2),1.D0,
     &                   IPRoce)
         IF ( IPRoce.EQ.0 ) THEN
            IF ( LPRi.GT.4 .AND. IDEb(68).GE.4 ) WRITE (LO,'(1X,A)')
     &            'PHO_EVENT: ' , 'rejection by PHO_SAMPRO (call,Ecm)' , 
     &           KEVent , ECM
            Irej = 50
            RETURN
         END IF
C  sampling statistics
         iprsam(IPRoce) = iprsam(IPRoce) + 1
 
         itry2 = 0
 100     itry2 = itry2 + 1
         IF ( itry2.GT.1 ) CALL PHO_EVEINI(2,P1,P2,jm1,jm2)
C  sample number of cut graphs according to IPROCE and
C  generate parton configurations+strings
         CALL PHO_PARTON(IPRoce,jm1,jm2,P1,P2,Irej)
C  collect statistics
         isps = isps + KSPom
         ihps = ihps + KHPom
         isrs = isrs + KSReg
         ists = ists + KSTrg + KHTrg
         isls = isls + KSLoo + KHLoo
         idis = idis + MIN(KHDir,1)
         idps = idps + KHDpo + KSDpo
         IF ( (IDIfr1+IDIfr2+IDDpom.EQ.0) .AND. (KHDir.GT.0) )
     &        idns(KHDir) = idns(KHDir) + 1
C  rejection?
         IF ( Irej.NE.0 ) THEN
            IF ( IDEb(68).GE.4 ) THEN
               IF ( LPRi.GT.4 ) WRITE (LO,'(/1X,A,2I5)')
     &               'PHO_EVENT: rejection by PHO_PARTON' , itry2 , Irej
               CALL PHO_PREVNT(-1)
            END IF
            IF ( (Irej.EQ.50) .AND. (Nev.EQ.1) ) RETURN
            IFAil(1) = IFAil(1) + 1
            IF ( itry1.GT.5 ) RETURN
            IF ( Irej.GE.5 ) THEN
               IF ( ISWmdl(2).EQ.0 ) RETURN
               GOTO 50
            END IF
            IF ( itry2.LT.5 ) GOTO 100
            GOTO 50
         END IF
C  fragmentation of strings
 
C  fragmentation of strings
C  In DPMJET case FSR and string fragmentation is done separately
         IF ( IPAmdl(13).EQ.0 ) THEN
            CALL PHO_STRFRA(Irej)
            IF ( Irej.NE.0 ) THEN
               IFAil(23) = IFAil(23) + 1
               IF ( (LPRi.GT.4) .AND. (IDEb(68).GE.4) ) THEN
                  WRITE (LO,'(/1X,A,2I5)')
     &                    'PHO_EVENT: rejection by PHO_STRFRA' , itry2 , 
     &                   Irej
                  CALL PHO_PREVNT(-1)
               END IF
               GOTO 50
            END IF
         END IF
 
C  check of conservation of quantum numbers
         IF ( IDEb(68).GE.-5 ) THEN
            CALL PHO_CHECK(-1,Irej)
            IF ( Irej.NE.0 ) GOTO 50
         END IF
C  event now completely processed and accepted
C  acceptance statistics
         ipracc(IPRoce) = ipracc(IPRoce) + 1
         ispa = ispa + KSPom
         ihpa = ihpa + KHPom
         isra = isra + KSReg
         ista = ista + (KSTrg+KHTrg)
         isla = isla + (KSLoo+KHLoo)
         idia = idia + MIN(KHDir,1)
         idpa = idpa + KHDpo + KSDpo
         IF ( (IDIfr1+IDIfr2.EQ.0) .AND. (KHDir.GT.0) ) idna(KHDir)
     &        = idna(KHDir) + 1
         DO i = 1 , IPOix2
            IF ( IPOres(i).EQ.0 ) THEN
               IF ( (LPRi.GT.4) .AND. (IDEb(67).GE.10) ) WRITE (6,*)
     &               'PHO_EVENT: diffraction statistics problem'
               GOTO 150
            END IF
            ienacc(IPOres(i)) = ienacc(IPOres(i)) + 1
 150     END DO
         KACcep = KACcep + 1
 
C  debug output (partial / full event listing)
         IF ( (IDEb(68).EQ.1) .AND. (MOD(KACcep,50).EQ.0) .AND. 
     &        LPRi.GT.4 ) WRITE (LO,'(1X,A,2I12)')
     &         'call to PHO_EVENT no' , KEVent , KACcep
         IF ( IDEb(67).GE.10 ) THEN
            IF ( IDEb(67).LE.15 ) THEN
               CALL PHO_PREVNT(-1)
            ELSE IF ( IDEb(67).LE.20 ) THEN
               CALL PHO_PREVNT(0)
            ELSE IF ( IDEb(67).LE.25 ) THEN
               CALL PHO_PREVNT(1)
            ELSE
               CALL PHO_PREVNT(2)
            END IF
         END IF
C
C  effective weight
         DO i = 1 , 10
            IF ( IPOwgc(i).GT.0 ) HSWght(0) = HSWght(0)*HSWght(i)
         END DO
         IF ( IVWght(1).EQ.1 ) THEN
            wg = HSWght(0)
            IF ( wg.GT.1.01D0 ) THEN
               IF ( EVWght(1).LT.1.01D0 ) THEN
                  IF ( LPRi.GT.4 ) WRITE (LO,'(1X,A,2I12,1PE12.3)')
     &                  'PHO_EVENT: cross section weight > 1' , KEVent , 
     &                 KACcep , wg
                  IF ( LPRi.GT.4 ) WRITE (LO,'(5X,A,1P3E11.3)')
     &                  'SIGCUR,SIGMAX,EVWGHT(1):' , SIGgen(3) , 
     &                 SIGgen(4) , EVWght(1)
               END IF
               EVWght(1) = HSWght(0)
               HSWght(0) = 1.D0
            ELSE
               EVWght(1) = 1.D0
            END IF
         END IF
 
C  effective cross section
         SIGgen(2) = SIGgen(4)*DBLE(KACcep)/DBLE(KEVent)
         ecmsum = ecmsum + ECM
         SIGgen(3) = SIGgen(3)*HSWght(0)
      ELSE IF ( Nev.EQ.-2 ) THEN
 
C  ---------------- end of event generation ----------------------
 
         IF ( LPRi.GT.4 )
     &         WRITE (LO,'(/3(/1X,A),//1X,A,3I12,/1X,A,F12.1)')
     &         '====================================================' , 
     &        '  --------- summary of event generation ----------' , 
     &        '====================================================' , 
     &        'called,generated,accepted events:' , KEVent , kevgen , 
     &        KACcep , 'average CMS energy:' , 
     &        ecmsum/DBLE(MAX(1,KACcep))
 
C  write out statistics
         IF ( KACcep.GT.0 ) THEN
 
            fac1 = SIGgen(4)/DBLE(KEVent)
            fac2 = Fac/DBLE(KACcep)
            IF ( LPRi.GT.4 ) WRITE (LO,'(/1X,A,/1X,A)')
     &            'PHO_EVENT: generated and accepted events' , 
     &           '----------------------------------------'
            IF ( LPRi.GT.4 ) WRITE (LO,'(3X,A)') 
     &   'process, sampled, accepted, cross section (internal/external)'
            IF ( LPRi.GT.4 ) WRITE (LO,'(3X,A,2I12,1P2E13.3)')
     &            'non.diff.' , iprsam(1) , ipracc(1) , DBLE(ipracc(1))
     &           *fac1 , DBLE(ipracc(1))*fac2
            IF ( LPRi.GT.4 ) WRITE (LO,'(3X,A,2I12,1P2E13.3)')
     &            'elas sca.' , iprsam(2) , ipracc(2) , DBLE(ipracc(2))
     &           *fac1 , DBLE(ipracc(2))*fac2
            IF ( LPRi.GT.4 ) WRITE (LO,'(3X,A,2I12,1P2E13.3)')
     &            'qela sca.' , iprsam(3) , ipracc(3) , DBLE(ipracc(3))
     &           *fac1 , DBLE(ipracc(3))*fac2
            IF ( LPRi.GT.4 ) WRITE (LO,'(3X,A,2I12,1P2E13.3)')
     &            'doub.pom.' , iprsam(4) , ipracc(4) , DBLE(ipracc(4))
     &           *fac1 , DBLE(ipracc(4))*fac2
            IF ( LPRi.GT.4 ) WRITE (LO,'(3X,A,2I12,1P2E13.3)')
     &            'diff.par1' , iprsam(5) , ipracc(5) , DBLE(ipracc(5))
     &           *fac1 , DBLE(ipracc(5))*fac2
            IF ( LPRi.GT.4 ) WRITE (LO,'(3X,A,2I12,1P2E13.3)')
     &            'diff.par2' , iprsam(6) , ipracc(6) , DBLE(ipracc(6))
     &           *fac1 , DBLE(ipracc(6))*fac2
            IF ( LPRi.GT.4 ) WRITE (LO,'(3X,A,2I12,1P2E13.3)')
     &            'doub.dif.' , iprsam(7) , ipracc(7) , DBLE(ipracc(7))
     &           *fac1 , DBLE(ipracc(7))*fac2
            IF ( LPRi.GT.4 ) WRITE (LO,'(3X,A,2I12,1P2E13.3)')
     &            'dir all  ' , iprsam(8) , ipracc(8) , DBLE(ipracc(8))
     &           *fac1 , DBLE(ipracc(8))*fac2
            IF ( LPRi.GT.4 ) WRITE (LO,'(3X,A,2I12,1P2E13.3)')
     &            'dir X res' , idns(1) , idna(1) , DBLE(idna(1))*fac1 , 
     &           DBLE(idna(1))*fac2
            IF ( LPRi.GT.4 ) WRITE (LO,'(3X,A,2I12,1P2E13.3)')
     &            'res X dir' , idns(2) , idna(2) , DBLE(idna(2))*fac1 , 
     &           DBLE(idna(2))*fac2
            IF ( LPRi.GT.4 ) WRITE (LO,'(3X,A,2I12,1P2E13.3)')
     &            'dir X dir' , idns(3) , idna(3) , DBLE(idna(3))*fac1 , 
     &           DBLE(idna(3))*fac2
            IF ( LPRi.GT.4 ) WRITE (LO,'(3X,A,2I12,1P2E13.3)')
     &            'soft pom.' , isps , ispa , DBLE(ispa)*fac1 , 
     &           DBLE(ispa)*fac2
            IF ( LPRi.GT.4 ) WRITE (LO,'(3X,A,2I12,1P2E13.3)')
     &            'hard pom.' , ihps , ihpa , DBLE(ihpa)*fac1 , 
     &           DBLE(ihpa)*fac2
            IF ( LPRi.GT.4 ) WRITE (LO,'(3X,A,2I12,1P2E13.3)')
     &            'soft reg.' , isrs , isra , DBLE(isra)*fac1 , 
     &           DBLE(isra)*fac2
            IF ( LPRi.GT.4 ) WRITE (LO,'(3X,A,2I12,1P2E13.3)')
     &            'enh. trg.' , ists , ista , DBLE(ista)*fac1 , 
     &           DBLE(ista)*fac2
            IF ( LPRi.GT.4 ) WRITE (LO,'(3X,A,2I12,1P2E13.3)')
     &            'enh. log.' , isls , isla , DBLE(isla)*fac1 , 
     &           DBLE(isla)*fac2
            IF ( LPRi.GT.4 ) WRITE (LO,'(3X,A,2I12,1P2E13.3)')
     &            'doub.pom.' , idps , idpa , DBLE(idpa)*fac1 , 
     &           DBLE(idpa)*fac2
            IF ( ISWmdl(14).GT.0 ) THEN
               IF ( LPRi.GT.4 ) WRITE (LO,'(3X,A,I3)')
     &               'recursive pomeron splitting:' , ISWmdl(14)
               IF ( LPRi.GT.4 ) WRITE (LO,'(5X,A,I12)')
     &              '1->2pom-cut :' , ienacc(8)
               IF ( LPRi.GT.4 ) WRITE (LO,'(5X,A,I12)')
     &              '1->doub-pom :' , ienacc(4)
               IF ( LPRi.GT.4 ) WRITE (LO,'(5X,A,I12)')
     &              '1->diff-dis1:' , ienacc(5)
               IF ( LPRi.GT.4 ) WRITE (LO,'(5X,A,I12)')
     &              '1->diff-dis2:' , ienacc(6)
               IF ( LPRi.GT.4 ) WRITE (LO,'(5X,A,I12)')
     &              '1->doub-diff:' , ienacc(7)
            END IF
            IF ( LPRi.GT.4 ) WRITE (LO,'(2(/1X,A,1PE12.3)/)')
     &            ' sampled cross section (mb)' , SIGgen(1) , 
     &           'accepted cross section (mb)' , SIGgen(2)
 
            CALL PHO_REJSTA(-2)
            CALL PHO_SAMPRO(1,IFPap(1),IFPap(2),ECM,PVIrt(1),PVIrt(2),
     &                      0.D0,-2)
            CALL PHO_PARTON(-2,0,0,P1,P2,Irej)
C  statistics of hard scattering processes
            IF ( LPRi.GT.4 ) WRITE (LO,'(2(/1X,A))')
     &            'PHO_EVENT: statistics of hard scattering processes' , 
     &           '--------------------------------------------------'
            DO k = 1 , 4
               IF ( MH_tried(0,k,IDXmpar).GT.0 ) THEN
                  IF ( LPRi.GT.4 ) WRITE (LO,'(/5X,A,I3)') 
     &          'process (accepted,x-section internal/external) for IP:'
     &          , k
                  DO m = 0 , MAX_PRO_2
                     IF ( LPRi.GT.4 )
     &                     WRITE (LO,'(1X,I3,1X,A,2X,2I12,1P2E13.3)')
     &                    m , PROc(m) , MH_tried(m,k,IDXmpar) , 
     &                    MH_acc_1(m,k,IDXmpar) , 
     &                    DBLE(MH_acc_1(m,k,IDXmpar))*fac1 , 
     &                    DBLE(MH_acc_2(m,k,IDXmpar))*fac2
                  END DO
               END IF
            END DO
 
         ELSE
            IF ( LPRi.GT.4 ) WRITE (LO,'(/1X,A,I4,/)')
     &            'no output of statistics' , KEVent
         END IF
         IF ( LPRi.GT.4 ) WRITE (LO,'(/3(/1X,A)/)')
     &         '======================================================'
     &        , '   ------- end of event generation summary --------' , 
     &        '======================================================'
      ELSE
         IF ( LPRi.GT.4 ) WRITE (LO,'(/1X,A,I7)')
     &         'PHO_EVENT:ERROR: unsupported NEV' , Nev
      END IF
 
      END SUBROUTINE
