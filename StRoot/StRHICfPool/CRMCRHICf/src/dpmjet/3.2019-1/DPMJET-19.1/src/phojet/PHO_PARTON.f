
      SUBROUTINE PHO_PARTON(Iproc,Jm1,Jm2,P1,P2,Irej)
C********************************************************************
C
C     calculation of complete parton configuration
C
C     input:  IPROC   process ID  1 nondiffractive
C                                 2 elastic
C                                 3 quasi-ela. rho,omega,phi prod.
C                                 4 double Pomeron
C                                 5 single diff 1
C                                 6 single diff 2
C                                 7 double diff diss.
C                                 8 single-resolved / direct photon
C             JM1,2   index of mother particles in /POEVT1/
C
C
C     output: complete parton configuration in /POEVT1/
C             IREJ                1 failure
C                                 0 success
C                                50 rejection due to user cutoffs
C
C********************************************************************
      IMPLICIT NONE
      DOUBLE PRECISION dum , P1 , P2 , sprob , TINY
      INTEGER id1a , id1s , id2a , id2s , id3a , id3s , idia , idis , 
     &        idpa , idps , ihpa , ihps , iint , ip , ipar1 , ipar2 , 
     &        Iproc , Irej , isla , isls
      INTEGER ispa , isps , isra , isrs , ista , ists , itry2 , jint , 
     &        Jm1 , Jm2 , kint , mhard , mhdir , mhpom , mint , mploo , 
     &        mptri , msloo , msoft , mspom
      INTEGER msreg , mstrg
      SAVE 
 
      DIMENSION P1(4) , P2(4)
 
      PARAMETER (TINY=1.D-10)
 
C  input/output channels
      INCLUDE 'inc/poinou'
C  event debugging information
      INCLUDE 'inc/podebg'
C  model switches and parameters
      INCLUDE 'inc/pomdls'
C  table of particle indices for recursive PHOJET calls
      INCLUDE 'inc/porecu'
C  general process information
      INCLUDE 'inc/poprcs'
C  global event kinematics and particle IDs
      INCLUDE 'inc/pogcms'
C  cross sections
      INCLUDE 'inc/pocsec'
C  event weights and generated cross section
      INCLUDE 'inc/powght'
C  internal rejection counters
      INCLUDE 'inc/poloop'
 
      Irej = 0
C  clear event statistics
      KSPom = 0
      KHPom = 0
      KSReg = 0
      KHDir = 0
      KSTrg = 0
      KHTrg = 0
      KSLoo = 0
      KHLoo = 0
      KHArd = 0
      KSOft = 0
      KSDpo = 0
      KHDpo = 0
 
C-------------------------------------------------------------------
C  nondiffractive resolved processes
 
      IF ( Iproc.EQ.1 ) THEN
C  sample number of interactions
C555    CONTINUE
         iint = 0
         ip = 1
C  generate only hard events
         IF ( ISWmdl(2).EQ.0 ) THEN
            mhpom = 1
            mspom = 0
            msreg = 0
            mhdir = 0
            HSWght(1) = 1.D0
         ELSE
C  minimum bias events
            IPOwgc(1) = 0
 20         CALL PHO_SAMPRB(ECM,ip,iint,jint,kint)
            IPOwgc(1) = IPOwgc(1) + 1
            mint = 0
            mhdir = 0
            mstrg = 0
            msloo = 0
C
C  resolved soft processes: pomeron and reggeon
            mspom = iint
            msreg = jint
C  resolved hard process: hard pomeron
            mhpom = kint
C  resolved absorptive corrections
            mptri = 0
            mploo = 0
C  restrictions given by user
            IF ( mspom.LT.ISWcut(1) ) GOTO 20
            IF ( msreg.LT.ISWcut(2) ) GOTO 20
            IF ( mhpom.LT.ISWcut(3) ) GOTO 20
            HSWght(1) = 1.D0/DBLE(IPOwgc(1))
C  ----------------------------
            IF ( ISWmdl(15).EQ.0 ) THEN
               mhpom = 0
               IF ( msreg.GT.0 ) THEN
                  mspom = 0
                  msreg = 1
               ELSE
                  mspom = 1
                  msreg = 0
               END IF
            ELSE IF ( ISWmdl(15).EQ.1 ) THEN
               IF ( mhpom.GT.0 ) THEN
                  mhpom = 1
                  mspom = 0
                  msreg = 0
               ELSE IF ( mspom.GT.0 ) THEN
                  mspom = 1
                  msreg = 0
               ELSE
                  msreg = 1
               END IF
            ELSE IF ( ISWmdl(15).EQ.2 ) THEN
               mhpom = MIN(1,mhpom)
            ELSE IF ( ISWmdl(15).EQ.3 ) THEN
               mspom = MIN(1,mspom)
            END IF
         END IF
C  ----------------------------
 
C  statistics
         isps = isps + mspom
         ihps = ihps + mhpom
         isrs = isrs + msreg
         ists = ists + mstrg
         isls = isls + msloo
 
         IF ( LPRi.GT.4 .AND. IDEb(3).GE.5 )
     &         WRITE (LO,'(1X,A,I10,I7,6I4)')
     &         'PHO_PARTON: EV,SP,SR,HP,HD,ET,EL' , KEVent , mspom , 
     &        msreg , mhpom , mhdir , mptri , mploo
 
         itry2 = 0
 50      itry2 = itry2 + 1
         IF ( itry2.GT.1 ) CALL PHO_EVEINI(2,P1,P2,Jm1,Jm2)
         KSPom = mspom
         KSReg = msreg
         KHPom = mhpom
         KHDir = mhdir
         KSTrg = mptri
         KSLoo = mploo
 
         CALL PHO_STDPAR(Jm1,Jm2,1,mspom,msreg,mhpom,mhdir,Irej)
         IF ( Irej.NE.0 ) THEN
            IF ( Irej.EQ.50 ) RETURN
            IF ( IDEb(3).GE.2 ) THEN
               IF ( LPRi.GT.4 ) WRITE (LO,'(/1X,A,I5)')
     &               'PHO_PARTON: rejection by PHO_STDPAR ' , itry2
               CALL PHO_PREVNT(-1)
            END IF
            RETURN
         END IF
         IF ( mhpom.GT.0 ) THEN
            IDNodf = 3
         ELSE IF ( mspom.GT.0 ) THEN
            IDNodf = 2
         ELSE
            IDNodf = 1
         END IF
C  check of quantum numbers of parton configurations
         IF ( IDEb(3).GE.0 ) THEN
            CALL PHO_CHECK(1,Irej)
            IF ( Irej.NE.0 ) GOTO 50
         END IF
C  sample strings to prepare fragmentation
         CALL PHO_STRING(1,Irej)
         IF ( Irej.NE.0 ) THEN
            IF ( Irej.EQ.50 ) RETURN
            IFAil(30) = IFAil(30) + 1
            IF ( IDEb(3).GE.2 ) THEN
               IF ( LPRi.GT.4 ) WRITE (LO,'(/1X,A,I5)')
     &               'PHO_PARTON: rejection by PHO_STRING' , itry2
               CALL PHO_PREVNT(-1)
            END IF
            IF ( itry2.LT.20 ) GOTO 50
            IF ( IDEb(3).GE.1 ) THEN
               IF ( LPRi.GT.4 ) WRITE (LO,'(/1X,A,I5)')
     &               'PHO_PARTON: rejection' , itry2
               CALL PHO_PREVNT(-1)
            END IF
            RETURN
         END IF
 
C  statistics
         ispa = ispa + KSPom
         ihpa = ihpa + KHPom
         isra = isra + KSReg
         ista = ista + KSTrg
         isla = isla + KSLoo
 
C-------------------------------------------------------------------
C  elastic scattering / quasi-elastic rho/omega/phi production
 
      ELSE IF ( (Iproc.EQ.2) .OR. (Iproc.EQ.3) ) THEN
         IF ( LPRi.GT.4 .AND. IDEb(3).GE.5 ) WRITE (LO,'(1X,A,I10,I4)')
     &         'PHO_PARTON: ela./q-ela.sca:(EV,IPROC)' , KEVent , Iproc
 
C  DPMJET call with special projectile / target: transform into CMS
         IF ( (IPAmdl(13).GT.0) .AND. (IPOix3.EQ.0) )
     &        CALL PHO_DFWRAP(1,Jm1,Jm2)
 
         CALL PHO_QELAST(Iproc,Jm1,Jm2,Irej)
 
         IF ( Irej.NE.0 ) THEN
C  DPMJET call with special projectile / target: clean up
            IF ( (IPAmdl(13).GT.0) .AND. (IPOix3.EQ.0) )
     &           CALL PHO_DFWRAP(-2,Jm1,Jm2)
            IF ( IDEb(3).GE.2 ) THEN
               IF ( LPRi.GT.4 ) WRITE (LO,'(/1X,A,I5)')
     &               'PHO_PARTON: rejection by PHO_QELAST' , Irej
               CALL PHO_PREVNT(-1)
            END IF
            RETURN
         END IF
 
C  DPMJET call with special projectile / target: transform back
         IF ( (IPAmdl(13).GT.0) .AND. (IPOix3.EQ.0) )
     &        CALL PHO_DFWRAP(2,Jm1,Jm2)
 
C  prepare possible decays
         CALL PHO_STRING(1,Irej)
         IF ( Irej.NE.0 ) THEN
            IF ( Irej.EQ.50 ) RETURN
            IFAil(30) = IFAil(30) + 1
            RETURN
         END IF
 
C---------------------------------------------------------------------
C  double Pomeron scattering
 
      ELSE IF ( Iproc.EQ.4 ) THEN
         msoft = 0
         mhard = 0
         IF ( LPRi.GT.4 .AND. IDEb(3).GE.5 ) WRITE (LO,'(1X,A,I10)')
     &         'PHO_PARTON: EV,double-pomeron scattering' , KEVent
         idps = idps + 1
         itry2 = 0
 100     itry2 = itry2 + 1
         IF ( itry2.GT.1 ) CALL PHO_EVEINI(2,P1,P2,Jm1,Jm2)
C
         CALL PHO_CDIFF(Jm1,Jm2,msoft,mhard,1,Irej)
         IF ( Irej.NE.0 ) THEN
            IF ( IDEb(3).GE.2 ) THEN
               IF ( LPRi.GT.4 ) WRITE (LO,'(/1X,A,I5)')
     &               'PHO_PARTON: rejection by PHO_CDIFF' , Irej
               CALL PHO_PREVNT(-1)
            END IF
            RETURN
         END IF
C  check of quantum numbers of parton configurations
         IF ( IDEb(3).GE.0 ) THEN
            CALL PHO_CHECK(1,Irej)
            IF ( Irej.NE.0 ) GOTO 100
         END IF
C  sample strings to prepare fragmentation
         CALL PHO_STRING(1,Irej)
         IF ( Irej.NE.0 ) THEN
            IF ( Irej.EQ.50 ) RETURN
            IFAil(30) = IFAil(30) + 1
            IF ( IDEb(3).GE.2 ) THEN
               IF ( LPRi.GT.4 ) WRITE (LO,'(/1X,A,I5)')
     &               'PHO_PARTON: rejection by PHO_STRING' , itry2
               CALL PHO_PREVNT(-1)
            END IF
            IF ( itry2.LT.10 ) GOTO 100
            IF ( LPRi.GT.4 ) WRITE (LO,'(/1X,A,I5)')
     &            'PHO_PARTON: rejection' , itry2
            CALL PHO_PREVNT(-1)
            RETURN
         END IF
         idpa = idpa + 1
 
C-----------------------------------------------------------------------
C  single / double diffraction dissociation
 
      ELSE IF ( (Iproc.GE.5) .AND. (Iproc.LE.7) ) THEN
         msoft = 0
         mhard = 0
         IF ( Iproc.EQ.5 ) id1s = id1s + 1
         IF ( Iproc.EQ.6 ) id2s = id2s + 1
         IF ( Iproc.EQ.7 ) id3s = id3s + 1
         itry2 = 0
 150     itry2 = itry2 + 1
         IF ( itry2.GT.1 ) CALL PHO_EVEINI(2,P1,P2,Jm1,Jm2)
         ipar1 = 1
         ipar2 = 1
         IF ( Iproc.EQ.5 ) ipar2 = 0
         IF ( Iproc.EQ.6 ) ipar1 = 0
         IF ( LPRi.GT.4 .AND. IDEb(3).GE.5 ) WRITE (LO,'(1X,A,I10,3I4)')
     &         'PHO_PARTON: diffraction, EV, IPROC, IPAR1, IPAR2' , 
     &        KEVent , Iproc , ipar1 , ipar2
C  calculate rapidity gap survival probability
         sprob = 1.D0
         IF ( ECM.GT.10.D0 ) THEN
            IF ( (ipar1.GE.1) .AND. (ipar2.EQ.0) ) THEN
               IF ( SIGtr1(1).LT.1.D-10 ) THEN
                  sprob = 1.D0
               ELSE
                  sprob = SIGhsd(1)
     &                    /(SIGtr1(1)-2.D0*(SIGdpo(1)+SIGdpo(2)))
               END IF
            ELSE IF ( (ipar1.EQ.0) .AND. (ipar2.GE.1) ) THEN
               IF ( SIGtr2(1).LT.1.D-10 ) THEN
                  sprob = 1.D0
               ELSE
                  sprob = SIGhsd(2)
     &                    /(SIGtr2(1)-2.D0*(SIGdpo(1)+SIGdpo(3)))
               END IF
            ELSE IF ( (ipar1.GE.1) .AND. (ipar2.GE.1) ) THEN
               IF ( SIGloo.LT.1.D-10 ) THEN
                  sprob = 1.D0
               ELSE
                  sprob = SIGhdd/SIGloo
               END IF
            END IF
         END IF
 
C*sr
C temporary patch, r.e. 8.6.99
         IF ( IPAmdl(13).GT.0 ) THEN
            IF ( LPRi.GT.4 .AND. IDEb(3).GE.15 ) WRITE (LO,'(1X,A)')
     &            'PHO_PARTON: SPROB set to 1. (DPMJET temporary??)'
            sprob = 1.D0
         END IF
 
C  DPMJET call with special projectile / target: transform into CMS
         IF ( (IPAmdl(13).GT.0) .AND. (IPOix3.EQ.0) )
     &        CALL PHO_DFWRAP(1,Jm1,Jm2)
 
         CALL PHO_DIFDIS(ipar1,ipar2,Jm1,Jm2,sprob,0,msoft,mhard,Irej)
 
         IF ( Irej.NE.0 ) THEN
C  DPMJET call with special projectile / target: clean up
            IF ( (IPAmdl(13).GT.0) .AND. (IPOix3.EQ.0) )
     &           CALL PHO_DFWRAP(-2,Jm1,Jm2)
            IF ( IDEb(3).GE.2 ) THEN
               IF ( LPRi.GT.4 ) WRITE (LO,'(/1X,A,I5)')
     &               'PHO_PARTON: rejection by PHO_DIFDIS' , Irej
               CALL PHO_PREVNT(-1)
            END IF
            RETURN
         END IF
 
C  DPMJET call with special projectile / target: transform back
         IF ( (IPAmdl(13).GT.0) .AND. (IPOix3.EQ.0) )
     &        CALL PHO_DFWRAP(2,Jm1,Jm2)
 
C  check of quantum numbers of parton configurations
         IF ( IDEb(3).GE.0 ) THEN
            CALL PHO_CHECK(1,Irej)
            IF ( Irej.NE.0 ) GOTO 150
         END IF
C  sample strings to prepare fragmentation
         CALL PHO_STRING(1,Irej)
         IF ( Irej.NE.0 ) THEN
            IF ( Irej.EQ.50 ) RETURN
            IFAil(30) = IFAil(30) + 1
            IF ( IDEb(3).GE.2 ) THEN
               IF ( LPRi.GT.4 ) WRITE (LO,'(/1X,A,I5)')
     &               'PHO_PARTON: rejection by PHO_STRING' , itry2
               CALL PHO_PREVNT(-1)
            END IF
            IF ( itry2.LT.10 ) GOTO 150
            IF ( LPRi.GT.4 ) WRITE (LO,'(/1X,A,I5)')
     &            'PHO_PARTON: rejection' , itry2
            CALL PHO_PREVNT(-1)
            RETURN
         END IF
         IF ( Iproc.EQ.5 ) id1a = id1a + 1
         IF ( Iproc.EQ.6 ) id2a = id2a + 1
         IF ( Iproc.EQ.7 ) id3a = id3a + 1
 
C-----------------------------------------------------------------------
C  single / double direct processes
 
      ELSE IF ( Iproc.EQ.8 ) THEN
         msreg = 0
         mspom = 0
         mhpom = 0
         mhdir = 1
         IF ( IDEb(3).GE.5 ) THEN
            IF ( LPRi.GT.4 ) WRITE (LO,'(1X,A,I10)')
     &            'PHO_PARTON: EV,direct proc' , KEVent
         END IF
         idis = idis + mhdir
         itry2 = 0
 200     itry2 = itry2 + 1
         IF ( itry2.GT.1 ) CALL PHO_EVEINI(2,P1,P2,Jm1,Jm2)
         KSPom = mspom
         KSReg = msreg
         KHPom = mhpom
         KHDir = 4
 
         CALL PHO_STDPAR(Jm1,Jm2,1,mspom,msreg,mhpom,mhdir,Irej)
         IF ( Irej.NE.0 ) THEN
            IF ( Irej.EQ.50 ) RETURN
            IF ( IDEb(3).GE.2 ) THEN
               IF ( LPRi.GT.4 ) WRITE (LO,'(/1X,A,I5)')
     &               'PHO_PARTON: rejection by PHO_STDPAR' , itry2
               CALL PHO_PREVNT(-1)
            END IF
            RETURN
         END IF
         IDNodf = 4
C  check of quantum numbers of parton configurations
         IF ( IDEb(3).GE.0 ) THEN
            CALL PHO_CHECK(1,Irej)
            IF ( Irej.NE.0 ) GOTO 200
         END IF
C  sample strings to prepare fragmentation
         CALL PHO_STRING(1,Irej)
         IF ( Irej.NE.0 ) THEN
            IF ( Irej.EQ.50 ) RETURN
            IFAil(30) = IFAil(30) + 1
            IF ( IDEb(3).GE.2 ) THEN
               IF ( LPRi.GT.4 ) WRITE (LO,'(/1X,A,I5)')
     &               'PHO_PARTON: rejection by PHO_STRING' , itry2
               CALL PHO_PREVNT(-1)
            END IF
            IF ( itry2.LT.10 ) GOTO 200
            IF ( LPRi.GT.4 ) WRITE (LO,'(/1X,A,I5)')
     &            'PHO_PARTON: rejection' , itry2
            CALL PHO_PREVNT(-1)
            RETURN
         END IF
         IF ( Iproc.EQ.5 ) id1a = id1a + 1
         IF ( Iproc.EQ.6 ) id2a = id2a + 1
         IF ( Iproc.EQ.7 ) id3a = id3a + 1
         idia = idia + mhdir
 
C-----------------------------------------------------------------------
C  initialize control statistics
 
      ELSE IF ( Iproc.EQ.-1 ) THEN
         CALL PHO_SAMPRB(ECM,-1,0,0,0)
         CALL PHO_STDPAR(-1,0,0,0,0,0,0,Irej)
         CALL PHO_SEAFLA(-1,0,0,dum)
         IF ( (IFPap(1).EQ.22) .OR. (IFPap(2).EQ.22) )
     &        CALL PHO_QELAST(-1,1,2,0)
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
         id1s = 0
         id1a = 0
         id2s = 0
         id2a = 0
         id3s = 0
         id3a = 0
         idps = 0
         idpa = 0
         idis = 0
         idia = 0
         CALL PHO_STRING(-1,Irej)
         CALL PHO_DIFDIS(0,0,0,0,0.D0,-1,0,0,Irej)
         RETURN
 
C-----------------------------------------------------------------------
C  produce statistics summary
 
      ELSE IF ( Iproc.EQ.-2 ) THEN
         IF ( ISWmdl(2).NE.0 ) CALL PHO_SAMPRB(ECM,-2,0,0,0)
         IF ( IDEb(3).GE.0 ) THEN
            IF ( LPRi.GT.4 ) WRITE (LO,'(/1X,A,/1X,A)') 
     &        'PHO_PARTON: internal statistics on parton configurations'
     &        , 
     &        '--------------------------------------------------------'
            IF ( LPRi.GT.4 ) WRITE (LO,'(5X,A)')
     &            'process          sampled      accepted'
            IF ( LPRi.GT.4 ) WRITE (LO,'(5X,A,2I12)') 'soft pom.' , 
     &           isps , ispa
            IF ( LPRi.GT.4 ) WRITE (LO,'(5X,A,2I12)') 'hard pom.' , 
     &           ihps , ihpa
            IF ( LPRi.GT.4 ) WRITE (LO,'(5X,A,2I12)') 'soft reg.' , 
     &           isrs , isra
            IF ( LPRi.GT.4 ) WRITE (LO,'(5X,A,2I12)') 'enh. tri.' , 
     &           ists , ista
            IF ( LPRi.GT.4 ) WRITE (LO,'(5X,A,2I12)') 'enh. loo.' , 
     &           isls , isla
            IF ( LPRi.GT.4 ) WRITE (LO,'(5X,A,2I12)') 'diff.pa1.' , 
     &           id1s , id1a
            IF ( LPRi.GT.4 ) WRITE (LO,'(5X,A,2I12)') 'diff.pa2.' , 
     &           id2s , id2a
            IF ( LPRi.GT.4 ) WRITE (LO,'(5X,A,2I12)') 'doub.dif.' , 
     &           id3s , id3a
            IF ( LPRi.GT.4 ) WRITE (LO,'(5X,A,2I12)') 'doub.pom.' , 
     &           idps , idpa
            IF ( LPRi.GT.4 ) WRITE (LO,'(5X,A,2I12/)') 'dir.phot.' , 
     &           idis , idia
         END IF
         CALL PHO_STDPAR(-2,0,0,0,0,0,0,Irej)
         IF ( (IFPap(1).EQ.22) .OR. (IFPap(2).EQ.22) )
     &        CALL PHO_QELAST(-2,1,2,0)
         CALL PHO_STRING(-2,Irej)
         CALL PHO_DIFDIS(0,0,0,0,0.D0,-2,0,0,Irej)
         CALL PHO_SEAFLA(-2,0,0,dum)
         RETURN
      ELSE
         IF ( LPRi.GT.4 ) WRITE (LO,'(1X,A,I2)')
     &         'PARTON:ERROR: unknown process ID ' , Iproc
         STOP
      END IF
 
      END SUBROUTINE
