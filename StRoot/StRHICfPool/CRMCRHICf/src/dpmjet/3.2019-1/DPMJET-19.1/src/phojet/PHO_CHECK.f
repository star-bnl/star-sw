
      SUBROUTINE PHO_CHECK(Md,Idev)
C**********************************************************************
C
C     check quantum numbers of entries in /POEVT1/ and /POEVT2/
C           (energy, momentum, charge, baryon number conservation)
C
C     input:    MD      -1  check overall momentum conservation
C                           and perform detailed check only in case of
C                           deviations
C                        1  test all branchings, mother-daughter
C                           relations
C
C     output:   IDEV     0  no deviations
C                        1  deviations found
C
C**********************************************************************
      IMPLICIT NONE
      DOUBLE PRECISION ddabs , ddrel , ecm1 , ecm2 , ee1 , ee2 , esc , 
     &                 px1 , px2 , py1 , py2 , pz1 , pz2
      INTEGER i , iba1 , iba2 , ich1 , ich2 , Idev , ierr , ii , 
     &        IPHO_BAR3 , IPHO_CHR3 , k , k1 , k2 , Md , mode
      SAVE 
 
C  input/output channels
      INCLUDE 'inc/poinou'
C  event debugging information
      INCLUDE 'inc/podebg'
C  model switches and parameters
      INCLUDE 'inc/pomdls'
C  global event kinematics and particle IDs
      INCLUDE 'inc/pogcms'
C  nucleon-nucleus / nucleus-nucleus interface to DPMJET
      INCLUDE 'inc/pohdfl'
 
C  standard particle data interface
 
 
      INCLUDE 'inc/poevt1'
C  extension to standard particle data interface (PHOJET specific)
      INCLUDE 'inc/poevt2'
 
C  color string configurations including collapsed strings and hadrons
      INCLUDE 'inc/postrg'
 
C  count number of errors to avoid disk overflow
      DATA ierr/0/
 
      Idev = 0
C  conservation check suppressed
      IF ( (IPAmdl(15).EQ.0) .OR. (IDEb(20).LE.-10) ) RETURN
 
      IF ( IPAmdl(13).GT.0 ) THEN
 
C  DPMJET call with x limitations
         mode = -1
         ecm1 = SQRT(XPSub*XTSub)*ECM
 
      ELSE
 
C  standard call
         mode = Md
C  first two entries are considered as scattering particles
         ee1 = PHEp(4,1) + PHEp(4,2)
         px1 = PHEp(1,1) + PHEp(1,2)
         py1 = PHEp(2,1) + PHEp(2,2)
         pz1 = PHEp(3,1) + PHEp(3,2)
 
      END IF
 
      ddrel = PARmdl(75)
      ddabs = PARmdl(76)
      IF ( mode.EQ.-1 ) THEN
 
C  overall check only (less time consuming)
 
 
         ich2 = 0
         iba2 = 0
         ee2 = 0.D0
         px2 = 0.D0
         py2 = 0.D0
         pz2 = 0.D0
 
         DO k = 3 , NHEp
C  recognize only existing particles as possible daughters
            IF ( ABS(ISThep(k)).EQ.1 ) THEN
               ich2 = ich2 + IPHO_CHR3(k,2)
               iba2 = iba2 + IPHO_BAR3(k,2)
               ee2 = ee2 + PHEp(4,k)
               px2 = px2 + PHEp(1,k)
               py2 = py2 + PHEp(2,k)
               pz2 = pz2 + PHEp(3,k)
            END IF
         END DO
 
C  check energy-momentum conservation
         esc = ECM*ddrel
 
         IF ( IPAmdl(13).GT.0 ) THEN
 
C  DPMJET call with x limitations
            ecm2 = SQRT((ee2-pz2)*(ee2+pz2)-px2**2-py2**2)
            IF ( ABS(ecm1-ecm2).GT.esc ) THEN
               IF ( LPRi.GT.4 ) WRITE (LO,'(1X,A,/,5X,A,1P,2E12.4)')
     &               'PHO_CHECK: c.m. energy conservation violated' , 
     &              'initial/final energy:' , ecm1 , ecm2
               Idev = 1
            END IF
 
         ELSE
 
C  standard call
            IF ( ABS(ee1-ee2).GT.esc ) THEN
               IF ( LPRi.GT.4 ) WRITE (LO,'(1X,A,/,5X,A,1P,2E12.4)')
     &               'PHO_CHECK: energy conservation violated' , 
     &              'initial/final energy:' , ee1 , ee2
               Idev = 1
            END IF
            IF ( ABS(px1-px2).GT.esc ) THEN
               IF ( LPRi.GT.4 ) WRITE (LO,'(1X,A,/,5X,A,1P,2E12.4)')
     &               'PHO_CHECK: x-momentum conservation violated' , 
     &              'initial/final x-momentum:' , px1 , px2
               Idev = 1
            END IF
            IF ( ABS(py1-py2).GT.esc ) THEN
               IF ( LPRi.GT.4 ) WRITE (LO,'(1X,A,/,5X,A,1P,2E12.4)')
     &               'PHO_CHECK: y-momentum conservation violated' , 
     &              'initial/final y-momentum:' , py1 , py2
               Idev = 1
            END IF
            IF ( ABS(pz1-pz2).GT.esc ) THEN
               IF ( LPRi.GT.4 ) WRITE (LO,'(1X,A,/,5X,A,1P,2E12.4)')
     &               'PHO_CHECK: z-momentum conservation violated' , 
     &              'initial/final z-momentum:' , pz1 , pz2
               Idev = 1
            END IF
 
C  check of quantum number conservation
 
            ich1 = IPHO_CHR3(1,2) + IPHO_CHR3(2,2)
            iba1 = IPHO_BAR3(1,2) + IPHO_BAR3(2,2)
 
            IF ( ich1.NE.ich2 ) THEN
               IF ( LPRi.GT.4 ) WRITE (LO,'(1X,A,/,5X,A,2I5)')
     &               'PHO_CHECK: charge conservation violated' , 
     &              'initial/final charge sum' , ich1 , ich2
               Idev = 1
            END IF
            IF ( iba1.NE.iba2 ) THEN
               IF ( LPRi.GT.4 ) WRITE (LO,'(1X,2A,/,5X,A,2I5)')
     &               'PHO_CHECK: ' , 
     &              'baryonic charge conservation violated' , 
     &              'initial/final baryonic charge sum' , iba1 , iba2
               Idev = 1
            END IF
 
         END IF
 
C  perform detailed checks in case of deviations
         IF ( (IDEb(20).GE.0) .AND. (Idev.NE.0) ) THEN
            IF ( IPAmdl(13).GT.0 ) GOTO 300
            ddrel = ddrel/2.D0
            ddabs = ddabs/2.D0
            IF ( LPRi.GT.4 ) WRITE (LO,'(/1X,2A,2E12.4)')
     &           'PHO_CHECK: ' , 'increasing precision of tests to' , 
     &           ddrel , ddabs
 
 
            i = 1
            GOTO 100
         END IF
         GOTO 99999
      ELSE
         i = 1
      END IF
 
C  recognize only decayed particles as mothers
 100  IF ( ISThep(i).EQ.2 ) THEN
C  search for other mother particles
         k = JDAhep(1,i)
         IF ( k.EQ.0 ) THEN
            IF ( IPAmdl(178).NE.0 .AND. LPRi.GT.4 )
     &            WRITE (LO,'(1X,2A,I4)') 'PHO_CHECK: ' , 
     &           'entry marked as decayed but no dauther given:' , i
            GOTO 200
         END IF
         k1 = JMOhep(1,k)
         k2 = JMOhep(2,k)
C  sum over mother particles
         ich1 = IPHO_CHR3(k1,2)
         iba1 = IPHO_BAR3(k1,2)
         ee1 = PHEp(4,k1)
         px1 = PHEp(1,k1)
         py1 = PHEp(2,k1)
         pz1 = PHEp(3,k1)
         IF ( k2.LT.0 ) THEN
            k2 = -k2
            IF ( (k1.GT.i) .OR. (k2.LT.i) ) THEN
               IF ( LPRi.GT.4 ) WRITE (LO,'(/,1X,2A,3I4)')
     &              'PHO_CHECK: ' , 
     &              'inconsistent mother/daughter relation found' , i , 
     &              k1 , k2
               CALL PHO_PREVNT(-1)
            END IF
            DO ii = k1 + 1 , k2
               IF ( ABS(ISThep(ii)).LE.2 ) THEN
                  ich1 = ich1 + IPHO_CHR3(ii,2)
                  iba1 = iba1 + IPHO_BAR3(ii,2)
                  ee1 = ee1 + PHEp(4,ii)
                  px1 = px1 + PHEp(1,ii)
                  py1 = py1 + PHEp(2,ii)
                  pz1 = pz1 + PHEp(3,ii)
               END IF
            END DO
         ELSE IF ( (k2.GT.0) .AND. (k2.NE.k1) ) THEN
            ich1 = ich1 + IPHO_CHR3(k2,2)
            iba1 = iba1 + IPHO_BAR3(k2,2)
            ee1 = ee1 + PHEp(4,k2)
            px1 = px1 + PHEp(1,k2)
            py1 = py1 + PHEp(2,k2)
            pz1 = pz1 + PHEp(3,k2)
         END IF
 
C  sum over daughter particles
         ich2 = 0
         iba2 = 0
         ee2 = 0.D0
         px2 = 0.D0
         py2 = 0.D0
         pz2 = 0.D0
         DO ii = JDAhep(1,i) , JDAhep(2,i)
            IF ( ABS(ISThep(ii)).LE.2 ) THEN
               ich2 = ich2 + IPHO_CHR3(ii,2)
               iba2 = iba2 + IPHO_BAR3(ii,2)
               ee2 = ee2 + PHEp(4,ii)
               px2 = px2 + PHEp(1,ii)
               py2 = py2 + PHEp(2,ii)
               pz2 = pz2 + PHEp(3,ii)
            END IF
         END DO
C  conservation check
         esc = MAX(MAX(ee1,ee2)*ddrel,ddabs)
         IF ( ABS(ee1-ee2).GT.esc ) THEN
            IF ( LPRi.GT.4 ) WRITE (LO,'(1X,A,/,5X,A,I3,2X,1P,2E10.3)')
     &            'PHO_CHECK: energy conservation violated for' , 
     &           'entry,initial,final:' , i , ee1 , ee2
            Idev = 1
         END IF
         esc = MAX(MAX(ABS(px1),ABS(px2))*ddrel,ddabs)
         IF ( ABS(px1-px2).GT.esc ) THEN
            IF ( LPRi.GT.4 ) WRITE (LO,'(1X,A,/,5X,A,I3,2X,1P,2E12.3)')
     &            'PHO_CHECK: x-momentum conservation violated for' , 
     &           'entry,initial,final:' , i , px1 , px2
            Idev = 1
         END IF
         esc = MAX(MAX(ABS(py1),ABS(py2))*ddrel,ddabs)
         IF ( ABS(py1-py2).GT.esc ) THEN
            IF ( LPRi.GT.4 ) WRITE (LO,'(1X,A,/,5X,A,I3,2X,1P,2E12.3)')
     &            'PHO_CHECK: y-momentum conservation violated for' , 
     &           'entry,initial,final:' , i , py1 , py2
            Idev = 1
         END IF
         esc = MAX(MAX(ABS(pz1),ABS(pz2))*ddrel,ddabs)
         IF ( ABS(pz1-pz2).GT.esc ) THEN
            IF ( LPRi.GT.4 ) WRITE (LO,'(1X,A,/,5X,A,I3,2X,1P,2E12.3)')
     &            'PHO_CHECK: z-momentum conservation violated for' , 
     &           'entry,initial,final:' , i , pz1 , pz2
            Idev = 1
         END IF
         IF ( ich1.NE.ich2 ) THEN
            IF ( LPRi.GT.4 ) WRITE (LO,'(1X,A,/,5X,A,I3,2X,2I5)')
     &            'PHO_CHECK: charge conservation violated for' , 
     &           'entry,initial,final:' , i , ich1 , ich2
            Idev = 1
         END IF
         IF ( iba1.NE.iba2 ) THEN
            IF ( LPRi.GT.4 ) WRITE (LO,'(1X,2A,/,5X,A,I3,2X,2I5)')
     &            'PHO_CHECK: ' , 
     &           'baryon charge conservation violated for' , 
     &           'entry,initial,final:' , i , iba1 , iba2
            Idev = 1
         END IF
         IF ( IDEb(20).GE.35 ) THEN
            IF ( LPRi.GT.4 ) WRITE (LO,
     &           '(/,1X,A,A,2(2X,I4,A,I4),2(/,5X,A,4E13.4),/5X,A,4I5)')
     &            'PHO_CHECK diagnostics:' , 
     &           '(1.mother/l.mother,1.daughter/l.daughter):' , k1 , 
     &           '/' , k2 , JDAhep(1,i) , '/' , JDAhep(2,i) , 
     &           'mother momenta   ' , px1 , py1 , pz1 , ee1 , 
     &           'daughter momenta ' , px2 , py2 , pz2 , ee2 , 
     &           'charge,baryon no ' , ich1 , ich2 , iba1 , iba2
         END IF
      END IF
 200  i = i + 1
      IF ( i.LE.NHEp ) GOTO 100
 
 
 300  ierr = ierr + Idev
 
C  write complete event in case of deviations
      IF ( (IDEb(20).GE.0) .AND. (Idev.NE.0) ) THEN
         CALL PHO_PREVNT(1)
         IF ( ISTr.GT.0 ) THEN
            CALL PHO_PRSTRG
 
            IF ( ISWmdl(6).GE.0 ) CALL PYLIST(1)
 
         END IF
      END IF
 
C  stop after too many errors
      IF ( ierr.GT.IPAmdl(179) ) THEN
         WRITE (LO,'(////1X,2A,I6,////)') 'PHO_CHECK:ERROR:' , 
     &          'too many inconsistencies found, program terminated' , 
     &          ierr
         CALL PHO_ABORT
      END IF
 
 
99999 END SUBROUTINE
