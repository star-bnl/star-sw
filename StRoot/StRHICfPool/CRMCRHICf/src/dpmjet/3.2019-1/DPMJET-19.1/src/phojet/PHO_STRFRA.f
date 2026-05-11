
      SUBROUTINE PHO_STRFRA(Irej)
C********************************************************************
C
C     do all fragmentation of strings
C
C     output:  IREJ    0   successful
C                      1   rejection
C                     50   rejection due to user cutoffs
C
C********************************************************************
 
      IMPLICIT NONE
      SAVE 
 
C  input/output channels
      INCLUDE 'inc/poinou'
C  some constants
      INCLUDE 'inc/pocons'
C  event debugging information
      INCLUDE 'inc/podebg'
C  general process information
      INCLUDE 'inc/poprcs'
C  model switches and parameters
      INCLUDE 'inc/pomdls'
C  global event kinematics and particle IDs
      INCLUDE 'inc/pogcms'
C  hard scattering data
      INCLUDE 'inc/pohslt'
C  standard particle data interface
      INCLUDE 'inc/poevt1'
C  extension to standard particle data interface (PHOJET specific)
      INCLUDE 'inc/poevt2'
 
C  color string configurations including collapsed strings and hadrons
      INCLUDE 'inc/postrg'
 
      INTEGER Irej
 
      DOUBLE PRECISION px , py , pz , he , xmb , pt1 , pt2
 
      INTEGER i , ii , ij , ifound , ip , ip_old , ipmoth , ipos , 
     &        ibam , ijoin , igen , is , ish , j , k1 , k2 , 
     &        nhep1 , nlines
 
      INTEGER indx(500) , indx_max
 
      DOUBLE PRECISION DT_RNDM
      INTEGER IPHO_PDG2ID
      EXTERNAL DT_RNDM , IPHO_PDG2ID
 
      DOUBLE PRECISION PYP , rqlun
      INTEGER PYK
      EXTERNAL PYP, PYK
 
      INCLUDE 'inc/pydat1'
      INCLUDE 'inc/pyint1'
      INCLUDE 'inc/pyjets'
 
      DIMENSION ijoin(MSCAHD)
 
      Irej = 0
      IF ( ABS(ISWmdl(6)).GT.3 ) THEN
         IF ( LPRi.GT.4 ) WRITE (LO,'(/1X,2A,I3)')
     &        'PHO_STRFRA:ERROR: ' , 'invalid value of ISWMDL(6)' , 
     &        ISWmdl(6)
         CALL PHO_ABORT
      END IF
 
Cc**anfe Reset internal popcorn flags before fragmentation
      DO i = 1 , 5
         MSTu(120+i) = 0
      END DO
 
C  popcorn suppression
C        IF(PARMDL(134).GT.0.D0) THEN
C          IF(DT_RNDM(DUM).LT.PARMDL(134)) THEN
C            MSTJ(12) = 2
C          ELSE
C            MSTJ(12) = 1
C          ENDIF
C        ENDIF
 
C  copy partons to fragmentation code JETSET
      ip = 0
      ip_old = 1
 
      DO j = 1 , ISTr
 
C  select partons with common production process
         igen = NPOs(4,j)
         IF ( igen.GE.0 ) THEN
 
            indx_max = 0
            DO i = j , ISTr
               IF ( (igen.EQ.NPOs(4,i)) .OR. (IPAmdl(17).EQ.0) ) THEN
 
C  write final particles/resonances to JETSET
                  IF ( NCOde(i).EQ.-99 ) THEN
                     ii = NPOs(1,i)
                     ip = ip + 1
                     P(ip,1) = PHEp(1,ii)
                     P(ip,2) = PHEp(2,ii)
                     P(ip,3) = PHEp(3,ii)
                     P(ip,4) = PHEp(4,ii)
                     P(ip,5) = PHEp(5,ii)
                     K(ip,1) = 1
                     K(ip,2) = IDHep(ii)
                     K(ip,3) = 0
                     K(ip,4) = 0
                     K(ip,5) = 0
                     IPHist(2,ii) = ip
 
                     IF ( indx_max.EQ.500 ) THEN
                        IF ( LPRi.GT.4 ) WRITE (LO,'(1x,2a,i8,I12)')
     &                        'PHO_STRFRA: ' , 
     &                     'no space left in index vector (indx,Kevent)'
     &                     , indx_max , KEVent
                        Irej = 1
                        RETURN
                     END IF
 
                     indx_max = indx_max + 1
                     indx(indx_max) = ii
C  write partons to JETSET
                  ELSE IF ( NCOde(i).GE.0 ) THEN
                     k1 = JMOhep(1,NPOs(1,i))
                     k2 = MAX(JMOhep(1,NPOs(1,i)),-JMOhep(2,NPOs(1,i)))
                     ij = 0
                     DO ii = k1 , k2
                        ip = ip + 1
                        P(ip,1) = PHEp(1,ii)
                        P(ip,2) = PHEp(2,ii)
                        P(ip,3) = PHEp(3,ii)
                        P(ip,4) = PHEp(4,ii)
                        P(ip,5) = PHEp(5,ii)
                        K(ip,1) = 1
                        K(ip,2) = IDHep(ii)
                        K(ip,3) = 0
                        K(ip,4) = 0
                        K(ip,5) = 0
                        IPHist(2,ii) = ip
                        ij = ij + 1
                        ijoin(ij) = ip
                        indx_max = indx_max + 1
                        indx(indx_max) = ii
 
                     END DO
                     ii = JMOhep(2,NPOs(1,i))
                     IF ( (ii.GT.0) .AND. (ii.NE.k1) ) THEN
                        ip = ip + 1
                        P(ip,1) = PHEp(1,ii)
                        P(ip,2) = PHEp(2,ii)
                        P(ip,3) = PHEp(3,ii)
                        P(ip,4) = PHEp(4,ii)
                        P(ip,5) = PHEp(5,ii)
                        K(ip,1) = 1
                        K(ip,2) = IDHep(ii)
                        K(ip,3) = 0
                        K(ip,4) = 0
                        K(ip,5) = 0
                        IPHist(2,ii) = ip
                        ij = ij + 1
                        ijoin(ij) = ip
                        indx_max = indx_max + 1
                        indx(indx_max) = ii
 
                     END IF
                     N = ip
C  connect partons to strings
 
                     CALL PYJOIN(ij,ijoin)
 
                  END IF
 
                  NPOs(4,i) = -NPOs(4,i)
               END IF
            END DO
 
C  set Lund counter
            N = ip
            IF ( ip.NE.0 ) THEN
 
C  hard final state evolution
               IF ( (ISWmdl(8).EQ.1) .OR. (ISWmdl(8).EQ.3) ) THEN
                  ish = 0
                  DO k1 = 1 , indx_max
                     i = indx(k1)
                     IF ( IPHist(1,i).LE.-100 ) THEN
C**anfe  Added bounds check for IJOIN
                        IF ( ish.LE.MSCAHD ) THEN
                           ish = ish + 1
                           ijoin(ish) = i
                        ELSE
                           IF ( LPRi.GT.4 ) WRITE (LO,'(1x,2A,2I5)')
     &                         'PHO_STRFRA:ERROR: ' , 
     &                        'no space left in IJOIN (ISH,MSCAHD)' , 
     &                        ish , MSCAHD
                           CALL PHO_ABORT
                        END IF
                     END IF
                  END DO
                  IF ( ish.GE.2 ) THEN
                     DO k1 = 1 , ish
                        IF ( ijoin(k1).NE.0 ) THEN
                           i = ijoin(k1)
                           IF ( (IPAmdl(102).NE.1) .OR. 
     &                        (IPHist(1,i).EQ.-100) ) THEN
                              DO k2 = k1 + 1 , ish
                               IF ( ijoin(k2).NE.0 ) THEN
                               ii = ijoin(k2)
                               IF ( IPHist(1,i).EQ.IPHist(1,ii) ) THEN
                               pt1 = SQRT(PHEp(1,ii)**2+PHEp(2,ii)**2)
                               pt2 = SQRT(PHEp(1,i)**2+PHEp(2,i)**2)
                               rqlun = MIN(pt1,pt2)
 
                               IF ( LPRi.GT.4 .AND. IDEb(22).GE.10 )
     &                            WRITE (LO,'(1X,A,2I5,E12.4)')
     &                             'PHO_STRFRA: PYSHOW called' , i , 
     &                            ii , rqlun
                               CALL PYSHOW(IPHist(2,i),IPHist(2,ii),
     &                            rqlun)
 
                               ijoin(k1) = 0
                               ijoin(k2) = 0
                               GOTO 2
                               END IF
                               END IF
                              END DO
                           END IF
                        END IF
 2                   END DO
                  END IF
               END IF
 
C  fragment parton / hadron configuration (hadronization & decay)
 
               IF ( ISWmdl(6).NE.0 ) THEN
                  ii = MSTu(21)
                  MSTu(21) = 1
 
                  CALL PYEXEC
 
                  MSTu(21) = ii
C  Lund warning?
                  IF ( MSTu(28).NE.0 ) THEN
                     IF ( IDEb(22).GE.10 ) THEN
                        IF ( LPRi.GT.4 ) WRITE (LO,'(1X,A,I12,I3)') 
     &                      'PHO_STRFRA:(1) Lund code warning (EV/code)'
     &                      , KEVent , MSTu(28)
                        CALL PHO_PREVNT(2)
                     END IF
                  END IF
C  event accepted?
                  IF ( MSTu(24).NE.0 ) THEN
                     IF ( IDEb(22).GE.2 ) THEN
                        IF ( LPRi.GT.4 ) WRITE (LO,'(1X,A,I12,I3)') 
     &                 'PHO_STRFRA:(1) rejection by Lund code (EV/code)'
     &                 , KEVent , MSTu(24)
                        CALL PHO_PREVNT(2)
                     END IF
                     Irej = 1
                     RETURN
                  END IF
               END IF
 
               ip = N
C  change particle status in JETSET to avoid internal adjustments
               DO k1 = ip_old , ip
                  K(k1,1) = K(k1,1) + 1000
               END DO
               ip_old = ip + 1
            END IF
         END IF
 
      END DO
 
C  restore original JETSET particle status codes
      DO i = 1 , N
         K(i,1) = K(i,1) - 1000
      END DO
 
C       IF(IDEB(22).GE.25) THEN
C         WRITE(LO,'(//1X,2A)') 'PHO_STRFRA: ',
C    &      'particle/string system before fragmentation'
C         CALL PHO_PREVNT(2)
C       ENDIF
 
C  copy hadrons back to POEVT1 / POEVT2
 
      IF ( ip.GT.0 ) THEN
         nhep1 = NHEp + 1
 
         nlines = PYK(0,1)
 
C  copy hadrons back with full history information
         IF ( IPAmdl(178).EQ.1 ) THEN
            DO ii = 1 , ISTr
               IF ( NCOde(ii).GE.0 ) THEN
                  k1 = IPHist(2,NPOs(2,ii))
                  k2 = IPHist(2,-NPOs(3,ii))
               ELSE IF ( NCOde(ii).EQ.-99 ) THEN
                  k1 = IPHist(2,NPOs(1,ii))
                  k2 = k1
               ELSE
                  GOTO 20
               END IF
               ifound = 0
               DO j = 1 , nlines
 
                  IF ( PYK(j,7).EQ.1 ) THEN
                     ipmoth = PYK(j,15)
 
                     IF ( (ipmoth.GE.k1) .AND. (ipmoth.LE.k2) ) THEN
 
                        ibam = IPHO_PDG2ID(PYK(j,8))
 
                        IF ( (ibam.EQ.0) .AND. (ISWmdl(6).NE.0) ) THEN
                           IF ( IDEb(22).GE.2 ) THEN
                              IF ( LPRi.GT.4 ) WRITE (LO,'(/1X,2A)')
     &                            'PHO_STRFRA: ' , 
     &                           'LUND interface (1) rejection'
                              CALL PHO_PREVNT(2)
                           END IF
                           Irej = 1
                           RETURN
                        END IF
                        ifound = ifound + 1
 
                        px = PYP(j,1)
                        py = PYP(j,2)
                        pz = PYP(j,3)
                        he = PYP(j,4)
                        xmb = PYP(j,5)**2
 
C  register parton/hadron
                        is = 1
                        IF ( ibam.EQ.0 ) THEN
                           IF ( ISWmdl(6).EQ.0 ) THEN
                              is = -1
                           ELSE
                              IF ( IDEb(22).GE.2 ) THEN
                               IF ( LPRi.GT.4 ) WRITE (LO,'(/1X,2A)')
     &                             'PHO_STRFRA: ' , 
     &                            'LUND interface (2) rejection'
                               CALL PHO_PREVNT(2)
                              END IF
                              Irej = 1
                              RETURN
                           END IF
                        END IF
 
                        CALL PHO_REGPAR(is,PYK(j,8),ibam,NPOs(1,ii),0,
     &                     px,py,pz,he,j,0,0,0,ipos,1)
 
                        ISThep(ipos) = 1
                     END IF
                  END IF
               END DO
               IF ( ifound.EQ.0 ) THEN
                  IF ( IDEb(2).GE.2 ) THEN
                     IF ( LPRi.GT.4 ) WRITE (LO,'(2A,I12,I3)')
     &                     'PHO_STRFRA: ' , 
     &                    'no particles found for string (EVE,ISTR):' , 
     &                    KEVent , ii
                  END IF
                  ISThep(NPOs(1,ii)) = 2
               END IF
 20         END DO
         ELSE
C  copy hadrons back without history information
            JDAhep(1,1) = nhep1
            JDAhep(1,2) = nhep1
            DO j = 1 , nlines
 
               IF ( PYK(j,7).EQ.1 ) THEN
                  ibam = IPHO_PDG2ID(PYK(j,8))
 
                  IF ( (ibam.EQ.99999) .AND. (ISWmdl(6).NE.0) ) THEN
                     IF ( IDEb(22).GE.2 ) THEN
                        IF ( LPRi.GT.4 ) WRITE (LO,'(/1X,A)')
     &                        'PHO_STRFRA: LUND interface (3) rejection'
                        CALL PHO_PREVNT(2)
                     END IF
                     Irej = 1
                     RETURN
                  END IF
 
                  px = PYP(j,1)
                  py = PYP(j,2)
                  pz = PYP(j,3)
                  he = PYP(j,4)
                  xmb = PYP(j,5)**2
 
C  register parton/hadron
                  is = 1
                  IF ( ibam.EQ.0 ) THEN
                     IF ( ISWmdl(6).EQ.0 ) THEN
                        is = -1
                     ELSE
                        IF ( IDEb(22).GE.2 ) THEN
                           IF ( LPRi.GT.4 ) WRITE (LO,'(/1X,A)') 
     &                        'PHO_STRFRA: LUND interface (4) rejection'
                           CALL PHO_PREVNT(2)
                        END IF
                        Irej = 1
                        RETURN
                     END IF
                  END IF
 
                  CALL PHO_REGPAR(is,PYK(j,8),ibam,1,2,px,py,pz,he,j,0,
     &               0,0,ipos,1)
 
                  ISThep(ipos) = 1
               END IF
            END DO
            DO ii = 1 , ISTr
               IF ( (NCOde(ii).GE.0) .OR. (NCOde(ii).EQ.-99) )
     &              ISThep(NPOs(1,ii)) = 2
            END DO
         END IF
      END IF
 
C  debug event status
      IF ( IDEb(22).GE.15 ) THEN
         IF ( LPRi.GT.4 ) WRITE (LO,'(//1X,A)')
     &         'PHO_STRFRA: particle system after fragmentation'
         CALL PHO_PREVNT(2)
      END IF
 
      END SUBROUTINE
