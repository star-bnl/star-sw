
      SUBROUTINE PHO_MASCOR(Irej)
C********************************************************************
C
C    check and adjust parton momenta for fragmentation
C
C    input:      /POEVT1/
C                /POSTRG/
C                IREJ    -1          initialization
C                        -2          output of statistics
C
C    output:     /POEVT1/
C                /POSTRG/
C                IREJ    0  successful
C                        1  failure
C
C    in case of very small string mass:
C       - direct manipulation of /POEVT1/ and /POEVT2/
C       - string will be deleted from /POSTRG/ (label -99)
C
C********************************************************************
      IMPLICIT NONE
      DOUBLE PRECISION am1 , am2 , am3 , am4 , chmass , cmass0 , 
     &                 cmass1 , cmass2 , dele , delm1 , delm2 , DEPS , 
     &                 DT_RNDM , EMIN , EPS , esum , fac , gam , gamb , 
     &                 p1
      DOUBLE PRECISION pc1 , pc2 , ptr
      INTEGER i , iccor , ictot , idev , ii , im1 , im2 , ineed , ip1 , 
     &        ip2 , ipos , Irej , irejl , ist , ista , istb , iter , 
     &        itouch , j1 , j2
      INTEGER k , k1 , k1a , k1b , k2 , k2a , k2b , niter
      SAVE 
 
      PARAMETER (EPS=1.D-10,EMIN=0.3D0,DEPS=1.D-15)
 
C  input/output channels
      INCLUDE 'inc/poinou'
C  event debugging information
      INCLUDE 'inc/podebg'
C  internal rejection counters
      INCLUDE 'inc/poloop'
C  model switches and parameters
      INCLUDE 'inc/pomdls'
 
C  standard particle data interface
 
 
      INCLUDE 'inc/poevt1'
C  extension to standard particle data interface (PHOJET specific)
      INCLUDE 'inc/poevt2'
 
C  color string configurations including collapsed strings and hadrons
      INCLUDE 'inc/postrg'
 
      DIMENSION pc1(4) , pc2(4) , p1(4) , ptr(4) , gam(3) , gamb(3)
 
      IF ( Irej.EQ.-1 ) THEN
         ictot = 0
         iccor = 0
         RETURN
      ELSE IF ( Irej.EQ.-2 ) THEN
         IF ( LPRi.GT.4 ) WRITE (LO,'(/1X,A,2I8/)')
     &         'PHO_MASCOR: total/converted strings' , ictot , iccor
         RETURN
      END IF
 
      Irej = 0
      niter = 100
      iter = 0
      ictot = ictot + ISTr
      IF ( ISWmdl(7).EQ.-1 ) RETURN
C  debug /POSTRG/
      IF ( IDEb(42).GE.25 ) CALL PHO_PRSTRG
 
      itouch = 0
 100  iter = iter + 1
      IF ( iter.GE.niter ) THEN
         Irej = 1
         IF ( IDEb(42).GE.2 ) THEN
            IF ( LPRi.GT.4 ) WRITE (LO,'(1X,A,2I5)')
     &            'PHO_MASCOR: rejection' , iter , niter
            IF ( IDEb(42).GE.10 ) CALL PHO_PREVNT(0)
         END IF
         RETURN
      END IF
 
C  check mass limits
      IF ( DT_RNDM(cmass0).LT.0.5D0 ) THEN
         im1 = 1
         im2 = ISTr
         ist = 1
      ELSE
         im1 = ISTr
         im2 = 1
         ist = -1
      END IF
      DO i = im1 , im2 , ist
         j1 = NPOs(1,i)
         cmass0 = PHEp(5,j1)
C  get masses
         IF ( NCOde(i).EQ.3 ) THEN
            CALL PHO_MEMASS(IPAr1(i),IPAr2(i),am1,am2,am3,am4,ip1,ip2)
         ELSE IF ( (NCOde(i).EQ.4) .OR. (NCOde(i).EQ.6) ) THEN
            CALL PHO_BAMASS(IPAr1(i),IPAr2(i),IPAr3(i),am1,am2,am3,am4,
     &                      ip1,ip2)
         ELSE IF ( NCOde(i).EQ.5 ) THEN
            CALL PHO_DQMASS(IPAr1(i),IPAr2(i),IPAr3(i),IPAr4(i),am1,am2)
            am3 = 0.D0
            am4 = 0.D0
            ip1 = 0
            ip2 = 0
         ELSE IF ( NCOde(i).EQ.7 ) THEN
            am1 = 0.15D0
            am2 = 0.3D0
            am3 = 0.765D0
            am4 = 1.5D0
C??????????????????????????????????
            ip1 = 23
            ip2 = 33
C??????????????????????????????????
         ELSE IF ( NCOde(i).LT.0 ) THEN
            GOTO 200
         ELSE
            IF ( LPRi.GT.4 ) WRITE (LO,'(/,1X,A,2I5)')
     &            'PHO_MASCOR:ERROR: string no,NCODE ' , j1 , NCOde(i)
            CALL PHO_ABORT
         END IF
         IF ( LPRi.GT.4 .AND. IDEb(42).GE.20 )
     &         WRITE (LO,'(1X,A,/3X,I3,5E11.3,2I5)')
     &         'PHO_MASCOR: string no CHMASS,AM1,AM2,AM3,AM4,IP1,IP2:' , 
     &        i , cmass0 , am1 , am2 , am3 , am4 , ip1 , ip2
C  select masses to correct
         IBHad(i) = 0
         NNCh(i) = 0
C  correction needed?
C  no resonances for diquark-antidiquark and gluon-gluon strings
         IF ( NCOde(i).NE.5 ) THEN
            ineed = 0
C  resonances possible
            IF ( ISWmdl(7).EQ.0 ) THEN
               IF ( cmass0.LT.am1*0.99D0 ) THEN
                  IBHad(i) = ip1
                  NNCh(i) = -1
                  chmass = am1
                  ineed = 1
               ELSE IF ( cmass0.LT.MIN(am2,am4)*1.2D0 ) THEN
                  delm1 = 1.D0/((cmass0-am1)**2+EPS)
                  delm2 = 1.D0/((cmass0-am3)**2+EPS)
                  IF ( DT_RNDM(delm1).LT.delm1/(delm1+delm2) ) THEN
                     IBHad(i) = ip1
                     NNCh(i) = -1
                     chmass = am1
                  ELSE
                     IBHad(i) = ip2
                     NNCh(i) = 1
                     chmass = am3
                  END IF
               END IF
            ELSE IF ( (ISWmdl(7).EQ.1) .OR. (ISWmdl(7).EQ.2) ) THEN
               IF ( cmass0.LT.am1*0.99 ) THEN
                  IBHad(i) = ip1
                  NNCh(i) = -1
                  chmass = am1
                  ineed = 1
               END IF
            ELSE IF ( ISWmdl(7).EQ.3 ) THEN
               IF ( cmass0.LT.am1 ) THEN
                  Irej = 1
                  RETURN
               END IF
            ELSE
               IF ( LPRi.GT.4 ) WRITE (LO,'(/1X,A,I5)')
     &               'PHO_MASCOR:ERROR:UNSUPPORTED ISWMDL(7)' , 
     &              ISWmdl(7)
               CALL PHO_ABORT
            END IF
         ELSE IF ( cmass0.LT.1.3D0*am1 ) THEN
            IF ( ISWmdl(7).LE.2 ) THEN
               IBHad(i) = 90
               NNCh(i) = -1
               chmass = am1*1.3D0
            ELSE
               Irej = 1
               RETURN
            END IF
         END IF
C
C  correction necessary?
         IF ( IBHad(i).NE.0 ) THEN
C  find largest invar. mass
            ipos = 0
            cmass1 = -1.D0
            DO j2 = NHEp , 3 , -1
 
               IF ( ABS(ISThep(j2)).EQ.1 ) THEN
                  IF ( (IPHist(1,j2).LE.0) .OR. (IPHist(1,j2).GT.ISTr) )
     &                 THEN
                     IF ( LPRi.GT.4 ) WRITE (LO,'(1X,2A,I7,I12)')
     &                     'PHO_MASCOR: ' , 
     &                    'inconsistent IPHIST(1,J2) entry (J2,KEV):' , 
     &                    j2 , KEVent
                     CALL PHO_PREVNT(0)
                  ELSE IF ( NCOde(IPHist(1,j2)).GT.0 ) THEN
                     cmass2 = (PHEp(4,j1)+PHEp(4,j2))
     &                        **2 - (PHEp(1,j1)+PHEp(1,j2))
     &                        **2 - (PHEp(2,j1)+PHEp(2,j2))
     &                        **2 - (PHEp(3,j1)+PHEp(3,j2))**2
                     IF ( cmass2.GT.cmass1 ) THEN
                        ipos = j2
                        cmass1 = cmass2
                     END IF
                  END IF
               END IF
 
            END DO
            j2 = ipos
            IF ( (j1.EQ.j2) .OR. (cmass1.LE.EMIN) ) THEN
               IF ( ineed.EQ.1 ) THEN
                  Irej = 1
                  RETURN
               ELSE
                  IBHad(i) = 0
                  NNCh(i) = 0
                  GOTO 200
               END IF
            END IF
            ista = ISThep(j1)
            istb = ISThep(j2)
            cmass1 = SQRT(cmass1)
            cmass2 = PHEp(5,j2)
            IF ( cmass1.LT.(cmass2+chmass) ) cmass2 = cmass1 - 
     &           1.1D0*chmass
            Irej = 1
            IF ( cmass2.GT.0.D0 ) CALL PHO_MSHELL(PHEp(1,j1),PHEp(1,j2),
     &           chmass,cmass2,pc1,pc2,Irej)
            IF ( Irej.NE.0 ) THEN
               IFAil(24) = IFAil(24) + 1
               IF ( IDEb(42).GE.2 ) THEN
                  IF ( LPRi.GT.4 ) WRITE (LO,'(1X,A,2I4)')
     &                  'PHO_MASCOR: rejection by PHO_MSHELL (J1,J2):' , 
     &                 j1 , j2
                  IF ( IDEb(42).GE.10 ) CALL PHO_PREVNT(0)
               END IF
               Irej = 1
               RETURN
            END IF
C  momentum transfer
            DO ii = 1 , 4
               ptr(ii) = PHEp(ii,j2) - pc2(ii)
            END DO
            IF ( LPRi.GT.4 .AND. IDEb(42).GE.10 )
     &            WRITE (LO,'(1X,A,/5X,2I3,4E12.3)')
     &            'PHO_MASCOR: J1,J2,transfer' , j1 , j2 , ptr
C  copy parents of strings
C  register partons belonging to first string
            IF ( IDHep(j1).EQ.90 ) THEN
               k1 = JMOhep(1,j1)
               k2 = MAX(JMOhep(1,j1),-JMOhep(2,j1))
               esum = 0.D0
               DO ii = k1 , k2
                  esum = esum + PHEp(4,ii)
               END DO
               IF ( JMOhep(2,j1).GT.0 ) esum = esum + 
     &              PHEp(4,JMOhep(2,j1))
               IF ( ABS(esum).LT.1.D-38 ) THEN
                  Irej = 1
                  RETURN
               END IF
               DO ii = k1 , k2
                  fac = PHEp(4,ii)/esum
                  DO k = 1 , 4
                     p1(k) = PHEp(k,ii) + fac*ptr(k)
                  END DO
                  CALL PHO_REGPAR(-1,IDHep(ii),0,j1,j2,p1(1),p1(2),
     &               p1(3),p1(4),IPHist(1,ii),IPHist(2,ii),ICOlor(1,ii),
     &               ICOlor(2,ii),ipos,1)
               END DO
               k1a = ipos + k1 - k2
               IF ( JMOhep(2,j1).GT.0 ) THEN
                  ii = JMOhep(2,j1)
                  fac = PHEp(4,ii)/esum
                  DO k = 1 , 4
                     p1(k) = PHEp(k,ii) + fac*ptr(k)
                  END DO
                  CALL PHO_REGPAR(-1,IDHep(ii),0,j1,j2,p1(1),p1(2),
     &               p1(3),p1(4),IPHist(1,ii),IPHist(2,ii),ICOlor(1,ii),
     &               ICOlor(2,ii),ipos,1)
               END IF
               k2a = -ipos
            ELSE
               k1a = j1
               k2a = j2
            END IF
C  register partons belonging to second string
            IF ( IDHep(j2).EQ.90 ) THEN
               CALL PHO_GETLTR(PHEp(1,j2),pc2,gam,gamb,dele,irejl)
               k1 = JMOhep(1,j2)
               k2 = MAX(JMOhep(1,j2),-JMOhep(2,j2))
               esum = 0.D0
               DO ii = k1 , k2
                  esum = esum + PHEp(4,ii)
               END DO
               IF ( JMOhep(2,j2).GT.0 ) esum = esum + 
     &              PHEp(4,JMOhep(2,j2))
               DO ii = k1 , k2
                  IF ( esum.GT.0.D0 ) fac = PHEp(4,ii)/esum
                  IF ( irejl.EQ.0 ) THEN
                     CALL PHO_MKSLTR(PHEp(1,ii),p1,gam,gamb)
                     p1(4) = p1(4) + fac*dele
                  ELSE
                     DO k = 1 , 4
                        p1(k) = PHEp(k,ii) - fac*ptr(k)
                     END DO
                  END IF
                  CALL PHO_REGPAR(-1,IDHep(ii),0,j1,j2,p1(1),p1(2),
     &               p1(3),p1(4),IPHist(1,ii),IPHist(2,ii),ICOlor(1,ii),
     &               ICOlor(2,ii),ipos,1)
               END DO
               k1b = ipos + k1 - k2
               IF ( JMOhep(2,j2).GT.0 ) THEN
                  ii = JMOhep(2,j2)
                  fac = PHEp(4,ii)/esum
                  IF ( irejl.EQ.0 ) THEN
                     CALL PHO_MKSLTR(PHEp(1,ii),p1,gam,gamb)
                     p1(4) = p1(4) + fac*dele
                  ELSE
                     DO k = 1 , 4
                        p1(k) = PHEp(k,ii) - fac*ptr(k)
                     END DO
                  END IF
                  CALL PHO_REGPAR(-1,IDHep(ii),0,j1,j2,p1(1),p1(2),
     &               p1(3),p1(4),IPHist(1,ii),IPHist(2,ii),ICOlor(1,ii),
     &               ICOlor(2,ii),ipos,1)
               END IF
               k2b = -ipos
            ELSE
               k1b = j1
               k2b = j2
            END IF
C  register first string/collapsed to hadron
            IF ( (ISWmdl(7).EQ.0) .OR. (ISWmdl(7).EQ.1) ) THEN
               IF ( NCOde(i).NE.5 ) THEN
                  CALL PHO_REGPAR(1,0,IBHad(i),k1a,k2a,pc1(1),pc1(2),
     &               pc1(3),pc1(4),IPHist(1,j1),IPHist(2,j1),0,0,ipos,1)
C  label string as collapsed to hadron/resonance
                  NCOde(i) = -99
                  IDHep(j1) = 92
               ELSE
                  CALL PHO_REGPAR(-1,90,0,k1a,k2a,pc1(1),pc1(2),pc1(3),
     &               pc1(4),IPHist(1,j1),IPHist(2,j1),0,0,ipos,1)
                  IDHep(j1) = 91
               END IF
               NPOs(1,i) = ipos
               NPOs(2,i) = k1a
               NPOs(3,i) = k2a
            ELSE
               CALL PHO_REGPAR(ista,IDHep(j1),IMPart(j1),k1a,k2a,pc1(1),
     &            pc1(2),pc1(3),pc1(4),IPHist(1,j1),IPHist(2,j1),
     &            ICOlor(1,j1),ICOlor(2,j1),ipos,1)
               IF ( IDHep(j1).EQ.90 ) THEN
                  NPOs(1,IPHist(1,j1)) = ipos
                  NPOs(2,IPHist(1,j1)) = k1a
                  NPOs(3,IPHist(1,j1)) = k2a
C  label string as collapsed to resonance-string
                  IDHep(j1) = 91
               ELSE IF ( (IPHist(1,j1).GE.1) .AND. 
     &                   (IPHist(1,j1).LE.ISTr) ) THEN
                  IF ( NPOs(1,IPHist(1,j1)).EQ.j1 ) NPOs(1,IPHist(1,j1))
     &                 = ipos
               END IF
            END IF
C  register second string/hadron/parton
            CALL PHO_REGPAR(istb,IDHep(j2),IMPart(j2),k1b,k2b,pc2(1),
     &                      pc2(2),pc2(3),pc2(4),IPHist(1,j2),
     &                      IPHist(2,j2),ICOlor(1,j2),ICOlor(2,j2),ipos,
     &                      1)
            IF ( IDHep(j2).EQ.90 ) THEN
               NPOs(1,IPHist(1,j2)) = ipos
               NPOs(2,IPHist(1,j2)) = k1b
               NPOs(3,IPHist(1,j2)) = k2b
C  label string touched by momentum transfer
               IDHep(j2) = 91
            ELSE IF ( (IPHist(1,j2).GE.1) .AND. (IPHist(1,j2).LE.ISTr) )
     &                THEN
               IF ( NPOs(1,IPHist(1,j2)).EQ.j2 ) NPOs(1,IPHist(1,j2))
     &              = ipos
            END IF
            iccor = iccor + 1
            itouch = itouch + 1
C  consistency checks
            IF ( IDEb(42).GE.5 ) THEN
               CALL PHO_CHECK(-1,idev)
               IF ( IDEb(42).GE.25 ) CALL PHO_PREVNT(0)
            END IF
C  jump to next iteration
            GOTO 100
         END IF
 200  END DO
C  debug output
      IF ( IDEb(42).GE.15 ) THEN
         IF ( (itouch.GT.0) .OR. (IDEb(42).GE.25) ) THEN
            IF ( LPRi.GT.4 ) WRITE (LO,'(1X,A,I5)')
     &            'PHO_MASCOR: iterations:' , iter
            CALL PHO_PREVNT(1)
         END IF
      END IF
      END SUBROUTINE
