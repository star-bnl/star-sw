
      SUBROUTINE PHO_STRING(Imode,Irej)
C********************************************************************
C
C    calculation of string combinatorics, Lorentz boosts and
C                   particle codes
C
C                - splitting of gluons
C                - strings will be built up from pairs of partons
C                  according to their color labels
C                  with IDHEP(..) = -1
C                - there can be other particles between to string partons
C                  (these will be unchanged by string construction)
C                - string mass fine correction
C
C    input:      IMODE    1  complete string processing
C                        -1 initialization
C                        -2 output of statistics
C
C    output:     /POSTRG/
C                IREJ    1 combination of strings impossible
C                        0 successful combination
C                       50 rejection due to user cutoffs
C
C********************************************************************
      IMPLICIT NONE
      DOUBLE PRECISION DEPS , EPS , p1 , p2 , p3 , p4 , sprob
      INTEGER i , i1 , i2 , iba1 , icg1 , icg2 , ich1 , icol1 , iddfs1 , 
     &        iddfs2 , iddpos , idev , idif1 , idif2 , igen , ii , 
     &        Imode , IPHO_BAR3 , IPHO_CHR3 , ipos
      INTEGER iq1 , iq2 , Irej , itemp , jm1 , jm2 , k , khdirs , 
     &        khpoms , kspoms , ksregs , lhdir , lhpom , lspom , lsreg , 
     &        mhard , msoft , nhepo , nrpom
      SAVE 
 
      PARAMETER (DEPS=1.D-15,EPS=1.D-5)
 
C  input/output channels
      INCLUDE 'inc/poinou'
C  event debugging information
      INCLUDE 'inc/podebg'
C  general process information
      INCLUDE 'inc/poprcs'
C  current beam selection
      INCLUDE 'inc/pobeam'
C  internal rejection counters
      INCLUDE 'inc/poloop'
C  model switches and parameters
      INCLUDE 'inc/pomdls'
C  hard cross sections and MC selection weights
      INCLUDE 'inc/pohrcs'
 
C  standard particle data interface
 
 
      INCLUDE 'inc/poevt1'
C  extension to standard particle data interface (PHOJET specific)
      INCLUDE 'inc/poevt2'
 
C  color string configurations including collapsed strings and hadrons
      INCLUDE 'inc/postrg'
C  table of particle indices for recursive PHOJET calls
      INCLUDE 'inc/porecu'
C  some constants
      INCLUDE 'inc/pocons'
 
      Irej = 0
      IF ( Imode.EQ.-1 ) THEN
         CALL PHO_POMCOR(-1)
         CALL PHO_MASCOR(-1)
         CALL PHO_PARCOR(-1,Irej)
 
         RETURN
      ELSE IF ( Imode.EQ.-2 ) THEN
         CALL PHO_POMCOR(-2)
         CALL PHO_MASCOR(-2)
         CALL PHO_PARCOR(-2,Irej)
 
         RETURN
      END IF
 
C  generate enhanced graphs
      IF ( IPOix2.GT.0 ) THEN
 50      i1 = MAX(1,IPOix1)
         i2 = IPOix2
         IF ( ISWmdl(14).EQ.1 ) IPOix1 = 0
         kspoms = KSPom - 1
         ksregs = KSReg
         khpoms = KHPom
         khdirs = KHDir
         iddfs1 = IDIfr1
         iddfs2 = IDIfr2
         iddpos = IDDpom
         DO i = i1 , i2
            IPOix3 = i
            KSPom = 0
            KSReg = 0
            KHPom = 0
            KHDir = 0
            IF ( IPOres(i).EQ.8 ) THEN
               KSPom = 2
               lspom = 2
               lhpom = 0
               lsreg = 0
               lhdir = 0
               igen = ABS(IPHist(2,IPOpos(1,i)))
               CALL PHO_STDPAR(IPOpos(1,i),IPOpos(2,i),igen,lspom,lsreg,
     &            lhpom,lhdir,Irej)
               IF ( Irej.NE.0 ) THEN
                  IF ( IDEb(4).GE.2 ) THEN
                     IF ( LPRi.GT.4 ) WRITE (LO,'(/1X,A,I5)')
     &                     'PHO_STRING: sec.rejection by PHO_STDPAR' , 
     &                    Irej
                     CALL PHO_PREVNT(-1)
                  END IF
                  RETURN
               END IF
               KSPom = kspoms + lspom
               KSReg = ksregs + lsreg
               KHPom = khpoms + lhpom
               KHDir = khdirs + lhdir
            ELSE IF ( IPOres(i).EQ.4 ) THEN
               itemp = ISWmdl(17)
               ISWmdl(17) = 0
               CALL PHO_CDIFF(IPOpos(1,i),IPOpos(2,i),msoft,mhard,1,
     &                        Irej)
               ISWmdl(17) = itemp
               IF ( Irej.NE.0 ) THEN
                  IF ( IDEb(4).GE.2 ) THEN
                     IF ( LPRi.GT.4 ) WRITE (LO,'(/1X,A,I5)')
     &                     'PHO_STRING: sec.rejection by PHO_CDIFF' , 
     &                    Irej
                     CALL PHO_PREVNT(-1)
                  END IF
                  RETURN
               END IF
               KSDpo = KSDpo + 1
               KSPom = kspoms + KSPom
               KSReg = ksregs + KSReg
               KHPom = khpoms + KHPom
               KHDir = khdirs + KHDir
            ELSE
               idif1 = 1
               idif2 = 1
               IF ( IPOres(i).EQ.5 ) THEN
                  idif2 = 0
                  KSTrg = KSTrg + 1
               ELSE IF ( IPOres(i).EQ.6 ) THEN
                  idif1 = 0
                  KSTrg = KSTrg + 1
               ELSE
                  KSLoo = KSLoo + 1
               END IF
               itemp = ISWmdl(16)
               ISWmdl(16) = 0
               sprob = 1.D0
               CALL PHO_DIFDIS(idif1,idif2,IPOpos(1,i),IPOpos(2,i),
     &            sprob,0,msoft,mhard,Irej)
               ISWmdl(16) = itemp
               IF ( Irej.NE.0 ) THEN
                  IF ( IDEb(4).GE.2 ) THEN
                     IF ( LPRi.GT.4 ) WRITE (LO,'(/1X,A,I5)')
     &                     'PHO_STRING: sec.rejection by PHO_DIFDIS' , 
     &                    Irej
                     CALL PHO_PREVNT(-1)
                  END IF
                  RETURN
               END IF
               KSPom = kspoms + KSPom
               KSReg = ksregs + KSReg
               KHPom = khpoms + KHPom
               KHDir = khdirs + KHDir
            END IF
            IDIfr1 = iddfs1
            IDIfr2 = iddfs2
            IDDpom = iddpos
         END DO
         IF ( IPOix2.GT.i2 ) THEN
            IPOix1 = i2 + 1
            GOTO 50
         END IF
      END IF
 
C  optional: split gluons to q-qbar pairs
      IF ( ISWmdl(9).GT.0 ) THEN
         nhepo = NHEp
         DO i = 3 , nhepo
            IF ( (ISThep(i).EQ.-1) .AND. (IDHep(i).EQ.21) ) THEN
               icg1 = ICOlor(1,i)
               icg2 = ICOlor(2,i)
               iq1 = 0
               iq2 = 0
               DO k = 3 , nhepo
                  IF ( ICOlor(1,k).EQ.-icg1 ) THEN
                     iq1 = k
                     IF ( iq1*iq2.NE.0 ) GOTO 60
                  ELSE IF ( ICOlor(1,k).EQ.-icg2 ) THEN
                     iq2 = k
                     IF ( iq1*iq2.NE.0 ) GOTO 60
                  END IF
               END DO
               IF ( LPRi.GT.4 ) WRITE (LO,'(/1X,2A,3I6)')
     &               'PHO_STRING:ERROR:(1) ' , 
     &              'no matching color found (IG,ICG1,ICG2)' , i , 
     &              icg1 , icg2
               CALL PHO_ABORT
 60            CALL PHO_GLU2QU(i,iq1,iq2,Irej)
               IF ( Irej.NE.0 ) THEN
                  IF ( IDEb(19).GE.5 ) THEN
                     IF ( LPRi.GT.4 ) WRITE (LO,'(/,1X,A)')
     &                     'PHO_STRING: no gluon splitting possible'
                     CALL PHO_PREVNT(0)
                  END IF
                  RETURN
               END IF
            END IF
         END DO
      END IF
 
C  construct strings and write entries sorted by strings
 
      ISTr = ISTr + 1
      nhepo = NHEp
      DO i = 3 , nhepo
 
         IF ( ISTr.GT.MSTR ) THEN
            IF ( LPRi.GT.4 ) WRITE (LO,'(1X,2A,2I4)') 'PHO_STRING: ' , 
     &           'event has too many strings (ISTR,MSTR):' , ISTr , MSTR
            CALL PHO_PREVNT(0)
            Irej = 1
            RETURN
         END IF
 
         IF ( ISThep(i).EQ.1 ) THEN
C  hadrons / resonances / clusters
            NPOs(1,ISTr) = i
            NPOs(2,ISTr) = 0
            NPOs(3,ISTr) = 0
            NPOs(4,ISTr) = ABS(IPHist(2,i))
            NCOde(ISTr) = -99
            IPHist(1,i) = ISTr
C**anfe For (semi-) stable particles set IDs to 0 to keep
C       printouts consistent
            IPAr1(ISTr) = 0
            IPAr2(ISTr) = 0
            IPAr3(ISTr) = 0
            IPAr4(ISTr) = 0
            ISTr = ISTr + 1
         ELSE IF ( (ISThep(i).EQ.-1) .AND. (IDHep(i).NE.21) ) THEN
C  quark /diquark terminated strings
            icol1 = -ICOlor(1,i)
            p1 = PHEp(1,i)
            p2 = PHEp(2,i)
            p3 = PHEp(3,i)
            p4 = PHEp(4,i)
            ich1 = IPHO_CHR3(i,2)
            iba1 = IPHO_BAR3(i,2)
            CALL PHO_REGPAR(-1,IDHep(i),IMPart(i),i,0,p1,p2,p3,p4,
     &                      IPHist(1,i),IPHist(2,i),ICOlor(1,i),
     &                      ICOlor(2,i),ipos,1)
            jm1 = ipos
 
            nrpom = 0
 80         DO k = 3 , nhepo
               IF ( ISThep(k).EQ.-1 ) THEN
                  IF ( IDHep(k).EQ.21 ) THEN
                     IF ( ICOlor(1,k).EQ.icol1 ) THEN
                        icol1 = -ICOlor(2,k)
                        GOTO 100
                     ELSE IF ( ICOlor(2,k).EQ.icol1 ) THEN
                        icol1 = -ICOlor(1,k)
                        GOTO 100
                     END IF
                  ELSE IF ( ICOlor(1,k).EQ.icol1 ) THEN
                     icol1 = 0
                     GOTO 100
                  END IF
               END IF
            END DO
            IF ( LPRi.GT.4 ) WRITE (LO,'(/1X,A,I5)')
     &            'PHO_STRING:ERROR:(2) no matching color found for' , 
     &           -icol1
            CALL PHO_ABORT
 100        p1 = p1 + PHEp(1,k)
            p2 = p2 + PHEp(2,k)
            p3 = p3 + PHEp(3,k)
            p4 = p4 + PHEp(4,k)
            nrpom = MAX(nrpom,IPHist(1,k))
            ich1 = ich1 + IPHO_CHR3(k,2)
            iba1 = iba1 + IPHO_BAR3(k,2)
            CALL PHO_REGPAR(-1,IDHep(k),IMPart(k),k,0,PHEp(1,k),
     &                      PHEp(2,k),PHEp(3,k),PHEp(4,k),IPHist(1,k),
     &                      IPHist(2,k),ICOlor(1,k),ICOlor(2,k),ipos,1)
C  further parton involved?
            IF ( icol1.NE.0 ) GOTO 80
            jm2 = ipos
C  register string
            igen = IPHist(2,k)
            CALL PHO_REGPAR(-1,90,0,jm1,-jm2,p1,p2,p3,p4,ISTr,igen,ich1,
     &                      iba1,ipos,1)
C  store additional string information
            NPOs(1,ISTr) = ipos
            NPOs(2,ISTr) = jm1
            NPOs(3,ISTr) = -jm2
            NPOs(4,ISTr) = ABS(IPHist(2,k))
C  calculate CPC string codes
            CALL PHO_ID2STR(IDHep(jm1),IDHep(jm2),NCOde(ISTr),
     &                      IPAr1(ISTr),IPAr2(ISTr),IPAr3(ISTr),
     &                      IPAr4(ISTr))
            ISTr = ISTr + 1
         END IF
      END DO
 
      DO i = 3 , nhepo
 
         IF ( ISTr.GT.MSTR ) THEN
            IF ( LPRi.GT.4 ) WRITE (LO,'(1X,2A,2I4)') 'PHO_STRING: ' , 
     &           'event has too many strings (ISTR,MSTR):' , ISTr , MSTR
            CALL PHO_PREVNT(0)
            Irej = 1
            RETURN
         END IF
 
         IF ( ISThep(i).EQ.-1 ) THEN
C  gluon loop-strings
            icol1 = -ICOlor(1,i)
            p1 = PHEp(1,i)
            p2 = PHEp(2,i)
            p3 = PHEp(3,i)
            p4 = PHEp(4,i)
            iba1 = 0
            ich1 = 0
            CALL PHO_REGPAR(-1,IDHep(i),IMPart(i),i,0,p1,p2,p3,p4,
     &                      IPHist(1,i),IPHist(2,i),ICOlor(1,i),
     &                      ICOlor(2,i),ipos,1)
            jm1 = ipos
C
            nrpom = 0
            DO WHILE ( ICOlor(2,i).NE.icol1 )
               DO k = i , nhepo
                  IF ( ISThep(k).EQ.-1 ) THEN
                     IF ( ICOlor(1,k).EQ.icol1 ) THEN
                        icol1 = -ICOlor(2,k)
                        GOTO 110
                     ELSE IF ( ICOlor(2,k).EQ.icol1 ) THEN
                        icol1 = -ICOlor(1,k)
                        GOTO 110
                     END IF
                  END IF
               END DO
               IF ( LPRi.GT.4 ) WRITE (LO,'(/1X,A,I5)')
     &               'PHO_STRING:ERROR:(3) no matching color found for'
     &              , -icol1
               CALL PHO_ABORT
 110           p1 = p1 + PHEp(1,k)
               p2 = p2 + PHEp(2,k)
               p3 = p3 + PHEp(3,k)
               p4 = p4 + PHEp(4,k)
               nrpom = MAX(nrpom,IPHist(1,k))
               CALL PHO_REGPAR(-1,IDHep(k),IMPart(k),k,0,PHEp(1,k),
     &            PHEp(2,k),PHEp(3,k),PHEp(4,k),IPHist(1,k),IPHist(2,k),
     &            ICOlor(1,k),ICOlor(2,k),ipos,1)
C  further parton involved?
               IF ( icol1.EQ.0 ) GOTO 120
            END DO
 120        jm2 = ipos
C  register string
            igen = IPHist(2,k)
            CALL PHO_REGPAR(-1,90,0,jm1,-jm2,p1,p2,p3,p4,ISTr,igen,ich1,
     &                      iba1,ipos,1)
C  store additional string information
            NPOs(1,ISTr) = ipos
            NPOs(2,ISTr) = jm1
            NPOs(3,ISTr) = -jm2
            NPOs(4,ISTr) = ABS(IPHist(2,k))
C  calculate CPC string codes
            CALL PHO_ID2STR(IDHep(jm1),IDHep(jm2),NCOde(ISTr),
     &                      IPAr1(ISTr),IPAr2(ISTr),IPAr3(ISTr),
     &                      IPAr4(ISTr))
            ISTr = ISTr + 1
         END IF
      END DO
 
      ISTr = ISTr - 1
 
      IF ( IDEb(19).GE.17 ) THEN
         IF ( LPRi.GT.4 ) WRITE (LO,'(1X,A)')
     &         'PHO_STRING: after string construction'
         CALL PHO_PREVNT(0)
      END IF
 
C  pomeron corrections
      CALL PHO_POMCOR(Irej)
      IF ( Irej.NE.0 ) THEN
         IFAil(38) = IFAil(38) + 1
         IF ( IDEb(19).GE.3 ) THEN
            IF ( LPRi.GT.4 ) WRITE (LO,'(1X,A,I6)')
     &            'PHO_STRING: rejection by PHO_POMCOR (IREJ)' , Irej
            CALL PHO_PREVNT(-1)
         END IF
         RETURN
      END IF
 
C  string mass corrections
      CALL PHO_MASCOR(Irej)
      IF ( Irej.NE.0 ) THEN
         IFAil(34) = IFAil(34) + 1
         IF ( IDEb(19).GE.3 ) THEN
            IF ( LPRi.GT.4 ) WRITE (LO,'(1X,A,I6)')
     &            'PHO_STRING: rejection by PHO_MASCOR (IREJ)' , Irej
            CALL PHO_PREVNT(-1)
         END IF
         RETURN
      END IF
 
C  parton mass corrections
      DO i = 1 , ISTr
         IF ( NCOde(i).GE.0 ) THEN
            CALL PHO_PARCOR(NPOs(1,i),Irej)
            IF ( Irej.NE.0 ) THEN
               IFAil(35) = IFAil(35) + 1
               IF ( IDEb(19).GE.3 ) THEN
                  IF ( LPRi.GT.4 ) WRITE (LO,'(1X,A,I6)')
     &                  'PHO_STRING: rejection by PHO_PARCOR (IREJ)' , 
     &                 Irej
                  CALL PHO_PREVNT(-1)
               END IF
               RETURN
            END IF
         END IF
      END DO
 
C  statistics of hard processes
      DO i = 3 , NHEp
         IF ( ISThep(i).EQ.25 ) THEN
            k = IMPart(i)
            ii = IDHep(i)
            MH_acc_2(k,ii,IDXmpar) = MH_acc_2(k,ii,IDXmpar) + 1
         END IF
      END DO
 
C  debug: write out strings
      IF ( IDEb(19).GE.5 ) THEN
         IF ( IDEb(19).GE.10 ) CALL PHO_CHECK(1,idev)
         IF ( IDEb(19).GE.15 ) THEN
            CALL PHO_PREVNT(0)
         ELSE
            CALL PHO_PRSTRG
         END IF
      END IF
 
      END SUBROUTINE
