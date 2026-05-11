
      SUBROUTINE PHO_POSPOM(Ip,Ind1,Ind2,Igen,Ipom,Kcut,Iswap,Irej)
C***********************************************************************
C
C     registration of one cut pomeron (soft/semihard)
C
C     input:   IP      particle combination the pomeron belongs to
C              IND1,2  position of X values in /POSOFT/
C                      1 corresponds to a valence-pomeron
C              IGEN    production process of mother particles
C              IPOM    pomeron number
C              KCUT    total number of cut pomerons and reggeons
C
C     output:  ISWAP   exchange of x values
C              IND1,2  increased by the number of partons belonging
C                      to the generated pomeron cut
C              IREJ    success/failure
C
C**********************************************************************
      IMPLICIT NONE
      DOUBLE PRECISION cmass1 , cmass2 , DEPS , DT_RNDM , ea1 , ea2 , 
     &                 eb1 , eb2 , esub , fac1 , fac2 , p1 , p2 , pacc , 
     &                 sum , wgx , wgxcdf , wgxhdd , wgxhsd , wgxpom
      DOUBLE PRECISION xi
      INTEGER i , i1 , i2 , ica1 , ica2 , icb1 , icb2 , icc1 , icc2 , 
     &        icd1 , icd2 , idha1 , idha2 , idpd1 , idpd2 , idum , 
     &        ifla1 , ifla2 , iflb1 , iflb2
      INTEGER Igen , Ind1 , Ind2 , indx1 , indx2 , Ip , IPHO_ID2PDG , 
     &        IPHO_PDG2ID , Ipom , ipos , ipos1 , ipos2 , Irej , isam , 
     &        Iswap , jm1 , jm2 , Kcut
      SAVE 
 
      PARAMETER (DEPS=1.D-8)
 
C  input/output channels
      INCLUDE 'inc/poinou'
C  event debugging information
      INCLUDE 'inc/podebg'
C  internal rejection counters
      INCLUDE 'inc/poloop'
C  current beam selection
      INCLUDE 'inc/pobeam'
C  model switches and parameters
      INCLUDE 'inc/pomdls'
C  general process information
      INCLUDE 'inc/poprcs'
C  global event kinematics and particle IDs
      INCLUDE 'inc/pogcms'
C  data of c.m. system of Pomeron / Reggeon exchange
      INCLUDE 'inc/popcms'
C  obsolete cut-off information
      INCLUDE 'inc/pocut1'
C  energy-interpolation table
      INCLUDE 'inc/potabl'
C  light-cone x fractions and c.m. momenta of soft cut string ends
      INCLUDE 'inc/posoft'
 
C  standard particle data interface
 
 
      INCLUDE 'inc/poevt1'
C  extension to standard particle data interface (PHOJET specific)
      INCLUDE 'inc/poevt2'
 
C  table of particle indices for recursive PHOJET calls
      INCLUDE 'inc/porecu'
 
      DIMENSION p1(4) , p2(4) , wgxhsd(2) , wgx(6)
 
      Irej = 0
      Iswap = 0
      jm1 = NPOsp(1)
      jm2 = NPOsp(2)
      indx1 = Ind1
      indx2 = Ind2
      ea1 = XS1(Ind1)*ECMp/2.D0
      ea2 = XS1(Ind1+1)*ECMp/2.D0
      eb1 = XS2(Ind2)*ECMp/2.D0
      eb2 = XS2(Ind2+1)*ECMp/2.D0
      cmass1 = MIN(ea1,ea2)
      cmass2 = MIN(eb1,eb2)
      ipos1 = 0
      ipos2 = 0
      ipos = 0
 
C  debug output
      IF ( IDEb(9).GE.20 ) THEN
         IF ( LPRi.GT.4 ) WRITE (LO,'(1X,2A,5I4)') 'PHO_POSPOM: ' , 
     &        'IP,IND1,IND2,KCUT,IPOIX1' , Ip , Ind1 , Ind2 , Kcut , 
     &        IPOix1
         IF ( LPRi.GT.4 ) WRITE (LO,'(1X,A,2I4,1P2E12.4)')
     &         'MOTHER1/2,MASS1/2' , jm1 , jm2 , cmass1 , cmass2
      END IF
 
C  flavours
      IF ( Ind1.EQ.1 ) THEN
         CALL PHO_VALFLA(jm1,ifla1,ifla2,ea1,ea2)
      ELSE
         CALL PHO_SEAFLA(jm1,ifla1,ifla2,cmass1)
      END IF
      IF ( Ind2.EQ.1 ) THEN
         CALL PHO_VALFLA(jm2,iflb1,iflb2,eb1,eb2)
      ELSE
         CALL PHO_SEAFLA(jm2,iflb1,iflb2,cmass2)
      END IF
      DO i = 1 , 4
         p1(i) = PSOft1(i,Ind1) + PSOft1(i,Ind1+1)
         p2(i) = PSOft2(i,Ind2) + PSOft2(i,Ind2+1)
      END DO
 
C  pomeron resolved?
      IF ( (ISWmdl(14).GT.0) .AND. (IPOix1.GT.0) ) THEN
C  find energy for cross section calculation
         IF ( IPAmdl(16).EQ.2 ) THEN
            esub = ECMp
         ELSE IF ( IPAmdl(16).EQ.3 ) THEN
            IF ( IPRoce.EQ.1 ) THEN
               esub = ECM
            ELSE
               esub = ECMp
            END IF
         ELSE
            esub = SQRT((p1(4)+p2(4))**2-(p1(1)+p2(1))**2-(p1(2)+p2(2))
     &             **2-(p1(3)+p2(3))**2)
         END IF
C  load cross sections from interpolation table
         IF ( esub.LE.SIGecm(1,Ip,IDXmpar) ) THEN
            i1 = 1
            i2 = 2
         ELSE IF ( esub.LT.SIGecm(ISImax(IDXmpar),Ip,IDXmpar) ) THEN
            DO i = 2 , ISImax(IDXmpar)
               IF ( esub.LE.SIGecm(i,Ip,IDXmpar) ) GOTO 20
            END DO
 20         i1 = i - 1
            i2 = i
         ELSE
            IF ( LPRi.GT.4 ) WRITE (LO,'(/1X,A,2E12.3)')
     &            'PHO_POSPOM: energy too high' , esub , 
     &           SIGecm(ISImax(IDXmpar),Ip,IDXmpar)
            CALL PHO_PREVNT(-1)
            i1 = ISImax(IDXmpar) - 1
            i2 = ISImax(IDXmpar)
         END IF
         fac2 = 0.D0
         IF ( i1.NE.i2 ) fac2 = LOG(esub/SIGecm(i1,Ip,IDXmpar))
     &        /LOG(SIGecm(i2,Ip,IDXmpar)/SIGecm(i1,Ip,IDXmpar))
         fac1 = 1.D0 - fac2
 
         wgxpom = fac2*(SIGtab(56,i2,Ip,IDXmpar)
     &            +SIGtab(57,i2,Ip,IDXmpar))
     &            + fac1*(SIGtab(56,i1,Ip,IDXmpar)
     &            +SIGtab(57,i1,Ip,IDXmpar))
         wgxhsd(1) = fac2*SIGtab(60,i2,Ip,IDXmpar)
     &               + fac1*SIGtab(60,i1,Ip,IDXmpar)
         wgxhsd(2) = fac2*SIGtab(62,i2,Ip,IDXmpar)
     &               + fac1*SIGtab(62,i1,Ip,IDXmpar)
         wgxhdd = fac2*(SIGtab(61,i2,Ip,IDXmpar)
     &            +SIGtab(63,i2,Ip,IDXmpar)+SIGtab(64,i2,Ip,IDXmpar))
     &            + fac1*(SIGtab(61,i1,Ip,IDXmpar)
     &            +SIGtab(63,i1,Ip,IDXmpar)+SIGtab(64,i1,Ip,IDXmpar))
         wgxcdf = fac2*(SIGtab(65,i2,Ip,IDXmpar)
     &            +SIGtab(66,i2,Ip,IDXmpar)+SIGtab(67,i2,Ip,IDXmpar)
     &            +SIGtab(68,i2,Ip,IDXmpar))
     &            + fac1*(SIGtab(65,i1,Ip,IDXmpar)
     &            +SIGtab(66,i1,Ip,IDXmpar)+SIGtab(67,i1,Ip,IDXmpar)
     &            +SIGtab(68,i1,Ip,IDXmpar))
 
C  one-pomeron cut
         wgx(1) = wgxpom - 3.D0*(wgxhsd(1)+wgxhsd(2)+wgxhdd)
     &            + 15.D0*wgxcdf
C  central diff. cut
         wgx(2) = wgxcdf
C  diff. diss. of particle 1
         wgx(3) = wgxhsd(1)
C  diff. diss. of particle 2
         wgx(4) = wgxhsd(2)
C  double diff. dissociation
         wgx(5) = wgxhdd
C  two-pomeron cut
         wgx(6) = 2.D0*(wgxhsd(1)+wgxhsd(2)+wgxhdd)
 
C       IF((WGX(1).LT.0.D0).AND.((IP.EQ.1).OR.(IDEB(9).GE.1))) THEN
C         WRITE(LO,'(1X,A,/1X,A,I3,1P,2E11.3)') ' PHO_POSPOM: ',
C    &      ' unitarity bound reached for ',IP,ESUB,WGX(1)
C         WRITE(LO,'(5X,A)') 'WGXHSD(1),WGXHSD(2),WGXHDD,WGXCDF,WGXPOM:'
C         WRITE(LO,'(5X,1P5E11.3)') WGXHSD,WGXHDD,WGXCDF,WGXPOM
C         WRITE(LO,'(5X,A,/,5X,1P,6E11.3)') 'weight factors WG(1-6)',WGX
C       ENDIF
 
         sum = wgx(1) + wgx(2) + wgx(3) + wgx(4) + wgx(5) + wgx(6)
 
C  selection loop
 50      xi = DT_RNDM(sum)*sum
         i = 0
         sum = 0.D0
 100     i = i + 1
         sum = sum + wgx(i)
         IF ( (xi.GT.sum) .AND. (i.LT.6) ) GOTO 100
C  phase space correction
         IF ( i.NE.1 ) THEN
            isam = 4
            IF ( i.EQ.6 ) isam = 8
            pacc = EXP(-PARmdl(8)*DBLE(isam*PARmdl(160+Ip))/esub)
C         IF(DT_RNDM(SUM).GT.PACC) I=1
            IF ( DT_RNDM(sum).GT.pacc ) GOTO 50
         END IF
 
C  do not generate diffraction for events with only one cut pomeron
         IF ( (Kcut.EQ.1) .AND. (i.LT.6) ) i = 1
 
C  do not generate recursive calls for remants with
C  diquark-anti-diquark flavour contents
         IF ( (ABS(ifla1).GT.1000) .AND. (ifla1+ifla2.EQ.0) ) i = 1
         IF ( (ABS(iflb1).GT.1000) .AND. (iflb1+iflb2.EQ.0) ) i = 1
 
C  debug output
         IF ( LPRi.GT.4 .AND. IDEb(9).GE.20 )
     &         WRITE (LO,'(1X,A,/1X,I2,1P7E11.3)')
     &         'PHO_POSPOM: IPRO,ESUB,WGX(1-6)' , i , esub , wgx
 
         IF ( i.GT.1 ) THEN
            CALL PHO_HACODE(ifla1,ifla2,idha1,idum)
            CALL PHO_HACODE(iflb1,iflb2,idha2,idum)
            idpd1 = IPHO_ID2PDG(idha1)
            idpd2 = IPHO_ID2PDG(idha2)
C**anfe Replace pi0 as lightest possible u-u~/d-d~ hadron and
C     and rho as lightest c-c~ hadron with f(0) and f(2)
            IF ( ifla1.EQ.ifla2 ) THEN
               IF ( ABS(ifla1).LE.2 ) THEN
                  idpd1 = 10221
                  idha1 = IPHO_PDG2ID(10221)
               ELSE
                  idpd1 = 225
                  idha1 = IPHO_PDG2ID(225)
               END IF
            END IF
            IF ( iflb1.EQ.iflb2 ) THEN
               IF ( ABS(iflb1).LE.2 ) THEN
                  idpd2 = 10221
                  idha2 = IPHO_PDG2ID(10221)
               ELSE
                  idpd2 = 225
                  idha2 = IPHO_PDG2ID(225)
               END IF
            END IF
 
            CALL PHO_REGPAR(1,idpd1,idha1,jm1,jm2,p1(1),p1(2),p1(3),
     &                      p1(4),Ipom,Igen,0,0,ipos1,1)
            CALL PHO_REGPAR(1,idpd2,idha2,jm2,jm1,p2(1),p2(2),p2(3),
     &                      p2(4),Ipom,Igen,0,0,ipos1,1)
 
            Ind1 = Ind1 + 2
            Ind2 = Ind2 + 2
C  update index
            IPOix2 = IPOix2 + 1
 
            IF ( IPOix2.GT.MAXIPX ) THEN
               IF ( LPRi.GT.4 ) WRITE (LO,'(1X,2A,2I5)')
     &               'PHO_POSPOM: no space left in ' , 
     &              '/PORECU/ (IPOIX2,MAXIPX):' , IPOix2 , MAXIPX
               Irej = 1
               RETURN
            END IF
 
            IPOres(IPOix2) = i + 2
            IPOpos(1,IPOix2) = ipos1 - 1
            IPOpos(2,IPOix2) = ipos1
            RETURN
         END IF
      END IF
 
C100  CONTINUE
      IF ( ISWmdl(12).EQ.0 ) THEN
C  sample colors
         CALL PHO_SELCOL(0,0,ica1,ica2,icb1,icb2,1)
         CALL PHO_SELCOL(0,0,icc1,icc2,icd1,icd2,1)
 
C  purely gluonic pomeron or sea strings formed by gluons
 
         IF ( ((IDHep(jm1).EQ.990) .AND. (IPAmdl(20).GT.0)) .OR. 
     &        ((IPAmdl(19).EQ.1) .AND. (Ind1.NE.1)) ) THEN
            ifla1 = 21
            ifla2 = 21
         END IF
         IF ( ((IDHep(jm2).EQ.990) .AND. (IPAmdl(20).GT.0)) .OR. 
     &        ((IPAmdl(19).EQ.1) .AND. (Ind2.NE.1)) ) THEN
            iflb1 = 21
            iflb2 = 21
         END IF
 
C  color connection
         IF ( ifla1.NE.21 ) THEN
            IF ( ((ABS(ifla1).GT.6) .AND. (ifla1.GT.0)) .OR. 
     &           ((ABS(ifla1).LE.6) .AND. (ifla1.LT.0)) )
     &           CALL PHO_SWAPI(ica1,icd1)
         END IF
         IF ( iflb1.NE.21 ) THEN
            IF ( ((ABS(iflb1).GT.6) .AND. (iflb1.LT.0)) .OR. 
     &           ((ABS(iflb1).LE.6) .AND. (iflb1.GT.0)) )
     &           CALL PHO_SWAPI(icb1,icc1)
         END IF
         Iswap = 0
         IF ( ica1*icb1.GT.0 ) THEN
            IF ( (Ind1.NE.1) .AND. (Ind2.NE.1) ) THEN
               IF ( DT_RNDM(cmass1).GT.0.5D0 ) THEN
                  CALL PHO_SWAPI(ifla1,ifla2)
                  CALL PHO_SWAPI(ica1,icd1)
               ELSE
                  CALL PHO_SWAPI(iflb1,iflb2)
                  CALL PHO_SWAPI(icb1,icc1)
               END IF
            ELSE IF ( Ind1.NE.1 ) THEN
               CALL PHO_SWAPI(ifla1,ifla2)
               CALL PHO_SWAPI(ica1,icd1)
            ELSE IF ( Ind2.NE.1 ) THEN
               CALL PHO_SWAPI(iflb1,iflb2)
               CALL PHO_SWAPI(icb1,icc1)
            ELSE IF ( (ifla1.EQ.-ifla2) .AND. (iflb1.EQ.-iflb2) ) THEN
               IF ( DT_RNDM(cmass1).GT.0.5D0 ) THEN
                  CALL PHO_SWAPI(ifla1,ifla2)
                  CALL PHO_SWAPI(ica1,icd1)
               ELSE
                  CALL PHO_SWAPI(iflb1,iflb2)
                  CALL PHO_SWAPI(icb1,icc1)
               END IF
            ELSE IF ( ifla1.EQ.-ifla2 ) THEN
               CALL PHO_SWAPI(ifla1,ifla2)
               CALL PHO_SWAPI(ica1,icd1)
            ELSE IF ( iflb1.EQ.-iflb2 ) THEN
               CALL PHO_SWAPI(iflb1,iflb2)
               CALL PHO_SWAPI(icb1,icc1)
            ELSE
               Iswap = 1
               IF ( IDEb(9).GE.5 ) THEN
                  IF ( LPRi.GT.4 ) WRITE (LO,'(1X,A,I12)')
     &                  'PHO_POSPOM: string end swap (KEVENT)' , KEVent
                  IF ( LPRi.GT.4 ) WRITE (LO,'(5X,A,4I7)') 'flavors:' , 
     &                 ifla1 , ifla2 , iflb1 , iflb2
                  IF ( LPRi.GT.4 ) WRITE (LO,'(5X,A,4I7)') 'colors :' , 
     &                 ica1 , icd1 , icb1 , icc1
               END IF
            END IF
         END IF
 
C  registration
 
C  purely gluonic pomeron or sea strings formed by gluons
         IF ( ifla1.EQ.21 ) THEN
            CALL PHO_REGPAR(-1,21,0,jm1,jm2,p1(1),p1(2),p1(3),p1(4),
     &                      Ipom,Igen,ica1,icd1,ipos1,1)
            Ind1 = Ind1 + 2
 
C  strings formed by quarks
         ELSE
C  valence quark labels
            IF ( (indx1.EQ.1) .AND. (IPHist(2,jm1).GE.0) .AND. 
     &           (IDHep(jm1).NE.990) ) THEN
               ica2 = 1
               icd2 = 1
            END IF
C  registration
            CALL PHO_REGPAR(-1,ifla1,0,jm1,jm2,PSOft1(1,Ind1),
     &                      PSOft1(2,Ind1),PSOft1(3,Ind1),PSOft1(4,Ind1)
     &                      ,Ipom,Igen,ica1,ica2,ipos1,1)
            Ind1 = Ind1 + 1
            CALL PHO_REGPAR(-1,ifla2,0,jm1,jm2,PSOft1(1,Ind1),
     &                      PSOft1(2,Ind1),PSOft1(3,Ind1),PSOft1(4,Ind1)
     &                      ,Ipom,Igen,icd1,icd2,ipos,1)
            Ind1 = Ind1 + 1
 
         END IF
 
C  purely gluonic pomeron or sea strings formed by gluons
         IF ( iflb1.EQ.21 ) THEN
            CALL PHO_REGPAR(-1,iflb1,0,jm2,jm1,p2(1),p2(2),p2(3),p2(4),
     &                      Ipom,Igen,icb1,icc1,ipos2,1)
            Ind2 = Ind2 + 2
 
C  strings formed by quarks
         ELSE
C  valence quark labels
            IF ( (indx2.EQ.1) .AND. (IPHist(2,jm2).GE.0) .AND. 
     &           (IDHep(jm2).NE.990) ) THEN
               icb2 = 1
               icc2 = 1
            END IF
C  registration
            CALL PHO_REGPAR(-1,iflb1,0,jm2,jm1,PSOft2(1,Ind2),
     &                      PSOft2(2,Ind2),PSOft2(3,Ind2),PSOft2(4,Ind2)
     &                      ,Ipom,Igen,icb1,icb2,ipos,1)
            Ind2 = Ind2 + 1
            CALL PHO_REGPAR(-1,iflb2,0,jm2,jm1,PSOft2(1,Ind2),
     &                      PSOft2(2,Ind2),PSOft2(3,Ind2),PSOft2(4,Ind2)
     &                      ,Ipom,Igen,icc1,icc2,ipos2,1)
            Ind2 = Ind2 + 1
 
         END IF
 
C  soft pt assignment
         IF ( ISWmdl(18).EQ.0 ) THEN
            CALL PHO_PARTPT(0,ipos1,ipos2,PTCut(Ip),Irej)
            IF ( Irej.NE.0 ) THEN
               IFAil(25) = IFAil(25) + 1
               RETURN
            END IF
         END IF
C       CALL PHO_BFKL(P1,P2,IPART,IREJ)
C       IF(IREJ.NE.0) RETURN
      END IF
 
      END SUBROUTINE
