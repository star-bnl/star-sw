
      SUBROUTINE PHO_POMCOR(Irej)
C********************************************************************
C
C    join quarks to gluons in case of too small masses
C
C    input:              /POEVT1/
C                        /POSTRG/
C                IREJ    -1          initialization
C                        -2          output of statistics
C
C    output:             /POEVT1/
C                        /POSTRG/
C                IREJ    0  successful
C                        1  failure
C
C
C********************************************************************
      IMPLICIT NONE
      DOUBLE PRECISION am1 , am2 , am3 , am4 , cmass , cmass0 , 
     &                 DT_RNDM , EPS , pj , prob1 , prob2
      INTEGER i , i1 , i2 , iccor , ictot , ifl1 , ifl2 , ii , ip1 , 
     &        ip2 , ipos , Irej , is , istro , iter , j1 , j2 , je , 
     &        jj , k
      INTEGER k1 , k2 , kk1 , kk2 , ks , niter , nrpom
      SAVE 
 
      PARAMETER (EPS=1.D-10)
 
C  input/output channels
      INCLUDE 'inc/poinou'
C  event debugging information
      INCLUDE 'inc/podebg'
C  model switches and parameters
      INCLUDE 'inc/pomdls'
 
C  standard particle data interface
 
 
      INCLUDE 'inc/poevt1'
C  extension to standard particle data interface (PHOJET specific)
      INCLUDE 'inc/poevt2'
 
C  color string configurations including collapsed strings and hadrons
      INCLUDE 'inc/postrg'
 
      DIMENSION pj(4)
 
      IF ( Irej.EQ.-1 ) THEN
         ictot = 0
         iccor = 0
         RETURN
      ELSE IF ( Irej.EQ.-2 ) THEN
         IF ( LPRi.GT.4 ) WRITE (LO,'(/1X,A,2I8)')
     &         'PHO_POMCOR: total/joined strings' , ictot , iccor
         RETURN
      END IF
C
      Irej = 0
C
      niter = 100
      iter = 0
      ictot = ictot + ISTr
      IF ( ISWmdl(25).LE.0 ) RETURN
C  debug string entries
      IF ( IDEb(83).GE.25 ) CALL PHO_PRSTRG
C
C50   CONTINUE
      iter = iter + 1
      IF ( iter.GE.niter ) THEN
         Irej = 1
         IF ( IDEb(83).GE.2 ) THEN
            IF ( LPRi.GT.4 ) WRITE (LO,'(1X,A,2I5)')
     &            'PHO_POMCOR: rejection' , iter , niter
            IF ( IDEb(83).GE.10 ) CALL PHO_PREVNT(0)
         END IF
         RETURN
      END IF
C
C  check mass limits
      istro = ISTr
      DO i = 1 , istro
         IF ( NCOde(i).GE.0 ) THEN
            j1 = NPOs(1,i)
            nrpom = IPHist(2,j1)
            IF ( nrpom.LT.100 ) THEN
               cmass0 = PHEp(5,j1)
C  get masses
               IF ( NCOde(i).EQ.3 ) THEN
                  CALL PHO_MEMASS(IPAr1(i),IPAr2(i),am1,am2,am3,am4,ip1,
     &               ip2)
               ELSE IF ( (NCOde(i).EQ.4) .OR. (NCOde(i).EQ.6) ) THEN
                  CALL PHO_BAMASS(IPAr1(i),IPAr2(i),IPAr3(i),am1,am2,
     &               am3,am4,ip1,ip2)
               ELSE IF ( NCOde(i).EQ.5 ) THEN
                  CALL PHO_DQMASS(IPAr1(i),IPAr2(i),IPAr3(i),IPAr4(i),
     &               am1,am2)
                  am3 = 0.D0
                  am4 = 0.D0
                  ip1 = 0
                  ip2 = 0
               ELSE IF ( NCOde(i).EQ.7 ) THEN
                  GOTO 100
               ELSE IF ( NCOde(i).LT.0 ) THEN
                  GOTO 100
               ELSE
                  IF ( LPRi.GT.4 ) WRITE (LO,'(/,1X,A,2I5)')
     &                  'ERROR:PHO_POMCOR:STRING NO,NCODE ' , j1 , 
     &                 NCOde(i)
                  CALL PHO_ABORT
               END IF
               IF ( IDEb(83).GE.5 .AND. LPRi.GT.4 )
     &               WRITE (LO,'(1X,A,/3X,2I4,5E11.3,2I5)') 
     &           'PHO_POMCOR: STRING,POM,CHMASS,AM1,AM2,AM3,AM4,IP1,IP2'
     &           , i , nrpom , cmass0 , am1 , am2 , am3 , am4 , ip1 , 
     &           ip2
C  select masses to correct
               IF ( cmass0.LT.MAX(am2,am4) ) THEN
                  DO k = 1 , istro
                     IF ( (k.NE.i) .AND. (NCOde(k).GE.0) ) THEN
                        j2 = NPOs(1,k)
C  join quarks to gluon
                        IF ( nrpom.EQ.IPHist(2,j2) ) THEN
C  flavour check
                           ifl1 = 0
                           ifl2 = 0
                           prob1 = 0.D0
                           prob2 = 0.D0
                           kk1 = NPOs(2,i)
                           kk2 = NPOs(2,k)
                           IF ( IDHep(kk1)+IDHep(kk2).EQ.0 ) THEN
                              cmass = (PHEp(4,kk1)+PHEp(4,kk2))
     &                           **2 - (PHEp(1,kk1)+PHEp(1,kk2))
     &                           **2 - (PHEp(2,kk1)+PHEp(2,kk2))
     &                           **2 - (PHEp(2,kk1)+PHEp(2,kk2))**2
                              ifl1 = ABS(IDHep(kk1))
                              IF ( ifl1.GT.2 ) THEN
                               prob1 = 0.1D0/MAX(cmass,EPS)
                              ELSE
                               prob1 = 0.9D0/MAX(cmass,EPS)
                              END IF
                           END IF
                           kk1 = ABS(NPOs(3,i))
                           kk2 = ABS(NPOs(3,k))
                           IF ( IDHep(kk1)+IDHep(kk2).EQ.0 ) THEN
                              cmass = (PHEp(4,kk1)+PHEp(4,kk2))
     &                           **2 - (PHEp(1,kk1)+PHEp(1,kk2))
     &                           **2 - (PHEp(2,kk1)+PHEp(2,kk2))
     &                           **2 - (PHEp(2,kk1)+PHEp(2,kk2))**2
                              ifl2 = ABS(IDHep(kk1))
                              IF ( ifl2.GT.2 ) THEN
                               prob2 = 0.1D0/MAX(cmass,EPS)
                              ELSE
                               prob2 = 0.9D0/MAX(cmass,EPS)
                              END IF
                           END IF
                           IF ( ifl1+ifl2.NE.0 ) THEN
C  fusion possible
                              iccor = iccor + 1
                              IF ( (DT_RNDM(cmass)*(prob1+prob2))
     &                           .LT.prob1 ) THEN
                               jj = 2
                               je = 3
                              ELSE
                               jj = 3
                               je = 2
                              END IF
                              kk1 = ABS(NPOs(jj,i))
                              kk2 = ABS(NPOs(jj,k))
                              i1 = ABS(NPOs(je,i))
                              i2 = kk1
                              is = SIGN(1,i2-i1)
                              i2 = i2 - is
                              k1 = kk2
                              k2 = ABS(NPOs(je,k))
                              ks = SIGN(1,k2-k1)
                              k1 = k1 + ks
                              ip1 = NHEp + 1
C  copy mother partons of string I
                              DO ii = i1 , i2 , is
                               CALL PHO_REGPAR(-1,IDHep(ii),0,j1,j2,
     &                            PHEp(1,ii),PHEp(2,ii),PHEp(3,ii),
     &                            PHEp(4,ii),i,IPHist(2,ii),ICOlor(1,ii)
     &                            ,ICOlor(2,ii),ipos,1)
                              END DO
C  register gluon
                              DO ii = 1 , 4
                               pj(ii) = PHEp(ii,kk1) + PHEp(ii,kk2)
                              END DO
                              CALL PHO_REGPAR(-1,21,0,j1,j2,pj(1),pj(2),
     &                           pj(3),pj(4),i,IPHist(2,kk2),
     &                           ICOlor(1,kk1),ICOlor(1,kk2),ipos,1)
C  copy mother partons of string K
                              DO ii = k1 , k2 , ks
                               CALL PHO_REGPAR(-1,IDHep(ii),0,j1,j2,
     &                            PHEp(1,ii),PHEp(2,ii),PHEp(3,ii),
     &                            PHEp(4,ii),i,IPHist(2,ii),ICOlor(1,ii)
     &                            ,ICOlor(2,ii),ipos,1)
                              END DO
C  create new string entry
                              DO ii = 1 , 4
                               pj(ii) = PHEp(ii,j1) + PHEp(ii,j2)
                              END DO
                              ip2 = ipos
                              CALL PHO_REGPAR(-1,90,0,ip1,-ip2,pj(1),
     &                           pj(2),pj(3),pj(4),i,IPHist(2,j1),
     &                           ICOlor(1,j1)+ICOlor(1,j2),ICOlor(2,j1)
     &                           +ICOlor(2,j2),ipos,1)
C  delete string K in /POSTRG/
                              NCOde(k) = -999
C  update string I in /POSTRG/
                              NPOs(1,i) = ipos
                              NPOs(2,i) = ip1
                              NPOs(3,i) = -ip2
C  calculate new CPC string codes
                              CALL PHO_ID2STR(IDHep(ip1),IDHep(ip2),
     &                           NCOde(i),IPAr1(i),IPAr2(i),IPAr3(i),
     &                           IPAr4(i))
                           END IF
                           GOTO 100
                        END IF
                     END IF
                  END DO
               END IF
            END IF
         END IF
 100  END DO
      IF ( IDEb(83).GE.20 ) THEN
         IF ( LPRi.GT.4 ) WRITE (LO,'(1X,A)')
     &         'PHO_POMCOR: after string recombination'
         IF ( IDEb(83).GE.22 ) THEN
            CALL PHO_PRSTRG
            CALL PHO_PREVNT(0)
         END IF
      END IF
 
      END SUBROUTINE
