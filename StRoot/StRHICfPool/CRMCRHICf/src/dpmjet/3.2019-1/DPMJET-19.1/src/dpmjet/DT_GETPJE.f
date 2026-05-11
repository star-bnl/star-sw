
      SUBROUTINE DT_GETPJE(Mo1,Mo2,Pp,Pt,Mode,Ipje,Irej)
 
C***********************************************************************
C This subroutine copies PHOJET partons and strings from POEVT1 into   *
C DTEVT1.                                                              *
C      MO1,MO2   indices of first and last mother-parton in DTEVT1     *
C      PP,PT     4-momenta of projectile/target being handled by       *
C                PHOJET                                                *
C This version dated 11.12.99 is written by S. Roesler                 *
C***********************************************************************
 
      IMPLICIT NONE
      DOUBLE PRECISION chklev , dum1 , dum2 , dum3 , OHALF , ONE , pe , 
     &                 Pp , ppe , ppx , ppy , ppz , Pt , ptotmp , px , 
     &                 py , pz , r1 , r2
      DOUBLE PRECISION TINY1 , TINY10 , ZERO
      INTEGER i , idchsy , idstg , idum1 , idum2 , idx , idx1 , idx2 , 
     &        idxms , idxms1 , idxms2 , idxstg , iesss1 , iesss2 , 
     &        iesss4 , iesss5
      INTEGER iesss6 , iesss7 , iesss8 , iesss9 , igluon , iloop , imo , 
     &        inhkk , Ipje , ipom1 , ipom2 , Irej , irej2 , isptn , 
     &        isptn1 , isptn2 , istat
      INTEGER istx , jstrg , kk , m1ptn , m1ptn1 , m1ptn2 , m1strg , 
     &        m2ptn , m2ptn1 , m2ptn2 , m2strg
      INTEGER maxlop , Mo1 , Mo2 , Mode , ngluon , npval , ntval
      SAVE 
 
      INCLUDE 'inc/dtflka'
 
      PARAMETER (TINY10=1.0D-10,TINY1=1.0D-1,ZERO=0.0D0,ONE=1.0D0,
     &           OHALF=0.5D0)
 
      LOGICAL lflip
 
C event history
 
 
      INCLUDE 'inc/dtevt1'
C extended event history
      INCLUDE 'inc/dtevt2'
C Lorentz-parameters of the current interaction
      INCLUDE 'inc/dtltra'
C DTUNUC-PHOJET interface, Lorentz-param. of n-n subsystem
      INCLUDE 'inc/dtltsu'
C flags for input different options
      INCLUDE 'inc/dtflg1'
C statistics: double-Pomeron exchange
      INCLUDE 'inc/dtflg2'
C statistics
      INCLUDE 'inc/dtsta1'
C rejection counter
      INCLUDE 'inc/dtrejc'
 
C  standard particle data interface
 
 
      INCLUDE 'inc/poevt1'
C  extension to standard particle data interface (PHOJET specific)
      INCLUDE 'inc/poevt2'
 
C  color string configurations including collapsed strings and hadrons
      INCLUDE 'inc/postrg'
C  general process information
      INCLUDE 'inc/poprcs'
C  model switches and parameters
      INCLUDE 'inc/pomdls'
C  event debugging information
      INCLUDE 'inc/podebg'
 
 
      DIMENSION Pp(4) , Pt(4)
      DATA maxlop/10000/
      DATA iesss1 , iesss2 , iesss4 , iesss5 , iesss6 , iesss7 , 
     &     iesss8 , iesss9/0 , 0 , 0 , 0 , 0 , 0 , 0 , 0/
 
      inhkk = NHKk
      lflip = .TRUE.
 100  npval = 0
      ntval = 0
      Irej = 0
 
C   store initial momenta for energy-momentum conservation check
      IF ( LEMcck ) THEN
         CALL DT_EVTEMC(Pp(1),Pp(2),Pp(3),Pp(4),1,idum1,idum2)
         CALL DT_EVTEMC(Pt(1),Pt(2),Pt(3),Pt(4),2,idum1,idum2)
      END IF
C copy partons and strings from POEVT1 into DTEVT1
      DO i = 1 , ISTr
C        IF ((NCODE(I).EQ.-99).AND.(IPAMDL(17).EQ.0)) THEN
         IF ( NCOde(i).EQ.-99 ) THEN
            idxstg = NPOs(1,i)
            idstg = IDHep(idxstg)
            px = PHEp(1,idxstg)
            py = PHEp(2,idxstg)
            pz = PHEp(3,idxstg)
            pe = PHEp(4,idxstg)
            IF ( Mode.LT.0 ) THEN
               istat = 70000 + Ipje
               CALL DT_EVTPUT(2,istat,Mo1,Mo2,px,py,pz,pe,11,idstg,0)
               IF ( LEMcck ) THEN
                  px = -px
                  py = -py
                  pz = -pz
                  pe = -pe
                  CALL DT_EVTEMC(px,py,pz,pe,2,idum1,idum2)
               END IF
            ELSE
               CALL DT_DALTRA(GAM,BGX,BGY,BGZ,px,py,pz,pe,ptotmp,ppx,
     &                        ppy,ppz,ppe)
               istat = 70000 + Ipje
               CALL DT_EVTPUT(2,istat,Mo1,Mo2,ppx,ppy,ppz,ppe,11,idstg,
     &                        0)
               IF ( LEMcck ) THEN
                  px = -ppx
                  py = -ppy
                  pz = -ppz
                  pe = -ppe
                  CALL DT_EVTEMC(px,py,pz,pe,2,idum1,idum2)
               END IF
            END IF
            NOBam(NHKk) = 0
            IHIst(1,NHKk) = IPHist(1,idxstg)
            IHIst(2,NHKk) = 0
         ELSE IF ( NCOde(i).GE.0 ) THEN
C   indices of partons and string in POEVT1
            idx1 = ABS(JMOhep(1,NPOs(1,i)))
            idx2 = ABS(JMOhep(2,NPOs(1,i)))
            IF ( (idx1.GT.idx2) .OR. (JMOhep(2,NPOs(1,i)).GT.0) ) THEN
 
               IF ( LPRi.GT.4 ) WRITE (LOUt,*)
     &              ' GETPJE: IDX1.GT.IDX2 ' , idx1 , idx2 , 
     &              ' or JMOHEP(2,NPOS(1,I)).GT.0 ' , 
     &              JMOhep(2,NPOs(1,i)) , ' ! '
               STOP ' GETPJE 1'
            END IF
            idxstg = NPOs(1,i)
C   find "mother" string of the string
            idxms1 = ABS(JMOhep(1,idx1))
            idxms2 = ABS(JMOhep(1,idx2))
            IF ( idxms1.NE.idxms2 ) THEN
               idxms1 = idxstg
               idxms2 = idxstg
C              STOP ' GETPJE: IDXMS1.NE.IDXMS2 !'
            END IF
C   search POEVT1 for the original hadron of the parton
            iloop = 0
            ipom1 = 0
 120        iloop = iloop + 1
 
            IF ( IDHep(idxms1).EQ.990 ) ipom1 = 1
 
            idxms1 = ABS(JMOhep(1,idxms1))
            IF ( (idxms1.NE.1) .AND. (idxms1.NE.2) .AND. 
     &           (iloop.LT.maxlop) ) GOTO 120
 
            IF ( LPRi.GT.4 .AND. iloop.EQ.maxlop ) WRITE (LOUt,*)
     &            ' GETPJE: MAXLOP in 1 ! '
            ipom2 = 0
            iloop = 0
 140        iloop = iloop + 1
 
            IF ( IDHep(idxms2).EQ.990 ) ipom2 = 1
 
            IF ( (iloop.EQ.1) .OR. (IDHep(idxms2).GE.7777) ) THEN
               idxms2 = ABS(JMOhep(2,idxms2))
            ELSE
               idxms2 = ABS(JMOhep(1,idxms2))
            END IF
            IF ( (idxms2.NE.1) .AND. (idxms2.NE.2) .AND. 
     &           (iloop.LT.maxlop) ) GOTO 140
 
            IF ( LPRi.GT.4 .AND. iloop.EQ.maxlop ) WRITE (LOUt,*)
     &            ' GETPJE: MAXLOP in 5 ! '
C   parton 1
            IF ( idxms1.EQ.1 ) THEN
               isptn1 = ISThkk(Mo1)
               m1ptn1 = Mo1
               m2ptn1 = Mo1 + 2
            ELSE
               isptn1 = ISThkk(Mo2)
               m1ptn1 = Mo2 - 2
               m2ptn1 = Mo2
            END IF
C   parton 2
            IF ( idxms2.EQ.1 ) THEN
               isptn2 = ISThkk(Mo1)
               m1ptn2 = Mo1
               m2ptn2 = Mo1 + 2
            ELSE
               isptn2 = ISThkk(Mo2)
               m1ptn2 = Mo2 - 2
               m2ptn2 = Mo2
            END IF
C   check for mis-identified mothers and switch mother indices if necessary
            IF ( (idxms1.EQ.idxms2) .AND. (IPRoce.NE.5) .AND. 
     &           (IPRoce.NE.6) .AND. 
     &           ((IDHep(idx1).NE.21) .OR. (IDHep(idx2).NE.21)) .AND. 
     &           (lflip) ) THEN
               IF ( PHEp(3,idx1).GT.PHEp(3,idx2) ) THEN
                  isptn1 = ISThkk(Mo1)
                  m1ptn1 = Mo1
                  m2ptn1 = Mo1 + 2
                  isptn2 = ISThkk(Mo2)
                  m1ptn2 = Mo2 - 2
                  m2ptn2 = Mo2
               ELSE
                  isptn1 = ISThkk(Mo2)
                  m1ptn1 = Mo2 - 2
                  m2ptn1 = Mo2
                  isptn2 = ISThkk(Mo1)
                  m1ptn2 = Mo1
                  m2ptn2 = Mo1 + 2
               END IF
            END IF
C   register partons in temporary common
C     parton at chain end
            px = PHEp(1,idx1)
            py = PHEp(2,idx1)
            pz = PHEp(3,idx1)
            pe = PHEp(4,idx1)
C flag only partons coming from Pomeron with 41/42
C           IF ((IPOM1.NE.0).OR.(NPOS(4,I).GE.4)) THEN
            IF ( ipom1.NE.0 ) THEN
               istx = ABS(isptn1)/10
               imo = ABS(isptn1) - 10*istx
               isptn1 = -(40+imo)
            ELSE IF ( (ICOlor(2,idx1).EQ.0) .OR. (IDHep(idx1).EQ.21) )
     &                THEN
               istx = ABS(isptn1)/10
               imo = ABS(isptn1) - 10*istx
               IF ( (IDHep(idx1).EQ.21) .OR. 
     &              (ABS(IPHist(1,idx1)).GE.100) ) THEN
                  isptn1 = -(60+imo)
               ELSE
                  isptn1 = -(50+imo)
               END IF
            END IF
            IF ( isptn1.EQ.-21 ) npval = npval + 1
            IF ( isptn1.EQ.-22 ) ntval = ntval + 1
            IF ( Mode.LT.0 ) THEN
               CALL DT_EVTPUT(isptn1,IDHep(idx1),m1ptn1,m2ptn1,px,py,pz,
     &                        pe,0,0,0)
            ELSE
               CALL DT_DALTRA(GAM,BGX,BGY,BGZ,px,py,pz,pe,ptotmp,ppx,
     &                        ppy,ppz,ppe)
               CALL DT_EVTPUT(isptn1,IDHep(idx1),m1ptn1,m2ptn1,ppx,ppy,
     &                        ppz,ppe,0,0,0)
            END IF
            IHIst(1,NHKk) = IPHist(1,idx1)
            IHIst(2,NHKk) = 0
            DO kk = 1 , 4
C                          J.R.19.11.01
               CALL DT_RANNOR(r1,r2)
               VHKk(kk,NHKk) = VHKk(kk,m2ptn1) + r1*0.8D-12
               WHKk(kk,NHKk) = WHKk(kk,m1ptn1)
            END DO
            VHKk(4,NHKk) = VHKk(3,m2ptn1)/BLAb - VHKk(3,m1ptn1)/BGLab
            WHKk(4,NHKk) = -WHKk(3,m1ptn1)/BLAb + WHKk(3,m2ptn1)/BGLab
            m1strg = NHKk
C     gluon kinks
            ngluon = idx2 - idx1 - 1
            IF ( ngluon.GT.0 ) THEN
               DO igluon = 1 , ngluon
                  idx = idx1 + igluon
                  idxms = ABS(JMOhep(1,idx))
                  IF ( (idxms.NE.1) .AND. (idxms.NE.2) ) THEN
                     iloop = 0
 142                 iloop = iloop + 1
                     idxms = ABS(JMOhep(1,idxms))
                     IF ( (idxms.NE.1) .AND. (idxms.NE.2) .AND. 
     &                    (iloop.LT.maxlop) ) GOTO 142
 
                     IF ( iloop.EQ.maxlop .AND. LPRi.GT.4 )
     &                    WRITE (LOUt,*) ' GETPJE: MAXLOP in 3 ! '
                  END IF
                  IF ( idxms.EQ.1 ) THEN
                     isptn = ISThkk(Mo1)
                     m1ptn = Mo1
                     m2ptn = Mo1 + 2
                  ELSE
                     isptn = ISThkk(Mo2)
                     m1ptn = Mo2 - 2
                     m2ptn = Mo2
                  END IF
                  px = PHEp(1,idx)
                  py = PHEp(2,idx)
                  pz = PHEp(3,idx)
                  pe = PHEp(4,idx)
                  IF ( (ICOlor(2,idx).EQ.0) .OR. (IDHep(idx).EQ.21) )
     &                 THEN
                     istx = ABS(isptn)/10
                     imo = ABS(isptn) - 10*istx
                     IF ( (IDHep(idx).EQ.21) .OR. 
     &                    (ABS(IPHist(1,idx)).GE.100) ) THEN
                        isptn = -(60+imo)
                     ELSE
                        isptn = -(50+imo)
                     END IF
                  END IF
                  IF ( isptn.EQ.-21 ) npval = npval + 1
                  IF ( isptn.EQ.-22 ) ntval = ntval + 1
                  IF ( Mode.LT.0 ) THEN
                     CALL DT_EVTPUT(isptn,IDHep(idx),m1ptn,m2ptn,px,py,
     &                  pz,pe,0,0,0)
                  ELSE
                     CALL DT_DALTRA(GAM,BGX,BGY,BGZ,px,py,pz,pe,ptotmp,
     &                  ppx,ppy,ppz,ppe)
                     CALL DT_EVTPUT(isptn,IDHep(idx),m1ptn,m2ptn,ppx,
     &                  ppy,ppz,ppe,0,0,0)
                  END IF
                  IHIst(1,NHKk) = IPHist(1,idx)
                  IHIst(2,NHKk) = 0
                  DO kk = 1 , 4
C                          J.R.19.11.01
                     CALL DT_RANNOR(r1,r2)
                     VHKk(kk,NHKk) = VHKk(kk,m2ptn) + r1*0.8D-12
                     WHKk(kk,NHKk) = WHKk(kk,m1ptn)
                  END DO
                  VHKk(4,NHKk) = VHKk(3,m2ptn)/BLAb - VHKk(3,m1ptn)
     &               /BGLab
                  WHKk(4,NHKk) = -WHKk(3,m1ptn)/BLAb + WHKk(3,m2ptn)
     &               /BGLab
               END DO
            END IF
C     parton at chain end
            px = PHEp(1,idx2)
            py = PHEp(2,idx2)
            pz = PHEp(3,idx2)
            pe = PHEp(4,idx2)
C flag only partons coming from Pomeron with 41/42
C           IF ((IPOM2.NE.0).OR.(NPOS(4,I).GE.4)) THEN
            IF ( ipom2.NE.0 ) THEN
               istx = ABS(isptn2)/10
               imo = ABS(isptn2) - 10*istx
               isptn2 = -(40+imo)
            ELSE IF ( (ICOlor(2,idx2).EQ.0) .OR. (IDHep(idx2).EQ.21) )
     &                THEN
               istx = ABS(isptn2)/10
               imo = ABS(isptn2) - 10*istx
               IF ( (IDHep(idx2).EQ.21) .OR. 
     &              (ABS(IPHist(1,idx2)).GE.100) ) THEN
                  isptn2 = -(60+imo)
               ELSE
                  isptn2 = -(50+imo)
               END IF
            END IF
            IF ( isptn2.EQ.-21 ) npval = npval + 1
            IF ( isptn2.EQ.-22 ) ntval = ntval + 1
            IF ( Mode.LT.0 ) THEN
               CALL DT_EVTPUT(isptn2,IDHep(idx2),m1ptn2,m2ptn2,px,py,pz,
     &                        pe,0,0,0)
            ELSE
               CALL DT_DALTRA(GAM,BGX,BGY,BGZ,px,py,pz,pe,ptotmp,ppx,
     &                        ppy,ppz,ppe)
               CALL DT_EVTPUT(isptn2,IDHep(idx2),m1ptn2,m2ptn2,ppx,ppy,
     &                        ppz,ppe,0,0,0)
            END IF
            IHIst(1,NHKk) = IPHist(1,idx2)
            IHIst(2,NHKk) = 0
            DO kk = 1 , 4
C                          J.R.19.11.01
               CALL DT_RANNOR(r1,r2)
               VHKk(kk,NHKk) = VHKk(kk,m2ptn2) + r1*0.8D-12
               WHKk(kk,NHKk) = WHKk(kk,m1ptn2)
            END DO
            VHKk(4,NHKk) = VHKk(3,m2ptn2)/BLAb - VHKk(3,m1ptn2)/BGLab
            WHKk(4,NHKk) = -WHKk(3,m1ptn2)/BLAb + WHKk(3,m2ptn2)/BGLab
            m2strg = NHKk
C   register string
            jstrg = 100*IPRoce + NCOde(i)
            px = PHEp(1,idxstg)
            py = PHEp(2,idxstg)
            pz = PHEp(3,idxstg)
            pe = PHEp(4,idxstg)
            IF ( Mode.LT.0 ) THEN
               istat = 70000 + Ipje
               CALL DT_EVTPUT(jstrg,istat,m1strg,m2strg,px,py,pz,pe,0,0,
     &                        0)
               IF ( LEMcck ) THEN
                  px = -px
                  py = -py
                  pz = -pz
                  pe = -pe
                  CALL DT_EVTEMC(px,py,pz,pe,2,idum1,idum2)
               END IF
            ELSE
               CALL DT_DALTRA(GAM,BGX,BGY,BGZ,px,py,pz,pe,ptotmp,ppx,
     &                        ppy,ppz,ppe)
               istat = 70000 + Ipje
               CALL DT_EVTPUT(jstrg,istat,m1strg,m2strg,ppx,ppy,ppz,ppe,
     &                        0,0,0)
               IF ( LEMcck ) THEN
                  px = -ppx
                  py = -ppy
                  pz = -ppz
                  pe = -ppe
                  CALL DT_EVTEMC(px,py,pz,pe,2,idum1,idum2)
               END IF
            END IF
            NOBam(NHKk) = 0
            IHIst(1,NHKk) = 0
            IHIst(2,NHKk) = 0
            DO kk = 1 , 4
C                          J.R.19.11.01
               CALL DT_RANNOR(r1,r2)
               VHKk(kk,NHKk) = VHKk(kk,Mo2) + r1*0.8D-12
               WHKk(kk,NHKk) = WHKk(kk,Mo1)
            END DO
            VHKk(4,NHKk) = VHKk(3,Mo2)/BLAb - VHKk(3,Mo1)/BGLab
            WHKk(4,NHKk) = -WHKk(3,Mo1)/BLAb + WHKk(3,Mo2)/BGLab
         END IF
      END DO
 
      IF ( ((npval.GT.2) .OR. (ntval.GT.2)) .AND. (lflip) ) THEN
         NHKk = inhkk
         lflip = .FALSE.
         GOTO 100
      END IF
 
      IF ( LEMcck ) THEN
         IF ( UMO.GT.1.0D5 ) THEN
            chklev = 1.0D0
         ELSE
            chklev = TINY1
         END IF
         CALL DT_EVTEMC(dum1,dum2,dum3,chklev,-1,1000,irej2)
 
         IF ( irej2.GT.ZERO ) CALL PHO_PREVNT(0)
 
      END IF
 
C internal statistics
C   dble-Po statistics.
      IF ( IPRoce.NE.4 ) IPOpo = 0
 
      INTflg = IPRoce
      idchsy = IDCh(Mo1)
      IF ( (IPRoce.GE.1) .AND. (IPRoce.LE.8) ) THEN
         ICEvtg(idchsy,IPRoce+2) = ICEvtg(idchsy,IPRoce+2) + 1
      ELSE
 
         IF ( LPRi.GT.4 ) WRITE (LOUt,99010) IPRoce , NEVhkk , Mo1
99010    FORMAT (1X,'GETFSP:   warning! incons. process id. (',I2,
     &           ') at evt(chain) ',I6,'(',I2,')')
      END IF
      IF ( IPRoce.EQ.5 ) THEN
         IF ( (IDIfr1.GE.1) .AND. (IDIfr1.LE.3) )
     &        ICEvtg(idchsy,18+IDIfr1) = ICEvtg(idchsy,18+IDIfr1) + 1
      ELSE IF ( IPRoce.EQ.6 ) THEN
C           WRITE(LOUT,1001) IPROCE,IDIFR1,IDIFR2
         IF ( (IDIfr2.GE.1) .AND. (IDIfr2.LE.3) )
     &        ICEvtg(idchsy,21+IDIfr2) = ICEvtg(idchsy,21+IDIfr2) + 1
      ELSE IF ( IPRoce.EQ.7 ) THEN
         IF ( (IDIfr1.GE.1) .AND. (IDIfr1.LE.3) .AND. (IDIfr2.GE.1)
     &        .AND. (IDIfr2.LE.3) ) THEN
            IF ( (IDIfr1.EQ.1) .AND. (IDIfr2.EQ.1) ) ICEvtg(idchsy,25)
     &           = ICEvtg(idchsy,25) + 1
            IF ( (IDIfr1.EQ.2) .AND. (IDIfr2.EQ.2) ) ICEvtg(idchsy,26)
     &           = ICEvtg(idchsy,26) + 1
            IF ( (IDIfr1.EQ.1) .AND. (IDIfr2.EQ.2) ) ICEvtg(idchsy,27)
     &           = ICEvtg(idchsy,27) + 1
            IF ( (IDIfr1.EQ.2) .AND. (IDIfr2.EQ.1) ) ICEvtg(idchsy,28)
     &           = ICEvtg(idchsy,28) + 1
         ELSE
 
            IF ( LPRi.GT.4 ) WRITE (LOUt,99020) IPRoce , IDIfr1 , IDIfr2
C           WRITE(LOUT,1001) IPROCE,IDIFR1,IDIFR2
99020       FORMAT (1X,'GETFSP:   warning! incons. diffrac. id. ',
     &              '(IPROCE,IDIFR1,IDIFR2=',3I3,')')
         END IF
      END IF
      IF ( (IDIfr1+IDIfr2.EQ.0) .AND. (KHDir.GE.1) .AND. (KHDir.LE.3) )
     &     THEN
         ICEvtg(idchsy,10+KHDir) = ICEvtg(idchsy,10+KHDir) + 1
         ICEvtg(idchsy,10+KHDir) = ICEvtg(idchsy,10+KHDir) + 1
         ICEvtg(idchsy,10+KHDir) = ICEvtg(idchsy,10+KHDir) + 1
      END IF
      ICEvtg(idchsy,14) = ICEvtg(idchsy,14) + KSPom
      ICEvtg(idchsy,15) = ICEvtg(idchsy,15) + KHPom
      ICEvtg(idchsy,16) = ICEvtg(idchsy,16) + KSReg
      ICEvtg(idchsy,17) = ICEvtg(idchsy,17) + (KSTrg+KHTrg)
      ICEvtg(idchsy,18) = ICEvtg(idchsy,18) + (KSLoo+KHLoo)
 
 
C9999 CONTINUE
C     IREJ = 1
C     RETURN
      END SUBROUTINE
