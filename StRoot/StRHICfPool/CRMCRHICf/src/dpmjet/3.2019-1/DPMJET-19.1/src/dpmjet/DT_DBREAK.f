
      SUBROUTINE DT_DBREAK(Mode)
 
C***********************************************************************
C This is the steering subroutine for the different diquark breaking   *
C mechanisms.                                                          *
C                                                                      *
C MODE = 1  breaking of projectile diquark in qq-q chain using         *
C           a sea quark (q-qq chain) of the same projectile            *
C      = 2  breaking of target     diquark in q-qq chain using         *
C           a sea quark (qq-q chain) of the same target                *
C      = 3  breaking of projectile diquark in qq-q chain using         *
C           a sea quark (q-aq chain) of the same projectile            *
C      = 4  breaking of target     diquark in q-qq chain using         *
C           a sea quark (aq-q chain) of the same target                *
C      = 5  breaking of projectile anti-diquark in aqaq-aq chain using *
C           a sea anti-quark (aq-aqaq chain) of the same projectile    *
C      = 6  breaking of target     anti-diquark in aq-aqaq chain using *
C           a sea anti-quark (aqaq-aq chain) of the same target        *
C      = 7  breaking of projectile anti-diquark in aqaq-aq chain using *
C           a sea anti-quark (aq-q chain) of the same projectile       *
C      = 8  breaking of target     anti-diquark in aq-aqaq chain using *
C           a sea anti-quark (q-aq chain) of the same target           *
C                                                                      *
C Original version by J. Ranft.                                        *
C This version dated 17.5.00  is written by S. Roesler.                *
C***********************************************************************
 
      IMPLICIT NONE
      DOUBLE PRECISION acc , chklev , dum1 , dum2 , dum3 , pe , px , 
     &                 py , pz , rej
      INTEGER i , iacc , ianc , iances , idchn1 , idchn2 , idum1 , 
     &        idum2 , idx1 , idx2 , idxmo1 , idxmo2 , igcoun , ip1 , 
     &        ip11 , ip12 , ip2 , ip21 , ip22 , ipq
      INTEGER irej , is1p , is1t , is2p , is2t , isp1p , isp1t , isp2p , 
     &        isp2t , it1 , it11 , it12 , it2 , it21 , it22 , j , k , 
     &        Mode
      SAVE 
 
      INCLUDE 'inc/dtflka'
 
C event history
 
 
      INCLUDE 'inc/dtevt1'
C extended event history
      INCLUDE 'inc/dtevt2'
C flags for input different options
      INCLUDE 'inc/dtflg1'
C pointer to chains in hkkevt common (used by qq-breaking mechanisms)
      INCLUDE 'inc/dtixch'
C diquark-breaking mechanism
      INCLUDE 'inc/dtdiqb'
C flags for particle decays
      INCLUDE 'inc/dtfrpa'
 
C
C chain identifiers
C ( 1 = q-aq,   2 = aq-q,   3 = q-qq,   4 = qq-q,
C   5 = aq-adq, 6 = adq-aq, 7 = dq-adq, 8 = adq-dq )
      DIMENSION idchn1(8) , idchn2(8)
      DATA idchn1/4 , 3 , 4 , 3 , 6 , 5 , 6 , 5/
      DATA idchn2/3 , 4 , 1 , 2 , 5 , 6 , 2 , 1/
C
C parton identifiers
C ( +-21/22 = valence, +-31/32 = Glauber-sea, +-41/42 = Pomeron (diff),
C   +-51/52 = unitarity-sea, +-61/62 = gluons )
      DIMENSION isp1p(8,3) , isp1t(8,3) , isp2p(8,3) , isp2t(8,3)
      DATA isp1p/21 , 21 , 21 , 21 , 21 , 21 , 21 , 21 , 31 , 31 , 31 , 
     &     31 , 31 , 31 , 31 , 31 , 41 , 41 , 41 , 41 , 51 , 51 , 51 , 
     &     51/
      DATA isp1t/22 , 22 , 22 , 22 , 22 , 22 , 22 , 22 , 32 , 32 , 32 , 
     &     32 , 32 , 32 , 32 , 32 , 42 , 42 , 42 , 42 , 52 , 52 , 52 , 
     &     52/
      DATA isp2p/31 , 21 , 31 , 31 , 21 , 21 , 21 , 21 , 51 , 31 , 41 , 
     &     41 , 31 , 31 , 31 , 31 , 0 , 41 , 51 , 51 , 51 , 51 , 51 , 
     &     51/
      DATA isp2t/22 , 32 , 32 , 32 , 22 , 22 , 22 , 22 , 32 , 52 , 42 , 
     &     42 , 32 , 32 , 32 , 32 , 42 , 0 , 52 , 52 , 52 , 52 , 52 , 
     &     52/
 
      IF ( NCHain.LE.0 ) RETURN
      DO i = 1 , NCHain
         idx1 = IDXchn(1,i)
         is1p = ABS(ISThkk(JMOhkk(1,idx1)))
         is1t = ABS(ISThkk(JMOhkk(2,idx1)))
         IF ( (IDXchn(2,i).EQ.idchn1(Mode)) .AND. 
     &        ((is1p.EQ.isp1p(Mode,1)) .OR. (is1p.EQ.isp1p(Mode,2)) .OR. 
     &        (is1p.EQ.isp1p(Mode,3))) .AND. 
     &        ((is1t.EQ.isp1t(Mode,1)) .OR. (is1t.EQ.isp1t(Mode,2)) .OR. 
     &        (is1t.EQ.isp1t(Mode,3))) ) THEN
            DO j = 1 , NCHain
               idx2 = IDXchn(1,j)
               is2p = ABS(ISThkk(JMOhkk(1,idx2)))
               is2t = ABS(ISThkk(JMOhkk(2,idx2)))
               IF ( (IDXchn(2,j).EQ.idchn2(Mode)) .AND. 
     &              ((is2p.EQ.isp2p(Mode,1)) .OR. 
     &              (is2p.EQ.isp2p(Mode,2)) .OR. (is2p.EQ.isp2p(Mode,3))
     &              ) .AND. 
     &              ((is2t.EQ.isp2t(Mode,1)) .OR. (is2t.EQ.isp2t(Mode,2)
     &              ) .OR. (is2t.EQ.isp2t(Mode,3))) ) THEN
C   find mother nucleons of the diquark to be splitted and of the
C   sea-quark and reject this combination if it is not the same
                  IF ( (Mode.EQ.1) .OR. (Mode.EQ.3) .OR. (Mode.EQ.5)
     &                 .OR. (Mode.EQ.7) ) THEN
                     iances = 1
                  ELSE
                     iances = 2
                  END IF
                  idxmo1 = JMOhkk(iances,idx1)
 5                IF ( (JMOhkk(1,idxmo1).NE.0) .AND. 
     &                 (JMOhkk(2,idxmo1).NE.0) ) THEN
                     ianc = iances
                  ELSE
                     ianc = 1
                  END IF
                  IF ( JMOhkk(ianc,idxmo1).NE.0 ) THEN
                     idxmo1 = JMOhkk(ianc,idxmo1)
                     GOTO 5
                  END IF
                  idxmo2 = JMOhkk(iances,idx2)
 10               IF ( (JMOhkk(1,idxmo2).NE.0) .AND. 
     &                 (JMOhkk(2,idxmo2).NE.0) ) THEN
                     ianc = iances
                  ELSE
                     ianc = 1
                  END IF
                  IF ( JMOhkk(ianc,idxmo2).NE.0 ) THEN
                     idxmo2 = JMOhkk(ianc,idxmo2)
                     GOTO 10
                  END IF
                  IF ( idxmo1.EQ.idxmo2 ) THEN
C   quark content of projectile parton
                     ip1 = IDHkk(JMOhkk(1,idx1))
                     ip11 = ip1/1000
                     ip12 = (ip1-1000*ip11)/100
                     ip2 = IDHkk(JMOhkk(2,idx1))
                     ip21 = ip2/1000
                     ip22 = (ip2-1000*ip21)/100
C   quark content of target parton
                     it1 = IDHkk(JMOhkk(1,idx2))
                     it11 = it1/1000
                     it12 = (it1-1000*it11)/100
                     it2 = IDHkk(JMOhkk(2,idx2))
                     it21 = it2/1000
                     it22 = (it2-1000*it21)/100
C   split diquark and form new chains
                     IF ( Mode.EQ.1 ) THEN
                        IF ( it1.EQ.4 ) GOTO 20
                        CALL MGSQBS1(idx1,JMOhkk(1,idx1),JMOhkk(2,idx1),
     &                     idx2,JMOhkk(1,idx2),JMOhkk(2,idx2),irej,ip11,
     &                     ip12,ip2,it1,it21,it22,1,ipq,igcoun)
                     ELSE IF ( Mode.EQ.2 ) THEN
                        IF ( it2.EQ.4 ) GOTO 20
                        CALL MGSQBS2(idx1,JMOhkk(1,idx1),JMOhkk(2,idx1),
     &                     idx2,JMOhkk(1,idx2),JMOhkk(2,idx2),irej,ip1,
     &                     ip21,ip22,it11,it12,it2,1,ipq,igcoun)
                     ELSE IF ( Mode.EQ.3 ) THEN
                        IF ( it1.EQ.4 ) GOTO 20
                        CALL MUSQBS1(idx1,JMOhkk(1,idx1),JMOhkk(2,idx1),
     &                     idx2,JMOhkk(1,idx2),JMOhkk(2,idx2),irej,ip11,
     &                     ip12,ip2,it1,it2,1,ipq,igcoun)
                     ELSE IF ( Mode.EQ.4 ) THEN
                        IF ( it2.EQ.4 ) GOTO 20
                        CALL MUSQBS2(idx1,JMOhkk(1,idx1),JMOhkk(2,idx1),
     &                     idx2,JMOhkk(1,idx2),JMOhkk(2,idx2),irej,ip1,
     &                     ip21,ip22,it1,it2,1,ipq,igcoun)
                     ELSE IF ( Mode.EQ.5 ) THEN
                        CALL MGSQBS1(idx1,JMOhkk(1,idx1),JMOhkk(2,idx1),
     &                     idx2,JMOhkk(1,idx2),JMOhkk(2,idx2),irej,ip11,
     &                     ip12,ip2,it1,it21,it22,2,ipq,igcoun)
                     ELSE IF ( Mode.EQ.6 ) THEN
                        CALL MGSQBS2(idx1,JMOhkk(1,idx1),JMOhkk(2,idx1),
     &                     idx2,JMOhkk(1,idx2),JMOhkk(2,idx2),irej,ip1,
     &                     ip21,ip22,it11,it12,it2,2,ipq,igcoun)
                     ELSE IF ( Mode.EQ.7 ) THEN
                        CALL MUSQBS1(idx1,JMOhkk(1,idx1),JMOhkk(2,idx1),
     &                     idx2,JMOhkk(1,idx2),JMOhkk(2,idx2),irej,ip11,
     &                     ip12,ip2,it1,it2,2,ipq,igcoun)
                     ELSE IF ( Mode.EQ.8 ) THEN
                        CALL MUSQBS2(idx1,JMOhkk(1,idx1),JMOhkk(2,idx1),
     &                     idx2,JMOhkk(1,idx2),JMOhkk(2,idx2),irej,ip1,
     &                     ip21,ip22,it1,it2,2,ipq,igcoun)
                     END IF
                     IF ( irej.GE.1 ) THEN
 
                        IF ( (ipq.LT.0) .OR. (ipq.GE.4) .AND. 
     &                       LPRi.GT.4 ) WRITE (LOUt,*) 'ipq !!!' , 
     &                       ipq , Mode
                        DBRkr(ipq,Mode) = DBRkr(ipq,Mode) + 1.0D0
C   accept or reject new chains corresponding to PDBSEA
                     ELSE
                        IF ( (ipq.EQ.1) .OR. (ipq.EQ.2) ) THEN
                           acc = DBRka(1,Mode) + DBRka(2,Mode)
                           rej = DBRkr(1,Mode) + DBRkr(2,Mode)
                        ELSE IF ( ipq.EQ.3 ) THEN
                           acc = DBRka(3,Mode)
                           rej = DBRkr(3,Mode)
                        ELSE
 
                           IF ( LPRi.GT.4 ) WRITE (LOUt,*)
     &                         ' inconsistent IPQ ! ' , ipq
                           STOP
                        END IF
                        IF ( acc/(acc+rej).LE.PDBsea(ipq) ) THEN
                           DBRka(ipq,Mode) = DBRka(ipq,Mode) + 1.0D0
                           iacc = 1
                        ELSE
                           DBRkr(ipq,Mode) = DBRkr(ipq,Mode) + 1.0D0
                           iacc = 0
                        END IF
C   new chains have been accepted and are now copied into HKKEVT
                        IF ( iacc.EQ.1 ) THEN
                           IF ( LEMcck ) THEN
                              CALL DT_EVTEMC(PHKk(1,idx1),PHKk(2,idx1),
     &                           PHKk(3,idx1),PHKk(4,idx1),1,idum1,
     &                           idum2)
                              CALL DT_EVTEMC(PHKk(1,idx2),PHKk(2,idx2),
     &                           PHKk(3,idx2),PHKk(4,idx2),2,idum1,
     &                           idum2)
                           END IF
                           IDHkk(idx1) = 99888
                           IDHkk(idx2) = 99888
                           IDXchn(2,i) = -1
                           IDXchn(2,j) = -1
                           DO k = 1 , igcoun
                              NHKk = NHKk + 1
                              CALL HKKHKT(NHKk,k)
                              IF ( (LEMcck) .AND. (IDHkk(NHKk).EQ.88888)
     &                           ) THEN
                               px = -PHKk(1,NHKk)
                               py = -PHKk(2,NHKk)
                               pz = -PHKk(3,NHKk)
                               pe = -PHKk(4,NHKk)
                               CALL DT_EVTEMC(px,py,pz,pe,2,idum1,idum2)
                              END IF
                           END DO
                           IF ( LEMcck ) THEN
                              chklev = 0.1D0
                              CALL DT_EVTEMC(dum1,dum2,dum3,chklev,-1,
     &                           9000,irej)
                              IF ( irej.NE.0 ) CALL DT_EVTOUT(4)
                           END IF
                           GOTO 100
                        END IF
                     END IF
                  END IF
               END IF
 20         END DO
         END IF
 100  END DO
      END SUBROUTINE
