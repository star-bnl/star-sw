
 
C***********************************************************************
C This version dated 15.01.95 is written by S. Roesler                 *
C***********************************************************************
 
      SUBROUTINE DT_GETCSY(Ifpr1,Pp1,Mop1,Ifpr2,Pp2,Mop2,Ifta1,Pt1,Mot1,
     &                     Ifta2,Pt2,Mot2,Irej)
      IMPLICIT NONE
      DOUBLE PRECISION amch1 , amch1n , amch2 , amch2n , amdif1 , 
     &                 amdif2 , pch1 , pch2 , Pp1 , Pp2 , Pt1 , Pt2 , 
     &                 TINY10
      INTEGER i , idch1 , idch2 , idr1 , idr2 , IDT_IPDG2B , idum , 
     &        idxr1 , idxr2 , ifp1 , ifp2 , Ifpr1 , Ifpr2 , ift1 , 
     &        ift2 , Ifta1 , Ifta2 , Irej , irej1 , kch
      INTEGER Mop1 , Mop2 , Mot1 , Mot2
      SAVE 
 
      INCLUDE 'inc/dtflka'
 
      PARAMETER (TINY10=1.0D-10)
 
C event history
 
 
      INCLUDE 'inc/dtevt1'
C extended event history
      INCLUDE 'inc/dtevt2'
C rejection counter
      INCLUDE 'inc/dtrejc'
C flags for input different options
      INCLUDE 'inc/dtflg1'
C flags for diffractive interactions (DTUNUC 1.x)
      INCLUDE 'inc/dtflg3'
 
 
      DIMENSION Pp1(4) , Pp2(4) , Pt1(4) , Pt2(4) , ifp1(2) , ifp2(2) , 
     &          ift1(2) , ift2(2) , pch1(4) , pch2(4)
      Irej = 0
 
C get quark content of partons
      DO i = 1 , 2
         ifp1(i) = 0
         ifp2(i) = 0
         ift1(i) = 0
         ift2(i) = 0
      END DO
      ifp1(1) = IDT_IPDG2B(Ifpr1,1,2)
      IF ( ABS(Ifpr1).GE.1000 ) ifp1(2) = IDT_IPDG2B(Ifpr1,2,2)
      ifp2(1) = IDT_IPDG2B(Ifpr2,1,2)
      IF ( ABS(Ifpr2).GE.1000 ) ifp2(2) = IDT_IPDG2B(Ifpr2,2,2)
      ift1(1) = IDT_IPDG2B(Ifta1,1,2)
      IF ( ABS(Ifta1).GE.1000 ) ift1(2) = IDT_IPDG2B(Ifta1,2,2)
      ift2(1) = IDT_IPDG2B(Ifta2,1,2)
      IF ( ABS(Ifta2).GE.1000 ) ift2(2) = IDT_IPDG2B(Ifta2,2,2)
 
C get kind of chains (1 - q-aq, 2 - q-qq/aq-aqaq, 3 - qq-aqaq)
      idch1 = 2
      IF ( (ifp1(2).EQ.0) .AND. (ift1(2).EQ.0) ) idch1 = 1
      IF ( (ifp1(2).NE.0) .AND. (ift1(2).NE.0) ) idch1 = 3
      idch2 = 2
      IF ( (ifp2(2).EQ.0) .AND. (ift2(2).EQ.0) ) idch2 = 1
      IF ( (ifp2(2).NE.0) .AND. (ift2(2).NE.0) ) idch2 = 3
 
C store initial configuration for energy-momentum cons. check
 
C sample intrinsic p_t at chain-ends
      IF ( LEMcck ) CALL DT_EMC1(Pp1,Pp2,Pt1,Pt2,1,1,idum)
      CALL DT_GETSPT(Pp1,Ifpr1,ifp1,Pp2,Ifpr2,ifp2,Pt1,Ifta1,ift1,Pt2,
     &               Ifta2,ift2,amch1,idch1,amch2,idch2,IDCh(Mop1),
     &               irej1)
      IF ( irej1.NE.0 ) THEN
 
         IF ( LPRi.GT.4 .AND. IOUlev(1).GT.0 ) WRITE (LOUt,*)
     &         'rejected 1 in GETCSY'
         IRPt = IRPt + 1
         GOTO 100
      END IF
 
C      IF ((IRESCO.EQ.1).OR.(IFRAG(1).EQ.1)) THEN
C         IF ((IDCH1.EQ.3).OR.((IDCH1.GT.1).AND.(IDCH2.EQ.1))) THEN
C* check second chain for resonance
C            CALL DT_CH2RES(IFP2(1),IFP2(2),IFT2(1),IFT2(2),IDR2,IDXR2,
C     &                  AMCH2,AMCH2N,IDCH2,IREJ1)
C            IF (IREJ1.NE.0) GOTO 9999
C            IF (IDR2.NE.0) THEN
C               CALL DT_CHKINE(PP2,IFPR2,PP1,IFPR1,PT2,IFTA2,PT1,IFTA1,
C     &                     AMCH2,AMCH2N,AMCH1,IREJ1)
C               IF (IREJ1.NE.0) GOTO 9999
C            ENDIF
C* check first chain for resonance
C            CALL DT_CH2RES(IFP1(1),IFP1(2),IFT1(1),IFT1(2),IDR1,IDXR1,
C     &                  AMCH1,AMCH1N,IDCH1,IREJ1)
C            IF (IREJ1.NE.0) GOTO 9999
C            IF (IDR1.NE.0) IDR1 = 100*IDR1
C         ELSE
C* check first chain for resonance
C            CALL DT_CH2RES(IFP1(1),IFP1(2),IFT1(1),IFT1(2),IDR1,IDXR1,
C     &                  AMCH1,AMCH1N,IDCH1,IREJ1)
C            IF (IREJ1.NE.0) GOTO 9999
C            IF (IDR1.NE.0) THEN
C               CALL DT_CHKINE(PP1,IFPR1,PP2,IFPR2,PT1,IFTA1,PT2,IFTA2,
C     &                     AMCH1,AMCH1N,AMCH2,IREJ1)
C               IF (IREJ1.NE.0) GOTO 9999
C            ENDIF
C* check second chain for resonance
C            CALL DT_CH2RES(IFP2(1),IFP2(2),IFT2(1),IFT2(2),IDR2,IDXR2,
C     &                  AMCH2,AMCH2N,IDCH2,IREJ1)
C            IF (IREJ1.NE.0) GOTO 9999
C            IF (IDR2.NE.0) IDR2 = 100*IDR2
C         ENDIF
C      ENDIF
 
      IF ( (IREsco.EQ.1) .OR. (IFRag(1).EQ.1) ) THEN
C check chains for resonances
         CALL DT_CH2RES(ifp1(1),ifp1(2),ift1(1),ift1(2),idr1,idxr1,
     &                  amch1,amch1n,idch1,irej1)
         IF ( irej1.NE.0 ) GOTO 100
         CALL DT_CH2RES(ifp2(1),ifp2(2),ift2(1),ift2(2),idr2,idxr2,
     &                  amch2,amch2n,idch2,irej1)
         IF ( irej1.NE.0 ) GOTO 100
C change kinematics corresponding to resonance-masses
         IF ( (idr1.NE.0) .AND. (idr2.EQ.0) ) THEN
            CALL DT_CHKINE(Pp1,Ifpr1,Pp2,Ifpr2,Pt1,Ifta1,Pt2,Ifta2,
     &                     amch1,amch1n,amch2,irej1)
            IF ( irej1.GT.0 ) GOTO 100
            IF ( irej1.EQ.-1 ) idr1 = 100*idr1
            CALL DT_CH2RES(ifp2(1),ifp2(2),ift2(1),ift2(2),idr2,idxr2,
     &                     amch2,amch2n,idch2,irej1)
            IF ( irej1.NE.0 ) GOTO 100
            IF ( idr2.NE.0 ) idr2 = 100*idr2
         ELSE IF ( (idr1.EQ.0) .AND. (idr2.NE.0) ) THEN
            CALL DT_CHKINE(Pp2,Ifpr2,Pp1,Ifpr1,Pt2,Ifta2,Pt1,Ifta1,
     &                     amch2,amch2n,amch1,irej1)
            IF ( irej1.GT.0 ) GOTO 100
            IF ( irej1.EQ.-1 ) idr2 = 100*idr2
            CALL DT_CH2RES(ifp1(1),ifp1(2),ift1(1),ift1(2),idr1,idxr1,
     &                     amch1,amch1n,idch1,irej1)
            IF ( irej1.NE.0 ) GOTO 100
            IF ( idr1.NE.0 ) idr1 = 100*idr1
         ELSE IF ( (idr1.NE.0) .AND. (idr2.NE.0) ) THEN
            amdif1 = ABS(amch1-amch1n)
            amdif2 = ABS(amch2-amch2n)
            IF ( amdif2.LT.amdif1 ) THEN
               CALL DT_CHKINE(Pp2,Ifpr2,Pp1,Ifpr1,Pt2,Ifta2,Pt1,Ifta1,
     &                        amch2,amch2n,amch1,irej1)
               IF ( irej1.GT.0 ) GOTO 100
               IF ( irej1.EQ.-1 ) idr2 = 100*idr2
               CALL DT_CH2RES(ifp1(1),ifp1(2),ift1(1),ift1(2),idr1,
     &                        idxr1,amch1,amch1n,idch1,irej1)
               IF ( irej1.NE.0 ) GOTO 100
               IF ( idr1.NE.0 ) idr1 = 100*idr1
            ELSE
               CALL DT_CHKINE(Pp1,Ifpr1,Pp2,Ifpr2,Pt1,Ifta1,Pt2,Ifta2,
     &                        amch1,amch1n,amch2,irej1)
               IF ( irej1.GT.0 ) GOTO 100
               IF ( irej1.EQ.-1 ) idr1 = 100*idr1
               CALL DT_CH2RES(ifp2(1),ifp2(2),ift2(1),ift2(2),idr2,
     &                        idxr2,amch2,amch2n,idch2,irej1)
               IF ( irej1.NE.0 ) GOTO 100
               IF ( idr2.NE.0 ) idr2 = 100*idr2
            END IF
         END IF
      END IF
 
C store final configuration for energy-momentum cons. check
      IF ( LEMcck ) THEN
         CALL DT_EMC1(Pp1,Pp2,Pt1,Pt2,-2,1,idum)
         CALL DT_EMC1(Pp1,Pp2,Pt1,Pt2,3,1,irej1)
         IF ( irej1.NE.0 ) GOTO 100
      END IF
 
C put partons and chains into DTEVT1
      DO i = 1 , 4
         pch1(i) = Pp1(i) + Pt1(i)
         pch2(i) = Pp2(i) + Pt2(i)
      END DO
      CALL DT_EVTPUT(-ISThkk(Mop1),Ifpr1,Mop1,0,Pp1(1),Pp1(2),Pp1(3),
     &               Pp1(4),0,0,0)
      CALL DT_EVTPUT(-ISThkk(Mot1),Ifta1,Mot1,0,Pt1(1),Pt1(2),Pt1(3),
     &               Pt1(4),0,0,0)
      kch = 100 + IDCh(Mop1)*10 + 1
      CALL DT_EVTPUT(kch,88888,-2,-1,pch1(1),pch1(2),pch1(3),pch1(4),
     &               idr1,idxr1,IDCh(Mop1))
      CALL DT_EVTPUT(-ISThkk(Mop2),Ifpr2,Mop2,0,Pp2(1),Pp2(2),Pp2(3),
     &               Pp2(4),0,0,0)
      CALL DT_EVTPUT(-ISThkk(Mot2),Ifta2,Mot2,0,Pt2(1),Pt2(2),Pt2(3),
     &               Pt2(4),0,0,0)
      kch = kch + 1
      CALL DT_EVTPUT(kch,88888,-2,-1,pch2(1),pch2(2),pch2(3),pch2(4),
     &               idr2,idxr2,IDCh(Mop2))
 
      RETURN
 
 100  IF ( (IDCh(Mop1).LE.3) .AND. (IDCh(Mop2).LE.3) ) THEN
C "cancel" sea-sea chains
         CALL DT_RJSEAC(Mop1,Mop2,Mot1,Mot2,irej1)
         IF ( irej1.EQ.0 ) THEN
C*sr 16.5. flag for EVENTB
            Irej = -1
            RETURN
         END IF
      END IF
      Irej = 1
      END SUBROUTINE
