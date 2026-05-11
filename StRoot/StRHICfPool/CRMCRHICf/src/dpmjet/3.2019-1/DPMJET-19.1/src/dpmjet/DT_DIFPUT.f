
      SUBROUTINE DT_DIFPUT(Ifp1,Ifp2,Pp,Mop,Kp,Ift1,Ift2,Pt,Mot,Kt,Ncsy,
     &                     Irej)
 
C***********************************************************************
C Dump diffractive chains into DTEVT1                                  *
C This version dated 12.02.95 is written by S. Roesler                 *
C***********************************************************************
 
      IMPLICIT NONE
      DOUBLE PRECISION DT_RNDM , OHALF , ONE , p1 , p2 , p3 , p4 , pch , 
     &                 Pp , pp1 , pp2 , Pt , pt1 , pt2 , xm1 , xm2 , 
     &                 ZERO
      INTEGER id1 , id2 , Ifp1 , Ifp2 , Ift1 , Ift2 , Irej , irej1 , k , 
     &        Kp , Kt , Mop , Mot , Ncsy
      SAVE 
 
      INCLUDE 'inc/dtflka'
 
      PARAMETER (ZERO=0.0D0,OHALF=0.5D0,ONE=1.0D0)
 
      LOGICAL lchk
 
C kinematics of diffractive interactions (DTUNUC 1.x)
      INCLUDE 'inc/dtdiki'
C event history
 
 
      INCLUDE 'inc/dtevt1'
C extended event history
      INCLUDE 'inc/dtevt2'
C rejection counter
      INCLUDE 'inc/dtrejc'
 
      DIMENSION pp1(4) , pp2(4) , pt1(4) , pt2(4) , pch(4) , Pp(4) , 
     &          Pt(4) , p1(4) , p2(4) , p3(4) , p4(4)
 
      Irej = 0
 
      IF ( Kp.EQ.1 ) THEN
         DO k = 1 , 4
            pch(k) = PPLm1(k) + PPLm2(k)
         END DO
         id1 = Ifp1
         id2 = Ifp2
         IF ( DT_RNDM(ONE).GT.OHALF ) THEN
            id1 = Ifp2
            id2 = Ifp1
         END IF
         CALL DT_EVTPUT(21,id1,Mop,0,PPLm1(1),PPLm1(2),PPLm1(3),PPLm1(4)
     &                  ,0,0,0)
         CALL DT_EVTPUT(21,id2,Mop,0,PPLm2(1),PPLm2(2),PPLm2(3),PPLm2(4)
     &                  ,0,0,0)
         CALL DT_EVTPUT(281,88888,-2,-1,pch(1),pch(2),pch(3),pch(4),
     &                  IDPr,IDXpr,8)
      ELSE IF ( Kp.EQ.2 ) THEN
         DO k = 1 , 4
            pp1(k) = XPH(1)*Pp(k)
            pp2(k) = XPH(2)*Pp(k)
            pt1(k) = -XPPo(1)*PPOm(k)
            pt2(k) = -XPPo(2)*PPOm(k)
         END DO
         CALL DT_CHKCSY(Ifp1,IFPpo(1),lchk)
         xm1 = ZERO
         xm2 = ZERO
         IF ( lchk ) THEN
            CALL DT_MASHEL(pp1,pt1,xm1,xm2,p1,p2,irej1)
            IF ( irej1.NE.0 ) GOTO 100
            CALL DT_MASHEL(pp2,pt2,xm1,xm2,p3,p4,irej1)
            IF ( irej1.NE.0 ) GOTO 100
            DO k = 1 , 4
               pp1(k) = p1(k)
               pt1(k) = p2(k)
               pp2(k) = p3(k)
               pt2(k) = p4(k)
            END DO
            CALL DT_EVTPUT(-21,Ifp1,Mop,0,pp1(1),pp1(2),pp1(3),pp1(4),0,
     &                     0,8)
            CALL DT_EVTPUT(-41,IFPpo(1),Mot,0,pt1(1),pt1(2),pt1(3),
     &                     pt1(4),0,0,8)
            CALL DT_EVTPUT(-21,Ifp2,Mop,0,pp2(1),pp2(2),pp2(3),pp2(4),0,
     &                     0,8)
            CALL DT_EVTPUT(-41,IFPpo(2),Mot,0,pt2(1),pt2(2),pt2(3),
     &                     pt2(4),0,0,8)
         ELSE
            CALL DT_MASHEL(pp1,pt2,xm1,xm2,p1,p2,irej1)
            IF ( irej1.NE.0 ) GOTO 100
            CALL DT_MASHEL(pp2,pt1,xm1,xm2,p3,p4,irej1)
            IF ( irej1.NE.0 ) GOTO 100
            DO k = 1 , 4
               pp1(k) = p1(k)
               pt2(k) = p2(k)
               pp2(k) = p3(k)
               pt1(k) = p4(k)
            END DO
            CALL DT_EVTPUT(-21,Ifp1,Mop,0,pp1(1),pp1(2),pp1(3),pp1(4),0,
     &                     0,8)
            CALL DT_EVTPUT(-41,IFPpo(2),Mot,0,pt2(1),pt2(2),pt2(3),
     &                     pt2(4),0,0,8)
            CALL DT_EVTPUT(-21,Ifp2,Mop,0,pp2(1),pp2(2),pp2(3),pp2(4),0,
     &                     0,8)
            CALL DT_EVTPUT(-41,IFPpo(1),Mot,0,pt1(1),pt1(2),pt1(3),
     &                     pt1(4),0,0,8)
         END IF
         Ncsy = Ncsy + 1
      ELSE
         CALL DT_EVTPUT(1,IDHkk(Mop),Mop,0,PSC(1),PSC(2),PSC(3),PSC(4),
     &                  0,0,0)
      END IF
 
      IF ( Kt.EQ.1 ) THEN
         DO k = 1 , 4
            pch(k) = PTLm1(k) + PTLm2(k)
         END DO
         id1 = Ift1
         id2 = Ift2
         IF ( DT_RNDM(ONE).GT.OHALF ) THEN
            id1 = Ift2
            id2 = Ift1
         END IF
         CALL DT_EVTPUT(22,id1,Mot,0,PTLm1(1),PTLm1(2),PTLm1(3),PTLm1(4)
     &                  ,0,0,0)
         CALL DT_EVTPUT(22,id2,Mot,0,PTLm2(1),PTLm2(2),PTLm2(3),PTLm2(4)
     &                  ,0,0,0)
         CALL DT_EVTPUT(281,88888,-2,-1,pch(1),pch(2),pch(3),pch(4),
     &                  IDTr,IDXtr,8)
      ELSE IF ( Kt.EQ.2 ) THEN
         DO k = 1 , 4
            pp1(k) = XTPo(1)*PPOm(k)
            pp2(k) = XTPo(2)*PPOm(k)
            pt1(k) = XTH(2)*Pt(k)
            pt2(k) = XTH(1)*Pt(k)
         END DO
         CALL DT_CHKCSY(IFTpo(1),Ift1,lchk)
         xm1 = ZERO
         xm2 = ZERO
         IF ( lchk ) THEN
            CALL DT_MASHEL(pp1,pt1,xm1,xm2,p1,p2,irej1)
            IF ( irej1.NE.0 ) GOTO 100
            CALL DT_MASHEL(pp2,pt2,xm1,xm2,p3,p4,irej1)
            IF ( irej1.NE.0 ) GOTO 100
            DO k = 1 , 4
               pp1(k) = p1(k)
               pt1(k) = p2(k)
               pp2(k) = p3(k)
               pt2(k) = p4(k)
            END DO
            CALL DT_EVTPUT(-41,IFTpo(1),Mop,0,pp1(1),pp1(2),pp1(3),
     &                     pp1(4),0,0,8)
            CALL DT_EVTPUT(-21,Ift1,Mot,0,pt1(1),pt1(2),pt1(3),pt1(4),0,
     &                     0,8)
            CALL DT_EVTPUT(-41,IFTpo(2),Mop,0,pp2(1),pp2(2),pp2(3),
     &                     pp2(4),0,0,8)
            CALL DT_EVTPUT(-21,Ift2,Mot,0,pt2(1),pt2(2),pt2(3),pt2(4),0,
     &                     0,8)
         ELSE
            CALL DT_MASHEL(pp1,pt2,xm1,xm2,p1,p2,irej1)
            IF ( irej1.NE.0 ) GOTO 100
            CALL DT_MASHEL(pp2,pt1,xm1,xm2,p3,p4,irej1)
            IF ( irej1.NE.0 ) GOTO 100
            DO k = 1 , 4
               pp1(k) = p1(k)
               pt2(k) = p2(k)
               pp2(k) = p3(k)
               pt1(k) = p4(k)
            END DO
            CALL DT_EVTPUT(-41,IFTpo(1),Mop,0,pp1(1),pp1(2),pp1(3),
     &                     pp1(4),0,0,8)
            CALL DT_EVTPUT(-21,Ift2,Mot,0,pt2(1),pt2(2),pt2(3),pt2(4),0,
     &                     0,8)
            CALL DT_EVTPUT(-41,IFTpo(2),Mop,0,pp2(1),pp2(2),pp2(3),
     &                     pp2(4),0,0,8)
            CALL DT_EVTPUT(-21,Ift1,Mot,0,pt1(1),pt1(2),pt1(3),pt1(4),0,
     &                     0,8)
         END IF
         Ncsy = Ncsy + 1
      ELSE
         CALL DT_EVTPUT(1,IDHkk(Mot),Mot,0,PSC(1),PSC(2),PSC(3),PSC(4),
     &                  0,0,0)
      END IF
 
      RETURN
 
 100  IRDiff(2) = IRDiff(2) + 1
      Irej = 1
      END SUBROUTINE
