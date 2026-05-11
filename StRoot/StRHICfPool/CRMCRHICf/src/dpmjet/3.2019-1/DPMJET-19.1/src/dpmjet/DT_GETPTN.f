
      SUBROUTINE DT_GETPTN(Ip,Nn,Ncsy,Irej)
 
C***********************************************************************
C This subroutine collects partons at chain ends from temporary        *
C commons and puts them into DTEVT1.                                   *
C This version dated 15.01.95 is written by S. Roesler                 *
C***********************************************************************
 
      IMPLICIT NONE
      DOUBLE PRECISION am1 , am2 , amds , amss , amvd , amvs , amvv , 
     &                 ech , OHALF , pp , pp1 , pp2 , pt , pt1 , pt2 , 
     &                 ptoch , TINY10 , ZERO
      INTEGER i , IDT_IB2PDG , idxp , idxt , ifp1 , ifp2 , ift1 , ift2 , 
     &        Ip , Irej , irej1 , istck , k , MAXINT , MAXNCL , MAXSQU , 
     &        MAXVQU , mop , mot , Ncsy
      INTEGER Nn
      SAVE 
 
      INCLUDE 'inc/dtflka'
 
      PARAMETER (TINY10=1.0D-10,ZERO=0.0D0,OHALF=0.5D0)
 
      LOGICAL lchk
 
 
      PARAMETER (MAXNCL=260,MAXVQU=MAXNCL,MAXSQU=20*MAXVQU,
     &           MAXINT=MAXVQU+MAXSQU)
C event history
 
 
      INCLUDE 'inc/dtevt1'
C extended event history
      INCLUDE 'inc/dtevt2'
C flags for input different options
      INCLUDE 'inc/dtflg1'
C auxiliary common for chain system storage (DTUNUC 1.x)
      INCLUDE 'inc/dtchsy'
C statistics
      INCLUDE 'inc/dtsta1'
C flags for diffractive interactions (DTUNUC 1.x)
      INCLUDE 'inc/dtflg3'
C x-values of partons (DTUNUC 1.x)
      INCLUDE 'inc/dtdpmx'
C flavors of partons (DTUNUC 1.x)
      INCLUDE 'inc/dtdpmf'
C auxiliary common for x-value and flavor storage of partons (DTUNUC 1.x)
      INCLUDE 'inc/dtdpmi'
C auxiliary common for x-value and flavor storage of partons (DTUNUC 1.x)
      INCLUDE 'inc/dtdpm0'
 
      DIMENSION pp1(4) , pp2(4) , pt1(4) , pt2(4) , pp(4) , pt(4)
 
      DATA amss , amvs , amds , amvd , amvv/0.4D0 , 2.0D0 , 2.0D0 , 
     &     2.5D0 , 2.0D0/
 
      Irej = 0
      Ncsy = 0
      NPOint(2) = NHKk + 1
 
C sea-sea chains
      DO i = 1 , NSS
         IF ( ISKpch(1,i).NE.99 ) THEN
            ICChai(1,1) = ICChai(1,1) + 2
            idxp = INTss1(i)
            idxt = INTss2(i)
            mop = JDAhkk(1,IPOsp(IFRosp(idxp)))
            mot = JDAhkk(1,IPOst(IFRost(idxt)))
            DO k = 1 , 4
               pp1(k) = XPSq(idxp)*PHKk(k,mop)
               pp2(k) = XPSaq(idxp)*PHKk(k,mop)
               pt1(k) = XTSaq(idxt)*PHKk(k,mot)
               pt2(k) = XTSq(idxt)*PHKk(k,mot)
            END DO
            ptoch = SQRT((pp1(1)+pt1(1))**2+(pp1(2)+pt1(2))
     &              **2+(pp1(3)+pt1(3))**2)
            ech = pp1(4) + pt1(4)
            am1 = (ech+ptoch)*(ech-ptoch)
            ptoch = SQRT((pp2(1)+pt2(1))**2+(pp2(2)+pt2(2))
     &              **2+(pp2(3)+pt2(3))**2)
            ech = pp2(4) + pt2(4)
            am2 = (ech+ptoch)*(ech-ptoch)
            IF ( (am1.GT.0.0D0) .AND. (am2.GT.0.0D0) ) THEN
               am1 = SQRT(am1)
               am2 = SQRT(am2)
               IF ( (am1.LT.amss) .OR. (am2.LT.amss) ) THEN
               END IF
            ELSE
 
               IF ( LPRi.GT.4 ) WRITE (LOUt,99010) NEVhkk , i , am1 , 
     &              am2
C              WRITE(LOUT,5000) NEVHKK,I,AM1,AM2
99010          FORMAT (1X,'incon. chain mass SS: ',2I5,2E10.3)
            END IF
            ifp1 = IDT_IB2PDG(IPSq(idxp),0,2)
            ifp2 = IDT_IB2PDG(IPSaq(idxp),0,2)
            ift1 = IDT_IB2PDG(ITSaq(idxt),0,2)
            ift2 = IDT_IB2PDG(ITSq(idxt),0,2)
            CALL DT_EVTPUT(-31,ifp1,mop,0,pp1(1),pp1(2),pp1(3),pp1(4),0,
     &                     0,1)
            CALL DT_EVTPUT(-32,ift1,mot,0,pt1(1),pt1(2),pt1(3),pt1(4),0,
     &                     0,1)
            CALL DT_EVTPUT(-31,ifp2,mop,0,pp2(1),pp2(2),pp2(3),pp2(4),0,
     &                     0,1)
            CALL DT_EVTPUT(-32,ift2,mot,0,pt2(1),pt2(2),pt2(3),pt2(4),0,
     &                     0,1)
            Ncsy = Ncsy + 1
         END IF
      END DO
 
C disea-sea chains
      DO i = 1 , NDS
         IF ( ISKpch(2,i).NE.99 ) THEN
            ICChai(1,2) = ICChai(1,2) + 2
            idxp = INTds1(i)
            idxt = INTds2(i)
            mop = JDAhkk(1,IPOsp(IFRosp(idxp)))
            mot = JDAhkk(1,IPOst(IFRost(idxt)))
            DO k = 1 , 4
               pp1(k) = XPSq(idxp)*PHKk(k,mop)
               pp2(k) = XPSaq(idxp)*PHKk(k,mop)
               pt1(k) = XTSq(idxt)*PHKk(k,mot)
               pt2(k) = XTSaq(idxt)*PHKk(k,mot)
            END DO
            ptoch = SQRT((pp1(1)+pt1(1))**2+(pp1(2)+pt1(2))
     &              **2+(pp1(3)+pt1(3))**2)
            ech = pp1(4) + pt1(4)
            am1 = (ech+ptoch)*(ech-ptoch)
            ptoch = SQRT((pp2(1)+pt2(1))**2+(pp2(2)+pt2(2))
     &              **2+(pp2(3)+pt2(3))**2)
            ech = pp2(4) + pt2(4)
            am2 = (ech+ptoch)*(ech-ptoch)
            IF ( (am1.GT.0.0D0) .AND. (am2.GT.0.0D0) ) THEN
               am1 = SQRT(am1)
               am2 = SQRT(am2)
               IF ( (am1.LT.amds) .OR. (am2.LT.amds) ) THEN
               END IF
            ELSE
 
               IF ( LPRi.GT.4 ) WRITE (LOUt,99020) NEVhkk , i , am1 , 
     &              am2
C              WRITE(LOUT,5001) NEVHKK,I,AM1,AM2
99020          FORMAT (1X,'incon. chain mass DS: ',2I5,2E10.3)
            END IF
            ifp1 = IDT_IB2PDG(IPSq(idxp),IPSq2(idxp),2)
            ifp2 = IDT_IB2PDG(-IPSq(idxp),-IPSq2(idxp),2)
            ift1 = IDT_IB2PDG(ITSq(idxt),0,2)
            ift2 = IDT_IB2PDG(ITSaq(idxt),0,2)
            CALL DT_EVTPUT(-31,ifp1,mop,0,pp1(1),pp1(2),pp1(3),pp1(4),0,
     &                     0,2)
            CALL DT_EVTPUT(-32,ift1,mot,0,pt1(1),pt1(2),pt1(3),pt1(4),0,
     &                     0,2)
            CALL DT_EVTPUT(-31,ifp2,mop,0,pp2(1),pp2(2),pp2(3),pp2(4),0,
     &                     0,2)
            CALL DT_EVTPUT(-32,ift2,mot,0,pt2(1),pt2(2),pt2(3),pt2(4),0,
     &                     0,2)
            Ncsy = Ncsy + 1
         END IF
      END DO
 
C sea-disea chains
      DO i = 1 , NSD
         IF ( ISKpch(3,i).NE.99 ) THEN
            ICChai(1,3) = ICChai(1,3) + 2
            idxp = INTsd1(i)
            idxt = INTsd2(i)
            mop = JDAhkk(1,IPOsp(IFRosp(idxp)))
            mot = JDAhkk(1,IPOst(IFRost(idxt)))
            DO k = 1 , 4
               pp1(k) = XPSq(idxp)*PHKk(k,mop)
               pp2(k) = XPSaq(idxp)*PHKk(k,mop)
               pt1(k) = XTSq(idxt)*PHKk(k,mot)
               pt2(k) = XTSaq(idxt)*PHKk(k,mot)
            END DO
            ptoch = SQRT((pp1(1)+pt1(1))**2+(pp1(2)+pt1(2))
     &              **2+(pp1(3)+pt1(3))**2)
            ech = pp1(4) + pt1(4)
            am1 = (ech+ptoch)*(ech-ptoch)
            ptoch = SQRT((pp2(1)+pt2(1))**2+(pp2(2)+pt2(2))
     &              **2+(pp2(3)+pt2(3))**2)
            ech = pp2(4) + pt2(4)
            am2 = (ech+ptoch)*(ech-ptoch)
            IF ( (am1.GT.0.0D0) .AND. (am2.GT.0.0D0) ) THEN
               am1 = SQRT(am1)
               am2 = SQRT(am2)
               IF ( (am1.LT.amds) .OR. (am2.LT.amds) ) THEN
               END IF
            ELSE
 
               IF ( LPRi.GT.4 ) WRITE (LOUt,99030) NEVhkk , i , am1 , 
     &              am2
C              WRITE(LOUT,5002) NEVHKK,I,AM1,AM2
99030          FORMAT (1X,'incon. chain mass SD: ',2I5,2E10.3)
            END IF
            ifp1 = IDT_IB2PDG(IPSq(idxp),0,2)
            ifp2 = IDT_IB2PDG(IPSaq(idxp),0,2)
            ift1 = IDT_IB2PDG(ITSq(idxt),ITSq2(idxt),2)
            ift2 = IDT_IB2PDG(-ITSq(idxt),-ITSq2(idxt),2)
            CALL DT_EVTPUT(-31,ifp1,mop,0,pp1(1),pp1(2),pp1(3),pp1(4),0,
     &                     0,3)
            CALL DT_EVTPUT(-32,ift1,mot,0,pt1(1),pt1(2),pt1(3),pt1(4),0,
     &                     0,3)
            CALL DT_EVTPUT(-31,ifp2,mop,0,pp2(1),pp2(2),pp2(3),pp2(4),0,
     &                     0,3)
            CALL DT_EVTPUT(-32,ift2,mot,0,pt2(1),pt2(2),pt2(3),pt2(4),0,
     &                     0,3)
            Ncsy = Ncsy + 1
         END IF
      END DO
 
C disea-valence chains
      DO i = 1 , NDV
         IF ( ISKpch(5,i).NE.99 ) THEN
            ICChai(1,5) = ICChai(1,5) + 2
            idxp = INTdv1(i)
            idxt = INTdv2(i)
            mop = JDAhkk(1,IPOsp(IFRosp(idxp)))
            mot = JDAhkk(1,IPOst(IFRovt(idxt)))
            DO k = 1 , 4
               pp1(k) = XPSq(idxp)*PHKk(k,mop)
               pp2(k) = XPSaq(idxp)*PHKk(k,mop)
               pt1(k) = XTVq(idxt)*PHKk(k,mot)
               pt2(k) = XTVd(idxt)*PHKk(k,mot)
            END DO
            ptoch = SQRT((pp1(1)+pt1(1))**2+(pp1(2)+pt1(2))
     &              **2+(pp1(3)+pt1(3))**2)
            ech = pp1(4) + pt1(4)
            am1 = (ech+ptoch)*(ech-ptoch)
            ptoch = SQRT((pp2(1)+pt2(1))**2+(pp2(2)+pt2(2))
     &              **2+(pp2(3)+pt2(3))**2)
            ech = pp2(4) + pt2(4)
            am2 = (ech+ptoch)*(ech-ptoch)
            IF ( (am1.GT.0.0D0) .AND. (am2.GT.0.0D0) ) THEN
               am1 = SQRT(am1)
               am2 = SQRT(am2)
               IF ( (am1.LT.amvd) .OR. (am2.LT.amvd) ) THEN
               END IF
            ELSE
 
               IF ( LPRi.GT.4 ) WRITE (LOUt,99040) NEVhkk , i , am1 , 
     &              am2
C              WRITE(LOUT,5003) NEVHKK,I,AM1,AM2
99040          FORMAT (1X,'incon. chain mass DV: ',2I5,2E10.3)
            END IF
            ifp1 = IDT_IB2PDG(IPSq(idxp),IPSq2(idxp),2)
            ifp2 = IDT_IB2PDG(-IPSq(idxp),-IPSq2(idxp),2)
            ift1 = IDT_IB2PDG(ITVq(idxt),0,2)
            ift2 = IDT_IB2PDG(ITTv1(idxt),ITTv2(idxt),2)
            CALL DT_EVTPUT(-31,ifp1,mop,0,pp1(1),pp1(2),pp1(3),pp1(4),0,
     &                     0,5)
            CALL DT_EVTPUT(-22,ift1,mot,0,pt1(1),pt1(2),pt1(3),pt1(4),0,
     &                     0,5)
            CALL DT_EVTPUT(-31,ifp2,mop,0,pp2(1),pp2(2),pp2(3),pp2(4),0,
     &                     0,5)
            CALL DT_EVTPUT(-22,ift2,mot,0,pt2(1),pt2(2),pt2(3),pt2(4),0,
     &                     0,5)
            Ncsy = Ncsy + 1
         END IF
      END DO
 
C valence-sea chains
      DO i = 1 , NVS
         IF ( ISKpch(6,i).NE.99 ) THEN
            ICChai(1,6) = ICChai(1,6) + 2
            idxp = INTvs1(i)
            idxt = INTvs2(i)
            mop = JDAhkk(1,IPOsp(IFRovp(idxp)))
            mot = JDAhkk(1,IPOst(IFRost(idxt)))
            DO k = 1 , 4
               pp1(k) = XPVq(idxp)*PHKk(k,mop)
               pp2(k) = XPVd(idxp)*PHKk(k,mop)
               pt1(k) = XTSaq(idxt)*PHKk(k,mot)
               pt2(k) = XTSq(idxt)*PHKk(k,mot)
            END DO
            ifp1 = IDT_IB2PDG(IPVq(idxp),0,2)
            ifp2 = IDT_IB2PDG(IPPv1(idxp),IPPv2(idxp),2)
            ift1 = IDT_IB2PDG(ITSaq(idxt),0,2)
            ift2 = IDT_IB2PDG(ITSq(idxt),0,2)
            CALL DT_CHKCSY(ifp1,ift1,lchk)
            IF ( lchk ) THEN
               CALL DT_EVTPUT(-21,ifp1,mop,0,pp1(1),pp1(2),pp1(3),pp1(4)
     &                        ,0,0,6)
               CALL DT_EVTPUT(-32,ift1,mot,0,pt1(1),pt1(2),pt1(3),pt1(4)
     &                        ,0,0,6)
               CALL DT_EVTPUT(-21,ifp2,mop,0,pp2(1),pp2(2),pp2(3),pp2(4)
     &                        ,0,0,6)
               CALL DT_EVTPUT(-32,ift2,mot,0,pt2(1),pt2(2),pt2(3),pt2(4)
     &                        ,0,0,6)
               ptoch = SQRT((pp1(1)+pt1(1))**2+(pp1(2)+pt1(2))
     &                 **2+(pp1(3)+pt1(3))**2)
               ech = pp1(4) + pt1(4)
               am1 = (ech+ptoch)*(ech-ptoch)
               ptoch = SQRT((pp2(1)+pt2(1))**2+(pp2(2)+pt2(2))
     &                 **2+(pp2(3)+pt2(3))**2)
               ech = pp2(4) + pt2(4)
               am2 = (ech+ptoch)*(ech-ptoch)
            ELSE
               CALL DT_EVTPUT(-21,ifp1,mop,0,pp1(1),pp1(2),pp1(3),pp1(4)
     &                        ,0,0,6)
               CALL DT_EVTPUT(-32,ift2,mot,0,pt2(1),pt2(2),pt2(3),pt2(4)
     &                        ,0,0,6)
               CALL DT_EVTPUT(-21,ifp2,mop,0,pp2(1),pp2(2),pp2(3),pp2(4)
     &                        ,0,0,6)
               CALL DT_EVTPUT(-32,ift1,mot,0,pt1(1),pt1(2),pt1(3),pt1(4)
     &                        ,0,0,6)
               ptoch = SQRT((pp1(1)+pt2(1))**2+(pp1(2)+pt2(2))
     &                 **2+(pp1(3)+pt2(3))**2)
               ech = pp1(4) + pt2(4)
               am2 = (ech+ptoch)*(ech-ptoch)
               ptoch = SQRT((pp2(1)+pt1(1))**2+(pp2(2)+pt1(2))
     &                 **2+(pp2(3)+pt1(3))**2)
               ech = pp2(4) + pt1(4)
               am1 = (ech+ptoch)*(ech-ptoch)
            END IF
            IF ( (am1.GT.0.0D0) .AND. (am2.GT.0.0D0) ) THEN
               am1 = SQRT(am1)
               am2 = SQRT(am2)
               IF ( (am1.LT.amss) .OR. (am2.LT.amvs) ) THEN
               END IF
            ELSE
 
               IF ( LPRi.GT.4 ) WRITE (LOUt,99050) NEVhkk , i , am1 , 
     &              am2
C              WRITE(LOUT,5004) NEVHKK,I,AM1,AM2
99050          FORMAT (1X,'incon. chain mass VS: ',2I5,2E10.3)
            END IF
            Ncsy = Ncsy + 1
         END IF
      END DO
 
C sea-valence chains
      DO i = 1 , NSV
         IF ( ISKpch(4,i).NE.99 ) THEN
            ICChai(1,4) = ICChai(1,4) + 2
            idxp = INTsv1(i)
            idxt = INTsv2(i)
            mop = JDAhkk(1,IPOsp(IFRosp(idxp)))
            mot = JDAhkk(1,IPOst(IFRovt(idxt)))
            DO k = 1 , 4
               pp1(k) = XPSq(idxp)*PHKk(k,mop)
               pp2(k) = XPSaq(idxp)*PHKk(k,mop)
               pt1(k) = XTVd(idxt)*PHKk(k,mot)
               pt2(k) = XTVq(idxt)*PHKk(k,mot)
            END DO
            ptoch = SQRT((pp1(1)+pt1(1))**2+(pp1(2)+pt1(2))
     &              **2+(pp1(3)+pt1(3))**2)
            ech = pp1(4) + pt1(4)
            am1 = (ech+ptoch)*(ech-ptoch)
            ptoch = SQRT((pp2(1)+pt2(1))**2+(pp2(2)+pt2(2))
     &              **2+(pp2(3)+pt2(3))**2)
            ech = pp2(4) + pt2(4)
            am2 = (ech+ptoch)*(ech-ptoch)
            IF ( (am1.GT.0.0D0) .AND. (am2.GT.0.0D0) ) THEN
               am1 = SQRT(am1)
               am2 = SQRT(am2)
               IF ( (am1.LT.amvs) .OR. (am2.LT.amss) ) THEN
               END IF
            ELSE
 
               IF ( LPRi.GT.4 ) WRITE (LOUt,99060) NEVhkk , i , am1 , 
     &              am2
C              WRITE(LOUT,5005) NEVHKK,I,AM1,AM2
99060          FORMAT (1X,'incon. chain mass SV: ',2I5,2E10.3)
            END IF
            ifp1 = IDT_IB2PDG(IPSq(idxp),0,2)
            ifp2 = IDT_IB2PDG(IPSaq(idxp),0,2)
            ift1 = IDT_IB2PDG(ITTv1(idxt),ITTv2(idxt),2)
            ift2 = IDT_IB2PDG(ITVq(idxt),0,2)
            CALL DT_EVTPUT(-31,ifp1,mop,0,pp1(1),pp1(2),pp1(3),pp1(4),0,
     &                     0,4)
            CALL DT_EVTPUT(-22,ift1,mot,0,pt1(1),pt1(2),pt1(3),pt1(4),0,
     &                     0,4)
            CALL DT_EVTPUT(-31,ifp2,mop,0,pp2(1),pp2(2),pp2(3),pp2(4),0,
     &                     0,4)
            CALL DT_EVTPUT(-22,ift2,mot,0,pt2(1),pt2(2),pt2(3),pt2(4),0,
     &                     0,4)
            Ncsy = Ncsy + 1
         END IF
      END DO
 
C valence-disea chains
      DO i = 1 , NVD
         IF ( ISKpch(7,i).NE.99 ) THEN
            ICChai(1,7) = ICChai(1,7) + 2
            idxp = INTvd1(i)
            idxt = INTvd2(i)
            mop = JDAhkk(1,IPOsp(IFRovp(idxp)))
            mot = JDAhkk(1,IPOst(IFRost(idxt)))
            DO k = 1 , 4
               pp1(k) = XPVq(idxp)*PHKk(k,mop)
               pp2(k) = XPVd(idxp)*PHKk(k,mop)
               pt1(k) = XTSq(idxt)*PHKk(k,mot)
               pt2(k) = XTSaq(idxt)*PHKk(k,mot)
            END DO
            ifp1 = IDT_IB2PDG(IPVq(idxp),0,2)
            ifp2 = IDT_IB2PDG(IPPv1(idxp),IPPv2(idxp),2)
            ift1 = IDT_IB2PDG(ITSq(idxt),ITSq2(idxt),2)
            ift2 = IDT_IB2PDG(-ITSq(idxt),-ITSq2(idxt),2)
            CALL DT_CHKCSY(ifp1,ift1,lchk)
            IF ( lchk ) THEN
               CALL DT_EVTPUT(-21,ifp1,mop,0,pp1(1),pp1(2),pp1(3),pp1(4)
     &                        ,0,0,7)
               CALL DT_EVTPUT(-32,ift1,mot,0,pt1(1),pt1(2),pt1(3),pt1(4)
     &                        ,0,0,7)
               CALL DT_EVTPUT(-21,ifp2,mop,0,pp2(1),pp2(2),pp2(3),pp2(4)
     &                        ,0,0,7)
               CALL DT_EVTPUT(-32,ift2,mot,0,pt2(1),pt2(2),pt2(3),pt2(4)
     &                        ,0,0,7)
               ptoch = SQRT((pp1(1)+pt1(1))**2+(pp1(2)+pt1(2))
     &                 **2+(pp1(3)+pt1(3))**2)
               ech = pp1(4) + pt1(4)
               am1 = (ech+ptoch)*(ech-ptoch)
               ptoch = SQRT((pp2(1)+pt2(1))**2+(pp2(2)+pt2(2))
     &                 **2+(pp2(3)+pt2(3))**2)
               ech = pp2(4) + pt2(4)
               am2 = (ech+ptoch)*(ech-ptoch)
            ELSE
               CALL DT_EVTPUT(-21,ifp1,mop,0,pp1(1),pp1(2),pp1(3),pp1(4)
     &                        ,0,0,7)
               CALL DT_EVTPUT(-32,ift2,mot,0,pt2(1),pt2(2),pt2(3),pt2(4)
     &                        ,0,0,7)
               CALL DT_EVTPUT(-21,ifp2,mop,0,pp2(1),pp2(2),pp2(3),pp2(4)
     &                        ,0,0,7)
               CALL DT_EVTPUT(-32,ift1,mot,0,pt1(1),pt1(2),pt1(3),pt1(4)
     &                        ,0,0,7)
               ptoch = SQRT((pp1(1)+pt2(1))**2+(pp1(2)+pt2(2))
     &                 **2+(pp1(3)+pt2(3))**2)
               ech = pp1(4) + pt2(4)
               am1 = (ech+ptoch)*(ech-ptoch)
               ptoch = SQRT((pp2(1)+pt1(1))**2+(pp2(2)+pt1(2))
     &                 **2+(pp2(3)+pt1(3))**2)
               ech = pp2(4) + pt1(4)
               am2 = (ech+ptoch)*(ech-ptoch)
            END IF
            IF ( (am1.GT.0.0D0) .AND. (am2.GT.0.0D0) ) THEN
               am1 = SQRT(am1)
               am2 = SQRT(am2)
               IF ( (am1.LT.amvd) .OR. (am2.LT.amvd) ) THEN
               END IF
            ELSE
 
               IF ( LPRi.GT.4 ) WRITE (LOUt,99070) NEVhkk , i , am1 , 
     &              am2
C              WRITE(LOUT,5006) NEVHKK,I,AM1,AM2
99070          FORMAT (1X,'incon. chain mass VD: ',2I5,2E10.3)
            END IF
            Ncsy = Ncsy + 1
         END IF
      END DO
 
C valence-valence chains
      DO i = 1 , NVV
         IF ( ISKpch(8,i).NE.99 ) THEN
            ICChai(1,8) = ICChai(1,8) + 2
            idxp = INTvv1(i)
            idxt = INTvv2(i)
            mop = JDAhkk(1,IPOsp(IFRovp(idxp)))
            mot = JDAhkk(1,IPOst(IFRovt(idxt)))
            DO k = 1 , 4
               pp1(k) = XPVq(idxp)*PHKk(k,mop)
               pp2(k) = XPVd(idxp)*PHKk(k,mop)
               pt1(k) = XTVd(idxt)*PHKk(k,mot)
               pt2(k) = XTVq(idxt)*PHKk(k,mot)
            END DO
            ifp1 = IDT_IB2PDG(IPVq(idxp),0,2)
            ifp2 = IDT_IB2PDG(IPPv1(idxp),IPPv2(idxp),2)
            ift1 = IDT_IB2PDG(ITTv1(idxt),ITTv2(idxt),2)
            ift2 = IDT_IB2PDG(ITVq(idxt),0,2)
 
C check for diffractive event
            IDIff = 0
            IF ( ((ISIngd.GT.0) .OR. (IDOubd.GT.0)) .AND. (Ip.EQ.1)
     &           .AND. (Nn.EQ.1) ) THEN
               DO k = 1 , 4
                  pp(k) = pp1(k) + pp2(k)
                  pt(k) = pt1(k) + pt2(k)
               END DO
               istck = NHKk
               CALL DT_DIFEVT(ifp1,ifp2,pp,mop,ift1,ift2,pt,mot,IDIff,
     &                        Ncsy,irej1)
C           IF (IREJ1.NE.0) GOTO 9999
               IF ( irej1.NE.0 ) THEN
                  IDIff = 0
                  NHKk = istck
               END IF
            ELSE
               IDIff = 0
            END IF
 
            IF ( IDIff.EQ.0 ) THEN
C   valence-valence chain system
               CALL DT_CHKCSY(ifp1,ift1,lchk)
               IF ( lchk ) THEN
C    baryon-baryon
                  CALL DT_EVTPUT(-21,ifp1,mop,0,pp1(1),pp1(2),pp1(3),
     &               pp1(4),0,0,8)
                  CALL DT_EVTPUT(-22,ift1,mot,0,pt1(1),pt1(2),pt1(3),
     &               pt1(4),0,0,8)
                  CALL DT_EVTPUT(-21,ifp2,mop,0,pp2(1),pp2(2),pp2(3),
     &               pp2(4),0,0,8)
                  CALL DT_EVTPUT(-22,ift2,mot,0,pt2(1),pt2(2),pt2(3),
     &               pt2(4),0,0,8)
                  ptoch = SQRT((pp1(1)+pt1(1))**2+(pp1(2)+pt1(2))
     &                    **2+(pp1(3)+pt1(3))**2)
                  ech = pp1(4) + pt1(4)
                  am1 = (ech+ptoch)*(ech-ptoch)
                  ptoch = SQRT((pp2(1)+pt2(1))**2+(pp2(2)+pt2(2))
     &                    **2+(pp2(3)+pt2(3))**2)
                  ech = pp2(4) + pt2(4)
                  am2 = (ech+ptoch)*(ech-ptoch)
               ELSE
C    antibaryon-baryon
                  CALL DT_EVTPUT(-21,ifp1,mop,0,pp1(1),pp1(2),pp1(3),
     &               pp1(4),0,0,8)
                  CALL DT_EVTPUT(-22,ift2,mot,0,pt2(1),pt2(2),pt2(3),
     &               pt2(4),0,0,8)
                  CALL DT_EVTPUT(-21,ifp2,mop,0,pp2(1),pp2(2),pp2(3),
     &               pp2(4),0,0,8)
                  CALL DT_EVTPUT(-22,ift1,mot,0,pt1(1),pt1(2),pt1(3),
     &               pt1(4),0,0,8)
                  ptoch = SQRT((pp1(1)+pt2(1))**2+(pp1(2)+pt2(2))
     &                    **2+(pp1(3)+pt2(3))**2)
                  ech = pp1(4) + pt2(4)
                  am1 = (ech+ptoch)*(ech-ptoch)
                  ptoch = SQRT((pp2(1)+pt1(1))**2+(pp2(2)+pt1(2))
     &                    **2+(pp2(3)+pt1(3))**2)
                  ech = pp2(4) + pt1(4)
                  am2 = (ech+ptoch)*(ech-ptoch)
               END IF
               IF ( (am1.GT.0.0D0) .AND. (am2.GT.0.0D0) ) THEN
                  am1 = SQRT(am1)
                  am2 = SQRT(am2)
                  IF ( (am1.LT.amvv) .OR. (am2.LT.amvv) ) THEN
                  END IF
               ELSE
 
                  IF ( LPRi.GT.4 ) WRITE (LOUt,99080) NEVhkk , i , am1 , 
     &                 am2
C                 WRITE(LOUT,5007) NEVHKK,I,AM1,AM2
99080             FORMAT (1X,'incon. chain mass VV: ',2I5,2E10.3)
               END IF
               Ncsy = Ncsy + 1
            END IF
         END IF
      END DO
      IF ( ISThkk(NPOint(2)).EQ.1 ) NPOint(2) = NPOint(2) + 1
 
C energy-momentum & flavor conservation check
      IF ( ABS(IDIff).NE.1 ) THEN
         IF ( IDIff.NE.0 ) THEN
            IF ( LEMcck ) CALL DT_EMC2(9,10,0,0,0,3,-21,-22,-41,1,0,1,3,
     &           10,Irej)
         ELSE
            IF ( LEMcck ) CALL DT_EMC2(9,10,0,0,0,3,-21,-22,-31,-32,0,1,
     &           3,10,Irej)
         END IF
         IF ( Irej.NE.0 ) THEN
            CALL DT_EVTOUT(4)
            STOP
         END IF
      END IF
 
 
C9999 CONTINUE
C     IREJ  = 1
C     RETURN
      END SUBROUTINE
