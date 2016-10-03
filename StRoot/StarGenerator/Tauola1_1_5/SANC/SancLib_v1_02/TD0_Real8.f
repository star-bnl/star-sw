      COMPLEX*16 FUNCTION TDD0(Ps1,Ps2,Ps3,Ps4,Qs,Ps,Ms1,Ms2,Ms3,Ms4)
      IMPLICIT  NONE
      INTEGER IA1,IAL
      REAL*8 PI2,PF2,S,T,RM12,RM22,RM32,RM42,D0(2)
      REAL*8 QPI,QPIS,QEPS,QDELTA
      REAL*8 Ms1,Ms2,Ms3,Ms4,Ps1,Ps2,Ps3,Ps4,Qs,Ps
      
      COMMON/TQPARAM/QPI,QPIS,QEPS,QDELTA
*-        
      IA1=2
      IAL=2
      QDELTA= 9.025809333D0 
      QEPS= 1D-90
      QPI= 3.1415926535898D0
      QPIS= QPI*QPI 
*-
      PI2=Ps1
      PI2=Ps2
      PF2=Ps3
      PF2=Ps4
      S=Qs
      T=Ps
      RM12=Ms4
      RM22=Ms1
      RM32=Ms2
      RM42=Ms3

      CALL TDFF(IA1,IAL,PI2,PF2,S,T,RM12,RM22,RM32,RM42,D0)
*-
      TDD0=DCMPLX(D0(1),D0(2))
*-
      RETURN
      END

      SUBROUTINE TDFF(IA1,IAL,PI2,PF2,S,T,RM12,RM22,RM32,RM42,D0)
*
      IMPLICIT REAL*8(A-H,O-Z)
      DIMENSION TMP0(2),UC0(2),VC0(2),D0(2)
      COMMON/TQPARAM/QPI,QPIS,QEPS,QDELTA
*
      KFLAG= 1
*
      P212= T
      P213= PF2
      P214= PF2
      P223= PI2
      P224= PI2
      P234= S
      RL12= T+RM12+RM32
      RL34= S+RM22+RM42
      PIM32= PI2+RM32
      PFM12= PF2+RM12
      RL13= PFM12+RM22
      RL14= PFM12+RM42
      RL23= PIM32+RM22
      RL24= PIM32+RM42
*
*    A2 IS FIXED TO 1
*
      A2= 1.D0
      TM= -T
      CALL TALPHA(RM12,RM32,TM,AP,AM,OMAP,OMAM)
*
      IF(IA1.EQ.1) THEN
          A1= AP*A2
      ELSE IF(IA1.EQ.2) THEN
          A1= AM*A2
      ELSE
          PRINT*,' IA1 > 2'
      ENDIF
      IF(ABS(A1).LT.1.D-10) THEN
       IF(ABS(AP).GT.ABS(AM)) THEN
        A1= AP
       ELSE
        A1= AM
       ENDIF
      ENDIF
      ANUM= RM32*A2*A2-RM12*A1*A1
      A3= ANUM/(RL23*A2-RL13*A1)
      A4= ANUM/(RL24*A2-RL14*A1)
*
*    TRASFORMED MOMENTA AND MASSES
*
      Q213= (-RM12*A1+RL13*A3)*A1-RM22*A3*A3
      Q214= (-RM12*A1+RL14*A4)*A1-RM42*A4*A4
      Q223= Q213
      Q224= Q214
      Q234= (-RM42*A4+RL34*A3)*A4-RM22*A3*A3
      CM12= RM12*A1*A1
      CM22= RM32*A2*A2
      CM32= RM22*A3*A3
      CM42= RM42*A4*A4
      RA= -Q234
      RB= -Q223
      RC= -Q224+Q223+Q234
      RD= CM32-CM42+Q234
      RE= CM22-CM32+Q224-Q234
      RK= CM12-CM22
      RAA= CM32-CM42-Q234
      RBB= CM22-CM32+Q223
      RCC= CM12-CM32+Q223
      RDD= CM12-CM32+Q224-Q234
      REE= CM32-CM42+Q223-Q224
      RFF= CM22-CM32-Q223
      RGG= CM12-CM32-Q223
      RHH= CM22-CM42+Q224
      RII= CM22-CM42+Q223-Q234
      RLL= CM22-CM42-Q224
      RMM= CM12-CM42+Q214
      RNN= CM12-CM42+Q223-Q234
      ROO= CM12-CM42-Q224
*
      DISC= (Q234-2.D0*(Q224+Q223))*Q234+(Q224-Q223)*(Q224-Q223)
*
      IF(DISC.LT.0.D0) THEN
         IF(A1.LT.0.D0.OR.A2.LT.0.D0.OR.A3.LT.0.D0.
     #      OR.A4.LT.0.D0) THEN
            PRINT*,' WHILE COMPUTING D0 ALPHA IS COMPLEX AND '
            PRINT*,' THE A_I ARE NOT ALL POSITIVE '
            STOP
         ELSE    
            U12= -RB
            U22= -RA
            US= -(RA+RB+RC)
            UM12= RA+RB+RC+RD+RE+CM42
            UM22= RA+RD+CM42
            UM32= CM42
            CALL TCFF(KFLAG,U12,U22,US,UM12,UM22,UM32,UC0,UC1,UC2)        
            V12= -RB
            V22= -RA
            VS= -(RA+RB+RC)
            VM12= RA+RB+RC+RD+RE+RK+CM42
            VM22= RA+RD+CM42
            VM32= CM42
            CALL TCFF(KFLAG,V12,V22,VS,VM12,VM22,VM32,VC0,VC1,VC2)        
            TMP0(1)= A1*A2*A3*A4/RK*(UC0(1)-VC0(1))
            TMP0(2)= A1*A2*A3*A4/RK*(UC0(2)-VC0(2))
         ENDIF
      ELSE
          CALL TALPHA(Q223,Q234,Q224,AL1,AL2,OMAL1,OMAL2)
      IF(IAL.EQ.1) THEN
          ALP= AL1
          OMALP= OMAL1
          DEN= SQRT(DISC)
      ELSE IF(IAL.EQ.2) THEN
          ALP= AL2
          OMALP= OMAL2
          DEN= -SQRT(DISC)
      ELSE
          PRINT*,'IAL > 2'
          STOP
      ENDIF
      IF(ABS(OMALP).LT.1.D-10) THEN
          OOALP= 1.D0
         DO K=1,10
             OOALP= OOALP*OMALP+1.D0
         ENDDO
          Y0I= -(RHH-RE*OMALP)/DEN
          Y1I= -(RII-RBB*OMALP)/DEN
          OMY1I= (RLL-RFF*OMALP)/DEN
          Y0II= -(RMM-RDD*OMALP)/DEN
          Y1II= -(RNN-RCC*OMALP)/DEN
          OMY1II= (ROO-RGG*OMALP)/DEN
      ELSE
          OOALP= 1.D0/ALP
          Y0I= -(RD+RE*ALP)/DEN
          Y1I= -(RAA+RBB*ALP)/DEN
          OMY1I= (REE+RFF*ALP)/DEN
          Y0II= -(RD+RDD*ALP)/DEN
          Y1II= -(RAA+RCC*ALP)/DEN
          OMY1II= (REE+RGG*ALP)/DEN
      ENDIF
      Y2I= Y0I/OMALP
      OMY2I= OMY1I/OMALP
      Y3I= -Y0I*OOALP
      OMY3I= Y1I*OOALP
      Y2II= Y0II/OMALP
      OMY2II= OMY1II/OMALP
      Y3II= -Y0II*OOALP
      OMY3II= Y1II*OOALP
      CALL TROOTS(Q223,CM32,CM22,RP1R,RP1I,RM1R,RM1I,
     #           OMRP1R,OMRM1R)
      CALL TROOTS(Q224,CM42,CM22,RP2R,RP2I,RM2R,RM2I,
     #           OMRP2R,OMRM2R)
      CALL TROOTS(Q234,CM42,CM32,RP3R,RP3I,RM3R,RM3I,
     #           OMRP3R,OMRM3R)
      CALL TROOTS(Q214,CM42,CM12,RP4R,RP4I,RM4R,RM4I,
     #           OMRP4R,OMRM4R)
      CALL TROOTS(Q213,CM32,CM12,RP5R,RP5I,RM5R,RM5I,
     #           OMRP5R,OMRM5R)
      AA1= -Q223
      AA2= -Q224
      AA3= -Q234
      AA4= -Q214
      AA5= -Q213
      YIM= 0.D0
      EB1R= -1.D0+(CM32-CM22)/Q223
      EB1I= 0.D0
      EC1I= QEPS/Q223
      EB2R= -1.D0+(CM42-CM22)/Q224
      EB2I= 0.D0
      EC2I= QEPS/Q224
      EB3R= -1.D0+(CM42-CM32)/Q234
      EB3I= 0.D0
      EC3I= QEPS/Q234
      EB4R= -1.D0+(CM42-CM12)/Q214
      EB4I= 0.D0
      EC4I= QEPS/Q214
      EB5R= -1.D0+(CM32-CM12)/Q213
      EB5I= 0.D0
      EC5I= QEPS/Q213
      CALL TS2(Y1I,YIM,OMY1I,AA1,EB1R,EB1I,EC1I,RP1R,RP1I,RM1R,RM1I,
     #        OMRP1R,OMRM1R,S2I1R,S2I1I)
      CALL TS2(Y2I,YIM,OMY2I,AA2,EB2R,EB2I,EC2I,RP2R,RP2I,RM2R,RM2I,
     #        OMRP2R,OMRM2R,S2I2R,S2I2I)
      CALL TS2(Y3I,YIM,OMY3I,AA3,EB3R,EB3I,EC3I,RP3R,RP3I,RM3R,RM3I,
     #        OMRP3R,OMRM3R,S2I3R,S2I3I)
      CALL TS2(Y1II,YIM,OMY1II,AA5,EB5R,EB5I,EC5I,RP5R,RP5I,RM5R,RM5I,
     #        OMRP5R,OMRM5R,S2II1R,S2II1I)
      CALL TS2(Y2II,YIM,OMY2II,AA4,EB4R,EB4I,EC4I,RP4R,RP4I,RM4R,RM4I,
     #        OMRP4R,OMRM4R,S2II2R,S2II2I)
      CALL TS2(Y3II,YIM,OMY3II,AA3,EB3R,EB3I,EC3I,RP3R,RP3I,RM3R,RM3I,
     #        OMRP3R,OMRM3R,S2II3R,S2II3I)
*
*    S2INFTY AND SEXTRA ARE CALLED ONLY IF SOME OF THE A'S
*    ARE NEGATIVE
*
      IF(A2*A4.LT.0.D0) THEN
          CALL TS2INFTY(Q224,CM42,CM22,Y2I,R124R,R124I)
      ELSE
          R124R= 0.D0
          R124I= 0.D0
      ENDIF
*
      IF(A3*A4.LT.0.D0) THEN
          CALL TS2INFTY(Q234,CM42,CM32,Y3I,R234R,R234I)
      ELSE
          R234R= 0.D0
          R234I= 0.D0
      ENDIF
*
      IF(A2*A3.LT.0.D0) THEN
          CALL TS2INFTY(Q223,CM32,CM22,Y1I,R323R,R323I)
      ELSE
          R323R= 0.D0
          R323I= 0.D0
      ENDIF
*
      IF(A1*A4.LT.0.D0) THEN
          CALL TS2INFTY(Q214,CM42,CM12,Y2II,R414R,R414I)
      ELSE
          R414R= 0.D0
          R414I= 0.D0
      ENDIF
*
      IF(A3*A4.LT.0.D0) THEN
          CALL TS2INFTY(Q234,CM42,CM32,Y3II,R534R,R534I)
      ELSE
          R534R= 0.D0
          R534I= 0.D0
      ENDIF
*
      IF(A1*A3.LT.0.D0) THEN
          CALL TS2INFTY(Q213,CM32,CM12,Y1II,R613R,R613I)
      ELSE
          R613R= 0.D0
          R613I= 0.D0
      ENDIF
*
      IF(A1*A2.LT.0.D0) THEN
          CALL TSEXTRA(A3,A4,Q234,Q224,Q223,SR,SI)
          ARGE= A1/A2-0.5D0*RL12/RM12
         IF(ARGE.GT.0.D0) THEN
             SIGN= +1.D0
         ELSE
             SIGN= -1.D0
         ENDIF
      ELSE
          SR= 0.D0
          SI= 0.D0
          SIGN= 1.D0
      ENDIF
*
*    SCALAR 4-POINT FUNCTION D0
*
      TMP0(1)= A1*A2*A3*A4/RK*((S2I1R-S2I2R+S2I3R-
     #         S2II1R+S2II2R-S2II3R-R124R+R234R+
     #         R323R+R414R-R534R-
     #         R613R)/DEN+SIGN*SR)
      TMP0(2)= A1*A2*A3*A4/RK*((S2I1I-S2I2I+S2I3I-
     #         S2II1I+S2II2I-S2II3I-R124I+R234I+
     #         R323I+R414I-R534I-
     #         R613I)/DEN+SIGN*SI)
      ENDIF
*
      D0(1)= TMP0(1)
      D0(2)= TMP0(2)

      RETURN
      END
*-
*-
*-----CFF-------------------------------------------------------
*     COMPUTES THE ONE-LOOP THREE-POINT FORM FACTORS/I*PI^2   
*     C0,C11,C12,C21,C22,C23,C24                              
*     ACCORDING TO THE CONVENTION                             
*                                                              
*     (Q^2+M1^2)((Q+P1)^2+M2^2)((Q+P1+P2)^2+M3^2)             
*                                                             
*     INPUT PARAMETERS ARE  P1^2,P2^2, S=P5^2=(P1+P2)^2       
*                           M1^2,M2^2,M3^2                    
*                                                              
*     JFLAG = 1,2 SELECTS ONE OF THE ROOTS FOR ALPHA          
*     ! NOW ALPHA CAN BE COMPLEX                              
*
      SUBROUTINE TCFF(JFLAG,P12,P22,S,RM12,RM22,RM32,C0,UC01,UC02)
      IMPLICIT REAL*8(A-H,O-Z)
*
      COMMON/TQPARAM/QPI,QPIS,QEPS,QDELTA
*
      DIMENSION C0(2)!,C1(2,2),C2(2,4)
*      DIMENSION B0_12(2),B1_12(2),B21_12(2)
*      DIMENSION B0_13(2),B1_13(2),B21_13(2)
*      DIMENSION B0_23(2),B1_23(2),B21_23(2)
*      DIMENSION R2_32(2,2),TMP2_32(2,2),TMP0(2),TMP24(2)
      DIMENSION TMP0(2)
*      DIMENSION XMI(2,2),R1(2,2),TMP1(2,2),R2_13(2,2),TMP2_13(2,2)
*
      P1DP2= 0.5D0*(S-P12-P22)
      DEN= -S*S/4.D0*(1.D0+((P12-P22)*(P12-P22)/S-
     #     2.D0*(P12+P22))/S)
      DM12= RM12-RM22
      DM13= RM12-RM32
      DM23= RM22-RM32
      RA= -P22
      RB= -P12
      RC= -2.D0*P1DP2
      RD= DM23+P22
      RE= DM12+P12+2.D0*P1DP2
      RAA= -P22+DM23
      RBB= P12+DM12
      RCC= -2.D0*P1DP2-P22+DM23
      RDD= -P12+DM12
      DISCP= +2.D0*SQRT(ABS(DEN))
      DISCM= -DISCP
      IF(DEN.LT.0.D0) THEN
         CALL TALPHA(P12,P22,S,AP,AM,OMAP,OMAM)
         IF(JFLAG.EQ.1) THEN
            ALP= AP
            OMALP= OMAP
            DISC= DISCP
         ELSE IF(JFLAG.EQ.2) THEN
            ALP= AM
            OMALP= OMAM
            DISC= DISCM
         ELSE
            PRINT*,'JFLAG > 2'
            STOP
         ENDIF
         ALPI= 0.D0
         Y0R= -(RD+RE*ALP)/DISC
         Y0I= 0.D0
         Y1R= -(RAA+RBB*ALP)/DISC
         Y1I= Y0I
         OMY1R= (RCC+RDD*ALP)/DISC
         Y2R= Y0R/OMALP
         Y2I= Y0I/OMALP
         OMY2R= OMY1R/OMALP
         Y3R= -Y0R/ALP
         Y3I= -Y0I/ALP
         OMY3R= Y1R/ALP
      ELSE IF(DEN.GT.0) THEN
         ALP= (P12+P22-S)/2.D0/P12
         OMALP= (P12-P22+S)/2.D0/P12
         CALPI= SQRT(DEN)/P12        
         IF(JFLAG.EQ.1) THEN
            ALPI= -CALPI
            DISC= DISCP
         ELSE IF(JFLAG.EQ.2) THEN
            ALPI=  CALPI
            DISC= DISCM
         ELSE
             PRINT*,'JFLAG > 2'
             STOP
         ENDIF
         Y0R= -RE*ALPI/DISC
         Y0I= (RD+RE*ALP)/DISC
         Y1R= -RBB*ALPI/DISC
         Y1I= (RAA+RBB*ALP)/DISC         
         OMY1R= RDD*ALPI/DISC
         OMALPM2= OMALP*OMALP+ALPI*ALPI
         Y2R= (Y0R*OMALP-Y0I*ALPI)/OMALPM2
         Y2I= (Y0R*ALPI+Y0I*OMALP)/OMALPM2
         OMY2R= (OMY1R*OMALP+Y1I*ALPI)/OMALPM2
         ALPM2= ALP*ALP+ALPI*ALPI
         Y3R= -(Y0R*ALP+Y0I*ALPI)/ALPM2
         Y3I= -(-Y0R*ALPI+Y0I*ALP)/ALPM2
         OMY3R= (Y1R*ALP+Y1I*ALPI)/ALPM2
      ENDIF       
*
      CALL TROOTS(P12,RM22,RM12,RP1R,RP1I,RM1R,RM1I,OMRP1R,OMRM1R)
      CALL TROOTS(S,RM32,RM12,RP2R,RP2I,RM2R,RM2I,OMRP2R,OMRM2R)
      CALL TROOTS(P22,RM32,RM22,RP3R,RP3I,RM3R,RM3I,OMRP3R,OMRM3R)
*
      A1= -P12
      A2= -S
      A3= -P22
      EB1R= -1.D0-DM12/P12
      EB1I= 0.D0
      EC1I= QEPS/P12
      EB2R= -1.D0-DM13/S
      EB2I= 0.D0
      EC2I= QEPS/S
      EB3R= -1.D0-DM23/P22
      EB3I= 0.D0
      EC3I= QEPS/P22
      CALL TS2(Y1R,Y1I,OMY1R,A1,EB1R,EB1I,EC1I,RP1R,RP1I,RM1R,RM1I,
     #        OMRP1R,OMRM1R,S21R,S21I)
      CALL TS2(Y2R,Y2I,OMY2R,A2,EB2R,EB2I,EC2I,RP2R,RP2I,RM2R,RM2I,
     #        OMRP2R,OMRM2R,S22R,S22I)
      CALL TS2(Y3R,Y3I,OMY3R,A3,EB3R,EB3I,EC3I,RP3R,RP3I,RM3R,RM3I,
     #        OMRP3R,OMRM3R,S23R,S23I)
*
*     SCALAR 3-POINT FUNCTION C0
*
      IF(DEN.LT.0.D0) THEN
         TMP0(1)= (S21R-S22R+S23R)/DISC
         TMP0(2)= (S21I-S22I+S23I)/DISC
      ELSE IF(DEN.GT.0.D0) THEN
         TMP0(1)=  (S21I-S22I+S23I)/DISC
         TMP0(2)= -(S21R-S22R+S23R)/DISC
      ENDIF

*
      DO I= 1,2
          C0(I)= TMP0(I)
      ENDDO
*
      RETURN
      END
*
*-
*-
      SUBROUTINE TALPHA(P12,P22,S,ALP,ALM,OMALP,OMALM)
      IMPLICIT REAL*8(A-H,O-Z)
*
      ARG= (P22-2.D0*(P12+S))*P22+(S-P12)*(S-P12)
      RT= SQRT(ARG)
      S1= ABS(S/P12)
      S2= ABS(S/P22)
*
*     ONE OF THE ROOTS VERY NEAR TO 1    
*
      IF(S1.LT.1.D-10.OR.S2.LT.1.D-10) THEN
          B= P12-P22+S
          IF(B.GT.0.D0) THEN
              Q= -0.5D0*(B+RT)
              BEP= -S/Q
              BEM= -Q/P12
          ELSE 
              Q= -0.5D0*(B-RT)
              BEP= -Q/P12
              BEM= -S/Q
          ENDIF
          ALP= 1.D0-BEM
          ALM= 1.D0-BEP
          OMALP= BEM
          OMALM= BEP
          RETURN      
      ELSE
          B= P12+P22-S
          IF(B.GT.0.D0) THEN
              Q= -0.5D0*(B+RT)
              ALP= -P22/Q
              ALM= -Q/P12
          ELSE 
              Q= -0.5D0*(B-RT)
              ALP= -Q/P12
              ALM= -P22/Q
          ENDIF
          OMALP= 1.D0-ALP
          OMALM= 1.D0-ALM
          RETURN
      ENDIF
      END
*-
*-

      SUBROUTINE TS2(Y0R,Y0I,OMY0R,A,EBR,EBI,ECI,RPR,RPI,RMR,RMI,
     #               OMRPR,OMRMR,S2R,S2I)
      IMPLICIT REAL*8(A-H,O-Z)
*
      COMMON/TQPARAM/QPI,QPIS,QEPS,QDELTA
      DIMENSION CARG(2),COMARG(2),CLN(2)
*
*     RP AND RM ARE THE ROOTS OF A*X^2+B*X+C = 0
*     EB= B/A AND EC= C/A ARE KEPT TO COMPUTE IM(RP*RM)= IM(EC)
*     AND IM{(Y0-RP)(Y0-RM)}= IM{Y0^2+EB*Y0+EC}
*
      A1= A*ECI
      A2= A*(2.D0*Y0R*Y0I+EBR*Y0I+EBI*Y0R+ECI)
      IF(A1.LT.0.D0) THEN
          DEL= 1.D-20
      ELSE
          DEL= -1.D-20
      ENDIF      
      IF(A2.LT.0.D0) THEN
          DELP= 1.D-20
      ELSE
          DELP= -1.D-20
      ENDIF
*
*     COMPUTES THE LOG WHICH OCCURS IN ASSOCIATION WITH ETA-FUNCTIONS
*
      Y0M2= Y0R*Y0R+Y0I*Y0I
      CARG(1)= Y0R/Y0M2
      CARG(2)= -Y0I/Y0M2
      COMARG(1)= -(OMY0R*Y0R-Y0I*Y0I)/Y0M2
      COMARG(2)= Y0I/Y0M2
      CALL TCQLNOMX(CARG,COMARG,CLN)
*
*     IN CALLING ETA ONLY THE SIGN OF THE ARGUMENTS IS RELEVANT
*
      A11= -RPI
      A12= -RMI
      A1P= ECI
      A21= Y0I-RPI
      A22= Y0I-RMI
      A2P= 2.D0*Y0R*Y0I+EBR*Y0I+EBI*Y0R+ECI
      A31= -DEL
      A32= DELP
      A3P= A*(DELP-DEL)
      CALL TRLOG(Y0R,Y0I,OMY0R,RPR,RPI,OMRPR,RFPR,RFPI)
      CALL TRLOG(Y0R,Y0I,OMY0R,RMR,RMI,OMRMR,RFMR,RFMI)
      ETA1= ETA(A11,A12,A1P)
      ETA2= ETA(A21,A22,A2P)
      ETA3= ETA(A31,A32,A3P)
      S2R= RFPR+RFMR-CLN(2)*(ETA1-ETA2-ETA3)     
      S2I= RFPI+RFMI+CLN(1)*(ETA1-ETA2-ETA3)     
      RETURN
      END
*-
*-
      SUBROUTINE TS2INFTY(Q2,RMI2,RMJ2,YK,S2R,S2I)
      IMPLICIT REAL*8(A-H,O-Z)
*
      COMMON/TQPARAM/QPI,QPIS,QEPS,QDELTA
*
      DIMENSION ARGP(2),ARGM(2),CLNP(2),CLNM(2)
*
      CALL TROOTS(Q2,RMI2,RMJ2,Y1R,Y1I,Y2R,Y2I,OMY1R,OMY2R)
      IF(Y1I.GT.0.D0) THEN
          YPR= Y1R
          YPI= Y1I
          YMR= Y2R
          YMI= Y2I
      ELSE IF(Y2I.GT.0.D0) THEN
          YPR= Y2R
          YPI= Y2I
          YMR= Y1R
          YMI= Y1I
      ENDIF
      ARGP(1)= YK-YPR
      ARGP(2)= -YPI
      ARGM(1)= YK-YMR
      ARGM(2)= -YMI
      CALL TCQLNX(ARGM,CLNM)
      CALL TCQLNX(ARGP,CLNP)
      S2R= -QPIS+QPI*(CLNM(2)-CLNP(2))
      S2I= -QPI*(CLNM(1)-CLNP(1))
      RETURN
      END
*-
*-
      SUBROUTINE TSEXTRA(A3,A4,Q234,Q224,Q223,SR,SI)
      IMPLICIT REAL*8(A-H,O-Z)
*
      COMMON/TQPARAM/QPI,QPIS,QEPS,QDELTA
*
      DIMENSION OOX1(2),OOX2(2),OOY1(2),OOY2(2),OOZ1(2),OOZ2(2)
      DIMENSION OMOOX1(2),OMOOX2(2),OMOOY1(2),OMOOY2(2),OMOOZ1(2),
     #          OMOOZ2(2)
      DIMENSION CLNX1(2),CLNX2(2),CLNY1(2),CLNY2(2),CLNZ1(2),CLNZ2(2)
*
      AM12= -Q224
      AM22= -Q223
      CALL TROOTS(Q234,AM12,AM22,X1R,X1I,X2R,X2I,OMX1R,OMX2R)
      AM12= -Q234
      AM22= -Q223
      CALL TROOTS(Q224,AM12,AM22,Y1R,Y1I,Y2R,Y2I,OMY1R,OMY2R)
      AM12= -Q234
      AM22= -Q224
      CALL TROOTS(Q223,AM12,AM22,Z1R,Z1I,Z2R,Z2I,OMZ1R,OMZ2R)
      DX1= X1R*X1R+X1I*X1I
      DX2= X2R*X2R+X2I*X2I
      DY1= Y1R*Y1R+Y1I*Y1I
      DY2= Y2R*Y2R+Y2I*Y2I
      DZ1= Z1R*Z1R+Z1I*Z1I      
      DZ2= Z2R*Z2R+Z2I*Z2I
      OOX1(1)= X1R/DX1
      OOX1(2)= -X1I/DX1
      OOX2(1)= X2R/DX2
      OOX2(2)= -X2I/DX2
      OOY1(1)= Y1R/DY1
      OOY1(2)= -Y1I/DY1
      OOY2(1)= Y2R/DY2
      OOY2(2)= -Y2I/DY2
      OOZ1(1)= Z1R/DZ1
      OOZ1(2)= -Z1I/DZ1
      OOZ2(1)= Z2R/DZ2
      OOZ2(2)= -Z2I/DZ2
      OMOOX1(1)= 1.D0-X1R/DX1
      OMOOX1(2)= X1I/DX1
      OMOOX2(1)= 1.D0-X2R/DX2
      OMOOX2(2)= X2I/DX2
      OMOOY1(1)= 1.D0-Y1R/DY1
      OMOOY1(2)= Y1I/DY1
      OMOOY2(1)= 1.D0-Y2R/DY2
      OMOOY2(2)= Y2I/DY2
      OMOOZ1(1)= 1.D0-Z1R/DZ1
      OMOOZ1(2)= Z1I/DZ1
      OMOOZ2(1)= 1.D0-Z2R/DZ2
      OMOOZ2(2)= Z2I/DZ2
      CALL TCQLNOMX(OOX1,OMOOX1,CLNX1)
      CALL TCQLNOMX(OOX2,OMOOX2,CLNX2)
      CALL TCQLNOMX(OOY1,OMOOY1,CLNY1)
      CALL TCQLNOMX(OOY2,OMOOY2,CLNY2)
      CALL TCQLNOMX(OOZ1,OMOOZ1,CLNZ1)
      CALL TCQLNOMX(OOZ2,OMOOZ2,CLNZ2)
      DENX= (X1R-X2R)**2+(X1I-X2I)**2
      DENY= (Y1R-Y2R)**2+(Y1I-Y2I)**2
      DENZ= (Z1R-Z2R)**2+(Z1I-Z2I)**2
      CLNXR= CLNX1(1)-CLNX2(1)
      CLNXI= CLNX1(2)-CLNX2(2)
      CLNYR= CLNY1(1)-CLNY2(1)
      CLNYI= CLNY1(2)-CLNY2(2)
      CLNZR= CLNZ1(1)-CLNZ2(1)
      CLNZI= CLNZ1(2)-CLNZ2(2)
      XR= X1R-X2R
      XI= X1I-X2I
      YR= Y1R-Y2R
      YI= Y1I-Y2I
      ZR= Z1R-Z2R
      ZI= Z1I-Z2I
      CF= QPI/Q234
      S234R= CF/DENX*(XR*CLNXI-XI*CLNXR)
      S234I= -CF/DENX*(XR*CLNXR+XI*CLNXI)
      S324R= CF/DENY*(YR*CLNYI-YI*CLNYR)
      S324I= -CF/DENY*(YR*CLNYR+YI*CLNYI)
      S423R= CF/DENZ*(ZR*CLNZI-ZI*CLNZR)
      S423I= -CF/DENZ*(ZR*CLNZR+ZI*CLNZI)
      ATEST= A3*A4
      IF(ATEST.GT.0.D0) THEN
          SR= S234R
          SI= S234I
          RETURN
      ELSE
          SR= -S324R-S423R
          SI= -S324I-S423I
          RETURN
      ENDIF
*
      END
*-
*-
      SUBROUTINE TSPENCE(XR,XI,OMXR,CLI2R,CLI2I)
      IMPLICIT REAL*8(A-H,O-Z)
*
      COMMON/TQPARAM/QPI,QPIS,QEPS,QDELTA
*
      DIMENSION B(0:14),BF(0:14)
      DIMENSION CLNX(2),CLNOMX(2),CLNOY(2),CLNZ(2),CLNOMZ(2)
      DIMENSION ADD1(2),ADD2(2),ADD3(2),PAR(2),RES(2),CT(15),SN(15)
      DIMENSION X(2),OMX(2),Y(2),OY(2),OMY(2),Z(2),OMZ(2),T(2),OMT(2)
*
      X(1)= XR
      X(2)= XI
      OMX(1)= OMXR
      OMX(2)= -XI
      IF(XR.LT.0.D0) THEN
          Y(1)= OMXR
          Y(2)= -XI
          SIGN1= -1.D0
          CALL TCQLNX(X,CLNX)
          CALL TCQLNOMX(X,OMX,CLNOMX)
          ADD1(1)= QPIS/6.D0-CLNX(1)*CLNOMX(1)+CLNX(2)*CLNOMX(2)
          ADD1(2)= -CLNX(1)*CLNOMX(2)-CLNX(2)*CLNOMX(1)
      ELSE
          Y(1)= X(1)
          Y(2)= X(2)
          SIGN1= 1.D0
          ADD1(1)= 0.D0
          ADD1(2)= 0.D0
      ENDIF
      OMY(1)= 1.D0-Y(1)
      OMY(2)= -Y(2)
      YM2= Y(1)*Y(1)+Y(2)*Y(2)
      YM= SQRT(YM2)
      IF(YM.GT.1.D0) THEN
          Z(1)= Y(1)/YM2
          Z(2)= -Y(2)/YM2
          SIGN2= -1.D0
          OY(1)= -Y(1)
          OY(2)= -Y(2)
          CALL TCQLNX(OY,CLNOY)
          ADD2(1)= -QPIS/6.D0-0.5D0*((CLNOY(1))**2-(CLNOY(2))**2)
          ADD2(2)= -CLNOY(1)*CLNOY(2)
      ELSE
          Z(1)= Y(1)
          Z(2)= Y(2)
          SIGN2= 1.D0
          ADD2(1)= 0.D0
          ADD2(2)= 0.D0
      ENDIF
      OMZ(1)= 1.D0-Z(1)
      OMZ(2)= -Z(2)
      ZR= Z(1)
      IF(ZR.GT.0.5D0) THEN
          T(1)= 1.D0-Z(1)
          T(2)= -Z(2)
          OMT(1)= 1.D0-T(1)
          OMT(2)= -T(2)
          SIGN3= -1.D0
          CALL TCQLNX(Z,CLNZ)
          CALL TCQLNOMX(Z,OMZ,CLNOMZ)
          ADD3(1)= QPIS/6.D0-CLNZ(1)*CLNOMZ(1)+CLNZ(2)*CLNOMZ(2)
          ADD3(2)= -CLNZ(1)*CLNOMZ(2)-CLNZ(2)*CLNOMZ(1)
      ELSE
          T(1)= Z(1)
          T(2)= Z(2)
          OMT(1)= 1.D0-T(1)
          OMT(2)= -T(2)
          SIGN3= 1.D0
          ADD3(1)= 0.D0
          ADD3(2)= 0.D0
      ENDIF
      CALL TCQLNOMX(T,OMT,PAR)
      B(0)= 1.D0
      B(1)= -1.D0/2.D0
      B(2)= 1.D0/6.D0
      B(4)= -1.D0/30.D0
      B(6)= 1.D0/42.D0
      B(8)= -1.D0/30.D0
      B(10)= 5.D0/66.D0
      B(12)= -691.D0/2730.D0
      B(14)= 7.D0/6.D0
      FACT= 1.D0
      DO N=0,14
          BF(N)= B(N)/FACT
          FACT= FACT*(N+2.D0)
      ENDDO
      PARR= PAR(1)
      PARI= PAR(2)
      PARM2= PARR*PARR+PARI*PARI
      PARM= SQRT(PARM2)
      CT(1)= PARR/PARM
      SN(1)= PARI/PARM
      DO N=2,15
          CT(N)= CT(1)*CT(N-1)-SN(1)*SN(N-1)
          SN(N)= SN(1)*CT(N-1)+CT(1)*SN(N-1)
      ENDDO
*      
      RES(1)= -((((((((BF(14)*CT(15)*PARM2+BF(12)*CT(13))*PARM2+
     #                 BF(10)*CT(11))*PARM2+BF(8)*CT(9))*PARM2+
     #                 BF(6)*CT(7))*PARM2+BF(4)*CT(5))*PARM2+
     #                 BF(2)*CT(3))*(-PARM)+BF(1)*CT(2))*(-PARM)+
     #                 BF(0)*CT(1))*PARM 
      RES(2)= -((((((((BF(14)*SN(15)*PARM2+BF(12)*SN(13))*PARM2+
     #                 BF(10)*SN(11))*PARM2+BF(8)*SN(9))*PARM2+
     #                 BF(6)*SN(7))*PARM2+BF(4)*SN(5))*PARM2+
     #                 BF(2)*SN(3))*(-PARM)+BF(1)*SN(2))*(-PARM)+
     #                 BF(0)*SN(1))*PARM 
      CLI2R= SIGN1*(SIGN2*(SIGN3*RES(1)+ADD3(1))+ADD2(1))+ADD1(1)
      CLI2I= SIGN1*(SIGN2*(SIGN3*RES(2)+ADD3(2))+ADD2(2))+ADD1(2)
*
      RETURN
      END
*-
*-
*-----CQLNOMX---------------------------------------
*     COMPUTES LN(1-X)                 
*     USUALLY |X| << 1                 
*
      SUBROUTINE TCQLNOMX(ARG,OMARG,RES)
      IMPLICIT REAL*8(A-H,O-Z)
      DIMENSION ARG(2),OMARG(2),RES(2),ARES(2),CT(10),SN(10)
*
      ZR= ARG(1)
      ZI= ARG(2)
      ZM2= ZR*ZR+ZI*ZI
      ZM= SQRT(ZM2)
      IF(ZM.LT.1.D-7) THEN
          CT(1)= ZR/ZM
          SN(1)= ZI/ZM
          DO N=2,10
              CT(N)= CT(1)*CT(N-1)-SN(1)*SN(N-1)
              SN(N)= SN(1)*CT(N-1)+CT(1)*SN(N-1)
          ENDDO
          ARES(1)= CT(10)/10.D0
          ARES(2)= SN(10)/10.D0
          DO K=9,1,-1
              ARES(1)= ARES(1)*ZM+CT(K)/K
              ARES(2)= ARES(2)*ZM+SN(K)/K
          ENDDO
          ARES(1)= -ARES(1)*ZM
          ARES(2)= -ARES(2)*ZM
      ELSE
          CALL TCQLNX(OMARG,ARES)
      ENDIF
          DO I= 1,2
              RES(I)= ARES(I)
          ENDDO
      RETURN
      END
*-
*-
      SUBROUTINE TRLOG(Z0R,Z0I,OMZ0R,Z1R,Z1I,OMZ1R,RFUNR,RFUNI)
      IMPLICIT REAL*8(A-H,O-Z)
*
      COMMON/TQPARAM/QPI,QPIS,QEPS,QDELTA
*
      DIMENSION CA1(2),CA2(2),CA3(2),ADD(2)
      DIMENSION CLN1(2),CLN2(2),CLN3(2),CT(10),SN(10)
*
      Z1M2= Z1R*Z1R+Z1I*Z1I
      Z1M= SQRT(Z1M2)
      Z0M2= Z0R*Z0R+Z0I*Z0I
      Z0M= SQRT(Z0M2)
      ZRT= Z1M/Z0M
*
*     |Z1| << |Z0|
*
      IF(ZRT.LT.1.D-10) THEN
          CT(1)= (Z1R*Z0R+Z1I*Z0I)/Z1M/Z0M
          SN(1)= (Z0R*Z1I-Z1R*Z0I)/Z1M/Z0M
          DO K=2,10
              CT(K)= CT(1)*CT(K-1)-SN(1)*SN(K-1)
              SN(K)= SN(1)*CT(K-1)+CT(1)*SN(K-1)
          ENDDO
          SUMR= CT(10)
          SUMI= SN(10)
          DO J=9,1,-1
              SUMR= SUMR*ZRT+CT(J)
              SUMI= SUMI*ZRT+SN(J)
          ENDDO      
          SUMR= SUMR*ZRT
          SUMI= SUMI*ZRT
          A1R= 1.D0+SUMR
          A1I= SUMI
          A2R= (Z0I*Z0I-OMZ0R*Z0R)/Z0M2*A1R-Z0I/Z0M2*A1I
          A2I= (Z0I*Z0I-OMZ0R*Z0R)/Z0M2*A1I+Z0I/Z0M2*A1R
          OMA1R= -SUMR
          OMA2R= 1.D0-A2R
          Z01R= Z0R-Z1R
          Z01I= Z0I-Z1I
          DEN= Z01R*Z01R+Z01I*Z01I
          ADD(1)= 0.D0
          ADD(2)= 0.D0
          SIGN= +1.D0
      ELSE
*
*     IF Z0R AND/OR Z1R ARE ROOTS VERY NEAR TO 1 
*     Z0R-Z1R = (1-Z1R)-(1-Z0R)
*
          AOMZ0R= ABS(OMZ0R)
          AOMZ1R= ABS(OMZ1R)
          AZ1I= ABS(Z1I)
          IF(AOMZ0R.LT.1.D-10.OR.AOMZ1R.LT.1.D-10) THEN
              Z01R= OMZ1R-OMZ0R
          ELSE
              Z01R= Z0R-Z1R
          ENDIF
          Z01I= Z0I-Z1I
*         
*     IF Z0 AND Z1R ARE ROOTS VERY NEAR TO 1 
*     AND |Z1I| << 1 , Z0I = 0
*
          IF(AOMZ0R.LT.1.D-10.AND.AOMZ1R.LT.1.D-10.
     #        AND.AZ1I.LT.1.D-10.AND.Z0I.EQ.0.D0) THEN
              DEN= Z01R*Z01R+Z1I*Z1I
              A1R= Z01R/Z0R
              A1I= -Z1I/Z0R
              OMA1R= Z1R/Z0R
              CA3(1)= -A1R
              CA3(2)= -A1I
              CALL TCQLNX(CA3,CLN3)
              ADD(1)= -QPIS/6.D0-0.5D0*(CLN3(1)*CLN3(1)-CLN3(2)*CLN3(2))
              ADD(2)= -CLN3(1)*CLN3(2)      
              SIGN= -1.D0
          ELSE      
              DEN= Z01R*Z01R+Z01I*Z01I
              A1R= (Z0R*Z01R+Z0I*Z01I)/DEN
              A1I= (-Z0R*Z01I+Z0I*Z01R)/DEN
              OMA1R= -(Z01I*Z1I+Z1R*Z01R)/DEN
              ADD(1)= 0.D0
              ADD(2)= 0.D0      
              SIGN= +1.D0
          ENDIF
          A2R= -(OMZ0R*Z01R-Z0I*Z01I)/DEN
          A2I= (OMZ0R*Z01I+Z0I*Z01R)/DEN
          OMA2R= (OMZ1R*Z01R-Z01I*Z1I)/DEN
      ENDIF
*
*     IN CALLING ETA ONLY THE SIGN OF THE ARGUMENTS IS RELEVANT
*
      A1= -Z1I
      A2= -Z01I
      API= Z1R*Z0I-Z0R*Z1I
      APII= -Z0I*OMZ1R+Z1I*OMZ0R
      CALL TSPENCE(A1R,A1I,OMA1R,SP1R,SP1I)
      CALL TSPENCE(A2R,A2I,OMA2R,SP2R,SP2I)
      CA1(1)= A1R
      CA1(2)= A1I
      CA2(1)= A2R
      CA2(2)= A2I
      CALL TCQLNX(CA1,CLN1)
      CALL TCQLNX(CA2,CLN2)
      ETAI= ETA(A1,A2,API)
      ETAII= ETA(A1,A2,APII)
      RFUNR= SIGN*SP1R-SP2R-ETAI*CLN1(2)+ETAII*CLN2(2)+ADD(1)
      RFUNI= SIGN*SP1I-SP2I+ETAI*CLN1(1)-ETAII*CLN2(1)+ADD(2)
      RETURN
      END      
*-
*-
*-----CQLNX---------------------------------------------
*     COMPUTES  LN(Z)                              
*
      SUBROUTINE TCQLNX(ARG,RES)
      IMPLICIT REAL*8(A-H,O-Z)
*
      DIMENSION ARG(2),AARG(2),RES(2)
*
      QPI= 3.141592653589793238462643D0
      DO I= 1,2
          AARG(I)= ABS(ARG(I))
      ENDDO
      ZM2= (ARG(1))**2+(ARG(2))**2
      ZM= SQRT(ZM2)
      RES(1)= LOG(ZM)
      IF(ARG(1).EQ.0.D0) THEN
          IF(ARG(2).GT.0.D0) THEN
              TETA= QPI/2.D0
          ELSE
              TETA= -QPI/2.D0
          ENDIF
          RES(2)= TETA
          RETURN
      ELSE IF(ARG(2).EQ.0.D0) THEN 
               IF(ARG(1).GT.0.D0) THEN
                   TETA= 0.D0
               ELSE
                   TETA= QPI
               ENDIF
          RES(2)= TETA
          RETURN
      ELSE
          TNTETA= AARG(2)/AARG(1)
          TETA= ATAN(TNTETA)
          SR= ARG(1)/AARG(1)
          SI= ARG(2)/AARG(2)
          IF(SR.GT.0.D0) THEN
              RES(2)= SI*TETA
          ELSE
              RES(2)= SI*(QPI-TETA)
          ENDIF
          RETURN
      ENDIF
      END
*-
*-
*-----ROOTS--------------------------------------------
*     COMPUTES THE ROOTS OF THE QUADRATIC FORM    
*     -P2*X^2+(P2+M2^2-M1^2)*X+M1^2 = 0           
*     WITH:                                       
*     REAL MASSES  M^2=M^2-I*EPS                  
*
      SUBROUTINE TROOTS(P2,RM12,RM22,RPR,RPI,RMR,RMI,OMRPR,OMRMR)
      IMPLICIT REAL*8(A-H,O-Z)
*
      COMMON/TQPARAM/QPI,QPIS,QEPS,QDELTA
*      
      IF(RM12.EQ.RM22) THEN
          DISC2= (P2+4.D0*RM12)*P2
      ELSE
          DISC2= (RM22+2.D0*(P2-RM12))*RM22+(P2+RM12)*(P2+RM12)
      ENDIF   
      IF(DISC2.GT.0.D0) THEN
          DISC= SQRT(DISC2)
          RPI= +QEPS/DISC
          RMI= -QEPS/DISC
          R1= 1.D-10*ABS(P2)
          R2= 1.D-10*RM12
*
*     ONE OF THE ROOTS VERY NEAR TO 1
*
          IF(RM22.LT.R1.OR.RM22.LT.R2) THEN
              A= -P2
              B= P2+RM12-RM22
              C= RM22
              IF(B.GT.0.D0) THEN
                  Q= -0.5D0*(B+DISC)
                  RMR= 1.D0-C/Q
                  RPR= 1.D0-Q/A
                  OMRPR= Q/A
                  OMRMR= C/Q
              ELSE 
                  Q= -0.5D0*(B-DISC)
                  RMR= 1.D0-Q/A
                  RPR= 1.D0-C/Q
                  OMRPR= C/Q
                  OMRMR= Q/A
              ENDIF
          ELSE
              A= -P2
              B= P2+RM22-RM12
              C= RM12
              IF(B.GT.0.D0) THEN
                  Q= -0.5D0*(B+DISC)
                  RPR= C/Q
                  RMR= Q/A
                  OMRPR= 1.D0-RPR
                  OMRMR= 1.D0-RMR
              ELSE 
                  Q= -0.5D0*(B-DISC)
                  RPR= Q/A
                  RMR= C/Q
                  OMRPR= 1.D0-RPR
                  OMRMR= 1.D0-RMR
              ENDIF
          ENDIF
      ELSE
          DISC= SQRT(-DISC2)
          A= -P2
          B= P2+RM22-RM12
          RPR= -B/2.D0/A
          RMR= -B/2.D0/A
          OMRPR= 1.D0-RPR
          OMRMR= 1.D0-RMR
          RPI= +DISC/2.D0/A
          RMI= -DISC/2.D0/A
      ENDIF
      RETURN
      END
*-
*-
*-----ETA-----------------------------------------------------
*     COMPUTES THE FUNTION  ETA(A,B)                       
*     LN(AB)= LN(A)+LN(B)+ETA(A,B)                         
*
      REAL*8 FUNCTION ETA(Z1I,Z2I,ZPI)
      IMPLICIT REAL*8(A-H,O-Z)
*
      COMMON/TQPARAM/QPI,QPIS,QEPS,QDELTA
*
*     ONLY THE SIGN OF THE ARGUMENTS IS RELEVANT
*
      IF(Z1I.LT.0.D0.AND.Z2I.LT.0.D0.AND.ZPI.GT.0.D0) THEN
          ETA= 2.D0*QPI
          RETURN
      ELSE IF(Z1I.GT.0.D0.AND.Z2I.GT.0.D0.AND.ZPI.LT.0.D0) THEN
               ETA= -2.D0*QPI
               RETURN
      ELSE
          ETA= 0.D0
          RETURN
      ENDIF
      END
*
*-----ETAQ-----------------------------------------------------
*     COMPUTES THE FUNTION  ETA(A,1/B)                     
*     LN(A/B)= LN(A)-LN(B)+ETA(A,1/B)                      
*
      REAL*8 FUNCTION ETAQ(Z1I,Z2I,ZQI)
      IMPLICIT REAL*8(A-H,O-Z)
      COMMON/TQPARAM/QPI,QPIS,QEPS,QDELTA
*
      IF(Z1I.LT.0.D0.AND.Z2I.GT.0.D0.AND.ZQI.GT.0.D0) THEN
          ETAQ= 2.D0*QPI
          RETURN
      ELSE IF(Z1I.GT.0.D0.AND.Z2I.LT.0.D0.AND.ZQI.LT.0.D0) THEN
               ETAQ= -2.D0*QPI
               RETURN
      ELSE
          ETAQ= 0.D0
          RETURN
      ENDIF
      END

