#define RLU(x) RLUSTAR(x)
C ********************************************************************

      SUBROUTINE LSMALL
C--                                                                  --C
C--   Created:     951031                                            --C
C--   Last update: 980722                                            --C
C--   Purpose:     Take care of small mass systems with one diquark, --C  
C--                charmonium or bottonium                           --C

      IMPLICIT NONE

C--       arg for spec. version
      INTEGER ILON
      REAL WEIGHT

C--       global variables
      INTEGER  NLUPDM,NPLBUF
      PARAMETER  (NLUPDM=4000,NPLBUF=5)
      COMMON/LUJETS/N,K(NLUPDM,5),P(NLUPDM,NPLBUF),V(NLUPDM,5)
      INTEGER  N,K
      REAL  P,V
      SAVE /LUJETS/

*
* to avoid variable conflictions, a second keep element is necessary
* with the same common block name (see LPTOU2)
*
      COMMON /LEPTOU/ CUT(14),LST(40),PARL(30),
     &                X,Y,W2,Q2,U
      REAL CUT,PARL,X,Y,W2,Q2,U
      INTEGER LST
      SAVE /LEPTOU/

      COMMON/LUDAT1/MSTU(200),PARU(200),MSTJ(200),PARJ(200)
      INTEGER  MSTU,MSTJ
      REAL  PARU,PARJ
      SAVE /LUDAT1/

      COMMON/LUDAT2/KCHG(500,3),PMAS(500,4),PARF(2000),VCKM(4,4)
      INTEGER  KCHG
      REAL  PMAS,PARF,VCKM
      SAVE /LUDAT2/


C--       functions
      REAL RLU,ULMASS,RLUSTAR
      INTEGER LUCOMP
C--       local variables
      INTEGER I,J,IFIRST,I1,I2,IAQ,IQ,NTRY1,NTRY2,KFB,KFH1,KFH2
      INTEGER KIAQ,KIQ,KDUMMY,NPARTON
      REAL*8 ENERGY,MAXENERGY,INVMASS,THRESHOLD
      REAL*8 PSUM(5)
      REAL*8 TOT2,M1,M2,ROTARG,PABS,COSTHE,PTEMP,PHI,PI
      REAL*8 PCPS,PC2,PN2,PS2,A,B,C,EPS2,EPS1
      REAL*8 W441,W443,W445,W10441,W20443,W30443,TMP
      REAL*8 W551,W553,W555,W10551,W20553,W30553
      LOGICAL FIRST,CHARMONIUM,BOTTONIUM,DIQUARK
      DATA PI/3.14159265359D0/
            
C--       do not use spec. version
      ILON=0      

C--       find light singlet system and hadronize them
      FIRST=.TRUE.
      NPARTON=N
      CHARMONIUM=.FALSE.
      BOTTONIUM=.FALSE.
      DIQUARK=.FALSE.
      ENERGY=2.D0*PARJ(32)
      DO 20 I=1,NPARTON
        IF (K(I,1).EQ.2) THEN
          IF (FIRST) THEN
            PSUM(1)=P(I,1)
            PSUM(2)=P(I,2)
            PSUM(3)=P(I,3)
            PSUM(4)=P(I,4)
            MSTJ(93)=1
            PSUM(5)=ULMASS(K(I,2))
            FIRST=.FALSE.
            IFIRST=I
          ELSE
            PSUM(1)=PSUM(1)+P(I,1)
            PSUM(2)=PSUM(2)+P(I,2)
            PSUM(3)=PSUM(3)+P(I,3)
            PSUM(4)=PSUM(4)+P(I,4)
            MSTJ(93)=1
            PSUM(5)=PSUM(5)+ULMASS(K(I,2))
          ENDIF
        ELSEIF (K(I,1).EQ.1 .AND. .NOT. FIRST) THEN
          PSUM(1)=PSUM(1)+P(I,1)
          PSUM(2)=PSUM(2)+P(I,2)
          PSUM(3)=PSUM(3)+P(I,3)
          PSUM(4)=PSUM(4)+P(I,4)
          MSTJ(93)=1
          PSUM(5)=PSUM(5)+ULMASS(K(I,2))
          INVMASS=PSUM(4)**2-PSUM(1)**2-PSUM(2)**2-PSUM(3)**2
          ENERGY=SQRT(MAX(0.D0,INVMASS))-PSUM(5)
          FIRST=.TRUE.
C--   only light systems with two c- or b-quarks or one diquark are of interest
          IF (ENERGY.LT.DBLE(PARJ(32)) .AND. 
     &       ((ABS(K(IFIRST,2)).EQ.4 .AND. ABS(K(I,2)).EQ.4 
     &       .OR. ABS(K(IFIRST,2)).EQ.5 .AND. ABS(K(I,2)).EQ.5) .OR.  
     /      (LUCOMP(K(IFIRST,2)).EQ.90 .OR. LUCOMP(K(I,2)).EQ.90))) THEN

            I1=IFIRST
            I2=I
C--       add system as cluster
            N=N+1
            DO 30 J=1,4
              P(N,J)=PSUM(J)
 30         CONTINUE
            P(N,5)=SQRT(MAX(0.D0,INVMASS))
            K(N,1)=11
            K(N,2)=91
            K(N,3)=I1
            K(N,4)=N+1
            K(N,5)=N+2
            IF(ABS(K(I1,2)).EQ.4) THEN
               CHARMONIUM=.TRUE.
               THRESHOLD=2.*ULMASS(421)
            ELSEIF(ABS(K(I1,2)).EQ.5) THEN
               BOTTONIUM=.TRUE.
               THRESHOLD=2.*ULMASS(521)
            ELSEIF(LUCOMP(K(I1,2)).EQ.90  .OR.
     /             LUCOMP(K(I2,2)).EQ.90) THEN
               DIQUARK=.TRUE.
               THRESHOLD=ULMASS(2212)+ULMASS(111)
            ENDIF
C--       inactivate old system
            DO 40 J=I1,I2
              K(J,1)=K(J,1)+10
              K(J,4)=N
 40         CONTINUE
 
C--       try to make two particles
            NTRY1=0
            NTRY2=0
 50         CONTINUE
C--       take quark or diquark end first
            IF (K(I1,2).EQ.4 .OR. K(I1,2).EQ.5 .OR.
     /          LUCOMP(K(I1,2)).EQ.90) THEN
              IQ=I1
              IAQ=I2
            ELSE
              IQ=I2
              IAQ=I1
            ENDIF
            KIQ=K(IQ,2)
            KIAQ=K(IAQ,2)
            KDUMMY=0
            KFB=0
            KFH1=0
            KFH2=0
            CALL LUKFDI(KIQ,KDUMMY,KFB,KFH1)
            KDUMMY=0
            CALL LUKFDI(KIAQ,-KFB,KDUMMY,KFH2)
            IF (KFH1.EQ.0 .OR. KFH2.EQ.0) THEN
              NTRY1=NTRY1+1
              IF (NTRY1.GE.100) THEN
                LST(21)=200
                RETURN
              ENDIF
              GOTO 50
            ENDIF
C--       consistency checks
            IF (LUCOMP(KFH1).EQ.0 .OR. LUCOMP(KFH2).EQ.0) THEN
              LST(21)=201
              RETURN
            ENDIF
            IF (KCHG(LUCOMP(KFH1),2)*ISIGN(1,KFH1)+
     +          KCHG(LUCOMP(KFH2),2)*ISIGN(1,KFH2) .NE.0) THEN
              LST(21)=202
              RETURN
            ENDIF
            P(N+1,5)=ULMASS(KFH1)
            P(N+2,5)=ULMASS(KFH2)
            IF (P(N,5).LE.P(N+1,5)+P(N+2,5)+PARJ(64) .AND.
     &          P(N,5).GE.THRESHOLD+PARJ(64) .AND.
     &          NTRY2.LE.100) THEN
              NTRY2=NTRY2+1
              GOTO 50
            ENDIF
            IF (P(N,5).GE.P(N+1,5)+P(N+2,5)+PARJ(64)) THEN
C--       make two particles
C--       isotropic decay in cms (dcostheta*dphi)
              TOT2=INVMASS
              M1=DBLE(P(N+1,5))
              M2=DBLE(P(N+2,5))            
              ROTARG=(TOT2-M1**2-M2**2)**2-4.D0*M1**2*M2**2
              IF (ROTARG.LT.0.D0) THEN
                LST(21)=203
                RETURN
              ENDIF
              PABS=0.5D0*SQRT(ROTARG/TOT2)
              COSTHE=-1.D0+2.D0*RLU(0)
              PTEMP=PABS*SQRT(1.D0-COSTHE**2)
              PHI=2.D0*PI*RLU(0)
              P(N+1,4)=SQRT(PABS**2+M1**2)
              P(N+1,3)=PABS*COSTHE
              P(N+1,2)=PTEMP*COS(PHI)
              P(N+1,1)=PTEMP*SIN(PHI)
              P(N+2,4)=SQRT(PABS**2+M2**2)
              P(N+2,3)=-P(N+1,3)
              P(N+2,2)=-P(N+1,2)
              P(N+2,1)=-P(N+1,1)
C--       K-vector
              K(N+1,1)=1
              K(N+1,2)=KFH1
              K(N+1,3)=N
              K(N+1,4)=0
              K(N+1,5)=0
              K(N+2,1)=1
              K(N+2,2)=KFH2
              K(N+2,3)=N
              K(N+2,4)=0
              K(N+2,5)=0
C--       boost to lab
              MSTU(33)=1
              CALL LUDBRB(N+1,N+2,0.,0.,PSUM(1)/PSUM(4),
     &                    PSUM(2)/PSUM(4),PSUM(3)/PSUM(4)) 
C--       V-vector
              DO 60 J=1,5 
                V(N,J)=V(IQ,J) 
                V(N+1,J)=V(IQ,J) 
                V(N+2,J)=V(IAQ,J) 
 60           CONTINUE 
              V(N,5)=0. 
              V(N+1,5)=0. 
              V(N+2,5)=0.
              N=N+2 
            ELSE


C--       make one particle instead
              IF(CHARMONIUM) THEN

                W441=1.D0
                W443=3.D0
                W445=5.D0
                W10441=1.D0
                W20443=3.D0
C--       extra suppression of psiprime from radial exitation
                W30443=3.D0/2.D0

                TMP=RLU(0)*(W441+W443+W445+W10441+W20443+W30443)
                IF (TMP .LT. W441) THEN
                  KFH1=441
                ELSEIF (TMP .LT. W441+W443) THEN
                  KFH1=443
                ELSEIF (TMP .LT. W441+W443+W445) THEN
                  KFH1=445
                ELSEIF (TMP .LT. W441+W443+W445+W10441) THEN
                  KFH1=10441
                ELSEIF (TMP .LT. W441+W443+W445+W10441+W20443) THEN
                  KFH1=20443
                ELSE
                  KFH1=30443
                ENDIF
                
C--       code for spec. version
                IF (ILON.EQ.1) THEN
                  KFH1=441
                  WEIGHT=W441/(W441+W443+W445+W10441+W20443+W30443)
                ENDIF
                IF (ILON.EQ.2) THEN
                  KFH1=443
                  WEIGHT=W443/(W441+W443+W445+W10441+W20443+W30443)
                ENDIF
                IF (ILON.EQ.3) THEN
                  KFH1=445
                  WEIGHT=W445/(W441+W443+W445+W10441+W20443+W30443)
                ENDIF
                IF (ILON.EQ.4) THEN
                  KFH1=10441
                  WEIGHT=W10441/(W441+W443+W445+W10441+W20443+W30443)
                ENDIF
                IF (ILON.EQ.5) THEN
                  KFH1=20443
                  WEIGHT=W20443/(W441+W443+W445+W10441+W20443+W30443)
                ENDIF
                IF (ILON.EQ.6) THEN
                  KFH1=30443
                  WEIGHT=W30443/(W441+W443+W445+W10441+W20443+W30443)
                ENDIF
                
              ELSEIF(BOTTONIUM) THEN

                W551=1.D0
                W553=3.D0
                W555=5.D0
                W10551=1.D0
                W20553=3.D0
                W30553=3.D0/2.D0

                TMP=RLU(0)*(W551+W553+W555+W10551+W20553+W30553)
                IF (TMP .LT. W551) THEN
                  KFH1=551
                ELSEIF (TMP .LT. W551+W553) THEN
                  KFH1=553
                ELSEIF (TMP .LT. W551+W553+W555) THEN
                  KFH1=555
                ELSEIF (TMP .LT. W551+W553+W555+W10551) THEN
                  KFH1=10551
                ELSEIF (TMP .LT. W551+W553+W555+W10551+W20553) THEN
                  KFH1=20553
                ELSE
                  KFH1=30553
                ENDIF

C--       code for spec. version
                IF (ILON.EQ.1) THEN
                  KFH1=551
                  WEIGHT=W551/(W551+W553+W555+W10551+W20553+W30553)
                ENDIF
                IF (ILON.EQ.2) THEN
                  KFH1=553
                  WEIGHT=W553/(W551+W553+W555+W10551+W20553+W30553)
                ENDIF
                IF (ILON.EQ.3) THEN
                  KFH1=555
                  WEIGHT=W555/(W551+W553+W555+W10551+W20553+W30553)
                ENDIF
                IF (ILON.EQ.4) THEN
                  KFH1=10551
                  WEIGHT=W10551/(W551+W553+W555+W10551+W20553+W30553)
                ENDIF
                IF (ILON.EQ.5) THEN
                  KFH1=20553
                  WEIGHT=W20553/(W551+W553+W555+W10551+W20553+W30553)
                ENDIF
                IF (ILON.EQ.6) THEN
                  KFH1=30553
                  WEIGHT=W30553/(W551+W553+W555+W10551+W20553+W30553)
                ENDIF

              ELSEIF(DIQUARK) THEN
70              CONTINUE
                KIQ=K(IQ,2)
                KIAQ=K(IAQ,2)
                KDUMMY=0
                KFH1=0
                CALL LUKFDI(KIQ,KIAQ,KDUMMY,KFH1)
                IF (KFH1.EQ.0) GOTO 70
C--       isospin conservation
                IF (KFH1.EQ.2214) KFH1=2212
                IF (KFH1.EQ.2114) KFH1=2112
                IF (KFH1.EQ.-2214) KFH1=-2212
                IF (KFH1.EQ.-2114) KFH1=-2112
         
              ENDIF

              K(N,5)=N+1
              K(N+1,1)=1
              K(N+1,2)=KFH1
              K(N+1,3)=N
              K(N+1,4)=0
              K(N+1,5)=0
              P(N+1,5)=ULMASS(KFH1)
C--       find particle to shuffle energy & momentum to and from
              MAXENERGY=0.D0
              I1=0
              DO 80 J=1,N-1
                IF (0.LT.K(J,1) .AND. K(J,1).LT.10 .AND.
     &              LUCOMP(K(J,2)).NE.0 .AND.
     &              (K(J,2).EQ.21 .OR. ABS(K(J,2)).LT.10 .OR.
     &               ABS(K(J,2)).GT.100) ) THEN
                  INVMASS=(P(N,4)+P(J,4))**2-(P(N,1)+P(J,1))**2-
     -                    (P(N,2)+P(J,2))**2-(P(N,3)+P(J,3))**2
                  ENERGY=SQRT(MAX(0.D0,INVMASS))-P(N,5)-P(J,5)
                  IF (ENERGY.GT.MAXENERGY ) THEN
                    I1=J
                    MAXENERGY=ENERGY
                  ENDIF
                ENDIF
 80           CONTINUE
C--       shuffle energy & momentum
              IF (I1.NE.0) THEN
                PCPS=DBLE(P(N,4))*DBLE(P(I1,4))
     -              -DBLE(P(N,1))*DBLE(P(I1,1))
     -              -DBLE(P(N,2))*DBLE(P(I1,2))
     -              -DBLE(P(N,3))*DBLE(P(I1,3))
                PC2=DBLE(P(N,5))**2
                PN2=DBLE(P(N+1,5))**2
                PS2=DBLE(P(I1,5))**2
                A=PC2+PS2+2.D0*PCPS
                B=PC2+PN2+2.D0*PCPS
                C=(PN2-PC2)*(4.D0*PCPS*(PCPS+PC2)-PC2*(PN2-PC2))/
     /            4.D0/(PCPS**2-PC2*PS2)
                IF (B**2-4.D0*C*A.LT.0.D0) THEN
                  LST(21)=204
                  RETURN
                ENDIF
                EPS2=(-B+SQRT(MAX(0.D0,B**2-4.D0*C*A)))/2.D0/A
                EPS1=(PN2-PC2+2.D0*EPS2*(PS2+PCPS))/2.D0/(PC2+PCPS)
                DO 90 J=1,4 
                  P(N+1,J)=(1.+EPS1)*P(N,J)-EPS2*P(I1,J) 
                  P(I1,J)=(1.+EPS2)*P(I1,J)-EPS1*P(N,J) 
                  V(N,J)=V(I1,J) 
                  V(N+1,J)=V(I1,J) 
 90             CONTINUE 
                V(N,5)=0. 
                V(N+1,5)=0. 
                N=N+1
              ELSE
                LST(21)=205
                RETURN
              ENDIF
            ENDIF
            CHARMONIUM=.FALSE.
            BOTTONIUM=.FALSE.
            DIQUARK=.FALSE.
         ENDIF
        ENDIF
 20   CONTINUE
          
      RETURN
      
      END
