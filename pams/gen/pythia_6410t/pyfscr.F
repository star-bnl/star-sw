  
C*********************************************************************
 
C...PYFSCR
C...Performs colour annealing.
C...MSTP(95) : CR Type
C...         = 1  : old cut-and-paste reconnections, handled in PYMIHK
C...         = 2  : Type I(no gg loops); hadron-hadron only
C...         = 3  : Type I(no gg loops); all beams
C...         = 4  : Type II(gg loops)  ; hadron-hadron only
C...         = 5  : Type II(gg loops)  ; all beams
C...         = 6  : Type S             ; hadron-hadron only
C...         = 7  : Type S             ; all beams
C...Types I and II are described in Sandhoff+Skands, in hep-ph/0604120.
C...Type S is driven by starting only from free triplets, not octets.
C...A string piece remains unchanged with probability
C...    PKEEP = (1-PARP(78))**N
C...This scaling corresponds to each string piece having to go through
C...N other ones, each with probability PARP(78) for reconnection, where
C...N is here chosen simply as the number of multiple interactions,
C...for a rough scaling with the general level of activity.
 
      SUBROUTINE PYFSCR(IP)
C...Double precision and integer declarations.
      IMPLICIT DOUBLE PRECISION(A-H, O-Z)
      INTEGER PYK,PYCHGE,PYCOMP
C...Commonblocks.
      COMMON/PYJETS/N,NPAD,K(4000,5),P(4000,5),V(4000,5)
      COMMON/PYDAT1/MSTU(200),PARU(200),MSTJ(200),PARJ(200)
      COMMON/PYPARS/MSTP(200),PARP(200),MSTI(200),PARI(200)
      COMMON/PYDAT2/KCHG(500,4),PMAS(500,4),PARF(2000),VCKM(4,4)
      COMMON/PYINT1/MINT(400),VINT(400)
C...The common block of colour tags.
      COMMON/PYCTAG/NCT,MCT(4000,2)
      SAVE /PYJETS/,/PYDAT1/,/PYDAT2/,/PYINT1/,/PYCTAG/,
     &/PYPARS/
C...MCN: Temporary storage of new colour tags
      DOUBLE PRECISION MCN(4000,2)
 
C...Function to give four-product.
      FOUR(I,J)=P(I,4)*P(J,4)
     &          -P(I,1)*P(J,1)-P(I,2)*P(J,2)-P(I,3)*P(J,3)
 
C...Check valid range of MSTP(95), local copy
      IF (MSTP(95).LE.1.OR.MSTP(95).GE.8) RETURN
      MSTP95=MOD(MSTP(95),10)
C...Set whether CR allowed inside resonance systems or not
C...(not implemented yet)
C      MRESCR=1
C      IF (MSTP(95).GE.10) MRESCR=0
 
C...Check whether colour tags already defined
      IF (MINT(33).EQ.0) THEN
C...Erase any existing colour tags for this event
        DO 100 I=1,N
          MCT(I,1)=0
          MCT(I,2)=0
  100   CONTINUE
C...Create colour tags for this event
        DO 120 I=1,N
          IF (K(I,1).EQ.3) THEN
            DO 110 KCS=4,5
              KCSIN=KCS
              IF (MCT(I,KCSIN-3).EQ.0) THEN
                CALL PYCTTR(I,KCSIN,I)
              ENDIF
  110       CONTINUE
          ENDIF
  120 CONTINUE
C...Instruct PYPREP to use colour tags
        MINT(33)=1
      ENDIF
 
C...For MSTP(95) even, only apply to hadron-hadron
      IF (MOD(MSTP(95),2).EQ.0) THEN
         KA1=IABS(MINT(11))
         KA2=IABS(MINT(12))
         IF (KA1.LT.100.OR.KA2.LT.100) GOTO 9999
      ENDIF
 
C...Initialize new tag array (but do not delete old yet)
      LCT=NCT
      DO 130 I=MAX(1,IP),N
         MCN(I,1)=0
         MCN(I,2)=0
  130 CONTINUE
 
C...For each final-state dipole, check whether string should be
C...preserved.
      DO 150 ICT=1,NCT
        IC=0
        IA=0
        DO 140 I=MAX(1,IP),N
          IF (K(I,1).EQ.3.AND.MCT(I,1).EQ.ICT) IC=I
          IF (K(I,1).EQ.3.AND.MCT(I,2).EQ.ICT) IA=I
  140   CONTINUE
        IF (IC.NE.0.AND.IA.NE.0) THEN
C...Chiefly consider large strings.
          PKEEP=(1D0-PARP(78))**MINT(31)
          IF (PYR(0).LE.PKEEP) THEN
            LCT=LCT+1
            MCN(IC,1)=LCT
            MCN(IA,2)=LCT
          ENDIF
        ENDIF
  150 CONTINUE
 
C...Loop over event record, starting from IP
C...(Ignore junctions for now.)
      NLOOP=0
  160 NLOOP=NLOOP+1
      MCIMAX=0
      MCJMAX=0
      RLMAX=0D0
      ILMAX=0
      JLMAX=0
      DO 230 I=MAX(1,IP),N
         IF (K(I,1).NE.3) GOTO 230
C...Check colour charge
         MCI=KCHG(PYCOMP(K(I,2)),2)*ISIGN(1,K(I,2))
         IF (MCI.EQ.0) GOTO 230
C...For Seattle algorithm, only start from partons with one dangling
C...colour tag
         IF (MSTP(95).EQ.6.OR.MSTP(95).EQ.7) THEN
           IF (MCI.EQ.2.AND.MCN(I,1).EQ.0.AND.MCN(I,2).EQ.0) GOTO 230
         ENDIF
C...  Find optimal partner
         JLOPT=0
         MCJOPT=0
         MBROPT=0
         MGGOPT=0
         RLOPT=1D19
C...Loop over I colour/anticolour, check whether already connected
  170    DO 220 ICL=1,2
            IF (MCN(I,ICL).NE.0) GOTO 220
            IF (ICL.EQ.1.AND.MCI.EQ.-1) GOTO 220
            IF (ICL.EQ.2.AND.MCI.EQ.1) GOTO 220
C...Check whether this is a dangling colour tag (ie to junction!)
            IFOUND=0
            DO 180 J=MAX(1,IP),N
               IF (K(J,1).EQ.3.AND.MCT(J,3-ICL).EQ.MCT(I,ICL)) IFOUND=1
  180       CONTINUE
            IF (IFOUND.EQ.0) GOTO 220
            DO 210 J=MAX(1,IP),N
               IF (K(J,1).NE.3.OR.I.EQ.J) GOTO 210
C...Do not make direct connections between partons in same Beam Remnant
               MBRSTR=0
               IF (K(I,3).LE.2.AND.K(J,3).LE.2.AND.K(I,3).EQ.K(J,3))
     &              MBRSTR=1
C...Check colour charge
               MCJ=KCHG(PYCOMP(K(J,2)),2)*ISIGN(1,K(J,2))
               IF (MCJ.EQ.0.OR.(MCJ.EQ.MCI.AND.MCI.NE.2)) GOTO 210
C...Check for gluon loops
               MGGSTR=0
               IF (MCJ.EQ.2.AND.MCI.EQ.2) THEN
                 ICLA=3-ICL
                 IF (MCN(I,ICLA).EQ.MCN(J,ICL).AND.MSTP(95).LE.3.AND.
     &                MCN(I,ICLA).NE.0) MGGSTR=1
               ENDIF
C...Loop over J colour/anticolour, check whether already connected
               DO 200 JCL=1,2
                  IF (MCN(J,JCL).NE.0) GOTO 200
                  IF (JCL.EQ.ICL) GOTO 200
                  IF (JCL.EQ.1.AND.MCJ.EQ.-1) GOTO 200
                  IF (JCL.EQ.2.AND.MCJ.EQ.1) GOTO 200
C...Check whether this is a dangling colour tag (ie to junction!)
                  IFOUND=0
                  DO 190 J2=MAX(1,IP),N
                     IF (K(J2,1).EQ.3.AND.MCT(J2,3-JCL).EQ.MCT(J,JCL))
     &                    IFOUND=1
  190             CONTINUE
                  IF (IFOUND.EQ.0) GOTO 200
C...Save connection with smallest lambda measure
C...If best so far was a BR string and this is not, also save.
C...If best so far was a gg string and this is not, also save.
                  RL=FOUR(I,J)
                  IF (RL.LT.RLOPT.OR.(RL.EQ.RLOPT.AND.PYR(0).LE.0.5D0)
     &                 .OR.(MBROPT.EQ.1.AND.MBRSTR.EQ.0)
     &                 .OR.(MGGOPT.EQ.1.AND.MGGSTR.EQ.0)) THEN
                     RLOPT=RL
                     JLOPT=J
                     ICOPT=ICL
                     JCOPT=JCL
                     MCJOPT=MCJ
                     MBROPT=MBRSTR
                     MGGOPT=MGGSTR
                  ENDIF
  200          CONTINUE
  210       CONTINUE
  220    CONTINUE
         IF (JLOPT.NE.0) THEN
C...Save pair with largest RLOPT so far
            IF (RLOPT.GE.RLMAX) THEN
               RLMAX=RLOPT
               ILMAX=I
               JLMAX=JLOPT
               ICMAX=ICOPT
               JCMAX=JCOPT
               MCJMAX=MCJOPT
               MCIMAX=MCI
            ENDIF
         ENDIF
  230 CONTINUE
C...Save and iterate
      IF (ILMAX.GT.0) THEN
         LCT=LCT+1
         MCN(ILMAX,ICMAX)=LCT
         MCN(JLMAX,JCMAX)=LCT
         IF (NLOOP.LE.2*(N-IP)) THEN
            GOTO 160
         ELSE
            PRINT*, 'infinite loop!'
            STOP
         ENDIF
      ELSE
C...Save and exit. First check for leftover gluon(s)
         DO 260 I=MAX(1,IP),N
C...Check colour charge
            MCI=KCHG(PYCOMP(K(I,2)),2)*ISIGN(1,K(I,2))
            IF (K(I,1).NE.3.OR.MCI.NE.2) GOTO 260
            IF(MCN(I,1).EQ.0.AND.MCN(I,2).EQ.0) THEN
C...Decide where to put left-over gluon (minimal insertion)
               ILMAX=0
               RLMAX=1D19
               DO 250 KCT=NCT+1,LCT
                  DO 240 IT=MAX(1,IP),N
                     IF (IT.EQ.I.OR.K(IT,1).NE.3) GOTO 240
                     IF (MCN(IT,1).EQ.KCT) IC=IT
                     IF (MCN(IT,2).EQ.KCT) IA=IT
  240             CONTINUE
                  RL=FOUR(IC,I)*FOUR(IA,I)
                  IF (RL.LT.RLMAX) THEN
                     RLMAX=RL
                     ICMAX=IC
                     IAMAX=IA
                  ENDIF
  250          CONTINUE
               LCT=LCT+1
               MCN(I,1)=MCN(ICMAX,1)
               MCN(I,2)=LCT
               MCN(ICMAX,1)=LCT
            ENDIF
  260    CONTINUE
         DO 270 I=MAX(1,IP),N
C...Do not erase parton shower colour history
            IF (K(I,1).NE.3) GOTO 270
C...Check colour charge
            MCI=KCHG(PYCOMP(K(I,2)),2)*ISIGN(1,K(I,2))
            IF (MCI.EQ.0) GOTO 270
            IF (MCN(I,1).NE.0) MCT(I,1)=MCN(I,1)
            IF (MCN(I,2).NE.0) MCT(I,2)=MCN(I,2)
  270    CONTINUE
      ENDIF
 
 9999 RETURN
      END
