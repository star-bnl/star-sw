
      SUBROUTINE PHO_REGPAR(Isth,Idpdg,Idbam,Jm1,Jm2,P1,P2,P3,P4,Iphis1,
     &                      Iphis2,Ic1,Ic2,Ipos,Imode)
C**********************************************************************
C
C     registration of particle in /POEVT1/ and /POEVT2/
C
C     input:    ISTH             status code of particle
C                                 -2     initial parton hard scattering
C                                 -1     parton
C                                  0     string
C                                  1     visible particle (no color)
C                                  2     decayed particle
C               IDPDG            PDG particle ID code
C               IDBAM            CPC particle ID code
C               JM1,JM2          first and second mother index
C               P1..P4           four momentum
C               IPHIS1           extended history information
C                                  IPHIS1<100: JM1 from particle 1
C                                  IPHIS1>100: JM1 from particle 2
C                                  1    valence quark
C                                  2    valence diquark
C                                  3    sea quark
C                                  4    sea diquark
C                                  (neg. for antipartons)
C               IPHIS2           extended history information
C                                  positive: JM2 from particle 1
C                                  negative: JM2 from particle 2
C                                  (see IPHIS1)
C               IC1,IC2          color labels for partons
C               IMODE            1  register given parton
C                                0  reset /POEVT1/ and /POEVT2/
C                                2  return data of entry IPOS
C
C               IPOS             position of particle in /POEVT1/
C
C**********************************************************************
      IMPLICIT NONE
      DOUBLE PRECISION DEPS , P1 , P2 , P3 , P4 , tmp
      INTEGER Ic1 , Ic2 , Idbam , idbami , Idpdg , idpdgi , ii , Imode , 
     &        Iphis1 , Iphis2 , IPHO_BAR3 , IPHO_CHR3 , IPHO_ID2PDG , 
     &        IPHO_PDG2ID , Ipos , Isth , Jm1 , Jm2
      SAVE 
 
      PARAMETER (DEPS=1.D-20)
 
C  input/output channels
      INCLUDE 'inc/poinou'
C  event debugging information
      INCLUDE 'inc/podebg'
 
C  standard particle data interface
 
 
      INCLUDE 'inc/poevt1'
C  extension to standard particle data interface (PHOJET specific)
      INCLUDE 'inc/poevt2'
 
      IF ( Imode.EQ.1 ) THEN
         IF ( IDEb(76).GE.26 ) THEN
            IF ( LPRi.GT.4 ) WRITE (LO,'(1X,A,/2X,I3,I6,3I4,4E10.3)')
     &            'PHO_REGPAR: ISTH,IDPDG,IDBAM,JM1,JM2,P1,P2,P3,P4' , 
     &           Isth , Idpdg , Idbam , Jm1 , Jm2 , P1 , P2 , P3 , P4
            IF ( LPRi.GT.4 ) WRITE (LO,'(1X,A,/2X,6I6)')
     &            'PHO_REGPAR: IPHIS1,IPHIS2,IC1,IC2,IPOS,IMODE' , 
     &           Iphis1 , Iphis2 , Ic1 , Ic2 , Ipos , Imode
         END IF
         IF ( NHEp.EQ.NMXHEP ) THEN
            IF ( LPRi.GT.4 ) WRITE (LO,'(/1X,2A,2I6/)') 'PHO_REGPAR: ' , 
     &           'no space left in /POEVT1/ (NHEP,NMXHEP):' , NHEp , 
     &           NMXHEP
            CALL PHO_ABORT
         END IF
         NHEp = NHEp + 1
         idbami = Idbam
         idpdgi = Idpdg
         IF ( ABS(Isth).LE.2 ) THEN
            IF ( (Idbam.NE.0) .AND. (Idpdg.EQ.0) ) THEN
               idpdgi = IPHO_ID2PDG(Idbam)
            ELSE IF ( (Idbam.EQ.0) .AND. (Idpdg.NE.0) ) THEN
               idbami = IPHO_PDG2ID(Idpdg)
            END IF
         END IF
C  standard data
         ISThep(NHEp) = Isth
         IDHep(NHEp) = idpdgi
         JMOhep(1,NHEp) = Jm1
         JMOhep(2,NHEp) = Jm2
C  update of mother-daugther relations
         IF ( ABS(Isth).LE.1 ) THEN
            IF ( Jm1.GT.0 ) THEN
               IF ( JDAhep(1,Jm1).EQ.0 ) THEN
                  JDAhep(1,Jm1) = NHEp
                  ISThep(Jm1) = 2
               END IF
               JDAhep(2,Jm1) = NHEp
            END IF
            IF ( (Jm2.NE.Jm1) .AND. (Jm2.GT.0) ) THEN
               IF ( JDAhep(1,Jm2).EQ.0 ) THEN
                  JDAhep(1,Jm2) = NHEp
                  ISThep(Jm2) = 2
               END IF
               JDAhep(2,Jm2) = NHEp
            ELSE IF ( Jm2.LT.0 ) THEN
               DO ii = Jm1 + 1 , -Jm2
                  IF ( JDAhep(1,ii).EQ.0 ) THEN
                     JDAhep(1,ii) = NHEp
                     ISThep(ii) = 2
                  END IF
                  JDAhep(2,ii) = NHEp
               END DO
            END IF
         END IF
         PHEp(1,NHEp) = P1
         PHEp(2,NHEp) = P2
         PHEp(3,NHEp) = P3
         PHEp(4,NHEp) = P4
         IF ( (ABS(Isth).LE.3) .OR. (Isth.EQ.20) .OR. (Isth.EQ.21) )
     &        THEN
            tmp = (P4-P3)*(P4+P3) - P1**2 - P2**2
            PHEp(5,NHEp) = SIGN(SQRT(ABS(tmp)),tmp)
         ELSE
            PHEp(5,NHEp) = 0.D0
         END IF
         JDAhep(1,NHEp) = 0
         JDAhep(2,NHEp) = 0
C  extended information
         IMPart(NHEp) = idbami
C  extended history information
         IPHist(1,NHEp) = Iphis1
         IPHist(2,NHEp) = Iphis2
C  charge/baryon number or color labels
         IF ( Isth.EQ.1 ) THEN
            ICOlor(1,NHEp) = IPHO_CHR3(NHEp,2)
            ICOlor(2,NHEp) = IPHO_BAR3(NHEp,2)
         ELSE
            ICOlor(1,NHEp) = Ic1
            ICOlor(2,NHEp) = Ic2
         END IF
 
         Ipos = NHEp
         IF ( IDEb(76).GE.26 ) THEN
            IF ( LPRi.GT.4 ) WRITE (LO,'(1X,A,2I4,2X,2I4,E12.3,I5)')
     &            'PHO_REGPAR: IPHIST1/2,IC1/2,MASS,IPOS' , 
     &           IPHist(1,NHEp) , IPHist(2,NHEp) , ICOlor(1,NHEp) , 
     &           ICOlor(2,NHEp) , PHEp(5,NHEp) , Ipos
         END IF
 
      ELSE IF ( Imode.EQ.0 ) THEN
         NHEp = 0
      ELSE IF ( Imode.EQ.2 ) THEN
         IF ( (Ipos.LT.1) .OR. (Ipos.GT.NHEp) ) THEN
            IF ( LPRi.GT.4 ) WRITE (LO,'(1X,2A,2I8)') 'PHO_REGPAR: ' , 
     &           'index out of bounds (NHEP,IPOS)' , NHEp , Ipos
            RETURN
         END IF
         Isth = ISThep(Ipos)
         Idpdg = IDHep(Ipos)
         Idbam = IMPart(Ipos)
         Jm1 = JMOhep(1,Ipos)
         Jm2 = JMOhep(2,Ipos)
         P1 = PHEp(1,Ipos)
         P2 = PHEp(2,Ipos)
         P3 = PHEp(3,Ipos)
         P4 = PHEp(4,Ipos)
         Iphis1 = IPHist(1,Ipos)
         Iphis2 = IPHist(2,Ipos)
         Ic1 = ICOlor(1,Ipos)
         Ic2 = ICOlor(2,Ipos)
      ELSE
         IF ( LPRi.GT.4 ) WRITE (LO,'(1X,A,I8)')
     &         'PHO_REGPAR: invalid mode' , Imode
      END IF
      END SUBROUTINE
