
      SUBROUTINE DT_FLAHAD(Id,If1,If2,If3)
 
C***********************************************************************
C sampling of FLAvor composition for HADrons/photons                   *
C              ID         BAMJET-id of hadron                          *
C              IF1,2,3    flavor content                               *
C                         (u,d,s: 1,2,3;  au,ad,as: -1,-1,-3)          *
C Note:  -  u,d numbering as in BAMJET                                 *
C        -  All ID are accepted. ID above 39 taken from PHOJET.        *
C This version dated 12.03.96 is written by S. Roesler                 *
C***********************************************************************
 
      IMPLICIT NONE
      DOUBLE PRECISION DT_RNDM , one
      INTEGER Id , idcpc , idpdg , IDT_IPDGHA , If1 , If2 , If3 , 
     &        IPHO_PDG2ID , ipoqua , iq , ix , jsel , k
      SAVE 
 
C auxiliary common for reggeon exchange (DTUNUC 1.x)
      INCLUDE 'inc/dtquar'
      INCLUDE 'inc/popar2'
 
      DIMENSION jsel(3,6)
      DATA jsel/1 , 2 , 3 , 2 , 3 , 1 , 3 , 1 , 2 , 1 , 3 , 2 , 2 , 1 , 
     &     3 , 3 , 2 , 1/
      DIMENSION ipoqua(3)
 
      one = 1.0D0
      IF ( Id.EQ.7 ) THEN
C photon (charge dependent flavour sampling)
         k = INT(DT_RNDM(one)*6.D0+1.D0)
         IF ( k.LE.4 ) THEN
            If1 = 2
            If2 = -2
         ELSE IF ( k.EQ.5 ) THEN
            If1 = 1
            If2 = -1
         ELSE
            If1 = 3
            If2 = -3
         END IF
         IF ( DT_RNDM(one).LT.0.5D0 ) THEN
            k = If1
            If1 = If2
            If2 = k
         END IF
         If3 = 0
      ELSE
C hadron
         ix = INT(1.0D0+5.99999D0*DT_RNDM(one))
         IF ( Id.LE.39 ) THEN
            If1 = MQUark(jsel(1,ix),Id)
            If2 = MQUark(jsel(2,ix),Id)
            If3 = MQUark(jsel(3,ix),Id)
 
C Use PHOJET quark tables if BAMJET id exceeds 39 (limit of MQUARK)
         ELSE
            idpdg = IDT_IPDGHA(Id)
            ! WRITE(6,*) 'DT_FLAHAD: ID, IDPDG', ID, IDPDG
            idcpc = IPHO_PDG2ID(idpdg)
            DO iq = 1 , 3
               ipoqua(iq) = IQ_list(iq,ABS(idcpc))
               ipoqua(iq) = SIGN(ipoqua(iq),idcpc)
 
C Swap u and d quarks to convert from PHOJET to DPMJET
               IF ( ABS(ipoqua(iq)).EQ.1 ) THEN
                  ipoqua(iq) = SIGN(2,ipoqua(iq))
               ELSE IF ( ABS(ipoqua(iq)).EQ.2 ) THEN
                  ipoqua(iq) = SIGN(1,ipoqua(iq))
               END IF
            END DO
            ! WRITE(6,*) 'DT_FLAHAD: ID, IPOQUA', ID, IPOQUA
            If1 = ipoqua(jsel(1,ix))
            If2 = ipoqua(jsel(2,ix))
            If3 = ipoqua(jsel(3,ix))
         END IF
         ! WRITE(6,*) 'DT_FLAHAD: ID, IF1, IF2, IF3', ID,IF1,IF2,IF3
         IF ( (If1.EQ.0) .AND. (If3.NE.0) ) THEN
            If1 = If3
            If3 = 0
         ELSE IF ( (If2.EQ.0) .AND. (If3.NE.0) ) THEN
            If2 = If3
            If3 = 0
         END IF
      END IF
 
      END SUBROUTINE
