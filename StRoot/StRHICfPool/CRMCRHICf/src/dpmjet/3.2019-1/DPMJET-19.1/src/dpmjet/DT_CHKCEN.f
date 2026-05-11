
      SUBROUTINE DT_CHKCEN(Ip,It,Np,Nt,Iback)
 
C***********************************************************************
C Check of number of involved projectile nucleons if central production*
C is requested.                                                        *
C Adopted from a part of the old KKEVT routine which was written by    *
C J. Ranft/H.-J.Moehring.                                              *
C This version dated 13.01.95 is written by S. Roesler                 *
C***********************************************************************
 
      IMPLICIT NONE
      INTEGER Iback , Ip , It , Np , Nt
      SAVE 
 
      INCLUDE 'inc/dtflka'
 
C statistics
      INCLUDE 'inc/dtsta1'
C central particle production, impact parameter biasing
      INCLUDE 'inc/dtimpa'
 
      Iback = 0
 
C old version
      IF ( ICEntr.EQ.2 ) THEN
         IF ( Ip.LT.It ) THEN
            IF ( Ip.LE.8 ) THEN
               IF ( Np.LT.Ip-1 ) Iback = 1
            ELSE IF ( Ip.LE.16 ) THEN
               IF ( Np.LT.Ip-2 ) Iback = 1
            ELSE IF ( Ip.LE.32 ) THEN
               IF ( Np.LT.Ip-3 ) Iback = 1
            ELSE IF ( Ip.GE.33 ) THEN
               IF ( Np.LT.Ip-5 ) Iback = 1
            END IF
         ELSE IF ( Ip.EQ.It ) THEN
            IF ( Ip.EQ.32 ) THEN
               IF ( (Np.LT.22) .OR. (Nt.LT.22) ) Iback = 1
            ELSE
               IF ( Np.LT.Ip-Ip/8 ) Iback = 1
            END IF
         ELSE IF ( ABS(Ip-It).LT.3 ) THEN
            IF ( Np.LT.Ip-Ip/8 ) Iback = 1
         END IF
C new version (DPMJET, 5.6.99)
      ELSE IF ( Ip.LT.It ) THEN
         IF ( Ip.LE.8 ) THEN
            IF ( Np.LT.Ip-1 ) Iback = 1
         ELSE IF ( Ip.LE.16 ) THEN
            IF ( Np.LT.Ip-2 ) Iback = 1
         ELSE IF ( Ip.LT.32 ) THEN
            IF ( Np.LT.Ip-3 ) Iback = 1
         ELSE IF ( Ip.GE.32 ) THEN
            IF ( It.LE.150 ) THEN
C   Example: S-Ag
               IF ( Np.LT.Ip-1 ) Iback = 1
            ELSE
C   Example: S-Au
               IF ( Np.LT.Ip ) Iback = 1
            END IF
         END IF
      ELSE IF ( Ip.EQ.It ) THEN
C   Example: S-S
         IF ( Ip.EQ.32 ) THEN
            IF ( (Np.LT.22) .OR. (Nt.LT.22) ) Iback = 1
C   Example: Pb-Pb
         ELSE
            IF ( Np.LT.Ip-Ip/4 ) Iback = 1
         END IF
      ELSE IF ( ABS(Ip-It).LT.3 ) THEN
         IF ( Np.LT.Ip-Ip/8 ) Iback = 1
      END IF
 
      ICCpro = ICCpro + 1
 
      END SUBROUTINE
