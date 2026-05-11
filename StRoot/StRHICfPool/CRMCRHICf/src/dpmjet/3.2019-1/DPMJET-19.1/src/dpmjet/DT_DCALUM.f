
      SUBROUTINE DT_DCALUM(N,Itta)
 
      IMPLICIT NONE
      DOUBLE PRECISION am1 , am12 , am2 , am22 , ams , amss , elab , 
     &                 plab2 , umoo
      INTEGER ie , iee , ieo , ik , ik1 , ike , iki , iko , in , ire , 
     &        Itta , k11 , k22 , N
      SAVE 
 
 
C particle properties (BAMJET index convention),
C (dublicate of DTPART for HADRIN)
      INCLUDE 'inc/hnablt'
      INCLUDE 'inc/hnredv'
      INCLUDE 'inc/hnspli'
      INCLUDE 'inc/hnreac'
 
      ire = NURe(N,Itta/8+1)
      ieo = IEIi(ire) + 1
      iee = IEIi(ire+1)
      am1 = AMH(N)
      am12 = am1**2
      am2 = AMH(Itta)
      am22 = am2**2
      DO ie = ieo , iee
         plab2 = PLAbf(ie)**2
         elab = SQRT(am12+am22+2.0D0*SQRT(plab2+am12)*am2)
         UMO(ie) = elab
      END DO
      iko = IKIi(ire) + 1
      ike = IKIi(ire+1)
      umoo = UMO(ieo)
      DO ik = iko , ike
         IF ( NRK(2,ik).LE.0 ) THEN
            iki = NRK(1,ik)
            amss = 5.0D0
            k11 = K1H(iki)
            k22 = K2H(iki)
            DO ik1 = k11 , k22
               in = NZKi(ik1,1)
               ams = AMH(in)
               in = NZKi(ik1,2)
               IF ( in.GT.0 ) ams = ams + AMH(in)
               in = NZKi(ik1,3)
               IF ( in.GT.0 ) ams = ams + AMH(in)
               IF ( ams.LT.amss ) amss = ams
            END DO
            IF ( umoo.LT.amss ) umoo = amss
            THResh(ik) = umoo
         END IF
      END DO
      END SUBROUTINE
