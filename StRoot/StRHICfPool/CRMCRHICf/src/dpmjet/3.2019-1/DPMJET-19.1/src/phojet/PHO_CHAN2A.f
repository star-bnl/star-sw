
      SUBROUTINE PHO_CHAN2A(Bb)
C***********************************************************************
C
C     simple two channel model to realize low mass diffraction
C     (version A, iteration of triple- and loop-Pomeron)
C
C     input:     BB      impact parameter (mb**1/2)
C
C     output:    /POINT4/
C                AMPEL      elastic amplitude
C                AMPVM(4,4) q-elastic VM production
C                AMLMSD(2)  low mass single diffraction amplitude
C                AMHMSD(2)  high mass single diffraction amplitude
C                AMLMDD     low mass double diffraction amplitude
C                AMHMDD     high mass double diffraction amplitude
C                AMPDP(4)   central diffraction amplitude
C
C***********************************************************************
      IMPLICIT NONE
      DOUBLE PRECISION ab , absum , ampcha , ampela , ampvme , b24 , 
     &                 Bb , cfac , chda , chdb , chdd , chdh , chdpa , 
     &                 chdpb , chdpd , chdpe , chds , chi , chifac , 
     &                 DEPS
      DOUBLE PRECISION EIGHT , ex1chi , ex2chi , expfac , tmp
      INTEGER i , ieltab , ii , j , k
      SAVE 
 
      PARAMETER (DEPS=1.D-5,EIGHT=8.D0)
 
C  input/output channels
      INCLUDE 'inc/poinou'
C  event debugging information
      INCLUDE 'inc/podebg'
C  model switches and parameters
      INCLUDE 'inc/pomdls'
C  some constants
      INCLUDE 'inc/pocons'
C  complex Born graph amplitudes used for unitarization
      INCLUDE 'inc/point4'
C  unitarized amplitudes for different diffraction channels
      INCLUDE 'inc/point5'
C  Reggeon phenomenology parameters
      INCLUDE 'inc/popreg'
C  parameters of 2x2 channel model
      INCLUDE 'inc/po2cha'
C  global event kinematics and particle IDs
      INCLUDE 'inc/pogcms'
 
C  local variables
      DIMENSION ab(9,4) , chi(4) , chds(4) , chdh(4) , chda(4) , chdb(4)
     &          , chdd(4) , chdpe(4) , chdpa(4) , chdpb(4) , chdpd(4) , 
     &          ampcha(4) , ex1chi(4) , ex2chi(4) , absum(4) , 
     &          ampela(4,0:9)
      DIMENSION chifac(4,4) , expfac(4,4) , ieltab(4,4)
 
C  combinatorical factors
      DATA chifac/1.D0 , 1.D0 , -1.D0 , -1.D0 , 1.D0 , -1.D0 , 1.D0 , 
     &     -1.D0 , 1.D0 , -1.D0 , -1.D0 , 1.D0 , 1.D0 , 1.D0 , 1.D0 , 
     &     1.D0/
      DATA expfac/1.D0 , 1.D0 , 1.D0 , 1.D0 , 1.D0 , -1.D0 , -1.D0 , 
     &     1.D0 , -1.D0 , 1.D0 , -1.D0 , 1.D0 , -1.D0 , -1.D0 , 1.D0 , 
     &     1.D0/
      DATA ieltab/1 , 2 , 3 , 4 , 2 , 1 , 4 , 3 , 3 , 4 , 1 , 2 , 4 , 
     &     3 , 2 , 1/
 
      IF ( LPRi.GT.4 .AND. IDEb(86).GE.20 ) WRITE (LO,'(1X,A,E12.3)')
     &      'PHO_CHAN2A: impact parameter B' , Bb
 
      b24 = Bb**2/4.D0
      DO i = 1 , 4
         ab(1,i) = ZXP(1,i)*EXP(-b24/BXP(1,i)) + ZXR(1,i)
     &             *EXP(-b24/BXR(1,i))
         ab(2,i) = ZXH(1,i)*EXP(-b24/BXH(1,i))
         ab(3,i) = -ZXT1a(1,i)*EXP(-b24/BXT1a(1,i))
         ab(4,i) = -ZXT2a(1,i)*EXP(-b24/BXT2a(1,i))
         ab(5,i) = -ZXL(1,i)*EXP(-b24/BXL(1,i)) - ZXT1b(1,i)
     &             *EXP(-b24/BXT1b(1,i)) - ZXT2b(1,i)
     &             *EXP(-b24/BXT2b(1,i))
         ab(6,i) = ZXDpe(1,i)*EXP(-b24/BXDpe(1,i))
         ab(7,i) = ZXDpa(1,i)*EXP(-b24/BXDpa(1,i))
         ab(8,i) = ZXDpb(1,i)*EXP(-b24/BXDpb(1,i))
         ab(9,i) = ZXDpd(1,i)*EXP(-b24/BXDpd(1,i))
      END DO
 
      DO i = 1 , 4
         absum(i) = 0.D0
         DO ii = 9 , 1 , -1
            absum(i) = absum(i) + ab(ii,i)
         END DO
      END DO
      IF ( LPRi.GT.4 .AND. IDEb(86).GE.20 ) WRITE (LO,'(1X,A,4E12.3)')
     &      'PHO_CHAN2A: ABSUM' , absum
 
      DO i = 1 , 4
         chi(i) = 0.D0
         chds(i) = 0.D0
         chdh(i) = 0.D0
         chda(i) = 0.D0
         chdb(i) = 0.D0
         chdd(i) = 0.D0
         chdpe(i) = 0.D0
         chdpa(i) = 0.D0
         chdpb(i) = 0.D0
         chdpd(i) = 0.D0
         ampela(i,0) = 0.D0
         ampela(i,9) = 0.D0
         DO k = 1 , 4
            ampela(i,k) = 0.D0
            ampela(i,k+4) = 0.D0
            AMPvm(i,k) = 0.D0
            chi(i) = chi(i) + chifac(k,i)*absum(k)
            chds(i) = chds(i) + chifac(k,i)*ab(1,k)
            chdh(i) = chdh(i) + chifac(k,i)*ab(2,k)
            chda(i) = chda(i) + chifac(k,i)*ab(3,k)
            chdb(i) = chdb(i) + chifac(k,i)*ab(4,k)
            chdd(i) = chdd(i) + chifac(k,i)*ab(5,k)
            chdpe(i) = chdpe(i) + chifac(k,i)*ab(6,k)
            chdpa(i) = chdpa(i) + chifac(k,i)*ab(7,k)
            chdpb(i) = chdpb(i) + chifac(k,i)*ab(8,k)
            chdpd(i) = chdpd(i) + chifac(k,i)*ab(9,k)
         END DO
         IF ( chi(i).LT.-DEPS ) THEN
            IF ( IDEb(86).GE.0 ) THEN
               IF ( LPRi.GT.4 ) WRITE (LO,'(1X,A,I3,2E12.3)')
     &               'PHO_CHAN2A: neg.eigenvalue (I,B,CHI)' , i , Bb , 
     &              chi(i)
               IF ( LPRi.GT.4 ) WRITE (LO,'(5X,A,5E12.3)') 'E,CHIs:' , 
     &              ECM , (absum(k),k=1,4)
            END IF
         END IF
         IF ( ABS(chi(i)).GT.200.D0 ) THEN
            ex1chi(i) = 0.D0
            ex2chi(i) = 0.D0
         ELSE
            tmp = EXP(-chi(i))
            ex1chi(i) = tmp
            ex2chi(i) = tmp*tmp
         END IF
      END DO
      IF ( IDEb(86).GE.20 ) THEN
         IF ( LPRi.GT.4 ) WRITE (LO,'(1X,A,4E12.3)')
     &         'PHO_CHAN2A: EX1CHI' , ex1chi
      END IF
 
      ampela(1,0) = 4.D0
      DO k = 1 , 4
         DO j = 1 , 4
            cfac = 2.D0*expfac(j,k)*ex2chi(j)
            ampela(k,0) = ampela(k,0) - expfac(j,k)*ex1chi(j)
            ampela(k,1) = ampela(k,1) + cfac*chds(j)
            ampela(k,2) = ampela(k,2) + cfac*chdh(j)
            ampela(k,3) = ampela(k,3) - cfac*chda(j)
            ampela(k,4) = ampela(k,4) - cfac*chdb(j)
            ampela(k,5) = ampela(k,5) - cfac*chdd(j)
            ampela(k,6) = ampela(k,6) + cfac*chdpe(j)
            ampela(k,7) = ampela(k,7) + cfac*chdpa(j)
            ampela(k,8) = ampela(k,8) + cfac*chdpb(j)
            ampela(k,9) = ampela(k,9) + cfac*chdpd(j)
         END DO
      END DO
      IF ( IDEb(86).GE.25 ) THEN
         DO i = 1 , 9
            IF ( LPRi.GT.4 ) WRITE (LO,'(1X,A,I3,4E10.3)')
     &            'PHO_CHAN2A: AMPELA(1-4,I)' , i , (ampela(k,1),k=1,4)
         END DO
      END IF
 
C  VDM factors --> amplitudes
C  low mass excitations
      DO i = 1 , 4
         ampcha(i) = 0.D0
         DO k = 1 , 4
            ampcha(i) = ampcha(i) + AMPfac(k)*ampela(ieltab(k,i),0)
         END DO
      END DO
      ampvme = ampcha(1)/EIGHT
      AMLmsd(1) = ampcha(2)/EIGHT
      AMLmsd(2) = ampcha(3)/EIGHT
      AMLmdd = ampcha(4)/EIGHT
C  elastic part, high mass diffraction
      AMPel = 0.5D0*ZXD(1,1)*EXP(-b24/BXD(1,1))
      AMPsof = 0.D0
      AMPhar = 0.D0
      AMHmsd(1) = 0.D0
      AMHmsd(2) = 0.D0
      AMHmdd = 0.D0
      AMPdp(1) = 0.D0
      AMPdp(2) = 0.D0
      AMPdp(3) = 0.D0
      AMPdp(4) = 0.D0
      DO i = 1 , 4
         AMPel = AMPel + ELAfac(i)*ampela(i,0)/8.D0
         AMPsof = AMPsof + ELAfac(i)*ampela(i,1)
         AMPhar = AMPhar + ELAfac(i)*ampela(i,2)
         AMHmsd(1) = AMHmsd(1) + ELAfac(i)*ampela(i,3)
         AMHmsd(2) = AMHmsd(2) + ELAfac(i)*ampela(i,4)
         AMHmdd = AMHmdd + ELAfac(i)*ampela(i,5)
         AMPdp(1) = AMPdp(1) + ELAfac(i)*ampela(i,6)
         AMPdp(2) = AMPdp(2) + ELAfac(i)*ampela(i,7)
         AMPdp(3) = AMPdp(3) + ELAfac(i)*ampela(i,8)
         AMPdp(4) = AMPdp(4) + ELAfac(i)*ampela(i,9)
      END DO
      AMPsof = AMPsof/16.D0
      AMPhar = AMPhar/16.D0
      AMHmsd(1) = AMHmsd(1)/16.D0
      AMHmsd(2) = AMHmsd(2)/16.D0
      AMHmdd = AMHmdd/16.D0
      AMPdp(1) = AMPdp(1)/16.D0
      AMPdp(2) = AMPdp(2)/16.D0
      AMPdp(3) = AMPdp(3)/16.D0
      AMPdp(4) = AMPdp(4)/16.D0
      IF ( DREAL(AMHmsd(1)).LE.0.D0 ) AMHmsd(1) = 0.D0
      IF ( DREAL(AMHmsd(2)).LE.0.D0 ) AMHmsd(2) = 0.D0
      IF ( DREAL(AMHmdd).LE.0.D0 ) AMHmdd = 0.D0
      IF ( DREAL(AMPdp(1)).LE.0.D0 ) AMPdp(1) = 0.D0
      IF ( DREAL(AMPdp(2)).LE.0.D0 ) AMPdp(2) = 0.D0
      IF ( DREAL(AMPdp(3)).LE.0.D0 ) AMPdp(3) = 0.D0
      IF ( DREAL(AMPdp(4)).LE.0.D0 ) AMPdp(4) = 0.D0
 
C  vector-meson production, weight factors
      IF ( (IFPap(1).EQ.22) .OR. (IFPap(2).EQ.22) ) THEN
         IF ( IFPap(1).EQ.22 ) THEN
            IF ( IFPap(2).EQ.22 ) THEN
               DO i = 1 , 4
                  DO j = 1 , 4
                     AMPvm(i,j) = PARmdl(9+i)*PARmdl(9+j)*ampvme
                  END DO
               END DO
            ELSE
               AMPvm(1,1) = PARmdl(10)*ampvme
               AMPvm(2,1) = PARmdl(11)*ampvme
               AMPvm(3,1) = PARmdl(12)*ampvme
               AMPvm(4,1) = PARmdl(13)*ampvme
            END IF
         ELSE IF ( IFPap(2).EQ.22 ) THEN
            AMPvm(1,1) = PARmdl(10)*ampvme
            AMPvm(1,2) = PARmdl(11)*ampvme
            AMPvm(1,3) = PARmdl(12)*ampvme
            AMPvm(1,4) = PARmdl(13)*ampvme
         END IF
      END IF
C  debug output
      IF ( IDEb(86).GE.10 ) THEN
         IF ( LPRi.GT.4 ) WRITE (LO,'(/,1X,A)')
     &         'PHO_CHAN2A: impact parameter amplitudes'
         IF ( LPRi.GT.4 ) WRITE (LO,'(1X,A,1P,2E12.3)') '       AMPEL' , 
     &        AMPel
         IF ( LPRi.GT.4 ) WRITE (LO,'(1X,A,1P,8E10.3)') 'AMPVM(1,1-4)' , 
     &        (AMPvm(1,k),k=1,4)
         IF ( LPRi.GT.4 ) WRITE (LO,'(1X,A,1P,8E10.3)') 'AMPVM(2,1-4)' , 
     &        (AMPvm(2,k),k=1,4)
         IF ( LPRi.GT.4 ) WRITE (LO,'(1X,A,1P,8E10.3)') 'AMPVM(3,1-4)' , 
     &        (AMPvm(3,k),k=1,4)
         IF ( LPRi.GT.4 ) WRITE (LO,'(1X,A,1P,8E10.3)') 'AMPVM(4,1-4)' , 
     &        (AMPvm(4,k),k=1,4)
         IF ( LPRi.GT.4 ) WRITE (LO,'(1X,A,1P,4E12.3)') '  AMPSOF/HAR' , 
     &        AMPsof , AMPhar
         IF ( LPRi.GT.4 ) WRITE (LO,'(1X,A,1P,4E12.3)') '      AMLMSD' , 
     &        AMLmsd
         IF ( LPRi.GT.4 ) WRITE (LO,'(1X,A,1P,4E12.3)') '      AMHMSD' , 
     &        AMHmsd
         IF ( LPRi.GT.4 ) WRITE (LO,'(1X,A,1P,2E12.3)') '      AMLMDD' , 
     &        AMLmdd
         IF ( LPRi.GT.4 ) WRITE (LO,'(1X,A,1P,2E12.3)') '      AMHMDD' , 
     &        AMHmdd
         IF ( LPRi.GT.4 ) WRITE (LO,'(1X,A,1P,8E10.3)') '  AMPDP(1-4)' , 
     &        AMPdp
      END IF
 
      END SUBROUTINE
