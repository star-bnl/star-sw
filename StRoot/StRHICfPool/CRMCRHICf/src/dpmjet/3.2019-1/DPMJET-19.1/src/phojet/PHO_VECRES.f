
      SUBROUTINE PHO_VECRES(Ivec,Rmass,Idpdg,Idbam)
C**********************************************************************
C
C     sampling of vector meson resonance in diffractive processes
C     (nothing done for hadrons)
C
C     input:   /POSVDM/     VDMFAC factors
C
C     output:  IVEC         0   incoming hadron
C                           1   rho 0
C                           2   omega
C                           3   phi
C                           4   pi+/pi- background
C              RMASS        mass of vector meson (GeV)
C              IDPDG        particle ID according to PDG
C              IDBAM        particle ID according to CPC
C
C**********************************************************************
      IMPLICIT NONE
      DOUBLE PRECISION DT_RNDM , EPS , PHO_PMASS , rmas1 , rmas2 , 
     &                 Rmass , sum , xi
      INTEGER idba1 , idba2 , Idbam , Idpdg , idpdo , ifl1 , ifl2 , 
     &        IPHO_ID2PDG , IPHO_PDG2ID , itrans , Ivec , k
      SAVE 
 
      PARAMETER (EPS=1.D-10)
 
C  input/output channels
      INCLUDE 'inc/poinou'
C  event debugging information
      INCLUDE 'inc/podebg'
C  nucleon-nucleus / nucleus-nucleus interface to DPMJET
      INCLUDE 'inc/pohdfl'
C  parameters of the "simple" Vector Dominance Model
      INCLUDE 'inc/posvdm'
C  some constants
      INCLUDE 'inc/pocons'
 
C  particle code translation
      DIMENSION itrans(4)
C                  rho0,omega,phi,pi+/pi-
      DATA itrans/113 , 223 , 333 , 92/
 
      idpdo = Idpdg
C
C  vector meson production
      IF ( Idpdg.EQ.22 ) THEN
         xi = DT_RNDM(Rmass)*(VMFa(1)+VMFa(2)+VMFa(3)+VMFa(4))
         sum = 0.D0
         DO k = 1 , 4
            sum = sum + VMFa(k)
            IF ( xi.LE.sum ) GOTO 50
         END DO
C
 50      Idpdg = itrans(k)
         Idbam = IPHO_PDG2ID(Idpdg)
         Ivec = k
C  sample mass of vector meson
         CALL PHO_SAMASS(Idpdg,Rmass)
 
C  hadronic resonance of multi-pomeron coupling
      ELSE IF ( Idpdg.EQ.990 ) THEN
         k = 4
         Idpdg = 91
         Idbam = IPHO_PDG2ID(Idpdg)
         Ivec = 4
C  sample mass of two-pion system
         CALL PHO_SAMASS(Idpdg,Rmass)
 
C  hadron remnants in inucleus interactions
      ELSE IF ( Idpdg.EQ.81 ) THEN
         IF ( IHFld(1,1).EQ.0 ) THEN
            CALL PHO_SEAFLA(1,ifl1,ifl2,Rmass)
            CALL PHO_HACODE(ifl1,ifl2,idba1,idba2)
         ELSE
            CALL PHO_HACODE(IHFld(1,1),IHFld(1,2),idba1,idba2)
         END IF
         rmas1 = PHO_PMASS(idba1,0)
         rmas2 = PHO_PMASS(idba2,0)
         IF ( (idba2.NE.0) .AND. 
     &        (DT_RNDM(rmas1).LT.(rmas1/(rmas1+rmas2))) ) THEN
            Idbam = idba2
            Rmass = rmas2
         ELSE
            Idbam = idba1
            Rmass = rmas1
         END IF
         Idpdg = IPHO_ID2PDG(Idbam)
         Ivec = 0
      ELSE IF ( Idpdg.EQ.82 ) THEN
         IF ( IHFld(2,1).EQ.0 ) THEN
            CALL PHO_SEAFLA(2,ifl1,ifl2,Rmass)
            CALL PHO_HACODE(ifl1,ifl2,idba1,idba2)
         ELSE
            CALL PHO_HACODE(IHFld(2,1),IHFld(2,2),idba1,idba2)
         END IF
         rmas1 = PHO_PMASS(idba1,0)
         rmas2 = PHO_PMASS(idba2,0)
         IF ( (idba2.NE.0) .AND. 
     &        (DT_RNDM(rmas1).LT.(rmas1/(rmas1+rmas2))) ) THEN
            Idbam = idba2
            Rmass = rmas2
         ELSE
            Idbam = idba1
            Rmass = rmas1
         END IF
         Idpdg = IPHO_ID2PDG(Idbam)
         Ivec = 0
      END IF
C  debug output
      IF ( IDEb(47).GE.5 ) THEN
         IF ( LPRi.GT.4 ) WRITE (LO,'(1X,A,/10X,3I7,E12.4)')
     &         'PHO_VECRES: IDPDG-OLD,IDPDG,IDBAM,MASS' , idpdo , 
     &        Idpdg , Idbam , Rmass
      END IF
 
      END SUBROUTINE
