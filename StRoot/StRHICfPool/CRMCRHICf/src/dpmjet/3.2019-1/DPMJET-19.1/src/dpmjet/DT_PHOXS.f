
      SUBROUTINE DT_PHOXS(Kproj,Ktarg,Ecm,Plab,Stot,Sine,Sdif1,Bel,Mode)
 
C***********************************************************************
C Total/inelastic proton-nucleon cross sections taken from PHOJET-     *
C interpolation tables.                                                *
C This version dated 05.11.97 is written by S. Roesler                 *
C***********************************************************************
 
      IMPLICIT NONE
      DOUBLE PRECISION alphap , Bel , deldl , dum , Ecm , epn , epsdl , 
     &                 GEV2MB , ONE , PI , Plab , s , Sdif1 , sigel , 
     &                 Sine
      DOUBLE PRECISION Stot , TWO , TWOPI , ZERO
      INTEGER IDT_IPDGHA , idum , Kproj , Ktarg , Mode
      SAVE 
 
      INCLUDE 'inc/dtflka'
 
      PARAMETER (ZERO=0.0D0,ONE=1.0D0,TWO=2.0D0)
      PARAMETER (TWOPI=6.283185307179586454D+00,PI=TWOPI/TWO,
     &           GEV2MB=0.38938D0)
 
      LOGICAL lfirst
      DATA lfirst/.TRUE./
 
Cf2py intent(out) STOT,SINE,SDIF1,BEL
 
C nucleon-nucleon event-generator
      INCLUDE 'inc/dtmodl'
C particle properties (BAMJET index convention)
      INCLUDE 'inc/dtpart'
 
C  current beam selection
      INCLUDE 'inc/pobeam'
C  cross-section common block
      INCLUDE 'inc/pocsec'
C*
      IF ( (MCGene.NE.2) .AND. (Mode.NE.1) ) THEN
 
         IF ( LPRi.GT.4 ) WRITE (LOUt,*) MCGene
C1000    FORMAT(1X,'PHOXS: warning! PHOJET not initialized (',I2,')')
         STOP
      END IF
 
      IF ( Ecm.LE.ZERO ) THEN
         epn = SQRT(AAM(Kproj)**2+Plab**2)
         Ecm = SQRT(AAM(Kproj)**2+AAM(Ktarg)**2+2.0D0*epn*AAM(Ktarg))
      END IF
 
      IF ( Mode.EQ.1 ) THEN
C DL
         deldl = 0.0808D0
         epsdl = -0.4525D0
         s = Ecm*Ecm
         Stot = 21.7D0*s**deldl + 56.08D0*s**epsdl
         alphap = 0.25D0
         Bel = 8.5D0 + 2.D0*alphap*LOG(s)
         sigel = Stot**2/(16.D0*PI*Bel*GEV2MB)
         Sine = Stot - sigel
         Sdif1 = ZERO
      ELSE
C Phojet
C         IP = 1
C         IF(ECM.LE.SIGECM(1,IP,IDXMPAR)) THEN
C           I1 = 1
C           I2 = 1
C         ELSEIF (ECM.LT.SIGECM(ISIMAX(IDXMPAR),IP,IDXMPAR)) THEN
C           DO 1 I=2,ISIMAX(IDXMPAR)
C              IF (ECM.LE.SIGECM(I,IP,IDXMPAR)) GOTO 2
C    1      CONTINUE
C    2      CONTINUE
C           I1 = I-1
C           I2 = I
C         ELSE
C           IF (LFIRST) THEN
C
C              IF (LPRI.GT.4)
C     .        WRITE(LOUT,'(/1X,A,2E12.3)')
C     &          'PHOXS: warning! energy above initialization limit (',
C     &          ECM,SIGECM(ISIMAX(IDXMPAR),IP,IDXMPAR)
C             LFIRST = .FALSE.
C           ENDIF
C           I1 = ISIMAX(IDXMPAR)
C           I2 = ISIMAX(IDXMPAR)
C         ENDIF
C         FAC2 = ZERO
C         IF (I1.NE.I2) FAC2 = LOG(ECM/SIGECM(I1,IP,IDXMPAR))
C     &                       /LOG(SIGECM(I2,IP,IDXMPAR)/
C     &                        SIGECM(I1,IP,IDXMPAR))
C         FAC1  = ONE-FAC2
C         STOT  = FAC2*SIGTAB( 1,I2,IP,IDXMPAR)
C     &          +FAC1*SIGTAB( 1,I1,IP,IDXMPAR)
C         SINE  = FAC2*SIGTAB(28,I2,IP,IDXMPAR)
C     &          +FAC1*SIGTAB(28,I1,IP,IDXMPAR)
C         SDIF1 = FAC2*(SIGTAB(30,I2,IP,IDXMPAR)+
C     &           SIGTAB(32,I2,IP,IDXMPAR))+
C     &           FAC1*(SIGTAB(30,I1,IP,IDXMPAR)+
C     &           SIGTAB(32,I1,IP,IDXMPAR))
C         BEL   = FAC2*SIGTAB(39,I2,IP,IDXMPAR)+
C     &           FAC1*SIGTAB(39,I1,IP,IDXMPAR)
 
C     let PHOJET know about projectile and target and initialize the index
         CALL PHO_SETPAR(1,IDT_IPDGHA(Kproj),0,Kproj)
         CALL PHO_SETPAR(2,IDT_IPDGHA(Ktarg),0,Ktarg)
         CALL PHO_SETPCOMB
 
C     Calculate cross-sections in ordinary PHOJET interpolation routine
         CALL PHO_CSINT(1,IDT_IPDGHA(Kproj),IDT_IPDGHA(Ktarg),idum,idum,
     &                  Ecm,dum,dum)
C     Read out the result
         Stot = SIGtot
         Sine = SIGine
C     Single diffractive cross-section
         Sdif1 = SIGlsd(1) + SIGhsd(1) + SIGlsd(2) + SIGhsd(2)
         Bel = SLOel
      END IF
 
      END SUBROUTINE
