
      SUBROUTINE DT_SHNDIF(Ecm,Kproj,Ktarg,Sigdif,Sigdih)
 
C*********************************************************************
C   Single diffractive hadron-nucleon cross sections                 *
C                                              S.Roesler 14/1/93     *
C                                                                    *
C   The cross sections are calculated from extrapolated single       *
C   diffractive antiproton-proton cross sections (DTUJET92) using    *
C   scaling relations between total and single diffractive cross     *
C   sections.                                                        *
C*********************************************************************
 
      IMPLICIT NONE
      DOUBLE PRECISION chmsd1 , chmsd4 , chmsd5 , csd1 , csd4 , csd5 , 
     &                 dumzer , Ecm , epn , f , frac , ppn , sdiapp , 
     &                 shmsd , Sigdif , Sigdih , sigel , sigto , ZERO
      INTEGER Kproj , kpscal , kt , Ktarg , ktscal
      SAVE 
      PARAMETER (ZERO=0.0D0)
 
C particle properties (BAMJET index convention)
      INCLUDE 'inc/dtpart'
C
      csd1 = 4.201483727D0
      csd4 = -0.4763103556D-02
      csd5 = 0.4324148297D0
C
      chmsd1 = 0.8519297242D0
      chmsd4 = -0.1443076599D-01
      chmsd5 = 0.4014954567D0
C
      epn = (Ecm**2-AAM(Kproj)**2-AAM(Ktarg)**2)/(2.0D0*AAM(Ktarg))
      ppn = SQRT((epn-AAM(Kproj))*(epn+AAM(Kproj)))
C
      sdiapp = csd1 + csd4*LOG(ppn)**2 + csd5*LOG(ppn)
      shmsd = chmsd1 + chmsd4*LOG(ppn)**2 + chmsd5*LOG(ppn)
      frac = shmsd/sdiapp
C
      IF ( Kproj.EQ.2 .OR. Kproj.EQ.9 .OR. Kproj.EQ.12 .OR. 
     &     Kproj.EQ.13 .OR. Kproj.EQ.14 .OR. Kproj.EQ.15 .OR. 
     &     Kproj.EQ.16 .OR. Kproj.EQ.18 .OR. Kproj.EQ.19 .OR. 
     &     Kproj.EQ.23 .OR. Kproj.EQ.24 .OR. Kproj.EQ.25 ) THEN
C
C
         kpscal = 2
         ktscal = 1
C     F      = SDIAPP/DT_SHNTOT(KPSCAL,KTSCAL,ECM,ZERO)
         dumzer = ZERO
         CALL DT_XSHN(kpscal,ktscal,dumzer,Ecm,sigto,sigel)
         f = sdiapp/sigto
         kt = 1
C     SIGDIF = DT_SHNTOT(KPROJ,KT,ECM,ZERO)*F
         CALL DT_XSHN(Kproj,kt,dumzer,Ecm,sigto,sigel)
         Sigdif = sigto*f
         Sigdih = frac*Sigdif
         RETURN
      ELSE IF ( Kproj.EQ.3 .OR. Kproj.EQ.4 .OR. Kproj.EQ.5 .OR. 
     &          Kproj.EQ.6 .OR. Kproj.EQ.7 .OR. Kproj.EQ.10 .OR. 
     &          Kproj.EQ.11 ) THEN
C
C-------------------------- leptons..
         Sigdif = 1.D-10
         Sigdih = 1.D-10
         GOTO 99999
      END IF
C
C---------------------------- p - p , n - p , sigma0+- - p ,
C                             Lambda - p
      csd1 = 6.004476070D0
      csd4 = -0.1257784606D-03
      csd5 = 0.2447335720D0
      Sigdif = csd1 + csd4*LOG(ppn)**2 + csd5*LOG(ppn)
      Sigdih = frac*Sigdif
99999 END SUBROUTINE
