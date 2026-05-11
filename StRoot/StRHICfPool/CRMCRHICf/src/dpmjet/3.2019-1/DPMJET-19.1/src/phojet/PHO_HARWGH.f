
      SUBROUTINE PHO_HARWGH(Pds,Pda,Pdb,Fdistr)
C***********************************************************************
C
C     calculate product of PDFs and coupling constants
C     according to selected MSPR (process type)
C
C     input:    /POCKIN/
C
C     output:   PDS     resulting from PDFs alone
C               FDISTR  complete weight function
C               PDA,PDB fields containing the PDFs
C
C***********************************************************************
      IMPLICIT NONE
      DOUBLE PRECISION alpha1 , alpha2 , Fdistr , Pda , Pdb , Pds , s2 , 
     &                 s3 , s4 , s5 , s6 , ssr , TINY , TINY6
      INTEGER i
      SAVE 
 
      PARAMETER (TINY=1.D-30,TINY6=1.D-06)
 
C  input/output channels
      INCLUDE 'inc/poinou'
C  event debugging information
      INCLUDE 'inc/podebg'
C  current beam selection
      INCLUDE 'inc/pobeam'
C  model switches and parameters
      INCLUDE 'inc/pomdls'
C  data of c.m. system of Pomeron / Reggeon exchange
      INCLUDE 'inc/popcms'
C  currently activated parton density parametrizations
      INCLUDE 'inc/poppdf'
C  hard scattering parameters used for most recent hard interaction
      INCLUDE 'inc/pohapa'
C  some hadron information, will be deleted in future versions
      INCLUDE 'inc/pohdrn'
C  scale parameters for parton model calculations
      INCLUDE 'inc/pohscl'
C  data on most recent hard scattering
      INCLUDE 'inc/pockin'
C  hard cross sections and MC selection weights
      INCLUDE 'inc/pohrcs'
C  some constants
      INCLUDE 'inc/pocons'
 
      DOUBLE PRECISION PHO_ALPHAS , PHO_ALPHAE
      DIMENSION Pda(-6:6) , Pdb(-6:6)
 
      Fdistr = 0.D0
C  set hard scale  QQ  for alpha and partondistr.
      IF ( NQQal.EQ.1 ) THEN
         QQAl = AQQal*PT*PT
      ELSE IF ( NQQal.EQ.2 ) THEN
         QQAl = AQQal*X1*X2*ECMp*ECMp*U*V/(1.D0+V*V+U*U)
      ELSE IF ( NQQal.EQ.3 ) THEN
         QQAl = AQQal*X1*X2*ECMp*ECMp
      ELSE IF ( NQQal.EQ.4 ) THEN
         QQAl = AQQal*X1*X2*ECMp*ECMp*(U*V)**(1.D0/3.D0)
      END IF
      IF ( NQQpd.EQ.1 ) THEN
         QQPd = AQQpd*PT*PT
      ELSE IF ( NQQpd.EQ.2 ) THEN
         QQPd = AQQpd*X1*X2*ECMp*ECMp*U*V/(1.D0+V*V+U*U)
      ELSE IF ( NQQpd.EQ.3 ) THEN
         QQPd = AQQpd*X1*X2*ECMp*ECMp
      ELSE IF ( NQQpd.EQ.4 ) THEN
         QQPd = AQQpd*X1*X2*ECMp*ECMp*(U*V)**(1.D0/3.D0)
      END IF
C  coupling constants, PDFs
      IF ( MSPr.LT.9 ) THEN
         alpha1 = PHO_ALPHAS(QQAl,3)
         alpha2 = alpha1
         CALL PHO_PDF(1,X1,QQPd,0.D0,Pda)
         CALL PHO_PDF(2,X2,QQPd,0.D0,Pdb)
         IF ( MSPr.EQ.1 .OR. MSPr.EQ.4 ) THEN
            Pds = Pda(0)*Pdb(0)
         ELSE
            s2 = 0.D0
            s3 = 0.D0
            s4 = 0.D0
            s5 = 0.D0
            DO i = 1 , NF
               s2 = s2 + Pda(i)*Pdb(-i) + Pda(-i)*Pdb(i)
               s3 = s3 + Pda(i)*Pdb(i) + Pda(-i)*Pdb(-i)
               s4 = s4 + Pda(i) + Pda(-i)
               s5 = s5 + Pdb(i) + Pdb(-i)
            END DO
            IF ( (MSPr.EQ.2) .OR. (MSPr.EQ.5) .OR. (MSPr.EQ.6) ) THEN
               Pds = s2
            ELSE IF ( (MSPr.EQ.3) .OR. (MSPr.EQ.-1) ) THEN
               Pds = Pda(0)*s5 + Pdb(0)*s4
            ELSE IF ( MSPr.EQ.7 ) THEN
               Pds = s3
            ELSE IF ( MSPr.EQ.8 ) THEN
               Pds = s4*s5 - (s2+s3)
            END IF
         END IF
      ELSE IF ( MSPr.LT.12 ) THEN
         alpha2 = PHO_ALPHAS(QQAl,2)
         IF ( IDPdg1.EQ.22 ) THEN
            alpha1 = PHO_ALPHAE(QQAl)
         ELSE IF ( IDPdg1.EQ.990 ) THEN
            alpha1 = PARmdl(74)
         END IF
         CALL PHO_PDF(2,X2,QQPd,0.D0,Pdb)
         s4 = 0.D0
         s6 = 0.D0
         DO i = 1 , NF
            s4 = s4 + Pdb(i) + Pdb(-i)
C  charge counting
C         IF(MOD(I,2).EQ.0) THEN
C           S6  = S6+(PDB(I)+PDB(-I))*4.D0/9.D0
C         ELSE
C           S6  = S6+(PDB(I)+PDB(-I))*1.D0/9.D0
C         ENDIF
            s6 = s6 + (Pdb(i)+Pdb(-i))*Q_Ch2(i)
         END DO
         IF ( MSPr.NE.10 ) THEN
            Pds = Pdb(0)
         ELSE IF ( IDPdg1.EQ.990 ) THEN
            Pds = s4
         ELSE
            Pds = s6
         END IF
      ELSE IF ( MSPr.LT.14 ) THEN
         alpha1 = PHO_ALPHAS(QQAl,1)
         IF ( IDPdg2.EQ.22 ) THEN
            alpha2 = PHO_ALPHAE(QQAl)
         ELSE IF ( IDPdg2.EQ.990 ) THEN
            alpha2 = PARmdl(74)
         END IF
         CALL PHO_PDF(1,X1,QQPd,0.D0,Pda)
         s4 = 0.D0
         s6 = 0.D0
         DO i = 1 , NF
            s4 = s4 + Pda(i) + Pda(-i)
C  charge counting
C         IF(MOD(I,2).EQ.0) THEN
C           S6  = S6+(PDA(I)+PDA(-I))*4.D0/9.D0
C         ELSE
C           S6  = S6+(PDA(I)+PDA(-I))*1.D0/9.D0
C         ENDIF
            s6 = s6 + (Pda(i)+Pda(-i))*Q_Ch2(i)
         END DO
         IF ( MSPr.NE.12 ) THEN
            Pds = Pda(0)
         ELSE IF ( IDPdg2.EQ.990 ) THEN
            Pds = s4
         ELSE
            Pds = s6
         END IF
      ELSE IF ( MSPr.EQ.14 ) THEN
         ssr = X1*X2*ECMp*ECMp
         IF ( IDPdg1.EQ.22 ) THEN
            alpha1 = PHO_ALPHAE(ssr)
         ELSE IF ( IDPdg1.EQ.990 ) THEN
            alpha1 = PARmdl(74)
         END IF
         IF ( IDPdg2.EQ.22 ) THEN
            alpha2 = PHO_ALPHAE(ssr)
         ELSE IF ( IDPdg2.EQ.990 ) THEN
            alpha2 = PARmdl(74)
         END IF
         Pds = 1.D0
      ELSE
         IF ( LPRi.GT.4 ) WRITE (LO,'(/1X,A,I4)')
     &         'PHO_HARWGH:ERROR: invalid hard process number (MSPR)' , 
     &        MSPr
         CALL PHO_ABORT
      END IF
 
C  complete weight
      Fdistr = HFAc(MSPr,IDXmpar)*alpha1*alpha2*Pds
 
C  debug output
      IF ( LPRi.GT.4 .AND. IDEb(15).GE.20 )
     &      WRITE (LO,'(1X,A,/5X,I3,2I6,4E10.3)')
     &      'PHO_HARWGH: MSPR,ID1,ID2,AL1,AL2,PDS,FDIS' , MSPr , 
     &     IDPdg1 , IDPdg2 , alpha1 , alpha2 , Pds , Fdistr
 
      END SUBROUTINE
