
      SUBROUTINE PHO_GETPDF(Npar,Pdfna,Ala,Q2mi,Q2ma,Xmi,Xma)
C***************************************************************
C
C     get PDF information
C
C     input:      NPAR     1  first PDF in /POPPDF/
C                          2  second PDF in /POPPDF/
C
C     output:     PDFNA    name of PDf parametrization
C                 ALA      QCD LAMBDA (4 flavours, in GeV)
C                 Q2MI     minimal Q2
C                 Q2MA     maximal Q2
C                 XMI      minimal X
C                 XMA      maximal X
C
C***************************************************************
      IMPLICIT NONE
      DOUBLE PRECISION Ala , Q2ma , Q2mi , qini , qmax , value , Xma , 
     &                 Xmi
      INTEGER nflav , nloops , Npar
      SAVE 
 
      CHARACTER*8 Pdfna
      CHARACTER*1024 fname
C  input/output channels
      INCLUDE 'inc/poinou'
 
C  PHOLIB 4.15 common
      INCLUDE 'inc/w50512'
      INCLUDE 'inc/w50513'
 
C  PHOPDF version 2.0 common
      INCLUDE 'inc/phcom1'
      INCLUDE 'inc/phcom2'
 
C  currently activated parton density parametrizations
      INCLUDE 'inc/poppdf'

      LOGICAL ct14init(3)
      DATA ct14init/.FALSE. , .FALSE. , .FALSE./
 
      DIMENSION param(20) , value(20)
      CHARACTER*20 param
 
      IF ( (Npar.NE.1) .AND. (Npar.NE.2) ) THEN
         IF ( LPRi.GT.4 ) WRITE (LO,'(/1X,A,I6)')
     &         'PHO_GETPDF:ERROR: invalid PDF number (1,2)' , Npar
         CALL PHO_ABORT
      END IF
      Ala = 0.D0
 
      IF ( IEXt(Npar).EQ.0 ) THEN
 
C  internal parametrizations
         IF ( IGRp(Npar).EQ.2 ) THEN
            IF ( ISEt(Npar).EQ.1 ) THEN
               IF ( .NOT.ct14init(1) ) THEN
                  fname = DATdir(1:LENdir)//'CT14LL.pds'
                  CALL PHO_SETCT14(fname, LENdir + 10)
                  ct14init(1) = .TRUE.
                  ct14init(2) = .FALSE.
                  ct14init(3) = .FALSE.
               END IF
               Pdfna = 'CT14-LO'
            ELSE IF ( ISEt(Npar).EQ.2 ) THEN
               IF ( .NOT.ct14init(2) ) THEN
                  fname = DATdir(1:LENdir)//'CT14LN.pds'
                  CALL PHO_SETCT14(fname, LENdir + 10)
                  ct14init(2) = .TRUE.
                  ct14init(1) = .FALSE.
                  ct14init(3) = .FALSE.
               END IF
               Pdfna = 'CT14-LLO'
            ELSE IF ( ISEt(Npar).EQ.3 ) THEN
               IF ( .NOT.ct14init(3) ) THEN
                  fname = DATdir(1:LENdir)//'CT14nlo_NF4.pds'
                  CALL PHO_SETCT14(fname, LENdir + 15)
                  ct14init(3) = .TRUE.
                  ct14init(1) = .FALSE.
                  ct14init(2) = .FALSE.
               END IF
               Pdfna = 'CT14-NLO'
            END IF
            CALL PHO_CT14GETPARS(XMIn,qini,qmax,nloops,nflav)
            Ala = 0.2807
            Xmi = XMIn
            Q2mi = qini**2
            Q2ma = qmax**2
         ELSE IF ( ITYpe(Npar).EQ.1 ) THEN
C  proton PDFs
            IF ( IGRp(Npar).EQ.5 ) THEN
               IF ( ISEt(Npar).EQ.3 ) THEN
                  Ala = 0.2D0
                  Q2mi = 0.3D0
                  Pdfna = 'GRV92 HO'
               ELSE IF ( ISEt(Npar).EQ.4 ) THEN
                  Ala = 0.2D0
                  Q2mi = 0.25D0
                  Pdfna = 'GRV92 LO'
               ELSE IF ( ISEt(Npar).EQ.5 ) THEN
                  Ala = 0.2D0
                  Q2mi = 0.4D0
                  Pdfna = 'GRV94 HO'
               ELSE IF ( ISEt(Npar).EQ.6 ) THEN
                  Ala = 0.2D0
                  Q2mi = 0.4D0
                  Pdfna = 'GRV94 LO'
               ELSE IF ( ISEt(Npar).EQ.7 ) THEN
                  Ala = 0.2D0
                  Q2mi = 0.4D0
                  Pdfna = 'GRV94 DI'
               ELSE IF ( ISEt(Npar).EQ.8 ) THEN
                  Ala = 0.175D0
                  Q2mi = 0.8D0
                  Pdfna = 'GRV98 LO'
               ELSE IF ( ISEt(Npar).EQ.9 ) THEN
                  Ala = 0.175D0
                  Q2mi = 0.8D0
                  Pdfna = 'GRV98 SC'
               END IF
            END IF
         ELSE IF ( ITYpe(Npar).EQ.2 ) THEN
C  pion PDFs
            IF ( IGRp(Npar).EQ.5 ) THEN
               IF ( ISEt(Npar).EQ.1 ) THEN
                  Ala = 0.2D0
                  Q2mi = 0.3D0
                  Pdfna = 'GRV-P HO'
               ELSE IF ( ISEt(Npar).EQ.2 ) THEN
                  Ala = 0.2D0
                  Q2mi = 0.25D0
                  Pdfna = 'GRV-PiLO'
               END IF
            END IF
         ELSE IF ( ITYpe(Npar).EQ.3 ) THEN
C  photon PDFs
            IF ( IGRp(Npar).EQ.5 ) THEN
               IF ( ISEt(Npar).EQ.1 ) THEN
                  Ala = 0.2D0
                  Q2mi = 0.3D0
                  Pdfna = 'GRV-G LH'
               ELSE IF ( ISEt(Npar).EQ.2 ) THEN
                  Ala = 0.2D0
                  Q2mi = 0.3D0
                  Pdfna = 'GRV-G HO'
               ELSE IF ( ISEt(Npar).EQ.3 ) THEN
                  Ala = 0.2D0
                  Q2mi = 0.25D0
                  Pdfna = 'GRV-G LO'
               END IF
            ELSE IF ( IGRp(Npar).EQ.8 ) THEN
               IF ( ISEt(Npar).EQ.1 ) THEN
                  Ala = 0.2D0
                  Q2mi = 4.D0
                  Pdfna = 'AGL-G LO'
               END IF
            END IF
         ELSE IF ( ITYpe(Npar).EQ.20 ) THEN
C  pomeron PDFs
            IF ( IGRp(Npar).EQ.4 ) THEN
               CALL PHO_CKMTPA(990,Xmi,Xma,Ala,Q2mi,Q2ma,Pdfna)
            ELSE
               Ala = 0.3D0
               Q2mi = 2.D0
               Pdfna = 'POM-PDF1'
            END IF
         END IF
 
C  external parametrizations
 
      ELSE IF ( IEXt(Npar).EQ.1 ) THEN
C  PDFLIB call: old numbering
         param(1) = 'MODE'
         param(2) = ' '
         value(1) = IGRp(Npar)
         CALL PDFSET(param,value)
         Q2mi = Q2Min
         Q2ma = Q2Max
         Xmi = XMIn
         Xma = XMAx
         Ala = QCDl4
         Pdfna = 'PDFLIB1'
      ELSE IF ( IEXt(Npar).EQ.2 ) THEN
C  PDFLIB call: new numbering
         param(1) = 'NPTYPE'
         param(2) = 'NGROUP'
         param(3) = 'NSET'
         param(4) = ' '
         value(1) = ITYpe(Npar)
         value(2) = IGRp(Npar)
         value(3) = ISEt(Npar)
         CALL PDFSET(param,value)
         Q2mi = Q2Min
         Q2ma = Q2Max
         Xmi = XMIn
         Xma = XMAx
         Ala = QCDl4
         Pdfna = 'PDFLIB2'
      ELSE IF ( IEXt(Npar).EQ.3 ) THEN
C  PHOLIB interface
         Ala = ALM(IGRp(Npar),ISEt(Npar))
         Q2mi = 2.D0
         Pdfna = CHPar(IGRp(Npar))
 
C  some special internal parametrizations
 
      ELSE IF ( IEXt(Npar).EQ.4 ) THEN
C  photon PDFs depending on virtualities
         IF ( IGRp(Npar).EQ.1 ) THEN
C  Schuler/Sjostrand parametrization
            Ala = 0.2D0
            IF ( ISEt(Npar).EQ.1 ) THEN
               Q2mi = 0.2D0
               Pdfna = 'SaS-1D  '
            ELSE IF ( ISEt(Npar).EQ.2 ) THEN
               Q2mi = 0.2D0
               Pdfna = 'SaS-1M  '
            ELSE IF ( ISEt(Npar).EQ.3 ) THEN
               Q2mi = 2.D0
               Pdfna = 'SaS-2D  '
            ELSE IF ( ISEt(Npar).EQ.4 ) THEN
               Q2mi = 2.D0
               Pdfna = 'SaS-2M  '
            END IF
         ELSE IF ( IGRp(Npar).EQ.5 ) THEN
C  Gluck/Reya/Stratmann parametrization
            IF ( ISEt(Npar).EQ.4 ) THEN
               Ala = 0.2D0
               Q2mi = 0.6D0
               Pdfna = 'GRS-G LO'
            END IF
         END IF
      ELSE IF ( IEXt(Npar).EQ.5 ) THEN
C  Schuler/Sjostrand anomalous only
         Ala = 0.2D0
         Q2mi = 0.2D0
         Pdfna = 'SaS anom'
      END IF
      IF ( Ala.LT.0.01D0 ) THEN
         IF ( LPRi.GT.4 ) WRITE (LO,'(/1X,2A,/10X,5I6)')
     &         'PHO_GETPDF:ERROR: ' , 
     &        'unsupported PDF (NPAR,IEXT,ITYPE,IGRP,ISET)' , Npar , 
     &        IEXt(Npar) , ITYpe(Npar) , IGRp(Npar) , ISEt(Npar)
         CALL PHO_ABORT
      END IF
 
      END SUBROUTINE
