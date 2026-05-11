
      SUBROUTINE PHO_ACTPDF(Idpdg,K)
C***************************************************************
C
C     activate PDF for QCD calculations
C
C     input:      IDPDG    PDG particle number
C                 K        1  first PDF in /POPPDF/
C                          2  second PDF in /POPPDF/
C                         -2  write current settings
C
C     output:     /POPPDF/
C
C***************************************************************
      IMPLICIT NONE
      DOUBLE PRECISION alam2 , dummy , PHO_ALPHAS , q2ma , xma , xmi
      INTEGER Idpdg , K
      SAVE 
 
C  input/output channels
      INCLUDE 'inc/poinou'
C  event debugging information
      INCLUDE 'inc/podebg'
C  currently activated parton density parametrizations
      INCLUDE 'inc/poppdf'
C  currently activated parton density parametrizations
      INCLUDE 'inc/pohdfl'
 
      IF ( K.GT.0 ) THEN
 
C  read PDF from table
         CALL PHO_SETPDF(Idpdg,ITYpe(K),IGRp(K),ISEt(K),IEXt(K),IPAva(K)
     &                   ,1)
 
C** pass proper mother information to PDF routine.
         IF ( (Idpdg.EQ.81) .OR. (Idpdg.EQ.82) ) THEN
            IPArid(K) = IDEqp(MOD(Idpdg,80))
         ELSE
            IPArid(K) = Idpdg
         END IF
C  get PDF parameters
         CALL PHO_GETPDF(K,PDFnam(K),PDFlam(K),PDFq2m(K),q2ma,xmi,xma)
C  initialize alpha_s calculation
         alam2 = PDFlam(K)*PDFlam(K)
         dummy = PHO_ALPHAS(alam2,-K)
 
         IF ( IDEb(2).GE.20 ) THEN
            IF ( LPRi.GT.4 ) WRITE (LO,'(1X,A)') 
     &          'PHO_ACTPDF: LAMBDA,Q2MIN,NAME,ITYPE,IPAR,ISET,IEXT,PAR'
            IF ( LPRi.GT.4 ) WRITE (LO,'(1X,A,I2,2E12.3,2X,A8,4I4,I7)')
     &            'SIDE' , K , PDFlam(K) , PDFq2m(K) , PDFnam(K) , 
     &           ITYpe(K) , IGRp(K) , ISEt(K) , IEXt(K) , IPArid(K)
         END IF
         NPAold = K
 
      ELSE IF ( K.EQ.-2 ) THEN
 
C  write table of current PDFs
         IF ( LPRi.GT.4 ) WRITE (LO,'(1X,A)')
     &         'PHO_ACTPDF: LAMBDA,Q2MIN,NAME,ITYPE,IPAR,ISET,IEXT,PAR'
         IF ( LPRi.GT.4 ) WRITE (LO,'(1X,A,2E12.3,2X,A8,4I4,I7)')
     &         'SIDE 1:' , PDFlam(1) , PDFq2m(1) , PDFnam(1) , ITYpe(1)
     &        , IGRp(1) , ISEt(1) , IEXt(1) , IPArid(1)
         IF ( LPRi.GT.4 ) WRITE (LO,'(1X,A,2E12.3,2X,A8,4I4,I7)')
     &         'SIDE 2:' , PDFlam(2) , PDFq2m(2) , PDFnam(2) , ITYpe(2)
     &        , IGRp(2) , ISEt(2) , IEXt(2) , IPArid(2)
 
      ELSE
 
         IF ( LPRi.GT.4 ) WRITE (LO,'(/1X,A,2I4)')
     &         'PHO_ACTPDF:ERROR: invalid arguments' , Idpdg , K
         CALL PHO_ABORT
 
      END IF
 
      END SUBROUTINE
