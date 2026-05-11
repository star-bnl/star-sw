
      SUBROUTINE PHO_SETPDF(Idpdg,Ityp,Ipar,Iset,Iext,Ipaval,Mode)
C***************************************************************
C
C     assigns  PDF numbers to particles
C
C     input:      IDPDG    PDG number of particle
C                 ITYP     particle type
C                 IPAR     PDF paramertization
C                 ISET     number of set
C                 IEXT     library number for PDF calculation
C                 IPAVAL   (only output)
C                          1 PDF with valence quarks
C                          0 PDF without valence quarks
C                 MODE     -1   add entry to table
C                           1   read from table
C                           2   map particle IPAR to existing entry
C                           2   output of table
C
C***************************************************************
      IMPLICIT NONE
      INTEGER i , idcmp , Idpdg , ientry , Iext , ii , Ipar , Ipaval , 
     &        ipdfs , Iset , Ityp , ityp1 , Mode
      SAVE 
 
C  input/output channels
      INCLUDE 'inc/poinou'
C  event debugging information
      INCLUDE 'inc/podebg'
C  nucleon-nucleus / nucleus-nucleus interface to DPMJET
      INCLUDE 'inc/pohdfl'
 
      DIMENSION ipdfs(5,50)
      DATA ientry/0/
 
      IF ( Mode.EQ.1 ) THEN
         i = 1
         IF ( Idpdg.EQ.81 ) THEN
            idcmp = IDEqp(1)
            Ipaval = IHFls(1)
         ELSE IF ( Idpdg.EQ.82 ) THEN
            idcmp = IDEqp(2)
            Ipaval = IHFls(2)
         ELSE
            idcmp = Idpdg
            Ipaval = 1
         END IF
 50      IF ( idcmp.EQ.ipdfs(1,i) ) THEN
            Ityp = ipdfs(2,i)
            Ipar = ipdfs(3,i)
            Iset = ipdfs(4,i)
            Iext = ipdfs(5,i)
            IF ( LPRi.GT.4 .AND. IDEb(80).GE.15 )
     &            WRITE (LO,'(1X,A,I7,5X,3I4)')
     &            'PHO_SETPDF: ID,IPAR,ISET,IEXT' , idcmp , Ipar , 
     &           Iset , Iext
            RETURN
         END IF
         i = i + 1
         IF ( i.GT.ientry ) THEN
            IF ( LPRi.GT.4 ) WRITE (LO,'(/1X,A,I7)')
     &            'PHO_SETPDF: no PDF assigned to ' , idcmp
            CALL PHO_ABORT
         END IF
         GOTO 50
 
      ELSE IF ( Mode.EQ.2 ) THEN
         ii = 0
         DO i = 1 , ientry
            ii = ii + 1
            IF ( Idpdg.EQ.ipdfs(1,i) ) GOTO 100
         END DO
 
 100     IF ( ii.LE.ientry ) THEN
            ientry = ientry + 1
            ipdfs(1,ientry) = Ipar
            ipdfs(2,ientry) = ipdfs(2,ii)
            ipdfs(3,ientry) = ipdfs(3,ii)
            ipdfs(4,ientry) = ipdfs(4,ii)
            ipdfs(5,ientry) = ipdfs(5,ii)
            IF ( (LPRi.GT.4) .AND. (IDEb(80).GT.0) )
     &            WRITE (LO,'(1X,A,I7,A,I7)') 'PHO_SETPDF: Particle ' , 
     &           Ipar , ' mapped to ' , Idpdg
         ELSE
            WRITE (LO,'(1X,A,I7,A,I7)') 'PHO_SETPDF: PDF of particle' , 
     &             Ipar , 'unknown.'
            CALL PHO_ABORT
         END IF
 
      ELSE IF ( Mode.EQ.-1 ) THEN
         DO i = 1 , ientry
            IF ( Idpdg.EQ.ipdfs(1,i) ) THEN
               IF ( LPRi.GT.4 ) WRITE (LO,'(/1X,A,5I6)')
     &               'PHO_SETPDF: overwrite old particle PDF' , Idpdg , 
     &              ipdfs(2,i) , ipdfs(3,i) , ipdfs(4,i) , ipdfs(5,i)
               GOTO 150
            END IF
         END DO
         i = ientry + 1
         IF ( i.GT.50 ) THEN
            IF ( LPRi.GT.4 ) WRITE (LO,'(/1X,A,/1x,6I6)')
     &            'PHO_SETPDF:ERROR: no space left in IPDFS:' , i , 
     &           Idpdg , ipdfs(2,i) , ipdfs(3,i) , ipdfs(4,i) , 
     &           ipdfs(5,i)
            STOP
         END IF
         ientry = i
 150     ipdfs(1,i) = Idpdg
         IF ( Idpdg.EQ.990 ) THEN
            ityp1 = 20
         ELSE IF ( Idpdg.EQ.22 ) THEN
            ityp1 = 3
         ELSE IF ( ABS(Idpdg).LT.1000 ) THEN
            ityp1 = 2
         ELSE
            ityp1 = 1
         END IF
         ipdfs(2,i) = ityp1
         ipdfs(3,i) = Ipar
         ipdfs(4,i) = Iset
         ipdfs(5,i) = Iext
      ELSE IF ( Mode.EQ.-2 ) THEN
         IF ( LPRi.GT.4 ) WRITE (LO,'(/1X,A)')
     &         'PHO_SETPDF: PDFs assigned by user:'
         DO i = 1 , ientry
            IF ( LPRi.GT.4 ) WRITE (LO,'(5X,I4,A,I7,A,4I5)') i , 
     &           '  particle:' , ipdfs(1,i) , '   PDF-set  ' , 
     &           ipdfs(2,i) , ipdfs(3,i) , ipdfs(4,i) , ipdfs(5,i)
         END DO
      ELSE
         IF ( LPRi.GT.4 ) WRITE (LO,'(/1X,A,I5)')
     &         'PHO_SETPDF:ERROR: invalid mode ' , Mode
      END IF
      END SUBROUTINE
