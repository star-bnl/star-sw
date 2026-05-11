
      SUBROUTINE PHO_SETMDL(Indx,Ival,Imode)
C**********************************************************************
C
C     set model switches
C
C     input:  INDX       model parameter number
C                        (positive: ISWMDL, negative: IPAMDL)
C             IVAL       new value
C             IMODE      -1  print value of parameter INDX
C                        1   set new value
C                        -2  print current settings
C
C**********************************************************************
      IMPLICIT NONE
      INTEGER i , Imode , Indx , Ival , k
      SAVE 
 
C  input/output channels
      INCLUDE 'inc/poinou'
C  model switches and parameters
      INCLUDE 'inc/pomdls'
 
      IF ( Imode.EQ.-2 ) THEN
         IF ( LPRi.GT.4 ) WRITE (LO,'(/1X,A,/1X,A,/)')
     &         'PHO_SETMDL: current settings' , 
     &        '----------------------------'
         DO i = 1 , 48 , 3
            IF ( ISWmdl(i).EQ.-9999 ) GOTO 99999
            IF ( ISWmdl(i+1).EQ.-9999 ) THEN
               IF ( LPRi.GT.4 ) WRITE (LO,'(5X,I3,A1,A,I6)') i , ':' , 
     &              MDLna(i) , ISWmdl(i)
               GOTO 99999
            ELSE IF ( ISWmdl(i+2).EQ.-9999 ) THEN
               IF ( LPRi.GT.4 ) WRITE (LO,'(2(5X,I3,A1,A,I6))') i , 
     &              ':' , MDLna(i) , ISWmdl(i) , i + 1 , ':' , 
     &              MDLna(i+1) , ISWmdl(i+1)
               GOTO 99999
            ELSE
               IF ( LPRi.GT.4 ) WRITE (LO,'(3(5X,I3,A1,A,I6))')
     &              (i+k,':',MDLna(i+k),ISWmdl(i+k),k=0,2)
            END IF
         END DO
      ELSE IF ( Imode.EQ.-1 ) THEN
         IF ( LPRi.GT.4 ) WRITE (LO,'(1X,A,1X,A,I6)') 'PHO_SETMDL:' , 
     &        MDLna(Indx) , ISWmdl(Indx)
      ELSE IF ( Imode.EQ.1 ) THEN
         IF ( Indx.GT.0 ) THEN
            IF ( ISWmdl(Indx).NE.Ival ) THEN
               IF ( LPRi.GT.4 ) WRITE (LO,'(1X,A,I4,1X,A,2I6)')
     &               'PHO_SETMDL:ISWMDL(OLD/NEW):' , Indx , MDLna(Indx)
     &              , ISWmdl(Indx) , Ival
               ISWmdl(Indx) = Ival
            END IF
         ELSE IF ( Indx.LT.0 ) THEN
            IF ( IPAmdl(-Indx).NE.Ival ) THEN
               IF ( LPRi.GT.4 ) WRITE (LO,'(1X,A,I4,1X,2I6)')
     &               'PHO_SETMDL:IPAMDL(OLD/NEW):' , -Indx , 
     &              IPAmdl(-Indx) , Ival
               IPAmdl(-Indx) = Ival
            END IF
         END IF
      ELSE
         IF ( LPRi.GT.4 ) WRITE (LO,'(/1X,A,I6)')
     &         'PHO_SETMDL:ERROR: unsupported mode' , Imode
      END IF
99999 END SUBROUTINE
