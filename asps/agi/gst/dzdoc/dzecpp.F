$Id: dzecpp.F,v 1.1 2000/01/06 01:01:10 nevski Exp $
$Log: dzecpp.F,v $
Revision 1.1  2000/01/06 01:01:10  nevski
dzdoc for agi

      SUBROUTINE DZECPP(LUNIN,LUNOUT)
      CHARACTER*80 CLINE
      COMMON/DZDPPC/CLINE
   10 CONTINUE
      CALL DZDCRD(LUNIN,CLINE,IRC)
      IF(IRC.NE.0)GOTO 30
*      READ(LUNIN,'(A)',END=30 )CLINE
      IF(CLINE(1:1).EQ.'%')THEN
         IF(CLINE(1:9).EQ.'%%PICTURE')THEN
            GOTO 30
         ELSE
            GOTO 10
         ENDIF
      ELSE
         ILC = INDXBC(CLINE,' ')
         WRITE(LUNOUT,'(A)')CLINE(1:ILC)
         GOTO 10
      ENDIF
   30 CONTINUE
      END

**********************************************************************
