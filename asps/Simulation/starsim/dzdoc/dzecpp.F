* $Id: dzecpp.F,v 1.1.1.1 2004/01/12 23:49:39 potekhin Exp $
* $Log: dzecpp.F,v $
* Revision 1.1.1.1  2004/01/12 23:49:39  potekhin
*
* Revision 1.1  2001/02/27 10:15:24  nevski
*  first working release
*
* Revision 1.2  2000/01/06 23:57:24  nevski
* cvs header record corrected
*
* Revision 1.1  2000/01/06 01:01:10  nevski
* dzdoc for agi
*
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
