* $Id: palogo.F,v 1.1.1.1 2004/01/12 23:49:38 potekhin Exp $
* $Log: palogo.F,v $
* Revision 1.1.1.1  2004/01/12 23:49:38  potekhin
*
* Revision 1.3  2003/09/11 11:50:28  nevski
* correct logo length
*
* Revision 1.2  2003/08/24 16:32:50  nevski
* save atlsim version in user run header
*
* Revision 1.1  2001/02/27 10:14:54  nevski
*  first working release
*
      SUBROUTINE PALOGO
#include "atlsim/agcvers.inc"
*
*     Print STAF version identification
*
      CHARACTER  CHPAW*8, MONTH(12)*12
      DATA MONTH/'January','February','March','April','May','June',
     > 'July','August','September','October','November','December'/
      CHARACTER*240     PROG
      COMMON/AgPROGRAM/ PROG
      CHARACTER*120     LOGO
      COMMON/AgLOGO/    LOGO

      CALL AVERSION
      CHPAW  = PROG
      IDAY   = mod(IDATQQ,100)
      IMONTH = mod(IDATQQ/100,100)
      IYEAR  = IDATQQ/10000
      NCM    = LENOCC(MONTH(IMONTH))

      IF      (IYEAR.LT.90)  then
           IYEAR=2000+IYEAR
      ELSE IF (IYEAR.LT.100) then
           IYEAR=1900+IYEAR
      ENDIF
      PRINT 3000,CHPAW,VERSQQ,IDAY,MONTH(IMONTH)(1:NCM),
     >           IYEAR,MONTH(IMONTH)(NCM+1:)
 3000 FORMAT(' ******************************************************'/,
     >       ' *                                                    *'/,
     >       ' *            W E L C O M E    to   ', A7,'           *'/,
     >       ' *                                                    *'/,
     >       ' *        Version ',A8,4X,I2,1X,A,1X,I4,A,4X,        '*'/,
     >       ' *                                                    *'/,
     >       ' ******************************************************')
*
      Call DATIME(Idate,Itime)
      ID = mod(IDATE,100)
      IM = mod(IDATE/100,100)
      IY = IDATE/10000+2000
      NC = LENOCC(MONTH(IM))
*
      write (LOGO,3001) CHPAW,VERSQQ,IDAY,MONTH(IMONTH)(1:NCM),IYEAR,
     >                  Id,Month(Im)(1:Nc),IY,Itime/100,mod(Itime,100)
 3001 FORMAT('* ',A7,'  Version ',A8,I3,1X,A,I5,
     >               ', Started on ',I2,1x,A,I5,' at ',I2,':',I2.2,' *')
      END
