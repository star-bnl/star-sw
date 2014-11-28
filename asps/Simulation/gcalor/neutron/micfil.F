* $Id: micfil.F,v 1.1.1.1 2004/01/15 00:08:48 potekhin Exp $
* $Name:  $
#include <commons/config.h>
      SUBROUTINE MICFIL(CNAME)
C***********************************************************************
C set a option in MICAP
C
C INPUT:
C          CNAME  - in case NKEY=10 the file name
C
C************************************************************************
C
C #include "geant321/mmicap.inc"
      INTEGER       LMIFIL
C
      CHARACTER*(*) CNAME
C
      LOGICAL FIRST
      DATA FIRST/.TRUE./
C
      WRITE(6,6000) 
      WRITE(6,6010) 
 6000 FORMAT(' Gcalor CVS/Srt conversion exposed a routine MICFIL and ',
     +     /,'        common /MICFIL/ with the same name',
     +     /,' As you can see the quick fix is to disable the code ')

 6010 FORMAT(' Gcalor MICFIL is going to STOP')
      IF (FIRST) STOP
C-disable      IF(FIRST) THEN
C-disable         FIRST = .FALSE.
C-disable         NFIL = 101
C-disable         CALL CHKZEB(NFIL,IXCONS)
C-disable         CALL MZLINK(IXCONS,'MICFIL',LMIFIL,LMIFIL,LMIFIL)
C-disable         CALL MZBOOK(IXCONS,LMIFIL,0,2,'MIFL',0,0,NFIL,0,0)
C-disable      ELSE
C-disableC increase the bank for the x-section file name
C-disable        NFIL = 101 + IQ(LMIFIL-1)
C-disable        CALL CHKZEB(NFIL,IXCONS)
C-disable        CALL MZPUSH(IXCONS,LMIFIL,0,101,'I')
C-disable      ENDIF
C-disableC store x-section file name in bank 'MIFL'
C-disableC find the last free index in the bank
C-disable      IF(LNBLNK(CNAME).GT.0) THEN
C-disable        I = LMIFIL+IQ(LMIFIL-1)-100+1
C-disable        CALL UCTOH(CNAME,IQ(I),4,LNBLNK(CNAME))
C-disable        IQ(I-1) = LNBLNK(CNAME)
C-disable      ELSE
C-disable        PRINT*,' MICSET : invalid file name '
C-disable      ENDIF
C-disable   30 RETURN
      END
