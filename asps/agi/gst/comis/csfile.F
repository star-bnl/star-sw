*
* $Id: csfile.F,v 1.3 1998/06/05 20:55:12 perev Exp $
*
* $Log: csfile.F,v $
* Revision 1.3  1998/06/05 20:55:12  perev
* AGI commit
*
* Revision 1.2  1998/04/16 17:02:18  fisyak
* 2nd pass with gstar
*
*
* -----------------------------------------------------------------
#include "comis/pilot.h"
*CMZ :          10/11/97  12.13.09  by  Pavel Nevski
*CMZ :  1.18/14 20/09/94  14.48.52  by  Fons Rademakers
*-- Author : V.Berezhnoi
************************************************************************
      SUBROUTINE CSFILE(FILENAME)
*                                                                      *
* Description: comis loader top level interface                        *
* Modifications:                                                       *
* pn, 29.10.97 according to the mail from VB (20.09.97):               *
*     FILENAME in concatineted parameters replaced by LIBNAME          *
************************************************************************
      CHARACTER*(*) FILENAME
      COMMON /CSERRNO/ IERR
      COMMON /CSDEBUG/ ICSDEBUG
#if defined(CERNLIB_SHL)
#include "comis/cshlnm.inc"
*     CHPATH='/tmp/' comes from CSHLNM
      CHARACTER  LIBNAME*72,LINE*80,PATH*72,NAME*72,CPID*8,FEXT*8
      INTEGER    SYSTEMF
      LOGICAL    EXIST1
*
      ICASE   = 0
      IERR    = 0
      L       = LENOCC(FILENAME)
      LCHPATH = LENOCC(CHPATH)
      if (ICSDEBUG.gt.0) print *,' ===> in CSFILE (',
     *                                  filename(:L),') <==='
      CALL CSPARSFN(FILENAME(:L),PATH,NAME,FEXT)
      LPATH   = LENOCC(PATH)
      LNAME   = LENOCC(NAME)
 
      LX=LENOCC(FEXT)
      IF (FEXT.EQ.'.csl')THEN
        ICASE = 4
      ELSE IF (FEXT.EQ.'.sl')THEN
        ICASE = 3
      ELSE IF (FEXT.EQ.'.c')THEN
        ICASE = 2
      ELSE IF (FEXT(LX-1:LX).EQ.'77')THEN
        ICASE = 1
        INQUIRE(FILE=FILENAME(:L),EXIST=EXIST1)
        IF (.NOT.EXIST1) FEXT=FEXT(1:LX-2)
      ENDIF
      LEXT    = LENOCC(FEXT)
*
      IF(ICASE.GT.0)THEN
        CALL GETPIDF(IPID)
        CPID    = ' '
        WRITE (CPID,'(I8)') IPID
        LP      = LOG10(REAL(IPID))+1
        CPID    = CPID(9-LP:)
        LIBNAME = CHPATH(:LCHPATH)// NAME(:LNAME) // '_' // CPID(:LP)
        LL      = LENOCC(LIBNAME)
        If (IcsDebug.GT.0) print *,' libname =',LL,libname(:ll)
        If (ICASE.EQ.1) THEN
*---- file...77
          IF (LPATH.GT.0) THEN
            LINE=PATH(:LPATH)//NAME(:LNAME)//FEXT(:LEXT)
          ELSE
            LINE=NAME(:LNAME)//FEXT(:LEXT)
          ENDIF
          CALL CSRMSL(LIBNAME(:LL))
          CALL CSF77 (LINE,LIBNAME(:LL)//'.f',IERR)
          IF (IERR.NE.0) THEN
            PRINT *,' error during conversion to f77::File: ',
     +          FILENAME(:L)
            RETURN
          ENDIF
        ELSE IF (ICASE.EQ.2) THEN
*---- file.c
          LINE = 'cp '//FILENAME(:L)//' '//LIBNAME(:LL)//'.c'
          LE   = LENOCC(LINE)
          IERR = SYSTEMF(LINE(:LE))
          IF (IERR.NE.0) RETURN
          CALL CSRMSL(LIBNAME(:LL))
        ELSE IF (ICASE.EQ.3) THEN
*---- file.sl
          IL = L-3
          LIBNAME = FILENAME(:IL)
          LL = IL
          CALL CSRMSL(LIBNAME(:LL))
*-- ' ' means do not produced output file
*         CALL CSF77 (FILENAME(:IL)//'.f',' ',IERR)
          CALL CSF77 (LIBNAME(:IL) //'.f',' ',IERR)
          IF (IERR.NE.0) THEN
            PRINT *,' error during translation csf77-file: '
     +            ,FILENAME(:L)
            RETURN
          ENDIF
        ELSE IF(ICASE.EQ.4)THEN
*---- file.csl
          IL = L-4
          LIBNAME= FILENAME(:IL)
          LL = IL
          CALL CSRMSL(LIBNAME(:LL))
        ENDIF
        IF (INDEX(LIBNAME(:LL),'/') .EQ. 0) THEN
           LINE = LIBNAME
           LIBNAME = './'//LINE
           LL = LL + 2
        ENDIF
        CALL CSCRSL(LIBNAME(:LL),CPID(:LP),FEXT(:LEXT),IERR)
        IF (IERR.EQ.0) RETURN
      ENDIF
#endif
      IERR=0
      CALL CSFILX(FILENAME)
      END
 
 
