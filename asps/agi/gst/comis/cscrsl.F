*
* $Id: cscrsl.F,v 1.2 1998/04/16 17:02:18 fisyak Exp $
*
* $Log: cscrsl.F,v $
* Revision 1.2  1998/04/16 17:02:18  fisyak
* 2nd pass with gstar
*
*
* -----------------------------------------------------------------
#include "comis/pilot.h"
*CMZ :          29/10/97  09.43.10  by  Pavel Nevski
*CMZ :  1.18/02 07/04/94  15.26.09  by  Vladimir Berezhnoi
*-- Author :    Vladimir Berezhnoi   07/01/94
*************************************************************************
      SUBROUTINE       CSCRSL (NAME,CPID,FEXT,IERR)
*                                                                       *
* Description: interface to CS_SHL_LOAD                                 *
* Modifications:                                                        *
* pn, 29.10.97 modified according to the mail from VB (20.09.97):       *
*     NAME in concatineted parameters replaced by CHLINE                *
*                                                                       *
*************************************************************************
      CHARACTER *(*)   NAME,CPID,FEXT
      COMMON /CSTBCS/  ITBC,ITBS
#if defined(CERNLIB_SHL)
#include "comis/cshlnm.inc"
      CHARACTER*72     CHLINE,FEXEC
      INTEGER          SYSTEMF,CS_SHL_LOAD
      COMMON /CSDEBUG/ ICSDEBUG
*--
      if (ICSDEBUG.gt.0) print *,' ===> in CSCRSL (',NAME,') <==='
      LN=LENOCC(NAME)
      LP=LENOCC(CPID)
*c    IF (ITBS.NE.0) CALL CSRMSL(NAME)
 
      IF (FEXT.ne.'.sl' .and. FEXT.ne.'.csl') THEN  ! do the library
        FEXEC=CHPATH(:LENOCC(CHPATH))//'exec_'//CPID//'.exec'
        CALL CSCREXEC(NAME,CPID,FEXT,FEXEC,IERR)
 
        CHLINE='/bin/sh '//FEXEC
        IERR=SYSTEMF(CHLINE(:LENOCC(CHLINE)))
        IF (IERR .NE. 0) RETURN
 
        CHLINE='/bin/rm -f '//FEXEC
        IERR=SYSTEMF(CHLINE(:LENOCC(CHLINE)))
        IF (IERR .NE. 0) RETURN
      endif
 
*c now really load
      CHLINE=NAME(1:LN)
      IERR=CS_SHL_LOAD(CHLINE(1:LN)//'.sl')
      IF (IERR.ne.0) THEN
        CALL CS_SHL_UNLOAD(CHLINE(1:LN)//'.sl')
        return
      ENDIF
      ITBS=ITBS+1
#endif
      END
 
