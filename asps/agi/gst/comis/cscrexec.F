*
* $Id: cscrexec.F,v 1.2 1998/04/16 17:02:17 fisyak Exp $
*
* $Log: cscrexec.F,v $
* Revision 1.2  1998/04/16 17:02:17  fisyak
* 2nd pass with gstar
*
*
* -------------------------------------------------------
#include "comis/pilot.h"
#if defined(CERNLIB_SHL)
*CMZ :          10/11/97  11.27.40  by  Pavel Nevski
*CMZ :  1.17/07 10/01/94  16.41.15  by  Vladimir Berezhnoi
*-- Author :    Vladimir Berezhnoi   07/01/94
*************************************************************************
      SUBROUTINE CSCREXEC(NAME,CPID,FEXT,FEXEC,IERR)
*                                                                       *
* Description: comis exec processor                                     *
* Modifications:                                                        *
* PN, 29.10.97 - LINUX version added according to VB mail of 20.09.97   *
*              - system branches cleaned up, USERLIB is a common option *
*************************************************************************
      CHARACTER *(*)      NAME,CPID,FEXT,FEXEC
#include "comis/cshlnm.inc"
      CHARACTER *256  CHLINE
      CHARACTER *4096 USERLIBS
      COMMON /CSDEBUG/ ICSDEBUG
 
      IERR  = 0
      LN    = LENOCC(NAME)
      LP    = LENOCC(CPID)
      LPATH = LENOCC(CHPATH)
      LEXEC = LENOCC(FEXEC)
      if (ICSDEBUG.gt.0) print *,' ===> in CSCREXEC <=== ',NAME(:LN)
 
**    CALL PALUNF(60,3,LUNOUT)
      CALL CSLUNF(LUNOUT)
      IF (LUNOUT.EQ.0) GO TO 99
      OPEN(LUNOUT,FILE=FEXEC(:LEXEC), STATUS='UNKNOWN',ERR=99)
      WRITE (LUNOUT,'(A)')        '#! /bin/sh'
      WRITE (LUNOUT,'(A)')        'olddir=`pwd`'
*     WRITE (LUNOUT,'(A)')        'cd '//CHPATH(:LPATH)
      CHLINE=                     'cd '//CHPATH(:LPATH)
      L=LENOCC(CHLINE)
      WRITE (LUNOUT,'(A)') CHLINE(:L)
*     WRITE (LUNOUT,'(A)')        '/bin/rm -f '//NAME(:LN)//'.sl'
      CHLINE=                     '/bin/rm -f '//NAME(:LN)//'.sl'
      L=LENOCC(CHLINE)
      WRITE (LUNOUT,'(A)') CHLINE(:L)
      IF(FEXT.EQ.'.c')THEN
*                                 'cc -c .... name.c'
        L=LENOCC(CHCC)
        CHLINE=CHCC(:L)//' '//NAME(:LN)// '.c'
      ELSE
*                                 'f77 -c .... name.f'
        L=LENOCC(CHF77)
        CHLINE=CHF77(:L)//' '//NAME(:LN)// '.f'
      ENDIF
*
      L=LENOCC(CHLINE)
      WRITE(LUNOUT,'(A)') CHLINE(:L)
      WRITE(LUNOUT,'(A)')         'errno=$?'
      WRITE(LUNOUT,'(A)')         'if [ $errno != 0 ]'
      WRITE(LUNOUT,'(A)')         'then'
      WRITE(LUNOUT,'(A)')         '   exit $errno'
      WRITE(LUNOUT,'(A)')         'fi'
*
#if defined(CERNLIB_HPUX)
      CHLINE=                     'ld -b -o '
     +                    // NAME(:LN) //'.sl '// NAME(:LN) //'.o'
#endif
#if defined(CERNLIB_AIX)
      CHLINE=                     '/usr/ucb/nm '// NAME(:LN) //
     + '.o | egrep '' [BAD] ''| cut -f3 -d'' '' |sed -e ''s/^#/ #/'' '
     + // '| sort | uniq > '// NAME(:LN) //'.exp'
      L=LENOCC(CHLINE)
      WRITE (LUNOUT,'(A)') CHLINE(:L)
      CHLINE=                     'ld -bE:'// NAME(:LN) //'.exp -o '
     + // NAME(:LN) //'.sl /pathtoimp/aixpawimp.o '// NAME(:LN) //'.o'
     + // ' -bh:4 -T512 -H512'
      L=LENOCC(CHLINE)
      IF(FEXT.EQ.'.c')THEN
        CHLINE= CHLINE(:L)// ' -lc'
      ELSE
        CHLINE= CHLINE(:L)// ' -lxlf90'
      ENDIF
#endif
#if (defined(CERNLIB_SUN)||defined(CERNLIB_MSDOS))&&(defined(CERNLIB_SOLARIS))
      CHLINE=                     '/usr/ccs/bin/ld -G -o '
     +                    // NAME(:LN) //'.sl '// NAME(:LN) //'.o'
#endif
#if (defined(CERNLIB_SUN))&&(!defined(CERNLIB_SOLARIS))
      CHLINE=                     'ld -o '
     +                    // NAME(:LN) //'.sl '// NAME(:LN) //'.o'
#endif
#if defined(CERNLIB_SGI)
      CHLINE=                     'ld -shared -o '
     +                    // NAME(:LN) //'.sl '// NAME(:LN) //'.o'
#endif
#if defined(CERNLIB_ALPHA_OSF)
      CHLINE=                     'ld -shared -o '
     +                    // NAME(:LN) //'.sl '// NAME(:LN) //'.o'
     +                    // ' -lUfor -lfor -lFutil -lm -lots -lc'
#endif
#if defined(CERNLIB_LINUX)
      CHLINE=                     'ld -shared -o '
     +                    // NAME(:LN) //'.sl '// NAME(:LN) //'.o'
#endif
*
      USERLIBS=                   '         '
      CALL GETENV('USERLIB',USERLIBS)
      L       = LENOCC(CHLINE)
      LUSRLIB = max(1,LENOCC(USERLIBS))
      WRITE (LUNOUT,'(A)') CHLINE(:L) // USERLIBS(:LUSRLIB)
*
      WRITE(LUNOUT,'(A)')         'errno=$?'
      WRITE(LUNOUT,'(A)')         'if [ $errno != 0 ]'
      WRITE(LUNOUT,'(A)')         'then'
      WRITE(LUNOUT,'(A)')         '   exit $errno'
      WRITE(LUNOUT,'(A)')         'fi'
      CHLINE=                     '/bin/chmod 555 '//NAME(:LN)//'.sl'
      L=LENOCC(CHLINE)
      WRITE(LUNOUT,'(A)')CHLINE(:L)
*?     CHLINE='/bin/rm -f *_'//CPID(:LP)//'.o *_'//CPID(:LP)//'.f'
*      CHLINE=                    '/bin/rm -f *_'//CPID(:LP)//'.o'
      CHLINE=                     '/bin/rm -f '//NAME(:LN)//'.o'
      L=LENOCC(CHLINE)
      WRITE(LUNOUT,'(A)')CHLINE(:L)
      WRITE(LUNOUT,'(A)')         'cd $olddir'
      WRITE(LUNOUT,'(A)')         'exit 0'
      CALL CSCLOS(LUNOUT)
      CLOSE(LUNOUT)
      RETURN
*
 99   PRINT *,' CS: could not open file: ',FEXEC(:LENOCC(FEXEC))
      IERR=1
      IF(LUNOUT.GT.0)CALL CSCLOS(LUNOUT)
      END
#endif
