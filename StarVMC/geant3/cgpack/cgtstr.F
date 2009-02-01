*
* $Id: cgtstr.F,v 1.1.1.2 2009/02/01 17:10:07 fisyak Exp $
*
* $Log: cgtstr.F,v $
* Revision 1.1.1.2  2009/02/01 17:10:07  fisyak
* Merge with .DEV2
*
* Revision 1.1.1.1  2008/12/10 20:46:03  fisyak
* Merge with macos version
*
* Revision 1.1.1.1  2005/05/25 22:36:40  fisyak
* Alice version of geant3 (-minicern)
*
* Revision 1.1.1.1  2002/07/24 15:56:27  rdm
* initial import into CVS
*
* Revision 1.1.1.1  2002/06/16 15:17:54  hristov
* Separate distribution  of Geant3
*
* Revision 1.1.1.1  1999/05/18 15:55:04  fca
* AliRoot sources
*
* Revision 1.1.1.1  1995/10/24 10:19:45  cernlib
* Geant
*
*
#include "geant321/pilot.h"
*CMZ :  3.21/02 29/03/94  15.41.32  by  S.Giani
*-- Author :
      SUBROUTINE CGTSTR(CG,IREP)
************************************************************************
*                                                                      *
*     Name: CGTSTR                                                     *
*     Author: E. Chernyaev                       Date:    12.02.89     *
*                                                Revised:              *
*                                                                      *
*     Function: Test structure of CG-object                            *
*                                                                      *
*     References: none                                                 *
*                                                                      *
*     Input:  CG(*) - CG-object                                        *
*                                                                      *
*     Output: IREP - replay (0 - O.K, -1. - error in structure)        *
*                                                                      *
*     Errors: none                                                     *
*                                                                      *
************************************************************************
#include "geant321/cggpar.inc"
      REAL      CG(*)
*-
      IREP   =-1
      LTOTAL = CG(KCGSIZ)
      IF (LTOTAL .LT. LCGHEA)                  GOTO 999
      NFACE  = CG(KCGNF)
      IF (NFACE .LT. 0)                        GOTO 999
      IF (NFACE .GT. 99999)                    GOTO 999
*
      JCG    = LCGHEA
      IF (NFACE .EQ. 0)                         GOTO 200
      DO 100 NF=1,NFACE
        IF (LTOTAL .LT. JCG+LCGFAC)            GOTO 999
        NEDGE  = CG(JCG+KCGNE)
        IF (NEDGE .LE. 0)                      GOTO 999
        IF (NEDGE .GT. 99999)                  GOTO 999
        JCG    = JCG + LCGFAC + NEDGE*LCGEDG
        IF (LTOTAL .LT. JCG)                   GOTO 999
  100   CONTINUE
  200 IF (JCG .EQ. LTOTAL)                      IREP = 0
*
  999 RETURN
      END
