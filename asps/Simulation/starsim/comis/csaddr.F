*
* $Id: csaddr.F,v 1.1.1.1 2004/01/12 23:49:39 potekhin Exp $
*
* $Log: csaddr.F,v $
* Revision 1.1.1.1  2004/01/12 23:49:39  potekhin
*
*
* Revision 1.2  2001/06/15 23:00:20  nevski
* if-endif corrected
*
* Revision 1.1  2001/02/27 10:15:10  nevski
*  first working release
*
* Revision 1.2  1998/04/16 17:02:17  fisyak
* 2nd pass with gstar
*
*
* ----------------------------------------------------------------
#include "comis/pilot.h"
*CMZ :          10/11/97  12.13.09  by  Pavel Nevski
*CMZ :  1.18/03 09/06/94  17.55.36  by  Rene Brun
*-- Author : V.Berezhnoi
******************************************************************************
#if !defined(CERNLIB_COMISF)
******************************************************************************
#ifdef CERNLIB_QMLXIA64
      integer*8 FUNCTION  CSADDR (CHNAME)
      integer*8 I, IADGP, CS_GET_FUNC, CSITGP
#else
      FUNCTION  CSADDR (CHNAME)
      INTEGER          CSADDR,CSLTGP,CSITGP,IPVS
#endif
*
*    returns the memory address of a comis-routine
******************************************************************************
      CHARACTER*(*)    CHNAME
      CHARACTER*80     NAME
#include "comis/cstab.inc"
#if defined(CERNLIB_SHL)
#ifndef CERNLIB_QMLXIA64
      INTEGER          CS_GET_FUNC
#endif
      COMMON /CSDEBUG/ ICSDEBUG
*
      NAME = CHNAME
      NC   = LENOCC(NAME)
      if (ICSDEBUG.gt.0) print *,' ===> in CSADDR (',NAME(:NC),') <==='
      CALL CSCHID(NAME(:NC))
      CALL CUTOL (NAME(:NC))
      I    = CSLTGP(IPVS)
      IF(I.GT.0)THEN
        IF(IFCS.EQ.0)THEN
          IADGP=CS_GET_FUNC(NAME(1:NC)//'_')
          IF(IADGP.NE.0)THEN
            IFCS=-2
            CALL CSRTGP(I)
          ELSE
            I=0
          ENDIF
        ENDIF
      ELSE
        IADGP=CS_GET_FUNC(NAME(1:NC)//'_')
        IF(IADGP.NE.0)THEN
          IFCS=-2
          ITYPGP=-2
          I=CSITGP(IPVS)
        ENDIF
      END IF
      CSADDR=I
#endif
#if !defined(CERNLIB_SHL)
      NAME=CHNAME
      CALL CSCHID(NAME)
      I=CSLTGP(IPVS)
      IF(I.GT.0)THEN
        IF(IFCS.EQ.0)I=0
      END IF
      CSADDR=I
#endif
      END

#endif
