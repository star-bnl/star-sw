*
* $Id: qtrace.inc,v 1.1.1.1 2004/07/17 20:01:58 perev Exp $
*
* $Log: qtrace.inc,v $
* Revision 1.1.1.1  2004/07/17 20:01:58  perev
* STAR version of Geant321 TGeant3 etc
*
* Revision 1.1.1.1  2002/07/24 15:56:28  rdm
* initial import into CVS
*
* Revision 1.1.1.1  2002/06/16 15:18:50  hristov
* Separate distribution  of Geant3
*
* Revision 1.1.1.1  1999/05/18 15:55:27  fca
* AliRoot sources
*
* Revision 1.1.1.1  1996/03/06 10:46:54  mclareni
* Zebra
*
*
#ifndef CERNLIB_ZEBRA_QTRACE_INC
#define CERNLIB_ZEBRA_QTRACE_INC
*
*        with check on capacity MQTRAC
*
* qtrace.inc
*
#if !defined(CERNLIB_QTRHOLL)||defined(CERNLIB_A6M)
      MQTRAC(NQTRAC+1) = NAMESR
      NQTRAC = NQTRAC + 1
      IF (NQTRAC.GE.41)      CALL ZFATAL
#else
      MQTRAC(NQTRAC+1) = NAMESR(1)
      MQTRAC(NQTRAC+2) = NAMESR(2)
      NQTRAC = NQTRAC + 2
      IF (NQTRAC.GE.41)      CALL ZFATAL
#endif

#endif
