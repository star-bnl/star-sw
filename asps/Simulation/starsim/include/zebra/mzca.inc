*
* $Id: mzca.inc,v 1.1.1.1 2004/01/12 23:49:41 potekhin Exp $
*
* $Log: mzca.inc,v $
* Revision 1.1.1.1  2004/01/12 23:49:41  potekhin
*
* Revision 1.2  2001/03/25 17:50:33  nevski
* mzca splitted
*
* Revision 1.1  2001/02/27 10:15:59  nevski
*  first working release
*
* Revision 1.1.1.1  1996/03/06 10:46:53  mclareni
* Zebra
*
*
#ifndef CERNLIB_ZEBRA_MZCA_INC
#define CERNLIB_ZEBRA_MZCA_INC
*
*     Process Master parameters
*
* mzca.inc
#if defined(CERNLIB_QTRHOLL)
      COMMON /MZCA/  NQSTOR,NQOFFT(16),NQOFFS(16),NQALLO(16), NQIAM
     +,              LQATAB,LQASTO,LQBTIS, LQWKTB,NQWKTB,LQWKFZ
     +,              MQKEYS(3),NQINIT,NQTSYS,NQM99,NQPERM,NQFATA,NQCASE
     +,              NQTRAC,MQTRAC(48)
                     INTEGER MQTRAC
                                       EQUIVALENCE (KQSP,NQOFFS(1))
#else
      COMMON /MZCA/  NQSTOR,NQOFFT(16),NQOFFS(16),NQALLO(16), NQIAM
     +,              LQATAB,LQASTO,LQBTIS, LQWKTB,NQWKTB,LQWKFZ
     +,              MQKEYS(3),NQINIT,NQTSYS,NQM99,NQPERM
     +,              NQFATA,NQCASE,NQTRAC
                                       EQUIVALENCE (KQSP,NQOFFS(1))
      COMMON /MZCA2/ MQTRAC(44)
                     CHARACTER  MQTRAC*8
#endif

#endif
