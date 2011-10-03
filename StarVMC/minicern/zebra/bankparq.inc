*
* $Id: bankparq.inc,v 1.1.1.1 2004/07/17 20:01:57 perev Exp $
*
* $Log: bankparq.inc,v $
* Revision 1.1.1.1  2004/07/17 20:01:57  perev
* STAR version of Geant321 TGeant3 etc
*
* Revision 1.1.1.1  2002/07/24 15:56:28  rdm
* initial import into CVS
*
* Revision 1.1.1.1  2002/06/16 15:18:49  hristov
* Separate distribution  of Geant3
*
* Revision 1.1.1.1  1999/05/18 15:55:27  fca
* AliRoot sources
*
* Revision 1.1.1.1  1996/03/06 10:46:56  mclareni
* Zebra
*
*
#ifndef CERNLIB_ZEBRA_BANKPARQ_INC
#define CERNLIB_ZEBRA_BANKPARQ_INC
*
*  BANK  PARAMETERS
*
* bankparq.inc
*
      PARAMETER(NBKHDQ=1,NBKCTQ=9,NBKOHQ=NBKHDQ+NBKCTQ,NBKJMQ=12)
      PARAMETER(MBKLNQ=0,JBKSKQ=1,NBKSKQ=16)
      PARAMETER(MBKIOQ=MBKLNQ,JBKIOQ=NBKSKQ+1,NBKIOQ=16)
      PARAMETER(JBKUSQ=1,NBKUSQ=18,JBKSYQ=NBKUSQ+1,NBKSYQ=32-NBKUSQ)

#endif
