*
* $Id: storparq.inc,v 1.1.1.1 2004/07/17 20:01:58 perev Exp $
*
* $Log: storparq.inc,v $
* Revision 1.1.1.1  2004/07/17 20:01:58  perev
* STAR version of Geant321 TGeant3 etc
*
* Revision 1.1.1.1  2002/07/24 15:56:28  rdm
* initial import into CVS
*
* Revision 1.1.1.1  2002/06/16 15:18:50  hristov
* Separate distribution  of Geant3
*
* Revision 1.1.1.1  1999/05/18 15:55:28  fca
* AliRoot sources
*
* Revision 1.1.1.1  1996/03/06 10:46:57  mclareni
* Zebra
*
*
#ifndef CERNLIB_ZEBRA_STORPARQ_INC
#define CERNLIB_ZEBRA_STORPARQ_INC
*
* STORE PARAMETERS
*
* storparq.inc
*
      PARAMETER(JSTCOQ=31,JSTIDQ=27,NSTIDQ=4,NSTTBQ=14)
      PARAMETER(MSYLAQ=1,MLAUSQ=1)
      PARAMETER(NLAENQ=5,MLAADQ=1,MLALTQ=MLAADQ+1)
      PARAMETER(MLANSQ=MLALTQ+1,MLAN1Q=MLANSQ+1,MLAN2Q=MLAN1Q+1)
      PARAMETER(JLANSQ=1,NLANSQ=15)
      PARAMETER(JLATMQ=31,NLATMQ=2,MLACTQ=1,MLATBQ=MLACTQ+1)

#endif
