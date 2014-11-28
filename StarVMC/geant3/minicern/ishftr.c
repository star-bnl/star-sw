/*
 * $Id: ishftr.c,v 1.1 2009/02/25 00:08:26 fisyak Exp $
 *
 * $Log: ishftr.c,v $
 * Revision 1.1  2009/02/25 00:08:26  fisyak
 * Clean up mess created after merging
 *
 * Revision 1.1.1.1  2004/07/17 20:01:57  perev
 * STAR version of Geant321 TGeant3 etc
 *
 * Revision 1.1.1.1  2002/07/24 15:56:28  rdm
 * initial import into CVS
 *
 * Revision 1.1.1.1  2002/06/16 15:18:47  hristov
 * Separate distribution  of Geant3
 *
 * Revision 1.1.1.1  1999/05/18 15:55:33  fca
 * AliRoot sources
 *
 * Revision 1.1.1.1  1996/02/15 17:50:07  mclareni
 * Kernlib
 *
 */
#include "kerngen/pilot.h"
#if defined(__linux__)||defined(Linux)||defined(Darwin)
/*>    ROUTINE ISHFT
  CERN PROGLIB#         ISHFTR          .VERSION KERNLNX  1.02  940511

  Logical right shift by *len (+ve) places
*/
unsigned int ishftr_(arg,len)
unsigned int *arg;
int *len;
{
   return(*arg >> *len);
}

/*> END <----------------------------------------------------------*/
#endif
