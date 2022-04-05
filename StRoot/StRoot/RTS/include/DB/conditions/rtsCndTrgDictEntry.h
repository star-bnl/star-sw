#ifndef _RTSCNDTRGDICTENTRY_H_
#define _RTSCNDTRGDICTENTRY_H_

#include "rtsDbConstants.h"

struct rtsCndTrgDictEntry
{
  ////////////
  // 10/04 index by hash rather than run
  //int idx_rn;    
  unsigned int hash;          
  ////////////

  /* char field[DB_MAX_STR_LEN];         removed as of version 10.0 */
  /* int objectIdx;                      removed as of version 10.0 */
  /* int registerIdx;                    removed as of version 10.0 */
  int object;              /* added as of version 10.0 */
  int idx;               /* added as of version 10.0 */
  int reg;                 /* added as of version 10.0 */
  char label[DB_MAX_STR_LEN];
  int value;                    /* -1 for non-register values... */

  // 3/3/09
  int defaultvalue;
};

#endif
