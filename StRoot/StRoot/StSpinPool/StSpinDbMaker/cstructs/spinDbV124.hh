#ifndef TAB_SPIN_DB_V124__hh
#define TAB_SPIN_DB_V124__hh
#include "spinConstDB.hh"

/*
 * RHIC fill related variables from V124
 * see StRoot/StSpinDbMaker/cstruct/spinConstDb.hh for constants
 */
struct spinDbV124 {

  int bucketOffset[SPINDbMaxRing]; /* time bucket offset for bXing=0 at STAR, range=(0,359)  */
  
  int rotatorState[SPINDbMaxRing]; /* enum: trans, long, ... */

  unsigned char  v124bits[SPINDbMaxBuckets];   /* (unpol,down,up,fill) x (blue, yellow) x (360)timeBuckets , packed*/

  char  comment[SPINDbMaxComment];
};

#endif

