/***************************************************************************
 *
 * $Id: StVecPtrTpcHit.hh,v 1.1 1999/01/15 20:40:24 wenaus Exp $
 *
 * Author: Thomas Ullrich, Jan 1999
 ***************************************************************************
 *
 * Description:
 *
 ***************************************************************************
 *
 * $Log: StVecPtrTpcHit.hh,v $
 * Revision 1.1  1999/01/15 20:40:24  wenaus
 * Commit Thomas' original code
 *
 * Revision 1.2  1999/01/15 22:54:20  wenaus
 * version with constructors for table-based loading
 *

#ifndef StVecPtrTpcHit_hh
#define StVecPtrTpcHit_hh
using namespace std;
#include <vector>
class StTpcHit;

#ifdef ST_NO_TEMPLATE_DEF_ARGS
typedef vector<StTpcHit*, allocator<StTpcHit*> > StVecPtrTpcHit;
typedef vector<StTpcHit*, allocator<StTpcHit*> >::iterator StVecPtrTpcHitIterator;
#else
typedef vector<StTpcHit*> StVecPtrTpcHit;
typedef vector<StTpcHit*>::iterator StVecPtrTpcHitIterator;
#endif

#endif
