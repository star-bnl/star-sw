/***************************************************************************
 *
 * $Id: StVecPtrTpcHit.hh,v 1.3 1999/03/04 15:57:10 wenaus Exp $
 *
 * Author: Thomas Ullrich, Jan 1999
 ***************************************************************************
 *
 * Description:
 *
 ***************************************************************************
 *
 * $Log: StVecPtrTpcHit.hh,v $
 * Revision 1.3  1999/03/04 15:57:10  wenaus
 * add std namespace for Sun CC5 compatibility
 *
 * Revision 1.3  1999/03/04 15:57:10  wenaus
 * add std namespace for Sun CC5 compatibility
 *
 * Revision 1.2  1999/01/15 22:54:20  wenaus
 * version with constructors for table-based loading
 *
 **************************************************************************/
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
