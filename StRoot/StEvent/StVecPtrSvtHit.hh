/***************************************************************************
 *
 * $Id: StVecPtrSvtHit.hh,v 1.2 1999/01/15 22:54:19 wenaus Exp $
 *
 * Author: Thomas Ullrich, Jan 1999
 ***************************************************************************
 *
 * Description:
 *
 ***************************************************************************
 *
 * $Log: StVecPtrSvtHit.hh,v $
 * Revision 1.2  1999/01/15 22:54:19  wenaus
 * version with constructors for table-based loading
 *
 * Revision 1.2  1999/01/15 22:54:19  wenaus
 * version with constructors for table-based loading
 *

#ifndef StVecPtrSvtHit_hh
#define StVecPtrSvtHit_hh
using namespace std;
#include <vector>
class StSvtHit;

#ifdef ST_NO_TEMPLATE_DEF_ARGS
typedef vector<StSvtHit*, allocator<StSvtHit*> > StVecPtrSvtHit;
typedef vector<StSvtHit*, allocator<StSvtHit*> >::iterator StVecPtrSvtHitIterator;
#else
typedef vector<StSvtHit*> StVecPtrSvtHit;
typedef vector<StSvtHit*>::iterator StVecPtrSvtHitIterator;
#endif

#endif
