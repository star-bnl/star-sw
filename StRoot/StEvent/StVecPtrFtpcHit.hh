/***************************************************************************
 *
 * $Id: StVecPtrFtpcHit.hh,v 1.2 1999/01/15 22:54:17 wenaus Exp $
 *
 * Author: Thomas Ullrich, Jan 1999
 ***************************************************************************
 *
 * Description:
 *
 ***************************************************************************
 *
 * $Log: StVecPtrFtpcHit.hh,v $
 * Revision 1.2  1999/01/15 22:54:17  wenaus
 * version with constructors for table-based loading
 *
 * Revision 1.2  1999/01/15 22:54:17  wenaus
 * version with constructors for table-based loading
 *

#ifndef StVecPtrFtpcHit_hh
#define StVecPtrFtpcHit_hh
using namespace std;
#include <vector>
class StFtpcHit;

#ifdef ST_NO_TEMPLATE_DEF_ARGS
typedef vector<StFtpcHit*, allocator<StFtpcHit*> > StVecPtrFtpcHit;
typedef vector<StFtpcHit*, allocator<StFtpcHit*> >::iterator StVecPtrFtpcHitIterator;
#else
typedef vector<StFtpcHit*> StVecPtrFtpcHit;
typedef vector<StFtpcHit*>::iterator StVecPtrFtpcHitIterator;
#endif

#endif
