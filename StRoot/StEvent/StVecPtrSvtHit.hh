/***************************************************************************
 *
 * $Id: StVecPtrSvtHit.hh,v 1.4 1999/03/04 18:17:40 ullrich Exp $
 *
 * Author: Thomas Ullrich, Jan 1999
 ***************************************************************************
 *
 * Description:
 *
 ***************************************************************************
 *
 * $Log: StVecPtrSvtHit.hh,v $
 * Revision 1.4  1999/03/04 18:17:40  ullrich
 * Namespace std not used if ST_NO_NAMESPACES defined
 *
 * Revision 1.3  1999/03/04 15:57:09  wenaus
 * add std namespace for Sun CC5 compatibility
 *
 * Revision 1.2  1999/01/15 22:54:19  wenaus
 * version with constructors for table-based loading
 *
 **************************************************************************/
#ifndef StVecPtrSvtHit_hh
#define StVecPtrSvtHit_hh
#include <vector>
#if !defined(ST_NO_NAMESPACES)
using namespace std;
#endif
class StSvtHit;

#ifdef ST_NO_TEMPLATE_DEF_ARGS
typedef vector<StSvtHit*, allocator<StSvtHit*> > StVecPtrSvtHit;
typedef vector<StSvtHit*, allocator<StSvtHit*> >::iterator StVecPtrSvtHitIterator;
#else
typedef vector<StSvtHit*> StVecPtrSvtHit;
typedef vector<StSvtHit*>::iterator StVecPtrSvtHitIterator;
#endif

#endif
