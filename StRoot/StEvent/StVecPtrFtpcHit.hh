/***************************************************************************
 *
 * $Id: StVecPtrFtpcHit.hh,v 1.4 1999/03/04 18:17:36 ullrich Exp $
 *
 * Author: Thomas Ullrich, Jan 1999
 ***************************************************************************
 *
 * Description:
 *
 ***************************************************************************
 *
 * $Log: StVecPtrFtpcHit.hh,v $
 * Revision 1.4  1999/03/04 18:17:36  ullrich
 * Namespace std not used if ST_NO_NAMESPACES defined
 *
 * Revision 1.3  1999/03/04 15:57:08  wenaus
 * add std namespace for Sun CC5 compatibility
 *
 * Revision 1.2  1999/01/15 22:54:17  wenaus
 * version with constructors for table-based loading
 *
 **************************************************************************/
#ifndef StVecPtrFtpcHit_hh
#define StVecPtrFtpcHit_hh

#include <vector>
#if !defined(ST_NO_NAMESPACES)
using namespace std;
#endif
class StFtpcHit;

#ifdef ST_NO_TEMPLATE_DEF_ARGS
typedef vector<StFtpcHit*, allocator<StFtpcHit*> > StVecPtrFtpcHit;
typedef vector<StFtpcHit*, allocator<StFtpcHit*> >::iterator StVecPtrFtpcHitIterator;
#else
typedef vector<StFtpcHit*> StVecPtrFtpcHit;
typedef vector<StFtpcHit*>::iterator StVecPtrFtpcHitIterator;
#endif

#endif
