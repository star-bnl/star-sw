/***************************************************************************
 *
 * $Id: StTpcHitCollection.hh,v 1.4 1999/03/04 18:17:27 ullrich Exp $
 *
 * Author: Thomas Ullrich, Jan 1999
 ***************************************************************************
 *
 * Description:
 *
 ***************************************************************************
 *
 * $Log: StTpcHitCollection.hh,v $
 * Revision 1.4  1999/03/04 18:17:27  ullrich
 * Namespace std not used if ST_NO_NAMESPACES defined
 *
 * Revision 1.3  1999/03/04 15:57:05  wenaus
 * add std namespace for Sun CC5 compatibility
 *
 * Revision 1.2  1999/01/15 22:54:00  wenaus
 * version with constructors for table-based loading
 *
 **************************************************************************/
#ifndef StTpcHitCollection_hh
#define StTpcHitCollection_hh

#include "StEvent/StTpcHit.hh"
#include <vector>
#if !defined(ST_NO_NAMESPACES)
using namespace std;
#endif

#ifdef ST_NO_TEMPLATE_DEF_ARGS
typedef vector<StTpcHit*, allocator<StTpcHit*> >            StTpcHitCollection;
typedef vector<StTpcHit*, allocator<StTpcHit*> >::iterator  StTpcHitIterator;
#else
typedef vector<StTpcHit*>            StTpcHitCollection;
typedef vector<StTpcHit*>::iterator  StTpcHitIterator;
#endif

#endif
