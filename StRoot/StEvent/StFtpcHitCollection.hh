/***************************************************************************
 *
 * $Id: StFtpcHitCollection.hh,v 1.3 1999/03/04 15:56:58 wenaus Exp $
 *
 * Author: Thomas Ullrich, Jan 1999
 ***************************************************************************
 *
 * Description:
 *
 ***************************************************************************
 *
 * $Log: StFtpcHitCollection.hh,v $
 * Revision 1.3  1999/03/04 15:56:58  wenaus
 * add std namespace for Sun CC5 compatibility
 *
 * Revision 1.2  1999/01/15 22:53:43  wenaus
 * version with constructors for table-based loading
 *
 **************************************************************************/
#ifndef StFtpcHitCollection_hh
#define StFtpcHitCollection_hh
using namespace std;
#include <vector>
#include "StEvent/StFtpcHit.hh"

#ifdef ST_NO_TEMPLATE_DEF_ARGS
typedef vector<StFtpcHit*, allocator<StFtpcHit*> >            StFtpcHitCollection;
typedef vector<StFtpcHit*, allocator<StFtpcHit*> >::iterator  StFtpcHitIterator;
#else
typedef vector<StFtpcHit*>            StFtpcHitCollection;
typedef vector<StFtpcHit*>::iterator  StFtpcHitIterator;
#endif

#endif
