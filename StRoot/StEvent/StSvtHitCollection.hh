/***************************************************************************
 *
 * $Id: StSvtHitCollection.hh,v 1.2 1999/01/15 22:53:56 wenaus Exp $
 *
 * Author: Thomas Ullrich, Jan 1999
 ***************************************************************************
 *
 * Description:
 *
 ***************************************************************************
 *
 * $Log: StSvtHitCollection.hh,v $
 * Revision 1.2  1999/01/15 22:53:56  wenaus
 * version with constructors for table-based loading
 *
 **************************************************************************/
#ifndef StSvtHitCollection_hh
#define StSvtHitCollection_hh
#include "StEvent/StSvtHit.hh"
#include <vector>

#ifdef ST_NO_TEMPLATE_DEF_ARGS
typedef vector<StSvtHit*, allocator<StSvtHit*> >            StSvtHitCollection;
typedef vector<StSvtHit*, allocator<StSvtHit*> >::iterator  StSvtHitIterator;
#else
typedef vector<StSvtHit*>            StSvtHitCollection;
typedef vector<StSvtHit*>::iterator  StSvtHitIterator;
#endif

#endif
