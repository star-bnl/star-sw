/***************************************************************************
 *
 * $Id: StSvtHitCollection.hh,v 1.1 1999/01/15 20:40:06 wenaus Exp $
 *
 * Author: Thomas Ullrich, Jan 1999
 ***************************************************************************
 *
 * Description:
 *
 ***************************************************************************
 *
 * $Log: StSvtHitCollection.hh,v $
 * Revision 1.1  1999/01/15 20:40:06  wenaus
 * Commit Thomas' original code
 *
 **************************************************************************/
#ifndef StSvtHitCollection_hh
#define StSvtHitCollection_hh
#include "StSvtHit.hh"
#include <vector>

#ifdef ST_NO_TEMPLATE_DEF_ARGS
typedef vector<StSvtHit*, allocator<StSvtHit*> >            StSvtHitCollection;
typedef vector<StSvtHit*, allocator<StSvtHit*> >::iterator  StSvtHitIterator;
#else
typedef vector<StSvtHit*>            StSvtHitCollection;
typedef vector<StSvtHit*>::iterator  StSvtHitIterator;
#endif

#endif
