/***************************************************************************
 *
 * $Id: StSmdHitCollection.hh,v 1.2 1999/01/15 22:53:53 wenaus Exp $
 *
 * Author: Thomas Ullrich, Jan 1999
 ***************************************************************************
 *
 * Description:
 *
 ***************************************************************************
 *
 * $Log: StSmdHitCollection.hh,v $
 * Revision 1.2  1999/01/15 22:53:53  wenaus
 * version with constructors for table-based loading
 *
 * Revision 1.2  1999/01/15 22:53:53  wenaus
 * version with constructors for table-based loading
 *
 **************************************************************************/
#ifndef StSmdHitCollection_hh
#define StSmdHitCollection_hh 
#include "StEvent/StSmdHit.hh"
#include <vector>

#ifdef ST_NO_TEMPLATE_DEF_ARGS
typedef vector<StSmdHit*, allocator<StSmdHit*> >           StSmdHitCollection;
typedef vector<StSmdHit*, allocator<StSmdHit*> >::iterator StSmdHitIterator;
#else
typedef vector<StSmdHit*>            StSmdHitCollection;
typedef vector<StSmdHit*>::iterator  StSmdHitIterator;
#endif

#endif
