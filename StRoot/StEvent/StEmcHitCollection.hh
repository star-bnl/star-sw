/***************************************************************************
 *
 * $Id: StEmcHitCollection.hh,v 1.2 1999/01/15 22:53:36 wenaus Exp $
 *
 * Author: Thomas Ullrich, Jan 1999
 ***************************************************************************
 *
 * Description:
 *
 ***************************************************************************
 *
 * $Log: StEmcHitCollection.hh,v $
 * Revision 1.2  1999/01/15 22:53:36  wenaus
 * version with constructors for table-based loading
 *
 * Revision 1.2  1999/01/15 22:53:36  wenaus
 * version with constructors for table-based loading
 *
 **************************************************************************/
#ifndef StEmcHitCollection_hh
#define StEmcHitCollection_hh
#include <vector>
#include "StEvent/StEmcHit.hh"

#ifdef ST_NO_TEMPLATE_DEF_ARGS
typedef vector<StEmcHit*, allocator<StEmcHit*> >           StEmcHitCollection;
typedef vector<StEmcHit*, allocator<StEmcHit*> >::iterator StEmcHitIterator;
#else
typedef vector<StEmcHit*>            StEmcHitCollection;
typedef vector<StEmcHit*>::iterator  StEmcHitIterator;
#endif

#endif
