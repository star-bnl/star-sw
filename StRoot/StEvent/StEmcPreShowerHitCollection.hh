/***************************************************************************
 *
 * $Id: StEmcPreShowerHitCollection.hh,v 1.2 1999/03/04 15:56:55 wenaus Exp $
 *
 * Author: Thomas Ullrich, Jan 1999
 ***************************************************************************
 *
 * Description:
 *
 ***************************************************************************
 *
 * $Log: StEmcPreShowerHitCollection.hh,v $
 * Revision 1.2  1999/03/04 15:56:55  wenaus
 * add std namespace for Sun CC5 compatibility
 *
 * Revision 1.2  1999/03/04 15:56:55  wenaus
 * add std namespace for Sun CC5 compatibility
 *
 * Revision 1.1  1999/02/23 15:45:50  ullrich
 * Initial Revision
 *
 **************************************************************************/
#ifndef StEmcPreShowerHitCollection_hh
#define StEmcPreShowerHitCollection_hh
using namespace std;
#include <vector>
#include "StEvent/StEmcPreShowerHit.hh"

#ifdef ST_NO_TEMPLATE_DEF_ARGS
typedef vector<StEmcPreShowerHit, allocator<StEmcPreShowerHit> >           StEmcPreShowerHitCollection;
typedef vector<StEmcPreShowerHit, allocator<StEmcPreShowerHit> >::iterator StEmcPreShowerHitIterator;
#else
typedef vector<StEmcPreShowerHit>            StEmcPreShowerHitCollection;
typedef vector<StEmcPreShowerHit>::iterator  StEmcPreShowerHitIterator;
#endif

#endif
