/***************************************************************************
 *
 * $Id: StEmcTowerHitCollection.hh,v 1.2 1999/03/04 15:56:56 wenaus Exp $
 *
 * Author: Thomas Ullrich, Jan 1999
 ***************************************************************************
 *
 * Description:
 *
 ***************************************************************************
 *
 * $Log: StEmcTowerHitCollection.hh,v $
 * Revision 1.2  1999/03/04 15:56:56  wenaus
 * add std namespace for Sun CC5 compatibility
 *
 * Revision 1.2  1999/03/04 15:56:56  wenaus
 * add std namespace for Sun CC5 compatibility
 *
 * Revision 1.1  1999/02/23 15:45:47  ullrich
 * Initial Revision
 *
 **************************************************************************/
#ifndef StEmcTowerHitCollection_hh
#define StEmcTowerHitCollection_hh
using namespace std;
#include <vector>
#include "StEvent/StEmcTowerHit.hh"

#ifdef ST_NO_TEMPLATE_DEF_ARGS
typedef vector<StEmcTowerHit, allocator<StEmcTowerHit> >           StEmcTowerHitCollection;
typedef vector<StEmcTowerHit, allocator<StEmcTowerHit> >::iterator StEmcTowerHitIterator;
#else
typedef vector<StEmcTowerHit>            StEmcTowerHitCollection;
typedef vector<StEmcTowerHit>::iterator  StEmcTowerHitIterator;
#endif

#endif
