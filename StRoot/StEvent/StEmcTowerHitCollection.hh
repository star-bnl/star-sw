/***************************************************************************
 *
 * $Id: StEmcTowerHitCollection.hh,v 1.3 1999/03/04 18:16:58 ullrich Exp $
 *
 * Author: Thomas Ullrich, Jan 1999
 ***************************************************************************
 *
 * Description:
 *
 ***************************************************************************
 *
 * $Log: StEmcTowerHitCollection.hh,v $
 * Revision 1.3  1999/03/04 18:16:58  ullrich
 * Namespace std not used if ST_NO_NAMESPACES defined
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

#include <vector>
#include "StEvent/StEmcTowerHit.hh"
#if !defined(ST_NO_NAMESPACES)
using namespace std;
#endif

#ifdef ST_NO_TEMPLATE_DEF_ARGS
typedef vector<StEmcTowerHit, allocator<StEmcTowerHit> >           StEmcTowerHitCollection;
typedef vector<StEmcTowerHit, allocator<StEmcTowerHit> >::iterator StEmcTowerHitIterator;
#else
typedef vector<StEmcTowerHit>            StEmcTowerHitCollection;
typedef vector<StEmcTowerHit>::iterator  StEmcTowerHitIterator;
#endif

#endif
