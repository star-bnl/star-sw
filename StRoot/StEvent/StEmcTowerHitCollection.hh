/***************************************************************************
 *
 * $Id: StEmcTowerHitCollection.hh,v 1.1 1999/02/23 15:45:47 ullrich Exp $
 *
 * Author: Thomas Ullrich, Jan 1999
 ***************************************************************************
 *
 * Description:
 *
 ***************************************************************************
 *
 * $Log: StEmcTowerHitCollection.hh,v $
 * Revision 1.1  1999/02/23 15:45:47  ullrich
 * Initial Revision
 *
 * Revision 1.1  1999/02/23 15:45:47  ullrich
 * Initial Revision
 *
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
