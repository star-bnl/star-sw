/***************************************************************************
 *
 * $Id: StEmcPreShowerHitCollection.hh,v 1.3 1999/03/04 18:16:56 ullrich Exp $
 *
 * Author: Thomas Ullrich, Jan 1999
 ***************************************************************************
 *
 * Description:
 *
 ***************************************************************************
 *
 * $Log: StEmcPreShowerHitCollection.hh,v $
 * Revision 1.3  1999/03/04 18:16:56  ullrich
 * Namespace std not used if ST_NO_NAMESPACES defined
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

#include <vector>
#include "StEvent/StEmcPreShowerHit.hh"
#if !defined(ST_NO_NAMESPACES)
using namespace std;
#endif

#ifdef ST_NO_TEMPLATE_DEF_ARGS
typedef vector<StEmcPreShowerHit, allocator<StEmcPreShowerHit> >           StEmcPreShowerHitCollection;
typedef vector<StEmcPreShowerHit, allocator<StEmcPreShowerHit> >::iterator StEmcPreShowerHitIterator;
#else
typedef vector<StEmcPreShowerHit>            StEmcPreShowerHitCollection;
typedef vector<StEmcPreShowerHit>::iterator  StEmcPreShowerHitIterator;
#endif

#endif
