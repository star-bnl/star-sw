/***************************************************************************
 *
 * $Id: StSmdEtaHitCollection.hh,v 1.2 1999/03/04 15:57:01 wenaus Exp $
 *
 * Author: Thomas Ullrich, Jan 1999
 ***************************************************************************
 *
 * Description:
 *
 ***************************************************************************
 *
 * $Log: StSmdEtaHitCollection.hh,v $
 * Revision 1.2  1999/03/04 15:57:01  wenaus
 * add std namespace for Sun CC5 compatibility
 *
 * Revision 1.2  1999/03/04 15:57:01  wenaus
 * add std namespace for Sun CC5 compatibility
 *
 * Revision 1.1  1999/02/23 15:45:53  ullrich
 * Initial Revision
 *
 **************************************************************************/
#ifndef StSmdEtaHitCollection_hh
#define StSmdEtaHitCollection_hh
using namespace std;
#include <vector>
#include "StEvent/StSmdEtaHit.hh"

#ifdef ST_NO_TEMPLATE_DEF_ARGS
typedef vector<StSmdEtaHit, allocator<StSmdEtaHit> >           StSmdEtaHitCollection;
typedef vector<StSmdEtaHit, allocator<StSmdEtaHit> >::iterator StSmdEtaHitIterator;
#else
typedef vector<StSmdEtaHit>            StSmdEtaHitCollection;
typedef vector<StSmdEtaHit>::iterator  StSmdEtaHitIterator;
#endif

#endif
