/***************************************************************************
 *
 * $Id: StSmdPhiHitCollection.hh,v 1.3 1999/03/04 18:17:19 ullrich Exp $
 *
 * Author: Thomas Ullrich, Jan 1999
 ***************************************************************************
 *
 * Description:
 *
 ***************************************************************************
 *
 * $Log: StSmdPhiHitCollection.hh,v $
 * Revision 1.3  1999/03/04 18:17:19  ullrich
 * Namespace std not used if ST_NO_NAMESPACES defined
 *
 * Revision 1.2  1999/03/04 15:57:01  wenaus
 * add std namespace for Sun CC5 compatibility
 *
 * Revision 1.1  1999/02/23 15:45:54  ullrich
 * Initial Revision
 *
 **************************************************************************/
#ifndef StSmdPhiHitCollection_hh
#define StSmdPhiHitCollection_hh

#include <vector>
#include "StEvent/StSmdPhiHit.hh"
#if !defined(ST_NO_NAMESPACES)
using namespace std;
#endif

#ifdef ST_NO_TEMPLATE_DEF_ARGS
typedef vector<StSmdPhiHit, allocator<StSmdPhiHit> >           StSmdPhiHitCollection;
typedef vector<StSmdPhiHit, allocator<StSmdPhiHit> >::iterator StSmdPhiHitIterator;
#else
typedef vector<StSmdPhiHit>            StSmdPhiHitCollection;
typedef vector<StSmdPhiHit>::iterator  StSmdPhiHitIterator;
#endif

#endif
