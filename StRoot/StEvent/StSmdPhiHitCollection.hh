/***************************************************************************
 *
 * $Id: StSmdPhiHitCollection.hh,v 1.1 1999/02/23 15:45:54 ullrich Exp $
 *
 * Author: Thomas Ullrich, Jan 1999
 ***************************************************************************
 *
 * Description:
 *
 ***************************************************************************
 *
 * $Log: StSmdPhiHitCollection.hh,v $
 * Revision 1.1  1999/02/23 15:45:54  ullrich
 * Initial Revision
 *
 * Revision 1.1  1999/02/23 15:45:54  ullrich
 * Initial Revision
 *
#ifndef StSmdPhiHitCollection_hh
#define StSmdPhiHitCollection_hh
using namespace std;
#include <vector>
#include "StEvent/StSmdPhiHit.hh"

#ifdef ST_NO_TEMPLATE_DEF_ARGS
typedef vector<StSmdPhiHit, allocator<StSmdPhiHit> >           StSmdPhiHitCollection;
typedef vector<StSmdPhiHit, allocator<StSmdPhiHit> >::iterator StSmdPhiHitIterator;
#else
typedef vector<StSmdPhiHit>            StSmdPhiHitCollection;
typedef vector<StSmdPhiHit>::iterator  StSmdPhiHitIterator;
#endif

#endif
