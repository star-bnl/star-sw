/***************************************************************************
 *
 * $Id: StFtpcHitCollection.hh,v 1.1 1999/01/15 20:39:47 wenaus Exp $
 *
 * Author: Thomas Ullrich, Jan 1999
 ***************************************************************************
 *
 * Description:
 *
 ***************************************************************************
 *
 * $Log: StFtpcHitCollection.hh,v $
 * Revision 1.1  1999/01/15 20:39:47  wenaus
 * Commit Thomas' original code
 *
 **************************************************************************/
#ifndef StFtpcHitCollection_hh
#define StFtpcHitCollection_hh
#include <vector>
#include "StFtpcHit.hh"

#ifdef ST_NO_TEMPLATE_DEF_ARGS
typedef vector<StFtpcHit*, allocator<StFtpcHit*> >            StFtpcHitCollection;
typedef vector<StFtpcHit*, allocator<StFtpcHit*> >::iterator  StFtpcHitIterator;
#else
typedef vector<StFtpcHit*>            StFtpcHitCollection;
typedef vector<StFtpcHit*>::iterator  StFtpcHitIterator;
#endif

#endif
