/***************************************************************************
 *
 * $Id: StSmdHitCollection.hh,v 1.1 1999/01/15 20:40:01 wenaus Exp $
 *
 * Author: Thomas Ullrich, Jan 1999
 ***************************************************************************
 *
 * Description:
 *
 ***************************************************************************
 *
 * $Log: StSmdHitCollection.hh,v $
 * Revision 1.1  1999/01/15 20:40:01  wenaus
 * Commit Thomas' original code
 *
 * Revision 1.2  1999/01/15 22:53:53  wenaus
 * version with constructors for table-based loading
 *
#include "StSmdHit.hh"
#ifndef StSmdHitCollection_hh
#define StSmdHitCollection_hh 
#include "StEvent/StSmdHit.hh"
#include <vector>

#ifdef ST_NO_TEMPLATE_DEF_ARGS
typedef vector<StSmdHit*, allocator<StSmdHit*> >           StSmdHitCollection;
typedef vector<StSmdHit*, allocator<StSmdHit*> >::iterator StSmdHitIterator;
#else
typedef vector<StSmdHit*>            StSmdHitCollection;
typedef vector<StSmdHit*>::iterator  StSmdHitIterator;
#endif

#endif
