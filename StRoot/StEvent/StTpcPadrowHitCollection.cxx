/***************************************************************************
 *
 * $Id: StTpcPadrowHitCollection.cxx,v 2.1 1999/10/13 19:45:30 ullrich Exp $
 *
 * Author: Thomas Ullrich, July 1999
 ***************************************************************************
 *
 * Description:
 *
 ***************************************************************************
 *
 * $Log: StTpcPadrowHitCollection.cxx,v $
 * Revision 2.1  1999/10/13 19:45:30  ullrich
 * Initial Revision
 *
 * Revision 2.1  1999/10/13 19:45:30  ullrich
 * Initial Revision
 *
 **************************************************************************/
#include "StTpcPadrowHitCollection.h"

static const char rcsid[] = "$Id: StTpcPadrowHitCollection.cxx,v 2.1 1999/10/13 19:45:30 ullrich Exp $";

ClassImp(StTpcPadrowHitCollection)

StTpcPadrowHitCollection::StTpcPadrowHitCollection() { /* noop */ }

StTpcPadrowHitCollection::~StTpcPadrowHitCollection()
{ /* noop */ }

const StSPtrVecTpcHit&
StTpcPadrowHitCollection::hits() const { return mHits; }

StSPtrVecTpcHit&
StTpcPadrowHitCollection::hits() { return mHits; }
