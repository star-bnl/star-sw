/***************************************************************************
 *
 * $Id: StSvtWaferHitCollection.cxx,v 2.1 1999/10/13 19:45:22 ullrich Exp $
 *
 * Author: Thomas Ullrich, Sep 1999
 ***************************************************************************
 *
 * Description:
 *
 ***************************************************************************
 *
 * $Log: StSvtWaferHitCollection.cxx,v $
 * Revision 2.1  1999/10/13 19:45:22  ullrich
 * Initial Revision
 *
 * Revision 2.1  1999/10/13 19:45:22  ullrich
 * Initial Revision
 *
 **************************************************************************/
#include "StSvtWaferHitCollection.h"

static const char rcsid[] = "$Id: StSvtWaferHitCollection.cxx,v 2.1 1999/10/13 19:45:22 ullrich Exp $";

ClassImp(StSvtWaferHitCollection)

StSvtWaferHitCollection::StSvtWaferHitCollection() { /* noop */ }

StSvtWaferHitCollection::~StSvtWaferHitCollection() { /* noop */ }

const StSPtrVecSvtHit&
StSvtWaferHitCollection::hits() const { return mHits; }

StSPtrVecSvtHit&
StSvtWaferHitCollection::hits() { return mHits; }
