/***************************************************************************
 *
 * $Id: StFtpcSectorHitCollection.cxx,v 2.1 1999/10/13 19:44:42 ullrich Exp $
 *
 * Author: Thomas Ullrich, July 1999
 ***************************************************************************
 *
 * Description:
 *
 ***************************************************************************
 *
 * $Log: StFtpcSectorHitCollection.cxx,v $
 * Revision 2.1  1999/10/13 19:44:42  ullrich
 * Initial Revision
 *
 * Revision 2.1  1999/10/13 19:44:42  ullrich
 * Initial Revision
 *
 **************************************************************************/
#include "StFtpcSectorHitCollection.h"

static const char rcsid[] = "$Id: StFtpcSectorHitCollection.cxx,v 2.1 1999/10/13 19:44:42 ullrich Exp $";

ClassImp(StFtpcSectorHitCollection)

StFtpcSectorHitCollection::StFtpcSectorHitCollection() { /* noop */ }

StFtpcSectorHitCollection::~StFtpcSectorHitCollection()
{ /* noop */ }

const StSPtrVecFtpcHit&
StFtpcSectorHitCollection::hits() const { return mHits; }

StSPtrVecFtpcHit&
StFtpcSectorHitCollection::hits() { return mHits; }
