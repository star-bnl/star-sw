/***************************************************************************
 *
 * $Id: StEmcModule.cxx,v 2.1 2000/02/23 17:34:10 ullrich Exp $
 *
 * Author: Akio Ogawa, Jan 2000
 ***************************************************************************
 *
 * Description:
 *
 ***************************************************************************
 *
 * $Log: StEmcModule.cxx,v $
 * Revision 2.1  2000/02/23 17:34:10  ullrich
 * Initial Revision
 *
 **************************************************************************/
#include "StEmcModule.h"
#include "StEmcRawHit.h"

static const char rcsid[] = "$Id: StEmcModule.cxx,v 2.1 2000/02/23 17:34:10 ullrich Exp $";

ClassImp(StEmcModule)

StEmcModule::StEmcModule() { /* noop */ }

StEmcModule::~StEmcModule()
{
    for (unsigned int i=0; i<mHits.size(); i++) {
	delete mHits[i];
	mHits[i] = 0;
    }
}
  
UInt_t
StEmcModule::numberOfHits() const {return mHits.size();}

const StSPtrVecEmcRawHit&
StEmcModule::hits() const { return mHits; }

StSPtrVecEmcRawHit&
StEmcModule::hits() { return mHits; }
