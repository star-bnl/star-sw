/***************************************************************************
 *
 * $Id: StEmcModule.cxx,v 2.2 2000/07/28 19:49:27 akio Exp $
 *
 * Author: Akio Ogawa, Jan 2000
 ***************************************************************************
 *
 * Description:
 *
 ***************************************************************************
 *
 * $Log: StEmcModule.cxx,v $
 * Revision 2.2  2000/07/28 19:49:27  akio
 * Change in Detector Id for Endcap SMD
 *
 * Revision 2.1  2000/02/23 17:34:10  ullrich
 * Initial Revision
 *
 **************************************************************************/
#include "StEmcModule.h"
#include "StEmcRawHit.h"

static const char rcsid[] = "$Id: StEmcModule.cxx,v 2.2 2000/07/28 19:49:27 akio Exp $";

ClassImp(StEmcModule)

StEmcModule::StEmcModule() { /* noop */ }

StEmcModule::~StEmcModule() { /* noop */ }
  
UInt_t
StEmcModule::numberOfHits() const {return mHits.size();}

const StSPtrVecEmcRawHit&
StEmcModule::hits() const { return mHits; }

StSPtrVecEmcRawHit&
StEmcModule::hits() { return mHits; }
