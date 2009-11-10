//
// $Id: StEventCompendiumMaker.cxx,v 1.2 2009/11/10 20:42:33 fisyak Exp $
//
//#include <iostream>
#include "StEventCompendiumMaker.h"
#include "TDataSet.h"
#include "StDetectorDbMaker/St_MagFactorC.h"

#include "StMessMgr.h"

#include "StEvent.h"
#include "StEventSummary.h"

static const char rcsid[] = "$Id: StEventCompendiumMaker.cxx,v 1.2 2009/11/10 20:42:33 fisyak Exp $";

void fillEventSummary(StEvent* e);

ClassImp(StEventCompendiumMaker)


StEventCompendiumMaker::StEventCompendiumMaker(const char *name): StMaker(name)
{

}
StEventCompendiumMaker::~StEventCompendiumMaker() { /* nopt */ }

Int_t StEventCompendiumMaker::Init()
{
    return StMaker::Init();
}
void StEventCompendiumMaker::Clear(const char* c)
{
    return StMaker::Clear(c);
}

Int_t StEventCompendiumMaker::Make(){
    
    StEvent* rEvent = 0;
    rEvent = (StEvent*) GetInputDS("StEvent");
    
    if (!rEvent) {
	gMessMgr->Warning() << "StEventCompendiumMaker::Make: No StEvent found, bail out!"  << endm;
	return kStWarn;
    }
    fillEventSummary(rEvent);
    
    // the magnetic field needs to be obtained from the database.
    // this are the magic words...
    St_MagFactorC* mMagTable = St_MagFactorC::instance();
    double scalef = mMagTable->ScaleFactor();
    double bfieldz = scalef * 4.97952;  // (value returned from gufld at 0,0,0 for FullField)
    rEvent->summary()->setMagneticField(bfieldz);
    
    if (Debug()) {
	rEvent->summary()->Dump();
    }
    return kStOK;
}
Int_t StEventCompendiumMaker::Finish()
{
    return kStOK;
}
