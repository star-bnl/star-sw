//
// $Id: StEventCompendiumMaker.cxx,v 1.1 2004/06/09 18:09:51 jeromel Exp $
//
//#include <iostream>
#include "StEventCompendiumMaker.h"

#include "TDataSet.h"
#include "tables/St_MagFactor_Table.h"

#include "StMessMgr.h"

#include "StEvent.h"
#include "StEventSummary.h"

static const char rcsid[] = "$Id: StEventCompendiumMaker.cxx,v 1.1 2004/06/09 18:09:51 jeromel Exp $";

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
    TDataSet *rlogdataset = GetDataBase("RunLog");
    St_MagFactor* mMagTable = (St_MagFactor*) rlogdataset->Find("MagFactor");
    if (mMagTable) {
	double scalef = (*mMagTable)[0].ScaleFactor;
	double bfieldz = scalef * 4.97952;  // (value returned from gufld at 0,0,0 for FullField)
	rEvent->summary()->setMagneticField(bfieldz);
    }
    else {
	gMessMgr->Warning() << "StEventCompendiumMaker::Make: St_MagFactor not found" << endm;
    }
    
    if (Debug()) {
	rEvent->summary()->Dump();
    }
    return kStOK;
}
Int_t StEventCompendiumMaker::Finish()
{
    return kStOK;
}
