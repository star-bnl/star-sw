#include "StDetectorDbMaker.h"
#include "StDetectorDbFTPCGas.h"
#include "StDetectorDbRichScalers.h"
#include "StDetectorDbTpcRDOMasks.h"
#include "StDetectorDbMagnet.h"
#include "StDetectorDbClock.h"
#include "StDetectorDbSpaceCharge.h"

#include "StEvent/StEvent.h"
#include "StEvent/StDetectorState.h"

ClassImp(StDetectorDbMaker)

//_____________________________________________________________________________
StDetectorDbMaker::StDetectorDbMaker(const char *name):StMaker(name){
    
}
//_____________________________________________________________________________
StDetectorDbMaker::~StDetectorDbMaker(){
}
//_____________________________________________________________________________
Int_t StDetectorDbMaker::Init(){
    
    return StMaker::Init();
}
Int_t StDetectorDbMaker::InitRun(int runNumber){

    // Update RDO Masks
    StDetectorDbTpcRDOMasks* masks = StDetectorDbTpcRDOMasks::instance();
    masks->update(this);

    // Update Magnet Current
    StDetectorDbMagnet* magnet = StDetectorDbMagnet::instance();
    magnet->update(this);

    // Update Space Charge Correction (must be done after magnet)
    StDetectorDbSpaceCharge* spaceCharge = StDetectorDbSpaceCharge::instance();
    spaceCharge->update(this);
    
    // Update Clock Frequency
    StDetectorDbClock* clock = StDetectorDbClock::instance();
    clock->update(this);
    
    return StMaker::InitRun(runNumber);
}
//_____________________________________________________________________________
Int_t StDetectorDbMaker::Make(){

   
    // Update FTPC Gas
    StDetectorDbFTPCGas* gas = StDetectorDbFTPCGas::instance();
    gas->update(this);

    // Update Rich Scalers/Voltages
    StDetectorDbRichScalers* scalers = StDetectorDbRichScalers::instance();
    scalers->update(this);

    // Also need to update instances for classes done in InitRun.
    // This is needed because of a feature in TTable
    StDetectorDbTpcRDOMasks* masks = StDetectorDbTpcRDOMasks::instance();
    StDetectorDbMagnet* magnet = StDetectorDbMagnet::instance();
    StDetectorDbSpaceCharge* spaceCharge = StDetectorDbSpaceCharge::instance();
    StDetectorDbClock* clock = StDetectorDbClock::instance();

    // Jamie Asked for SpaceCharge to be couted every event
    cout << "Space Charge Correction = " << spaceCharge->getSpaceChargeCoulombs()
	 << " Coulombs" << endl;
    
    // Fill StEvent::StDetectorState
    StEvent* rEvent = 0;
    rEvent = static_cast<StEvent*>(GetInputDS("StEvent"));
    if(rEvent){
	StDetectorState* richState = new StDetectorState(kRichId,scalers->getRichHVStatus());

	if(richState){
	    rEvent->addDetectorState(richState);
	}
	
    }

    return kStOK;
}










