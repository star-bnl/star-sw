#include "StMessMgr.h"
#include "StDetectorDbMaker.h"
#include "StDetectorDbFTPCGas.h"
#include "StDetectorDbFTPCVoltage.h"
#include "StDetectorDbRichScalers.h"
#include "StDetectorDbTpcRDOMasks.h"
#include "StDetectorDbMagnet.h"
#include "StDetectorDbClock.h"
#include "StDetectorDbSpaceCharge.h"
#include "StDetectorDbTpcVoltages.h"
#include "StDetectorDbBeamInfo.h"
#include "StDetectorDbTriggerID.h"
#include "StDetectorDbIntegratedTriggerID.h"

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

    // Update Beam Info
    StDetectorDbBeamInfo* beam = StDetectorDbBeamInfo::instance();
    beam->update(this);

    // Update triggerID 
    StDetectorDbTriggerID* trigger = StDetectorDbTriggerID::instance();
    trigger->update(this);
//    ~gMess << *trigger;

    // Update integratedTriggerID 
    StDetectorDbIntegratedTriggerID* inTrigger = StDetectorDbIntegratedTriggerID::instance();
    inTrigger->update(this);
//    ~gMess << *inTrigger;

    // Update the ftpc voltage
    StDetectorDbFTPCVoltage* ftpcVolt = StDetectorDbFTPCVoltage::instance();
    ftpcVolt->update(this);
//    ~gMess << *ftpcVolt;

    return StMaker::InitRun(runNumber);
}
//_____________________________________________________________________________
Int_t StDetectorDbMaker::Make(){

   
    // Update FTPC Gas
    StDetectorDbFTPCGas* gas = StDetectorDbFTPCGas::instance();
    gas->update(this);
    
    // Update the ftpc voltage
    StDetectorDbFTPCVoltage* ftpcVolt = StDetectorDbFTPCVoltage::instance();
    ftpcVolt->update(this);
//    ~gMess << *ftpcVolt;

    // Update Rich Scalers/Voltages
    StDetectorDbRichScalers* scalers = StDetectorDbRichScalers::instance();
    scalers->update(this);

    // Update the tpc voltages
    StDetectorDbTpcVoltages* tpcVolts = StDetectorDbTpcVoltages::instance();
    tpcVolts->update(this);
    
    // Also need to update instances for classes done in InitRun.
    // This is needed because of a feature in TTable
    // Please ingore unused variables..the call to instane() is needed!
    StDetectorDbTpcRDOMasks* masks = StDetectorDbTpcRDOMasks::instance();
    StDetectorDbMagnet* magnet = StDetectorDbMagnet::instance();
    StDetectorDbSpaceCharge* spaceCharge = StDetectorDbSpaceCharge::instance();
    StDetectorDbClock* clock = StDetectorDbClock::instance();
    StDetectorDbBeamInfo* beam = StDetectorDbBeamInfo::instance();
    StDetectorDbTriggerID* trigger = StDetectorDbTriggerID::instance();
    StDetectorDbIntegratedTriggerID* inTrigger = StDetectorDbIntegratedTriggerID::instance();
    if (masks||magnet||clock||beam||trigger||inTrigger){/*warnOff*/}
    
    
    
    
    
    // Jamie Asked for SpaceCharge to be couted every event
    ~gMess << "Space Charge Correction = " << spaceCharge->getSpaceChargeCoulombs()
	 << " Coulombs" << endm;

    return kStOK;
}
