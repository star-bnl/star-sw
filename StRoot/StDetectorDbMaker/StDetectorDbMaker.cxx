#include "StDetectorDbMaker.h"
#include "StDetectorDbFTPCGas.h"
#include "StDetectorDbRichScalers.h"
#include "StDetectorDbTpcRDOMasks.h"
#include "StDetectorDbMagnet.h"

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

    StDetectorDbMagnet* magnet = StDetectorDbMagnet::instance();
    magnet->update(this);
    
    return StMaker::InitRun(runNumber);
}
//_____________________________________________________________________________
Int_t StDetectorDbMaker::Make(){

   
    // Update FTPC Gas
   //  StDetectorDbFTPCGas* gas = StDetectorDbFTPCGas::instance();
//     gas->update(this);

    // Update Rich Scalers/Voltages
    StDetectorDbRichScalers* scalers = StDetectorDbRichScalers::instance();
    scalers->update(this);
    
    return kStOK;
}










