#include "StDb_tpcVoltages.h"

ClassImp(StDb_tpcVoltages)
//_______________________________________________________
void StDb_tpcVoltages::Streamer(typeAcceptor* accept){

if(!mstruct) mstruct = new tpcVoltages;

accept->pass("membraneVoltage",mstruct->membraneVoltage,sizeof(mstruct->membraneVoltage));
accept->pass("stripe181EastIFCVoltage",mstruct->stripe181EastIFCVoltage,sizeof(mstruct->stripe181EastIFCVoltage));
accept->pass("stripe182EastIFCVoltage",mstruct->stripe182EastIFCVoltage,sizeof(mstruct->stripe182EastIFCVoltage));
accept->pass("stripe181WestIFCVoltage",mstruct->stripe181WestIFCVoltage,sizeof(mstruct->stripe181WestIFCVoltage));
accept->pass("stripe182WestIFCVoltage",mstruct->stripe182WestIFCVoltage,sizeof(mstruct->stripe182WestIFCVoltage));
accept->pass("stripe181EastOFCVoltage",mstruct->stripe181EastOFCVoltage,sizeof(mstruct->stripe181EastOFCVoltage));
accept->pass("stripe182EastOFCVoltage",mstruct->stripe182EastOFCVoltage,sizeof(mstruct->stripe182EastOFCVoltage));
accept->pass("stripe181WestOFCVoltage",mstruct->stripe181WestOFCVoltage,sizeof(mstruct->stripe181WestOFCVoltage));
accept->pass("stripe182WestOFCVoltage",mstruct->stripe182WestOFCVoltage,sizeof(mstruct->stripe182WestOFCVoltage));

}

