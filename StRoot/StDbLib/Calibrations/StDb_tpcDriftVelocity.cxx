#include "StDb_tpcDriftVelocity.h"

ClassImp(StDb_tpcDriftVelocity)
//_______________________________________________________
void StDb_tpcDriftVelocity::Streamer(typeAcceptor* accept){

if(!mstruct) mstruct = new tpcDriftVelocity;

accept->pass("laserDriftVelocityEast",mstruct->laserDriftVelocityEast,sizeof(mstruct->laserDriftVelocityEast));
accept->pass("laserDriftVelocityWest",mstruct->laserDriftVelocityWest,sizeof(mstruct->laserDriftVelocityWest));
accept->pass("cathodeDriftVelocityEast",mstruct->cathodeDriftVelocityEast,sizeof(mstruct->cathodeDriftVelocityEast));
accept->pass("cathodeDriftVelocityWest",mstruct->cathodeDriftVelocityWest,sizeof(mstruct->cathodeDriftVelocityWest));

}

