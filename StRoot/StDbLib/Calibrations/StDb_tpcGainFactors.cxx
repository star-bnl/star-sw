#include "StDb_tpcGainFactors.h"

ClassImp(StDb_tpcGainFactors)
//_______________________________________________________
void StDb_tpcGainFactors::Streamer(typeAcceptor* accept){

if(!mstruct) mstruct = new tpcGainFactors;

accept->pass("innerSectorGainFactors",mstruct->innerSectorGainFactors,sizeof(mstruct->innerSectorGainFactors));
accept->pass("outerSectorGainFactors",mstruct->outerSectorGainFactors,sizeof(mstruct->outerSectorGainFactors));

}

