#include "StDb_tpcTimeOffsets.h"

ClassImp(StDb_tpcTimeOffsets)
//_______________________________________________________
void StDb_tpcTimeOffsets::Streamer(typeAcceptor* accept){

if(!mstruct) mstruct = new tpcTimeOffsets;

accept->pass("innerSectorTimeOffsets",mstruct->innerSectorTimeOffsets,sizeof(mstruct->innerSectorTimeOffsets));
accept->pass("outerSectorTimeOffsets",mstruct->outerSectorTimeOffsets,sizeof(mstruct->outerSectorTimeOffsets));

}

