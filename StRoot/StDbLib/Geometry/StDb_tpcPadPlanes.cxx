#include "StDb_tpcPadPlanes.h"

ClassImp(StDb_tpcPadPlanes)
//_______________________________________________________
void StDb_tpcPadPlanes::Streamer(typeAcceptor* accept){

if(!mstruct) mstruct = new tpcPadPlanes;

accept->pass("padRows",mstruct->padRows,sizeof(mstruct->padRows));
accept->pass("innerPadRows",mstruct->innerPadRows,sizeof(mstruct->innerPadRows));
accept->pass("innerPadRows48",mstruct->innerPadRows48,sizeof(mstruct->innerPadRows48));
accept->pass("innerPadRows52",mstruct->innerPadRows52,sizeof(mstruct->innerPadRows52));
accept->pass("outerPadRows",mstruct->outerPadRows,sizeof(mstruct->outerPadRows));
accept->pass("superInnerPadRows",mstruct->superInnerPadRows,sizeof(mstruct->superInnerPadRows));
accept->pass("superOuterPadRows",mstruct->superOuterPadRows,sizeof(mstruct->superOuterPadRows));
accept->pass("innerSectorPadWidth",mstruct->innerSectorPadWidth,sizeof(mstruct->innerSectorPadWidth));
accept->pass("innerSectorPadLength",mstruct->innerSectorPadLength,sizeof(mstruct->innerSectorPadLength));
accept->pass("innerSectorPadPitch",mstruct->innerSectorPadPitch,sizeof(mstruct->innerSectorPadPitch));
accept->pass("innerSectorRowPitch1",mstruct->innerSectorRowPitch1,sizeof(mstruct->innerSectorRowPitch1));
accept->pass("innerSectorRowPitch2",mstruct->innerSectorRowPitch2,sizeof(mstruct->innerSectorRowPitch2));
accept->pass("firstPadRow",mstruct->firstPadRow,sizeof(mstruct->firstPadRow));
accept->pass("firstOuterSectorPadRow",mstruct->firstOuterSectorPadRow,sizeof(mstruct->firstOuterSectorPadRow));
accept->pass("lastOuterSectorPadRow",mstruct->lastOuterSectorPadRow,sizeof(mstruct->lastOuterSectorPadRow));
accept->pass("firstRowWidth",mstruct->firstRowWidth,sizeof(mstruct->firstRowWidth));
accept->pass("lastRowWidth",mstruct->lastRowWidth,sizeof(mstruct->lastRowWidth));
accept->pass("outerSectorPadWidth",mstruct->outerSectorPadWidth,sizeof(mstruct->outerSectorPadWidth));
accept->pass("outerSectorPadLength",mstruct->outerSectorPadLength,sizeof(mstruct->outerSectorPadLength));
accept->pass("outerSectorPadPitch",mstruct->outerSectorPadPitch,sizeof(mstruct->outerSectorPadPitch));
accept->pass("outerSectorRowPitch",mstruct->outerSectorRowPitch,sizeof(mstruct->outerSectorRowPitch));
accept->pass("outerSectorLength",mstruct->outerSectorLength,sizeof(mstruct->outerSectorLength));
accept->pass("ioSectorSeparation",mstruct->ioSectorSeparation,sizeof(mstruct->ioSectorSeparation));
accept->pass("innerSectorEdge",mstruct->innerSectorEdge,sizeof(mstruct->innerSectorEdge));
accept->pass("outerSectorEdge",mstruct->outerSectorEdge,sizeof(mstruct->outerSectorEdge));
accept->pass("innerPadsPerRow",mstruct->innerPadsPerRow,sizeof(mstruct->innerPadsPerRow));
accept->pass("outerPadsPerRow",mstruct->outerPadsPerRow,sizeof(mstruct->outerPadsPerRow));
accept->pass("innerRowRadii",mstruct->innerRowRadii,sizeof(mstruct->innerRowRadii));
accept->pass("outerRowRadii",mstruct->outerRowRadii,sizeof(mstruct->outerRowRadii));

}

