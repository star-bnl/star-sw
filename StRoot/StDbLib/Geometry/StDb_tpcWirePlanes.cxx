#include "StDb_tpcWirePlanes.h"

ClassImp(StDb_tpcWirePlanes)
//_______________________________________________________
void StDb_tpcWirePlanes::Streamer(typeAcceptor* accept){

if(!mstruct) mstruct = new tpcWirePlanes;

accept->pass("anodeWireRadius",mstruct->anodeWireRadius,sizeof(mstruct->anodeWireRadius));
accept->pass("frischGridWireRadius",mstruct->frischGridWireRadius,sizeof(mstruct->frischGridWireRadius));
accept->pass("gatingGridWireRadius",mstruct->gatingGridWireRadius,sizeof(mstruct->gatingGridWireRadius));
accept->pass("anodeWirePitch",mstruct->anodeWirePitch,sizeof(mstruct->anodeWirePitch));
accept->pass("frischGridWirePitch",mstruct->frischGridWirePitch,sizeof(mstruct->frischGridWirePitch));
accept->pass("gatingGridWirePitch",mstruct->gatingGridWirePitch,sizeof(mstruct->gatingGridWirePitch));
accept->pass("innerSectorAnodeWirePadPlaneSeparation",mstruct->innerSectorAnodeWirePadPlaneSeparation,sizeof(mstruct->innerSectorAnodeWirePadPlaneSeparation));
accept->pass("innerSectorFrischGridPadPlaneSeparation",mstruct->innerSectorFrischGridPadPlaneSeparation,sizeof(mstruct->innerSectorFrischGridPadPlaneSeparation));
accept->pass("innerSectorGatingGridPadPlaneSeparation",mstruct->innerSectorGatingGridPadPlaneSeparation,sizeof(mstruct->innerSectorGatingGridPadPlaneSeparation));
accept->pass("outerSectorAnodeWirePadPlaneSeparation",mstruct->outerSectorAnodeWirePadPlaneSeparation,sizeof(mstruct->outerSectorAnodeWirePadPlaneSeparation));
accept->pass("outerSectorFrischGridPadPlaneSeparation",mstruct->outerSectorFrischGridPadPlaneSeparation,sizeof(mstruct->outerSectorFrischGridPadPlaneSeparation));
accept->pass("outerSectorGatingGridPadPlaneSeparation",mstruct->outerSectorGatingGridPadPlaneSeparation,sizeof(mstruct->outerSectorGatingGridPadPlaneSeparation));
accept->pass("numberOfInnerSectorAnodeWires",mstruct->numberOfInnerSectorAnodeWires,sizeof(mstruct->numberOfInnerSectorAnodeWires));
accept->pass("numberOfInnerSectorFrischGridWires",mstruct->numberOfInnerSectorFrischGridWires,sizeof(mstruct->numberOfInnerSectorFrischGridWires));
accept->pass("numberOfInnerSectorGatingGridWires",mstruct->numberOfInnerSectorGatingGridWires,sizeof(mstruct->numberOfInnerSectorGatingGridWires));
accept->pass("firstInnerSectorAnodeWire",mstruct->firstInnerSectorAnodeWire,sizeof(mstruct->firstInnerSectorAnodeWire));
accept->pass("firstInnerSectorFrischGridWire",mstruct->firstInnerSectorFrischGridWire,sizeof(mstruct->firstInnerSectorFrischGridWire));
accept->pass("firstInnerSectorGatingGridWire",mstruct->firstInnerSectorGatingGridWire,sizeof(mstruct->firstInnerSectorGatingGridWire));
accept->pass("lastInnerSectorAnodeWire",mstruct->lastInnerSectorAnodeWire,sizeof(mstruct->lastInnerSectorAnodeWire));
accept->pass("numberOfOuterSectorAnodeWires",mstruct->numberOfOuterSectorAnodeWires,sizeof(mstruct->numberOfOuterSectorAnodeWires));
accept->pass("numberOfOuterSectorFrischGridWires",mstruct->numberOfOuterSectorFrischGridWires,sizeof(mstruct->numberOfOuterSectorFrischGridWires));
accept->pass("numberOfOuterSectorGatingGridWires",mstruct->numberOfOuterSectorGatingGridWires,sizeof(mstruct->numberOfOuterSectorGatingGridWires));
accept->pass("firstOuterSectorAnodeWire",mstruct->firstOuterSectorAnodeWire,sizeof(mstruct->firstOuterSectorAnodeWire));
accept->pass("firstOuterSectorFrischGridWire",mstruct->firstOuterSectorFrischGridWire,sizeof(mstruct->firstOuterSectorFrischGridWire));
accept->pass("firstOuterSectorGatingGridWire",mstruct->firstOuterSectorGatingGridWire,sizeof(mstruct->firstOuterSectorGatingGridWire));
accept->pass("lastOuterSectorAnodeWire",mstruct->lastOuterSectorAnodeWire,sizeof(mstruct->lastOuterSectorAnodeWire));

}

