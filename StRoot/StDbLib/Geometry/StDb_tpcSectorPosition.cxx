#include "StDb_tpcSectorPosition.h"

ClassImp(StDb_tpcSectorPosition)
//_______________________________________________________
void StDb_tpcSectorPosition::Streamer(typeAcceptor* accept){

if(!mstruct) mstruct = new tpcSectorPosition;

accept->pass("innerSectorLocalxShift",mstruct->innerSectorLocalxShift,sizeof(mstruct->innerSectorLocalxShift));
accept->pass("innerSectorLocalyShift",mstruct->innerSectorLocalyShift,sizeof(mstruct->innerSectorLocalyShift));
accept->pass("innerSectorRotationAngle",mstruct->innerSectorRotationAngle,sizeof(mstruct->innerSectorRotationAngle));
accept->pass("innerSectorCovMatrix",mstruct->innerSectorCovMatrix,sizeof(mstruct->innerSectorCovMatrix));
accept->pass("outerSectorLocalxShift",mstruct->outerSectorLocalxShift,sizeof(mstruct->outerSectorLocalxShift));
accept->pass("outerSectorLocalyShift",mstruct->outerSectorLocalyShift,sizeof(mstruct->outerSectorLocalyShift));
accept->pass("outerSectorRotationAngle",mstruct->outerSectorRotationAngle,sizeof(mstruct->outerSectorRotationAngle));
accept->pass("outerSectorCovMatrix",mstruct->outerSectorCovMatrix,sizeof(mstruct->outerSectorCovMatrix));

}

