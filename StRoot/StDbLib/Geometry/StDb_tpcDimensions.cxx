#include "StDb_tpcDimensions.h"

ClassImp(StDb_tpcDimensions)
//_______________________________________________________
void StDb_tpcDimensions::Streamer(typeAcceptor* accept){

if(!mstruct) mstruct = new tpcDimensions;

accept->pass("numberOfSectors",mstruct->numberOfSectors,sizeof(mstruct->numberOfSectors));
accept->pass("tpcInnerRadius",mstruct->tpcInnerRadius,sizeof(mstruct->tpcInnerRadius));
accept->pass("tpcOuterRadius",mstruct->tpcOuterRadius,sizeof(mstruct->tpcOuterRadius));
accept->pass("tpcTotalLength",mstruct->tpcTotalLength,sizeof(mstruct->tpcTotalLength));
accept->pass("wheelInnerRadius",mstruct->wheelInnerRadius,sizeof(mstruct->wheelInnerRadius));
accept->pass("wheelOuterRadius",mstruct->wheelOuterRadius,sizeof(mstruct->wheelOuterRadius));
accept->pass("wheelThickness",mstruct->wheelThickness,sizeof(mstruct->wheelThickness));
accept->pass("senseGasOuterRadius",mstruct->senseGasOuterRadius,sizeof(mstruct->senseGasOuterRadius));
accept->pass("tpeaThickness",mstruct->tpeaThickness,sizeof(mstruct->tpeaThickness));
accept->pass("cathodeInnerRadius",mstruct->cathodeInnerRadius,sizeof(mstruct->cathodeInnerRadius));
accept->pass("cathodeOuterRadius",mstruct->cathodeOuterRadius,sizeof(mstruct->cathodeOuterRadius));
accept->pass("cathodeThickness",mstruct->cathodeThickness,sizeof(mstruct->cathodeThickness));
accept->pass("outerCuThickness",mstruct->outerCuThickness,sizeof(mstruct->outerCuThickness));
accept->pass("outerKaptonThickness",mstruct->outerKaptonThickness,sizeof(mstruct->outerKaptonThickness));
accept->pass("outerNomexThickness",mstruct->outerNomexThickness,sizeof(mstruct->outerNomexThickness));
accept->pass("outerGlueThickness",mstruct->outerGlueThickness,sizeof(mstruct->outerGlueThickness));
accept->pass("outerInsGasThickness",mstruct->outerInsGasThickness,sizeof(mstruct->outerInsGasThickness));
accept->pass("outerAlThickness",mstruct->outerAlThickness,sizeof(mstruct->outerAlThickness));
accept->pass("outerAlHoneycombThickness",mstruct->outerAlHoneycombThickness,sizeof(mstruct->outerAlHoneycombThickness));
accept->pass("innerGlueThickness",mstruct->innerGlueThickness,sizeof(mstruct->innerGlueThickness));
accept->pass("innerNomexThickness",mstruct->innerNomexThickness,sizeof(mstruct->innerNomexThickness));
accept->pass("innerKaptonThickness",mstruct->innerKaptonThickness,sizeof(mstruct->innerKaptonThickness));
accept->pass("innerAlThickness",mstruct->innerAlThickness,sizeof(mstruct->innerAlThickness));
accept->pass("innerGapWidI",mstruct->innerGapWidI,sizeof(mstruct->innerGapWidI));
accept->pass("innerGapWidO",mstruct->innerGapWidO,sizeof(mstruct->innerGapWidO));
accept->pass("innerGapHeit",mstruct->innerGapHeit,sizeof(mstruct->innerGapHeit));
accept->pass("innerGapRad",mstruct->innerGapRad,sizeof(mstruct->innerGapRad));
accept->pass("innerInWidth",mstruct->innerInWidth,sizeof(mstruct->innerInWidth));
accept->pass("innerOutWidth",mstruct->innerOutWidth,sizeof(mstruct->innerOutWidth));
accept->pass("innerHeight",mstruct->innerHeight,sizeof(mstruct->innerHeight));
accept->pass("innerPPDepth",mstruct->innerPPDepth,sizeof(mstruct->innerPPDepth));
accept->pass("innerAlDepth",mstruct->innerAlDepth,sizeof(mstruct->innerAlDepth));
accept->pass("innerMWCDepth",mstruct->innerMWCDepth,sizeof(mstruct->innerMWCDepth));
accept->pass("innerBoundary",mstruct->innerBoundary,sizeof(mstruct->innerBoundary));
accept->pass("innerRCenter",mstruct->innerRCenter,sizeof(mstruct->innerRCenter));
accept->pass("innerMWCInn",mstruct->innerMWCInn,sizeof(mstruct->innerMWCInn));
accept->pass("innerMWCOut",mstruct->innerMWCOut,sizeof(mstruct->innerMWCOut));
accept->pass("innerMVCHei",mstruct->innerMVCHei,sizeof(mstruct->innerMVCHei));
accept->pass("innerAirGaps",mstruct->innerAirGaps,sizeof(mstruct->innerAirGaps));
accept->pass("innerExtraAl",mstruct->innerExtraAl,sizeof(mstruct->innerExtraAl));
accept->pass("innerZGaps",mstruct->innerZGaps,sizeof(mstruct->innerZGaps));
accept->pass("innerZGapsSize",mstruct->innerZGapsSize,sizeof(mstruct->innerZGapsSize));
accept->pass("innerXExtraAl",mstruct->innerXExtraAl,sizeof(mstruct->innerXExtraAl));
accept->pass("innerZExtraAl",mstruct->innerZExtraAl,sizeof(mstruct->innerZExtraAl));
accept->pass("innerDXExtraAl",mstruct->innerDXExtraAl,sizeof(mstruct->innerDXExtraAl));
accept->pass("innerDZExtraAl",mstruct->innerDZExtraAl,sizeof(mstruct->innerDZExtraAl));
accept->pass("outerGapWidI",mstruct->outerGapWidI,sizeof(mstruct->outerGapWidI));
accept->pass("outerGapWidO",mstruct->outerGapWidO,sizeof(mstruct->outerGapWidO));
accept->pass("outerGapHeit",mstruct->outerGapHeit,sizeof(mstruct->outerGapHeit));
accept->pass("outerGapRad",mstruct->outerGapRad,sizeof(mstruct->outerGapRad));
accept->pass("outerInWidth",mstruct->outerInWidth,sizeof(mstruct->outerInWidth));
accept->pass("outerOutWidth",mstruct->outerOutWidth,sizeof(mstruct->outerOutWidth));
accept->pass("outerHeight",mstruct->outerHeight,sizeof(mstruct->outerHeight));
accept->pass("outerPPDepth",mstruct->outerPPDepth,sizeof(mstruct->outerPPDepth));
accept->pass("outerAlDepth",mstruct->outerAlDepth,sizeof(mstruct->outerAlDepth));
accept->pass("outerMWCDepth",mstruct->outerMWCDepth,sizeof(mstruct->outerMWCDepth));
accept->pass("outerBoundary",mstruct->outerBoundary,sizeof(mstruct->outerBoundary));
accept->pass("outerRCenter",mstruct->outerRCenter,sizeof(mstruct->outerRCenter));
accept->pass("outerMWCInn",mstruct->outerMWCInn,sizeof(mstruct->outerMWCInn));
accept->pass("outerMWCOut",mstruct->outerMWCOut,sizeof(mstruct->outerMWCOut));
accept->pass("outerMVCHei",mstruct->outerMVCHei,sizeof(mstruct->outerMVCHei));
accept->pass("outerAirGaps",mstruct->outerAirGaps,sizeof(mstruct->outerAirGaps));
accept->pass("outerExtraAl",mstruct->outerExtraAl,sizeof(mstruct->outerExtraAl));
accept->pass("outerZGaps",mstruct->outerZGaps,sizeof(mstruct->outerZGaps));
accept->pass("outerZGapsSize",mstruct->outerZGapsSize,sizeof(mstruct->outerZGapsSize));
accept->pass("outerXExtraAl",mstruct->outerXExtraAl,sizeof(mstruct->outerXExtraAl));
accept->pass("outerZExtraAl",mstruct->outerZExtraAl,sizeof(mstruct->outerZExtraAl));
accept->pass("outerDXExtraAl",mstruct->outerDXExtraAl,sizeof(mstruct->outerDXExtraAl));
accept->pass("outerDZExtraAl",mstruct->outerDZExtraAl,sizeof(mstruct->outerDZExtraAl));

}

