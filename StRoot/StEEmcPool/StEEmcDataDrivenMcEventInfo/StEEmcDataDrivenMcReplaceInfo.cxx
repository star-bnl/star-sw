//
// Ilya Selyuzhenkov <ilya.selyuzhenkov@gmail.com>
// Indiana University Cyclotron Facility
//
// Pibero Djawotho <pibero@indiana.edu>
// Indiana University Cyclotron Facility
//

#include "StEEmcDataDrivenMcReplaceInfo.h"

ClassImp(StEEmcDataDrivenMcReplaceInfo);

StEEmcDataDrivenMcReplaceInfo::StEEmcDataDrivenMcReplaceInfo()
{
  InitArrays();
  Clear();
}

void StEEmcDataDrivenMcReplaceInfo::Clear(Option_t* options)
{
  pid = -999;
  parentPid = -999;
  firstHadronPid = -999;
  libraryShapeId = -999;
  momentum.SetXYZ(-999, -999,-999);
  energy = -999;
  energyScaleU = -999;
  energyScaleV = -999;
  highStripShiftU = -999;
  highStripShiftV = -999;
  libraryBinId = -999;
  memset(dEnergy, 0, sizeof(dEnergy));
  memset(totalEnergy, 0, sizeof(totalEnergy));
  memset(totalEnergyScaled, 0, sizeof(totalEnergyScaled));

  mMcHitsEsmdU->Clear();
  mMcHitsEsmdV->Clear();
}

void StEEmcDataDrivenMcReplaceInfo::InitArrays()
{
  mMcHitsEsmdU = new TClonesArray("StMcCalorimeterHit",1000);
  mMcHitsEsmdV = new TClonesArray("StMcCalorimeterHit",1000);
}

StMcCalorimeterHit* StEEmcDataDrivenMcReplaceInfo::newMcHitEsmdU()
{
  TClonesArray &mMcHitsEsmdUs = *mMcHitsEsmdU;
  return new (mMcHitsEsmdUs[mMcHitsEsmdUs.GetEntriesFast()]) StMcCalorimeterHit;
}

StMcCalorimeterHit *StEEmcDataDrivenMcReplaceInfo::newMcHitEsmdV()
{
  TClonesArray &mMcHitsEsmdVs = *mMcHitsEsmdV;
  return new (mMcHitsEsmdVs[mMcHitsEsmdVs.GetEntriesFast()]) StMcCalorimeterHit;
}

StMcCalorimeterHit *StEEmcDataDrivenMcReplaceInfo::addMcHitEsmdU(StMcCalorimeterHit* hit)
{
  TClonesArray &mMcHitsEsmdUs = *mMcHitsEsmdU;
  StMcCalorimeterHit* mcHitEsmdU = new (mMcHitsEsmdUs[mMcHitsEsmdUs.GetEntriesFast()]) StMcCalorimeterHit(*hit);
  mcHitEsmdU->setParentTrack(0);
  return mcHitEsmdU;
}

StMcCalorimeterHit *StEEmcDataDrivenMcReplaceInfo::addMcHitEsmdV(StMcCalorimeterHit* hit)
{
  TClonesArray &mMcHitsEsmdVs = *mMcHitsEsmdV;
  StMcCalorimeterHit *mcHitEsmdV = new (mMcHitsEsmdVs[mMcHitsEsmdVs.GetEntriesFast()]) StMcCalorimeterHit(*hit);
  mcHitEsmdV->setParentTrack(0);
  return mcHitEsmdV;
}
