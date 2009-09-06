#include "StJetMaker/emulator/StMuTowerEmu.h"
#include "StJetTower.h"

ClassImp(StJetTower);

StJetTower::StJetTower()
  : mAdc(0)
  , mPedestal(0)
  , mRms(0)
  , mStatus(0)
{
  mPt = mEta = mPhi = 0;
}

StJetTower::StJetTower(const StMuTowerEmu* tower)
  : StJetElement(tower->id(), tower->detectorId())
  , mAdc(tower->adc())
  , mPedestal(tower->pedestal())
  , mRms(tower->rms())
  , mStatus(tower->status())
{
  TVector3 mom(tower->px(), tower->py(), tower->pz());
  mPt  = mom.Pt();
  mEta = mom.Eta();
  mPhi = mom.Phi();
}
