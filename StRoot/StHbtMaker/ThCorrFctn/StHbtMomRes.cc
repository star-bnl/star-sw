#include "StHbtMaker/ThCorrFctn/StHbtMomRes.hh"

StHbtMomRes::StHbtMomRes() {
  // Use Fabrice's initial parameters for pions
  setPDGPid(211);
  mMult = 1.0;
}
StHbtMomRes::StHbtMomRes(int pdgPid) {
  setPDGPid(pdgPid);
} 
inline Float_t StHbtMomRes::getPtError(const Float_t pt) const {
  return mMult * (mPta + mPtb*pow(pt,mPtalfa));
}
inline Float_t StHbtMomRes::getPhiError(const Float_t p) const {
  return mMult * (mPhia + mPhib*pow(p,mPhialfa));
}
inline Float_t StHbtMomRes::getThetaError(const Float_t p) const {
  return mMult * (mThetaa + mThetab*pow(p,mThetaalfa));
}
void StHbtMomRes::setMult(const Float_t mult) {
  // Multiplier for the resolution calculation
  mMult = mult;
}
Float_t StHbtMomRes::getMult() const {
  return mMult;
}
void StHbtMomRes::setPDGPid(const int pdgPid) {
  /* Set the momentum resolution parameters
   * obtained from Fabrice's parametrizations
   * of resolution from embedding */

  switch (pdgPid) {
  case 211:
  case -211:
    mPta = 0.0177;
    mPtb = 7.94e-5;
    mPtalfa = -2.27;
    mPhia = 0.0641;
    mPhib = 0.0322;
    mPhialfa = -1.15;
    mThetaa = 0.0779;
    mThetab = 0.0444;
    mThetaalfa = -1.52;
    break;
  case 321:
  case -321:
    mPta = 0.0208;
    mPtb = 0.0006;
    mPtalfa = -2.3;
    mPhia = 0.082;
    mPhib = 0.0273;
    mPhialfa = -2.17;
    mThetaa = 0.0779;
    mThetab = 0.0444;
    mThetaalfa = -1.52;
    break;
  }
}

StHbtMomRes::StHbtMomRes(Float_t aPta, Float_t aPtb, Float_t aPtalfa, 
			 Float_t aPhia, Float_t aPhib, Float_t aPhialfa, 
			 Float_t aThetaa, Float_t aThetab, Float_t aThetaalfa):
  mPta(aPta), mPtb(aPtb), mPtalfa(aPtalfa),
  mPhia(aPhia), mPhib(aPhib), mPhialfa(aPhialfa),
  mThetaa(aThetaa), mThetab(aThetab), mThetaalfa(aThetaalfa)
{ /* no-op */ }

StHbtMomRes::~StHbtMomRes() 
{ /* no-op */ }
