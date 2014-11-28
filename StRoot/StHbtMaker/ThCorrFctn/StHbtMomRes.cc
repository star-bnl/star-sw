#include "StHbtMaker/ThCorrFctn/StHbtMomRes.hh"

StHbtMomRes::StHbtMomRes() {
  // Use Fabrice's initial parameters for pions
  setPDGPid(211);
  mMult = 1.0;
  mPShiftMult = 1.0;
}
StHbtMomRes::StHbtMomRes(int pdgPid) {
  setPDGPid(pdgPid);
} 
inline Float_t StHbtMomRes::getPtError(const Float_t pt) const {
  return mMult * (mPta + mPtb*::pow(pt,mPtalfa) + mPtc*pt);
}
inline Float_t StHbtMomRes::getPhiError(const Float_t p) const {
  return mMult * (mPhia + mPhib*::pow(p,mPhialfa));
}
inline Float_t StHbtMomRes::getThetaError(const Float_t p) const {
  return mMult * (mThetaa + mThetab*::pow(p,mThetaalfa));
}
inline Float_t StHbtMomRes::getPShift(const Float_t p) const {
  return mPShiftMult * mMult * (mPShifta + mPShiftb*::pow(p,mPShiftalfa));
}

void StHbtMomRes::setMult(const Float_t mult) {
  // Multiplier for the resolution calculation
  mMult = mult;
}

void StHbtMomRes::setPShiftMult(const Float_t mult) {
  mPShiftMult = mult;
}


Float_t StHbtMomRes::getMult() const {
  return mMult;
}

Float_t StHbtMomRes::getPShiftMult() const {
  return mPShiftMult;
}

void StHbtMomRes::setPDGPid(const int pdgPid) {
  /* Set the momentum resolution parameters
   * obtained from Fabrice's parametrizations
   * of resolution from embedding */

  /*
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
    mPShifta = -0.00274;
    mPShiftb = -0.000335;
    mPShiftalfa = -2.06;
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
    mPShifta = -0.00162;
    mPShiftb = -0.000793;
    mPShiftalfa = -2.74;
    break;*/
    /* Caution! These are not real parameters for protons -
       just copied from kaons for convenience, do not rely
       on them for any physics results! 
    case 2212:
    case -2212:
    mPta = 0.0208;
    mPtb = 0.0006;
    mPtalfa = -2.3;
    mPhia = 0.082;
    mPhib = 0.0273;
    mPhialfa = -2.17;
    mThetaa = 0.0779;
    mThetab = 0.0444;
    mThetaalfa = -1.52;
    mPShifta = -0.00162;
    mPShiftb = -0.000793;
    mPShiftalfa = -2.74;
    break;
  }
*/
  /* Just added new parameters */

  switch (pdgPid) {
  case 211:
  case -211:
    mPta = 0.01074;
    mPtb = 0.001918;
    mPtalfa = -0.9895;
    mPtc = 0.009706;
    mPhia = 0.00201;
    mPhib = 0.001018;
    mPhialfa = -1.274;
    mThetaa = 0.000908;
    mThetab = 0.001255;
    mThetaalfa = -1.141;
    mPShifta = -0.00;
    mPShiftb = -0.00;
    mPShiftalfa = 0;
    break;
  case 321:
  case -321:
    mPta = 0.01981;
    mPtb = 0.001371;
    mPtalfa = -2.112;
    mPtc = 0.0;
    mPhia = 0.001791;
    mPhib = 0.001319;
    mPhialfa = -1.686;
    mThetaa = 0.0005202;
    mThetab = 0.001752;
    mThetaalfa = -1.352;
    mPShifta = -0.004136;
    mPShiftb = 0.003511;
    mPShiftalfa = -1.192;
    break;
    case 2212:
    case -2212:
    mPta = 0.01708;
    mPtb = 0.006794;
    mPtalfa = -1.78;
    mPtc = 0.0;
    mPhia = 0.0006575;
    mPhib = 0.002813;
    mPhialfa = -1.583;
    mThetaa = 0.0002846;
    mThetab = 0.002458;
    mThetaalfa = -1.475;
    mPShifta = -0.006509;
    mPShiftb = 0.008757;
    mPShiftalfa = -1.373;
    break;
  }
}

StHbtMomRes::StHbtMomRes(Float_t aPta, Float_t aPtb, Float_t aPtalfa, Float_t aPtc,
			 Float_t aPhia, Float_t aPhib, Float_t aPhialfa, 
			 Float_t aThetaa, Float_t aThetab, Float_t aThetaalfa,
			 Float_t aPShifta, Float_t aPShiftb, Float_t aPShiftalfa):
  mPta(aPta), mPtb(aPtb), mPtalfa(aPtalfa), mPtc(aPtc), 
  mPhia(aPhia), mPhib(aPhib), mPhialfa(aPhialfa),
  mThetaa(aThetaa), mThetab(aThetab), mThetaalfa(aThetaalfa),
  mPShifta(aPShifta), mPShiftb(aPShiftb), mPShiftalfa(aPShiftalfa)
{ /* no-op */ }

StHbtMomRes::~StHbtMomRes() 
{ /* no-op */ }
