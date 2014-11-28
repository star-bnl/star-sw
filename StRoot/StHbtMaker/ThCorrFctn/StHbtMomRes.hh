/***************************************************************************
 *
 * $Id: StHbtMomRes.hh,
 *
 * Author: Adam Kisiel, Warsaw Univ. of Tech.
 ***************************************************************************
 *
 * Description: Abstracts a STAR Momentum resolution.
 * Stores the momentum resolution parametrizations
 * and returns the errors in STAR measured variables:
 * Pt, Phi and Theta.
 *
 ***************************************************************************/

#ifndef StHbtMomRes_hh
#define StHbtMomRes_hh

#include "StHbtMaker/Infrastructure/StHbtTypes.hh"

class StHbtMomRes {

public:
  StHbtMomRes();
  StHbtMomRes(Float_t aPta, Float_t aPtb, Float_t aPtalfa, Float_t aPtc,
	      Float_t aPhia, Float_t aPhib, Float_t aPhialfa, 
	      Float_t aThetaa, Float_t aThetab, Float_t aThetaalfa,
	      Float_t aPShifta, Float_t aPShiftb, Float_t aPShiftalfa);

  StHbtMomRes(int pdgPid);
  virtual ~StHbtMomRes();

  virtual Float_t getPtError(const Float_t pt) const;
  virtual Float_t getPhiError(const Float_t p) const;
  virtual Float_t getThetaError(const Float_t p) const;
  virtual Float_t getPShift(const Float_t p) const;
  virtual void setPDGPid(const int pdgPid);
  virtual void setMult(const Float_t mult);
  virtual void setPShiftMult(const Float_t mult);
  virtual Float_t getMult() const;
  virtual Float_t getPShiftMult() const;

protected:
  // Error parametrization parameters
  Float_t mPta, mPtb, mPtalfa, mPtc;
  Float_t mPhia, mPhib, mPhialfa;
  Float_t mThetaa, mThetab, mThetaalfa;
  Float_t mPShifta, mPShiftb, mPShiftalfa;
  Float_t mMult, mPShiftMult;
};

#endif
