/**********************************************************************
 *
 * $Id: StEbyeTrack.h,v 1.2 2000/09/01 22:59:11 jgreid Exp $
 *
 * Author: Jeff Reid, UW, July 2000
 *         incorporates elements of code by
 *         Poskanzer, Snellings, & Voloshin
 *
 **********************************************************************
 *
 * Description:  This maker defines the track structure for the
 *               event-by-event DST.
 *
 **********************************************************************
 *
 * $Log: StEbyeTrack.h,v $
 * Revision 1.2  2000/09/01 22:59:11  jgreid
 * version 1 revision ; multiple file handling + additional data members added
 *
 * Revision 1.1.1.1  2000/08/01 13:57:55  jgreid
 * EbyE DST creation and access tools
 *
 *
 *********************************************************************/

#ifndef _StEbyeTrack
#define _StEbyeTrack

#include <math.h>
#include "TObject.h"

class StEbyeTrack : public TObject {

private:

  Float_t       mPx;
  Float_t       mPy;
  Float_t       mPz;

  Float_t       mEta;
  Float_t       mPhi;

  Float_t       mBxPrimary;
  Float_t       mByPrimary;
  Float_t       mBzPrimary;

  Float_t       mBxGlobal;
  Float_t       mByGlobal;
  Float_t       mBzGlobal;

  Float_t       mPIDe;
  Float_t       mPIDpi;
  Float_t       mPIDp;
  Float_t       mPIDk;
  Float_t       mPIDd;

  Float_t       mDedx;       // in units of 10^-6
  Float_t       mChi2;

  Int_t         mNFitPoints;
  Int_t         mNFoundPoints;
  Int_t         mNMaxPoints;

  Int_t         mDetectorID;
  Int_t         mFlag;

  Short_t       mCharge;


public:
  StEbyeTrack() {}
  StEbyeTrack(StEbyeTrack* track);
  virtual ~StEbyeTrack() {}

  // functions which simply return data members
  Float_t Px() const { return mPx; }
  Float_t Py() const { return mPy; }
  Float_t Pz() const { return mPz; }

  Float_t Eta() const { return mEta; }
  Float_t Phi() const { return mPhi; }

  Float_t Bx() const { return mBxPrimary; }
  Float_t By() const { return mByPrimary; }
  Float_t Bz() const { return mBzPrimary; }

  Float_t BxPrimary() const { return mBxPrimary; }
  Float_t ByPrimary() const { return mByPrimary; }
  Float_t BzPrimary() const { return mBzPrimary; }

  Float_t BxGlobal() const { return mBxGlobal; }
  Float_t ByGlobal() const { return mByGlobal; }
  Float_t BzGlobal() const { return mBzGlobal; }

  Float_t PIDe() const { return mPIDe; }
  Float_t PIDpi() const { return mPIDpi; }
  Float_t PIDp() const { return mPIDp; }
  Float_t PIDk() const { return mPIDk; }
  Float_t PIDd() const { return mPIDd; }

  Float_t Dedx() const { return mDedx; }
  Float_t Chi2() const { return mChi2; }

  Int_t NFitPoints() const { return mNFitPoints; }
  Int_t NFoundPoints() const { return mNFoundPoints; }
  Int_t NMaxPoints() const { return mNMaxPoints; }

  Int_t DetectorID() const { return mDetectorID; }
  Int_t Flag() const { return mFlag; }

  Short_t Charge() const { return mCharge; }

  // functions which do some simple calculations
  //  using information contained in data members
  Float_t	Pt();
  Float_t	Mt(Float_t mass);
  Float_t	E(Float_t mass);
  Float_t	Rapidity(Float_t mass);

  Float_t       Dca();
  Float_t       DcaPrimary();
  Float_t       DcaGlobal();
  Float_t       PIDpiPlus();
  Float_t       PIDpiMinus();

  // functions used to set data members
  void SetPx(Float_t px) { mPx = px; }
  void SetPy(Float_t py) { mPy = py; }
  void SetPz(Float_t pz) { mPz = pz; }

  void SetEta(Float_t eta) { mEta = eta; }
  void SetPhi(Float_t phi) { mPhi = phi; }

  void SetBx(Float_t bx) { mBxPrimary = bx; }
  void SetBy(Float_t by) { mByPrimary = by; }
  void SetBz(Float_t bz) { mBzPrimary = bz; }

  void SetBxPrimary(Float_t bxp) { mBxPrimary = bxp; }
  void SetByPrimary(Float_t byp) { mByPrimary = byp; }
  void SetBzPrimary(Float_t bzp) { mBzPrimary = bzp; }

  void SetBxGlobal(Float_t bxg) { mBxGlobal = bxg; }
  void SetByGlobal(Float_t byg) { mByGlobal = byg; }
  void SetBzGlobal(Float_t bzg) { mBzGlobal = bzg; }

  void SetPIDe(Float_t pide) { mPIDe = pide; }
  void SetPIDpi(Float_t pidpi) { mPIDpi = pidpi; }
  void SetPIDp(Float_t pidp) { mPIDp = pidp; }
  void SetPIDk(Float_t pidk) { mPIDk = pidk; }
  void SetPIDd(Float_t pidd) { mPIDd = pidd; }

  void SetDedx(Float_t dedx) { mDedx = dedx; }
  void SetChi2(Float_t chi2) { mChi2 = chi2; }

  void SetNFitPoints(Int_t nfit) { mNFitPoints = nfit; }
  void SetNFoundPoints(Int_t nfound) { mNFoundPoints = nfound; }
  void SetNMaxPoints(Int_t nmax) { mNMaxPoints = nmax; }

  void SetDetectorID(Int_t did) { mDetectorID = did; }
  void SetFlag(Int_t flag) { mFlag = flag; }

  void SetCharge(Short_t charge) { mCharge = charge; }

  ClassDef(StEbyeTrack, 1)   // macro for rootcint
};

#endif
