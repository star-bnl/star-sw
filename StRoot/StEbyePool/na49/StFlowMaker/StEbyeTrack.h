/**********************************************************************
 *
 * $Id: StEbyeTrack.h,v 1.2 2001/05/14 23:04:08 posk Exp $
 *
 * Author: Jeff Reid, UW, July 2000
 +         Art Poskanzer, LBNL Nov. 2000
 +         Alexander Wetzler, IKF
 *
 **********************************************************************
 *
 * Description:  This maker defines the track structure for the
 *               event-by-event DST.
 *
 **********************************************************************
 *
 * $Log: StEbyeTrack.h,v $
 * Revision 1.2  2001/05/14 23:04:08  posk
 * Can select PID for event plane particles. Protons not used for 1st har.
 * event plane.
 *
 * Revision 1.1.1.1  2000/11/22 19:45:09  jcs
 * Imported sources for MICRODST
 *
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

  Int_t         mDetectorID;
  Int_t         mFlag;

  Short_t       mCharge;

  // added for Flow Analysis

  Float_t       mTmeanCharge;
  Float_t       mTmeanChargeV1;
  Float_t       mTmeanChargeV2;
  Float_t       mTmeanChargeM;

  Int_t         mNFitPoints;
  Int_t         mNFitPointsV1;
  Int_t         mNFitPointsV2;
  Int_t         mNFitPointsM;

  Int_t         mNFoundPoints; 
  Int_t         mNFoundPointsV1; 
  Int_t         mNFoundPointsV2; 
  Int_t         mNFoundPointsM; 

  Int_t         mNMaxPoints;
  Int_t         mNMaxPointsV1;
  Int_t         mNMaxPointsV2;
  Int_t         mNMaxPointsM;

  // end of additions

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

  // added for Flow Analysis

  Float_t     TmeanCharge() {return mTmeanCharge;}
  Float_t     TmeanChargeV1() {return mTmeanChargeV1;}
  Float_t     TmeanChargeV2() {return mTmeanChargeV2;}
  Float_t     TmeanChargeM() {return mTmeanChargeM;}
  
  Int_t       NFitPoints() { return mNFitPoints; }
  Int_t       NFitPointsV1() { return mNFitPointsV1; }
  Int_t       NFitPointsV2() { return mNFitPointsV2; }
  Int_t       NFitPointsM() { return mNFitPointsM; }

  Int_t       NFoundPoints() { return mNFoundPoints; }
  Int_t       NFoundPointsV1() { return mNFoundPointsV1; }
  Int_t       NFoundPointsV2() { return mNFoundPointsV2; }
  Int_t       NFoundPointsM() { return mNFoundPointsM; }

  Int_t       NMaxPoints() { return mNMaxPoints; }
  Int_t       NMaxPointsV1() { return mNMaxPointsV1; }
  Int_t       NMaxPointsV2() { return mNMaxPointsV2; }
  Int_t       NMaxPointsM() { return mNMaxPointsM; }
   
  
  // end of additions

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

  void SetDetectorID(Int_t did) { mDetectorID = did; }
  void SetFlag(Int_t flag) { mFlag = flag; }

  void SetCharge(Short_t charge) { mCharge = charge; }

  // added for Flow Analysis

  void SetTmeanCharge(Float_t tmeancharge) {mTmeanCharge = tmeancharge;}
  void SetTmeanChargeV1(Float_t tmeancharge) {mTmeanChargeV1 = tmeancharge;}
  void SetTmeanChargeV2(Float_t tmeancharge) {mTmeanChargeV2 = tmeancharge;}
  void SetTmeanChargeM(Float_t tmeancharge) {mTmeanChargeM = tmeancharge;}

  void SetNFitPoints(Int_t nfitpoints) {mNFitPoints = nfitpoints;}
  void SetNFitPointsV1(Int_t nfitpoints) {mNFitPointsV1 = nfitpoints;}
  void SetNFitPointsV2(Int_t nfitpoints) {mNFitPointsV2 = nfitpoints;}
  void SetNFitPointsM(Int_t nfitpoints) {mNFitPointsM = nfitpoints;}

  void SetNFoundPoints(Int_t nfoundpoints) {mNFoundPoints = nfoundpoints;}
  void SetNFoundPointsV1(Int_t nfoundpoints) {mNFoundPointsV1 = nfoundpoints;}
  void SetNFoundPointsV2(Int_t nfoundpoints) {mNFoundPointsV2 = nfoundpoints;}
  void SetNFoundPointsM(Int_t nfoundpoints) {mNFoundPointsM = nfoundpoints;}

  void SetNMaxPoints(Int_t nmaxpoints) {mNMaxPoints = nmaxpoints;}
  void SetNMaxPointsV1(Int_t nmaxpoints) {mNMaxPointsV1 = nmaxpoints;}
  void SetNMaxPointsV2(Int_t nmaxpoints) {mNMaxPointsV2 = nmaxpoints;}
  void SetNMaxPointsM(Int_t nmaxpoints) {mNMaxPointsM = nmaxpoints;}

  // end of additions

  ClassDef(StEbyeTrack, 1)   // macro for rootcint
};

#endif
