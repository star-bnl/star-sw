/**********************************************************************
 *
 * $Id: StEStructTrack.h,v 1.1 2003/10/15 18:20:51 porter Exp $
 *
 * Author: Jeff Porter merge of work from Aya Ishihara and Jeff Reid
 *
 **********************************************************************
 *
 * Description:  Estruct Track
 *
 ***********************************************************************/

#ifndef _StEStructTrack
#define _StEStructTrack

#include <math.h>
#include "TObject.h"
#include "StHelixD.hh"
#include "StThreeVectorF.hh"
#include "StThreeVectorD.hh"
#include "StLorentzVectorF.hh"
#include "StHelix.hh"
#include "StPhysicalHelixD.hh"

class StEStructTrack : public TObject {

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

  Float_t       mDedx;
  Float_t       mChi2;

  Int_t         mNFitPoints;
  Int_t         mNFoundPoints;
  Int_t         mNMaxPoints;

  Int_t         mDetectorID;
  Int_t         mFlag;

  Short_t       mCharge;
  ULong_t       mMap[2]; 
  UInt_t        mTPCNHits;

  //-> From old StEbye2ptTrack.... note none are persistent

  Bool_t            mIsComplete; //!
  StHelixD          mHelix; //!
  Float_t           mPt; //!
  Float_t           mYt; //!
  Float_t           mXt; //!
  StLorentzVectorF  mFourMomentum; //!
  StThreeVectorF    mStartPos; //!
  StThreeVectorF    mNominalTpcExitPoint; //!
  StThreeVectorF    mNominalTpcEntrancePoint; //!
  StThreeVectorF    mMidTpcPoint; //!
  int               mytbin; //!

public:

  StEStructTrack() : mIsComplete(false) {};
  StEStructTrack(StEStructTrack* track);
  virtual ~StEStructTrack() {};

  void FillTransientData();
  void evalPt();
  void evalYt();
  void evalXt();
  void evalFourMomentum(float mass=0);
  void evalTrajectory(float primvx, float primvy, float primvz, double bfield);
  void FillTpcReferencePoints();

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

  ULong_t TopologyMapData(const Int_t word) const { return mMap[word];} 
  UInt_t  TopologyMapTPCNHits() const {return mTPCNHits;}

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

  // accessors to transient data ...
  Bool_t        isComplete() { return mIsComplete; };
  const StThreeVectorF&   NominalTpcExitPoint()        const;     
  const StThreeVectorF&   NominalTpcEntrancePoint()    const;      
  const StThreeVectorF&   MidTpcPoint()    const; 
  StThreeVectorF     StartPos()                      const;
  StLorentzVectorF& FourMomentum();
  const StHelixD& Helix() const; 
  Float_t  Xt() const;
  Float_t  Yt() const;
  int      getYtBin();


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
  void SetTopologyMapData(const int word, const ULong_t map){ mMap[word] = map;}
  void SetTopologyMapTPCNHits(Int_t nhits) {mTPCNHits = nhits;}

  void SetComplete() { mIsComplete=true; };
  void SetInComplete() { mIsComplete=false; };

  ClassDef(StEStructTrack, 2)   // macro for rootcint
};

inline void StEStructTrack::evalPt(){ mPt=sqrt((mPx*mPx)+(mPy*mPy)); }
inline const StThreeVectorF& StEStructTrack::NominalTpcExitPoint() const {
  return mNominalTpcExitPoint; }
inline  const StThreeVectorF& StEStructTrack::NominalTpcEntrancePoint() const { return mNominalTpcEntrancePoint; };     
inline  const StThreeVectorF& StEStructTrack::MidTpcPoint() const{ return mMidTpcPoint; }; 
inline  StThreeVectorF  StEStructTrack::StartPos() const{ return mStartPos; };
inline  StLorentzVectorF& StEStructTrack::FourMomentum(){ return mFourMomentum;};
inline  const StHelixD& StEStructTrack::Helix() const{ return mHelix;}; 
inline  Float_t  StEStructTrack::Xt() const { return mXt;};
inline  Float_t  StEStructTrack::Yt() const { return mYt;};
inline  int      StEStructTrack::getYtBin() { return mytbin; };

#endif


/***********************************************************************
 *
 * $Log: StEStructTrack.h,v $
 * Revision 1.1  2003/10/15 18:20:51  porter
 * initial check in of Estruct Analysis maker codes.
 *
 *
 *********************************************************************/
